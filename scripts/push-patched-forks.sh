#!/usr/bin/env bash
# Publish this repo's patched flake inputs as GitHub forks.
#
# For each entry in the manifest (see lib/patched-forks-manifest.nix) this:
#   1. fetches the exact upstream rev the flake currently locks,
#   2. applies patches/<input>/*.patch on top of it as individual commits,
#   3. force-pushes the result to <targetOwner>/<targetRepo> on <branch>.
#
# The pushed branch is fully regenerated every run (rebased onto whatever rev
# flake.lock currently pins), so downstream flakes get "upstream-as-locked +
# your patches" at   github:<targetOwner>/<targetRepo>/<branch>.
#
# `git apply` is strict: if a patch no longer applies to the locked rev the run
# fails loudly — the same "tell me when it breaks" guarantee the FOD build in
# aspects/tooling/patch-inputs.nix gives.
#
# Auth: GH_TOKEN must be a token (classic PAT or GitHub App installation token)
# with contents:write on the destination forks. It is used both for `gh` and in
# the git push URL.
#
# Usage:
#   scripts/push-patched-forks.sh --manifest manifest.json
#   nix eval --impure --json --expr 'import ./lib/patched-forks-manifest.nix {}' \
#     | scripts/push-patched-forks.sh
#   DRY_RUN=1 scripts/push-patched-forks.sh --manifest manifest.json   # fetch+apply, no push
set -euo pipefail

manifest=""
dry_run="${DRY_RUN:-0}"
while [ "$#" -gt 0 ]; do
	case "$1" in
	--manifest)
		manifest="$2"
		shift 2
		;;
	--dry-run)
		dry_run=1
		shift
		;;
	-h | --help)
		sed -n '2,29p' "$0"
		exit 0
		;;
	*)
		echo "unknown arg: $1" >&2
		exit 2
		;;
	esac
done

repo_root="$(git rev-parse --show-toplevel 2>/dev/null || pwd)"

if [ -n "$manifest" ]; then
	manifest_json="$(cat "$manifest")"
else
	manifest_json="$(cat)"
fi

# Commit identity is resolved from the token by resolve_committer() below,
# unless PATCH_COMMIT_NAME / PATCH_COMMIT_EMAIL explicitly override it.
git_name=""
git_email=""

# Auto-create a missing fork before pushing (set AUTO_FORK=0 to manage forks by
# hand). The fork lands under the authenticated account, so GH_TOKEN must be for
# `targetOwner` (a PAT for that account, or a GitHub App installed on it).
auto_fork="${AUTO_FORK:-1}"

# Ensure <targetOwner>/<targetRepo> exists, forking <up> if not. Forking is
# async on GitHub's side, so poll until the new repo answers.
ensure_fork() {
	local up_owner="$1" up_repo="$2" t_owner="$3" t_repo="$4"
	if gh api "repos/${t_owner}/${t_repo}" >/dev/null 2>&1; then
		return 0
	fi
	if [ "$auto_fork" != 1 ]; then
		echo "  ERROR: ${t_owner}/${t_repo} does not exist and AUTO_FORK=0" >&2
		return 1
	fi
	echo "  forking ${up_owner}/${up_repo} → ${t_owner}/${t_repo}"
	# 202 + async provisioning. default_branch_only keeps it quick; the fork
	# still shares upstream's whole object network, so the locked base commit is
	# reachable when we push.
	gh api --method POST "repos/${up_owner}/${up_repo}/forks" \
		-F default_branch_only=true >/dev/null
	for _ in $(seq 1 30); do
		if gh api "repos/${t_owner}/${t_repo}" >/dev/null 2>&1; then
			echo "  fork ready: ${t_owner}/${t_repo}"
			return 0
		fi
		sleep 2
	done
	echo "  ERROR: fork ${t_owner}/${t_repo} was not available after ~60s" >&2
	return 1
}

# Resolve the git author/committer from whatever token GH_TOKEN holds, so the
# patch commits are attributed to that account. Order:
#   1. explicit PATCH_COMMIT_NAME + PATCH_COMMIT_EMAIL override,
#   2. a GitHub App token (GH_APP_SLUG set) → the "<slug>[bot]" identity, with
#      the canonical <id>+<slug>[bot]@users.noreply.github.com noreply address,
#   3. a user/PAT token → `gh api user` login + id.
# Falls back to a plain name if the API can't be reached (e.g. a tokenless dry
# run, where the identity is cosmetic anyway).
resolve_committer() {
	if [ -n "${PATCH_COMMIT_NAME:-}" ] && [ -n "${PATCH_COMMIT_EMAIL:-}" ]; then
		git_name="$PATCH_COMMIT_NAME"
		git_email="$PATCH_COMMIT_EMAIL"
		return
	fi
	local slug login id
	if [ -n "${GH_APP_SLUG:-}" ]; then
		slug="$GH_APP_SLUG"
		git_name="${slug}[bot]"
		id="$(gh api "/users/${slug}[bot]" --jq .id 2>/dev/null || true)"
		git_email="${id:+${id}+}${slug}[bot]@users.noreply.github.com"
		return
	fi
	login="$(gh api user --jq .login 2>/dev/null || true)"
	if [ -n "$login" ]; then
		id="$(gh api user --jq .id 2>/dev/null || true)"
		git_name="$login"
		git_email="${id:+${id}+}${login}@users.noreply.github.com"
		return
	fi
	git_name="auscyber-bot"
	git_email="auscyber-bot@users.noreply.github.com"
}

# Push needs a token; a dry run does not (it never touches a fork remote).
if [ "$dry_run" != 1 ]; then
	: "${GH_TOKEN:?set GH_TOKEN to a token with push access to the forks (or use --dry-run)}"
fi

process_entry() {
	local entry="$1"
	local name up_owner up_repo rev t_owner t_repo branch
	name="$(jq -r '.name' <<<"$entry")"
	up_owner="$(jq -r '.upstreamOwner' <<<"$entry")"
	up_repo="$(jq -r '.upstreamRepo' <<<"$entry")"
	rev="$(jq -r '.rev' <<<"$entry")"
	t_owner="$(jq -r '.targetOwner' <<<"$entry")"
	t_repo="$(jq -r '.targetRepo' <<<"$entry")"
	branch="$(jq -r '.branch' <<<"$entry")"

	echo
	echo "── ${name}: ${up_owner}/${up_repo}@${rev:0:12} → ${t_owner}/${t_repo}#${branch}"

	# Ensure the destination fork exists before we push to it (skipped on a dry
	# run, which never touches the fork remote).
	if [ "$dry_run" != 1 ]; then
		ensure_fork "$up_owner" "$up_repo" "$t_owner" "$t_repo"
	fi

	local work
	work="$(mktemp -d)"
	(
		cd "$work"
		git init -q
		git config advice.detachedHead false
		# GitHub allows fetching an exact commit by SHA, so we never need the
		# whole history — just the locked rev the patches were written against.
		git fetch --depth=1 -q "https://github.com/${up_owner}/${up_repo}.git" "$rev"
		git checkout -q -B "$branch" FETCH_HEAD

		# One commit per patch file, in manifest order, keeping the fork's
		# history readable ("patch: <file>").
		#
		# Apply with GNU `patch -p1` — exactly what nixpkgs `applyPatches` uses —
		# so the fork's tree is byte-identical to what the local FOD build in
		# aspects/tooling/patch-inputs.nix produces (`patch` tolerates the fuzz/
		# offset that strict `git apply` rejects, e.g. an nh flake.lock hunk).
		# The `--dry-run` guard means a failed match leaves the tree untouched,
		# so the `git apply` fallback (for binary hunks GNU patch can't handle)
		# never double-applies.
		local p
		while read -r rel; do
			[ -n "$rel" ] || continue
			p="${repo_root}/${rel}"
			if patch -p1 -f -s --dry-run -i "$p" >/dev/null 2>&1; then
				patch -p1 -f -s --no-backup-if-mismatch -i "$p"
			elif git apply --whitespace=nowarn "$p"; then
				:
			else
				echo "  ERROR: ${rel} does not apply to ${up_repo}@${rev:0:12}" >&2
				exit 1
			fi
			git add -A
			git -c user.name="$git_name" -c user.email="$git_email" \
				commit -q -m "patch: ${rel##*/}"
		done < <(jq -r '.patches[]' <<<"$entry")

		local head
		head="$(git rev-parse --short HEAD)"
		if [ "$dry_run" = 1 ]; then
			echo "  (dry-run) ${head} would push to github:${t_owner}/${t_repo}/${branch}"
		else
			git push -q --force \
				"https://x-access-token:${GH_TOKEN}@github.com/${t_owner}/${t_repo}.git" \
				"${branch}:refs/heads/${branch}"
			echo "  pushed ${head} → github:${t_owner}/${t_repo}/${branch}"
		fi
	)
	local rc=$?
	rm -rf "$work"
	return "$rc"
}

count="$(jq 'length' <<<"$manifest_json")"
if [ "$count" -eq 0 ]; then
	echo "manifest is empty — nothing to push."
	exit 0
fi

resolve_committer
echo "committing patches as ${git_name} <${git_email}>"
echo "Publishing ${count} patched fork(s)$([ "$dry_run" = 1 ] && echo ' (dry run)')…"

mapfile -t entries < <(jq -c '.[]' <<<"$manifest_json")
for entry in "${entries[@]}"; do
	process_entry "$entry"
done

echo
echo "── consume these as flake inputs ──"
for entry in "${entries[@]}"; do
	name="$(jq -r '.name' <<<"$entry")"
	t_owner="$(jq -r '.targetOwner' <<<"$entry")"
	t_repo="$(jq -r '.targetRepo' <<<"$entry")"
	branch="$(jq -r '.branch' <<<"$entry")"
	printf '  inputs.%s.url = "github:%s/%s/%s";\n' "$name" "$t_owner" "$t_repo" "$branch"
done
