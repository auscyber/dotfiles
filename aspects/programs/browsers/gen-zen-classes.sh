#!/usr/bin/env bash
# Derive current + upcoming UniMelb classes from the Obsidian vault -> JSON pin set.
# Runtime tools: coreutils(date), jq, yq-go(yq), util-linux(uuidgen), and the Obsidian CLI.
set -euo pipefail

VAULT_ROOT="${ZEN_CLASSES_VAULT:-/Users/ivypierlot/Work/Work}"
UNI="Uni"
OB="${OBSIDIAN_CLI:-/usr/local/bin/obsidian}"
HORIZON_DAYS="${ZEN_CLASSES_HORIZON:-7}"
# Frozen namespace UUID -> stable per-subject pin ids (never change this).
NS="7f3a2c10-9e64-5b18-a2d7-0c5f1e6b4a90"

have_cli() { [ -x "$OB" ] && timeout 15 "$OB" files >/dev/null 2>&1; }
USE_CLI=false; if have_cli; then USE_CLI=true; fi
echo "read path: $([ "$USE_CLI" = true ] && echo 'obsidian CLI' || echo 'markdown parse (fallback)')" >&2

# prop <name> <vault-relative-path>  -> scalar value ("" if empty/missing). Lists -> first item.
prop() {
  local name="$1" path="$2" v
  if [ "$USE_CLI" = true ]; then
    v="$(timeout 15 "$OB" property:read name="$name" path="$path" </dev/null 2>/dev/null | head -1 || true)"
    [ "$v" = "(empty)" ] && v=""
  else
    v="$(yq --front-matter=extract -r \
      ".\"$name\" // \"\" | (if type==\"!!seq\" then (.[0] // \"\") else . end)" \
      "$VAULT_ROOT/$path" 2>/dev/null || true)"
    [ "$v" = "null" ] && v=""
  fi
  # strip wikilink brackets / surrounding quotes / whitespace
  v="${v#"${v%%[![:space:]]*}"}"; v="${v%"${v##*[![:space:]]}"}"
  v="${v#\[\[}"; v="${v%\]\]}"
  printf '%s' "$v"
}

# list files under a Uni subfolder (excluding _-prefixed helper notes)
list_notes() {
  local sub="$1"
  if [ "$USE_CLI" = true ]; then
    timeout 20 "$OB" files 2>/dev/null
  else
    ( cd "$VAULT_ROOT" && find "$UNI/$sub" -maxdepth 1 -name '*.md' )
  fi | grep -E "^$UNI/$sub/[^/]+\.md$" | grep -v "/_" | sort
}

season_rank() { case "$1" in
  *Summer*) echo 0;; *"Semester 1"*|*Autumn*) echo 1;;
  *Winter*) echo 2;; *"Semester 2"*|*Spring*) echo 3;; *) echo 5;; esac; }

TODAY="$(date +%F)"
HORIZON="$(date -d "$TODAY +$HORIZON_DAYS days" +%F 2>/dev/null || date -v +"${HORIZON_DAYS}"d +%F)"

# ---- build term table -------------------------------------------------------
TERMS="[]"
while IFS= read -r tf; do
  [ -n "$tf" ] || continue
  term="$(basename "$tf" .md)"
  year="$(prop year "$tf")"; start="$(prop start_date "$tf")"
  end="$(prop end_date "$tf")"; active="$(prop active "$tf")"
  rank="$(season_rank "$term")"
  key=$(( ${year:-0} * 10 + rank ))
  TERMS="$(jq -c --arg t "$term" --arg y "$year" --arg s "$start" --arg e "$end" \
    --argjson a "$([ "$active" = true ] && echo true || echo false)" --argjson k "$key" \
    '. + [{term:$t,year:$y,start:$s,end:$e,active:$a,key:$k}]' <<<"$TERMS")"
done < <(list_notes "Study Periods")

ACTIVE_TERM="$(jq -r 'map(select(.active))|.[0].term // ""' <<<"$TERMS")"
ACTIVE_END="$(jq -r 'map(select(.active))|.[0].end // ""' <<<"$TERMS")"
ACTIVE_KEY="$(jq -r 'map(select(.active))|.[0].key // 0' <<<"$TERMS")"
# next chronological term after the active one
NEXT_TERM="$(jq -r --argjson ak "$ACTIVE_KEY" \
  'map(select(.key>$ak))|sort_by(.key)|.[0].term // ""' <<<"$TERMS")"
# does the active term end within the horizon?
ENDS_SOON=false
if [ -n "$ACTIVE_END" ] && [[ "$ACTIVE_END" < "$HORIZON" || "$ACTIVE_END" == "$HORIZON" ]] \
   && [[ "$ACTIVE_END" > "$TODAY" || "$ACTIVE_END" == "$TODAY" ]]; then ENDS_SOON=true; fi
# upcoming term set: start_date within [today,horizon]  ∪  (ends_soon ? next_term : ∅)
UPCOMING_TERMS="$(jq -r --arg today "$TODAY" --arg hz "$HORIZON" \
  'map(select(.start!="" and .start>=$today and .start<=$hz))|map(.term)|.[]' <<<"$TERMS")"
[ "$ENDS_SOON" = true ] && [ -n "$NEXT_TERM" ] && UPCOMING_TERMS="$(printf '%s\n%s' "$UPCOMING_TERMS" "$NEXT_TERM")"
UPCOMING_TERMS="$(printf '%s\n' "$UPCOMING_TERMS" | sed '/^$/d' | sort -u)"

{
  echo "TODAY=$TODAY HORIZON=$HORIZON" >&2
  echo "ACTIVE=$ACTIVE_TERM (ends $ACTIVE_END, ends_soon=$ENDS_SOON) NEXT=$NEXT_TERM" >&2
  echo "UPCOMING_TERMS=[$(echo "$UPCOMING_TERMS" | tr '\n' ',' )]" >&2
}

in_upcoming() { printf '%s\n' "$UPCOMING_TERMS" | grep -qxF "$1"; }
term_year() { jq -r --arg t "$1" 'map(select(.term==$t))|.[0].year // ""' <<<"$TERMS"; }

# ---- classify subjects ------------------------------------------------------
OUT="[]"
while IFS= read -r sf; do
  [ -n "$sf" ] || continue
  term="$(prop "Study Period" "$sf")"
  [ -n "$term" ] || continue
  kind=""
  if [ "$term" = "$ACTIVE_TERM" ]; then kind="current"
  elif in_upcoming "$term"; then kind="upcoming"
  else continue; fi
  sid="$(prop subject_id "$sf")"; [ -n "$sid" ] || continue
  cid="$(prop canvas_id "$sf")"
  stem="$(basename "$sf" .md)"
  year="$(term_year "$term")"
  lsid="$(printf '%s' "$sid" | tr '[:upper:]' '[:lower:]')"
  if [ -n "$cid" ]; then url="https://canvas.lms.unimelb.edu.au/courses/$cid"
  else url="https://handbook.unimelb.edu.au/$year/subjects/$lsid"; fi
  uuid="$(uuidgen --sha1 --namespace "$NS" --name "$sid")"
  OUT="$(jq -c --arg t "$stem ($sid)" --arg sid "$sid" --arg cid "$cid" --arg u "$url" \
    --arg term "$term" --arg kind "$kind" --arg uuid "$uuid" \
    '. + [{title:$t,subjectId:$sid,canvasId:$cid,url:$u,term:$term,kind:$kind,uuid:$uuid}]' <<<"$OUT")"
done < <(list_notes "Subjects")

# dedup by subjectId, deterministic order: current before upcoming, then subjectId
RESULT="$(jq 'unique_by(.subjectId)
    | sort_by(((if .kind=="current" then 0 else 1 end)|tostring) + "|" + .subjectId)' <<<"$OUT")"

REPO="${ZEN_CLASSES_REPO:-$PWD}"
if [ -e "$REPO/flake.nix" ]; then
  OUT_FILE="$REPO/aspects/programs/browsers/zen-classes.json"
  printf '%s\n' "$RESULT" >"$OUT_FILE"
  echo "wrote $OUT_FILE ($(jq length <<<"$RESULT") classes)" >&2
else
  printf '%s\n' "$RESULT"
fi
