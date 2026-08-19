---
name: reading-triage
description: Reconcile Uni/Readings against Zotero — fill missing metadata, set read status, pull annotations
tags: [zotero, vault, readings]
---

# Reading triage

The vault at `~/Work/Work` holds `Uni/Readings/<citekey>.md`, one note per Zotero item
(~1600 of them). Zotero is the source; the note is the working copy. This skill keeps them
consistent. It never invents a reading list.

## The schema — match it exactly

Frontmatter, as it already exists in that folder:

```yaml
title: "…"
related: []
collections:
  - 2026/Semester 1/Immigration and Identity/Dissertation/Maghrebain
zotero-key: YFV8L6DC          # note the hyphen
citekey: 1983CrimeRaciste2016 # also the filename
item_type: webpage
authors: "…"
date: 2016-08-02
date_added: 2026-06-28
date_modified: 2026-06-29
url: …
zotero_uri: zotero://select/library/items/YFV8L6DC
zotero_tags:
  - tagID: 572
    name: /unread
read_status: Not started
doi: ""
```

`Uni/Readings/_database.md` is a Notion-bases view over this folder — it declares the column
set. **Read it before adding any field.** A field it does not know about will not appear in
Ivy's table, and a field spelled differently from the schema silently drops out.

`citekey` is the join key to Zotero. `zotero-key` is the item key for the Zotero tools.

## What to do

Work in small batches and report what changed. Never sweep all 1600 notes in one pass.

1. **New Zotero items with no note.** Find them with `zotero_get_recent` /
   `zotero_search_items`, check `Uni/Readings/<citekey>.md`, and create the missing note with
   the frontmatter above filled from the Zotero metadata. Body starts empty.
2. **Notes with a dangling `zotero-key`.** The item was deleted or merged in Zotero. **Flag
   it** — add `needs_review: true` to the frontmatter and list it in your report. Do not
   delete the note.
3. **Gaps.** An empty `collections` is an untriaged reading. Infer from the Zotero collection
   path the item sits in, and from `Uni/Subjects/` for the subject name. **Ask when
   ambiguous** — a wrong `collections` value files a reading under the wrong subject and it is
   never seen again.
4. **Status.** `read_status` is currently `Not started` on essentially every note. Move a note
   forward only on evidence: Zotero annotations exist, or Ivy says so. Do not infer "read"
   from a `/unread` tag disappearing.
5. **Annotations.** When a reading has Zotero annotations and the note body has no annotation
   section, pull them through with `zotero_synthesize_annotations` and **append** them under a
   `## Annotations` heading. Never replace an existing one — append a dated subsection.

## Do not touch Todoist here

The vault already runs its own two-way Todoist sync (`Uni/_meta/js/todoist.js`): Todoist tasks
labelled `obsidian` ↔ notes in `Uni/Work/` tagged `#todoist`, keyed on a `todoist_id`
frontmatter field. It is Templater-driven and Ivy owns it.

Creating reading tasks in Todoist from here would produce a second, competing convention and
duplicate rows on the next sync. **Don't.** Readings are tracked by `read_status` in the vault.
If Ivy asks for a Todoist task about a reading, make exactly that one task and let the existing
sync alone.

## Rules

- Patch fields; never rewrite a note wholesale.
- Never delete a note. Flag with `needs_review: true`.
- Report at the end: created, updated, flagged, skipped — with counts and the flagged list.
