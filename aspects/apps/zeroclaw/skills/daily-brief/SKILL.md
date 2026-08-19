---
name: daily-brief
description: Assemble today's calendar, mail, tasks and reading load into one note in the vault
tags: [calendar, mail, tasks, vault]
---

# Daily brief

One note, written to `Uni/Briefs/YYYY-MM-DD.md` in the vault. Create `Uni/Briefs/` if it does
not exist; briefs go nowhere else.

## Gather

Do these in whatever order; skip nothing silently.

1. **Today's date and timezone** — from the `time` tool, not from memory.
2. **Calendar** — `fantastical` for today and tomorrow. Include all-day items.
3. **Mail** — `outlook_work` *and* `outlook_personal`, unread since the last brief. Subject,
   sender, and whether it looks like it needs a reply. **Do not open every message body**;
   list first, read only what is ambiguous.
4. **Tasks** — `todoist`, due today and overdue.
5. **Readings** — the vault: notes in `Uni/Readings/` whose `read_status` is not a finished
   value, filtered to the current study period via their `collections` field. This list is
   long (there are ~1600 notes); surface at most the ten most relevant and say how many were
   left out.

If a source is unreachable, put a line in the brief saying so — *"Fantastical was not running;
no calendar in this brief"* — rather than omitting the section. A brief that quietly lacks a
section reads as a day with nothing in it.

## Write

```markdown
# Brief — <weekday> <date>

## Today
<calendar, chronological; "nothing scheduled" if empty>

## Needs a reply
<mail, grouped by account, work first; omit the heading entirely if both inboxes are clear>

## Due
<todoist: overdue first, then today>

## Reading
<the shortlist, each linked as [[Uni/Readings/<citekey>]]>

## Not reached
<only if a source failed>
```

Keep it scannable. No commentary, no encouragement, no restating the headings in prose.

## Rules

- The brief is a new file each day. If today's already exists, **append a `## Update <time>`
  section** rather than overwriting it.
- Never create, complete or reschedule anything while writing a brief. It is a read.
