# USER.md — who you're helping

- **Name:** Ivy
- **Machine:** a MacBook Pro. Everything below is local to it.
- **Timezone:** the machine's own. Read it with the `time` tool rather than assuming; never
  do date arithmetic against a remembered timezone.

## Where things live

| Thing | Where | Reached by |
|---|---|---|
| Notes | the Obsidian vault at `~/Work/Work` | the `obsidian` MCP server |
| Sources, PDFs, annotations | Zotero (local, running) | the `zotero` MCP server |
| Tasks | Todoist | the `todoist` MCP server |
| Calendar | Fantastical | the `fantastical` MCP server |
| Mail + Outlook calendars | two accounts, work and personal | `outlook_work`, `outlook_personal` |

Two Outlook accounts, deliberately separate. **Never assume which one a request means.** If
it is not obvious from context, ask. Work mail and personal mail are different worlds and
crossing them is a real mistake, not a cosmetic one.

## Vault conventions

The vault is managed by Obsidian with `obsidian-git`, `zotlit`, `dataview`, `omnisearch`,
`js-engine` and `sqlite-db`. Other tools read the frontmatter you write: a field with the
wrong name or shape does not error, it silently drops out of a query and nobody notices for a
month. **Before inventing a field, look at how existing notes in the same folder spell it.**

The coursework model lives under `Uni/`:

| Folder | What it holds |
|---|---|
| `Uni/Readings/<citekey>.md` | one note per Zotero item, ~1600 of them. `citekey` is the filename and the join key; `zotero-key` is the item key. |
| `Uni/Subjects/` | one note per subject, with `subject_id`, `Study Period`, `category` |
| `Uni/Sessions/` | lectures and tutorials |
| `Uni/Work/` | assessments — **two-way-synced with Todoist**, see below |
| `Uni/Study Periods/` | semesters |
| `Uni/_Templates/`, `Uni/_meta/` | templates and the vault's own scripts. Not yours to edit. |

Each folder's `_database.md` is a Notion-bases view declaring that folder's column set. It is
the schema. Read it before adding a field to notes in that folder.

`Uni/Briefs/` is yours — daily briefs go there and nowhere else.

## The Todoist sync already exists

`Uni/_meta/js/todoist.js` runs a two-way sync between Todoist tasks labelled `obsidian` and
`Uni/Work/` notes tagged `#todoist`, keyed on a `todoist_id` frontmatter field. Ivy owns it and
it runs from Obsidian, not from you.

So: do not build a second convention on top of it. Make a Todoist task when Ivy asks for that
task, and otherwise leave the `obsidian` label and `Uni/Work/` alone. Readings are tracked by
`read_status` in the vault, not by Todoist tasks.
