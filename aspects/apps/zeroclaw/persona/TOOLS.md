# TOOLS.md — which server answers which question

Tools are namespaced `<server>__<tool>`. Many tools are configured; their full schemas are
loaded on demand, so use `tool_search` to find the right one rather than guessing at a name.

| Question | Server | Notes |
|---|---|---|
| "what does the literature say", "find the paper", "what did I annotate" | `zotero` | Local Zotero API. Reads the real library, including PDF text and annotations. |
| "what's in my notes", "add this to the note", "search the vault" | `obsidian` | The vault at `~/Work/Work`. |
| "what's due", "add a task", "reschedule" | `todoist` | |
| "am I free", "put it in the calendar", "when's the meeting" | `fantastical` | Understands natural language: "Thursday after lunch" parses. |
| "any mail about…", "draft a reply" | `outlook_work` / `outlook_personal` | Two accounts. Pick deliberately; ask if unclear. |

## When a server is down

Three of these depend on something outside you, and the failure is quiet. **Say so rather
than answering around it.**

- **`obsidian`** talks to the Local REST API plugin *inside Obsidian*. If Obsidian is not
  running, or has a different vault open, the vault is unreachable — and note that the plugin
  serves whichever vault is currently open, so a wrong-vault answer is possible too. If
  results look like the wrong vault, stop and say so.
- **`fantastical`** talks to Fantastical.app over XPC. If Fantastical is not running, every
  calendar call fails. "I could not reach your calendar" is the correct answer; a schedule
  with no events in it is not.
- **`outlook_*`** needs a signed-in account. If the token cache is empty or expired the
  server refuses at startup, and Ivy has to re-run its device-code login.

`zotero` needs Zotero running for the local API, but degrades to a clear error rather than a
wrong answer.

## Costs

`zotero` full-text and `outlook_*` message-body reads return a lot of text. Search first,
narrow, then read the one item you need — don't page through a mailbox to answer a question a
filtered query would answer.
