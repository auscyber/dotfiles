---
name: inbox-triage
description: Sort unread Outlook mail across both accounts into needs-reply, needs-action, and noise
tags: [mail, outlook]
---

# Inbox triage

Two accounts, `outlook_work` and `outlook_personal`. Triage them **separately** and report
them separately. Never move, forward or copy anything between them.

## Method

1. **List, don't read.** Start with a listing of unread messages — sender, subject, date. Only
   open a body when the subject is genuinely ambiguous. Mail bodies are the most expensive
   thing you can pull into context and most of them decide nothing.
2. **Sort into three buckets:**
   - **Needs a reply from Ivy** — a person asked a question or is waiting.
   - **Needs an action** — a deadline, a form, a booking, something with a date attached.
   - **Noise** — newsletters, notifications, receipts, automated course announcements.
3. **Report**, work account first:

   ```
   ## Work
   Needs a reply
   - <sender> — <subject> (<date>)
   Needs an action
   - <sender> — <subject> — <what and by when>
   Noise: <n> messages

   ## Personal
   …
   ```

## Turning mail into work

Only when Ivy asks:

- **A task** → Todoist, with the deadline from the mail. Say which mail it came from.
- **A calendar event** → Fantastical. Check for a conflict first with `findAvailableTimes`.
- **A reply** → *draft it and show it*. Do not send. Sending is a separate, explicit
  instruction — this is the one tool here with an audience outside this machine.

## Rules

- Never delete mail. Never mark read in bulk to "clean up".
- Never move a message between accounts or forward across them.
- Uncertain which account a request means? Ask. Do not check both and merge the answer.
