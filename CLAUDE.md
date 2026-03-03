# ssmbar - Social Security Microsimulation Benefit Calculator

An R package that calculates Social Security retirement benefits for hypothetical workers using the exact SSA formulas and parameters. Also includes a static GitHub Pages Benefit Explorer at https://anthonycolavito.github.io/ssmbar/.

**Owner**: Anthony Colavito (colavito@crfb.org) — Committee for a Responsible Federal Budget

---

## Required Reading

Before starting any work, read these companion files:

1. `.claude/skill.md` — Technical architecture, key functions, site dimensions, reform guide
2. `CLAUDE_GUIDELINES.md` — Behavioral rules (testing, source authority, autonomy, documentation)
3. `PROGRESS.md` (top ~50 lines) — Latest session log, what was done last

Before approaching any problem, search through PROGRESS.md and the above files to check if there is already a documented method or prior solution.

---

## Commit Workflow (Non-Negotiable)

Every commit must follow this exact sequence:

1. **Update `PROGRESS.md`** with a session log entry describing what changed — this goes in the same commit, not a separate one
2. **Run full test suite** (`devtools::test()`) immediately before committing — a test run from earlier in the session does not count
3. **Stage only files you actually changed** — do not use `git add -A` or `git add .`
4. **Write a commit message** that explains what changed and why

Failure to follow this sequence (e.g., committing without PROGRESS.md, relying on a stale test run, pushing and then updating PROGRESS.md as a second commit) violates project rules.

---

## Development Environment

**Mac:**
- R 4.0.3 at `/usr/local/bin/Rscript`
- ssmbar is NOT installed — use `devtools::load_all(".", quiet = TRUE)`
- 8 CPU cores available; use 6 for parallel work (`mclapply`)
- Kill zombie R processes before data generation: `ps aux | grep "R --no-echo" | grep -v grep`

**Windows (CRFB):**
- R 4.5.0 at `C:\Users\AnthonyColavito\AppData\Local\Programs\R\R-4.5.0\bin\Rscript.exe`

To run tests:
```bash
Rscript -e "devtools::test()"
```
