# Claude Behavioral Guidelines for ssmbar

Rules governing how Claude interacts with the codebase and the user. These are non-negotiable unless explicitly revised by the project owner.

---

## 1. Testing Requirements

### General Testing
- Run the **full test suite after changes, before committing**
- New **exported functions require tests**
- Tests for internal helper functions are optional (covered indirectly through exported functions)

### Benefit Calculation Functions (Elevated Standard)

Any function in the core computation pipeline requires exhaustive verification:

```
Earnings → AIME → PIA → COLA → Actuarial Adjustment → Final Benefit
```

For these functions, Claude must:

1. **Run validation scripts** (not just unit tests)
2. **Verify every intermediate step** of the computation
3. **Manual inspection** of code logic
4. **Unit tests** against expected values
5. **Validation against external sources**: Table V.C7, Actuarial Notes, hand calculations

Do not commit changes to benefit calculation functions until all five verification steps are complete.

---

## 2. Source Authority

### Hierarchy for Benefit Computation Rules

```
Statute (Title II, 42 USC 401-434) — GOVERNS
         ↓
Regulations (20 CFR 404) — clarifies statute
         ↓
POMS — operational detail
         ↓
SSA Publications — examples and explanations
```

**The statute controls.** If a regulation, POMS section, or publication appears to conflict with statutory text, flag it for discussion.

### Authority for Parameter Values

SSA publications are authoritative for:
- AWI series (Trustees Report)
- Bend points (annual SSA announcement)
- Scaled earnings factors (Actuarial Notes)
- COLA factors (Trustees Report)
- Taxable maximum (Trustees Report)

### Validation Targets

When testing hypothetical workers, SSA's published figures are the benchmark:
- Table V.C7 (Trustees Report)
- Actuarial Note tables
- SSA Fact Sheets

### Handling Statutory Ambiguity

When statute is ambiguous:

1. **Consult secondary sources** (regulations, POMS) to resolve
2. **Document explicitly** that secondary sources were consulted and how they resolved the ambiguity
3. **If ambiguity persists**: STOP. Flag for user. Do not proceed with assumptions.

Never make judgment calls on ambiguous statutory interpretation.

---

## 3. Scope of Autonomy

### Refactoring
- **Permitted** without prior permission
- **Must document** in commit message: what was refactored and why
- Tests must pass before and after

### Bug Fixes: Two Categories

**Coding errors** (typos, wrong variable, off-by-one, NA instead of 0):
- Fix and document
- No permission needed

**Benefit computation rule errors** (wrong number of years, wrong formula, wrong reduction factor):
- Do **NOT** fix
- Report to user
- Explain the discrepancy
- Discuss before any changes

*The distinction*: A coding bug is "the code doesn't do what it's trying to do." A rule bug is "the code does what it's trying to do, but what it's trying to do is wrong."

### User Directions
- **Verify all claims** the user makes
- **Verify all directions** the user gives
- If the user is correct, proceed
- If the user is wrong, **explain why** and **propose alternatives**
- Do **not** be obsequious
- Do **not** assume the user is pursuing the correct course of action
- Apply rigorous standards to all ideas and respectfully disagree when necessary

---

## 4. Documentation Rules

### Existing Documentation
- **Retain ALL** existing SSA documentation when making changes
- **Editing**: Not permitted without user permission. Explain proposed change and wait.
- **Removing**: Never permitted without explicit user permission

### Adding New Documentation

**Non-statutory comments** (code mechanics that don't implicate benefit rules):
- Permitted freely

**Statutory-implicating comments** (any comment describing benefit calculation behavior):
- Must use `# SUGGESTED:` prefix
- Must include `# TODO: verify against statute` flag

Format:
```r
# SUGGESTED: Computation period uses 35 highest earning years per 42 USC 415(b)(2)(B)(ii)
# TODO: verify against statute
```

---

## 5. Communication Standards

### Uncertainty
- **Convey doubts** when they arise
- **Discuss uncertainties** before proceeding
- If unsure whether a rule applies, ask

### Disagreement
- Push back when the user is wrong
- Explain reasoning clearly
- Propose concrete alternatives
- Maintain this stance even if the user insists — correctness matters more than agreement

---

## 6. File Operations
- **Never delete files** without explicit user permission
- **Ask permission** when uncertain about any action

---

## 7. Quick Reference: What Requires Permission?

| Action | Permission Needed? |
|--------|-------------------|
| Commit after tests pass | No |
| Refactor (behavior unchanged) | No, but document |
| Fix coding bug | No |
| Fix benefit rule bug | **Yes — discuss first** |
| Add non-statutory comment | No |
| Add statutory-implicating comment | No, but flag as SUGGESTED/TODO |
| Edit existing documentation | **Yes** |
| Remove documentation | **Yes** |
| Delete files | **Yes** |
| Proceed despite statutory ambiguity | **Yes — must discuss** |
