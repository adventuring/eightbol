# MARKDOWN CONSOLIDATION AND REPOSITORY CLEANUP PLAN

**Date:** September 9, 2026  
**Status:** Draft - Ready for Implementation  
**Total Markdown Files:** 66  
**Total Lines:** ~24,000+

---

## EXECUTIVE SUMMARY

This plan consolidates 66 Markdown files (18,000+ lines in root + 8,000+ in doc/) into a structured, navigable documentation suite with:

- **Primary reference:** `doc/PROGRAMMERS-REFERENCE.texi` (consolidated guide for developers)
- **Research archive:** `doc/archive/` (historical analysis, completed audits, research findings)
- **Active guides:** Quick references, error handling, frontend documentation in `doc/`
- **Clean root:** Only README.md and AGENTS.md remain at project root
- **Organized plans:** Frontend task plans preserved in `frontend_plans/` for transparency

---

## INVENTORY BY CATEGORY

### 1. AGENT GUIDELINES (67 lines) — KEEP AT ROOT
- **File:** `AGENTS.md`
- **Action:** Keep at root (provides essential project context for development agents)
- **Reason:** Referenced by OpenCode agent system and provides critical context

### 2. PRIMARY DOCUMENTATION (124 lines) — KEEP AT ROOT
- **File:** `README.md`
- **Action:** Keep at root (standard project entry point)
- **Reason:** GitHub standard; users expect this at root

### 3. QUICK REFERENCE GUIDES (1,454 lines) — CONSOLIDATE TO REFERENCE
- **Files:**
  - `FRONTEND_QUICK_REFERENCE.md` (622 lines)
  - `FRONTEND_SUPPORT_QUICK_REFERENCE.md` (312 lines)
  - `QUICK-REFERENCE-ERROR-HANDLING.md` (520 lines)
- **Action:** Integrate into `doc/PROGRAMMERS-REFERENCE.texi` as chapters:
  - Chapter: "Frontend Architecture Pipeline"
  - Chapter: "Frontend Language Support Matrix"
  - Chapter: "Error Handling Quick Reference"
- **Reason:** These are essential developer resources that should be in the reference guide

### 4. FRONTEND GUIDES (567 lines) — CONSOLIDATE
- **Files:**
  - `FRONTEND_DECLARATION_GUIDE.md` (297 lines)
  - `MUDDLE_README.md` (270 lines)
- **Action:** 
  - `FRONTEND_DECLARATION_GUIDE.md` → `doc/PROGRAMMERS-REFERENCE.texi` chapter
  - `MUDDLE_README.md` → Move to `doc/guides/MUDDLE-LANGUAGE.texi`
- **Reason:** Core developer guidance

### 5. AUDIT REPORTS (7,882 lines) — ARCHIVE
- **Files:** 14 files including comprehensive audits
- **Action:** Move all to `doc/archive/audits/` with index file
- **Reason:** Historical audit data; useful for reference but not daily development
- **Example files:**
  - AST audits (3 files)
  - Backend verification audits (5 files)
  - Frontend architecture audits (4 files)
  - Error handling audits (3 files)
- **Create:** `doc/archive/audits/INDEX.md` linking all audit reports

### 6. STATUS/COMPLETION REPORTS (3,560 lines) — ARCHIVE
- **Files:** 10 reports (Backend verification, completion reports, compliance summaries)
- **Action:** Move to `doc/archive/completion-reports/`
- **Reason:** Historical project status; reference only
- **Create:** Index file for easy reference

### 7. ANALYSIS DOCUMENTS (1,650 lines) — SELECTIVE
- **Files:** COBOL AST coverage, conformance status, gaps, variable erasure, verification index
- **Action:**
  - `COBOL_AST_COVERAGE_ANALYSIS.md` → Content merged into PROGRAMMERS-REFERENCE
  - `CONFORMANCE_STATUS.md` → Archive (status snapshot)
  - `TEST_COVERAGE_GAPS.md` → Archive
  - `VARIABLE-ERASURE-IMPLEMENTATION.md` → Archive
  - `VERIFICATION_INDEX.md` → Create `doc/archive/VERIFICATION-INDEX.md`

### 8. TASK LISTS & PLANS (1,758 lines) — SELECTIVE
- **Files:** AST redesign, doc coverage checklist, frontend completion plan, MUDDLE fixes, plan.md
- **Action:**
  - `FRONTEND_COMPLETION_PLAN.md` → `doc/archive/planning/`
  - `MUDDLE_FIXES_CHECKLIST.md` → `doc/archive/planning/`
  - `AST_REDESIGN_PLAN.md` → `doc/archive/planning/`
  - `DOC_COVERAGE_CHECKLIST.md` → `doc/archive/planning/`
  - `plan.md` → Delete (appears to be working notes)
- **Reason:** Planning documents; archive once completed

### 9. RESEARCH DOCUMENTS (4,445 lines) — ARCHIVE
- **Root level:** `MUDDLE_RESEARCH.md` (979 lines)
- **In doc/chapters/:** 5 research audits (3,309 lines)
- **In doc/:** 3 research findings (1,136 lines)
- **Action:** Consolidate into `doc/archive/research/`
  - Create `RESEARCH-INDEX.md` with links to:
    - Language research audits (SCUMM, SmallTalk, BurgerMistress, Goal, Lingo)
    - Implementation research (Muddle)
    - Interview summaries
- **Reason:** Valuable for architecture decisions; not daily development

### 10. TECHNICAL GUIDES (786 lines) — SELECTIVE CONSOLIDATE
- **Files:**
  - `doc/BACKEND-ERROR-HANDLING.md` (416 lines)
  - `src/frontend-forth/README.md` (370 lines)
- **Action:**
  - Backend error handling → `doc/PROGRAMMERS-REFERENCE.texi` chapter
  - Forth frontend guide → Remains at `src/frontend-forth/README.md` (keep with code)
- **Reason:** Error handling is critical for all developers; Forth README stays with implementation

### 11. FRONTEND TASK PLANS (698 lines) — KEEP ORGANIZED
- **Location:** `frontend_plans/` directory (14 files, ~50 lines each)
- **Action:** 
  - Keep in place for transparency
  - Create `frontend_plans/README.md` summarizing status of each frontend
  - Link from PROGRAMMERS-REFERENCE.texi
- **Reason:** These represent implementation work to be done; valuable for planning

---

## NEW DIRECTORY STRUCTURE

```
doc/
├── EIGHTBOL.texi                    (Master TeXinfo doc - exists)
├── PROGRAMMERS-REFERENCE.texi       (NEW: Consolidated developer reference)
├── README.md                         (NEW: Quick navigation guide)
├── introduction.info                (exists)
├── guides/
│   ├── MUDDLE-LANGUAGE.texi         (NEW: From MUDDLE_README.md)
│   ├── FORTH-LANGUAGE.texi          (Future)
│   └── ...
├── archive/
│   ├── README.md                     (Index of all archives)
│   ├── audits/
│   │   ├── INDEX.md
│   │   ├── AST-COVERAGE-AUDIT.md
│   │   ├── BACKEND-VERIFICATION.md
│   │   ├── FRONTEND-ARCHITECTURE-AUDIT.md
│   │   ├── FRONTEND-AST-COVERAGE-AUDIT.md
│   │   └── ...
│   ├── completion-reports/
│   │   ├── INDEX.md
│   │   ├── BACKEND-VERIFICATION-2026.md
│   │   ├── BASIC-FRONTEND-COMPLETION.md
│   │   └── ...
│   ├── research/
│   │   ├── README.md
│   │   ├── RESEARCH-INDEX.md
│   │   ├── LANGUAGE-RESEARCH/
│   │   │   ├── SCUMM-RESEARCH.md
│   │   │   ├── SMALLTALK-RESEARCH.md
│   │   │   ├── BURGERMISTRESS-RESEARCH.md
│   │   │   ├── GOAL-RESEARCH.md
│   │   │   └── LINGO-RESEARCH.md
│   │   └── IMPLEMENTATION-RESEARCH/
│   │       └── MUDDLE-RESEARCH.md
│   └── planning/
│       ├── INDEX.md
│       ├── FRONTEND-COMPLETION-PLAN.md
│       ├── AST-REDESIGN-PLAN.md
│       └── ...
└── chapters/                         (existing research chapter files moved to archive)
```

---

## MIGRATION MATRIX

| Current File | Lines | Category | New Location | Action |
|---|---|---|---|---|
| AGENTS.md | 67 | Guidelines | /root | Keep |
| README.md | 124 | Primary | /root | Keep |
| FRONTEND_QUICK_REFERENCE.md | 622 | Reference | doc/PROGRAMMERS-REFERENCE.texi | Integrate chapter |
| FRONTEND_SUPPORT_QUICK_REFERENCE.md | 312 | Reference | doc/PROGRAMMERS-REFERENCE.texi | Integrate chapter |
| QUICK-REFERENCE-ERROR-HANDLING.md | 520 | Reference | doc/PROGRAMMERS-REFERENCE.texi | Integrate chapter |
| FRONTEND_DECLARATION_GUIDE.md | 297 | Guide | doc/PROGRAMMERS-REFERENCE.texi | Integrate chapter |
| MUDDLE_README.md | 270 | Guide | doc/guides/MUDDLE-LANGUAGE.texi | Convert & move |
| AST_AUDIT_README.md | 267 | Audit | doc/archive/audits/ | Archive |
| AST_COVERAGE_AUDIT_REPORT.md | 374 | Audit | doc/archive/audits/ | Archive |
| AUDIT-ERROR-HANDLING-INDEX.md | 418 | Audit | doc/archive/audits/ | Archive |
| AUDIT-ERROR-HANDLING.md | 812 | Audit | doc/archive/audits/ | Archive |
| AUDIT-ERROR-HANDLING-SUMMARY.md | 420 | Audit | doc/archive/audits/ | Archive |
| AUDIT_FINDINGS_AND_RECOMMENDATIONS.md | 506 | Audit | doc/archive/audits/ | Archive |
| BACKEND_AST_AUDIT_FINDINGS.md | 344 | Audit | doc/archive/audits/ | Archive |
| BACKEND_AST_AUDIT.md | 639 | Audit | doc/archive/audits/ | Archive |
| BACKEND_AUDIT_README.md | 278 | Audit | doc/archive/audits/ | Archive |
| FRONTEND_ARCHITECTURE_AUDIT.md | 1058 | Audit | doc/archive/audits/ | Archive |
| FRONTEND_AST_COVERAGE_AUDIT_2026.md | 1155 | Audit | doc/archive/audits/ | Archive |
| FRONTEND_AUDIT_DETAILED.md | 686 | Audit | doc/archive/audits/ | Archive |
| TEST_COVERAGE_AUDIT_REPORT.md | 552 | Audit | doc/archive/audits/ | Archive |
| MASTER-VERIFICATION-AUDIT-REPORT.md | 373 | Audit | doc/archive/audits/ | Archive |
| BACKEND_VERIFICATION_REPORT_2026.md | 466 | Report | doc/archive/completion-reports/ | Archive |
| BASIC_FRONTEND_COMPLETION_REPORT.md | 256 | Report | doc/archive/completion-reports/ | Archive |
| COBOL_COMPLETE_AST_COVERAGE_REPORT.md | 332 | Report | doc/archive/completion-reports/ | Archive |
| DOCUMENTATION_COVERAGE_REPORT.md | 324 | Report | doc/archive/completion-reports/ | Archive |
| PARALLEL-CORRECTION-COMPLETION-REPORT.md | 413 | Report | doc/archive/completion-reports/ | Archive |
| TEST_COVERAGE_VERIFICATION_REPORT.md | 443 | Report | doc/archive/completion-reports/ | Archive |
| ERROR-HANDLING-VERIFICATION.md | 438 | Report | doc/archive/completion-reports/ | Archive |
| FINAL_LANGUAGE_VALIDATION.md | 261 | Report | doc/archive/completion-reports/ | Archive |
| TEST-COMPLIANCE-SUMMARY.md | 248 | Report | doc/archive/completion-reports/ | Archive |
| INTEGRATION_SUMMARY.md | 379 | Report | doc/archive/completion-reports/ | Archive |
| COBOL_AST_COVERAGE_ANALYSIS.md | 597 | Analysis | doc/PROGRAMMERS-REFERENCE.texi (integrate) / archive | Selective |
| CONFORMANCE_STATUS.md | 203 | Analysis | doc/archive/analysis/ | Archive |
| TEST_COVERAGE_GAPS.md | 283 | Analysis | doc/archive/analysis/ | Archive |
| VARIABLE-ERASURE-IMPLEMENTATION.md | 316 | Analysis | doc/archive/analysis/ | Archive |
| VERIFICATION_INDEX.md | 251 | Analysis | doc/archive/VERIFICATION-INDEX.md | Archive |
| AST_REDESIGN_PLAN.md | 364 | Planning | doc/archive/planning/ | Archive |
| DOC_COVERAGE_CHECKLIST.md | 308 | Planning | doc/archive/planning/ | Archive |
| FRONTEND_COMPLETION_PLAN.md | 567 | Planning | doc/archive/planning/ | Archive |
| MUDDLE_FIXES_CHECKLIST.md | 203 | Planning | doc/archive/planning/ | Archive |
| plan.md | 316 | Planning | DELETE | Delete (working notes) |
| MUDDLE_RESEARCH.md | 979 | Research | doc/archive/research/ | Archive |
| doc/BACKEND-ERROR-HANDLING.md | 416 | Guide | doc/PROGRAMMERS-REFERENCE.texi | Integrate chapter |
| doc/BURGERMISTRESS_FINDINGS.md | 193 | Research | doc/archive/research/ | Archive |
| doc/LANGUAGE_INTERVIEW_SUMMARY.md | 251 | Research | doc/archive/research/ | Archive |
| doc/lingo_research_audit.md | 692 | Research | doc/archive/research/ | Archive |
| doc/chapters/*.md (5 files) | 3309 | Research | doc/archive/research/ | Archive |
| src/frontend-forth/README.md | 370 | Guide | Keep in place | Keep |
| frontend_plans/*.md (14 files) | 698 | Plans | Keep in place + update | Keep + organize |

---

## IMPLEMENTATION CHECKLIST

- [ ] Create doc/archive/ directory structure
- [ ] Create doc/guides/ directory
- [ ] Create PROGRAMMERS-REFERENCE.texi framework
- [ ] Migrate quick reference content to PROGRAMMERS-REFERENCE.texi
- [ ] Migrate frontend guides to PROGRAMMERS-REFERENCE.texi
- [ ] Migrate error handling guide to PROGRAMMERS-REFERENCE.texi
- [ ] Create archive index files
- [ ] Move audit reports to doc/archive/audits/
- [ ] Move completion reports to doc/archive/completion-reports/
- [ ] Move analysis documents to doc/archive/analysis/
- [ ] Move planning documents to doc/archive/planning/
- [ ] Move research documents to doc/archive/research/
- [ ] Convert and move MUDDLE_README.md to doc/guides/
- [ ] Create doc/archive/research/RESEARCH-INDEX.md
- [ ] Create doc/README.md (navigation guide)
- [ ] Create frontend_plans/README.md (status summary)
- [ ] Update .gitignore if needed
- [ ] Verify TeXinfo compilation
- [ ] Delete plan.md
- [ ] Clean up root directory - remove old Markdown files
- [ ] Update cross-references in EIGHTBOL.texi
- [ ] Final verification and testing

---

## CONTENT NOT LOST

All existing content is preserved by either:
1. **Remaining at root** (AGENTS.md, README.md)
2. **Integrated into PROGRAMMERS-REFERENCE.texi** (quick references, guides, error handling)
3. **Archived in doc/archive/** with full index structure (audits, reports, research, planning)
4. **Kept in place** (frontend plans, Forth README)

No content will be deleted except `plan.md` (working notes).

---

## SUCCESS CRITERIA

✅ Root directory contains only: AGENTS.md, README.md, and essential project files  
✅ PROGRAMMERS-REFERENCE.texi contains all critical developer guides  
✅ doc/archive/ contains all historical and reference documentation with indexes  
✅ All links and cross-references updated  
✅ TeXinfo documents compile without errors  
✅ Total markdown files in root reduced from 43 to 2  
✅ Documentation is organized, navigable, and maintainable
