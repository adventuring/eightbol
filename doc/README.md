# Documentation Navigation Guide

This guide helps you find the right documentation for your needs in EIGHTBOL.

## Quick Navigation

### For Daily Development
- **PROGRAMMERS-REFERENCE.texi** — Start here! Quick reference, API docs, error handling, best practices
- **EIGHTBOL.texi** — Complete language and backend reference
- **../AGENTS.md** — Development guidelines and project conventions

### For Language and Backend Details
- **EIGHTBOL.texi** — Master documentation for all supported languages
  - Each language has a dedicated chapter (COBOL, BASIC, Forth, etc.)
  - Each processor has an ABI chapter (6502, Z80, m68k, etc.)
  - Abstract Syntax Tree (AST) chapter
  - Backend architecture chapter

### For Historical Context and Research
- **archive/README.md** — Overview of archived documentation
- **archive/audits/** — Comprehensive project audits
  - Frontend architecture audits
  - Backend verification audits
  - AST coverage audits
  - Error handling audits
- **archive/research/** — Language research and implementation studies
  - Language-specific research (SCUMM, SmallTalk, BurgerMistress, etc.)
  - Implementation research (Muddle)
  - Interview summaries
- **archive/completion-reports/** — Project status snapshots
- **archive/analysis/** — Technical analysis documents
- **archive/planning/** — Implementation plans and checklists

### For Frontend Development
- **PROGRAMMERS-REFERENCE.texi** → "Frontend Architecture" chapter
- **../frontend_plans/** — Language implementation task lists
- **EIGHTBOL.texi** → Individual language chapters

### For Backend Development
- **PROGRAMMERS-REFERENCE.texi** → "Backend Reference" chapter
- **EIGHTBOL.texi** → Backend and ABI chapters

### For Error Handling
- **PROGRAMMERS-REFERENCE.texi** → "Error Handling Guide" chapter
- **archive/audits/AUDIT-ERROR-HANDLING*.md** — Detailed error handling audits

### For Testing
- **PROGRAMMERS-REFERENCE.texi** → "Testing and Verification" chapter
- **EIGHTBOL.texi** → Testing chapter

---

## Content Organization

### Root-Level Documentation

- **../README.md** — Project overview and usage instructions
- **../AGENTS.md** — Agent guidelines and development conventions
- **PROGRAMMERS-REFERENCE.texi** — Developer quick reference (you are here!)

### Complete Reference

- **EIGHTBOL.texi** — Master TeXinfo documentation (1000+ pages)
  - Introduction and compilation pipeline
  - All language frontends (17 chapters)
  - AST reference
  - All backends and ABIs (16+ chapters)
  - Language conformance details

### Guides

- **guides/** — Language-specific guides and technical documentation
  - MUDDLE-LANGUAGE.texi (MUDDLE language guide)
  - Forth guide (in src/frontend-forth/README.md)

### Archive

- **archive/audits/** (15 files)
  - Frontend audits (4 files)
  - Backend audits (4 files)
  - AST audits (3 files)
  - Error handling audits (3 files)
  - Test coverage audit (1 file)

- **archive/completion-reports/** (11 files)
  - Verification reports
  - Completion reports
  - Compliance summaries

- **archive/analysis/** (5 files)
  - Coverage analysis
  - Conformance status
  - Test gaps
  - Implementation analysis

- **archive/planning/** (4 files)
  - Frontend completion plan
  - AST redesign plan
  - Documentation checklist
  - MUDDLE fixes checklist

- **archive/research/** (9 files)
  - Language research (7 files)
  - Implementation research (1 file)
  - Interview summaries (1 file)

---

## Reading Paths

### New Developer Getting Started

1. Read **../README.md** — Project overview
2. Read **../AGENTS.md** — Development conventions
3. Read **PROGRAMMERS-REFERENCE.texi** — Developer reference
4. Explore specific chapters in **EIGHTBOL.texi** for your area
5. Check **archive/** for background on your topic

### Contributing to a Specific Language

1. Check **archive/planning/FRONTEND_COMPLETION_PLAN.md** — Implementation status
2. Review **../frontend_plans/{language}_task.md** — Specific tasks
3. Examine language chapter in **EIGHTBOL.texi** — Language reference
4. Review language research in **archive/research/language-research/** — Background
5. Check related error handling docs in **archive/audits/**

### Working on Backend Code Generation

1. Read **PROGRAMMERS-REFERENCE.texi** → "Backend Reference" chapter
2. Examine processor ABI chapter in **EIGHTBOL.texi**
3. Review **archive/audits/BACKEND_AST_AUDIT*.md** — Backend requirements
4. Check **archive/analysis/CONFORMANCE_STATUS.md** — Processor status

### Fixing Error Handling

1. Read **PROGRAMMERS-REFERENCE.texi** → "Error Handling" chapter
2. Review **archive/audits/AUDIT-ERROR-HANDLING.md** — Complete error handling audit
3. Check **archive/audits/AUDIT-ERROR-HANDLING-SUMMARY.md** — Quick summary
4. See **doc/BACKEND-ERROR-HANDLING.md** — Backend-specific error handling

### Understanding Project History

1. Start with **archive/README.md**
2. Review **archive/audits/MASTER-VERIFICATION-AUDIT-REPORT.md** — Overall verification
3. Check **archive/completion-reports/BACKEND_VERIFICATION_REPORT_2026.md** — Latest status
4. Explore **archive/research/README.md** — Language research context

---

## Key Concepts Location

### Compilation Pipeline
- **PROGRAMMERS-REFERENCE.texi** → "Compilation Pipeline" chapter
- **EIGHTBOL.texi** → "Compilation Pipeline" chapter
- **../README.md** → "Pipeline" section

### AST (Abstract Syntax Tree)
- **PROGRAMMERS-REFERENCE.texi** → "AST Reference" chapter
- **EIGHTBOL.texi** → "Abstract Syntax Tree" chapter
- **archive/audits/AST_*.md** — AST audits and coverage

### Error Handling and Restart Patterns
- **PROGRAMMERS-REFERENCE.texi** → "Error Handling" chapter
- **doc/BACKEND-ERROR-HANDLING.md** → Backend-specific errors
- **archive/audits/AUDIT-ERROR-HANDLING*.md** — Complete error specifications

### Code Style and Conventions
- **../AGENTS.md** → "Format / Style" section
- **PROGRAMMERS-REFERENCE.texi** → "Development Guidelines" chapter

### Testing
- **PROGRAMMERS-REFERENCE.texi** → "Testing and Verification" chapter
- **EIGHTBOL.texi** → "Testing" chapter
- **archive/completion-reports/TEST_*.md** — Test status

### Language Support Status
- **PROGRAMMERS-REFERENCE.texi** → "Frontend Language Support" chapter
- **archive/completion-reports/COBOL_COMPLETE_AST_COVERAGE_REPORT.md**
- **archive/analysis/CONFORMANCE_STATUS.md**

---

## Tips

- **Bookmark PROGRAMMERS-REFERENCE.texi** — Your daily reference
- **Use archive/INDEX.md files** — Each subdirectory has an index
- **Cross-reference EIGHTBOL.texi** — Go there for deeper details
- **Check archive/** before asking — Your question may be answered
- **Keep README.md and AGENTS.md at root** — Permanent project references

---

**Last updated:** September 9, 2026  
**Total documentation:** ~30,000 lines across 60+ files  
**Status:** Consolidated and organized for easy navigation
