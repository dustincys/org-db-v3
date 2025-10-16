# Remaining Cleanup Tasks

## Untracked Files That Need Action

### 1. Utility Scripts (Decision Needed)

**Benchmarking scripts:**
- `python/benchmark_search.py`
- `python/benchmark_semantic_search.py`
- `python/profile_search.py`
- `python/debug_search_performance.py`

**Migration/Optimization scripts:**
- `python/migrate_to_f32_blob.py`
- `python/optimize_database.py`
- `python/implement_file_level_embeddings.py`

**Options:**
- **A) Keep as-is** - Add them to git untracked (useful for development)
- **B) Move to `python/scripts/`** - Organize but keep accessible
- **C) Delete** - Remove if no longer needed
- **D) Add to examples/** - If they serve as usage examples

**Recommendation:** Create `python/scripts/` directory:
```
python/scripts/
├── benchmarks/
│   ├── benchmark_search.py
│   └── benchmark_semantic_search.py
├── profiling/
│   ├── debug_search_performance.py
│   └── profile_search.py
└── migrations/
    ├── migrate_to_f32_blob.py
    ├── optimize_database.py
    └── implement_file_level_embeddings.py
```

### 2. Schema Files (Should Be Tracked)

These look like they're part of the codebase:
- `python/org_db_server/models/image_schema.py` - Image DB schema
- `python/org_db_server/models/semantic_schema.py` - Semantic DB schema

**Action:** Add to git (these are source code, not temporary files)

### 3. Python README (Should Be Tracked)

- `python/README.md` - Python package documentation

**Issues:**
- References old docs paths (`docs/MULTI_DATABASE_ARCHITECTURE.md` instead of `../docs/architecture/multi-database.md`)
- License says "Part of the scimax project" instead of MIT

**Action:**
- Update documentation links
- Update license reference
- Add to git

### 4. Examples Output (Should Be Ignored)

- `examples/.ob-jupyter/` - Jupyter notebook output
- `examples/screenshots/` - Generated screenshots

**Already in .gitignore** ✓

### 5. Examples File

- `examples/graphs.org` - Example file

**Question:** Is this a test file or an example to include in the repo?
- If example: Add to git
- If test output: Add to .gitignore or delete

## Files That Should Be Updated

### 1. `python/README.md`

**Broken documentation links:**
```markdown
# Current (broken):
- [docs/MULTI_DATABASE_ARCHITECTURE.md](docs/MULTI_DATABASE_ARCHITECTURE.md)
- [docs/ANN_SEARCH_BUG_FIX.md](docs/ANN_SEARCH_BUG_FIX.md)

# Should be:
- [../docs/architecture/multi-database.md](../docs/architecture/multi-database.md)
- [../docs/performance/ann-search-optimization.md](../docs/performance/ann-search-optimization.md)
```

**License reference:**
```markdown
# Current:
Part of the scimax project.

# Should be:
MIT License - See [LICENSE](../LICENSE) for details.
```

### 2. `.gitignore`

**Remove temporary cleanup review:**
```
CLEANUP_REVIEW.md  # Remove this line after cleanup complete
```

## Repository Structure Issues

### Missing Standard Files

1. **CONTRIBUTING.md** - How to contribute
   - Setup instructions
   - Code style
   - PR process
   - Issue reporting

2. **.github/** directory
   - `ISSUE_TEMPLATE/bug_report.md`
   - `ISSUE_TEMPLATE/feature_request.md`
   - `workflows/tests.yml` (CI)

3. **CHANGELOG.md** - Version history
   - Track major changes
   - Release notes

### Python Package Issues

**`python/pyproject.toml` is minimal:**
- Missing: author, keywords, classifiers, repository URL
- Missing: optional dependencies groups
- Missing: entry points for CLI tools (if any)

## Recommended Actions (Priority Order)

### High Priority (Before Publishing to GitHub)

1. ✅ **Add schema files to git**
   ```bash
   git add python/org_db_server/models/image_schema.py
   git add python/org_db_server/models/semantic_schema.py
   ```

2. ✅ **Fix and add python/README.md**
   - Update documentation links
   - Fix license reference
   - Add to git

3. ✅ **Organize utility scripts**
   - Create `python/scripts/` structure
   - Move scripts there
   - Add to git (or .gitignore if not needed)

4. ✅ **Handle examples/graphs.org**
   - Decide: keep or ignore
   - Either add to git or .gitignore

5. ✅ **Clean .gitignore**
   - Remove `CLEANUP_REVIEW.md` line

### Medium Priority (Nice to Have)

6. ⏳ **Add CONTRIBUTING.md**
   - Development setup
   - Contribution guidelines

7. ⏳ **Improve python/pyproject.toml**
   - Add metadata
   - Add repository URL

8. ⏳ **Add .github/ templates**
   - Issue templates
   - PR template

### Low Priority (Can Do Later)

9. ⏳ **Add CHANGELOG.md**
10. ⏳ **Add GitHub Actions CI**
11. ⏳ **Add badges to README**

## Decision Points

Please decide on:

1. **Utility scripts** - Keep, organize, or delete?
2. **examples/graphs.org** - Include in repo or ignore?
3. **CONTRIBUTING.md** - Add now or later?
4. **.github/ templates** - Add now or later?

## Summary

**Must do before GitHub:**
- Add schema files (2 files)
- Fix python/README.md (broken links, license)
- Organize or remove utility scripts (7 files)
- Handle examples/graphs.org (1 file)
- Clean .gitignore (1 line)

**Total: ~11 files + 1 update to address**
