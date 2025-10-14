# Local Development Database

This directory contains the local development database for org-db-v3.

## Purpose

When working in the org-db-v3 project directory, the `.dir-locals.el` configuration automatically:

- Stores the database in `db/org-db-v3.db` (instead of `~/org-db/org-db-v3.db`)
- Uses port 8766 (instead of default 8765) to avoid conflicts with your global instance
- Enables debug mode for development

## Files

- `org-db-v3.db` - SQLite database (auto-generated, gitignored)
- `org-db-v3.db-shm` - Shared memory file (auto-generated, gitignored)
- `org-db-v3.db-wal` - Write-ahead log (auto-generated, gitignored)

## Usage

Simply open any file in the org-db-v3 project, and the local settings will be applied automatically.

You can verify the settings with:
```elisp
(getenv "ORG_DB_DB_PATH")  ; Should show path to db/org-db-v3.db
org-db-v3-server-port       ; Should be 8766
```

## Cleanup

To reset the local database, simply delete the files in this directory:
```bash
rm -f db/*.db*
```

The database will be recreated automatically the next time the server starts.
