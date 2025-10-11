"""Database service for org-db."""
import sqlite3
from pathlib import Path
from typing import Optional
from datetime import datetime

from org_db_server.models.db_models import SCHEMA

class Database:
    """Database connection and operations."""

    def __init__(self, db_path: Path):
        """Initialize database connection and create schema if needed."""
        self.db_path = db_path
        self.conn = sqlite3.connect(str(db_path), check_same_thread=False)
        self.conn.row_factory = sqlite3.Row

        # Enable foreign keys
        self.conn.execute("PRAGMA foreign_keys = ON")

        # Initialize schema
        self._initialize_schema()

    def _initialize_schema(self):
        """Create all tables if they don't exist."""
        cursor = self.conn.cursor()
        cursor.executescript(SCHEMA)
        self.conn.commit()

    def close(self):
        """Close database connection."""
        if self.conn:
            self.conn.close()

    def get_or_create_file_id(self, filename: str, md5: str, file_size: int) -> int:
        """Get file ID or create new file entry."""
        cursor = self.conn.cursor()

        # Try to get existing file
        cursor.execute("SELECT rowid FROM files WHERE filename = ?", (filename,))
        row = cursor.fetchone()

        if row:
            # Update existing file
            cursor.execute(
                """UPDATE files SET md5 = ?, last_updated = ?, file_size = ?, indexed_at = ?
                   WHERE rowid = ?""",
                (md5, datetime.now().isoformat(), file_size, datetime.now().isoformat(), row[0])
            )
            self.conn.commit()
            return row[0]
        else:
            # Create new file
            cursor.execute(
                """INSERT INTO files (filename, md5, last_updated, file_size, indexed_at)
                   VALUES (?, ?, ?, ?, ?)""",
                (filename, md5, datetime.now().isoformat(), file_size, datetime.now().isoformat())
            )
            self.conn.commit()
            return cursor.lastrowid
