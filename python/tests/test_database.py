"""Tests for database operations."""
import pytest
from pathlib import Path
from org_db_server.services.database import Database

@pytest.fixture
def temp_db(tmp_path):
    """Create a temporary database for testing."""
    db_path = tmp_path / "test.db"
    db = Database(db_path)
    yield db
    db.close()

def test_database_initialization(temp_db):
    """Test that database initializes with correct schema."""
    # Check that files table exists
    cursor = temp_db.conn.cursor()
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='files'")
    assert cursor.fetchone() is not None

    # Check that headlines table exists
    cursor.execute("SELECT name FROM sqlite_master WHERE type='table' AND name='headlines'")
    assert cursor.fetchone() is not None

def test_get_or_create_file_id(temp_db):
    """Test getting or creating file IDs."""
    # Create new file
    file_id1 = temp_db.get_or_create_file_id("/test/file.org", "abc123", 1024)
    assert file_id1 > 0

    # Get existing file
    file_id2 = temp_db.get_or_create_file_id("/test/file.org", "abc123", 1024)
    assert file_id1 == file_id2

    # Create different file
    file_id3 = temp_db.get_or_create_file_id("/test/other.org", "def456", 2048)
    assert file_id3 != file_id1
