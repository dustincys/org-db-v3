"""Tests for indexing API endpoints."""
import pytest
from fastapi.testclient import TestClient
from pathlib import Path

from org_db_server.main import app
from org_db_server.services.database import Database
from org_db_server.config import settings

@pytest.fixture
def client(tmp_path):
    """Create test client with temporary database."""
    # Override database path for testing
    test_db_path = tmp_path / "test.db"
    settings.db_path = test_db_path

    client = TestClient(app)
    yield client

def test_index_file_endpoint(client):
    """Test POST /api/index/file endpoint."""
    payload = {
        "filename": "/test/sample.org",
        "md5": "abc123",
        "file_size": 1024,
        "headlines": [
            {
                "title": "Test Heading",
                "level": 1,
                "todo_keyword": "TODO",
                "tags": ":test:",
                "begin": 10,
                "end": 50
            }
        ],
        "links": [],
        "keywords": [
            {"key": "TITLE", "value": "Test", "begin": 0}
        ],
        "src_blocks": []
    }

    response = client.post("/api/index/file", json=payload)

    assert response.status_code == 200
    data = response.json()
    assert "file_id" in data
    assert data["status"] == "indexed"
