"""Pydantic models for API requests/responses."""
from typing import List, Optional, Dict, Any
from pydantic import BaseModel, Field

class HeadlineData(BaseModel):
    """Headline data from Emacs."""
    title: str
    level: int
    todo_keyword: Optional[str] = None
    todo_type: Optional[str] = None
    archivedp: Optional[bool] = None
    commentedp: Optional[bool] = None
    begin: int
    end: Optional[int] = None
    tags: Optional[str] = None
    priority: Optional[str] = None
    scheduled: Optional[str] = None
    deadline: Optional[str] = None
    properties: Optional[Dict[str, str]] = None

class LinkData(BaseModel):
    """Link data from Emacs."""
    type: str
    path: str
    raw_link: str
    description: Optional[str] = None
    search_option: Optional[str] = None
    begin: int

class KeywordData(BaseModel):
    """Keyword data from Emacs."""
    key: str
    value: str
    begin: int

class SrcBlockData(BaseModel):
    """Source block data from Emacs."""
    language: str
    contents: str
    begin: int

class IndexFileRequest(BaseModel):
    """Request to index a file."""
    filename: str
    md5: str
    file_size: int
    headlines: List[HeadlineData] = Field(default_factory=list)
    links: List[LinkData] = Field(default_factory=list)
    keywords: List[KeywordData] = Field(default_factory=list)
    src_blocks: List[SrcBlockData] = Field(default_factory=list)

class IndexFileResponse(BaseModel):
    """Response from indexing a file."""
    file_id: int
    status: str
    headlines_count: int
    links_count: int
