CREATE TABLE headers(
       id INTEGER PRIMARY KEY autoincrement,
       message_id TEXT UNIQUE NOT NULL,
       in_reply_to TEXT,
       mime_type TEXT,
       subject TEXT,
       date DATE);

CREATE INDEX idx_headers_message_id ON headers(message_id);

CREATE TABLE bodies(
       id INTEGER PRIMARY KEY autoincrement,
       original_content TEXT,
       sanitized_content TEXT,
       mime_type TEXT NOT NULL,
       charset TEXT,
       transfer_encoding TEXT,
       name TEXT,
       message_id TEXT NOT NULL,
       UNIQUE(mime_type, message_id));

CREATE INDEX idx_bodies_message_id ON bodies(message_id);

CREATE TABLE categories(
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       name TEXT UNIQUE);

CREATE TABLE category_training_preferences(
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       language TEXT NOT NULL UNIQUE,
       use_in_training BOOLEAN);

CREATE TABLE communications(
       id INTEGER PRIMARY KEY autoincrement,
       message_id TEXT REFERENCES headers(message_id),
       contact_key TEXT REFERENCES contacts(contact_key),
       type TEXT,
       UNIQUE(message_id, contact_key, type));

CREATE TABLE contacts(
       contact_key TEXT PRIMARY KEY,
       name TEXT,
       address TEXT);

CREATE TABLE metadata(
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       message_id UNIQUE REFERENCES headers(message_id),
       language TEXT,
       language_modified DATE,
       language_confidence REAL,
       category REFERENCES categories(id),
       category_modified DATE,
       category_confidence REAL);

CREATE INDEX idx_metadata_message_id ON metadata(message_id);


CREATE TABLE preferences(
       preference TEXT UNIQUE,
       value TEXT);
