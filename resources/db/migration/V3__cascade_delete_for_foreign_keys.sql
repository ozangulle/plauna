CREATE TABLE new_headers(
       message_id TEXT PRIMARY KEY,
       in_reply_to TEXT,
       mime_type TEXT,
       subject TEXT,
       date DATE);

INSERT INTO new_headers SELECT message_id, in_reply_to, mime_type, subject, date FROM headers;

DROP TABLE headers;

ALTER TABLE new_headers RENAME TO headers;

CREATE INDEX idx_headers_message_id ON headers(message_id);


CREATE TABLE new_bodies(
       id INTEGER PRIMARY KEY autoincrement,
       content TEXT,
       mime_type TEXT NOT NULL,
       charset TEXT,
       transfer_encoding TEXT,
       filename TEXT,
       content_disposition TEXT,
       message_id TEXT,
       FOREIGN KEY (message_id) REFERENCES headers(message_id) ON DELETE CASCADE,
       UNIQUE(mime_type, message_id, content));

INSERT INTO new_bodies SELECT id, content, mime_type, charset, transfer_encoding, filename, content_disposition, message_id FROM bodies;

DROP TABLE bodies;

ALTER TABLE new_bodies RENAME TO bodies;

CREATE INDEX idx_bodies_message_id ON bodies(message_id);


CREATE TABLE new_communications(
       id INTEGER PRIMARY KEY autoincrement,
       message_id TEXT REFERENCES headers(message_id) ON DELETE CASCADE,
       contact_key TEXT REFERENCES contacts(contact_key) ON DELETE CASCADE,
       type TEXT,
       UNIQUE(message_id, contact_key, type));

INSERT INTO new_communications SELECT id, message_id, contact_key, type FROM communications;

DROP TABLE communications;

ALTER TABLE new_communications RENAME TO communications;


CREATE TABLE new_metadata(
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       message_id UNIQUE REFERENCES headers(message_id) ON DELETE CASCADE,
       language TEXT,
       language_modified DATE,
       language_confidence REAL,
       category REFERENCES categories(id),
       category_modified DATE,
       category_confidence REAL);

INSERT INTO new_metadata SELECT id, message_id, language, language_modified, language_confidence, category, category_modified, category_confidence FROM metadata;

DROP TABLE metadata;

ALTER TABLE new_metadata RENAME TO metadata;

CREATE INDEX idx_metadata_message_id ON metadata(message_id);
