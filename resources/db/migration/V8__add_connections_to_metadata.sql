CREATE TABLE new_metadata(
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       message_id UNIQUE REFERENCES headers(message_id),
       language TEXT,
       language_modified DATE,
       language_confidence REAL,
       category REFERENCES categories(id),
       category_modified DATE,
       category_confidence REAL,
       connection_id TEXT);

INSERT INTO new_metadata SELECT id, message_id, language, language_modified, language_confidence, category, category_modified, category_confidence, NULL FROM metadata;

DROP TABLE metadata;

ALTER TABLE new_metadata RENAME TO metadata;

