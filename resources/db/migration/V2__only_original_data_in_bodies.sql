ALTER TABLE bodies
DROP COLUMN sanitized_content;

ALTER TABLE bodies
RENAME COLUMN original_content TO content;

ALTER TABLE bodies
RENAME COLUMN name TO filename;

ALTER TABLE bodies
ADD content_disposition TEXT; 


CREATE TABLE new_bodies(
       id INTEGER PRIMARY KEY autoincrement,
       content TEXT,
       mime_type TEXT NOT NULL,
       charset TEXT,
       transfer_encoding TEXT,
       filename TEXT,
       content_disposition TEXT,
       message_id TEXT NOT NULL,
       UNIQUE(mime_type, message_id, content));

INSERT INTO new_bodies SELECT id, content, mime_type, charset, transfer_encoding, filename, content_disposition, message_id FROM bodies;

DROP TABLE bodies;

ALTER TABLE new_bodies RENAME TO bodies;

CREATE INDEX idx_bodies_message_id ON bodies(message_id);
