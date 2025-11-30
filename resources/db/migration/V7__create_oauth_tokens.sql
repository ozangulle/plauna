CREATE TABLE oauth_tokens(
       id INTEGER PRIMARY KEY autoincrement,
       connection_id INTEGER NOT NULL,
       access_token TEXT NOT NULL,
       expires_in INTEGER,
       scope TEXT,
       token_type TEXT,
       refresh_token TEXT,
       FOREIGN KEY (connection_id) REFERENCES connections(id) ON DELETE CASCADE);
