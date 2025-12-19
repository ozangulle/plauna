CREATE TABLE auth_providers(
       id INTEGER PRIMARY KEY autoincrement,
       name TEXT NOT NULL,
       auth_url TEXT NOT NULL,
       token_url TEXT NOT NULL,
       redirect_url TEXT NOT NULL,
       client_id TEXT NOT NULL,
       client_secret TEXT NOT NULL,
       scope TEXT NOT NULL);
