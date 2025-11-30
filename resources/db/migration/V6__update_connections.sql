CREATE TABLE new_connections(
       id TEXT PRIMARY KEY NOT NULL,
       host TEXT NOT NULL,
       user TEXT NOT NULL,
       secret TEXT NOT NULL,
       folder TEXT NOT NULL,
       debug BOOLEAN DEFAULT 0,
       port INTEGER ,
       security TEXT NOT NULL,
       check_ssl_certs BOOLEAN DEFAULT 1,
       auth_type TEXT,
       auth_provider INTEGER,
       FOREIGN KEY (auth_provider) REFERENCES auth_providers(id));

INSERT INTO new_connections SELECT id, host, user, secret, folder, debug, port, security, check_ssl_certs, NULL, NULL FROM connections;

DROP TABLE connections;

ALTER TABLE new_connections RENAME TO connections;
