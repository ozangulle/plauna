CREATE TABLE connections(
       id TEXT PRIMARY KEY NOT NULL,
       host TEXT NOT NULL,
       user TEXT NOT NULL,
       secret TEXT NOT NULL,
       folder TEXT NOT NULL,
       debug BOOLEAN DEFAULT 0,
       port INTEGER ,
       security TEXT NOT NULL,
       check_ssl_certs BOOLEAN DEFAULT 1);
