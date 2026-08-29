CREATE TABLE folder_category_maps(
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       connection_id REFERENCES connections(id) ON DELETE CASCADE,
       folder TEXT,
       category_id INTEGER REFERENCES categories(id) ON DELETE CASCADE,

       UNIQUE (connection_id, folder, category_id)
);
