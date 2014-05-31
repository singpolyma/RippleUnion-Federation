CREATE TABLE domains (
	domain TEXT NOT NULL PRIMARY KEY,
	pattern TEXT,
	proxy_uri TEXT
);

CREATE TABLE aliases (
	alias TEXT NOT NULL,
	domain TEXT NOT NULL,
	ripple TEXT NOT NULL,
	dt INTEGER
);
