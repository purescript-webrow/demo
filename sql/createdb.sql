BEGIN TRANSACTION;

CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS citext;

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'email') THEN
    CREATE DOMAIN EMAIL AS citext
      CHECK ( value ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$' );
  END IF;
END$$;

CREATE TABLE IF NOT EXISTS Account (
  created TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  email EMAIL UNIQUE,
  hashedPassword TEXT,
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  isAdmin BOOLEAN default False,
  salt TEXT NULL,
);

