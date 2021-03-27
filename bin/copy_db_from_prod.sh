#!/usr/bin/env fish
set DB /tmp/tennis.sql
ssh mariandrive.com 'pg_dump tennis' > $DB
psql -c 'DROP DATABASE tennis'
psql -c 'CREATE DATABASE tennis'
psql -c 'GRANT ALL ON DATABASE tennis TO haskell'
psql tennis -f $DB

