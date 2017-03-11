#!/bin/sh

set -x

## createdb -T template0 -E UTF-8 -l ja_JP.UTF-8 onsentest

psql -c 'DROP SCHEMA IF EXISTS DOUBUTSU CASCADE' onsentest

psql onsentest < backend.sql
