#!/bin/sh

set -x

## createdb -T template0 -E UTF-8 -l ja_JP.UTF-8 onsendev

psql -c 'DROP SCHEMA IF EXISTS DOUBUTSU CASCADE' onsendev

psql onsendev < backend.sql
