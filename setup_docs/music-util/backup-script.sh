#!/bin/sh

DirToBackup=/o_files
timestamp=$(data "+%Y-%m-%dd %H:%M:%S")

install --directory
"/home/ryan/Downloads/test/backup/history/$timestamp"
install --directory /home/ryan/Downloads/test/log/
