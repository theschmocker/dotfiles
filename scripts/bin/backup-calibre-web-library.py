#!/usr/bin/env python3

# Backup script for the Calibre Library managed by calibre-web on my home pi server

import os
import subprocess
import sys
from datetime import datetime
from typing import Literal
import argparse

REMOTE_USER = "schmo"
REMOTE_HOST = "pi-4-new"
REMOTE_DIR = "/mnt/blue_sandisk/CalibreLibrary/"
STORAGE_DIR = os.path.expanduser("~/calibre-web-backups")
CALIBRE_LIB_DIR_NAME = "CalibreLibrary"
CURRENT_CALIBRE_LIB_DIR = os.path.join(STORAGE_DIR, CALIBRE_LIB_DIR_NAME)
BACKUPS_DIR = os.path.join(STORAGE_DIR, "backups")

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-f", "--force-archive", action="store_true", help="Create a backup archive, even if there weren't any changes")
    args = parser.parse_args()
    create_dir_if_not_exists(STORAGE_DIR)

    print(f"Syncing {REMOTE_USER}@{REMOTE_HOST}:{REMOTE_DIR}...")
    sync_result = sync_calibre_library()

    if sync_result == "synced":
        print("Sync complete.")
        print("Creating archive...")
        archive_path = create_library_archive()
        print(f"Created archive at {archive_path}")
    elif sync_result == "up-to-date":
        print("Already up to date.")
        if args.force_archive:
            print("Creating archive (because of --force-archive)...")
            archive_path = create_library_archive()
            print(f"Created archive at {archive_path}")
    else:
        print("Sync failed.")

def create_dir_if_not_exists(path: str):
    """Create directory if it doesn't exist"""
    if not os.path.exists(path):
        os.makedirs(path, mode=0o755)

def sync_calibre_library() -> Literal["synced", "up-to-date", "error"]:
    cmd = [
        "rsync",
        "-rlDz",
        "--itemize-changes",
        "--checksum",
        "--ignore-times",
        "--delete",
        "--exclude=.DS_Store",
        f"{REMOTE_USER}@{REMOTE_HOST}:{REMOTE_DIR}",
        CURRENT_CALIBRE_LIB_DIR
    ]

    try:
        result = subprocess.run(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            text=True,
            check=False
        )

        for line in result.stdout.splitlines():
            if not line.startswith("."):
                print(line)

        if result.returncode != 0:
            return "error"

        # http://www.staroceans.org/e-book/understanding-the-output-of-rsync-itemize-changes.html
        has_changes = any(not line.startswith('.') for line in result.stdout.splitlines())
        return "synced" if has_changes else "up-to-date"

    except Exception as e:
        print(f"Sync failed: {e}", file=sys.stderr)
        return "error"

def create_library_archive() -> str:
    """Create compressed archive of CalibreLibrary. Returns the absolute path to the newly-created archive."""
    timestamp = datetime.now().strftime("%Y-%m-%d_%s")
    archive_name = f"CalibreLibrary-{timestamp}.tar.gz"
    archive_path = os.path.join(BACKUPS_DIR, archive_name)

    create_dir_if_not_exists(BACKUPS_DIR)

    cmd = [
        "tar",
        "-cz",
        "--no-mac-metadata",
        "--exclude=.DS_Store",
        "-f", archive_path,
        "-C", STORAGE_DIR,
        CALIBRE_LIB_DIR_NAME,
    ]

    try:
        subprocess.run(cmd, check=True)
        return archive_path
    except subprocess.CalledProcessError as e:
        print("Failed to archive.", file=sys.stderr)
        print(e.stderr, file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
