#!/usr/bin/env python3

import subprocess
import os
import shutil
import sys

DOTFILES_PATH = os.path.abspath(os.path.dirname(__file__))
STOW_DIRS = [
    "scripts",
    "bash",
    "zsh",
    "nvim",
]

def main():
    ensure_stow_installed()
    ensure_stow_global_ignore()

    for dir in STOW_DIRS:
        result = subprocess.run(
            ["stow", dir],
            cwd=DOTFILES_PATH,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        if result.returncode == 0:
            print(f"✅ \"{dir}\" stowed successfully.")
        else:
            print(f"❌ \"{dir}\" stow error:")
            for line in result.stderr.splitlines():
                print(f"\t{line}")

def is_executable(name: str) -> bool:
    return shutil.which(name) is not None

def ensure_stow_installed():
    if is_executable("stow"):
        return

    print("'stow' executable not present.")
    
    if not is_executable("brew"):
        sys.exit(1)
    
    answer = input("Attempt to install it with Homebrew? [Y/n]")
    if answer == "" or answer.lower().startswith("y"):
        install_result = subprocess.run(["brew", "install", "stow"])
        if install_result.returncode != '0':
            print("Failed to install 'stow'.")
            sys.exit(1)

STOW_GLOBAL_IGNORE_PATH = os.path.expanduser("~/.stow-global-ignore")
def ensure_stow_global_ignore():
    if os.path.exists(STOW_GLOBAL_IGNORE_PATH):
        return
    with open(STOW_GLOBAL_IGNORE_PATH, mode="w") as stow_ignore:
        stow_ignore.write("\\.DS_Store\n")
    print("Created ~/.stow-global-ignore")

if __name__ == '__main__':
    main()
