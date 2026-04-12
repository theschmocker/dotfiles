#!/usr/bin/env python3
"""
Adds the

[tool.pyright]
venvPath = "."
venv = ".venv"

block to a pyproject.toml file. Found this config somewhere online while
figuring out how to get pyright to work with dependencies installed with uv
"""

with open("./pyproject.toml", mode="r+") as pyproject:
    for line in pyproject.readlines():
        if line.startswith("[tool.pyright]"):
            print("Pyright already configured.")
            exit(0)

    pyproject.write("""

[tool.pyright]
venvPath = "."
venv = ".venv"
    """.rstrip())
print("pyright configured")
