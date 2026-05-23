#!/usr/bin/env bash
# Kill all running podman containers

set -euo pipefail

containers=$(podman ps -q)

if [ -z "$containers" ]; then
    echo "No running podman containers found."
    exit 0
fi

echo "Killing containers: $containers"
podman kill $containers
echo "Done."
