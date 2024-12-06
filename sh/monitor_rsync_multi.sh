#!/bin/bash

# Define source directories (indexed array)
sources=(
    "/Users/zhou/Documents/projects/dogecoin"    # macOS source path
    "/home/zhou/Documents/projects/dogecoin"    # Linux source path
)

# Define target directories (indexed array, same order as sources)
targets=(
    "ubuntu@192.168.131.161:~/babycoin"         # Linux target
    "ubuntu@192.168.131.161:~/babycoin"         # macOS target
)

# rsync options
RSYNC_OPTS="-avh --progress"

# Function to check and install fswatch
install_fswatch() {
    if ! command -v fswatch &> /dev/null; then
        echo "fswatch not found. Installing..."
        if [[ "$OSTYPE" == "darwin"* ]]; then
            # macOS (Darwin)
            brew install fswatch
        elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
            # Linux (Debian/Ubuntu based)
            sudo apt update && sudo apt install fswatch -y
        fi
    fi
}

# Check if fswatch is installed
install_fswatch

# Function to monitor a source-target pair
monitor_pair() {
    local source="$1"
    local target="$2"

    # Ensure source exists
    if [ ! -d "$source" ]; then
        echo "Error: Source directory $source does not exist. Skipping..."
        return
    fi

    # Start monitoring the source directory
    echo "Monitoring $source for changes..."
    fswatch -0 "$source" | while read -d "" event; do
        # Get the relative path of the changed file
        relative_path="${event#$source/}"

        # Sync the changed file
        echo "Detected change in $event. Syncing to $target/$relative_path..."
        rsync $RSYNC_OPTS "$event" "$target/$relative_path"

        if [ $? -eq 0 ]; then
            echo "Sync completed for $event -> $target/$relative_path"
        else
            echo "Error during sync for $event -> $target/$relative_path"
        fi
    done
}

# Loop through each source-target pair
for i in "${!sources[@]}"; do
    source="${sources[$i]}"
    target="${targets[$i]}"
    monitor_pair "$source" "$target" &
done

# Wait for all background processes
wait
