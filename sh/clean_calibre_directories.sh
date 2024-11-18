
#!/bin/bash

dst="$1"

if [[ "$dst" == "" ]]; then
	dst="."
fi

# Function to check if a directory is empty
is_empty_dir() {
  [ -z "$(ls -A "$1")" ]
}

# Find all directories
find "$dst" -type d | while read -r dir; do
  # Count the number of files in the directory
  file_count=$(find "$dir" -maxdepth 1 -type f | wc -l)

  # Check if there's exactly one file and it's an .opf file
  if [ "$file_count" -eq 1 ] && [ -f "$dir"/*.opf ]; then
    echo "Removing directory with only .opf file: $dir"
    rm -rf "$dir"
  elif is_empty_dir "$dir"; then
    echo "Removing empty directory: $dir"
    rmdir "$dir"
  fi
done
