#!/bin/bash
# Generate SVG diagrams from PlantUML files

# Check if plantuml is installed
if ! command -v plantuml &> /dev/null; then
    echo "PlantUML is not installed. Please install it first:"
    echo "  macOS: brew install plantuml"
    echo "  Ubuntu: apt-get install plantuml"
    echo "  Or download from: https://plantuml.com/download"
    exit 1
fi

# Get the directory of this script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Generate SVG for each PlantUML file
echo "Generating architecture diagrams..."

for puml_file in "$SCRIPT_DIR"/*.puml; do
    if [ -f "$puml_file" ]; then
        echo "Processing: $(basename "$puml_file")"
        plantuml -tsvg "$puml_file"
    fi
done

echo "Done! SVG files generated in: $SCRIPT_DIR"

# List generated files
echo ""
echo "Generated diagrams:"
ls -la "$SCRIPT_DIR"/*.svg 2>/dev/null || echo "No SVG files found. There may have been an error."
