#!/bin/bash

# Input file
INPUT_FILE="/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/doc/EIGHTBOL.texi"
OUTPUT_DIR="/home/brpocock/Projects/Phantasia/SkylineTool/eightbol/doc/chapters"

# Get all chapter lines
CHAPTER_LINES=$(grep -n "@chapter" "$INPUT_FILE" | cut -d: -f1)
# Add the end line (total lines + 1) for the last chapter boundary
TOTAL_LINES=$(wc -l < "$INPUT_FILE")
CHAPTER_LINES="$CHAPTER_LINES
$((TOTAL_LINES + 1))"

# Process each chapter
PREV_LINE=0
CHAPTER_NUM=0

echo "$CHAPTER_LINES" | while read LINE; do
    if [ -n "$LINE" ] && [ "$LINE" -gt "$PREV_LINE" ]; then
        CHAPTER_NUM=$((CHAPTER_NUM + 1))
        
        # Get chapter name from the line
        CHAPTER_LINE=$(sed -n "${PREV_LINE}p" "$INPUT_FILE" 2>/dev/null || echo "")
        if [ -n "$CHAPTER_LINE" ]; then
            # Extract chapter name and clean it for filename
            CHAPTER_NAME=$(echo "$CHAPTER_LINE" | sed -e 's/@chapter //' -e 's/[^a-zA-Z0-9]/_/g' -e 's/_\+/_/g' -e 's/^_//' -e 's/_$//' | tr '[:upper:]' '[:lower:]')
            
            # If chapter name is empty, use a default
            if [ -z "$CHAPTER_NAME" ]; then
                CHAPTER_NAME="chapter_${CHAPTER_NUM}"
            fi
            
            # Calculate start and end lines
            START_LINE=$((PREV_LINE + 1))
            END_LINE=$((LINE - 1))
            
            # Extract the chapter content
            if [ "$START_LINE" -le "$END_LINE" ]; then
                sed -n "${START_LINE},${END_LINE}p" "$INPUT_FILE" > "${OUTPUT_DIR}/${CHAPTER_NAME}.texi"
                echo "Created: ${OUTPUT_DIR}/${CHAPTER_NAME}.texi (lines ${START_LINE}-${END_LINE})"
            fi
        fi
        
        PREV_LINE=$LINE
    fi
done

# Handle the last chapter if needed
if [ $CHAPTER_NUM -gt 0 ]; then
    echo "Processed $CHAPTER_NUM chapters"
fi
