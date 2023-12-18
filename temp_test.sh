#!/bin/bash

# Check if the argument is provided
if [ -z "$1" ]; then
    echo "Please provide the file path as an argument."
    exit 1
fi



# Separate the extension from the file name
directory=$(dirname "$1")
filename=$(basename -- "$1")
extension="${filename##*.}"
filename="${filename%.*}"

echo "directory: $directory"
echo "Filename without extension: $filename"

# Delete the specified files if they exist
rm -f main.ll a.out main.s

# Run the Lua command, redirecting output and error to respective files
lua5.1 main.lua < "$directory/$filename.txt" > main.ll 2> "main.err"

# Check if the error file has content
if [ -s "main.err" ]; then
    echo "Error file has content. Exiting."
    exit 1
fi

# Verify if the file exists and check its content
if [ -f "main.ll" ]; then
    # Read the first line of the file
    first_line=$(head -n 1 "main.ll")

    # Check if the first line starts with "syntax error near"
    if [[ "$first_line" == "syntax error near"* ]]; then
        echo "Syntax error. Exiting."
        cat "main.ll"
        rm -f "main.err" "main.res"
        exit 1
    else
        echo "No syntax error detected."
    fi
else
    echo "File main.ll not found."
    exit 1
fi


echo "No errors found."

# Perform subsequent commands
llc-15 main.ll -opaque-pointers
clang-15 -no-pie main.s
./a.out > main.res