import os
import subprocess

# Define the directory to search for files in
directory = "./豫言编译器"

# Loop through all files in the directory and its subdirectories
for root, dirs, files in os.walk(directory):
    for filename in files:
        # Check if the file ends with "。豫"
        if filename.endswith("。豫"):
            # Open the file for reading and writing
            with open(os.path.join(root, filename), "r+") as file:
                # Read all lines from the file
                lines = file.readlines()
                # Loop through all lines in the file in reverse order
                for i in reversed(range(len(lines))):
                    # Check if the line starts with "寻观"
                    if lines[i].startswith("寻观"):
                        # Remove the line from the list of lines
                        deleted_line = lines[i]
                        del lines[i]
                        # Write the modified list of lines back to the file
                        file.seek(0)
                        file.writelines(lines)
                        file.truncate()
                        # Print the deleted line and the file currently being processed
                        print(f"Deleted line: {deleted_line.strip()} from file: {os.path.join(root, filename)}")
                        # Run the file through "./yy <file_path> --type-check-only"
                        result = subprocess.run(["./yy", os.path.join(root, filename), "--type-check-only"], capture_output=True)
                        # Check if the program type checks
                        if result.returncode == 0:
                            # If the program type checks, continue to the next line
                            continue
                        else:
                            # If the program does not type check, add the line back to the list of lines
                            lines.insert(i, deleted_line)
                            # Write the original list of lines back to the file
                            file.seek(0)
                            file.writelines(lines)
                            file.truncate()