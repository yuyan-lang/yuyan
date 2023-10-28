import os
import subprocess

# Define the directory to search for files in
directory = "./豫言编译器"

# Loop through all files in the directory
for filename in os.listdir(directory):
    # Check if the file ends with "。豫"
    if filename.endswith("。豫"):
        # Open the file for reading and writing
        with open(os.path.join(directory, filename), "r+") as file:
            # Read all lines from the file
            lines = file.readlines()
            # Loop through all lines in the file
            for i in range(len(lines)):
                # Check if the line starts with "寻观"
                if lines[i].startswith("寻观"):
                    # Remove the line from the list of lines
                    del lines[i]
                    # Write the modified list of lines back to the file
                    file.seek(0)
                    file.writelines(lines)
                    file.truncate()
                    # Run the file through "./yy <file_path> --type-check-only"
                    result = subprocess.run(["./yy", os.path.join(directory, filename), "--type-check-only"], capture_output=True)
                    # Check if the program type checks
                    if result.returncode == 0:
                        # If the program type checks, continue to the next line
                        break
                    else:
                        # If the program does not type check, add the line back to the list of lines
                        lines.insert(i, lines[i])
                        # Write the original list of lines back to the file
                        file.seek(0)
                        file.writelines(lines)
                        file.truncate()