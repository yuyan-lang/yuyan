
import sys
import os
import subprocess

if len(sys.argv) != 3:
    print("Usage: python copy_or_generate_debug.py <source> <destination>")
    sys.exit(1)

source = sys.argv[1]
destination = sys.argv[2]

def is_tool(name):
    """Check whether `name` is on PATH and marked as executable."""

    # from whichcraft import which
    from shutil import which

    return which(name) is not None

qat_available = is_tool("qat")
qat_available = False # for now, qat doesn't seem to work

# If qat is available, run it with -S flag and pipe the output to the destination file
if qat_available:
    try:
        with open(destination, 'wb') as dest_file:
            subprocess.run(["qat", "-S", source], check=True, stdout=dest_file)
    except subprocess.CalledProcessError as e:
        print(f"Error running qat: {e}")
        sys.exit(1)
else:
    # If qat is not available, simply copy the input to the output
    try:
        with open(source, 'rb') as src_file, open(destination, 'wb') as dest_file:
            dest_file.write(src_file.read())
    except Exception as e:
        print(f"Error copying file: {e}")
        sys.exit(1)

print("Operation completed successfully.")