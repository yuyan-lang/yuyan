#!/usr/local/bin/python3

import sys
import os
import time
f = open ("pylog", 'w')
time.sleep(1)
# print("Content-Length: ", len(""""{jsonrpc":"2.0","id":0,"result":{"capabilities":{"textDocumentSync":{"openClose":true, "change":1}}}}"""))
# print("")
sys.stderr.write(""""{jsonrpc":"2.0","id":0,"result":{"capabilities":{"textDocumentSync":{"openClose":true, "change":1}}}}""")
l = input()
l = input()
for i in range(4081):
    l = sys.stdin.read(1)

    # f.write(str(i))
    # f.write(":")
    # f.write("{}".format(l))
    # f.write("\n")
    f.write(l)
    f.flush()
print(""""{jsonrpc":"2.0","id":0,"result":{"capabilities":{"textDocumentSync":{"openClose":true, "change":1}}}}""")

time.sleep(10)

os.path.dirname()