import codecs

input_file = 'builtin.opt.ll'
output_file = 'output.txt'

with open(input_file, 'rb') as infile:
    with open(output_file, 'wb') as outfile:
        while True:
            c = infile.read(1)
            if not c:
                break
            if c == b'\\':
                hex_str = infile.read(2)
                if len(hex_str) == 2 and all(c in b'0123456789ABCDEFabcdef' for c in hex_str):
                    byte_val = int(hex_str, 16)
                    outfile.write(bytes([byte_val]))
                else:
                    outfile.write(c + hex_str)
            else:
                outfile.write(c)