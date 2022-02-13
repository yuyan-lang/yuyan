
from time import strptime
# read type `S = {l1: t1, l2: t2}` from stdin, write to stdout

def cap(s:str):
    return s[:1].upper() + s[1:]
import sys
if __name__ == "__main__":
    nameeqtp = sys.stdin.read()

    [tpname, recordtype] = [s.strip() for s in nameeqtp.split("=")]
    stripped = recordtype.strip("{}")
    fields = [[s.strip() for s in s.split(":")] for s in stripped.split(",")]

    res = []

    for [name, tp] in fields:
        # generate the getter and setter
        res.append(
            "    fun get" + cap(name) +"(recd : " + tpname + ") = #" + name + " recd\n" + \
            "    fun set" + cap(name) +"(recd : " + tpname + ") (newValue : " + tp + ") = {" + \
                    ",".join([cname + "=" + 
                    (" #" + cname + " recd" if name != cname else "newValue")
                    for [cname, ctp] in fields]) + "}\n"
        )

    print ("\n".join(res))
        

    

