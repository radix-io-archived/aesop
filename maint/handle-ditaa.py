#!/usr/bin/env python
import sys
import re
import subprocess
import tempfile
import getopt
import os

header = re.compile("\[DITAA START\s+\((.*?)\)?\]")

def main(document, ditaaPath, imageDir):
    parsed = []
    parsing = False
    filename = ""
    worklist = {}
    for line in document:
        m = header.match(line)
        if m:
            flags = m.groups()
            if flags:
                flags = "[%s]" % flags
            else:
                flags = ""
            handle, filename = tempfile.mkstemp(
                suffix=".jpg", prefix="ditaa-figure-", dir=imageDir)
            os.close(handle)
            parsed.append("image:%s%s\n\n" % (filename, flags))
            parsing = True
            worklist[filename] = ""
        elif line.strip() == "[DITAA END]":
            fd, asciiname = tempfile.mkstemp(
		suffix = ".txt", prefix="asciiart-", dir="/tmp")
	    a = os.fdopen(fd, "w+b")
            a.write(worklist[filename])
            a.close()
            parsing = False
            job = subprocess.Popen(["java", "-jar", ditaaPath, "-o", asciiname, filename], stdout=subprocess.PIPE)
            job.wait()
            os.unlink(asciiname)
        elif parsing:
            worklist[filename] += line
        else:
            parsed.append(line)

    print "".join(parsed)

if __name__ == "__main__":

    imageDir = None
    ditaaPath = None

    options, args = getopt.getopt(sys.argv[1:], "d:j:")
    for o, v in options:
        if o == "-d":
            imageDir = v
        if o == "-j":
            ditaaPath = v

    if imageDir == None:
        print "usage: [-d <image directory>] [-j <path to ditaa jar>]\n"
        sys.exit(1)
    
    if len(args) == 1:
        document = open(args[0], "r").readlines()
    else:
        document = sys.stdin.readlines()
    main(document, ditaaPath, imageDir)

#
#./handle-ditaa.py $1 | asciidoc -d book -o $TARGET - 
