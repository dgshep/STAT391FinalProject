#!/usr/bin/env python
# encoding: utf-8
"""
untitled.py

Created by Davis Shepherd on 2012-05-24.
Copyright (c) 2012 University of Washington. All rights reserved.
"""

import sys
import getopt
import struct

help_message = '''
The help message goes here.
'''


class Usage(Exception):
    def __init__(self, msg):
        self.msg = msg


def main(argv=None):
    if argv is None:
        argv = sys.argv
    try:
        try:
            opts, args = getopt.getopt(argv[1:], "i:ho:v", ["help", "output="])
        except getopt.error, msg:
            raise Usage(msg)
    
        # option processing
        images = -1
        for option, value in opts:
            if option == "-v":
                verbose = True
            if option in ("-h", "--help"):
                raise Usage(help_message)
            if option in ("-o", "--output"):
                output = value
            if option == "-i":
                images = int(value)
    
    except Usage, err:
        print >> sys.stderr, sys.argv[0].split("/")[-1] + ": " + str(err.msg)
        print >> sys.stderr, "\t for help use --help"
        return 2
        
    imageFile = open("/home/maeday/STAT 391/FinalProject/STAT391FinalProject/train-images-idx3-ubyte", "rb");
    out = open("/home/maeday/STAT 391/FinalProject/STAT391FinalProject/train-images-idx3-ubyte_proc", "wb");
    magicNumber = struct.unpack(">i", imageFile.read(4))[0]
    records = struct.unpack(">i", imageFile.read(4))[0]
    rows = struct.unpack(">i", imageFile.read(4))[0]
    columns = struct.unpack(">i", imageFile.read(4))[0]
    data = []
    if images == -1:
        images = records
    print images
    for i in range(images):
        if(i % (images/5) == 0):
            print (str(i*100.0/images) + "%")
        row = ""
        for j in range(rows*columns):
            row = row + (" " + str(struct.unpack(">B", imageFile.read(1))[0]))
        out.write(row + "\n")
    #out.write(str(data))
        


    


if __name__ == "__main__":
    sys.exit(main())
