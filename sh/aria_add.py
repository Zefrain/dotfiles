#!/usr/local/bin/python

import os
import sys
import getopt
import xmlrpclib

s = xmlrpclib.ServerProxy('http://localhost:6800/rpc')


def help():
    print sys.argv[0], '-i <inputurl> -o <outputfile>'


def main(argv):
    inputfile = argv[1]
    outputfile = os.getcwd()
    try:
        opts, args = getopt.getopt(argv, "hi:o:", ["iurl=", "ofile="])
    except getopt.GetoptError:
        help()
        sys.exit(2)
    for opt, arg in opts:
        if opt == '-h':
            help()
            sys.exit()
        elif opt in ("-i", "--ifile"):
            inputfile = arg
        elif opt in ("-o", "--ofile"):
            outputfile = arg
    print("s.aria2.addUri(['" + inputfile + "'], dict(dir='" + outputfile +
          "'))")

    try:
        s.aria2.addUri([inputfile], dict(dir=outputfile))  #
    except Exception as e:
        print(e)


if __name__ == "__main__":
    main(sys.argv[1:])
