# -*- coding: utf-8 -*-
"""
Created on Wed Jun 10 2015

@author: gquinn
"""

import os
import sys
import subprocess
import random

print 'Number of arguments:', len(sys.argv), 'arguments.'
print 'Argument List:', str(sys.argv)

print(sys.argv[1])
print(sys.argv[2])
print(sys.argv[3])
print(len(sys.argv))

if not os.path.exists(sys.argv[2]):
    os.makedirs(sys.argv[2])

Rprogram=sys.argv[2]+".r"

for i in range(int(sys.argv[1])):
    rs=str(int(100000*random.random()))
    plist=[]
    plist.append('Rscript')
    plist.append(Rprogram)
    plist.append(rs)
    for j in range(3,len(sys.argv)):
        plist.append(sys.argv[j])
    print(plist)
    rc = subprocess.call(plist)
