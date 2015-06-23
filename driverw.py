###############################################################################################
# -*- coding: utf-8 -*-
# driverw.py    python driver code for R simulations - Windows
#
# Usage:
#
# python driver.py niter program parm1 parm2 .....  parmk
#
#  where:
#
#     niter     is the number of iterations to run
#     program   is the name of the R program (minus the .r extension)
#     parm1     the first parameter passed to the R program
#     parm2     the second parameter passed to the R program
#    
#     parmk     the kth parameter passed to the R program
#
# save commands in the R programs will reference a subdirectory whose name matches the program name (minus the .r extension)
#
# 6/16/2015  Added shell=True to subprocess statement that calls the R program (needed for windows)
#
################################################################################################
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
    rc = subprocess.call(plist, shell=True)
