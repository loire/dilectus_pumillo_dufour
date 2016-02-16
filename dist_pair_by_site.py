#!/usr/bin/python
import itertools
from math import sqrt

# Here is a simple script to get all combination of two nesting site (NS) visited by each indiv
# Results is save in 
dat_site = {}
f = open("NS_site_data.txt")
for line in f:
#    print  line
    c = line.split(",")
    site = c[0]
    NS = c[1]
    x = int(c[2])
    y = int(c[3])
    if not site in dat_site:
        dat_site[site] = [(NS,x,y)]
    else:
        dat_site[site].append((NS,x,y))

def euclide_dist(x1,y1,x2,y2):
    return sqrt((x1-x2)**2 + (y2-y1)**2)


for i in  dat_site:
    for l in list(itertools.combinations(dat_site[i],2)):     
            print i,str(l[0][0])+"/"+str(l[1][0]),l[0][1],l[0][2],l[1][1],l[1][2],euclide_dist(l[0][1],l[0][2],l[1][1],l[1][2])

