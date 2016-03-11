#!/usr/bin/python
import sys
from numpy import array
from scipy import stats
from numpy.random import choice
from scipy.stats import mannwhitneyu
from scipy.stats import ranksums
from numpy import concatenate
from numpy import ravel
IndivFile = open("distance_NS_pairwise_per_indiv_all_dat.txt",'r')
IndivFile.readline()
# First get all data in adequat structure (dictionnaries)
InfoIndiv={}
ObsDistSiteSpecies={}
IndivNightBySite = {}
for line in IndivFile:
    c = line.split(",")
    indiv = c[0]
    INNS = int(c[1])
    night = int(c[2])
    site = c[3]
    species = c[4]
    geo = c[6]
    dist = float(c[-1])
    # Store info for each individual
    if not indiv in InfoIndiv:
        InfoIndiv[indiv]=[INNS,night,site,species,geo,[dist]]
    else:
        InfoIndiv[indiv][-1].append(dist)
    # Store distribution of NS distance for all individuals of a species for an habitat
    SiteSpecies = site+"__"+species
    if not SiteSpecies in ObsDistSiteSpecies:
        ObsDistSiteSpecies[SiteSpecies]=[dist]
    else:
        ObsDistSiteSpecies[SiteSpecies].append(dist)
    # Store number of night of each individuals for a species and an habitat (useful for further random sampling
    if not SiteSpecies in IndivNightBySite:
        IndivNightBySite[SiteSpecies]=[(indiv,night)]
    else:
        if not (indiv,night) in IndivNightBySite[SiteSpecies]:
            IndivNightBySite[SiteSpecies].append((indiv,night))


# Print data for visual check
for i in IndivNightBySite:
    print i, IndivNightBySite[i]
    IndivNightBySite[i] = array(INdivNightBySite[i])

for i in ObsDistSiteSpecies:
    print i, ObsDistSiteSpecies[i]

# Now we get all distances between NS in each site:

SiteFile = open("distance_NS_pairwise_per_site_all_dat.txt",'r')
SiteNS = {}
SiteFile.readline()
for line in SiteFile:
    c = line.split(",")
    site = c[0]
    distance = float(c[-1])
    if not site in SiteNS:
        SiteNS[site]=[distance]
    else:
        SiteNS[site].append(distance)

for site in SiteNS:
    print site, SiteNS[site]
    SiteNS[site]=array(SiteNS[site])


# now, look at each observed distribution and generate a random one:


for SiteSpecies in IndivNightBySite:
    c = SiteSpecies.split("__")
    site = c[0]
    sp = c[1]
    observed = ObsDistSiteSpecies[SiteSpecies]
    if len(observed)>20:
        print SiteSpecies, len(observed),len(SiteNS[site])
        print len(observed),len(choice(SiteNS[site],len(observed)))
        print mannwhitneyu(choice(SiteNS[site],len(observed)),observed)

#    for night in IndivNightBySite[site]:
#        print numpy.concatenate(expected,choice(SiteNS[site],night[1],replace="T"))
#    break
#











