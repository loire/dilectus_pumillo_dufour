require(dplyr)
require(tidyr)
require(ggplot2)


mytheme = theme_bw() +  theme(text = element_text(family="Times",size=14),axis.text.x = element_text(angle =45,hjust = 1))



dat = read.table("data_claire.csv",sep=",",header=T)
head(dat)

# Get number of individuals and number of site, when filtering for more than 4 observation per indiv : 

dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% select(indiv) %>% unique %>% dim
dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% select(NS_name_mission) %>% unique %>% dim


#Plot each nesting sites, in all mission and according to geographie and species
png("Individuals_nesting_sites.png",res = 300, units = 'cm',width=30,height=30)
dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% ggplot() + geom_point(aes(x=x,y=y,color=seqtot,shape=geography)) + facet_wrap(~ site_mission,scale="free")  + scale_color_discrete(name="species") + xlab("latitude") + ylab("longitude") + mytheme

dev.off()

# size of mission site: 
png("Size_of_mission_site.png",res=300, units = "cm", width=30, height = 15)
dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% group_by(site_mission) %>% summarise(area= (max(x) - min(x)) * (max(y) - min(y)) ) %>% .[order(.$area),] %>% mutate(site_mission = factor(site_mission,levels = site_mission[order(area)], ordered=T))   %>%  ggplot() + geom_bar(aes(x=site_mission,y=area),stat = "identity") + xlab("Site mission")  +  mytheme 
dev.off()



png("Number_of_nest_per_indivs.png",res=300, units = "cm", width=30, height = 15)
# Number of nests visited by each individuals: 
dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% regroup(list("site_mission", "indiv","sexe")) %>% summarise(n=n_distinct(NS_name_mission)) %>% ggplot() + geom_histogram(aes(x=n,fill = sexe),position = "dodge") + xlab("number of differents nests visited by individuals") + mytheme + theme(axis.text.x = element_text(angle = 0,hjust=0.5))
dev.off()




png("mean_distance_between_nests_visited_by_indivs",res=300, units = "cm", width=30, height = 15)
# Now we compute mean distances between nests for each indiv and plot the distribution, according to geography
dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% regroup(list("geography","site_mission", "indiv")) %>% summarise(d = mean(dist(cbind(x,y)))) %>% ggplot() + geom_histogram(aes(x=d,fill=geography),position="dodge") + mytheme + theme(axis.text.x = element_text(angle = 0,hjust=0.5)) + xlab("Mean distance between visited nests")
dev.off()



## Same but with a correction for area size 
#dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% group_by(site_mission) %>% summarise(area= (max(x) - min(x)) * (max(y) - min(y)) ) %>% inner_join(dat,by="site_mission") %>% regroup(list("geography","area","site_mission","indiv")) %>% summarise(d = mean(dist(cbind(x,y))) ) %>% mutate(dcor = d / area ) 
#

dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% regroup(list("site_mission","indiv")) %>% summarise(NS_dist = n_distinct(NS_name_mission)) %>% ggplot() + geom_histogram(aes(x=NS_dist)) + mytheme 


# Here we get the list, for each site and for each indivs, of all the nests (NS) (plotted previoulsy by sex)

tmp = dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% select(site_mission,indiv,NS_name_mission,x,y) %>% unique  %>% regroup(list("site_mission", "indiv")) 

# We write here a file to treat in python (dist_pair.py) because R is not practical for this task.

write.table(tmp, file = "NS_indiv_data.txt", sep =",",row.names=F,col.names=F,quote=F)

NSindivdat = read.table("distance_NS_pairwise_per_indiv.txt")
colnames(NSindivdat) = c("indiv","NS_name_mission","NS1/NS2","x1","y1","x2","y2")
head(NSindivdat)
NSindivdat %>% mutate(distance = dist(rbind(c(x1,x2),c(y1,y2)))) %>% head




