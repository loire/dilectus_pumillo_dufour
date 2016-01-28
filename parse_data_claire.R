require(dplyr)
require(tidyr)
require(ggplot2)


dat = read.table("data_claire.csv",sep=",",header=T)
head(dat)
dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% select(indiv) %>% unique %>% dim
dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% select(NS_name_mission) %>% unique %>% dim


png("data_claire_plot.png",res = 300, units = 'cm',width=30,height=30)
dat %>% mutate(date=as.Date(date,format ="%m/%d/%Y")) %>% filter(nuit>4) %>% ggplot() + geom_point(aes(x=x,y=y,color=seqtot,shape=geography)) + facet_wrap(~ site_mission,scale="free") + theme_bw() + theme(text = element_text(family="Times",size=14),axis.text.x = element_text(angle =90))

dev.off()
