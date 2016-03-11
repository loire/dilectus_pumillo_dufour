require(ggplot2)
require(dplyr)
require(tidyr)
require(xtable)

mytheme = theme_bw() +  theme(text = element_text(family="Times",size=14),axis.text.x = element_text(angle =45,hjust = 1))

data = read.table("data_distance.txt",header=T)
glimpse(data)

data$distance = as.numeric(data$distance)

# Make a graph ! 

pdf("distance_indiv_versus_site.pdf",height = 20, width=20)
ggplot(data) + geom_jitter(width = 0.5, size= 2, alpha = 0.6, aes(x=seqtot,y=distance,color=seqtot,shape=geography)) + facet_wrap(~ site_mission,scale="free_x") + mytheme + scale_x_discrete(name="Species") + scale_color_discrete(name="Species")  + scale_shape_discrete(name="Geography",labels=c("Allopatric","Site","Sympatric"))
dev.off()

# Launch wilcoxon test and print them on screen

test = data %>% group_by(site_mission) %>% do (res =  pairwise.wilcox.test((.)$distance,(.)$seqtot,paired=F)$p.value) 

for (i in 1:14) {print(test[i,]$site_mission) ; print(test[i,]$res[[1]])}





