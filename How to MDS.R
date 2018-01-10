#How to: MDS 
#By: Kristen Brown

#Required packages
library(vegan)
library(dplyr)
library(car)
library(ggplot2)
library(ggrepel)

#Load in your data
yourdata <- read.csv("/Users/imkri/Desktop/PhD/Seasonal Monitoring Experiment/Analysis/Benthic Community Composition/All benthic transects R2.csv", strip.white=T)
head(yourdata)

colnames(yourdata)

#Here, I make sure that my factors are called factors. Because year and depth are numeric, they might be confused with data, I want them as factors.
yourdata %>% mutate(Site=factor(Site)) %>% mutate(Year=factor(Year)) %>% 
  mutate(Season=factor(Season)) %>% mutate(Depth=factor(Depth))

#Run a MDS. Because we don't want to include our factors or the environmental data in the analysis, I specify which columns with ,(#:#) to analyze. 
yourdata.mds<-  metaMDS(yourdata[,(5:26)], k=2, autotransform = TRUE)

#Using the adonis function, we run the permutational MANOVA (PERMANOVA), with the fixed effects of site and season.
yourdata.adonis<-adonis(yourdata[,(5:26)] ~Site*Season, data=yourdata, permutations = 9999)
yourdata.adonis

#I want to have a preliminary look at what our MDS will look like. 
plot(yourdata.mds)

#I also want to know the stress of our plot. As a general rule, this should be < .2
yourdata.mds$stress

#Now I want to focus on the environmental data to create our vectors. You  wouldn't want to include all of these variables on the plot, because some of the predictors are likely correlated. 
yourdata[,c("Temperature_max", "Temperature_min", "Temperature_variance", 
            "Days.above.MMM",  "PAR_mean","PAR_SD","PAR_max_day", "CO2_mean_day",
            "CO2_mean_night" )]

#Using variance-inflation, we can detect multicollinearity of our predictors. Generally, vif should be < 5.
vif(lm(1:nrow(yourdata)~Temperature_max+ Temperature_min+ Days.above.MMM+ PAR_mean+
         PAR_SD +PAR_max_day+ CO2_mean_day+ CO2_mean_night, 
       data=yourdata[,c("Temperature_max", "Temperature_min", "Days.above.MMM",
                       "PAR_mean","PAR_SD","PAR_max_day", "CO2_mean_day",
                       "CO2_mean_night")]))

#After running vif, we can weed out a few of our correlated predictors. I don't in this example, but you should. 

#Using ordiplot, we can visualize our data points on the plot with the categories overlayed to help us explore our data.
ordiplot(yourdata.mds, type='text')

#Using envfit, we can fit our environmental vectors onto our ordination.
yourdata.envfit<- envfit(yourdata.mds, yourdata[,c("Temperature_max", "Temperature_min", "Days.above.MMM",  "PAR_mean","PAR_SD","PAR_max_day", "CO2_mean_day","CO2_mean_night")], na.rm=TRUE, permu= 9999)

#Here we visualize our ordination with the vectors overlayed. It allows for a prelimiary look at our data, but we can make it look a lot better. 
plot(yourdata.envfit, display='Sites')

#You can look at the significance of your vectors. 
yourdata.envfit

#Extract the vectors
yourdata.envfit.df<-as.data.frame(yourdata.envfit$vectors$arrows*sqrt(yourdata.envfit$vectors$r))
yourdata.envfit.df$species<-rownames(yourdata.envfit.df)

yourdata.sites.scores<- as.data.frame(scores(yourdata.mds, display = 'sites'))
yourdata.sites.scores<- data.frame(yourdata.sites.scores, yourdata)
yourdata.species.scores<- as.data.frame(scores(yourdata.mds, display = 'species'))
yourdata.species.scores$Species<- row.names(yourdata.species.scores)

#Make a pretty plot. There is A LOT going on in the plot, and you can choose what to include- either the catergory vectors, or the environmental vectors. Both are displayed here. 

yourdata.sites.scores$Site <- factor(yourdata.sites.scores$Site, 
                                     levels=c("Harry's Bommie 5m",
                                              "Harry's Bommie 8m",
                                              "Fourth Point 5m", 
                                              "Fourth Point 8m",
                                              "Harry's Tower",
                                              "Inner Reef Flat", 
                                              "Shallow Lagoon", "Deep Lagoon")) #the first line is manually ordering the factor levels, as I wish for them to be in the legend. 
niceplot<-ggplot()+
  geom_point(data=yourdata.sites.scores, aes(y=NMDS2, x=NMDS1, color=Site), size=2.5)+
  geom_label_repel(data=yourdata.envfit.df,aes(x=NMDS1,y=NMDS2,label= species, show.legend=FALSE), size=5)+ #Because I have a lot of labelled vectors on this plot with all of my categories, I use geom_label_repel to make sure none of the categorical variables are appearing on too of each other. 
  geom_segment(data=yourdata.envfit.df,aes(x=0,xend=NMDS1,y=0,yend=NMDS2), linetype='solid', arrow=arrow(length=unit(0.5, 'cm')), color='grey', alpha=0.9, show.legend=FALSE) + 
  geom_text_repel(data=yourdata.species.scores, aes(y=NMDS2, x=NMDS1, label=Species, hjust=-0.2),alpha=0.9,show.legend=FALSE)+ 
  geom_segment(data=yourdata.species.scores, aes(y=0, x=0, yend=NMDS2, xend=NMDS1), linetype='solid', arrow=arrow(length=unit(0.3, 'cm')), color='grey',alpha=0.9, show.legend=FALSE)+ #Because I have a lot of environemntal variables on my plot, I can use a different label, geom_text_repel to make sure the environmental factors aren't on top of each other. 
  scale_color_manual("Site", values=c("Harry's Bommie 5m"= 'olivedrab3',"Harry's Bommie 8m"='darkseagreen2' ,"Fourth Point 5m"='darkslategray3' , "Fourth Point 8m"= 'plum4',"Harry's Tower"= 'pink',"Inner Reef Flat"='indianred1', "Shallow Lagoon"='sandybrown', "Deep Lagoon"='lightgoldenrod1'), 
                     labels=c("Harry's Bommie 5m","Harry's Bommie 8m","Fourth Point 5m", "Fourth Point 8m","Reef Crest","Inner Reef Flat", "Shallow Lagoon", "Deep Lagoon"))+ #Manually choose the colors
  theme_classic()
niceplot

