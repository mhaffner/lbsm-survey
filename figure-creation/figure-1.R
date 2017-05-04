library(sqldf)
library(ggplot2)
library(plyr)
library(gridExtra)
library(grid)

setwd("~/git-repos/lbsm-survey/images")

surveyData <- read.csv('/home/matt/data/survey-responses.csv',na.strings=c("","NA"),header=T,sep=',')
newcolnames <- c("time","age","gen","race","ac_stan","major","own_phone","phone_prov","phone_os","lat_os","own_tab","tab_os","soc_med_used","soc_med_most_freq","pref_dev","geotagged","geo_soc_med","geo_soc_med_most_freq","inst_post","inst_mult_acc","inst_priv","inst_posts_per_day","inst_device","inst_loc_per","twit_post","twit_mult_acc","twit_priv","twit_posts_per_day","twit_pref_dev","twit_loc_per","push_twit","soc_med_push_twit","soc_med_push_twit_geo","twit_loc_type","why_geo","why_not_geo","geo_good","more_like_geo","no_geo_loc_info","geo_home","geo_away_home","geo_neg_con","geo_priv","zip","thoughts","email")
colnames(surveyData) <- newcolnames

surveyData$gen_mf <- NA
surveyData$gen_mf[surveyData$gen == "Female"] <- "Female" # anything other than solely white 
surveyData$gen_mf[surveyData$gen == "Male"] <- "Male"

surveyData$minority <- NA # make new column for white/non-white
surveyData$minority[surveyData$race != "White/Caucasian"] <- "Minority" # anything other than solely white 
surveyData$minority[surveyData$race == "I prefer not to disclose"] <- NA # remove those that did not disclose (since we don't know); must be done after the previous line
surveyData$minority[surveyData$race == "White/Caucasian"] <- "Non-minority" # white only

surveyData$ac_stan_collapsed <- NA # make new column for academic standing (3 groups)
surveyData$ac_stan_collapsed[surveyData$ac_stan == "Freshman"] <- "Underclassman" # anything other than solely white 
surveyData$ac_stan_collapsed[surveyData$ac_stan == "Sophomore"] <- "Underclassman" # anything other than solely white 
surveyData$ac_stan_collapsed[surveyData$ac_stan == "Junior"] <- "Upperclassman" # anything other than solely white 
surveyData$ac_stan_collapsed[surveyData$ac_stan == "Senior"] <- "Upperclassman" # anything other than solely white 
surveyData$ac_stan_collapsed[surveyData$ac_stan == "Masters student"] <- "Graduate student" # anything other than solely white 
surveyData$ac_stan_collapsed[surveyData$ac_stan == "PhD student"] <- "Graduate student" # anything other than solely white 

## set colors for plots
plotColors <- c("#bd0026","#f03b20","#fd8d3c","#fecc5c","#ffffb2") # red-oranges adapted from colorbrewer; first class is slightly brighter
## Type of location used on Twitter compared by gender
ccSurveyData <- surveyData[complete.cases(surveyData$twit_loc_type),]
# Female
tot1 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Female'")[,]
a1 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Female' and twit_loc_type='I do not allow Twitter to use my location'")[,] / tot1 * 100
b1 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Female' and twit_loc_type='General location (e.g. city or neighborhood)'")[,] / tot1 * 100 
c1 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Female' and twit_loc_type='Both precise location and general location'")[,] / tot1 * 100  
d1 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Female' and twit_loc_type='Precise location (exact latitude-longitude)'")[,] / tot1 * 100
e1 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Female' and twit_loc_type='Unsure/default setting'")[,] / tot1 * 100

# Male
tot2 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Male'")[,]
a2 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Male' and twit_loc_type='I do not allow Twitter to use my location'")[,] / tot2 * 100 
b2 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Male' and twit_loc_type='General location (e.g. city or neighborhood)'")[,] / tot2 * 100 
c2 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Male' and twit_loc_type='Both precise location and general location'")[,] / tot2 * 100  
d2 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Male' and twit_loc_type='Precise location (exact latitude-longitude)'")[,] / tot2 * 100
e2 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Male' and twit_loc_type='Unsure/default setting'")[,] / tot2 * 100

# ggplot
gender <- c(rep(c("Female", "Male"), each = 5))
twit_loc <- c(rep(c("I do not allow Twitter to use my location", "General location (e.g. city or neighborhood)", "Both precise location and general location", "Precise location(exact latitude-longitude)", "Unsure/default setting"), times = 2))
percentage <- c(a1,b1,c1,d1,e1,a2,b2,c2,d2,e2)
data <- data.frame(gender,twit_loc,percentage)

data_trans <- ddply(data, .(gender), transform, pos = cumsum(percentage) - (0.5 * percentage))
data_trans$twit_loc <- factor(data_trans$twit_loc, levels=c("I do not allow Twitter to use my location", "General location (e.g. city or neighborhood)", "Both precise location and general location", "Precise location(exact latitude-longitude)", "Unsure/default setting"))
data_trans$per_label <- round(data_trans$percentage)
data_trans$per_label <- sub("$", "%", data_trans$per_label)
data_trans$per_label[data_trans$per_label == "0%"] <- NA # remove labels from 0 value sections

p1 <- ggplot(data_trans, aes(x = gender, y = percentage)) +
  theme(text = element_text(size = 8.5),
        legend.position="none") +
  geom_bar(aes(fill = twit_loc), stat="identity") +
  geom_text(aes(label = per_label, y = pos), size = 2.5) +
  labs(x = "\nGender", y = "Percentage") +
  scale_fill_manual(values=plotColors,guide = guide_legend(reverse=TRUE, title='',size=5,direction = "vertical"))
p1

## Type of lcoation used on Twitter compared by race
###### race #####
ccSurveyData <- surveyData[complete.cases(surveyData$twit_loc_type),]
# Minority
tot1 <- sqldf("select count(*) from ccSurveyData where minority = 'Minority'")[,]
a1 <- sqldf("select count(*) from ccSurveyData where minority = 'Minority' and twit_loc_type='I do not allow Twitter to use my location'")[,] / tot1 * 100
b1 <- sqldf("select count(*) from ccSurveyData where minority = 'Minority' and twit_loc_type='General location (e.g. city or neighborhood)'")[,] / tot1 * 100 
c1 <- sqldf("select count(*) from ccSurveyData where minority = 'Minority' and twit_loc_type='Both precise location and general location'")[,] / tot1 * 100  
d1 <- sqldf("select count(*) from ccSurveyData where minority = 'Minority' and twit_loc_type='Precise location (exact latitude-longitude)'")[,] / tot1 * 100
e1 <- sqldf("select count(*) from ccSurveyData where minority = 'Minority' and twit_loc_type='Unsure/default setting'")[,] / tot1 * 100

# non-minority
tot2 <- sqldf("select count(*) from ccSurveyData where minority = 'Non-minority'")[,]
a2 <- sqldf("select count(*) from ccSurveyData where minority = 'Non-minority' and twit_loc_type='I do not allow Twitter to use my location'")[,] / tot2 * 100 
b2 <- sqldf("select count(*) from ccSurveyData where minority = 'Non-minority' and twit_loc_type='General location (e.g. city or neighborhood)'")[,] / tot2 * 100 
c2 <- sqldf("select count(*) from ccSurveyData where minority = 'Non-minority' and twit_loc_type='Both precise location and general location'")[,] / tot2 * 100  
d2 <- sqldf("select count(*) from ccSurveyData where minority = 'Non-minority' and twit_loc_type='Precise location (exact latitude-longitude)'")[,] / tot2 * 100
e2 <- sqldf("select count(*) from ccSurveyData where minority = 'Non-minority' and twit_loc_type='Unsure/default setting'")[,] / tot2 * 100

# ggplot
minority <- c(rep(c("Minority", "Non-minority"), each = 5))
twit_loc <- c(rep(c("I do not allow Twitter to use my location", "General location (e.g. city or neighborhood)", "Both precise location and general location", "Precise location(exact latitude-longitude)", "Unsure/default setting"), times = 2))
percentage <- c(a1,b1,c1,d1,e1,a2,b2,c2,d2,e2)
data <- data.frame(minority,twit_loc,percentage)

data_trans <- ddply(data, .(minority), transform, pos = cumsum(percentage) - (0.5 * percentage))
data_trans$twit_loc <- factor(data_trans$twit_loc, levels=c("I do not allow Twitter to use my location", "General location (e.g. city or neighborhood)", "Both precise location and general location", "Precise location(exact latitude-longitude)", "Unsure/default setting"))
data_trans$per_label <- round(data_trans$percentage)
data_trans$per_label <- sub("$", "%", data_trans$per_label)
data_trans$per_label[data_trans$per_label == "0%"] <- NA # remove labels from 0 value sections

p2 <- ggplot(data_trans, aes(x = minority, y = percentage)) +
  theme(text = element_text(size = 8.5),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none",
        aspect.ratio=1.50
        ) +
  geom_bar(aes(fill = twit_loc), stat="identity") +
  geom_text(aes(label = per_label, y = pos), size = 2.5) +
  coord_fixed(ratio = 0.02) +
  labs(x = "\nRace", y = "") +
  scale_fill_manual(values=plotColors,guide = guide_legend(reverse=FALSE, title='',size=5))
p2

## Academic standing
ccSurveyData <- surveyData[complete.cases(surveyData$twit_loc_type),]
## Underclassman
tot1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Underclassman'")[,]
a1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Underclassman' and twit_loc_type='I do not allow Twitter to use my location'")[,] / tot1 * 100
b1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Underclassman' and twit_loc_type='General location (e.g. city or neighborhood)'")[,] / tot1 * 100 
c1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Underclassman' and twit_loc_type='Both precise location and general location'")[,] / tot1 * 100  
d1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Underclassman' and twit_loc_type='Precise location (exact latitude-longitude)'")[,] / tot1 * 100
e1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Underclassman' and twit_loc_type='Unsure/default setting'")[,] / tot1 * 100

# Upperclassman
tot2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Upperclassman'")[,]
a2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Upperclassman' and twit_loc_type='I do not allow Twitter to use my location'")[,] / tot2 * 100 
b2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Upperclassman' and twit_loc_type='General location (e.g. city or neighborhood)'")[,] / tot2 * 100 
c2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Upperclassman' and twit_loc_type='Both precise location and general location'")[,] / tot2 * 100  
d2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Upperclassman' and twit_loc_type='Precise location (exact latitude-longitude)'")[,] / tot2 * 100
e2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Upperclassman' and twit_loc_type='Unsure/default setting'")[,] / tot2 * 100

# Graduate student
tot3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Graduate student'")[,]
a3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Graduate student' and twit_loc_type='I do not allow Twitter to use my location'")[,] / tot3 * 100 
b3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Graduate student' and twit_loc_type='General location (e.g. city or neighborhood)'")[,] / tot3 * 100 
c3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Graduate student' and twit_loc_type='Both precise location and general location'")[,] / tot3 * 100  
d3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Graduate student' and twit_loc_type='Precise location (exact latitude-longitude)'")[,] / tot3 * 100
e3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Graduate student' and twit_loc_type='Unsure/default setting'")[,] / tot3 * 100

ac_standing <- c(rep(c("Underclassman", "Upperclassman", "Graduate student"), each = 5))
twit_loc <- c(rep(c("I do not allow Twitter to use my location", "General location (e.g. city or neighborhood)", "Both precise location and general location", "Precise location(exact latitude-longitude)", "Unsure/default setting"), times = 3))
percentage <- c(a1,b1,c1,d1,e1,a2,b2,c2,d2,e2,a3,b3,c3,d3,e3)
data <- data.frame(ac_standing,twit_loc,percentage)

data_trans <- ddply(data, .(ac_standing), transform, pos = cumsum(percentage) - (0.5 * percentage))
data_trans$twit_loc <- factor(data_trans$twit_loc, levels=c("I do not allow Twitter to use my location", "General location (e.g. city or neighborhood)", "Both precise location and general location", "Precise location(exact latitude-longitude)", "Unsure/default setting"))
data_trans$ac_standing <- factor(data_trans$ac_standing, level=c("Underclassman", "Upperclassman", "Graduate student"))
data_trans$per_label <- round(data_trans$percentage)
data_trans$per_label <- sub("$", "%", data_trans$per_label)
data_trans$per_label[data_trans$per_label == "0%"] <- NA # remove labels from 0 value sections

p3 <- ggplot(data_trans, aes(x = ac_standing, y = percentage)) +
  theme(text = element_text(size = 8.5),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none") +
  geom_bar(aes(fill = twit_loc), stat="identity") +
  geom_text(aes(label = per_label, y = pos), size = 2.5) +
  #coord_fixed(.05) +
  labs(x = "\nAcademic Standing", y = "") +
  scale_fill_manual(values=plotColors,guide = guide_legend(reverse=FALSE, title='',size=5))
p3

g1 <- arrangeGrob(p1,p2,p3,ncol=3)
library(cowplot)
g1_legend <- get_legend(p1 + theme(legend.position = "bottom",legend.text=element_text(size = 7))) # cowplot required
detach("package:cowplot",unload=TRUE)
g1_combined <- grobTree(arrangeGrob(top="Type of location setting used on Twitter",g1,g1_legend,heights=c(9,3.5)),rectGrob(gp=gpar(lwd=2.5,fill=NA)))
#library(ggplot2) # reload ggplot2 to retake ggsave
theme_set(theme_gray()) # use ggplot2 theme; have to use cowplot to use get_legend
ggsave(g1_combined,device="eps",dpi=300,width = 7.7, height = 5, units = "in",limitsize=TRUE,file="Fig1.eps")
ggsave(g1_combined,device="tiff",dpi=300,width = 7.7, height = 5, units = "in",limitsize=FALSE,file="Fig1.tif")
