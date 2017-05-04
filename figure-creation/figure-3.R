## NOTE: in order to get r to use ggplot2 plotting instead of cowplot, R must be restarted for each figure

library(sqldf)
library(plyr)
library(ggplot2)
library(gridExtra)
library(grid)

setwd("~/git-repos/lbsm-survey/images")

surveyData <- read.csv('/home/matt/data/survey-responses.csv',na.strings=c("","NA"),header=T,sep=',')
newColnames <- c("time","age","gen","race","ac_stan","major","own_phone","phone_prov","phone_os","lat_os","own_tab","tab_os","soc_med_used","soc_med_most_freq","pref_dev","geotagged","geo_soc_med","geo_soc_med_most_freq","inst_post","inst_mult_acc","inst_priv","inst_posts_per_day","inst_device","inst_loc_per","twit_post","twit_mult_acc","twit_priv","twit_posts_per_day","twit_pref_dev","twit_loc_per","push_twit","soc_med_push_twit","soc_med_push_twit_geo","twit_loc_type","why_geo","why_not_geo","geo_good","more_like_geo","no_geo_loc_info","geo_home","geo_away_home","geo_neg_con","geo_priv","zip","thoughts","email")
colnames(surveyData) <- newColnames

## create modifed columns
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
plotColors <- c("#bd0026","#f03b20","#fd8d3c","#fecc5c","#ffffb2") # red-oranges adapted from colorbrewer

## figures for 'i feel that geotagging infringes upon my privacy'
# Figure for gender and geo_priv
ccSurveyData <- surveyData[complete.cases(surveyData$geo_priv),]

tot1 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Female'")[,]
a1 <- sqldf("select count(*) from ccSurveyData where gen_mf='Female' and geo_priv='Strongly disagree'")[,] / tot1 * 100
b1 <- sqldf("select count(*) from ccSurveyData where gen_mf='Female' and geo_priv='Disagree'")[,] / tot1 * 100
c1 <- sqldf("select count(*) from ccSurveyData where gen_mf='Female' and geo_priv='Neutral'")[,] / tot1 * 100
d1 <- sqldf("select count(*) from ccSurveyData where gen_mf='Female' and geo_priv='Agree'")[,] / tot1 * 100
e1 <- sqldf("select count(*) from ccSurveyData where gen_mf='Female' and geo_priv='Strongly agree'")[,] / tot1 * 100

tot2 <- sqldf("select count(*) from ccSurveyData where gen_mf = 'Male'")[,]
a2 <- sqldf("select count(*) from ccSurveyData where gen_mf='Male' and geo_priv='Strongly disagree'")[,] / tot2 * 100
b2 <- sqldf("select count(*) from ccSurveyData where gen_mf='Male' and geo_priv='Disagree'")[,] / tot2 * 100
c2 <- sqldf("select count(*) from ccSurveyData where gen_mf='Male' and geo_priv='Neutral'")[,] / tot2 * 100
d2 <- sqldf("select count(*) from ccSurveyData where gen_mf='Male' and geo_priv='Agree'")[,] / tot2 * 100
e2 <- sqldf("select count(*) from ccSurveyData where gen_mf='Male' and geo_priv='Strongly agree'")[,] / tot2 * 100

gender <- c(rep(c("Female", "Male"), each = 5))
likert <- c(rep(c("Strongly disagree   ", "Disagree   ", "Neutral    ", "Agree    ", "Strongly agree    "), times = 2))
percentage <- c(a1,b1,c1,d1,e1,a2,b2,c2,d2,e2)
data <- data.frame(gender,likert,percentage)

data_trans <- ddply(data, .(gender), transform, pos = cumsum(percentage) - (0.5 * percentage))
data_trans$likert <- factor(data_trans$likert, levels=c("Strongly disagree   ", "Disagree   ", "Neutral    ", "Agree    ", "Strongly agree    "))
data_trans$per_label <- round(data_trans$percentage)
data_trans$per_label <- sub("$", "%", data_trans$per_label)

p1 <- ggplot(data_trans, aes(x = gender, y = percentage)) +
  theme(text = element_text(size = 8.5),
        legend.position="none") +
  geom_bar(aes(fill = likert), stat="identity") +
  geom_text(aes(label = per_label, y = pos), size = 2.5) +
  labs(x = "\nGender", y = "Percentage") +
  scale_fill_manual(values=plotColors,guide = guide_legend(reverse=TRUE, title='',size=5,direction = "vertical"))


## Figure for minority and geo_priv
ccSurveyData <- surveyData[complete.cases(surveyData$geo_priv),]

tot1 <- sqldf("select count(*) from ccSurveyData where minority = 'Minority'")[,]
a1 <- sqldf("select count(*) from ccSurveyData where minority='Minority' and geo_priv='Strongly disagree'")[,] / tot1 * 100
b1 <- sqldf("select count(*) from ccSurveyData where minority='Minority' and geo_priv='Disagree'")[,] / tot1 * 100
c1 <- sqldf("select count(*) from ccSurveyData where minority='Minority' and geo_priv='Neutral'")[,] / tot1 * 100
d1 <- sqldf("select count(*) from ccSurveyData where minority='Minority' and geo_priv='Agree'")[,] / tot1 * 100
e1 <- sqldf("select count(*) from ccSurveyData where minority='Minority' and geo_priv='Strongly agree'")[,] / tot1 * 100

tot2 <- sqldf("select count(*) from ccSurveyData where minority = 'Non-minority'")[,]
a2 <- sqldf("select count(*) from ccSurveyData where minority='Non-minority' and geo_priv='Strongly disagree'")[,] / tot2 * 100
b2 <- sqldf("select count(*) from ccSurveyData where minority='Non-minority' and geo_priv='Disagree'")[,] / tot2 * 100
c2 <- sqldf("select count(*) from ccSurveyData where minority='Non-minority' and geo_priv='Neutral'")[,] / tot2 * 100
d2 <- sqldf("select count(*) from ccSurveyData where minority='Non-minority' and geo_priv='Agree'")[,] / tot2 * 100
e2 <- sqldf("select count(*) from ccSurveyData where minority='Non-minority' and geo_priv='Strongly agree'")[,] / tot2 * 100

minority <- c(rep(c("Minority", "Non-minority"), each = 5))
likert <- c(rep(c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), times = 2))
percentage <- c(a1,b1,c1,d1,e1,a2,b2,c2,d2,e2)
data <- data.frame(minority,likert,percentage)

data_trans <- ddply(data, .(minority), transform, pos = cumsum(percentage) - (0.5 * percentage))
data_trans$likert <- factor(data_trans$likert, levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
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
  geom_bar(aes(fill = likert), stat="identity") +
  geom_text(aes(label = per_label, y = pos), size = 2.5) +
  coord_fixed(ratio = 0.02) +
  labs(x = "\nRace", y = "") +
  scale_fill_manual(values=plotColors,guide = guide_legend(reverse=FALSE, title='',size=5))

## Figure for academic standing and geo_priv
ccSurveyData <- surveyData[complete.cases(surveyData$geo_priv),]

tot1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Underclassman'")[,]
a1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Underclassman' and geo_priv='Strongly disagree'")[,] / tot1 * 100
b1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Underclassman' and geo_priv='Disagree'")[,] / tot1 * 100
c1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Underclassman' and geo_priv='Neutral'")[,] / tot1 * 100
d1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Underclassman' and geo_priv='Agree'")[,] / tot1 * 100
e1 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Underclassman' and geo_priv='Strongly agree'")[,] / tot1 * 100

tot2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Upperclassman'")[,]
a2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Upperclassman' and geo_priv='Strongly disagree'")[,] / tot2 * 100
b2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Upperclassman' and geo_priv='Disagree'")[,] / tot2 * 100
c2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Upperclassman' and geo_priv='Neutral'")[,] / tot2 * 100
d2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Upperclassman' and geo_priv='Agree'")[,] / tot2 * 100
e2 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Upperclassman' and geo_priv='Strongly agree'")[,] / tot2 * 100

tot3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed = 'Graduate student'")[,]
a3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Graduate student' and geo_priv='Strongly disagree'")[,] / tot3 * 100
b3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Graduate student' and geo_priv='Disagree'")[,] / tot3 * 100
c3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Graduate student' and geo_priv='Neutral'")[,] / tot3 * 100
d3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Graduate student' and geo_priv='Agree'")[,] / tot3 * 100
e3 <- sqldf("select count(*) from ccSurveyData where ac_stan_collapsed='Graduate student' and geo_priv='Strongly agree'")[,] / tot3 * 100

ac_standing <- c(rep(c("Underclassman", "Upperclassman", "Graduate student"), each = 5))
likert <- c(rep(c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"), times = 3))
percentage <- c(a1,b1,c1,d1,e1,a2,b2,c2,d2,e2,a3,b3,c3,d3,e3)
data <- data.frame(ac_standing,likert,percentage)

data_trans <- ddply(data, .(ac_standing), transform, pos = cumsum(percentage) - (0.5 * percentage))
data_trans$likert <- factor(data_trans$likert, levels=c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))
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
  geom_bar(aes(fill = likert), stat="identity") +
  geom_text(aes(label = per_label, y = pos), size = 2.5) +
  labs(x = "\nAcademic Standing", y = "") +
  scale_fill_manual(values=plotColors,guide = guide_legend(reverse=FALSE, title='',size=5))

g1 <- arrangeGrob(p1,p2,p3,ncol=3)
library(cowplot)
g1_legend <- get_legend(p1 + theme(legend.position = "bottom",legend.text=element_text(size = 7))) # cowplot required
detach("package:cowplot",unload=TRUE)
g1_combined <- grobTree(arrangeGrob(top="LBSM perception",g1,g1_legend,heights=c(10,3.5)),rectGrob(gp=gpar(lwd=2.5,fill=NA)))
theme_set(theme_gray()) # use ggplot2 theme; have to use cowplot to use get_legend
ggsave(g1_combined,device="eps",dpi=300,width = 7.7, height = 5, units = "in",limitsize=TRUE,file="Fig3.eps")
ggsave(g1_combined,device="tiff",dpi=300,width = 7.7, height = 5, units = "in",limitsize=FALSE,file="Fig3.tif")
