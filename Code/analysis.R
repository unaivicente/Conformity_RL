setwd("~/Google Drive/Doctorado Social Neuroscience/Exp 1 - Social Conformity/Results/Originals Phase 1")


library(ggplot2) 
library(dplyr) 
library(lme4)
library(effects)
library(lmerTest)
library(effsize)
library(sjmisc)
library(plyr)


data_total <- read.csv('totaldata.csv', sep = ',')
names<-c('Block','TrialRep','Trial','Lead','win','AbsSuj')
data_total[,names] <- lapply(data_total[,names],factor)

#MODEL FIT#############################################################################
#Main Interaction Model: Group*TrialRep

total_mlm <- lmer(Inter_div_abs~TrialRep*Group+ (1|ID/AbsSuj),data=data_total)

summary(total_mlm)
anova(total_mlm)

#Fit Secondary Interaction Model: Group*Block

total_block <- lmer(Inter_div_abs~Block*Group + (1|ID/AbsSuj),data=data_total)
total_block

summary(total_block)
anova(total_block)
plot(Effect(c("Block","Group"),total_block))


#PLOT EFFECTS OF MAIN INTERACTION

ef1 <- effect(term = "TrialRep*Group", mod = total_mlm)
efdata1 <- as.data.frame(ef1)

f1 <- ggplot(efdata1, aes(x=TrialRep, y=fit, color=Group, group=Group)) + 
  ggtitle("Difference in Answers by Group")+
  geom_point() + 
  geom_line(size=1.2) +
  expand_limits(y = 0) +
  scale_x_discrete(limits=c("1","2","3"))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Group),alpha=0.3) + 
  labs(x= "Trial Repetition", y="Difference between Users", color="Groups", fill="Groups") + theme_gray() + theme(text=element_text(size=20))+
  theme_grey(base_size = 30)+
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  theme(plot.title = element_text(hjust = 0.5,size=19,face="bold"))


#PLOT EFFECTS OF 2ND INTERACTION


ef.b <- effect(term = "Block*Group", mod = total_block)
efdata.b <- as.data.frame(ef.b)

theme(plot.title = element_text(hjust = 0.5))

markB <- c("1", "2", "3", "4")

blockp <- ggplot(efdata.b, aes(x=Block, y=fit, color=Group, group=Group)) + 
  ggtitle("Difference in Answers by Group and Block")+
  geom_point() + 
  geom_line(size=1.2) +
  scale_x_discrete(labels= markB) +
  expand_limits(y = 0) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Group),alpha=0.3) + 
  labs(x= "Block", y="Difference Averages between Users", color="Groups", fill="Groups") + theme_gray() + theme(text=element_text(size=20))+
  theme_grey(base_size = 30)+
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  theme(plot.title = element_text(hjust = 0.5,size=19,face="bold"))


###Plotting the effects in ggplot2


ef1 <- effect(term = "TrialRep*Group", mod = total_mlm)
efdata1 <- as.data.frame(ef1)

###Plot the result


f1 <- ggplot(efdata1, aes(x=TrialRep, y=fit, color=Group, group=Group)) + 
  ggtitle("Difference in Answers by Group")+
  geom_point() + 
  geom_line(size=1.2) +
  expand_limits(y = 0) +
  scale_x_discrete(limits=c("1","2","3"))+
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, fill=Group),alpha=0.3) + 
  labs(x= "Trial Repetition", y="Difference between Users", color="Groups", fill="Groups") + theme_gray() + theme(text=element_text(size=20))+
  theme_grey(base_size = 30)+
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  theme(plot.title = element_text(hjust = 0.5,size=19,face="bold"))


###Mean Calc and Effect Size

data_total$Mean <- rep(c(mean(subset(data_total, ID=='I01' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I02' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I03' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I04' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I05' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I06' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I07' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I08' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I09' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I10' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I11' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I12' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I13' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I14' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I15' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I16' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I17' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I18' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I19' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='I20' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C01' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C02' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C03' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C04' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C05' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C06' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C07' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C08' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C09' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C10' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C11' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C12' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C13' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C14' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C15' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C16' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C17' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C18' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C19' & TrialRep==3)$User.D),
                         mean(subset(data_total, ID=='C20' & TrialRep==3)$User.D)), each = 480)

cohenD <- cohen.d(Mean~Group, data = data_total)
cohenD

hedgesG <- cohen.d(Mean~Group, data = data_total, hedges=TRUE)
hedgesG


#PRODUCTIVITY MEASURE (ACCURACY)

###First we print a new column to see the average of the users

data_total$UsrAvg <- apply(data_total[,6:7],1,mean)

###We take the third and print it per trial by 3

data_total$Productivity <- vector(length = dim(data_total)[1])
for(i in seq(1,dim(data_total)[1],by=3)){
  data_total$Productivity[c(i,i+1,i+2)] <- rep(data_total$UsrAvg[i+2]-data_total$Random.Point[i+2],each = 3)
}

data_total$Productivity_abs <- as.integer(lapply(data_total$Productivity,abs))


###Plotting

rep3dat <- as.data.frame(filter(data_total, TrialRep==3))
summary(rep3dat$ID)

mu <- ddply(rep3dat, "Group", summarise, grp.mean=mean(Productivity_abs))

dat <- as.data.frame(rep3dat[,c("Group","Productivity_abs")])

p <-ggplot(dat, aes(x=Productivity_abs))+
  geom_histogram(aes(fill=Group),alpha=0.4,position= position_dodge(),bins=50)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Group), linetype="dashed")+
  ggtitle("Accuracy to Actual Point") + 
  xlab("Closer to Accuracy") + 
  ylab("Levels")+
  labs(x= "Relative Accuracy to Point (0 means no distance to point)", y="Number of Responses") +
  theme_gray() +
  theme_grey(base_size = 30)+
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  theme(plot.title = element_text(hjust = 0.5,size=19,face="bold"))


###Analizing the data

cohenD <- cohen.d(Productivity~Group, data = data_total)
cohenD


#POST EXPERIMENT QUESTIONNAIRE DATA

postexp <- read.csv('postexp.csv', sep = ';')

postexp


postexp$avg <-  vector(length = dim(postexp)[1])
for (i in seq(1,dim(postexp)[1])){
  postexp$avg[i] <-
    if (postexp$Group[i]=='C'){
      print(mean(subset(postexp, Group=='C')$P))}
  else {
    print(mean(subset(postexp, Group=='I')$P))}
}                         

cohenD_AAG <- cohen.d(avg~Group, data = postexp)
cohenD_AAG


#ANOVA with Positivity factor AAG

dfM <- as.data.frame(t(c(mean(subset(postexp, Group=='C')$P),mean(subset(postexp, Group=='I')$P))))


df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("Positivity in Cooperative", "Positivity in Individual")
colnames(dfM) <- x
dfM

fitted <- aov(P~Group,data=postexp)
summary(fitted)
anova(fitted)

#ANOVA with "te ha gustado el experimento?"

dfM2 <- as.data.frame(t(c(mean(subset(postexp, Group=='C')$X2),mean(subset(postexp, Group=='I')$X2))))

x <- c("Positivity in Cooperative", "Positivity in Individual")
colnames(dfM2) <- x
dfM2

fitted2 <- aov(X2~Group,data=postexp)
summary(fitted2)

boxplot(X2~Group,data=postexp)

c(mean(subset(postexp, Group=='C')$X2),mean(subset(postexp, Group=='I')$X2))

#ANOVA with "recomendarias el experimento?"

dfM3 <- as.data.frame(t(c(mean(subset(postexp, Group=='C')$X3),mean(subset(postexp, Group=='I')$X3))))

x <- c("Positivity in Cooperative", "Positivity in Individual")
colnames(dfM3) <- x


fitted3 <- aov(X3~Group,data=postexp)
summary(fitted3)

c(mean(subset(postexp, Group=='C')$X3),mean(subset(postexp, Group=='I')$X3))



library(plyr)
library(ggplot2)
library(plotly)

#Plotting of responses in Question 2: "Did you enjoy the experiment?"

mu2 <- ddply(postexp, "Group", summarise, grp.mean=mean(X2))

dat2 <- as.data.frame(postexp[,c("Group","X2")])


p2 <-ggplot(dat2, aes(x=X2))+
  geom_bar(aes(color=Group,fill=Group),alpha=0.4, position= position_dodge(preserve = "single"))+
  geom_vline(data=mu2, aes(xintercept=grp.mean, color=Group), linetype="dashed")+
  ggtitle("Distribution of Responses in Experience Value (Likeability)") + 
  xlab("Density in Responses") + 
  ylab("Levels")+
  labs(x= "Levels (1 Lower - 5 Higher consideration of the experience)", y="Density in Responses") +
  theme_gray() +
  theme_grey(base_size = 30)+
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  theme(plot.title = element_text(hjust = 0.5,size=19,face="bold"))


p2

#Plotting of responses in Question 8: "Have you felt you have synched with your mate?"

mu8 <- ddply(postexp, "Group", summarise, grp.mean=mean(X8))

dat8 <- as.data.frame(postexp[,c("Group","X8")])

p8 <-ggplot(dat8, aes(x=X8))+
  geom_bar(aes(color=Group,fill=Group),alpha=0.4, position= position_dodge(preserve = "single"))+
  geom_vline(data=mu8, aes(xintercept=grp.mean, color=Group), linetype="dashed")+
  ggtitle("Distribution of Responses in Experience Value (Synchrony)") +
  xlab("Density in Responses") + 
  ylab("Levels")+
  labs(x= "Levels (1 Lower - 5 Higher consideration of the experience)", y="Density in Responses") +
  theme_gray() +
  theme_grey(base_size = 30)+
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  theme(plot.title = element_text(hjust = 0.5,size=19,face="bold"))


p8
#Plotting of responses in Question 11: "Have you felt in trust with your mate?"

mu11 <- ddply(postexp, "Group", summarise, grp.mean=mean(X11))

dat11 <- as.data.frame(postexp[,c("Group","X11")])

p11 <-ggplot(dat11, aes(x=X11))+
  geom_bar(aes(color=Group,fill=Group),alpha=0.4, position= position_dodge(preserve = "single"))+
  geom_vline(data=mu11, aes(xintercept=grp.mean, color=Group), linetype="dashed")+
  ggtitle("Distribution of Responses in Experience Value (Trust)") + 
  xlab("Density in Responses") + 
  ylab("Levels")+
  labs(x= "Levels (1 Lower - 5 Higher consideration of the experience)", y="Density in Responses") +
  theme_gray() +
  theme_grey(base_size = 30)+
  theme(axis.text=element_text(size=22),
        axis.title=element_text(size=20,face="bold"),
        legend.title=element_text(size=16),
        legend.text=element_text(size=14))+
  theme(plot.title = element_text(hjust = 0.5,size=19,face="bold"))


#Plotting of responses in Question 14: "Do you consider rewarding working with your mate?"

mu14 <- ddply(postexp, "Group", summarise, grp.mean=mean(X14))

dat14 <- as.data.frame(postexp[,c("Group","X14")])

p14 <-ggplot(dat14, aes(x=X14))+
  geom_bar(aes(color=Group,fill=Group),alpha=0.4, position= position_dodge(preserve = "single"))+
  geom_vline(data=mu14, aes(xintercept=grp.mean, color=Group), linetype="dashed")+
  ggtitle("Distribution of Responses in Experience Value (Reward)") + 
  xlab("Density in Responses") + 
  ylab("Levels")+
  labs(x= "Levels (1 Lower - 5 Higher consideration of the experience)", y="Density in Responses") +
  theme_gray() +
  theme_grey(base_size = 30)+
  theme(axis.text=element_text(size=22),
         axis.title=element_text(size=20,face="bold"),
         legend.title=element_text(size=16),
         legend.text=element_text(size=14))+
  theme(plot.title = element_text(hjust = 0.5,size=19,face="bold"))



# Plotting all together


library(cowplot)

pall <- plot_grid(p2,p8,p11, p14, labels = "AUTO", label_size = 20)


