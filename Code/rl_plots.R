library(ggplot2)
library(gridExtra)
library(grid)
library(wesanderson)


dat1 <- read.csv('first_expdata.csv', sep=',', header=TRUE)
colnames(dat1) <- c("Alpha","Likelihood")

dat1$Group <- rep(NA,nrow(dat1))
  for (i in 1:nrow(dat1)){
    if (i <= 40){
      dat1[i,]$Group <- print('C')
    }else{
      dat1[i,]$Group <- print('I')
    }
  }

dat1$Group <- factor(dat1$Group, levels = c('C','I'))


g1 <- dat1[1:40,]$Alpha
g2 <- dat1[41:80,]$Alpha
statA <- wilcox.test(g1, g2)
statL <- t.test(dat1[1:40,]$Likelihood, dat1[41:80,]$Likelihood, var.equal=TRUE)

pltt <- rev(wes_palette(n=2, name="Darjeeling1"))


#Boxplot Loglikelihood
p1 <- ggplot(dat1, aes(x=Group, y=Likelihood, color=Group)) + 
  geom_boxplot(notch=TRUE, width=0.5) +
  geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.25) +
  #scale_fill_manual(values=pltt) +
  scale_color_manual(values=pltt) +
  theme(legend.position = "none",
  axis.text=element_text(size=10),
  axis.title=element_text(size=12),
  plot.title = element_text(hjust = 0.5,size=19,face="bold")) +
  theme_light() +
  ggtitle("A. Boxplot of Log-Likelihoods by Group")
        
#Boxplot Alpha
p2 <- ggplot(dat1, aes(x=Group, y=Alpha, color=Group)) + 
  geom_boxplot(notch=TRUE, width=0.5) +
  geom_jitter(shape=16, position=position_jitter(0.1), alpha=0.25) +
  #scale_fill_manual(values=pltt) +
  scale_color_manual(values=pltt) +
  theme(legend.position = "none",
  axis.text=element_text(size=10),
  axis.title=element_text(size=12),
  plot.title = element_text(hjust = 0.5,size=19,face="bold")) +
  theme_light() +
  ggtitle("B. Boxplot of Alpha by Group")

# #Scatterplot and Density Contours
# p3 <- ggplot(dat1, aes(x=Alpha, y=Likelihood, color=Group)) +
#   geom_point(size=2, shape=20) +
#   stat_density2d(aes(x=Alpha, y=Likelihood, color=Group), linetype=1, alpha=0.25)+
#   scale_color_manual(values=pltt) +
#   #geom_polygon(alpha = .25, bins=4, size=0.1) +
#   theme(axis.text=element_text(size=10),
#   axis.title=element_text(size=12),
#   plot.title = element_text(hjust = 0.5,size=19,face="bold")) +
#   theme_light() +
#   ggtitle("C. Scatterplot with density contours of Likelihood ~ Alpha")


a3 <-grid.arrange(p1, p2,nrow=1,
                  top = textGrob("RL Diagnostic Plots",gp=gpar(fontsize=18,font=2)))


ggsave("rl_diags.pdf",a3, device="pdf", width = 30, height = 15, units="cm")

