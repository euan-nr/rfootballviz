library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
colnames(goalscorers1) <- c('Rk','Player','Name','Nation','Pos','Squad','Age','Born','Full90s','G','Sh','SoT','SoTPct','Sh/90','SoT/90','G/Sh','G/SoT','PSxG/90','Dist','FK','PK','PKatt','xG','npxG','npxG/Sh','G-xG','npG-xG')
filtered1 <- goalscorers1[c(3,6,10:17,22:26)]
colnames(filtered1) <- c('Player',  'Team', 'G', 'Sh','SoT','SoTPct','Sp90','SoTp90', 'GpS','GpSoT','xG','NPxG','NPxGpS','xGdiff','NPGdiff')
finGoal <- filtered1[-1,]
cols <- names(finGoal)[3:15]
finGoal[cols] <- lapply(finGoal[cols], as.numeric)
finGoal2 <- merge(finGoal,PLColours)
#view(finGoal2)
t <- ggtitle("Scoring Efficiency:\nxG Differential vs Goals Scored")
s <- labs(size=5,subtitle="                                     xG Differential is calculated by subtracting a player's xG from their goal count\n                                                        -it effectively is a measure of over/underperformance.")
xl <- xlab("Goals Scored")
yl <- ylab("xG Differential")
p <- ggplot(finGoal2, aes(x=G,y=xGdiff))+t+xl+yl+s
p2 <- p + theme(plot.title=element_text(size=14, face="bold",hjust=0.5), axis.title.x=element_text( size=12, face="bold", color="#2E2829"),axis.title.y=element_text( size=12, face="bold",color="#2E2829"))
p3 <- p2 + geom_point(aes(G,xGdiff),size=3.5,colour=finGoal2$Colour,pch=21,fill=finGoal2$Fill) + ggrepel::geom_text_repel(data=subset(finGoal, G>=10 | xGdiff>=3 | xGdiff <= -1),aes(G,xGdiff,label=Player),size=3)
p4 <- p3 + geom_hline(yintercept=0, linetype= "longdash", color="black", size= 0.5)
p5 <- p4 + scale_y_continuous(limits = c(-5, 5.5),minor_breaks = seq(-5, 6, 1)) + theme(panel.background=element_rect(fill="#e5e5e9", size=0.1,linetype= "dotdash"))
p6 <- p5 + geom_label(label="Overperforming xG (MORE CLINICAL)",x=12.5,y=5.2,size =3, label.size=0)+ geom_label(label="Underperforming xG (LESS CLINICAL)",x=12.5,y=-4.5,size =3, label.size=0)
p7 <- p6 + labs(caption = "Stats taken from Fbref.com on 9/4/22\nPlayers with 5+ PL goals only")
p8 <- p7 + annotate("text", x = 15.3, y = -1.5, label = "@_euanito",
            hjust=-1.1, vjust=-1.1, col="#e88d87", cex=6,
            fontface = "bold", alpha = 0.8)
p8 + scale_x_continuous(minor_breaks=seq(0,20,1))
ggsave('Scorerefficiency.png', width = 9, height = 6)