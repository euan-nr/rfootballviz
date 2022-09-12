library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
data <-PL.Results...Results
view(data)
t <- ggtitle("New Manager Bounce")
s <- labs(size=5,subtitle="text")
xl <- xlab("Matchday")
yl <- ylab("xG")
p <- ggplot(data, aes(x=Round))+geom_line(aes(y=AVL_xG),color="#670E36")+geom_line(aes(y=AVL_xGA),colour="#95BFE5")+scale_x_continuous(breaks=seq(1, 36, by = 1),limits=c(1,36))+t+xl+yl+s
p2<-p+geom_vline(xintercept=12, linetype= "longdash", color="black", size= 0.5)
p2
p3<-p2+geom_line(color="#003399")+geom_point()
p3
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