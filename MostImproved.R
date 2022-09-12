library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
colnames(MostImp) <- c('Rk','Player',	'Club/League22',	'M22',	'G22',	'A22',	'GA22',	'Club/League21',	'M21',	'G21',	'A21',	'GA21',	'TOTGA',	'GADIFF')
finImp1 <- MostImp[-1,]
finImp2 <- finImp1[-c(8)]
cols <- names(finImp2)[4:13]
finImp2[cols] <- lapply(finImp2[cols], as.numeric)
#view(finImp2)
t <- ggtitle("Most Improved Players 2021/22 (15/04/22)")
s <- labs(size=5,subtitle="                                How do players' 2020/21 G/A tally compare to this season so far?")
xl <- xlab("Goals and Assists 21/22")
yl <- ylab("Net Improvement from 20/21")
p <- ggplot(finImp2, aes(x=GA22,y=GADIFF))+t+xl+yl+s
p2 <- p + theme(plot.title=element_text(size=14, face="bold",hjust=0.5), axis.title.x=element_text( size=12, face="bold", color="#2E2829"),axis.title.y=element_text( size=12, face="bold",color="#2E2829"))
p3 <- p2 + geom_point(aes(GA22,GADIFF,color=GADIFF),size=3.5)+ scale_colour_gradient(name='GA DIFF',low="red", high="green") + ggrepel::geom_text_repel(data=subset(finImp2, GA22>=25 | GADIFF>=10 | GADIFF <= -10),aes(GA22,GADIFF,label=Player),size=3)
p4 <- p3 + geom_hline(yintercept=0, linetype= "longdash", color="black", size= 0.5)
p5 <- p4 + labs(caption = "Stats taken from transfermarkt.com on 15/4/22")
p5 + annotate("text", x = Inf, y = -10, label = "@_euanito",
               hjust=1.1, vjust=-1.1, col="#e88d87", cex=6,
               fontface = "bold", alpha = 0.8)
ggsave('MostImproved.png', width = 9, height = 6)