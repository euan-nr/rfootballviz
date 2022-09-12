library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
#view(goalkeeperstatsr)
colnames(GKstatsEurope) <- c('Rk','Player','Name','Nation','Pos','Squad','Comp','Age','Born','Full90s','GA','PKA','FK','CK','OG','PSxG','PSxG/SoT','PSxG+/-','PSxG/90','Cmp','Att','Cmp%','Att','Thr','Launch%','AvgLen','Att','Launch%','AvgLen','Opp','Stp','Stp%','#OPA','#OPA/90','AvgDist','SavePCT')
filtered1 <- GKstatsEurope[c(3,10:11,16,18,19,22,28,33,34,36)]
colnames(filtered1) <- c('Player',  'Full90s', 'GA', 'PSxG','PSxG-GA','PSxGDiffp90','LPCompPct','LPPct','SweepingActions', 'SAp90','SavePct')
filtered2 <- filtered1[-1,]
cols <- names(filtered2)[2:9]
filtered2[cols] <- lapply(filtered2[cols], as.numeric)
finKeep1 <- filter(filtered2, Full90s >=15) 
finKeep <- finKeep1[ -c(98), ]
#str(finKeep)
view(finKeep)
finKeep$col <- "grey"
finKeep[!is.na(finKeep$LPPct)& finKeep$LPPct >=50, "col"] <- "#83A785" #Green
finKeep[!is.na(finKeep$LPPct)& finKeep$LPPct <40, "col"] <- "#F25A67" #Red
finKeep[!is.na(finKeep$LPPct)& finKeep$LPPct <50 & finKeep$LPPct >=40, "col"] <- "#266DA1" #Blue
finKeep$sha <- "circle"
finKeep[!is.na(finKeep$SavePct) & finKeep$SavePct >=75, "sha"]<-"triangle"
finKeep[!is.na(finKeep$SavePct) & finKeep$SavePct <75 & finKeep$SavePct >=65, "sha"]<-"square"
finKeep[!is.na(finKeep$SavePct) & finKeep$SavePct <65, "sha"]<-"cross"
t <- ggtitle("Goalkeeper Effectiveness:\nSweeping Actions vs Post-Shot xG minus Goals Against")
xl <- xlab("PSxG-GA")
yl <- ylab("Defensive Actions Outside Penalty Area")
p <- ggplot(finKeep, aes(x=PSxG-GA,y=SAp90))+t+xl+yl
p2 <- p + theme(plot.title=element_text(size=14, face="bold",hjust=0.5), axis.title.x=element_text( size=12, face="bold", color="#2E2829"),axis.title.y=element_text( size=12, face="bold",color="#2E2829"))
#p3 <- p2 + geom_point(aes(PSxG-GA,SweepingActions,color=LPPct,shape=sha),size=3) + scale_shape_manual(name="Save %", labels=c("<65","65-75","75+"),values=c("cross","square","triangle")) +ggrepel::geom_text_repel(aes(label=Player),colour="black")
p3 <- p2 + geom_point(aes(PSxG-GA,SAp90,color=SavePct),size=4)+ scale_colour_gradient(name="Save %",low="red", high="green")+ ggrepel::geom_text_repel(data=subset(finKeep,PSxG-GA>=5|PSxG-GA<=-5|SAp90>=1.25|(SAp90>=1 & PSxG>0)|SAp90<=0.5),aes(PSxG-GA, SAp90,label=Player),size=3)
p4 <- p3 + theme(panel.background=element_rect(fill="#e5e5e9", size=0.1,linetype= "dotdash"))
p5 <- p4 + geom_vline(xintercept = 0, linetype="longdash", color = "black", size=0.5)
p6 <- p5 + geom_hline(yintercept=0.75, linetype= "longdash", color="black", size= 0.5)
p6
#key1 <- scale_color_manual(name="Long Pass Completion %",breaks=c(">=50%","40-50%","<40%"),values=c("#ACA24E"=">=50%","#F25A67"="40-50%","#266DA1"="<40%"))+ theme(legend.title=element_text(size=20),legend.text=element_text(size=14))
#key2 <- scale_shape_manual(name="Save %",breaks=c(">=75%","65-75%","<65%"),values=c("triangle"=">=75%","square"="65-75%","cross"="<65%"))+ theme(legend.title=element_text(size=20),legend.text=element_text(size=14))
p7 <- p6 + labs(subtitle="TL;DR: Tackles, interceptions etc outside area vs goals prevented ", caption = "Stats taken from Fbref.com on 1/4/22") + theme(plot.subtitle=element_text(hjust = 0.5,size= 9))
p8 <- p7+ geom_label(label="Good sweeper,\ngood stopper",x=8.25,y=38.25,size =3, label.padding = unit(0.09, "lines"),fill="#e5e5e9") + geom_label(label="Bad sweeper,\ngood stopper",x=8.25,y=3.25,size =3,label.padding = unit(0.09, "lines"),fill="#e5e5e9")+ geom_label(label="Bad sweeper,\nbad stopper",x=-9,y=3.25,size =3,label.padding = unit(0.09, "lines"),fill="#e5e5e9")+ geom_label(label="Good sweeper\nbad Stopper",x=-9,y=38.25,size =3,label.padding = unit(0.09, "lines"),fill="#e5e5e9")
p8 + annotate("text", x = Inf, y = 20, label = "@_euanito",
              hjust=1.1, vjust=-1.1, col="#e88d87", cex=6,
              fontface = "bold", alpha = 0.8)
