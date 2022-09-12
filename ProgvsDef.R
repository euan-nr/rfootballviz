library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
colnames(Progression_vs_Defense) <- c('Rank',	'Player','Name','Nation','Pos','Squad',	'Comp',	'Age','Born',	'90s',	'Tkl', 'TklW','Def 3rd','Mid 3rd','Att 3rd','Tkl','Att','Tkl%',	'Past',	'Press','Succ%','Def 3rd Press','Mid 3rd Press','Att 3rd Press','Blocks','Sh','ShSv','Pass','Int','Tkl+Int','Clr','Err','Tkl+Int+Succ','ProgAction','ProgCarry','ProgPass','TISp90','PAp90')
filtered1 <- Progression_vs_Defense[c(3,5,10,34:39)]
colnames(filtered1) <- c('Player','Pos' , 'Full90s', 'SuccD','ProgA','ProgC','ProgP','SD90','PA90')
filtered2 <- filtered1[-1,]
cols <- names(filtered2)[3:9]
filtered2[cols] <- lapply(filtered2[cols], as.numeric)
filtered3 <- filter(filtered2, Full90s >=10) 
finMid <- dplyr::filter(filtered3,!grepl('GK|DF|FW', Pos))
view(finMid)
t <- ggtitle("Midfielders: Defensive Actions vs Ball Progression")
xl <- xlab("Progressive Actions p 90")
yl <- ylab("Defensive Actions p 90")
p <- ggplot(finMid, aes(x=PA90,y=SD90))+t+xl+yl
p2 <- p + theme(plot.title=element_text(size=14, face="bold",hjust=0.5), axis.title.x=element_text( size=12, face="bold", color="#2E2829"),axis.title.y=element_text( size=12, face="bold",color="#2E2829"))
p3 <- p2 + geom_point(aes(PA90,SD90),size=2)+ggrepel::geom_text_repel(data=subset(finMid,SD90>=10|PA90>=13),aes(PA90,y=SD90,label=Player),size=3)
p3