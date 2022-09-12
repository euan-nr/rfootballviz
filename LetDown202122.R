#ggsave('LetDown.png', width = 9, height = library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
base_family=serif
data <- Big5STD
data <- filter(data, (Ast...15 >= 8 | xA...28 >=6) & Min>=1450)
data <- merge(x=data,y=LeagueColours,by="Comp")
#view(data)
#data$Sha<-"diamond"
#data[!is.na(data$Pos) & data$Pos=="GK","Sha"]<-"cross"
#data[!is.na(data$Pos) & data$Pos=="DF","Sha"]<-"square"
#data[!is.na(data$Pos) & data$Pos=="MF","Sha"]<-"circle"
#data[!is.na(data$Pos) & data$Pos=="FW","Sha"]<-"triangle"
#xlim(6,19) + ylim(2,15) + 
#caption = "Stats taken from Fbref.com",size=3.5
data$ADiff <- with(data,Ast...15 - xA...28)
#view(data)
t <- ggtitle("Let Down: Assists vs Expected Assists")
s <- labs(size=3.5,subtitle="for Big 5 Leagues (selected sample)", caption = "Stats taken from Fbref.com",size=3.5)
yl <- ylab("Assists")
xl <- xlab("Expected Assists")
p <- ggplot(data, aes(y=Ast...15,x=xA...28))+t+xl+yl+s+geom_abline(slope=1, intercept=0, linetype = "dotdash")+geom_abline(slope=1, intercept=-1, linetype = "dashed",size=0.25,color="red")+geom_abline(slope=1, intercept=1, linetype = "dashed",size=0.25,color="red")
p <- p + theme(
  panel.background = element_rect(fill = "#F9F2ED",
                                  colour = "#F9F2ED",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
p <- p +  theme(plot.title = element_text(hjust = 0.5)) +  theme(plot.subtitle = element_text(hjust = 0.5))
p<- p + scale_y_continuous(limits=c(1,20),breaks = scales::pretty_breaks(n = 4))+scale_x_continuous(limits=c(2,15),breaks = scales::pretty_breaks(n = 15))
p <- p + geom_point(aes(y=Ast...15,x=xA...28,shape=Pos,col=League),size=2) + scale_shape_manual(name="Position",guide='legend', values=c("square","triangle","circle"))+ scale_colour_manual(name="League",values=c("#D3010C","#ee8707","#dae025","#00ff85","#002d71"))
p <- p + ggrepel::geom_text_repel(data=filter(data, Ast...15>= 10 | ADiff<=-2.5  | ADiff >=2.5 | Gls...14>=15),aes(label=Name),size=3)
p <- p + annotate("text", y = 12.5, x = 5, label = "@CortexFtbl", colour ="red", cex=3.5,
                  fontface = "italic", alpha = 0.8)
#p <- p + annotate("text", y=14, x= 13.5, label = "perfectly balanced: A=xA", srt= 35, size = 3.5, fontface = "italic")
p <- p + geom_label(label="LET DOWN",y= 2.5,x=14,size =3,label.size = NA, label.padding = unit(0.09, "lines"),fill="#f54242") + geom_label(label="OVERPERFORMING",y = 17.5, x = 4,label.size = NA,label.padding = unit(0.09, "lines"),fill="seagreen3", size =3)
p <- p + geom_label(label="Poor/unlucky finishers\non end of passes?",y= 3.5,x=14,size =2.5,label.size = NA, label.padding = unit(0.09, "lines"),fill = "#F9F2ED") + geom_label(label="Lucky/ good finishers\non end of passes?",y = 18.5, x = 4,label.size = NA,label.padding = unit(0.09, "lines"),fill = "#F9F2ED", size =2.5)
p
ggsave('Let Down 202122.png', width = 9, height = 6)
