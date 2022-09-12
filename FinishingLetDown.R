library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
base_family=serif
data <- Big5STD
data <- filter(data, Gls...14 >= 13 & Min>=1500)
data <- merge(x=data,y=LeagueColours,by="Comp")
#view(data)
#data$Sha<-"diamond"
#data[!is.na(data$Pos) & data$Pos=="GK","Sha"]<-"cross"
#data[!is.na(data$Pos) & data$Pos=="DF","Sha"]<-"square"
#data[!is.na(data$Pos) & data$Pos=="MF","Sha"]<-"circle"
#data[!is.na(data$Pos) & data$Pos=="FW","Sha"]<-"triangle"
#xlim(6,19) + ylim(2,15) + 
#, caption = "Stats taken from Fbref.com",size=3.5
data$GD=(Gls...14)-(xG...26)
t <- ggtitle("Clinicality: Expected Goals vs Goals ")
s <- labs(size=3.5,subtitle="for Big 5 Leagues (13+ goals)", caption = "stats taken from Fbref.com",size=3.5)
xl <- xlab("Goals")
yl <- ylab("Expected Goals")
p <- ggplot(data, aes(x=Gls...14,y=xG...26))+t+xl+yl+s+geom_abline(slope=1, intercept=0, linetype = "dashed", size = 0.5)+geom_abline(slope=1, intercept=-1, linetype = "dashed",size=0.25,color="#e1e7ec")+geom_abline(slope=1, intercept=1, linetype = "dashed",size=0.25,color="#e1e7ec")
p <- p + geom_point(aes(x=Gls...14,y=xG...26,shape=Pos,fill=Colour),size=2,colour=data$Fill) + scale_shape_manual(name="Position", values=c("square","triangle","circle"))+ scale_fill_discrete(guide="none")
p <- p + ggrepel::geom_text_repel(data=filter(data,Gls...14>=15 | xG...26<=10),aes(label=Name),size=2.25,force=1.25)
p <- p + annotate("text", x = 13, y = 25, label = "@CortexFtbl", colour ="red", cex=6,
                  fontface = "bold", alpha = 0.8)
p <- p + annotate("text", x=31, y= 28, label = "perfectly balanced: G=xG", srt= 30, size = 3.5, fontface = "italic")
p <- p + geom_label(label="WASTEFUL",x= 14.5,y=33,size =3,label.size = NA, label.padding = unit(0.09, "lines"),fill="#f54242") + geom_label(label="CLINICAL",x = 33, y = 8,label.size = NA,label.padding = unit(0.09, "lines"),fill="seagreen3", size =3)
p <- p + geom_label(label="Should score more\n based on quality of chances",x= 14.5,y=31.25,size =2.5,label.size = NA, label.padding = unit(0.09, "lines"),fill = "#d0d8e0",check_overlap = T) + geom_label(label="Finishes better than\n chance quality suggests",x = 33, y = 9.75,label.size = NA,label.padding = unit(0.09, "lines"),fill = "#d0d8e0", size =2.5,check_overlap = T)
p <- p + theme(
  panel.background = element_rect(fill = "#a7b8c6",
                                  colour = "#a7b8c6",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
p <- p +  theme(plot.title = element_text(hjust = 0.5)) +  theme(plot.subtitle = element_text(hjust = 0.5))
p<- p + scale_x_continuous(limits=c(11,35),minor_breaks = seq(10, 36, 1))+scale_y_continuous(limits=c(5,35),minor_breaks = seq(4, 36, 1))
p
ggsave('Clinicality 202122.png', width = 9, height = 6)
    