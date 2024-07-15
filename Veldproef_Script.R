library(ggplot2)
library(sf)
library(reshape2)
library(tidyverse)

#MOLENS GEYSKENS

#read data
getwd()
data<-read.csv("data/Data_Opname.csv",sep=";",header=T)
data$X<-NULL

data$Mass[which(data$Mass>70)]<-NA
data<-na.omit(data)


data$Date<-as.POSIXct(strptime(data$Date, format = "%d/%m/%Y"),tz="UTC")

ggplot(data,aes(x=Date,y=Fraction,group=as.factor(ID),colour=as.factor(ID)))+
  geom_line()+
  scale_color_discrete(name = "Baitbox")

ggplot(data,aes(x=Date,y=Mass,group=as.factor(ID),colour=as.factor(ID)))+
  geom_line(linewidth=1)+
  scale_color_discrete(name = "Baitbox")


# Natlandhoeve Ulbeek

data<-read.csv("data/Opname_Ulbeek.csv",sep=";",header=T)
colnames(data)<-c("Lokaaspunt","Moment0","Moment1","Moment2","Moment3","Moment4",
                  "Moment1","Moment2","Moment3","Moment4","Moment5","Moment6","Moment7",
                  "Moment1","Moment2")
data_pre<-data[,c(1,2:6)]
data_bestrijding<-data[,c(1,7:13)]
data_post<-data[,c(1,14:15)]

data_pre <- melt(data_pre, id.vars = "Lokaaspunt", variable.name = "Moment", value.name = "Gewicht")
data_pre$Week<-gsub("[^0-9]", "", data_pre$Moment)


data_bestrijding <- melt(data_bestrijding, id.vars = "Lokaaspunt", variable.name = "Moment", value.name = "Gewicht")
data_bestrijding$Dag<-gsub("[^0-9]", "", data_bestrijding$Moment)
start<-cbind.data.frame(seq(from=1,to=23,by=1),rep("a",length.out=23),rep(40,length.out=23),rep(0,length.out=23))
colnames(start)<-colnames(data_bestrijding)
data_bestrijding<-rbind.data.frame(data_bestrijding,start)

data_post <- melt(data_post, id.vars = "Lokaaspunt", variable.name = "Moment", value.name = "Gewicht")
data_post$Week<-gsub("[^0-9]", "", data_post$Moment)
start<-cbind.data.frame(seq(from=1,to=23,by=1),rep("a",length.out=23),rep(40,length.out=23),rep(0,length.out=23))
colnames(start)<-colnames(data_post)
data_post<-rbind.data.frame(data_post,start)

#Barplot bestrijding

summary_data <- data_bestrijding %>%
  group_by(Dag) %>%
  summarise(
    mean_gewicht = mean(Gewicht),
    sd_gewicht = sd(Gewicht),
    se_gewicht = sd(Gewicht) / sqrt(n())
  )

ggplot(summary_data, aes(x = Dag, y = mean_gewicht)) +
  geom_bar(stat = "identity", fill = "#F8766D") +
  geom_errorbar(aes(ymin = mean_gewicht - se_gewicht, ymax = mean_gewicht + se_gewicht), width = 0.2) +
  labs(title = "Bestrijding",
       x = "Dag",
       y = "Gemiddeld gewicht lokaas (g)") +
  theme_minimal()

#Barplot census

summary_data_pre <- na.omit(data_pre) %>%
  group_by(Week) %>%
  summarise(
    mean_gewicht = mean(Gewicht),
    sd_gewicht = sd(Gewicht),
    se_gewicht = sd(Gewicht) / sqrt(n())
  )

plot_pre<-ggplot(summary_data_pre, aes(x = Week, y = mean_gewicht)) +
  geom_bar(stat = "identity", fill = "#00BFC4") +
  geom_errorbar(aes(ymin = mean_gewicht - se_gewicht, ymax = mean_gewicht + se_gewicht), width = 0.2) +
  labs(title = "Pre-Census",
       x = "Week",
       y = "Gemiddeld gewicht lokaas (g)") +
  theme_minimal()

summary_data_post <- na.omit(data_post) %>%
  group_by(Week) %>%
  summarise(
    mean_gewicht = mean(Gewicht),
    sd_gewicht = sd(Gewicht),
    se_gewicht = sd(Gewicht) / sqrt(n())
  )

add<-summary_data_post[1:2,]
add$Week<-c(3,4)
add$mean_gewicht<-c(0,0)
add$sd_gewicht<-c(0,0)
add$se_gewicht<-c(0,0)
summary_data_post<-rbind.data.frame(summary_data_post,add)

plot_post<-ggplot(summary_data_post, aes(x = Week, y = mean_gewicht)) +
  geom_bar(stat = "identity", fill = "#00BFC4") +
  geom_errorbar(aes(ymin = mean_gewicht - se_gewicht, ymax = mean_gewicht + se_gewicht), width = 0.2) +
  labs(title = "Post-Census",
       x = "Week                            ",
       y = "") +
  theme_minimal()

library(gridExtra)
grid.arrange(plot_pre, plot_post, ncol = 2)
