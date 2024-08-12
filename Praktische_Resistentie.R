library(ggplot2)
library(tidyverse)
library(ggpmisc)

#read data
data<-read.csv("data/Verdunningsreeks.csv",sep=";",header=T)
data$PCA_Inv<-1/data$PCA
data$PCA_Inv[which(is.na(data$Protrombinetijd==TRUE))]<-NA

#create scatter plot
#MAN
ggplot(data[which(data$Geslacht=="M"),], aes(x = PCA_Inv, y = Protrombinetijd,color=Geslacht)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(
    aes(label = after_stat(
      paste0(
        "italic(y)~`=`~", round(b_0, 2), "+", round(b_1, 2), "*italic(x)")
    )),
    output.type = "numeric",
    parse = TRUE
  ) +
  labs(title = "Scatter Plot of Protrombinetijd_Inv vs PCA by Geslacht",
       x = "1/PCA",
       y = "Protrombinetijd") +
  theme_minimal()
  #y=6.28+344.91*x --> 10% cutoff = 6.28+344.91*0.1 = 40.771

#VROUW
ggplot(data[which(data$Geslacht=="V"),], aes(x = PCA_Inv, y = Protrombinetijd,color=Geslacht)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(
    aes(label = after_stat(
      paste0(
        "italic(y)~`=`~", round(b_0, 2), "+", round(b_1, 2), "*italic(x)")
    )),
    output.type = "numeric",
    parse = TRUE
  ) +
  labs(title = "Scatter Plot of Protrombinetijd_Inv vs PCA by Geslacht",
       x = "1/PCA",
       y = "Protrombinetijd") +
  theme_minimal()
#y=5.08+435.36*x --> 10% cutoff = 5.08+435.36*0.1 = 48.616

plot<-ggplot(data, aes(x = PCA_Inv, y = Protrombinetijd,color=Geslacht)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  stat_poly_eq(
    aes(label = after_stat(
      paste0(
        "italic(y)~`=`~", round(b_0, 2), "+", round(b_1, 2), "*italic(x)")
    )),
    output.type = "numeric",
    parse = TRUE
  ) +
  labs(title = "",
       x = "inv PCA",
       y = "Protrombinetijd (s)") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 0.1, by = 0.02))+
  scale_y_continuous(breaks = seq(0, 60, by = 10))+
  ylim(0, 60)

#trendlijn v

x<-seq(0,0.115,by=0.01)
y<-5.08+(435.36*x)
v_line<-cbind.data.frame(x,y)

#trendlijn m

x<-seq(0,0.115,by=0.01)
y<-6.28+(344.91*x)
m_line<-cbind.data.frame(x,y)

#cutoff op grafiek laten zien

plot +
  geom_line(data = v_line, aes(x = x, y = y), color = "#00BFC4",linewidth=1)+
  geom_line(data = m_line, aes(x = x, y = y), color = "#F8766D",linewidth=1)


#############################

#analyse praktische resistentie

library(googlesheets4)

#read data bovenschelde
gs4_auth()
data <- read_sheet("https://docs.google.com/spreadsheets/d/1l0n8jdeQe-ZMlBwJ6b-g194EIGROKTqne04Ppsk1oCY/edit?gid=0#gid=0")

data$Resistent<-unlist(data$Resistent)
data$Resistent<-as.numeric(data$Resistent)
table(data$Resistent)

  #proportion of resistant rats
5/69

#read old data
data_old<-read.csv("data/Data_PraktischeResistentie_2003_2010.csv",sep=";",header=T)
table(data_old$log.pca.D)
  
  #proportion of resistant rats
22/(289+22)

#resistance by river basin
table(data_old$Bekken,data_old$log.pca.D)

  #proportion of resistant rats in bovenschelde
2/46 #including non-2010
2/44 #excluding non-2010

#test if difference is signficant
data_old_subset<-subset(data_old, data_old$Bekken=="Bovenschelde")
data_old_subset<-data_old_subset[3:nrow(data_old_subset),] #remove non-2010
data_old_subset$Year<-rep(2010,length.out=nrow(data_old_subset))
data_old_subset$Resistent<-data_old_subset$log.pca.D
data$Year<-rep(2024,length=nrow(data))

Resistent <- c(data_old_subset$Resistent,data$Resistent)
Year<-c(data_old_subset$Year,data$Year)

model<-glm(Resistent ~ Year,family=binomial)
summary(model)

#non significant increase