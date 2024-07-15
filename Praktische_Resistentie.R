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
  scale_x_continuous(breaks = seq(0, 0.1, by = 0.02))

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
  geom_line(data = v_line, aes(x = x, y = y), linetype = "dashed", color = "#00BFC4")+
  geom_line(data = m_line, aes(x = x, y = y), linetype = "dashed", color = "#F8766D")


