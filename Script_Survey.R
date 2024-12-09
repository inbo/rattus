#####################################################
############  Vragenlijst ###########################
#####################################################

library(readxl)
library(ggplot2)
library(tidyverse)
library(forcats)


# Read data

data <- read_excel("data/Vragenlijst Rattenmonitor INBO_Complete.xlsx", sheet = 1) 
#View(data)

##Plots

colnames(data)

#1 melding openbaar domein  --> actie ja/nee

# Summarize data: count occurrences of each value in the column
data_summary <- data %>%
  group_by(`Wanneer een inwoner melding maakt van ratten op openbaar domein, wordt er dan een actie ondernomen (bestrijding, uitdelen van rodenticiden, preventieve maatregelen, informatie verstrekken)?`) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Clean up the data
colnames(data_summary)[1] <- "answer"
data_summary <- data_summary %>%
  mutate(answer = ifelse(answer %in% c("Nee", 
                                       "Ja, door personeel van de gemeente",
                                       "Ja, door een externe firma in opdracht van de gemeente",
                                       "We krijgen zulke meldingen niet"), answer, "Ander"))
data_summary$percentage[which(data_summary$answer=="Ander")]<-sum(data_summary$percentage[which(data_summary$answer=="Ander")])

data_summary <- data_summary %>%
  group_by(answer) %>%
  filter(answer != "Ander" | row_number() == 1) %>%
  ungroup()

data_summary$answer <- factor(data_summary$answer, 
                              levels = c("Ja, door een externe firma in opdracht van de gemeente", 
                                         "Ja, door personeel van de gemeente", 
                                         "Nee",
                                         "We krijgen zulke meldingen niet", 
                                         "Ander"))


# Reorder levels
data_summary$answer <- fct_relevel(data_summary$answer,"Ja, door een externe firma in opdracht van de gemeente", 
                                   "Ja, door personeel van de gemeente", 
                                   "Nee",
                                   "We krijgen zulke meldingen niet", 
                                   "Ander")


# Create the pie chart
ggplot(data_summary, aes(x = "", y = percentage, fill = answer)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove background, axes, and grid
  guides(fill = guide_legend(title = NULL))+
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(legend.text = element_text(size = 12))

# Reverse the factor levels for fill (colors) without altering the actual data order
data_summary$answer_fill <- fct_rev(data_summary$answer)

# Create the stacked horizontal bar chart with consistent stacking and reversed colors
plot_melding_openbaar<-ggplot(data_summary, aes(x = 1, y = percentage, fill = answer_fill)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +  # Flip the coordinates for a horizontal bar chart
  theme_minimal() +  # Use a clean minimal theme
  labs(x = NULL, y = NULL, fill = NULL) +  # Remove axis labels and legend title
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4) +  # Ensure labels are positioned correctly
  theme(legend.position = "bottom",  # Move legend to the bottom
        axis.text = element_blank(),  # Remove axis text
        axis.ticks = element_blank())  # Remove axis ticks

#1 melding privaat terrein --> actie ja/nee

# Summarize data: count occurrences of each value in the column
data_summary <- data %>%
  group_by(`Wanneer een inwoner melding maakt van ratten op privaat terrein, wordt er dan een actie ondernomen (bestrijding, uitdelen van rodenticiden, preventieve maatregelen, informatie verstrekken)?`) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Clean up the data
colnames(data_summary)[1] <- "answer"
data_summary <- data_summary %>%
  mutate(answer = ifelse(answer %in% c("Nee", 
                                       "Ja, door personeel van de gemeente",
                                       "Ja, door een externe firma in opdracht van de gemeente",
                                       "We krijgen zulke meldingen niet"), answer, "Ander"))
data_summary$percentage[which(data_summary$answer=="Ander")]<-sum(data_summary$percentage[which(data_summary$answer=="Ander")])

data_summary <- data_summary %>%
  group_by(answer) %>%
  filter(answer != "Ander" | row_number() == 1) %>%
  ungroup()

data_summary$answer <- factor(data_summary$answer, 
                              levels = c("Ja, door een externe firma in opdracht van de gemeente", 
                                         "Ja, door personeel van de gemeente", 
                                         "Nee",
                                         "We krijgen zulke meldingen niet", 
                                         "Ander"))


# Reorder levels
data_summary$answer <- fct_relevel(data_summary$answer,"Ja, door een externe firma in opdracht van de gemeente", 
                                   "Ja, door personeel van de gemeente", 
                                   "Nee",
                                   "We krijgen zulke meldingen niet", 
                                   "Ander")


# Create the pie chart
ggplot(data_summary, aes(x = "", y = percentage, fill = answer)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove background, axes, and grid
  guides(fill = guide_legend(title = NULL))+
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(legend.text = element_text(size = 12))

# Reverse the factor levels for fill (colors) without altering the actual data order
data_summary$answer_fill <- fct_rev(data_summary$answer)

# Create the stacked horizontal bar chart with consistent stacking and reversed colors
plot_melding_privaat<-ggplot(data_summary, aes(x = 1, y = percentage, fill = answer_fill)) +
  geom_bar(stat = "identity", width = 0.5) +
  coord_flip() +  # Flip the coordinates for a horizontal bar chart
  theme_minimal() +  # Use a clean minimal theme
  labs(x = NULL, y = NULL, fill = NULL) +  # Remove axis labels and legend title
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4) +  # Ensure labels are positioned correctly
  theme(legend.position = "bottom",  # Move legend to the bottom
        axis.text = element_blank(),  # Remove axis text
        axis.ticks = element_blank())  # Remove axis ticks

#2

# Summarize data: count occurrences of each value in the column
data_summary <- data %>%
  filter(`Hoe worden deze gegevens bijgehouden binnen de gemeente?...19` != "NA") %>%  # Drop rows where the answer is "NA"
  group_by(`Hoe worden deze gegevens bijgehouden binnen de gemeente?...19`) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Clean up the data
colnames(data_summary)[1] <- "answer"

data_summary <- data_summary %>%
  mutate(answer = ifelse(answer %in% c("In een digitaal registratiesysteem waar enkel de melding wordt bijgehouden, zonder eventuele acties die hieraan gekoppeld zijn", 
                                       "In een digitaal registratiesysteem waar zowel de melding als de actie die hieraan gekoppeld zijn worden bijgehouden", "Op papier, waar enkel de melding wordt bijgehouden, zonder eventuele acties die hieraan gekoppeld zijn", "Op papier, waar zowel de melding als de hieraan gekoppelde acties worden bijgehouden",
                                       "Deze worden niet geregistreerd"), answer, "Ander"))

data_summary$percentage[which(data_summary$answer=="Ander")]<-sum(data_summary$percentage[which(data_summary$answer=="Ander")])

data_summary <- data_summary %>%
  group_by(answer) %>%
  filter(answer != "Ander" | row_number() == 1) %>%
  ungroup()

# Create the pie chart
ggplot(data_summary, aes(x = "", y = percentage, fill = answer)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove background, axes, and grid
  guides(fill = guide_legend(title = NULL))+
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(legend.text = element_text(size = 8.5))

#3

# Summarize data: count occurrences of each value in the column
data_summary <- data %>%
  filter(`Zou u bereid zijn om deze gegevens door te geven aan INBO voor de rattenmonitortool?` != "NA") %>%  # Drop rows where the answer is "NA"
  group_by(`Zou u bereid zijn om deze gegevens door te geven aan INBO voor de rattenmonitortool?`) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Clean up the data
colnames(data_summary)[1] <- "answer"
#View(data_summary)
data_summary <- data_summary %>%
  mutate(answer = ifelse(answer %in% c("Ja","Nee"), answer, "Ander"))

data_summary$percentage[which(data_summary$answer=="Ander")]<-sum(data_summary$percentage[which(data_summary$answer=="Ander")])

data_summary <- data_summary %>%
  group_by(answer) %>%
  filter(answer != "Ander" | row_number() == 1) %>%
  ungroup()

# Create the pie chart
ggplot(data_summary, aes(x = "", y = percentage, fill = answer)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +  # Remove background, axes, and grid
  guides(fill = guide_legend(title = NULL))+
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme(legend.text = element_text(size = 12))







