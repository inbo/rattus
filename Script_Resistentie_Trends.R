#Script tabellen
library(tidyverse)
library(reshape2)

# Set your directory path
directory_path <- "data/Tabellen"

# Get the list of all CSV files in the directory
file_list <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

read_and_assign <- function(file_path) {
  # Extract the file name without the extension
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Attempt to read the file with both read.csv and read.csv2
  data <- tryCatch(
    {
      # Try read.csv first (comma-separated, period as decimal)
      read.csv(file_path, header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
    },
    error = function(e) {
      # If read.csv fails, try read.csv2 (semicolon-separated, comma as decimal)
      message(paste("Retrying", file_name, "with read.csv2"))
      read.csv2(file_path, header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
    }
  )
  
  # Assign to a variable in the global environment if successful
  if (!is.null(data)) {
    assign(file_name, data, envir = .GlobalEnv)
    message(paste("Successfully read", file_name))
  }
}

# Apply the function to each file in the list
lapply(file_list, read_and_assign)


#Export EVMC
install.packages("openxlsx")
library(openxlsx)

# Write to Excel
#write.xlsx(M2_table, file = "M2_table.xlsx")




###################################################
resistent_table

# Melt the data for ggplot
resistent_table_melted <- melt(resistent_table, id.vars = "BekkenNaam", variable.name = "Year", value.name = "Value")
resistent_table_melted$Year <- as.numeric(gsub("X", "", resistent_table_melted$Year)) # Clean up year column


# Remove rows with NA values
resistent_table_cleaned <- resistent_table_melted[!is.na(resistent_table_melted$Value), ]

# Create the plot
ggplot(resistent_table_cleaned, aes(x = Year, y = Value, color = BekkenNaam, group = BekkenNaam)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title="Resistentie totaal - geen correctie",
    x = "Jaar",
    y = "Proportie",
    color = "Bekken"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )


#########################################################
resistent_table_corrected$Correction_Factor<-NULL

# Melt the data for ggplot
resistent_table_melted <- melt(resistent_table_corrected, id.vars = "BekkenNaam", variable.name = "Year", value.name = "Value")
resistent_table_melted$Year <- as.numeric(gsub("X", "", resistent_table_melted$Year)) # Clean up year column


# Remove rows with NA values
resistent_table_cleaned <- resistent_table_melted[!is.na(resistent_table_melted$Value), ]

# Create the plot
ggplot(resistent_table_cleaned, aes(x = Year, y = Value, color = BekkenNaam, group = BekkenNaam)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title="Resistentie totaal - correctie",
       x = "Jaar",
       y = "Proportie",
       color = "Bekken"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )

###################################################
M1_table

# Melt the data for ggplot
M1_table_melted <- melt(M1_table, id.vars = "BekkenNaam", variable.name = "Year", value.name = "Value")
M1_table_melted$Year <- as.numeric(gsub("X", "", M1_table_melted$Year)) # Clean up year column


# Remove rows with NA values
M1_table_cleaned <- M1_table_melted[!is.na(M1_table_melted$Value), ]

# Create the plot
ggplot(M1_table_cleaned, aes(x = Year, y = Value, color = BekkenNaam, group = BekkenNaam)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title="M1 - geen correctie",
       x = "Jaar",
       y = "Proportie",
       color = "Bekken"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )


#########################################################
M1_table_corrected$Correction_Factor<-NULL

# Melt the data for ggplot
M1_table_melted <- melt(M1_table_corrected, id.vars = "BekkenNaam", variable.name = "Year", value.name = "Value")
M1_table_melted$Year <- as.numeric(gsub("X", "", M1_table_melted$Year)) # Clean up year column


# Remove rows with NA values
M1_table_cleaned <- M1_table_melted[!is.na(M1_table_melted$Value), ]

# Create the plot
ggplot(M1_table_cleaned, aes(x = Year, y = Value, color = BekkenNaam, group = BekkenNaam)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title="M1 - correctie",
       x = "Jaar",
       y = "Proportie",
       color = "Bekken"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )


###################################################
M2_table

# Melt the data for ggplot
M2_table_melted <- melt(M2_table, id.vars = "BekkenNaam", variable.name = "Year", value.name = "Value")
M2_table_melted$Year <- as.numeric(gsub("X", "", M2_table_melted$Year)) # Clean up year column


# Remove rows with NA values
M2_table_cleaned <- M2_table_melted[!is.na(M2_table_melted$Value), ]

# Create the plot
ggplot(M2_table_cleaned, aes(x = Year, y = Value, color = BekkenNaam, group = BekkenNaam)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title="M2 - geen correctie",
       x = "Jaar",
       y = "Proportie",
       color = "Bekken"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )


#########################################################
M2_table_corrected$Correction_Factor<-NULL

# Melt the data for ggplot
M2_table_melted <- melt(M2_table_corrected, id.vars = "BekkenNaam", variable.name = "Year", value.name = "Value")
M2_table_melted$Year <- as.numeric(gsub("X", "", M2_table_melted$Year)) # Clean up year column


# Remove rows with NA values
M2_table_cleaned <- M2_table_melted[!is.na(M2_table_melted$Value), ]

# Create the plot
ggplot(M2_table_cleaned, aes(x = Year, y = Value, color = BekkenNaam, group = BekkenNaam)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title="M2 - correctie",
       x = "Jaar",
       y = "Proportie",
       color = "Bekken"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )
######
#export for evmc

resistent_table
resistent_table_corrected


######################################################

homozygosity_table<-homozygosity_table[-which(homozygosity_table$BekkenNaam=="BR"),]

# Melt the data for ggplot
homozygosity_table_melted <- melt(homozygosity_table, id.vars = "BekkenNaam", variable.name = "Year", value.name = "Value")
homozygosity_table_melted$Year <- as.numeric(gsub("X", "", homozygosity_table_melted$Year)) # Clean up year column


# Remove rows with NA values
homozygosity_table_cleaned <- homozygosity_table_melted[!is.na(homozygosity_table_melted$Value), ]

# Create the plot
ggplot(homozygosity_table_cleaned, aes(x = Year, y = Value, color = BekkenNaam, group = BekkenNaam)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title="Homozygositeit - Algemeen",
       x = "Jaar",
       y = "Proportie",
       color = "Bekken"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )


######################################################

homozygosity_table_M1

# Melt the data for ggplot
homozygosity_table_melted <- melt(homozygosity_table_M1, id.vars = "BekkenNaam", variable.name = "Year", value.name = "Value")
homozygosity_table_melted$Year <- as.numeric(gsub("X", "", homozygosity_table_melted$Year)) # Clean up year column


# Remove rows with NA values
homozygosity_table_cleaned <- homozygosity_table_melted[!is.na(homozygosity_table_melted$Value), ]

# Create the plot
ggplot(homozygosity_table_cleaned, aes(x = Year, y = Value, color = BekkenNaam, group = BekkenNaam)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title="Homozygositeit - M1",
       x = "Jaar",
       y = "Proportie",
       color = "Bekken"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )

#########################################################

homozygosity_table_M2<-read.csv("data/Tabellen/homozygosity_table_M2.csv",sep=";",dec=",")
homozygosity_table_M2<-homozygosity_table_M2[-which(homozygosity_table_M2$BekkenNaam=="BR"),]

# Melt the data for ggplot
homozygosity_table_melted <- melt(homozygosity_table_M2, id.vars = "BekkenNaam", variable.name = "Year", value.name = "Value")
homozygosity_table_melted$Year <- as.numeric(gsub("X", "", homozygosity_table_melted$Year)) # Clean up year column


# Remove rows with NA values
homozygosity_table_cleaned <- homozygosity_table_melted[!is.na(homozygosity_table_melted$Value), ]

# Create the plot
ggplot(homozygosity_table_cleaned, aes(x = Year, y = Value, color = BekkenNaam, group = BekkenNaam)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title="Homozygositeit - M2",
       x = "Jaar",
       y = "Proportie",
       color = "Bekken"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "right"
  )


