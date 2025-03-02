install.packages("tidyverse")
# libraries
library(readr)
library(ggplot2)
library(httpgd)
library(tidyverse)
# install tidyverse

# Setting up ----
# load datasets
wild_df <- read_csv("data/bird_flu_wildbirds.csv")
poultry_df <- read_csv("data/bird_flu_poultry.csv")


# Statistics and Corresponding Visuals ----
# create a contingency table
contingency_table <- table(df$`Flock Size`, df$`Outbreaks`)

# perform Chi-Square test
chi_square_test <- chisq.test(contingency_table)

print(chi_square_test)

# calculate Pearson's correlation coefficient
pearson_correlation <- cor.test(poultry_df$`Flock Size`, poultry_df$`Outbreaks`, method = "pearson")

print(pearson_correlation)


# visual of Pearson's correlation coefficient of Flock Size versus Outbreaks
scatter_plot.1 <- ggplot(df, aes(`Flock Size`, `Outbreaks`)) + geom_point() + labs(x = "Flock Size", y = "Outbreaks") + geom_smooth(method="lm")
ggsave(filename = "FS_vs_Outbreak_plot.jpeg", plot = scatter_plot.1)

scatter_plot.2 <- ggplot(df, aes(`Flock Type`, `Flock Size`)) + geom_point() + labs(x = "Flock Type", y = "Flock Size") + geom_smooth(method="lm") + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "FT_vs_FS_plot.jpeg", plot = scatter_plot.2)


# merging of poultry and wild birds data
colnames(poultry_df)[colnames(poultry_df) == "FIPS Codes"] <- "FIPS"
merged_df <- merge(poultry_df, wild_df, by = "FIPS")

# Pearson's correlation coefficient of Totals and Outbreaks
# calculate Pearson's correlation coefficient
pearson_correlation <- cor.test(merged_df$`Totals`, merged_df$`Outbreaks`, method = "pearson")

# print the result
print(pearson_correlation)



# Creating Visuals by Region ----
# states for different regions

pacific <- c("California", "Oregon", "Washington", "Alaska", "Hawaii")
mountain <- c("Nevada", "Arizona", "Idaho", "Montana", "Wyoming", "Utah", "Colorado", "New Mexico")
west_north_central <- c("North Dakota", "South Dakota", "Minnesota", "Nebraska", "Kansas", "Missouri", "Iowa")
west_south_central <- c("Texas", "Oklahoma", "Arkansas", "Louisiana")
east_north_central <- c("Wisconsin", "Illinois", "Indiana", "Michigan", "Ohio")
east_south_central <- c("Mississippi", "Alabama", "Tennessee", "Kentucky")
middle_atlantic <- c("Pennsylvania", "New York", "New Jersey", "Connecticut", "Maine", "Massachusetts", "New Hampshire", "Vermont")
new_england <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Vermont", "Rhode Island")
south_atlantic <- c("West Virginia", "Virginia", "Maryland", "Delaware", "North Carolina", "South Carolina", "Georgia", "Florida")


# create a function to assign a region based on state
assign_region <- function(state) {
  if (state %in% south_atlantic) {
    return("South Atlantic")
  } 
  else if (state %in% new_england) {
    return("New England")
  } 
  else if (state %in% middle_atlantic) {
    return("Middle Atlantic")
  } 
  else if (state %in% east_north_central) {
    return("East North Central")
  } 
  else if (state %in% east_south_central) {
    return("East South Central")
  } 
  else if (state %in% west_north_central) {
    return("West North Central")
  } 
  else if (state %in% west_south_central) {
    return("West South Central")
  }
  else if (state %in% mountain) {
    return("Mountain")
  } 
  else if (state %in% pacific) {
    return("Pacific")
  }  
    else {
    return("Unknown")  # In case the state doesn't match any region
  }
}

# apply the function to create a new column 'Region'
merged_df$Region <- sapply(merged_df$State.y, assign_region)

# count the number of states in each region
region_count <- table(merged_df$Region)

# create a pie chart
plot.3 <- ggplot(data = as.data.frame(region_count), aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_d(name = "Region", option = "D") +
  labs(title = "State Distribution by Region", x = "Region", y = "Count") +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
    legend.title = element_text(size = 25), 
    legend.text = element_text(size = 13))
ggsave(filename = "pie_plot.jpeg", plot = plot.3, width = 15)