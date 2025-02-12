# Importing necessary libraries
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)

data <- read.csv("C:/Users/user/Desktop/Dataset.csv")
View(data)

# Cleaning the Data ####

# Removing unnecessary columns:
data <- data %>% select(-GEO,-DGUID, -UOM, -UOM_ID, -SCALAR_FACTOR, 
                        -SCALAR_ID, -STATUS, -SYMBOL, - TERMINATED, -DECIMALS,-COORDINATE,-VECTOR)

# View the updated dataset
view(data)


# Convert the Date column from yyyy-mm to yyyy-mm-dd
data$DATE <- as.Date(paste0(data$REF_DATE, "-01"), "%Y-%m-%d")

str(data)

# Deleting REF_DATE variable
data <- data %>% select(-REF_DATE)

view(data)

write.csv(data, "casestudy2new.csv", row.names = FALSE) # cleaned data set which we will use for our questions below


data <- data %>% 
  rename(PRODUCTS = Products.and.product.groups)

view(data)





# Filter data for the past 61 months
start_date <- max(data$DATE) %m-% months(60)
data <- data %>% filter(DATE >= start_date)

# Add Quarter and Year columns
data <- data %>%
  mutate(Quarter = paste0(format(DATE, "%Y"), "-Q", ceiling(as.numeric(format(DATE, "%m")) / 3)))

# Calculate Overall CPI (Quarterly Rate of Change)
overall_cpi <- data %>%
  group_by(Quarter) %>%
  summarise(CPI = mean(VALUE)) %>%
  mutate(Quarterly_Change = (CPI - lag(CPI)) / lag(CPI) * 100)

print(overall_cpi)

# Calculate CPI Excluding Food and Energy
cpi_ex_food_energy <- data %>%
  filter(PRODUCTS  == "All-items excluding food and energy") %>%
  group_by(Quarter) %>%
  summarise(CPI = mean(VALUE)) %>%
  mutate(Quarterly_Change = (CPI - lag(CPI)) / lag(CPI) * 100)

print(cpi_ex_food_energy)


# Plot the data

# Combine the data for plotting 
overall_cpi$type <- "Overall CPI" 
cpi_ex_food_energy$type <- "CPI Excluding Food and Energy" 

combined_data <- bind_rows(overall_cpi, cpi_ex_food_energy) 

# Ensure Quarter is treated as a time variable
combined_data <- combined_data %>%
  mutate(
    YearQuarter = paste0(sub("-Q.*", "", Quarter), " Q", sub(".*Q", "", Quarter)), # Create labels like "2020 Q1"
    Quarterly_Change = as.numeric(Quarterly_Change)
  ) %>%
  arrange(YearQuarter)

# Create the plot

ggplot(combined_data, aes(x = YearQuarter, y = Quarterly_Change, color = type, group = type)) +
  geom_line(size = 1) +
  labs(
    title = "Quarterly Changes in CPI",
    x = "Quarter",
    y = "Quarterly Change (%)",
    color = "CPI Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # adjust x-axis labels



# Load the data
file_path <- "C:/Users/user/Desktop/casestudy2new.csv"
data <- read.csv(file_path)



# Convert DATE to Date type and arrange
data$DATE <- as.Date(data$DATE)
data <- data %>% arrange(DATE)

# Ensure we have at least 30 months of data (12 months for YoY + 18 months for plotting)
categories <- c("Food", "Energy", "Shelter")
data_for_calc <- data %>%
  filter(Products.and.product.groups %in% categories) %>%
  filter(DATE >= as.Date("2022-04-01") & DATE <= as.Date("2024-09-30"))

# Compute the required labels for each point
data_for_calc <- data_for_calc %>%
  group_by(Products.and.product.groups) %>%
  mutate(
    Current = VALUE,
    MoM = lag(VALUE, 1),
    YoY = lag(VALUE, 12),
    Label = ifelse(!is.na(MoM) & !is.na(YoY), 
                   paste0("Current: ", round(Current, 1), 
                          "\nMoM: ", round(MoM, 1), 
                          "\nYoY: ", round(YoY, 1)),
                   NA)
  ) %>%
  ungroup()

# Filter for the last 18 months for plotting
data_for_plot <- data_for_calc %>%
  filter(DATE >= as.Date("2023-04-01") & DATE <= as.Date("2024-09-30"))

# Updated Plot Code with Monthly Labels on x-axis
line_plot <- ggplot(data_for_plot, aes(x = DATE, y = VALUE, color = Products.and.product.groups)) +
  geom_line(size = 1) +
  geom_point() +
  geom_text(aes(label = Label), vjust = -1, size = 3, check_overlap = TRUE) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + # Add each month on the x-axis
  labs(
    title = "CPI Trends for Food, Shelter, and Energy (Past 18 Months)",
    subtitle = "Each point shows Current, MoM, and YoY values",
    x = "Date",
    y = "CPI Value",
    color = "Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability

# Print the updated plot
print(line_plot)


