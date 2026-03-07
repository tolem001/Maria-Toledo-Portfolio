# Assignment: Week 5/6
# Name: Toledo, Maria
# Date: 2025-04-20


## Check your current working directory using `getwd()`
getwd()

## List the contents of the working directory with the `dir()` function
dir()

## If the current directory does not contain the `data` directory, set the
## working directory to project root folder (the folder should contain the `data` directory
## Use `setwd()` if needed
setwd("C:/Users/rmtol/OneDrive/Desktop/Bellevue/DCS 640")

## Load the 4 files using read.csv
milwaukie_df <- read.csv("KiaHyundaiMilwaukeeData.csv")
head(milwaukie_df)

theftsmap_df <- read.csv("carTheftsMap.csv")
head(theftsmap_df)

kiahyundaithefts_df <- read.csv("kiaHyundaiThefts.csv")
head(kiahyundaithefts_df)

install.packages("readxl")
library(readxl) 

install.packages("tidyverse")  
library(tidyverse)

## Load file using read_excel
motherboard_df <- read_excel("C:/Users/rmtol/OneDrive/Desktop/Bellevue/DCS 640/Motherboard VICE News Kia Hyundai Theft Data1.xlsx")
head(motherboard_df)

# Filter for specific years
kia_2019 <- kiahyundaithefts_df %>%
  filter(year == 2019)
kia_2020 <- kiahyundaithefts_df %>%
  filter(year == 2020)
kia_2021 <- kiahyundaithefts_df %>%
  filter(year == 2021)
kia_2022 <- kiahyundaithefts_df %>%
  filter(year == 2022)

# Calculate yearly thefts
yearly_thefts <- data.frame(
  year = c(2019, 2020, 2021, 2022),
  total_theftsKh = c(
    sum(kia_2019$countKiaHyundaiThefts, na.rm = TRUE),
    sum(kia_2020$countKiaHyundaiThefts, na.rm = TRUE),
    sum(kia_2021$countKiaHyundaiThefts, na.rm = TRUE),
    sum(kia_2022$countKiaHyundaiThefts, na.rm = TRUE)
  )
)

# Visualize thefts over time from 2019 to 2022

ggplot(yearly_thefts, aes(x = year, y = total_theftsKh)) +
  geom_line(linewidth = 1, color = "black") +
  geom_point(size = 2, color = "red") +
  labs(title = "Yearly Theft Trends",
       x = "Year", y = "Total Kia/Hyundai Thefts") +
  theme_minimal()

# Summarize theft counts by state

# First. combine two datasets into one to compare all states since Milwaukie is separate

combined_df <- bind_rows(milwaukie_df, kiahyundaithefts_df)
state_thefts <- combined_df %>%
  group_by(state) %>%
  summarize(total_theftsKh = sum(countKiaHyundaiThefts))

ggplot(state_thefts, aes(x = reorder(state, -total_theftsKh), y = total_theftsKh)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Kia and Hyundai Thefts by State",
       x = "State", y = "Total Thefts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summarize all theft data

total_theftsAll <- kiahyundaithefts_df %>%
  summarize(
    total_kia_hyundai = sum(countKiaHyundaiThefts, na.rm = TRUE),
    total_other = sum(countOtherThefts, na.rm = TRUE)
  )

# Reshape the data for the pie chart
pie_data <- total_theftsAll %>%
  pivot_longer(cols = everything(), names_to = "Theft_Type", values_to = "Count") %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the pie chart to compare Kia/Hyundai with other makes

ggplot(pie_data, aes(x = "", y = Percentage, fill = Theft_Type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Breakdown of Kia/Hyundai Thefts vs. Other Car Brand Thefts",
       fill = "Theft Type") +
  theme_void() +  
  scale_fill_manual(values = c("pink", "grey")) 

# Summarize the data for 2022 for donut to compare Kia/Hyundai with other makes

donut_data <- kiahyundaithefts_df %>%
  filter(year == 2022) %>%
  summarize(
    total_kia_hyundai = sum(countKiaHyundaiThefts, na.rm = TRUE),
    total_other = sum(countOtherThefts, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Theft_Type", values_to = "Count") %>%
  mutate(Percentage = Count / sum(Count) * 100)


ggplot(donut_data, aes(x = 2, y = Percentage, fill = Theft_Type)) + 
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) + 
  labs(title = "Breakdown of Kia/Hyundai Thefts vs. Other Thefts in 2022", fill = "Theft Type") +
  theme_void() +
  scale_fill_manual(values = c("forestgreen", "springgreen"))

install.packages("treemap")
library(treemap)

# Reshape the data for a tree map to show thefts by city

tree_data <- kiahyundaithefts_df %>%
  pivot_longer(cols = c(countKiaHyundaiThefts, countOtherThefts),
               names_to = "Theft_Type",
               values_to = "Theft_Count")

treemap(tree_data,
        index = c("Theft_Type", "state", "city"),  
        vSize = "Theft_Count",                    
        vColor = "Theft_Type",                    
        type = "categorical",                     
        title = "Tree Map of Kia/Hyundai Thefts vs. Other Thefts by Location",
        palette = "Set2") 


# Create an area chart for Kia and Hyundai thefts by month for Milwaukie

# Combine month and year into a single time column
milwaukie_df <- milwaukie_df %>%
  mutate(time = paste(month, year)) 



ggplot(milwaukie_df, aes(x = time, y = countKiaHyundaiThefts, group = 1)) +
  geom_area(fill = "green", alpha = 0.6) +
  labs(title = "Kia/Hyundai Thefts Over Time in Milwaukee (Spanning Multiple Years)",
       x = "Time", y = "Count of Kia/Hyundai Thefts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Show as Quarter and Year
milwaukie_df <- milwaukie_df %>%
  mutate(quarter = paste("Q", ceiling(as.numeric(factor(month)) / 3), year))

ggplot(milwaukie_df, aes(x = quarter, y = countKiaHyundaiThefts, group = 1)) +
  geom_area(fill = "salmon", alpha = 0.6) +
  labs(title = "Kia Thefts Over Time in Milwaukee (Grouped by Quarter)",
       x = "Quarter", y = "Count of Kia/Hyundai Thefts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summarize city totals

city_totals <- combined_df %>%
  group_by(city) %>%
  summarize(total_thefts = sum(countKiaHyundaiThefts, na.rm = TRUE)) %>%
  arrange(desc(total_thefts))  

# Remove bottom 5 cities to show top cities with highest theft

filtered_city_totals <- city_totals %>%
  slice_head(n = nrow(city_totals) - 5) 

filtered_combined_df <- combined_df %>%
  filter(city %in% filtered_city_totals$city)  

ggplot(filtered_combined_df, aes(x = reorder(city, -total_theftsAll), y = countKiaHyundaiThefts, fill = city)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Kia/Hyundai Thefts by City",
       x = "City", y = "Total Thefts", fill = "City") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter for Milwaukie
milwaukie_data <- theftsmap_df %>%
  filter(geo_name == "Milwaukie PD")

neighborhood_thefts <- milwaukie_data %>%
  group_by(geo_name) %>%
  summarize(total_theftsM = sum(countCarThefts2022, na.rm = TRUE)) %>%
  arrange(desc(total_theftsM))  

install.packages("sf")
library(sf)

# Convert to spatial data
milwaukie_sf <- st_as_sf(milwaukie_data, coords = c("longitude", "latitude"), crs = 4326)

# Plot theft hotspot in Milwaukie
ggplot() +
  geom_sf(data = milwaukie_sf, aes(size = countCarThefts2022, color = countCarThefts2022)) +
  scale_color_viridis_c() +
  labs(title = "Theft Hotspot in Milwaukie",
       size = "Theft Count", color = "Theft Count") +
  theme_minimal()


# Filter for specific years for Milwaukie

kia_2019M <- milwaukie_df %>%
  filter(year == 2019)
kia_2020M <- milwaukie_df %>%
  filter(year == 2020)
kia_2021M <- milwaukie_df %>%
  filter(year == 2021)
kia_2022M <- milwaukie_df %>%
  filter(year == 2022)

# Calculate yearly thefts for Milwaukie

yearly_theftsM <- data.frame(
  year = c(2019, 2020, 2021, 2022),
  total_theftsKhM = c(
    sum(kia_2019M$countKiaHyundaiThefts, na.rm = TRUE),
    sum(kia_2020M$countKiaHyundaiThefts, na.rm = TRUE),
    sum(kia_2021M$countKiaHyundaiThefts, na.rm = TRUE),
    sum(kia_2022M$countKiaHyundaiThefts, na.rm = TRUE)
  )
)

# Create pie chart

ggplot(yearly_theftsM, aes(x = "", y = total_theftsKhM, fill = factor(year))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Yearly Kia/Hyundai Thefts in Milwaukee from 2019-2022",
       fill = "Year") +
  theme_void() +
  scale_fill_manual(values = c("hotpink", "orchid", "pink", "darkslateblue"))
