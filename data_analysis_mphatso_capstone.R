#=====================
# STEP 1: COLLECT DATA
#=====================

# Loading necessary libraries
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)


# Setting the file path
file_path <- "C:/Users/mphat/OneDrive/Desktop/Mphatso_capston_Project/"

# Loading each dataset into a separate data frame
august_2023 <- read_csv(paste0(file_path, "cleaned_cyclistic_bike_data.csv"))

#====================================================
# STEP 2: WRANGLING DATA AND COMBINING IT INTO A SINGLE FILE
#====================================================

# Combining all the data frames into one main data frame
cyclistic_bike_data <- bind_rows(august_2023)

#=======================================================
# STEP 3: CLEANING UP AND ADDING DATA TO PREPARE FOR ANALYSIS
#======================================================

# Finding matching station IDs where end_station_id matches start_station_id
matching_station_ids <- cyclistic_bike_data %>%
  filter(end_station_id %in% start_station_id) %>%
  select(end_station_id, start_station_id, end_station_name, start_station_name) %>%
  distinct()

# Creating a lookup table for end_station_id to end_station_name
end_station_lookup <- matching_station_ids %>%
  select(end_station_id, end_station_name) %>%
  distinct()

# Creating a lookup table for start_station_id to start_station_name
start_station_lookup <- matching_station_ids %>%
  select(start_station_id, start_station_name) %>%
  distinct()

# Checking for duplicate entries in lookup tables
end_station_lookup %>%
  group_by(end_station_id) %>%
  filter(n() > 1) %>%
  distinct()

start_station_lookup %>%
  group_by(start_station_id) %>%
  filter(n() > 1) %>%
  distinct()

# Removing duplicates by keeping the first occurrence
end_station_lookup_clean <- end_station_lookup %>%
  distinct(end_station_id, .keep_all = TRUE)

start_station_lookup_clean <- start_station_lookup %>%
  distinct(start_station_id, .keep_all = TRUE)

# Using the cleaned lookup tables to fill missing values
cyclistic_bike_data <- cyclistic_bike_data %>%
  left_join(end_station_lookup_clean, by = "end_station_id", suffix = c("", "_lookup")) %>%
  mutate(
    end_station_name = ifelse(is.na(end_station_name), end_station_name_lookup, end_station_name)
  ) %>%
  select(-end_station_name_lookup)

cyclistic_bike_data <- cyclistic_bike_data %>%
  left_join(start_station_lookup_clean, by = "start_station_id", suffix = c("", "_lookup")) %>%
  mutate(
    start_station_name = ifelse(is.na(start_station_name), start_station_name_lookup, start_station_name)
  ) %>%
  select(-start_station_name_lookup)

# Dropping rows with NA values in all columns except for start_station_name
cyclistic_bike_data_cleaned <- cyclistic_bike_data %>%
  filter(
    !is.na(end_station_name) &
      !is.na(end_station_id) &
      !is.na(end_lat) &
      !is.na(end_lng) &
      !is.na(start_station_id) &
      !is.na(start_lat) &
      !is.na(start_lng)
  )

# Saving the number of rows after cleaning
cleaned_row_count <- nrow(cyclistic_bike_data_cleaned)
print(paste("Number of rows remaining after cleaning:", cleaned_row_count))

# Counting the number of NA values in the start_station_name column
na_start_station_name_count <- sum(is.na(cyclistic_bike_data_cleaned$start_station_name))
print(paste("Number of NA values in start_station_name:", na_start_station_name_count))

# Checking for NA values in the dataset
na_summary <- cyclistic_bike_data_cleaned %>%
  summarize_all(~sum(is.na(.)))
print(na_summary)

# Creating the ride_length column
cyclistic_bike_data_cleaned <- cyclistic_bike_data_cleaned %>%
  mutate(ride_length = ended_at - started_at)

# Creating the day_of_week column
cyclistic_bike_data_cleaned <- cyclistic_bike_data_cleaned %>%
  mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE))

# Filtering the dataset for trips with a ride length of 0 or less than 0
zero_or_negative_ride_length <- cyclistic_bike_data_cleaned %>%
  filter(ride_length <= 0)
View(zero_or_negative_ride_length)
print(head(zero_or_negative_ride_length))

# Dropping all rides with a ride length equal to or less than 0 seconds
cyclistic_bike_data_cleaned <- cyclistic_bike_data_cleaned %>%
  filter(ride_length > 0)

# Dropping rides with a duration of 10 seconds or less (optional)
cyclistic_bike_data_cleaned <- cyclistic_bike_data_cleaned %>%
  filter(ride_length > 10)

# Printing the number of rows remaining after filtering
print(paste("Number of rows remaining after filtering:", nrow(cyclistic_bike_data_cleaned)))

# Calculating the mean of ride_length
mean_ride_length <- mean(cyclistic_bike_data_cleaned$ride_length, na.rm = TRUE)

# Calculating the max of ride_length
max_ride_length <- max(cyclistic_bike_data_cleaned$ride_length, na.rm = TRUE)

# Calculating the mode of day_of_week
mode_day_of_week <- cyclistic_bike_data_cleaned %>%
  group_by(day_of_week) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1) %>%
  pull(day_of_week)

# Printing the results
print(paste("Mean ride length:", mean_ride_length))
print(paste("Max ride length:", max_ride_length))
print(paste("Mode day of the week:", mode_day_of_week))

#=====================================
# STEP 4: CONDUCTING DESCRIPTIVE ANALYSIS
#=====================================

# Calculating the proportional share of rides by member type
ride_share_by_member_type <- cyclistic_bike_data_cleaned %>%
  group_by(member_casual) %>%
  summarize(total_rides = n()) %>%
  mutate(proportion = total_rides / sum(total_rides))

# Printing the calculated proportions
print(ride_share_by_member_type)

# Visualizing the proportional share using a pie chart
ggplot(ride_share_by_member_type, aes(x = "", y = proportion, fill = member_casual)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(title = "Proportional Share of Rides by Member Type", 
       fill = "Member Type") +
  geom_text(aes(label = scales::percent(proportion)), 
            position = position_stack(vjust = 0.5),
            color = "white")

# ANALYZING SHORT RIDES
# Filter rides with a ride length of less than 60 seconds
short_rides <- cyclistic_bike_data_cleaned %>%
  filter(ride_length < 60)

# Grouping by member type and summarize the count of short rides
short_rides_summary <- short_rides %>%
  group_by(member_casual) %>%
  summarize(count = n())

# Printing the summary to see the number of short rides by member type
print(short_rides_summary)

# Calculating the number of rides for each rideable_type by member type
rides_by_rideable_type_and_member_type <- cyclistic_bike_data_cleaned %>%
  group_by(member_casual, rideable_type) %>%
  summarize(ride_count = n(), .groups = 'drop')

# Printing the result to check
print(rides_by_rideable_type_and_member_type)

# Plotting a bar chart of rideable_type by member type
ggplot(rides_by_rideable_type_and_member_type, aes(x = rideable_type, y = ride_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Rides by Rideable Type and Member Type", 
       x = "Rideable Type", 
       y = "Number of Rides") +
  theme_minimal()

#=====================================
# STEP 5: ANALYZING TRENDS AND INSIGHTS
#=====================================

# Calculating average ride_length for users by day_of_week
average_ride_length_by_day <- cyclistic_bike_data_cleaned %>%
  group_by(day_of_week, member_casual) %>%
  summarise(average_ride_length = mean(ride_length, na.rm = TRUE))

# Printing the summary to check the results
print(average_ride_length_by_day)

# Creating a bar plot to show average ride length by day of the week
ggplot(average_ride_length_by_day, aes(x = day_of_week, y = average_ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Ride Length by Day of the Week and Member Type", 
       x = "Day of Week", 
       y = "Average Ride Length (seconds)") +
  theme_minimal()

# Adding the hour_of_day column to the dataset
cyclistic_bike_data_cleaned <- cyclistic_bike_data_cleaned %>%
  mutate(hour_of_day = hour(started_at))

# Now analyzing the number of rides by hour of the day
hourly_rides <- cyclistic_bike_data_cleaned %>%
  group_by(hour_of_day, member_casual) %>%
  summarize(ride_count = n(), .groups = 'drop')

print(hourly_rides)

# Plotting the number of rides by hour of the day
ggplot(hourly_rides, aes(x = hour_of_day, y = ride_count, color = member_casual, group = member_casual)) +
  geom_line() +
  labs(title = "Number of Rides by Hour of the Day", x = "Hour of Day", y = "Number of Rides") +
  theme_minimal()

# Calculating the most popular start stations
popular_start_stations <- cyclistic_bike_data_cleaned %>%
  group_by(start_station_name) %>%
  summarize(ride_count = n(), .groups = 'drop') %>%
  arrange(desc(ride_count))

# Top 10 most popular start stations
top_start_stations <- popular_start_stations %>%
  top_n(10, ride_count)

# Plot for start stations
ggplot(top_start_stations, aes(x = reorder(start_station_name, ride_count), y = ride_count, fill = start_station_name)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates to make the station names readable
  labs(title = "Top 10 Most Popular Start Stations", 
       x = "Start Station Name", 
       y = "Number of Rides") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend for clarity

# Calculate the most popular end stations
popular_end_stations <- cyclistic_bike_data_cleaned %>%
  group_by(end_station_name) %>%
  summarize(ride_count = n(), .groups = 'drop') %>%
  arrange(desc(ride_count))

# Top 10 most popular end stations
top_end_stations <- popular_end_stations %>%
  top_n(10, ride_count)

# Plot for end stations
ggplot(top_end_stations, aes(x = reorder(end_station_name, ride_count), y = ride_count, fill = end_station_name)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates to make the station names readable
  labs(title = "Top 10 Most Popular End Stations", 
       x = "End Station Name", 
       y = "Number of Rides") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend for clarity

# Calculate the most popular start stations by member type
popular_start_stations_by_member <- cyclistic_bike_data_cleaned %>%
  group_by(member_casual, start_station_name) %>%
  summarize(ride_count = n(), .groups = 'drop') %>%
  arrange(member_casual, desc(ride_count))

# Top 10 most popular start stations for each member type
top_start_stations_by_member <- popular_start_stations_by_member %>%
  group_by(member_casual) %>%
  slice_max(order_by = ride_count, n = 10)

# Plot for start stations by member type
ggplot(top_start_stations_by_member, aes(x = reorder(start_station_name, ride_count), y = ride_count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ member_casual) +  # Create separate plots for each member type
  coord_flip() +  # Flip coordinates to make the station names readable
  labs(title = "Top 10 Start Stations by Member Type", 
       x = "Start Station Name", 
       y = "Number of Rides") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend for clarity

# Calculate the most popular end stations by member type
popular_end_stations_by_member <- cyclistic_bike_data_cleaned %>%
  group_by(member_casual, end_station_name) %>%
  summarize(ride_count = n(), .groups = 'drop') %>%
  arrange(member_casual, desc(ride_count))

# Top 10 most popular end stations for each member type
top_end_stations_by_member <- popular_end_stations_by_member %>%
  group_by(member_casual) %>%
  slice_max(order_by = ride_count, n = 10)

# Plot for end stations by member type
ggplot(top_end_stations_by_member, aes(x = reorder(end_station_name, ride_count), y = ride_count, fill = member_casual)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ member_casual) +  # Create separate plots for each member type
  coord_flip() +  # Flip coordinates to make the station names readable
  labs(title = "Top 10 End Stations by Member Type", 
       x = "End Station Name", 
       y = "Number of Rides") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend for clarity

# Ride Frequency by Day of Week
ride_frequency_by_day <- cyclistic_bike_data_cleaned %>%
  group_by(day_of_week) %>%
  summarize(ride_count = n(), .groups = 'drop')

print(ride_frequency_by_day)

ggplot(ride_frequency_by_day, aes(x = day_of_week, y = ride_count, fill = day_of_week)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Rides by Day of the Week", x = "Day of Week", y = "Number of Rides") +
  theme_minimal()

#Member vs. Casual Usage Patterns
member_vs_casual_usage <- cyclistic_bike_data_cleaned %>%
  group_by(member_casual, rideable_type) %>%
  summarize(average_ride_length = mean(ride_length, na.rm = TRUE), .groups = 'drop')

print(member_vs_casual_usage)

ggplot(member_vs_casual_usage, aes(x = rideable_type, y = average_ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Ride Length by Member Type and Rideable Type", x = "Rideable Type", y = "Average Ride Length (seconds)") +
  theme_minimal()

# Calculate the number of rides by rideable type and member type
bike_usage_by_type <- cyclistic_bike_data_cleaned %>%
  group_by(rideable_type, member_casual) %>%
  summarize(ride_count = n(), .groups = 'drop')

print(bike_usage_by_type)

# Plot the number of rides by rideable type and member type
ggplot(bike_usage_by_type, aes(x = rideable_type, y = ride_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Rides by Rideable Type and Member Type", 
       x = "Rideable Type", 
       y = "Number of Rides", 
       fill = "Member Type") +
  theme_minimal()

# Ride Duration Analysis by Day of Week and Hour
avg_ride_duration <- cyclistic_bike_data_cleaned %>%
  group_by(day_of_week, hour_of_day) %>%
  summarize(average_ride_length = mean(ride_length, na.rm = TRUE), .groups = 'drop')

print(avg_ride_duration)

ggplot(avg_ride_duration, aes(x = hour_of_day, y = day_of_week, fill = average_ride_length)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Average Ride Duration by Day of Week and Hour of Day", x = "Hour of Day", y = "Day of Week") +
  theme_minimal()

# Analyze trends across different months to detect any seasonal variations in bike usage.
cyclistic_bike_data_cleaned <- cyclistic_bike_data_cleaned %>%
  mutate(month = month(started_at, label = TRUE))

monthly_rides <- cyclistic_bike_data_cleaned %>%
  group_by(month, member_casual) %>%
  summarize(ride_count = n(), .groups = 'drop')

print(monthly_rides)

ggplot(monthly_rides, aes(x = month, y = ride_count, color = member_casual, group = member_casual)) +
  geom_line() +
  labs(title = "Monthly Ride Trends by Member Type", x = "Month", y = "Number of Rides") +
  theme_minimal()

# Compare average ride duration across different types of bikes and between members and casual riders.
avg_ride_duration_by_type <- cyclistic_bike_data_cleaned %>%
  group_by(rideable_type, member_casual) %>%
  summarize(average_ride_length = mean(ride_length, na.rm = TRUE), .groups = 'drop')

ggplot(avg_ride_duration_by_type, aes(x = rideable_type, y = average_ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Ride Duration by Rideable Type and Member Type", x = "Rideable Type", y = "Average Ride Duration (seconds)") +
  theme_minimal()


# Calculate the utilization of stations
station_utilization <- cyclistic_bike_data_cleaned %>%
  group_by(start_station_name) %>%
  summarize(
    station_rides = n(),
    unique_bikes = n_distinct(ride_id)
  ) %>%
  arrange(desc(station_rides))

# View the top 10 most utilized stations
head(station_utilization, 10)

#To analyze the type of bike usage by time of year and member type
#Group and Summarize the Data
bike_usage_by_month_type <- cyclistic_bike_data_cleaned %>%
  group_by(rideable_type, month, member_casual) %>%
  summarize(ride_count = n(), .groups = 'drop')

# Print all rows of the bike usage by month, type, and member type
print(bike_usage_by_month_type, n = Inf)

ggplot(bike_usage_by_month_type, aes(x = month, y = ride_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~rideable_type) +
  labs(title = "Monthly Bike Usage by Rideable Type and Member Type", 
       x = "Month", 
       y = "Number of Rides", 
       fill = "Member Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
str()
#===================================
# STEP 6: FINALIZE AND SHARE RESULTS
#===================================

# Printing final summary of findings and recommendations
print("Analysis Summary:")
print(paste("Mean ride length:", mean_ride_length))
print(paste("Max ride length:", max_ride_length))
print(paste("Mode day of the week:", mode_day_of_week))
print("Proportional Share of Rides by Member Type:")
print(ride_share_by_member_type)
print("Number of Short Rides by Member Type:")
print(short_rides_summary)
print("Number of Rides by Rideable Type and Member Type:")
print(rides_by_rideable_type_and_member_type)
