# ============================================================
# 1. Setup & Import
# ============================================================
setwd("C:\\Users\\Huawei\\Desktop\\studies\\Degree in Artificial Intelligence\\Programming for Data Analysis\\assignment\\combined")

# Import flights
flights <- read.csv("flights.csv", header = TRUE)
flights_clean <- flights

# Import airport reference files 
## where you find the dataset, link?
airport_id   <- read.csv("L_AIRPORT_ID.csv")     # numeric IDs
airport_code <- read.csv("L_AIRPORT.csv")    # alpha codes

## import airline and airport code
airline <- read.csv("iata_airline_codes.csv") # airline column
airport <- read.csv("iata_airport_codes.csv") # origin airport & destination airport

str(flights)

## ============================================================
## 2. Cleaning
## ============================================================
library(dplyr)
unique(flights_clean$ORIGIN_AIRPORT)

# Character airport codes (letters only)
flight_origin_char <- flights_clean %>%
  filter(grepl("^[A-Z]+$", ORIGIN_AIRPORT))

# Numeric airport codes (digits only)
flight_origin_number <- flights_clean %>%
  filter(grepl("^[0-9]+$", ORIGIN_AIRPORT))

nrow(flight_origin_char)
nrow(flight_origin_number)


# Merge IDs with codes
merged_codes <- airport_id %>%
  inner_join(airport_code, by = "Description") %>%
  rename(FAA_ID = Code.x, IATA = Code.y)

# Lookup vector: numeric ??? IATA
# create a dictionary to store conversion from FAA_ID to IATA
faa_to_iata <- setNames(merged_codes$IATA, merged_codes$FAA_ID) 

# Replace numeric airport codes with IATA codes
flights_clean$ORIGIN_AIRPORT <- ifelse(
  grepl("^[0-9]+$", flights_clean$ORIGIN_AIRPORT),
  faa_to_iata[flights_clean$ORIGIN_AIRPORT],
  flights_clean$ORIGIN_AIRPORT
)
flights_clean$DESTINATION_AIRPORT <- ifelse(
  grepl("^[0-9]+$", flights_clean$DESTINATION_AIRPORT),
  faa_to_iata[flights_clean$DESTINATION_AIRPORT],
  flights_clean$DESTINATION_AIRPORT
)
#check unique values
unique(flights_clean$ORIGIN_AIRPORT)


# add AIRLINE_NAME column
flights_clean <- flights_clean %>%
  left_join(airline, by = c("AIRLINE" = "IATA_CODE")) %>%
  rename(AIRLINE_NAME = AIRLINE.y)

flights_clean %>%
  select(AIRLINE, AIRLINE_NAME) %>%
  distinct()


library(lubridate)
library(visdat)

# Convert types
## Factors make sure R treats them as groups rather than strings.
flights_clean$AIRLINE <- as.factor(flights_clean$AIRLINE)
flights_clean$ORIGIN_AIRPORT <- as.factor(flights_clean$ORIGIN_AIRPORT)
flights_clean$DESTINATION_AIRPORT <- as.factor(flights_clean$DESTINATION_AIRPORT)

# Create DATE column
flights_clean$DATE <- make_date(year = flights_clean$YEAR,
                                month = flights_clean$MONTH,
                                day = flights_clean$DAY)

# Convert day of week
flights_clean$DAY_OF_WEEK <- factor(flights_clean$DAY_OF_WEEK,
                                    levels = 1:7,
                                    labels = c("Monday",
                                               "Tuesday","Wednesday",
                                               "Thursday","Friday","Saturday","Sunday"))

# Drop unused columns
flights_clean$X <- NULL
flights_clean <- flights_clean %>%
  select(-YEAR, -MONTH, -DAY, -TAXI_IN, -TAXI_OUT, -WHEELS_ON, -WHEELS_OFF) %>%
  relocate(DATE, .before = 1)
## Wheels on & wheels off is similar as departure & arrive
## we drop it because it is not a strong / main reason for delay
## we drop day, month, year because we had created column Date

# visualize missing value
## Check missing values (before remove NA) 
vis_miss(flights_clean, warn_large_data = FALSE)

# replace NA with 0 for cancellation reasons 
flights_clean <- flights_clean %>%
  mutate(
    CANCELLATION_REASON = ifelse(CANCELLED == 0, "0", CANCELLATION_REASON),                
  )

# change NA to 0 for AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY, WEATHER_DELAY
flights_clean <- flights_clean %>%
  mutate(LATE_AIRCRAFT_DELAY = ifelse(is.na(LATE_AIRCRAFT_DELAY), 0, LATE_AIRCRAFT_DELAY)) %>%
  mutate(AIRLINE_DELAY = ifelse(is.na(AIRLINE_DELAY), 0, AIRLINE_DELAY)) %>%
  mutate(WEATHER_DELAY = ifelse(is.na(WEATHER_DELAY), 0, WEATHER_DELAY)) %>%
  mutate(AIR_SYSTEM_DELAY = ifelse(is.na(AIR_SYSTEM_DELAY), 0, AIR_SYSTEM_DELAY)) %>%
  mutate(SECURITY_DELAY = ifelse(is.na(SECURITY_DELAY), 0, SECURITY_DELAY))


# remove the row for the origin airport with NA 
flights_clean <- flights_clean %>%
  filter(!is.na(ORIGIN_AIRPORT))

# remove the row for the destination airport with NA 
flights_clean <- flights_clean %>%
  filter(!is.na(DESTINATION_AIRPORT))

# remove the row for the departure time with NA & cancelled = 0
flights_clean <- flights_clean %>%
  filter(!is.na(DEPARTURE_TIME & CANCELLED == 0))

# remove the row for the arrival time with NA & cancelled = 0
flights_clean <- flights_clean %>%
  filter(!is.na(ARRIVAL_TIME & CANCELLED == 0))

# remove the row for the departure delay with NA & cancelled = 0
flights_clean <- flights_clean %>%
  filter(!is.na(DEPARTURE_DELAY & CANCELLED == 0))

# remove the row for the arrival delay with NA & cancelled = 0
flights_clean <- flights_clean %>%
  filter(!is.na(ARRIVAL_DELAY & CANCELLED == 0))

# Check missing values (after cleaning)
vis_miss(flights_clean, warn_large_data = FALSE)
# the remaining missing value is CANCELLED = 1




# ============================================================
# 3. Preprocessing (Feature Engineering)
# ============================================================
## time of day for schedule departure
flights_time <- flights_clean %>%
  mutate(
    DEP_HOUR = floor(SCHEDULED_DEPARTURE / 100),
    TIME_OF_DAY = cut(DEP_HOUR,
                      breaks = c(-1, 5, 11, 17, 23), 
                      labels = c("Night", "Morning", "Afternoon", "Evening"))
  ) %>%
  relocate(DEP_HOUR, TIME_OF_DAY, .after = SCHEDULED_DEPARTURE)

# ORIGIN AIRPORT: ORIGIN_AIRPORT_NAME, ORIGIN_AIRPORT_CITY, ORIGIN_AIRPORT_STATE, ORIGIN_AIRPORT_COUNTRY, 
# ORIGIN_AIRPORT_LATITUDE, ORIGIN_AIRPORT_LONGITUDE
flights_clean_origin <- flights_clean %>%
  left_join(airport, by = c("ORIGIN_AIRPORT" = "IATA_CODE")) %>%
  rename(ORIGIN_AIRPORT_NAME = AIRPORT) %>%
  rename(ORIGIN_AIRPORT_CITY = CITY) %>%
  rename(ORIGIN_AIRPORT_STATE = STATE) %>%
  rename(ORIGIN_AIRPORT_COUNTRY = COUNTRY) %>%
  rename(ORIGIN_AIRPORT_LATITUDE = LATITUDE) %>%
  rename(ORIGIN_AIRPORT_LONGITUDE = LONGITUDE)

# DESTINATION_AIRPORT: DESTINATION_AIRPORT_NAME, DESTINATION_AIRPORT_CITY, DESTINATION_AIRPORT_STATE, 
# DESTINATION_AIRPORT_COUNTRY, DESTINATION_AIRPORT_LATITUDE, DESTINATION_AIRPORT_LONGITUDE
flights_clean_destination <- flights_clean %>%
  left_join(airport, by = c("DESTINATION_AIRPORT" = "IATA_CODE")) %>%
  rename(DESTINATION_AIRPORT_NAME = AIRPORT) %>%
  rename(DESTINATION_AIRPORT_CITY = CITY) %>%
  rename(DESTINATION_AIRPORT_STATE = STATE) %>%
  rename(DESTINATION_AIRPORT_COUNTRY = COUNTRY) %>%
  rename(DESTINATION_AIRPORT_LATITUDE = LATITUDE) %>%
  rename(DESTINATION_AIRPORT_LONGITUDE = LONGITUDE)

# ============================================================
# 4. exploration
# ============================================================
library(ggplot2)
library(gridExtra)

# Histograms
## show p1 graph: arrival delay
p1 <- ggplot(flights_clean, aes(x = ARRIVAL_DELAY)) +
  geom_histogram(binwidth = 15, fill = "steelblue", color = "black") +
  labs(title = "Arrival Delay (After Cleaning)", x = "Minutes", y = "Count") +
  theme_minimal()
p1


## show p2 graph: departure delay 
p2 <- ggplot(flights_clean, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 15, fill = "darkorange", color = "black") +
  labs(title = "Departure Delay (After Cleaning)", x = "Minutes", y = "Count") +
  theme_minimal()
p2

# Boxplots
## show p3 graph: arrival delay in boxplot
p3 <- ggplot(flights_clean, aes(y = ARRIVAL_DELAY)) +
  geom_boxplot(fill = "skyblue", outlier.size = 0.5) +
  labs(title = "Boxplot of Arrival Delay", y = "Minutes") +
  theme_minimal()
p3

## show p4 graph: departure delay in boxplot
p4 <- ggplot(flights_clean, aes(y = DEPARTURE_DELAY)) +
  geom_boxplot(fill = "orange", outlier.size = 0.5) +
  labs(title = "Boxplot of Departure Delay", y = "Minutes") +
  theme_minimal()
p4

## combine all to see the overview
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Delay by time of day
ggplot(flights_time, aes(x = TIME_OF_DAY, y = ARRIVAL_DELAY, fill = TIME_OF_DAY)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(title = "Arrival Delay by Time of Day",
       x = "Time of Day", y = "Minutes") +
  theme_minimal()

# Delay by weekday
ggplot(flights_time, aes(x = DAY_OF_WEEK, y = ARRIVAL_DELAY, fill = DAY_OF_WEEK)) +
  geom_boxplot(outlier.size = 0.5) +
  labs(title = "Arrival Delay by Day of Week",
       x = "Day of Week", y = "Minutes") +
  theme_minimal()





#======================================================
# 5. Analysis
#======================================================

#======================================================
# 5.1: objective 1 - Teoh Li Hong TP098983
#======================================================
#---------------------------------------5.1.1.1: analysis 1---------------------------------------------
library(dplyr)
library(ggplot2)

# Filter for weather-related cancellations (CANCELLATION_REASON = 'B')
weather_cancellations = flights_clean %>%
  filter(CANCELLATION_REASON == 'B')
# Bin the DEPARTURE_DELAY for these cancelled flights
#NA implies no departure time recorded before cancellation (immediate decision)
#Bins: No recorded delay (NA), On Time (<=0), Short Delay (1-30 min), Moderate Delay (31-60 min),
#Long Delay (>60 min)
delay_distribution = weather_cancellations %>%
  mutate(
    Delay_Severity = case_when(
      is.na(DEPARTURE_DELAY)  ~ "A_No Time Recorded (NA)", #use A_ for sorting first
      DEPARTURE_DELAY <= 0    ~ "B_On Time / Early",
      DEPARTURE_DELAY <= 1    ~ "BB_On Time / Early",
      DEPARTURE_DELAY <= 30   ~ "C_Short Delay (1-30 min)",
      DEPARTURE_DELAY <= 60   ~ "D_Moderate Delay (31-60 min)",
      TRUE                    ~ "E_Long Delay (>60 min)"
    )
  ) %>%
  group_by(Delay_Severity) %>%
  summarise(
    Total_Cancellations = n()
  ) %>%
  mutate(
    Percentage_of_Weather_Cancellations = (Total_Cancellations / sum(Total_Cancellations)) * 100
  )
#Output the table
print(delay_distribution)
#Visualise the distribution 
ggplot(delay_distribution, aes(x = Delay_Severity, y = Percentage_of_Weather_Cancellations)) +
  geom_bar(stat = "identity", fill = "skyblue") + 
  labs(
    title = "Severity of Initial Delay for Flights Cancelled Due to Weather",
    x = "Departure Delay Severity (Prior to Cancellation)",
    y = "Percentage of Weather Cancellation (%)"
  ) +
  theme_minimal() +
  coord_flip() # Make the labels easier to read


#------------------------------5.1.1.2: second chart-----------------------------------------------------
library(scales) # for percent_format

# DEPARTURE_DELAY group by AIRLINE
delay_distribution_by_airline = weather_cancellations %>%
  mutate(
    Delay_Severity = case_when(
      is.na(DEPARTURE_DELAY)  ~ "A_No Time Recorded (NA)", # Pre-emptive Cancellation
      DEPARTURE_DELAY <= 0    ~ "B_On Time / Early",
      DEPARTURE_DELAY <= 30   ~ "C_Short Delay (1-30 min)",
      DEPARTURE_DELAY <= 60   ~ "D_Moderate Delay (31-60 min)",
      TRUE                    ~ "E_Long Delay (>60 min)"
    )
  ) %>%
  # Group by AIRLINE and Delay_Severity to count cancellations
  group_by(AIRLINE, Delay_Severity) %>%
  summarise(
    Total_Cancellations = n(),
    .groups = 'drop' # Grouping can be dropped after counting
  ) %>%
  # Regroup by AIRLINE to calculate the percentage within each airline
  group_by(AIRLINE) %>%
  mutate(
    Percentage_of_Airline_Cancellations = (Total_Cancellations / sum(Total_Cancellations)) * 100
  ) %>%
  ungroup() %>%
  left_join(airline, by = c("AIRLINE" = "IATA_CODE")) %>%
  rename(AIRLINE_NAME = AIRLINE.y)

# Create a clean label for the chart, keeping the prefixed version for ordering
delay_distribution_by_airline = delay_distribution_by_airline %>%
  mutate(
    Display_Severity = sub("^[A-Z]{1,2}_", "", Delay_Severity)
  ) 

# Output the table
View(delay_distribution_by_airline)



# Visualise the distribution (100% stacked bar chart)
# Order the airlines by total number of weather cancellations 
airline_order = delay_distribution_by_airline %>%
  group_by(AIRLINE_NAME) %>%
  summarise(Total_Weather_Cancellations = sum(Total_Cancellations)) %>%
  arrange(desc(Total_Weather_Cancellations)) %>%
  pull(AIRLINE_NAME)

delay_distribution_by_airline$AIRLINE_NAME = factor(delay_distribution_by_airline$AIRLINE_NAME, levels = airline_order)

ggplot(delay_distribution_by_airline, aes(x = AIRLINE_NAME, y = Percentage_of_Airline_Cancellations, fill = Display_Severity)) +
  # position = "fill" makes it a 100% stacked bar chart
  geom_bar(stat = "identity", position = "fill") +
  # Format y-axis to show percentage labels correctly
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of Initial Delay Severity for Weather Cancellations, by Carrier",
    x = "Airline Carrier (Sorted by Total Weather Cancellations)",
    y = "Proportion of Airline's Weather Cancellations",
    fill = "Delay Severity Prior to Cancellation"
  ) +
  # Ensure the fill order is correct (A, B, C, D, E) for consistency
  scale_fill_discrete(limits = unique(delay_distribution_by_airline$Display_Severity[order(delay_distribution_by_airline$Delay_Severity)])) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Rotate X-axis labels
    legend.position = "bottom"
  )

#---------------------------------------5.1.2.1: analysis 2---------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)

# Calculate Weather Cancellation Rate by Month and Airport

cancellation_trends = flights_clean_origin %>%
  # Group the data by the origin airport and the month
  group_by(ORIGIN_AIRPORT, Month = month(DATE), ORIGIN_AIRPORT_NAME) %>%
  summarise(
    # Count total number of flights from that airport in that month
    Total_Flights = n(),
    # Count number of weather-related cancellations (CANCELLATION_REASON = 'B')
    Weather_Cancellations = sum(CANCELLATION_REASON == 'B', na.rm = TRUE),
    # Calculate the Weather Cancellation Rate (per 100 flights)
    Weather_Cancellation_Rate = (Weather_Cancellations / Total_Flights) * 100
  ) %>%
  ungroup() %>%
  # Convert MONTH number to a readable name for plotting
  mutate(
    Month_Name = month(Month, label = TRUE, abbr = FALSE)
  )

# Display the top rows of the aggregated data
print(head(cancellation_trends))

#save the Northest airports for specific plotting
# Define the Coordinate Boundaries
# Northest region: 39.7 N to 79.8 N latitude
MIN_LAT = 39.7
MAX_LAT = 47.6

#Northeast region: 69.4 W to 79.8 W longitude
MIN_LON = -79.8
MAX_LON = -69.4

# Filter the Dataframe
northeast_airport_codes = flights_clean_origin %>%
  # Select only unique rows based on the airport identifier for efficiency
  distinct(ORIGIN_AIRPORT, ORIGIN_AIRPORT_LATITUDE, ORIGIN_AIRPORT_LONGITUDE, ORIGIN_AIRPORT_NAME) %>%
  # Apply geographical filters
  filter(ORIGIN_AIRPORT_LATITUDE >= MIN_LAT & ORIGIN_AIRPORT_LATITUDE <= MAX_LAT &
           ORIGIN_AIRPORT_LONGITUDE >= MIN_LON & ORIGIN_AIRPORT_LONGITUDE <= MAX_LON
  ) %>%
  # Extract the unique airport codes as a character vector
  pull(ORIGIN_AIRPORT_NAME)

# Print the result
cat("Northeast Airports based on the coordinates (", MIN_LAT, "N to", MAX_LAT, "N and", abs(MIN_LON), "W to", abs(MAX_LON), "W):\n")
print(northeast_airport_codes)

# Visualisation: Plotting the Trends
# Plotting the overall trend for the top 10 airports with the highest average rate.
# or focusing on the hypothesized Northeast airports (better for testing your hypothesis).

# Filter for Northeast airports for a targeted plot #northeast_airports
northeast_trends = cancellation_trends %>%
  filter(ORIGIN_AIRPORT_NAME %in% northeast_airport_codes)

# Create the line plot showing the seasonal trend for the Northeast region
ggplot(northeast_trends, aes(x = Month_Name, y = Weather_Cancellation_Rate, group = ORIGIN_AIRPORT_NAME, color = ORIGIN_AIRPORT_NAME)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  labs(
    title = "Seasonal Trend of Weather Cancellation Rate in the Northeast Region",
    subtitle = "Percentage of flights cancelled due to weather, by Origin Airport (2015)",
    x = "Month",
    y = "Weather Cancellation Rage (%)",
    color = "Origin Airport"
  ) + 
  theme_minimal() +
  # Rotate x-axis text for better readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#------------------------------5.1.2.2: second chart-----------------------------------------------------
# Define Regional Flag based on the airport names vector created in Analysis 2
regional_trends = cancellation_trends %>%
  mutate(
    Region = ifelse(ORIGIN_AIRPORT_NAME %in% northeast_airport_codes, 
                    "Northeast Region", 
                    "Non-Northeast Region (US Average)")
  )

# Aggregate by Region and Month
regional_cancellation_summary = regional_trends %>%
  # Group only by the new Region and Month
  group_by(Region, Month_Name) %>%
  summarise(
    # Calculate the total number of flights and cancellations in each region/month
    Total_Flights_Region = sum(Total_Flights),
    Total_Weather_Cancellations_Region = sum(Weather_Cancellations),
    .groups = 'drop_last'
  ) %>%
  # Calculate the *actual* weather cancellation rate for the entire region/month
  mutate(
    Regional_Cancellation_Rate = (Total_Weather_Cancellations_Region / Total_Flights_Region) * 100
  ) %>%
  ungroup()

# Reorder Month_Name for correct plotting (ggplot usually does this fine, but this is safer)
regional_cancellation_summary$Month_Name = factor(regional_cancellation_summary$Month_Name, 
                                                  levels = month.name)

# Output the table
print(regional_cancellation_summary)

# Visualisation: Compare Northeast vs. Non-Northeast
ggplot(regional_cancellation_summary, aes(x = Month_Name, y = Regional_Cancellation_Rate, group = Region, color = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Regional Comparison: Seasonal Weather Cancellation Rate",
    subtitle = "Northeast vs. Non-Northeast US (2015)",
    x = "Month",
    y = "Weather Cancellation Rate (%)",
    color = "Region"
  ) +
  scale_color_manual(values = c("Northeast Region" = "#E64B35FF", "Non-Northeast Region (US Average)" = "#4DBBD5FF")) +
  theme_minimal() +
  # Highlight the winter months visually (Optional, but effective)
  geom_vline(xintercept = c("January", "February", "December"), linetype = "dashed", color = "gray50", alpha = 0.6) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

#---------------------------------------5.1.3.1: analysis 3---------------------------------------------
overall_cancellation_reasons = flights_clean %>%
  filter(CANCELLED == 1) %>% # Filter only canceled flights
  group_by(CANCELLATION_REASON) %>%
  summarise(
    Total_Cancellations = n(),
    .groups = 'drop'
  ) %>%
  # Calculate the percentage for Visualisation
  mutate(Percentage = (Total_Cancellations / sum(Total_Cancellations)) * 100)

# A: Airline/Carrier B: Weather C: National Air System D: Security
overall_cancellation_reasons = overall_cancellation_reasons %>%
  mutate(
    CANCELLATION_REASON = case_when(
      CANCELLATION_REASON == 'A' ~ 'A - Airline/Carrier', 
      CANCELLATION_REASON == 'B' ~ 'B - Weather',
      CANCELLATION_REASON == 'C' ~ 'C - National Air System',
      CANCELLATION_REASON == 'D' ~ 'D - Security',
      TRUE ~ CANCELLATION_REASON # Keep all other values (like NA or already changed values)
    )
  )

print("Overall Distribution of Cancellation Reasons (A, B, C, D):")
print(overall_cancellation_reasons)

ggplot(overall_cancellation_reasons, aes(x = CANCELLATION_REASON, y = Percentage, fill = CANCELLATION_REASON)) +
  # Add the bar segments
  geom_bar(stat = 'identity') +
  
  # Add the text labels
  geom_text(
    # Create a new label combining Percentage and Total_Cancellations
    aes(label = paste0(round(Percentage, 1), "%\n", "(N=", Total_Cancellations, ")")),
    vjust = -0.5, # Position the label just above the bar
    size = 4
  ) + 
  
  # Set up labels and theme
  labs(
    title = "Overall Distribution of Flight Cancellation Reasons",
    x = "Cancellation Reason",
    y = "Percentage (%)"
  ) + 
  theme_minimal() +
  # Adjust the y-axis limit to make room for the labels above the highest bar (54.2%)
  # You might need to adjust '60' based on the height of your highest bar
  ylim(0, 60)

#------------------------------5.1.3.2: second chart-----------------------------------------------------
library(scales) # for percent_format
# install.packages("waffle") 
library(waffle) 


# Define Winter Months (1=Jan, 2=Feb, 3=Mar, 11=Nov, 12=Dec)
WINTER_MONTHS = c(1, 2, 3, 11, 12)

# Filter the dataset for the Critical Segment (Northeast Winter Cancellations)
northeast_winter_cancellations = flights_clean_origin %>%
  # Filter only canceled flights
  filter(CANCELLED == 1) %>%
  # Filter for Northeast Airports (using the airport codes vector)
  filter(ORIGIN_AIRPORT_NAME %in% northeast_airport_codes) %>%
  # Filter for Winter Months
  filter(month(DATE) %in% WINTER_MONTHS)

# Aggregate the Cancellation Reasons for this Critical Segment
northeast_winter_reasons = northeast_winter_cancellations %>%
  group_by(CANCELLATION_REASON) %>%
  summarise(
    Total_Cancellations = n(),
    .groups = 'drop'
  ) %>%
  # Calculate the percentage for Visualization
  mutate(Percentage = (Total_Cancellations / sum(Total_Cancellations)) * 100)

# Map the Codes to Names for the Chart
northeast_winter_reasons = northeast_winter_reasons %>%
  mutate(
    CANCELLATION_REASON = case_when(
      CANCELLATION_REASON == 'A' ~ 'A - Airline/Carrier', 
      CANCELLATION_REASON == 'B' ~ 'B - Weather',
      CANCELLATION_REASON == 'C' ~ 'C - National Air System',
      CANCELLATION_REASON == 'D' ~ 'D - Security',
      TRUE ~ CANCELLATION_REASON
    )
  )

print("Cancellation Reason Distribution in the Northeast during Winter:")
print(northeast_winter_reasons)

# Visualisation 
# Prepare the data vector
# The waffle function requires a named vector of the counts (Total_Cancellations).
# We'll use the Cancellation Reason names as the names for the vector.
waffle_data = northeast_winter_reasons$Total_Cancellations
names(waffle_data) = northeast_winter_reasons$CANCELLATION_REASON

# Define Colors (Optional, but makes it consistent and clear)
waffle_colors = c(
  "B - Weather" = "tomato",           # Highlight Weather
  "A - Airline/Carrier" = "skyblue",  # Other major reason
  "C - National Air System" = "gold", # NAS
  "D - Security" = "darkgreen"        # Security
)

# Create the Waffle Chart
waffle(
  waffle_data, 
  rows = 10, 
  size = 0.5, 
  colors = waffle_colors,
  title = "Cancellation Reasons: Northeast Region During Winter Months",
  xlab = "The total colored area represents 100% of the cancellations in the Northeast during winter."
) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )


#---------------------------------------5.1.4.1: analysis 4---------------------------------------------
library(dplyr)
library(tidyr) # extra
library(ggplot2)
# install.packages("treemapify") # extra
library(treemapify)

# ---  Define Variables ---

# Northeast Airports (as identified in the analysis 2)
northeast_airport_codes 

# Winter Months (December = 12, January = 1, February = 2)
winter_months <- c(12, 1, 2)

# ---  Filter and Process Data ---

delay_distribution_data <- flights_clean_origin %>%
  # Filter for the high-risk group defined in the hypothesis
  filter(ORIGIN_AIRPORT_NAME %in% northeast_airport_codes,
         month(DATE) %in% winter_months,
         CANCELLED == 0) %>% # Focus only on completed flights to use delay components
  # Select and rename delay columns for clarity
  select(AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY, WEATHER_DELAY) %>%
  # Convert data from wide format (one column per delay type) to long format (Delay_Type and Total_Minutes)
  pivot_longer(
    cols = everything(),
    names_to = "Delay_Type",
    values_to = "Delay_Minutes"
  ) %>%
  # Aggregate total delay minutes for each type
  group_by(Delay_Type) %>%
  summarise(
    Total_Delay_Minutes = sum(Delay_Minutes, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Calculate the percentage contribution of each delay type
  mutate(
    Percentage_Contribution = (Total_Delay_Minutes / sum(Total_Delay_Minutes)) * 100,
    # Clean up Delay_Type names for the chart
    Delay_Type = case_when(
      Delay_Type == "AIR_SYSTEM_DELAY" ~ "Air System",
      Delay_Type == "SECURITY_DELAY" ~ "Security",
      Delay_Type == "AIRLINE_DELAY" ~ "Airline",
      Delay_Type == "LATE_AIRCRAFT_DELAY" ~ "Late Aircraft",
      Delay_Type == "WEATHER_DELAY" ~ "Weather",
      TRUE ~ Delay_Type
    )
  )
# Print the resulting data for numerical diagnosis
print(delay_distribution_data)

# ---  Visualization ---

# Filter out 0% contributions and sort
treemap_data <- delay_distribution_data %>%
  filter(Total_Delay_Minutes > 0) %>% # Remove Security Delay (0%)
  arrange(desc(Total_Delay_Minutes)) %>%
  # Add a label column specifically for the legend (Code + Percentage)
  mutate(Legend_Label = paste0(Delay_Type, " (", sprintf("%.1f%%", Percentage_Contribution), ")"))


# Create the Treemap Plot (Smaller Visual Footprint)
ggplot(treemap_data, 
       aes(area = Total_Delay_Minutes, 
           fill = Legend_Label, # Use the combined label for the legend
           subgroup = Delay_Type)) +
  
  geom_treemap(color = "white", linewidth = 1) +
  
  # Remove geom_treemap_text() to place the labels outside
  
  labs(
    title = "Contribution to Total Delay Minutes",
    subtitle = "Northeast Airports (Winter) - Percentage Contribution to Total Delay Minutes",
    fill = "Delay Reason (Percentage)" # Label the legend clearly
  ) +
  theme_minimal(base_size = 10) + # Reduce base font size for a "smaller" look
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8)
  )


#------------------------------5.1.4.2: second chart-----------------------------------------------------
library(scales) # for comma format
# Define Variables (Re-using from Analysis 4, first chart) ---
# Northeast Airports (northeast_airport_codes)
# Winter Months (winter_months <- c(12, 1, 2))

# Filter and Process Data ---

# Filter data for the high-risk group (Northeast Winter, CANCELLED == 0)
carrier_delay_contribution = flights_clean_origin %>%
  filter(ORIGIN_AIRPORT_NAME %in% northeast_airport_codes,
         month(DATE) %in% winter_months,
         CANCELLED == 0) %>%
  # Select the two key system delay components and the airline
  select(AIRLINE, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY) %>%
  
  # Convert to long format for easier stacking and aggregation
  pivot_longer(
    cols = c(AIRLINE_DELAY, LATE_AIRCRAFT_DELAY),
    names_to = "System_Delay_Type",
    values_to = "Delay_Minutes"
  ) %>%
  
  # Group by Airline and Delay Type to get total minutes
  group_by(AIRLINE, System_Delay_Type) %>%
  summarise(
    Total_Delay_Minutes = sum(Delay_Minutes, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  
  # Join with Airline Names for cleaner chart labels
  left_join(airline, by = c("AIRLINE" = "IATA_CODE")) %>%
  rename(AIRLINE_NAME = AIRLINE.y) %>%
  
  # Clean up delay names
  mutate(
    System_Delay_Type_Clean = ifelse(System_Delay_Type == "AIRLINE_DELAY", "Airline Delay", "Late Aircraft Delay")
  )

print(n=28, carrier_delay_contribution)
# Visualization ---

# Order the Airlines by their Total System Delay Minutes (descending)
airline_order_delay = carrier_delay_contribution %>%
  group_by(AIRLINE_NAME) %>%
  summarise(Total_Carrier_Delay = sum(Total_Delay_Minutes)) %>%
  arrange(desc(Total_Carrier_Delay)) %>%
  pull(AIRLINE_NAME)

carrier_delay_contribution$AIRLINE_NAME = factor(carrier_delay_contribution$AIRLINE_NAME, levels = airline_order_delay)

# Create the Stacked Bar Chart
ggplot(carrier_delay_contribution, aes(x = AIRLINE_NAME, y = Total_Delay_Minutes, fill = System_Delay_Type_Clean)) +
  geom_bar(stat = "identity", position = "stack") +
  
  labs(
    title = "Carrier Contribution to System Delays (Airline & Late Aircraft)",
    subtitle = "Northeast Airports during Winter Months (Delay Minutes)",
    x = "Airline Carrier (Sorted by Total Delay Minutes)",
    y = "Total Delay Minutes (Thousands)",
    fill = "System Delay Type"
  ) +
  # Format Y-axis to display in thousands for readability
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  scale_fill_manual(values = c("Airline Delay" = "#4DBBD5FF", "Late Aircraft Delay" = "#E64B35FF")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

#---------------------------------------5.1.5.1: analysis 5---------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

#northeast_airport_code in analysis 2
northeast_airport_codes 

# 2. Define High-Risk (Winter) and Low-Risk (Fall) Months
WINTER_MONTH <- 1 # January
FALL_MONTH <- 9 # September
TARGET_MONTHS <- c(WINTER_MONTH, FALL_MONTH)


# --------------------------------------------------------
# 1. DATA AGGREGATION AND TRANSFORMATION
# --------------------------------------------------------

delay_comparison_data <- flights_clean_origin %>%
  # Filter for Northeast, Completed Flights, and Target Months
  filter(
    ORIGIN_AIRPORT_NAME %in% northeast_airport_codes,
    CANCELLED == 0,
    month(DATE) %in% TARGET_MONTHS # Assuming MONTH column is already numeric (1-12) or created
  ) %>%
  # Select and group by the month (now named 'Risk_Period')
  mutate(
    Risk_Period = ifelse(month(DATE) == WINTER_MONTH, "High-Risk (January)", "Low-Risk (September)")
  ) %>%
  group_by(Risk_Period) %>%
  # Calculate the sum of all five delay categories for each month
  summarise(
    WEATHER_DELAY = sum(WEATHER_DELAY, na.rm = TRUE),
    AIR_SYSTEM_DELAY = sum(AIR_SYSTEM_DELAY, na.rm = TRUE),
    SECURITY_DELAY = sum(SECURITY_DELAY, na.rm = TRUE),
    AIRLINE_DELAY = sum(AIRLINE_DELAY, na.rm = TRUE),
    LATE_AIRCRAFT_DELAY = sum(LATE_AIRCRAFT_DELAY, na.rm = TRUE),
  ) %>%
  ungroup() %>%
  mutate(
    Total_Delay_Minutes = WEATHER_DELAY + AIR_SYSTEM_DELAY + SECURITY_DELAY + AIRLINE_DELAY + LATE_AIRCRAFT_DELAY
  ) %>%
  # Reshape data from wide to long format for ggplot (pivot_longer)
  pivot_longer(
    cols = c(WEATHER_DELAY, AIR_SYSTEM_DELAY, SECURITY_DELAY, AIRLINE_DELAY, LATE_AIRCRAFT_DELAY),
    names_to = "Delay_Type",
    values_to = "Delay_Minutes"
  ) %>%
  # Calculate percentage contribution WITHIN each Risk_Period
  group_by(Risk_Period) %>%
  mutate(
    Percentage_Contribution = (Delay_Minutes / Total_Delay_Minutes) * 100
  ) %>%
  ungroup() %>%
  # Categorize delay types for ordering and coloring
  mutate(
    Delay_Category = case_when(
      Delay_Type %in% c("LATE_AIRCRAFT_DELAY", "AIRLINE_DELAY") ~ "Operational/Logistical",
      Delay_Type %in% c("WEATHER_DELAY", "AIR_SYSTEM_DELAY") ~ "External/Systemic",
      TRUE ~ "Security"
    ),
    # Clean up Delay_Type names for the chart
    Delay_Type_Clean = gsub("_DELAY", "", gsub("_", " ", Delay_Type))
  )
delay_comparison_data

# --------------------------------------------------------
# 2. VISUALIZATION: FACETED BAR CHART
# --------------------------------------------------------

ggplot(delay_comparison_data, 
       aes(x = reorder(Delay_Type_Clean, -Percentage_Contribution), 
           y = Percentage_Contribution, 
           fill = Delay_Category)) +
  
  geom_bar(stat = "identity", position = position_dodge()) +
  
  # Add percentage labels
  geom_text(aes(label = sprintf("%.1f%%", Percentage_Contribution)), 
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  
  # Split the chart by Risk_Period
  facet_wrap(~ Risk_Period) + 
  
  labs(
    title = "Delay Distribution in High-Risk vs. Low-Risk Months",
    subtitle = "Percentage breakdown of delay causes for completed flights from Northeast airports.",
    x = "Delay Reason",
    y = "Percentage of Total Delay Minutes (%)",
    fill = "Delay Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  ) 


#------------------------------5.1.5.2: second chart-----------------------------------------------------
library(scales) 

# --- Define Variables ---
# Northeast Airports (northeast_airport_codes)
winter_months <- c(12, 1, 2)
# Define Low-Risk (Fall) Months for comparison (e.g., September, October, November)
fall_months <- c(9, 10, 11) 
comparison_months <- c(winter_months, fall_months)

# --- Filter and Process Data ---


# Filter data for Northeast Airports and Comparison Months (Winter/Fall)
seasonal_carrier_delays = flights_clean_origin %>%
  filter(ORIGIN_AIRPORT_NAME %in% northeast_airport_codes,
         month(DATE) %in% comparison_months,
         CANCELLED == 0) %>%
  # Create a Season variable
  mutate(
    Season = ifelse(month(DATE) %in% winter_months, "Winter (High-Risk)", "Fall (Low-Risk)")
  ) %>%
  # Aggregate the two key system delays (Airline and Late Aircraft)
  mutate(
    System_Delay_Minutes = AIRLINE_DELAY + LATE_AIRCRAFT_DELAY
  ) %>%
  # Group by Airline and Season to get total system delay minutes
  group_by(AIRLINE, Season) %>%
  summarise(
    Total_System_Delay_Minutes = sum(System_Delay_Minutes, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Join with Airline Names
  left_join(airline, by = c("AIRLINE" = "IATA_CODE")) %>%
  rename(AIRLINE_NAME = AIRLINE.y)
View(seasonal_carrier_delays)
print(n=27, seasonal_carrier_delays)

# --- Visualization ---
# Filter for only the top 8 carriers
top_carriers_order = seasonal_carrier_delays %>%
  group_by(AIRLINE_NAME) %>%
  summarise(Overall_Delay = sum(Total_System_Delay_Minutes)) %>%
  top_n(8, Overall_Delay) %>%
  arrange(desc(Overall_Delay)) %>%
  pull(AIRLINE_NAME)

lollipop_data = seasonal_carrier_delays %>%
  filter(AIRLINE_NAME %in% top_carriers_order) %>%
  pivot_wider(
    id_cols = AIRLINE_NAME,
    names_from = Season,
    values_from = Total_System_Delay_Minutes,
    names_prefix = "Delay_"
  ) %>%
  # Handle cases where an airline might not have delays in both seasons (fill with 0)
  mutate(
    `Delay_Winter (High-Risk)` = replace_na(`Delay_Winter (High-Risk)`, 0),
    `Delay_Fall (Low-Risk)` = replace_na(`Delay_Fall (Low-Risk)`, 0)
  ) %>%
  # Ensure airlines are ordered by total delay across both seasons
  arrange(desc(`Delay_Winter (High-Risk)` + `Delay_Fall (Low-Risk)`))

lollipop_data$AIRLINE_NAME = factor(lollipop_data$AIRLINE_NAME, levels = lollipop_data$AIRLINE_NAME)

# Create the Lollipop Chart (Dumbbell style to connect points)
ggplot(lollipop_data) +
  # Draw a line between Winter and Fall delays for each airline
  geom_segment(aes(x = AIRLINE_NAME, xend = AIRLINE_NAME, y = `Delay_Fall (Low-Risk)`, 
                   yend = `Delay_Winter (High-Risk)`),
               color = "gray", size = 1) +
  # Add points for Fall delays
  geom_point(aes(x = AIRLINE_NAME, y = `Delay_Fall (Low-Risk)`, color = "Fall (Low-Risk)"), 
             size = 4, alpha = 0.8) +
  # Add points for Winter delays
  geom_point(aes(x = AIRLINE_NAME, y = `Delay_Winter (High-Risk)`, color = "Winter (High-Risk)"), 
             size = 4, alpha = 0.8) +
  # Add labels to the points for exact values
  geom_text(aes(x = AIRLINE_NAME, y = `Delay_Fall (Low-Risk)`, 
                label = label_number(scale = 1e-3, suffix = "K")(`Delay_Fall (Low-Risk)`)), 
            color = "#4DBBD5FF", size = 3, vjust = 1.5) +
  geom_text(aes(x = AIRLINE_NAME, y = `Delay_Winter (High-Risk)`, 
                label = label_number(scale = 1e-3, suffix = "K")(`Delay_Winter (High-Risk)`)), 
            color = "#E64B35FF", size = 3, vjust = -0.5) +
  labs(
    title = "Carrier System Delay Volume: Winter vs. Fall Comparison",
    subtitle = "Total Minutes of Airline + Late Aircraft Delay (Northeast Airports)",
    x = "Airline Carrier",
    y = "Total System Delay Minutes (Thousands)",
    color = "Season" # Legend title for colors
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  scale_color_manual(values = c("Winter (High-Risk)" = "#E64B35FF", "Fall (Low-Risk)" = "#4DBBD5FF")) +
  coord_flip() + # Flip coordinates for better readability of airline names
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

#======================================================
# 5.2: objective 2: Ebad Ejaz Fakih TP082015
#======================================================
#======================================================
# 5. Analysis
#   Objective: Are flights from the Northeast in winter
#   more likely to be cancelled than others?
#   NOTE: This section does NOT modify earlier objects.
#======================================================
# [ Ebad Ejaz , TP082015] 

library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)

# Northeast mapping provided
NE_STATES <- c("ME","NH","VT","MA","RI","CT","NY","NJ","PA")

# --- Helper: derive Season from DATE (Winter = Dec/Jan/Feb) ---
get_season <- function(dt) {
  m <- month(dt)
  factor(ifelse(m %in% c(12, 1, 2), "Winter", "Non-Winter"),
         levels = c("Winter","Non-Winter"))
}

# --- Build an analysis data frame with origin STATE & SEASON ---
# We do not overwrite flights_clean; we create a new df for analysis.
analysis_df <- flights_clean %>%
  # add origin state via iata_airport_codes join
  left_join(airport %>% select(IATA_CODE, STATE),
            by = c("ORIGIN_AIRPORT" = "IATA_CODE")) %>%
  rename(ORIGIN_STATE = STATE) %>%
  mutate(
    SEASON = get_season(DATE),
    REGION = ifelse(ORIGIN_STATE %in% NE_STATES, "Northeast", "Other"),
    REGION = factor(REGION, levels = c("Northeast","Other")),
    CANCELLED = as.integer(CANCELLED) # ensure 0/1
  ) %>%
  # keep rows where origin state is known
  filter(!is.na(ORIGIN_STATE))

# ======================================================
# 5.2.1: Analysis 1: Cancellation Rate by Region x Season
#  - Compares Northeast vs Other across Winter/Non-Winter
#  - Chi-square test of independence + plot
# ======================================================

# Summary table
a1_tab <- analysis_df %>%
  group_by(REGION, SEASON) %>%
  summarise(
    flights = n(),
    cancelled = sum(CANCELLED == 1),
    cancel_rate = cancelled / flights
  ) %>%
  ungroup()

print(a1_tab)

# Chi-square test on 2x2 table (Winter vs Non-Winter **within** Northeast vs Other)
a1_matrix <- analysis_df %>%
  mutate(GROUP = paste(REGION, SEASON, sep = "_")) %>%
  group_by(GROUP) %>%
  summarise(cancelled = sum(CANCELLED == 1),
            not_cancelled = sum(CANCELLED == 0), .groups = "drop") %>%
  # reshape to 2x2: rows=REGION, cols=SEASON with counts of CANCELLED
  tidyr::separate(GROUP, into = c("REGION","SEASON"), sep = "_") %>%
  tidyr::pivot_wider(names_from = SEASON, values_from = cancelled) %>%
  select(-REGION) %>% as.matrix()

# Safeguard: if structure not exactly 2x2, skip test
if (all(dim(a1_matrix) == c(2,2))) {
  cat("\nChi-square test (counts = CANCELLED by REGION x SEASON):\n")
  print(chisq.test(a1_matrix, correct = FALSE))
}

# Plot: cancellation rate by Region & Season
ggplot(a1_tab, aes(x = REGION, y = cancel_rate, fill = SEASON)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_text(aes(label = scales::percent(cancel_rate, accuracy = 0.1)),
            position = position_dodge(width = 0.7), vjust = -0.25, size = 3) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
  labs(title = "Cancellation Rate by Region and Season",
       x = "Region of Origin Airport", y = "Cancellation Rate",
       fill = "Season") +
  theme_minimal()

# Interpretation guide (to include in your write-up):
# - If Northeast bars are higher than 'Other' in Winter, it supports the objective.
# - The Chi-square p-value < 0.05 indicates region-season and cancellation are associated.


# ======================================================
# 5.2.2: Analysis 2: Seasonal Cancellation Pattern in the Northeast
#  - Focus only on Northeast
#  - Compare Winter vs Non-Winter cancellation rates
#  - Proportion test with 95% CI + plot
# ======================================================

a2_df <- analysis_df %>%
  filter(REGION == "Northeast") %>%
  group_by(SEASON) %>%
  summarise(
    flights = n(),
    cancelled = sum(CANCELLED == 1),
    cancel_rate = cancelled / flights,
    .groups = "drop"
  )

print(a2_df)

# Proportion test: Winter vs Non-Winter within Northeast
if (nrow(a2_df) == 2) {
  x <- a2_df$cancelled
  n <- a2_df$flights
  a2_test <- prop.test(x = x, n = n, correct = FALSE)
  cat("\nProportion test (Northeast: Winter vs Non-Winter):\n")
  print(a2_test)
}

# Plot
ggplot(a2_df, aes(x = SEASON, y = cancel_rate, fill = SEASON)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(cancelled, "/", flights, " (",
                               scales::percent(cancel_rate, accuracy = 0.1), ")")),
            vjust = -0.3, size = 3) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, NA)) +
  labs(title = "Northeast: Cancellation Rate by Season",
       x = "Season", y = "Cancellation Rate") +
  theme_minimal() +
  theme(legend.position = "none")

# Interpretation guide:
# - If Winter rate > Non-Winter and prop.test p-value < 0.05, it supports the objective.


# ======================================================
# 5.2.3: Analysis 3: Carrier Reliability in Northeast Winter
#  - Within Northeast + Winter subset
#  - Rank carriers by cancellation %
#  - Apply a minimum flights threshold to avoid tiny samples
# ======================================================

a3_df <- analysis_df %>%
  filter(REGION == "Northeast", SEASON == "Winter") %>%
  mutate(CARRIER = if ("AIRLINE_NAME" %in% names(.)) AIRLINE_NAME else as.character(AIRLINE)) %>%
  group_by(CARRIER) %>%
  summarise(
    flights = n(),
    cancelled = sum(CANCELLED == 1),
    cancel_rate = cancelled / flights,
    .groups = "drop"
  ) %>%
  # Keep carriers with enough flights to be meaningful (e.g., >= 100)
  filter(flights >= 100) %>%
  arrange(desc(cancel_rate)) %>%
  mutate(CARRIER = fct_reorder(CARRIER, cancel_rate))

print(a3_df)

ggplot(a3_df, aes(x = CARRIER, y = cancel_rate)) +
  geom_col() +
  geom_text(aes(label = paste0(scales::percent(cancel_rate, accuracy = 0.1),
                               " (", cancelled, "/", flights, ")")),
            hjust = -0.05, size = 3) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, max(a3_df$cancel_rate) * 1.15)) +
  coord_flip() +
  labs(title = "Carrier Cancellation Rate — Northeast (Winter)",
       x = "Carrier", y = "Cancellation Rate") +
  theme_minimal()

# Interpretation guide:
# - Carriers at the top have higher cancellation rates in Northeast Winter.
# - Discuss operational reliability differences (e.g., de-icing ops, buffer times).

#======================================================
# 5.3: objective 3: Ammar Yaser Yaseen Alqadasi TP082370
#======================================================


# -----  5.3.1: Analysis: NE Winter Reliability -----

library(dplyr)
library(lubridate)

# Analysis: NE Winter Reliability
# Define regions and parameters
NE_STATES <- c("ME","NH","VT","MA","RI","CT","NY","NJ","PA")
WINTER_MONTHS <- c(12, 1, 2)
# Check if AIRLINE_NAME already exists
if (!"AIRLINE_NAME" %in% names(flights_clean)) {
  flights_data <- flights_clean %>%
    left_join(airline, by = c("AIRLINE" = "IATA_CODE")) %>%
    rename(AIRLINE_NAME = AIRLINE.y)
} else {
  flights_data <- flights_clean
}

# Get state info for airports
airports_state <- airport %>%
  select(IATA_CODE, STATE) %>%
  distinct()

# Filter to NE winter flights
ne_winter <- flights_data %>%
  mutate(MONTH = month(DATE)) %>%
  left_join(airports_state, by = c("ORIGIN_AIRPORT" = "IATA_CODE")) %>%
  filter(STATE %in% NE_STATES, MONTH %in% WINTER_MONTHS)

# Calculate reliability metrics by airline
reliability <- ne_winter %>%
  mutate(
    on_time = (CANCELLED == 0 & ARRIVAL_DELAY <= 15)
  ) %>%
  group_by(AIRLINE, AIRLINE_NAME) %>%
  summarise(
    total_flights = n(),
    on_time_pct = mean(on_time, na.rm = TRUE) * 100,
    cancel_pct = mean(CANCELLED == 1, na.rm = TRUE) * 100,
    avg_delay = mean(ARRIVAL_DELAY[CANCELLED == 0], na.rm = TRUE),
    reliability_score = 0.7 * (on_time_pct/100) + 0.3 * (1 - cancel_pct/100),
    .groups = "drop"
  ) %>%
  filter(total_flights >= 50) %>%
  arrange(desc(reliability_score))

print(reliability, n = Inf)








# 1. Reliability Score Comparison (vis)
# Create the visualization
viz1 <- ggplot(reliability, aes(x = reorder(AIRLINE_NAME, reliability_score), 
                                y = reliability_score * 100)) +
  geom_col(aes(fill = reliability_score), show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", reliability_score * 100)), 
            hjust = -0.2, size = 3.5) +
  scale_fill_gradient(low = "#d73027", high = "#1a9850") +
  coord_flip() +
  labs(title = "Airline Reliability Score",
       subtitle = "NE States Winter Performance (70% on-time + 30% non-cancellation)",
       x = NULL,
       y = "Reliability Score (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        panel.grid.major.y = element_blank()) +
  ylim(0, 100)

print(viz1)

# Save as PNG
ggsave("airline_reliability_score.png", 
       plot = viz1, 
       width = 10, 
       height = 8, 
       dpi = 300)


#5.3.1.1
# 2. On-Time vs Cancellation Rate Scatter
viz2 <- ggplot(reliability, aes(x = cancel_pct, y = on_time_pct)) +
  geom_point(aes(size = total_flights, color = reliability_score), alpha = 0.7) +
  geom_text(aes(label = AIRLINE), vjust = -0.8, size = 3, check_overlap = TRUE) +
  scale_color_gradient(low = "#d73027", high = "#1a9850", name = "Reliability\nScore") +
  scale_size_continuous(name = "Total Flights", range = c(3, 12)) +
  labs(title = "On-Time Performance vs Cancellation Rate",
       subtitle = "Bubble size = flight volume",
       x = "Cancellation Rate (%)",
       y = "On-Time Rate (%)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "right")

print(viz2)
ggsave("on_time_pct.png", 
       plot = viz2, 
       width = 10, 
       height = 8, 
       dpi = 300)


# 3. Multiple Metrics Heatmap-style Comparison
reliability_long <- reliability %>%
  select(AIRLINE_NAME, on_time_pct, cancel_pct, avg_delay) %>%
  mutate(
    on_time_rank = rank(-on_time_pct),
    cancel_rank = rank(cancel_pct),
    delay_rank = rank(avg_delay)
  ) %>%
  tidyr::pivot_longer(cols = c(on_time_rank, cancel_rank, delay_rank),
                      names_to = "metric",
                      values_to = "rank") %>%
  mutate(metric = factor(metric, 
                         levels = c("on_time_rank", "cancel_rank", "delay_rank"),
                         labels = c("On-Time\nPerformance", "Cancellation\nRate", "Average\nDelay")))
# 5.3.1.2
viz3 <- ggplot(reliability_long, 
               aes(x = metric, y = reorder(AIRLINE_NAME, -rank))) +
  geom_tile(aes(fill = rank), color = "white", size = 1) +
  geom_text(aes(label = round(rank)), color = "white", fontface = "bold", size = 4) +
  scale_fill_gradient(low = "#1a9850", high = "#d73027", name = "Rank\n(1=Best)") +
  labs(title = "Airline Performance Ranking Across Metrics",
       subtitle = "Lower rank number = better performance",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(face = "bold"),
        panel.grid = element_blank())

print(viz3)
ggsave("Multiple-Metrics.png", 
       plot = viz3, 
       width = 10, 
       height = 8, 
       dpi = 300)




#-------------------------------------------------#
#-------------------------------------------------#

#5.3.2: second_analysis
#second_analysis
# ----- Cancellation Reasons Analysis -----
# Prepare data with airline names
if (!"AIRLINE_NAME" %in% names(flights_clean)) {
  flights_data <- flights_clean %>%
    left_join(airline, by = c("AIRLINE" = "IATA_CODE")) %>%
    rename(AIRLINE_NAME = AIRLINE.y)
} else {
  flights_data <- flights_clean
}

# Get state info
airports_state <- airport %>%
  select(IATA_CODE, STATE) %>%
  distinct()

# Filter to NE winter flights
ne_winter <- flights_data %>%
  mutate(MONTH = month(DATE)) %>%
  left_join(airports_state, by = c("ORIGIN_AIRPORT" = "IATA_CODE")) %>%
  filter(STATE %in% NE_STATES, MONTH %in% WINTER_MONTHS)

# Analyze cancellation reasons
cancellation_analysis <- ne_winter %>%
  group_by(AIRLINE, AIRLINE_NAME) %>%
  summarise(
    total_flights = n(),
    total_cancels = sum(CANCELLED == 1, na.rm = TRUE),
    
    # Count by reason
    airline_fault = sum(CANCELLED == 1 & CANCELLATION_REASON == "A", na.rm = TRUE),
    weather = sum(CANCELLED == 1 & CANCELLATION_REASON == "B", na.rm = TRUE),
    nas_system = sum(CANCELLED == 1 & CANCELLATION_REASON == "C", na.rm = TRUE),
    security = sum(CANCELLED == 1 & CANCELLATION_REASON == "D", na.rm = TRUE),
    
    # Overall cancellation rate
    cancel_rate = (total_cancels / total_flights) * 100,
    
    # Rate per 100 flights by reason
    airline_per_100 = (airline_fault / total_flights) * 100,
    weather_per_100 = (weather / total_flights) * 100,
    nas_per_100 = (nas_system / total_flights) * 100,
    security_per_100 = (security / total_flights) * 100,
    
    .groups = "drop"
  ) %>%
  filter(total_flights >= 50) %>%  # Exclude small samples
  arrange(desc(cancel_rate))

# Display results
cat("\n=== Cancellation Analysis: NE Winter ===\n\n")
print(cancellation_analysis, n = Inf)

# Summary by reason (what % of all cancellations are due to each reason)
if (sum(cancellation_analysis$total_cancels) > 0) {
  cat("\n=== Overall Cancellation Breakdown ===\n")
  reason_summary <- cancellation_analysis %>%
    summarise(
      total_cancels = sum(total_cancels),
      airline_pct = sum(airline_fault) / sum(total_cancels) * 100,
      weather_pct = sum(weather) / sum(total_cancels) * 100,
      nas_pct = sum(nas_system) / sum(total_cancels) * 100,
      security_pct = sum(security) / sum(total_cancels) * 100
    )
  print(reason_summary)
}

# Top airlines by weather cancellations
cat("\n=== Airlines Most Affected by Weather ===\n")
weather_ranking <- cancellation_analysis %>%
  select(AIRLINE, AIRLINE_NAME, total_flights, weather, weather_per_100) %>%
  arrange(desc(weather_per_100)) %>%
  head(10)
print(weather_ranking, n = Inf)



library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Stacked Bar Chart - Cancellation Reasons by Airline
cancel_long <- cancellation_analysis %>%
  select(AIRLINE_NAME, airline_fault, weather, nas_system, security) %>%
  pivot_longer(cols = c(airline_fault, weather, nas_system, security),
               names_to = "reason",
               values_to = "count") %>%
  mutate(reason = factor(reason, 
                         levels = c("security", "nas_system", "weather", "airline_fault"),
                         labels = c("Security", "NAS/System", "Weather", "Airline Fault")))
#5.3.2.1
SecAnaviz1 <- ggplot(cancel_long, aes(x = reorder(AIRLINE_NAME, count, sum), y = count, fill = reason)) +
  geom_col() +
  scale_fill_manual(values = c("Security" = "#fee08b",
                               "NAS/System" = "#d73027", 
                               "Weather" = "#4575b4",
                               "Airline Fault" = "#fc8d59"),
                    name = "Cancellation\nReason") +
  coord_flip() +
  labs(title = "Cancellation Breakdown by Airline",
       subtitle = "NE States Winter - Total cancellations by reason",
       x = NULL,
       y = "Number of Cancellations") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "right",
        panel.grid.major.y = element_blank())

print(SecAnaviz1)
ggsave("Cancellation-Reasons-by-Airline.png", 
       plot = SecAnaviz1, 
       width = 10, 
       height = 8, 
       dpi = 300)
#5.3.2.2
# 2. Weather vs Airline Fault Scatter Plot
SecAnaviz2 <- ggplot(cancellation_analysis, 
                     aes(x = airline_per_100, y = weather_per_100)) +
  geom_point(aes(size = total_cancels, color = cancel_rate), alpha = 0.7) +
  geom_text(aes(label = AIRLINE), vjust = -0.8, size = 3, check_overlap = TRUE) +
  scale_color_gradient(low = "#1a9850", high = "#d73027", 
                       name = "Total Cancel\nRate (%)") +
  scale_size_continuous(name = "Total\nCancellations", range = c(3, 12)) +
  labs(title = "Weather vs Airline Fault Cancellations",
       subtitle = "Per 100 flights (bubble size = total cancellations)",
       x = "Airline Fault Cancellations (per 100 flights)",
       y = "Weather Cancellations (per 100 flights)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(SecAnaviz2)

#-------------------------------------------------#
#-------------------------------------------------#

# 5.3.3: Analysis 3: Delay Metrics 
library(dplyr)
library(lubridate)

# Define constants
NE_STATES <- c("ME", "NH", "VT", "MA", "RI", "CT", "NY", "NJ", "PA")
WINTER_MONTHS <- c(12, 1, 2)

# Prepare data
flights_with_airlines <- flights_clean %>%
  left_join(airline, by = c("AIRLINE" = "IATA_CODE"))

flights_with_states <- flights_with_airlines %>%
  left_join(
    airport %>% select(IATA_CODE, STATE),
    by = c("ORIGIN_AIRPORT" = "IATA_CODE")
  )

# Filter to Northeast winter flights that weren't cancelled
ne_winter_completed <- flights_with_states %>%
  filter(
    STATE %in% NE_STATES,
    month(DATE) %in% WINTER_MONTHS,
    CANCELLED == 0
  )

# Calculate delay metrics by airline
delay_analysis <- ne_winter_completed %>%
  group_by(AIRLINE) %>%
  summarise(
    airline_name = first(AIRLINE_NAME),
    flights = n(),
    median_delay = median(ARRIVAL_DELAY, na.rm = TRUE),
    p90_delay = quantile(ARRIVAL_DELAY, 0.90, na.rm = TRUE),
    pct_on_time = mean(ARRIVAL_DELAY <= 15, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  arrange(median_delay)

print(delay_analysis, n = Inf)



library(ggplot2)
library(tidyr)
library(dplyr)

# Assuming delay_analysis is already created from the previous code

# ===== 5.3.3.1: ThirdVis1: Median Delay by Airline =====
ThirdVis1 <- ggplot(delay_analysis, aes(x = reorder(airline_name, median_delay), y = median_delay)) +
  geom_col(aes(fill = median_delay), show.legend = FALSE) +
  scale_fill_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 0) +
  coord_flip() +
  labs(
    title = "ThirdVis1: Median Arrival Delay by Airline",
    subtitle = "Northeast Winter Flights (Completed Only)",
    x = "Airline",
    y = "Median Delay (minutes)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(ThirdVis1)
ggsave("Median-Delay-by-Airline.png", 
       plot = ThirdVis1, 
       width = 10, 
       height = 8, 
       dpi = 300)


# ===== 5.3.3.2: ThirdVis2: On-Time Performance =====
ThirdVis2 <- ggplot(delay_analysis, aes(x = reorder(airline_name, -pct_on_time), y = pct_on_time)) +
  geom_col(aes(fill = pct_on_time), show.legend = FALSE) +
  scale_fill_gradient(low = "red", high = "green") +
  coord_flip() +
  geom_hline(yintercept = 80, linetype = "dashed", color = "blue", alpha = 0.7) +
  labs(
    title = "ThirdVis2: On-Time Performance by Airline",
    subtitle = "% of flights with ≤15 min arrival delay (Northeast Winter)",
    x = "Airline",
    y = "On-Time Performance (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(ThirdVis2)
ggsave("On-Time-Performance.png", 
       plot = ThirdVis2, 
       width = 10, 
       height = 8, 
       dpi = 300)

# ===== 5.3.3.3: ThirdVis3: Delay Distribution Scatter =====
ThirdVis3 <- ggplot(delay_analysis, aes(x = median_delay, y = p90_delay)) +
  geom_point(aes(size = flights, color = pct_on_time), alpha = 0.7) +
  geom_text(aes(label = airline_name), vjust = -0.8, size = 3, check_overlap = TRUE) +
  scale_color_gradient(low = "red", high = "green", name = "On-Time %") +
  scale_size_continuous(name = "# Flights", range = c(3, 12)) +
  labs(
    title = "ThirdVis3: Delay Severity - Median vs 90th Percentile",
    subtitle = "Bubble size = number of flights",
    x = "Median Arrival Delay (minutes)",
    y = "90th Percentile Delay (minutes)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

ThirdVis3

# ===== 5.3.3.4: ThirdVis4: Performance Heatmap =====
heatmap_data <- delay_analysis %>%
  select(airline_name, median_delay, p90_delay, pct_on_time) %>%
  mutate(
    median_delay_scaled = scale(median_delay)[,1],
    p90_delay_scaled = scale(p90_delay)[,1],
    pct_on_time_scaled = scale(pct_on_time)[,1]
  ) %>%
  select(airline_name, ends_with("_scaled")) %>%
  pivot_longer(-airline_name, names_to = "metric", values_to = "value") %>%
  mutate(metric = case_when(
    metric == "median_delay_scaled" ~ "Median Delay",
    metric == "p90_delay_scaled" ~ "P90 Delay",
    metric == "pct_on_time_scaled" ~ "On-Time %"
  ))

ThirdVis4 <- ggplot(heatmap_data, aes(x = metric, y = reorder(airline_name, -value), fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "green", mid = "white", high = "red", midpoint = 0,
                       name = "Scaled\nValue") +
  labs(
    title = "ThirdVis4: Airline Performance Heatmap",
    subtitle = "Scaled metrics (higher = worse performance except On-Time %)",
    x = "Metric",
    y = "Airline"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(ThirdVis4)




#-------------------------------------------------#
#-------------------------------------------------#


# ----- 5.3.4: Analysis 4: Delay Recovery -----

# Prepare data
flights_with_airlines <- flights_clean %>%
  left_join(airline, by = c("AIRLINE" = "IATA_CODE"))

flights_with_states <- flights_with_airlines %>%
  left_join(
    airport %>% select(IATA_CODE, STATE),
    by = c("ORIGIN_AIRPORT" = "IATA_CODE")
  )

# Filter to Northeast winter completed flights with delay data
ne_winter_delays <- flights_with_states %>%
  filter(
    STATE %in% NE_STATES,
    month(DATE) %in% WINTER_MONTHS,
    CANCELLED == 0,
    !is.na(DEPARTURE_DELAY),
    !is.na(ARRIVAL_DELAY)
  ) %>%
  mutate(
    # Calculate recovery metrics
    delay_recovered = pmax(0, DEPARTURE_DELAY - ARRIVAL_DELAY),
    recovery_pct = ifelse(
      DEPARTURE_DELAY > 0,
      100 * pmax(0, DEPARTURE_DELAY - ARRIVAL_DELAY) / DEPARTURE_DELAY,
      NA
    )
  )

# Calculate recovery ability by airline
recovery_analysis <- ne_winter_delays %>%
  group_by(AIRLINE) %>%
  summarise(
    airline_name = first(AIRLINE_NAME),
    flights = n(),
    
    # Average delays
    avg_dep_delay = mean(DEPARTURE_DELAY, na.rm = TRUE),
    avg_arr_delay = mean(ARRIVAL_DELAY, na.rm = TRUE),
    
    # Recovery metrics
    avg_recovery_min = mean(delay_recovered, na.rm = TRUE),
    avg_recovery_pct = mean(recovery_pct, na.rm = TRUE),
    
    # Correlation (how much dep delay translates to arr delay)
    dep_arr_correlation = cor(DEPARTURE_DELAY, ARRIVAL_DELAY, use = "complete.obs"),
    
    .groups = "drop"
  ) %>%
  arrange(desc(avg_recovery_pct))

print(recovery_analysis, n = Inf)

library(ggplot2)
library(tidyr)
library(dplyr)

# Assuming recovery_analysis is already created from the previous code

# ===== 5.3.4.1:  FourthVis1: Average Recovery Percentage by Airline =====
FourthVis1 <- ggplot(recovery_analysis, aes(x = reorder(airline_name, avg_recovery_pct), y = avg_recovery_pct)) +
  geom_col(aes(fill = avg_recovery_pct), show.legend = FALSE) +
  scale_fill_gradient(low = "red", high = "green") +
  coord_flip() +
  labs(
    title = "FourthVis1: Average Delay Recovery by Airline",
    subtitle = "% of departure delay recovered by arrival (Northeast Winter)",
    x = "Airline",
    y = "Average Recovery (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(FourthVis1)
ggsave("Average-Recovery-Percentage-by-Airliner.png", 
       plot = FourthVis1, 
       width = 10, 
       height = 8, 
       dpi = 300)

# ===== 5.3.4.2: FourthVis2: Departure vs Arrival Delay Comparison =====
delay_comparison <- recovery_analysis %>%
  select(airline_name, avg_dep_delay, avg_arr_delay) %>%
  pivot_longer(-airline_name, names_to = "delay_type", values_to = "minutes") %>%
  mutate(delay_type = ifelse(delay_type == "avg_dep_delay", "Departure", "Arrival"))

FourthVis2 <- ggplot(delay_comparison, aes(x = reorder(airline_name, -minutes), y = minutes, fill = delay_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Departure" = "#FF6B6B", "Arrival" = "#4ECDC4"), name = "Delay Type") +
  coord_flip() +
  labs(
    title = "FourthVis2: Departure vs Arrival Delay",
    subtitle = "Airlines that recover well show larger gaps",
    x = "Airline",
    y = "Average Delay (minutes)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(FourthVis2)
ggsave("Departure-vs-Arrival-Delay-Comparison.png", 
       plot = FourthVis2, 
       width = 10, 
       height = 8, 
       dpi = 300)

# ===== 5.3.4.3: FourthVis3: Recovery Efficiency Scatter =====
FourthVis3 <- ggplot(recovery_analysis, aes(x = avg_dep_delay, y = avg_arr_delay)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_point(aes(size = flights, color = avg_recovery_pct), alpha = 0.7) +
  geom_text(aes(label = airline_name), vjust = -0.8, size = 3, check_overlap = TRUE) +
  scale_color_gradient(low = "red", high = "green", name = "Recovery %") +
  scale_size_continuous(name = "# Flights", range = c(3, 12)) +
  labs(
    title = "FourthVis3: Recovery Efficiency",
    subtitle = "Points below diagonal = better recovery (dashed line = no recovery)",
    x = "Average Departure Delay (minutes)",
    y = "Average Arrival Delay (minutes)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(FourthVis3)
ggsave("Recovery-Efficiency-Scatter.png", 
       plot = FourthVis3, 
       width = 10, 
       height = 8, 
       dpi = 300)


# ===== 5.3.4.4:FourthVis4: Recovery Metrics Heatmap =====
heatmap_data <- recovery_analysis %>%
  select(airline_name, avg_recovery_pct, avg_recovery_min, dep_arr_correlation) %>%
  mutate(
    recovery_pct_scaled = scale(avg_recovery_pct)[,1],
    recovery_min_scaled = scale(avg_recovery_min)[,1],
    correlation_scaled = scale(dep_arr_correlation)[,1]
  ) %>%
  select(airline_name, ends_with("_scaled")) %>%
  pivot_longer(-airline_name, names_to = "metric", values_to = "value") %>%
  mutate(metric = case_when(
    metric == "recovery_pct_scaled" ~ "Recovery %",
    metric == "recovery_min_scaled" ~ "Minutes Recovered",
    metric == "correlation_scaled" ~ "Dep-Arr Correlation"
  ))

FourthVis4 <- ggplot(heatmap_data, aes(x = metric, y = reorder(airline_name, value), fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "red", mid = "white", high = "green", midpoint = 0,
    name = "Scaled\nValue"
  ) +
  labs(
    title = "FourthVis4: Recovery Ability Heatmap",
    subtitle = "Green = better recovery; Red for correlation = worse (delays carry through)",
    x = "Metric",
    y = "Airline"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(FourthVis4)

ggsave("Recovery-Metrics-Heatmap.png", 
       plot = FourthVis4, 
       width = 10, 
       height = 8, 
       dpi = 300)



#-------------------------------------------------#
#-------------------------------------------------#




library(dplyr)
library(lubridate)
#5th analysis
# ----- 5.3.5: Analysis 5: Weather Sensitivity -----

# Prepare data
flights_with_airlines <- flights_clean %>%
  left_join(airline, by = c("AIRLINE" = "IATA_CODE"))

flights_with_states <- flights_with_airlines %>%
  left_join(
    airport %>% select(IATA_CODE, STATE),
    by = c("ORIGIN_AIRPORT" = "IATA_CODE")
  )

# Filter to Northeast winter flights
ne_winter <- flights_with_states %>%
  filter(
    STATE %in% NE_STATES,
    month(DATE) %in% WINTER_MONTHS
  ) %>%
  mutate(
    # Classify weather conditions
    has_weather_delay = WEATHER_DELAY > 0,
    is_cancelled = CANCELLED == 1,
    is_on_time = ARRIVAL_DELAY <= 15 & CANCELLED == 0
  )

# Calculate weather sensitivity by airline
weather_analysis <- ne_winter %>%
  group_by(AIRLINE) %>%
  summarise(
    airline_name = first(AIRLINE_NAME),
    total_flights = n(),
    
    # Weather exposure
    pct_weather_affected = 100 * mean(has_weather_delay, na.rm = TRUE),
    
    # Cancellation rates
    cancel_rate_good_weather = 100 * mean(is_cancelled & !has_weather_delay, na.rm = TRUE),
    cancel_rate_bad_weather = 100 * mean(is_cancelled & has_weather_delay, na.rm = TRUE),
    cancel_rate_increase = cancel_rate_bad_weather - cancel_rate_good_weather,
    
    # On-time performance (for completed flights)
    ontime_good_weather = 100 * mean(is_on_time & !has_weather_delay, na.rm = TRUE) / 
      pmax(mean(!has_weather_delay & !is_cancelled, na.rm = TRUE), 0.001),
    ontime_bad_weather = 100 * mean(is_on_time & has_weather_delay, na.rm = TRUE) / 
      pmax(mean(has_weather_delay & !is_cancelled, na.rm = TRUE), 0.001),
    ontime_decrease = ontime_bad_weather - ontime_good_weather,
    
    .groups = "drop"
  ) %>%
  arrange(desc(cancel_rate_increase))

print(weather_analysis, n = Inf)

library(ggplot2)
library(tidyr)
library(dplyr)

# Assuming weather_analysis is already created from the previous code

# ===== 5.3.5.1: FifthVis1: Cancellation Rate Increase by Airline =====
FifthVis1 <- ggplot(weather_analysis, aes(x = reorder(airline_name, cancel_rate_increase), 
                                          y = cancel_rate_increase)) +
  geom_col(aes(fill = cancel_rate_increase), show.legend = FALSE) +
  scale_fill_gradient(low = "green", high = "red") +
  coord_flip() +
  labs(
    title = "FifthVis1: Weather Sensitivity - Cancellation Impact",
    subtitle = "Increase in cancellation rate during bad weather (percentage points)",
    x = "Airline",
    y = "Cancellation Rate Increase (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(FifthVis1)
ggsave(" Cancellation-Rate-Increase-by-Airline.png", 
       plot = FourthVis4, 
       width = 10, 
       height = 8, 
       dpi = 300)

# ===== 5.3.5.2: FifthVis2: Good Weather vs Bad Weather Cancellation Rates =====
cancel_comparison <- weather_analysis %>%
  select(airline_name, cancel_rate_good_weather, cancel_rate_bad_weather) %>%
  pivot_longer(-airline_name, names_to = "weather_type", values_to = "cancel_rate") %>%
  mutate(weather_type = ifelse(weather_type == "cancel_rate_good_weather", 
                               "Good Weather", "Bad Weather"))

FifthVis2 <- ggplot(cancel_comparison, aes(x = reorder(airline_name, -cancel_rate), 
                                           y = cancel_rate, fill = weather_type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Good Weather" = "#4ECDC4", "Bad Weather" = "#FF6B6B"), 
                    name = "Weather Condition") +
  coord_flip() +
  labs(
    title = "FifthVis2: Cancellation Rates by Weather Condition",
    subtitle = "Comparing airline performance in good vs bad weather",
    x = "Airline",
    y = "Cancellation Rate (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

print(FifthVis2)
ggsave(" Good Weather vs Bad Weather Cancellation Rates.png", 
       plot = FifthVis2, 
       width = 10, 
       height = 8, 
       dpi = 300)

# ===== 5.3.5.3: FifthVis3: On-Time Performance Drop Scatter =====
FifthVis3 <- ggplot(weather_analysis, aes(x = ontime_good_weather, y = ontime_bad_weather)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_point(aes(size = total_flights, color = ontime_decrease), alpha = 0.7) +
  geom_text(aes(label = airline_name), vjust = -0.8, size = 3, check_overlap = TRUE) +
  scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = -20,
                        name = "Performance\nDrop (%)") +
  scale_size_continuous(name = "# Flights", range = c(3, 12)) +
  labs(
    title = "FifthVis3: On-Time Performance - Good vs Bad Weather",
    subtitle = "Points below diagonal = worse in bad weather (dashed line = no change)",
    x = "On-Time % (Good Weather)",
    y = "On-Time % (Bad Weather)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right"
  )

print(FifthVis3)
ggsave(" On-Time Performance Drop Scatter.png", 
       plot = FifthVis3, 
       width = 10, 
       height = 8, 
       dpi = 300)

# ===== 5.3.5.4: FifthVis4: Weather Sensitivity Heatmap =====
heatmap_data <- weather_analysis %>%
  select(airline_name, cancel_rate_increase, ontime_decrease, pct_weather_affected) %>%
  mutate(
    cancel_increase_scaled = scale(cancel_rate_increase)[,1],
    ontime_decrease_scaled = scale(ontime_decrease)[,1],
    weather_affected_scaled = scale(pct_weather_affected)[,1]
  ) %>%
  select(airline_name, ends_with("_scaled")) %>%
  pivot_longer(-airline_name, names_to = "metric", values_to = "value") %>%
  mutate(metric = case_when(
    metric == "cancel_increase_scaled" ~ "Cancel Rate Increase",
    metric == "ontime_decrease_scaled" ~ "On-Time Drop",
    metric == "weather_affected_scaled" ~ "Weather Exposure"
  ))

FifthVis4 <- ggplot(heatmap_data, aes(x = metric, y = reorder(airline_name, -value), fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "green", mid = "white", high = "red", midpoint = 0,
    name = "Scaled\nValue"
  ) +
  labs(
    title = "FifthVis4: Weather Sensitivity Heatmap",
    subtitle = "Red = more weather-sensitive (worse performance in bad weather)",
    x = "Metric",
    y = "Airline"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(FifthVis4)
ggsave("Weather Sensitivity Heatmap.png", 
       plot = FifthVis4, 
       width = 10, 
       height = 8, 
       dpi = 300)

#======================================================
# 5.4: objective 4: Akshit Jangid TP083906
#======================================================

#======================================================
# 5. Analysis
#   Objective: Extra arrival delay for Northeast flights
#   in winter, and airline reliability differences.
#======================================================
# [ Akshit Jangid , TP083906 ]

library(dplyr)
library(ggplot2)
library(lubridate)
library(forcats)

# Define Northeast states
NE_STATES <- c("ME","NH","VT","MA","RI","CT","NY","NJ","PA")

# Add Region + Season once
analysis_df <- flights_clean %>%
  left_join(airport %>% select(IATA_CODE, STATE),
            by = c("ORIGIN_AIRPORT" = "IATA_CODE")) %>%
  rename(ORIGIN_STATE = STATE) %>%
  mutate(
    SEASON = case_when(
      month(DATE) %in% c(12, 1, 2) ~ "Winter",
      TRUE ~ "Non-Winter"
    ),
    REGION = ifelse(ORIGIN_STATE %in% NE_STATES, "Northeast", "Other")
  ) %>%
  filter(!is.na(ARRIVAL_DELAY), !is.na(ORIGIN_STATE))

# Create quick subsets
df_NE       <- filter(analysis_df, REGION == "Northeast")
df_NE_winter <- filter(df_NE, SEASON == "Winter")
df_NE_nonwinter <- filter(df_NE, SEASON == "Non-Winter")

# ======================================================
# 5.4.1: Analysis 1: Average Arrival Delay by Region × Season
# ======================================================

a1_summary <- analysis_df %>%
  group_by(REGION, SEASON) %>%
  summarise(
    flights = n(),
    avg_delay = mean(ARRIVAL_DELAY),
    .groups = "drop"
  )

print(a1_summary)

# Test: Compare Northeast Winter vs All Others
t_res1 <- t.test(df_NE_winter$ARRIVAL_DELAY,
                 filter(analysis_df, !(REGION=="Northeast" & SEASON=="Winter"))$ARRIVAL_DELAY)
cat("\nT-test: Northeast Winter vs Others\n")
print(t_res1)

# Plot
ggplot(a1_summary, aes(x = REGION, y = avg_delay, fill = SEASON)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(avg_delay,1)),
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  labs(title = "Average Arrival Delay by Region and Season",
       y = "Avg Delay (minutes)", x = "Region") +
  theme_minimal()

# ======================================================
# 5.4.2: Analysis 2: Northeast Only — Winter vs Non-Winter
# ======================================================

a2_summary <- df_NE %>%
  group_by(SEASON) %>%
  summarise(
    flights = n(),
    avg_delay = mean(ARRIVAL_DELAY),
    .groups = "drop"
  )

print(a2_summary)

# Test: Winter vs Non-Winter inside Northeast
t_res2 <- t.test(df_NE_winter$ARRIVAL_DELAY, df_NE_nonwinter$ARRIVAL_DELAY)
cat("\nT-test: Northeast Winter vs Non-Winter\n")
print(t_res2)

# Plot
ggplot(a2_summary, aes(x = SEASON, y = avg_delay, fill = SEASON)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(avg_delay,1)), vjust = -0.3, size = 3) +
  labs(title = "Northeast Flights: Avg Arrival Delay by Season",
       y = "Avg Delay (minutes)", x = "Season") +
  theme_minimal() +
  theme(legend.position = "none")

# ======================================================
# 5.4.3: Analysis 3: Carrier Reliability in Northeast Winter
# ======================================================

a3_summary <- df_NE_winter %>%
  mutate(CARRIER = if ("AIRLINE_NAME" %in% names(.)) AIRLINE_NAME else as.character(AIRLINE)) %>%
  group_by(CARRIER) %>%
  summarise(
    flights = n(),
    avg_delay = mean(ARRIVAL_DELAY),
    .groups = "drop"
  ) %>%
  filter(flights >= 100) %>%
  arrange(desc(avg_delay)) %>%
  mutate(CARRIER = fct_reorder(CARRIER, avg_delay))

print(a3_summary)

# Plot
ggplot(a3_summary, aes(x = CARRIER, y = avg_delay)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(avg_delay,1)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "Carrier Avg Arrival Delay — Northeast Winter",
       y = "Avg Delay (minutes)", x = "Carrier") +
  theme_minimal()


