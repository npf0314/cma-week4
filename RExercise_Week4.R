library("readr")
library("dplyr")
library("sf")
library('ggplot2')

wildschwein <- read_delim("C:/_Data/Master/PaT_24/week_4/data/wildschwein_BE_2056.csv", ",")

# Careful! What Timezone is assumed?
sabi <- wildschwein |>
  st_as_sf(coords = c("E", "N"), crs = 2056, remove = FALSE) |>
  filter(TierName == "Sabi", DatetimeUTC >= "2015-07-01", DatetimeUTC < "2015-07-03")

distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

sabi <- sabi |>
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  # distance to pos -30 minutes
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  # distance to pos -15 minutes
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), # distance to pos +15 mintues
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  # distance to pos +30 minutes
  )

sabi <- sabi |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2))
  ) |>
  ungroup()

sabi


sabi <- sabi |>
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

sabi_filter <- sabi |>
  filter(!static)

sabi_filter |>
  ggplot(aes(E, N)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

#____________________________________________________________________________________________________

# Preperation

# Load necessary libraries
library(sf)
library(dplyr)
library(readr)

# Define file path
file_path <- "C:/_Data/Master/PaT_24/week_4/data/KML_Zeitinerpoliert.csv"

# Import the data
movement_data <- read_csv(file_path)

# Check the structure of the data
str(movement_data)

# Convert to sf object with the correct CRS (assume EPSG 2056)
movement_sf <- st_as_sf(movement_data, coords = c("Longitude_LV95", "Latitude_LV95"), crs = 2056, remove = FALSE)

# Extract coordinates and bind them back to the sf object
coordinates <- st_coordinates(movement_sf)
movement_sf <- cbind(movement_sf, coordinates)

# Explore the data and choose a single day (e.g., 2024-05-01)
single_day_data <- movement_sf %>%
  filter(as.Date(Time_Point) == as.Date("2024-05-01"))

# Display the filtered data
print(single_day_data)

# Save the modified data to a new CSV file if needed
write_csv(single_day_data, "C:/_Data/Master/PaT_24/week_4/data/single_day_data.csv")


#____________________________________________________________________________________________________

# Task 1: Segmentation

# Define a function to calculate Euclidean distances by element
distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

# Apply the segmentation algorithm
segmented_data <- single_day_data %>%
  mutate(
    nMinus2 = distance_by_element(lag(geometry, 2), geometry),  # distance to pos -30 minutes
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  # distance to pos -15 minutes
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)), # distance to pos +15 minutes
    nPlus2  = distance_by_element(geometry, lead(geometry, 2))  # distance to pos +30 minutes
  ) %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus2, nMinus1, nPlus1, nPlus2), na.rm = TRUE)
  ) %>%
  ungroup()

# Mark static points where stepMean is less than the mean of stepMean
segmented_data <- segmented_data %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE))

# Filter out static points
segmented_data_filter <- segmented_data %>%
  filter(!static)

# Plot the segmented data
segmented_data_filter %>%
  ggplot(aes(X, Y)) + # Use X and Y columns from st_coordinates
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")

# Save the segmented data to a new CSV file if needed
write_csv(segmented_data, "C:/_Data/Master/PaT_24/week_4/data/segmented_data.csv")