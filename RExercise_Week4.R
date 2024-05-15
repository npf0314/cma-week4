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

#____________________________________________________________________________________________________

# Task 2: Specify and apply threshold d

# Explore stepMean values using summary statistics
summary(segmented_data$stepMean)
hist(segmented_data$stepMean, main = "Histogram of stepMean", xlab = "stepMean")
boxplot(segmented_data$stepMean, main = "Boxplot of stepMean")

# Define threshold value as the mean of stepMean values
threshold_d <- mean(segmented_data$stepMean, na.rm = TRUE)

# Store the new information (boolean to differentiate between stops (TRUE) and moves (FALSE)) in a new column named static
segmented_data <- segmented_data %>%
  mutate(static = stepMean < threshold_d)

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

#____________________________________________________________________________________________________

# Task 3: Visualize segmented trajectories

# Plot the segmented data with colors to distinguish between stops and moves
segmented_data %>%
  ggplot(aes(X, Y, colour = static)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  labs(title = "Segmented Trajectories", x = "Longitude", y = "Latitude", colour = "Static") +
  theme_minimal()

# Save the segmented data with static information to a new CSV file if needed
write_csv(segmented_data, "C:/_Data/Master/PaT_24/week_4/data/segmented_data_with_static.csv")

#____________________________________________________________________________________________________

# Task 4: Segment-based analysis

# Define the rle_id function
rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}

# Assign unique IDs to subtrajectories
segmented_data <- segmented_data %>%
  mutate(segment_id = rle_id(static))

# Visualize the moving segments by colourizing them by segment_id
segmented_data %>%
  ggplot(aes(X, Y, colour = segment_id)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  labs(title = "Segmented Trajectories by Segment ID", x = "Longitude", y = "Latitude", colour = "Segment ID") +
  theme_minimal()

# Determine the segments' duration and remove short segments (e.g. segments with a duration < 5 minutes)
segmented_data <- segmented_data %>%
  group_by(segment_id) %>%
  mutate(duration = difftime(max(Time_Point), min(Time_Point), units = "mins")) %>%
  ungroup() %>%
  filter(duration >= 5)

# Visualize the filtered segments
segmented_data %>%
  ggplot(aes(X, Y, colour = segment_id)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  labs(title = "Filtered Segmented Trajectories by Segment ID", x = "Longitude", y = "Latitude", colour = "Segment ID") +
  theme_minimal()

# Save the final segmented data to a new CSV file if needed
write_csv(segmented_data, "C:/_Data/Master/PaT_24/week_4/data/final_segmented_data.csv")

#____________________________________________________________________________________________________

# Task 5: Similarity measures

# Load the new dataset pedestrian.csv
file_path_pedestrian <- "C:/_Data/Master/PaT_24/week_4//data/pedestrian.csv"
pedestrian_data <- read_csv(file_path_pedestrian)

# Check the structure of the data
str(pedestrian_data)

# Convert to sf object with the correct CRS (assuming EPSG 2056)
pedestrian_sf <- st_as_sf(pedestrian_data, coords = c("E", "N"), crs = 2056, remove = FALSE)

# Explore the trajectories
ggplot(pedestrian_sf, aes(x = st_coordinates(pedestrian_sf)[,1], y = st_coordinates(pedestrian_sf)[,2], colour = as.factor(TrajID))) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  labs(title = "Pedestrian Trajectories", x = "Longitude", y = "Latitude", colour = "Trajectory ID") +
  theme_minimal()

# Save the pedestrian data with trajectory information to a new CSV file if needed
write_csv(pedestrian_data, "C:/_Data/Master/PaT_24/week_4/data/pedestrian_data_with_trajectories.csv")

