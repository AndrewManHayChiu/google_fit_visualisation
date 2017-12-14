## Libraries
library(dplyr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(data.table)

## Load Data

folder <- "Daily Aggregations/"
files <- list.files(path = folder,
                   pattern = "*.csv")
files <- files[1:length(files) - 1]
## Temporarily set working directory to folder contianing daily aggregates
setwd("Daily Aggregations/")

####################################################
## Need to get the date of the file into each row ##
####################################################
read_csv_and_add_date <- function(x) {
  temp_data <- read.csv(x)
  date <- strsplit(x, "*.csv")[[1]][1]
  temp_data$date <- date
  return(temp_data)
}
daily_data <- do.call(bind_rows, lapply(files, read_csv_and_add_date))
## Return working directory back to original
setwd("..")

file <- "Daily Aggregations/Daily Summaries.csv"
data <- read.csv(file)

## Melbourne weather data from Bureau of Meteorology
file2 <- "max_temp.csv"
file3 <- "min_temp.csv"
file4 <- "rainfall.csv"
file5 <- "solar.csv"

max_temp <- read.csv(file2)
min_temp <- read.csv(file3)
rainfall <- read.csv(file4)

## Data Collection

## 531 rows of data (as at 13/12/2017)
## 18 fields
dim(data)

## Most fields are numeric, and one date variable
str(data)

# Data selection ----------------------------------------------------------

## Filter only 2017 data
## Select only:
## - Date
## - Calories
## - Distance
## - Step Count
## - Inactive Duration
## - Walking duration
## - Running duration
data %<>% 
  mutate(Date = ymd(Date)) %>%
  filter(Date >= ymd("2017-01-01")) %>%
  select(date = Date,
         calories = Calories..kcal.,
         distance = Distance..m.,
         steps = Step.count,
         inactive_duration = Inactive.duration..ms.,
         walking_duration = Walking.duration..ms.,
         running_duration = Running.duration..ms.)

## Combine min_temp and max_temp data to get
## both min and max temp into the same data frame

temperature <- inner_join(max_temp, 
                          min_temp, 
                          by = c("Year", 
                                 "Month", 
                                 "Day")) %>%
  select(Year, Month, Day,
         max = Maximum.temperature..Degree.C.,
         min = Minimum.temperature..Degree.C.)


# Data construction -------------------------------------------------------

## Walking duration and running duration is active duration
## Add together to derive active_duration
data %<>%
  mutate(active_duration = walking_duration + running_duration,
         total_duration = active_duration + inactive_duration)

## Combine year, month and day fields in temperature data frame
## to convert to date-time
temperature %<>% 
  mutate(date = paste(Year, Month, Day, sep = "-"),
         date = ymd(date))

## Convert daily_data dates into date format
## Convert start.time in daily_data to time
daily_data %<>%
  mutate(date = ymd(date))

daily_data$time <- sapply(daily_data$Start.time, function(x) strsplit(x, split = "*.000")[[1]][1])
daily_data$time <- hms(daily_data$time)

head(daily_data)


# Data exploration --------------------------------------------------------

summary(data)

### Steps
ggplot(data, aes(x = steps)) +
  geom_histogram()

### Steps appears to have a cycle based on days of the week.
ggplot(data, aes(x = date, y = steps)) +
  geom_line() +
  geom_point()

### Distance
ggplot(data, aes(x = distance)) +
  geom_histogram()

### Steps and distance
ggplot(data, aes(x = steps, y = distance)) +
  geom_point()

### Calories
ggplot(data, aes(x = calories)) +
  geom_histogram()

### Calories and distance
ggplot(data, aes(x = distance, y = calories)) +
  geom_point()

## minimum and maximum temperatures
ggplot(temperature) +
  geom_line(aes(x = date, y = min)) +
  geom_line(aes(x = date, y = max))

## step count at different hours of the day
ggplot(daily_data,
       aes(x = date, y = as.numeric(time), size = Step.count)) +
  geom_point()

# Visualisation -----------------------------------------------------------

## Angles for month labels
angles <- seq(0, -330, length.out = 12)

ymin = -15
ymax = 22

ggplot() +
  ## Bars for distance travelled each day
  geom_bar(data = data,
           aes(x = date,
               y = distance / 1000),
           stat = "identity") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B",
               limits = ymd(c("2017-01-01", "2017-12-31"))) +
  geom_point(data = daily_data,
             aes(x = date, y = as.numeric(time)/3000 + 7, size = Step.count)) +
  ## Lines for temperature
  geom_line(data = temperature,
            aes(x = date, y = (max - 45) / 2)) + # scale values to create inner circle
  geom_line(data = temperature,
            aes(x = date, y = (min - 45) / 2)) + # scale values to create inner circle
  ## Share area between temperature max and min
  geom_ribbon(data = temperature,
              aes(x = date,
                  ymin = (min - 45) / 2,
                  ymax = (max - 45) / 2),
              fill = "blue",
              alpha = 0.5) +
  ## shaded area for holiday to Europe
  geom_rect(data = temperature,
            aes(xmin = ymd("2017-01-09"),
                xmax = ymd("2017-01-31"),
                ymin = ymin, 
                ymax = ymax),
            colour = "grey50",
            alpha = 0.005) +
  ## shaded area for holiday to Gold Coast
  geom_rect(data = temperature,
            aes(xmin = ymd("2017-10-20"),
                xmax = ymd("2017-10-22"),
                ymin = ymin, 
                ymax = ymax),
            colour = "grey50",
            alpha = 0.005) +
  ## shaded area for holiday to Okinawa
  geom_rect(data = temperature,
            aes(xmin = ymd("2017-10-28"),
                xmax = ymd("2017-11-04"),
                ymin = ymin, 
                ymax = ymax),
            colour = "grey50",
            alpha = 0.005) +
  ## shaded area for holiday to Tokyo
  geom_rect(data = temperature,
            aes(xmin = ymd("2017-12-23"),
                xmax = ymd("2017-12-31"),
                ymin = ymin, 
                ymax = ymax),
            colour = "grey50",
            alpha = 0.005) +
  scale_y_continuous(limits = c(-30, 23)) +
  coord_polar() +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = angles),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "non") +
  labs(x = "",
       y = "",
       title = "2017",
       subtitle = "Activity")

