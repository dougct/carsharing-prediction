library(tidyverse)
library(viridis)
library(lubridate)
library(broom)
library(lme4)
library(pracma)

Sys.setlocale("LC_TIME", "C")
theme_set(theme_bw())

# Setup
city <- "Vancouver"
charts_dir <- paste('charts/', city, sep = "")

# Load thedataset.
df <- read_csv(paste('data/', city, 'Final.csv', sep = ""))

# Get the size, in bytes, of the dataframe.
object.size(df)

# Transform date columns to date.
df <- df %>%
  mutate(init_date = ymd_hms(init_date)) %>%
  mutate(end_date = ymd_hms(end_date))

# Compute days of the week as factors and label the days as weekdays or weekends.
df <- df %>%
  mutate(
    day = round_date(init_date, unit = "day"),
    weekday = as.factor(wday(day, label = TRUE)),
    isweekend = if_else(weekday %in% c("Sat", "Sun"), TRUE, FALSE)
  )

df <- df %>%
  filter(day >= ymd('2017-01-01') & day <= ymd('2017-12-31'))

df %>%
  group_by(weekday) %>%
  summarise(n = n())

# Compute hour of the day as factors.
df <- df %>%
  mutate(hourofday = as.factor(hour(init_date)))

# Filter the dataset.
df <- df %>%
  # Filter out bookings that take less than 60s and more than 1 hour.
  filter(
    as.duration(init_date %--% end_date) > as.duration(60) &
      as.duration(init_date %--% end_date) < as.duration(3600)
  ) %>%
  # Filter days with less than 500 or more than 6000 bookings.
  group_by(day) %>%
  filter(n_distinct(hourofday) == 24)


# Characterization.

# Get the number of bookings.
total_bookings <- dim(df)
total_bookings

# Get the number of zones in the dataset.
nr_zones <- length(unique(df$init_zone))
nr_zones

# Period covered by the data.
as.period(max(df$end_date) - min(df$end_date)) %>% day()

# Number of days that actually appear in the dataset (the crawler worked fine).
length(unique(df$day))

# Plot histogram of bookings per day of the week.
df %>%
  ggplot(aes(x = weekday)) +
  geom_bar() +
  labs(title = "Bookings per Day", x = "Day", y = "Number of Bookings") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  filename = paste(charts_dir, 'BookingsPerDay.pdf', sep = ""), 
  plot = last_plot()
)
embedFonts(paste(charts_dir, 'BookingsPerDay.pdf', sep = ""))

# Average number of bookings per day of the week.
df %>%
  group_by(day, weekday) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = weekday, y = n)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(),
    axis.line.y = element_line()
  ) +
  labs(title = "Average Number of Bookings per Day of the Week", x = "Day", y = "Number of Bookings") +
  theme(plot.title = element_text(hjust = 0.5))


ggsave(
  filename = paste(charts_dir, 'AverageBookingsPerDay.pdf', sep = ""),
  plot = last_plot()
)
embedFonts(paste(charts_dir, 'AverageBookingsPerDay.pdf', sep = ""))


# Plot histogram of bookings per hour of the day.
df %>%
  ggplot(aes(x = hourofday)) +
  geom_bar() +
  labs(title = "Bookings per Hour", x = "Hour", y = "Number of Bookings") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  filename = paste(charts_dir, 'BookingsPerHour.pdf', sep = ""),
  plot = last_plot()
)
embedFonts(paste(charts_dir, 'BookingsPerHour.pdf', sep = ""))

# Average number of bookings per period
df %>%
  mutate(
    weekday = as.factor(weekday),
    hourofday = as.factor(hourofday),
    isweekend = as.factor(isweekend),
    weather_main_id = as.factor(weather_main_id),
    periodofday = as.factor(get_time_interval(as.numeric(hourofday)))
  ) %>%
  group_by(day, periodofday) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = periodofday, y = n)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(),
    axis.line.y = element_line()
  ) +
  labs(title = "Average Number of Bookings per Period", x = "Period", y = "Number of Bookings") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  filename = paste(charts_dir, 'AverageBookingsPerPeriod.pdf', sep = ""),
  plot = last_plot()
)
embedFonts(paste(charts_dir, 'AverageBookingsPerPeriod.pdf', sep = ""))


# Average number of bookings per hour.
df %>%
  group_by(day, hourofday) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = hourofday, y = n)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.line.x = element_line(),
    axis.line.y = element_line()
  ) +
  labs(title = "Average Number of Bookings per Hour", x = "Hour", y = "Number of Bookings") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  filename = paste(charts_dir, 'AverageBookingsPerHour.pdf', sep = ""),
  plot = last_plot()
)
embedFonts(paste(charts_dir, 'AverageBookingsPerHour.pdf', sep = ""))

# Bookings per month.
df %>%
  mutate(month = month(init_date, label = TRUE)) %>%
  ggplot(aes(x = as.factor(month))) +
  geom_bar() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Number of Bookings per Month", x = "Month", y = "Number of Bookings") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  filename = paste(charts_dir, 'BookingsPerMonth.pdf', sep = ""),
  plot = last_plot()
)
embedFonts(paste(charts_dir, 'BookingsPerMonth.pdf', sep = ""))

# Time series of bookings per date.
df %>%
  group_by(day) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = day, y = n)) +
  geom_line() +
  labs(title = "Bookings Through Time", x = "Time", y = "Number of Bookings") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(
  filename = paste(charts_dir, 'BookingsTime.pdf', sep = ""),
  plot = last_plot()
)
embedFonts(paste(charts_dir, 'BookingsTime.pdf', sep = ""))

# Write the full dataset to a file.
system.time({
  df %>%
    write_csv(paste("data/", city, "Full.csv", sep = ""))
})

df_emergencies <- read_csv("data/emergencies_2017.csv") %>%
  mutate(date = ymd(paste(Year, Month, Day, sep = " "))) %>%
  mutate(weekday = as.factor(wday(date, label = TRUE)), hourofday = as.factor(Hour)) %>%
  group_by(weekday, hourofday) %>%
  summarise(avg_calls_hour = n() / 4)

df <-
  df %>% right_join(df_emergencies, by = c("weekday", "hourofday"))

# Write the full dataset with emergency calls information to a file.
system.time({
  df %>%
    write_csv(paste("data/", city, "FullEmergencyCalls.csv", sep = ""))
})
