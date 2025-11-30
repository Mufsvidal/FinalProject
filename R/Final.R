library(ggplot2)
library(tidyverse)

if (!file.exists("historical-weather-medellin.csv")) {
  stop("CSV file not found! Please check the path.")
}

medellin <- read.csv("historical-weather-medellin.csv")

expected_cols <- c("Date..yyyy.mm.dd.", "Max_temperature...C.", "Min_temperature...C.",
                   "Rain..mm.", "Wind..km.h.", "Description")

missing_cols <- setdiff(expected_cols, names(medellin))
if (length(missing_cols) > 0) {
  stop("Missing columns in CSV: ", paste(missing_cols, collapse = ", "))
}

medellin <- medellin %>% rename(
  Date = Date..yyyy.mm.dd.,
  MaxTemp = Max_temperature...C.,
  MinTemp = Min_temperature...C.,
  Rain = Rain..mm.,
  Wind = Wind..km.h.,
  Weather = Description
)

medellin$Date <- as.Date(medellin$Date)
if (any(is.na(medellin$Date))) {
  warning("Some dates could not be converted to Date class.")
}


medellin_monthly <- function(df, var) {
  df %>%
    mutate(Monthly = format(Date, "%Y-%m")) %>%
    group_by(Monthly) %>%
    summarise(Value = mean(.data[[var]], na.rm = TRUE), .groups = "drop") %>%
    mutate(Monthly = as.Date(paste0(Monthly, "-01")))
}
# average max, min and average temp
medellin_monthly_max <- medellin_monthly(medellin, "MaxTemp")
medellin_monthly_min <- medellin_monthly(medellin, "MinTemp")
medellin_monthly_average <- medellin %>%
  mutate(AvgTemp = (MaxTemp + MinTemp)/2) %>%
  medellin_monthly("AvgTemp")

# Plot for average max temp
ggplot(medellin_monthly_max, aes(x = Monthly, y = Value, color = Value)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Max Temperature in Medellin", x = "Date", y = "Max Temperature") +
  scale_color_gradient(low = "blue", high = "red")

# Plot for average min temp
ggplot(medellin_monthly_min, aes(x = Monthly, y = Value, color = Value)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Min Temperature in Medellin",
    x = "Date",
    y = "Min Temperature (°C)"
  ) +
  scale_color_gradient(low = "blue", high = "purple")

# Plot for average temp
ggplot(medellin_monthly_average, aes(x = Monthly, y = Value, color = Value)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Average Temperature in Medellin",
    x = "Date",
    y = "Average Temperature (°C)"
  ) +
  scale_color_gradient(low = "blue", high = "yellow")


