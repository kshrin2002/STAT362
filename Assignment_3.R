library(tidyverse)
library(nycflights13)
library(gcookbook)
library(MASS)
library(dplyr)
library(ggpubr)
# Q1a

# Using filter to find the flights departed in Feb
flights_dep_feb <- filter(flights, month == 2)

# Create a histogram
ggplot(flights_dep_feb,aes(x=dep_delay)) + 
  geom_histogram(binwidth = 10,fill = "lightblue",color = "red") +
  scale_x_continuous (name = "Departure Delay",
          breaks = seq(-100,100,10),
          limits = c(-100,100)) +
  labs(title = "Histogram of departure delay in February",
       x = "Departure Delay",
       y = "Frequency")

# Q1b

ggplot(data = flights_dep_feb,aes(x=dep_delay)) + 
  geom_histogram(binwidth = 5,fill = "lightblue",color = "red") + 
  scale_x_continuous (name = "Departure Delay",
          breaks = seq(-100,100,10),
          limits = c(-100,100)) + 
  labs(title = "Histogram of departure delay < 100 mins in February",
       x = "Departure Delay",
       y = "Frequency")

# Q2a

# Filter the flights
jan_first_depart <- filter(flights,month == 1 & day == 1 & dep_delay < 180)

# Create a scatterplot using geom_point()
ggplot(data = jan_first_depart,aes(x = dep_delay, y = arr_delay)) + 
  geom_point() +
  labs(title = "Arr delay vs dep delay on January 1st",
       x = "Dep delay", y = "Arr delay")

# 2b
jan_first_depart_10 <- filter(flights, month == 1 & day == 1 & dep_delay < 10)
# Create a scatterplot using geom_point()
ggplot(data = jan_first_depart_10,aes(x = dep_delay, y = arr_delay)) + 
  geom_point() +
  labs(title = "Arr delay vs dep delay on January 1st < 10 mins",
       x = "Dep delay", y = "Arr delay")

# 2c

cor(jan_first_depart$dep_delay, jan_first_depart$arr_delay, use ="complete.obs")
cor(jan_first_depart_10$dep_delay,jan_first_depart_10$arr_delay,use = "complete.obs")

# 3
plot_delay <- function(which_month,which_day,lower_range,upper_range){
  # Filter flights 
  filtered_flights <- filter (flights,month == which_month & day == which_day & dep_delay >= lower_range & dep_delay <= upper_range)
  
  # Create scatterplot
  ggplot(data = filtered_flights,aes(x = dep_delay,y = arr_delay)) +
    geom_point() + 
    labs("Arr delay vs dep delay on a specific month and day",
         x = "dep_delay", y = "arr_delay")
}

plot_delay(which_month = 1, which_day = 30, lower_range = -5, upper_range = 30)


# 4a

# bar chart
ggplot(flights,aes(x=factor(month))) + 
  geom_bar() + 
  labs(title = "Flights by month",
       x = "Month",y = "Flights")

# 4b

# Filter flights for January
jan_flights_filtered <- filter(flights, month == 1)

# bar chart
ggplot(jan_flights_filtered, aes(x = day)) +
  geom_bar() +
  labs(title = "Flights by Day in January",
       x = "Day", y = "Flights")

# 4c
jan_flights_avg_delay <- jan_flights_filtered %>%
  group_by(day) %>%
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE))

ggplot(data = jan_flights_avg_delay) +
  geom_col(mapping = aes(x = day, y = avg_arr_delay), fill = "grey") +
  labs(x = "Day", y = "Average Arrival Delay (minutes)", title = "Average Arrival Delay per Day in January") +
  theme_minimal()

# 4d

# Did not have time to complete it

# 5

# Average departure delay 
average_depart_delay <- flights %>%
  group_by(day_of_year = yday(time_hour)) %>%
  
  # Summarize large set of values in one column
  summarize(average_depart_delay = mean(dep_delay,na.rm = TRUE))
ggplot(average_depart_delay,aes(x=day_of_year,y=average_depart_delay)) + 
  geom_line() +
  labs(title = "Average departure delay over year",
       x = "Day",
       y = "Average delay")

# 6

# Load the dataset
data(birthwt,package="MASS")

# Subset the data when mother's age is >= 25
birthwt_dataset <- subset(birthwt, age >= 25)

# Create plot
ggplot(birthwt_dataset,aes(x=bwt)) +
  geom_density(color = "black") +
  labs(title="Kernel density estimate of birth weight for mothers age 25+",
       x = "Birth weight",
       y = "Density")

# 7a
ggplot(data = mpg)

# I see nothing


# 7b
ggplot(mpg,aes(x=drv,y=class)) +
  geom_point() + 
  labs(title = "Scatter plot of Class vs Drv",
       x = "Drv",
       y = "Car class")

# 7c

# The provided code does not work because the colour has to be outside of the aes function, since
# we assign constant value "blue" to the "color" aesthetic.

# 7d
# Corrected code:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# 8

# Data grouped by origin, calculate average arrival and departure delay

origin_delay <- flights %>%
  group_by(origin) %>%
  summarize(avg_arr_delay = mean(arr_delay, na.rm = TRUE),
            avg_dep_delay = mean(dep_delay, na.rm = TRUE))
data_plot <- tibble(origin = unlist(c(origin_delay[, 1], origin_delay[, 1])),
                    delay = unlist(c(origin_delay[, 2], origin_delay[, 3])),
                    Type = rep(c("Arr", "Dept"), each = 3))
ggplot(data_plot, aes(x = origin, y = delay, fill = Type)) +
  geom_col(position = "dodge") +
  labs(title = "Avg Arr Delay and Dep Delay by Origins",
       y = "Avg Delay")

# 9a

# Load the diamonds dataset
data(diamonds)

# Compute the average price of diamonds 
average_price_by_cut <- diamonds %>%
  group_by(cut) %>%
  summarize(avg_price = mean(price,na.rm=TRUE))

print(average_price_by_cut)

# I think it is unreasonable because avg price would go up according to the quality of the diamond, but 
# we are seeing that a diamond with a fair cut is more expensive than one with ideal cut is being more expensive than an ideal diamond
# 9b
diamonds %>%
  ggplot(mapping = aes(x=price)) + 
  geom_histogram() +
  facet_grid(cut ~ ., scales = "free_y")
# I think a fair diamond should not be more expensive than an ideal diamond if we were to make all the contributing factors same, since there could be any outlying conditions that could change this scenario

# 9c
diamonds %>%
  filter(carat<=1,carat >= 0.9) %>%
  ggplot(mapping=aes(x=price)) +
  geom_histogram() + 
  facet_grid(cut ~ .,scales = "free_y")

# 9d
diamonds %>%
  filter(cut == "Ideal",clarity == "VS2") %>%
  ggplot(mapping = aes(x=carat,y=price,color=color)) + 
  geom_point(size = 0.9)
# Two features: diamonds that have special feature such as specific/unique colors are more expensive and as carat increases, price of dimaonds increases

# 10a
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = TRUE)
# se displays confidence interval

# 10b
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(color = "blue")

# 10c
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(aes(x = displ, y = hwy, group = drv), se = FALSE)

# 10d
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(data = filter(mpg, drv == "r"),
              aes(x = displ, y = hwy), se = FALSE)

# 10e
ggplot(mpg, aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

# 10f
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(se = FALSE)

# 11

x <- seq(-4, 4, len = 1000)
gg_boxcar <- ggplot(mapping = aes(x = x, y = 1 / 2 * (abs(x) <= 1))) +
  geom_line() +
  labs(y = "", x = "", title = "Boxcar")

gg_Gaussian <- ggplot(mapping = aes(x = x, y = dnorm(x))) +
  geom_line() +
  labs(y = "", x = "", title = "Gaussian")
gg_Epanechnikov <- ggplot(mapping = aes(x = x, y = 3 / 4 * (1 - x^2) * (abs(x) <= 1))) +
  geom_line() +
  labs(y = "", x = "", title = "Epanechnikov")
gg_tricube <- ggplot(mapping = aes(x = x,
                                   y = 70 / 81 * (1 - abs(x)^3)^3 * (abs(x) <= 1))) +
  geom_line() +
  labs(y = "", x = "", title = "Tricube")
ggarrange(gg_boxcar, gg_Gaussian, gg_Epanechnikov, gg_tricube)

# 12a
flights_delay <- flights %>%
  group_by(month, origin) %>%
  summarize(delay = mean(dep_delay, na.rm = TRUE)) %>%
  ungroup()

ggplot(flights_delay, aes(x = month, y = delay, color = origin)) +
  geom_line() +
  labs(title = "Average Departure Delay by Month and Origin",
       x = "Month",
       y = "Average Departure Delay (minutes)",
       color = "origin") +
  theme_minimal() +
  guides(color = FALSE)

# 12b
ggplot(flights_delay, aes(x = month, y = delay, color = origin)) +
  geom_line() +
  labs(title = "Average Departure Delay by Month and Origin",
       x = "Month",
       y = "Average Departure Delay (minutes)",
       color = "origin") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 12c
ggplot(flights_delay, aes(x = month, y = delay, color = origin)) +
  geom_line() +
  labs(title = "Average Departure Delay by Month and Origin",
       x = "Month",
       y = "Average Departure Delay (minutes)",
       color = "origin") +
  theme_minimal() +
  theme(legend.position = "right", legend.justification = "center", legend.box.just = "center")

# 12d
ggplot(flights_delay, aes(x = month, y = delay, color = origin)) +
  geom_line() +
  labs(title = "Average Departure Delay by Month and Origin",
       x = "Month",
       y = "Average Departure Delay (minutes)",
       color = "ORIGIN") +
  theme_minimal() +
  theme(legend.position = "right", legend.justification = "center", legend.box.just = "center")


# 13

# Assuming you have a data frame named 'df'
df <- read.csv("C:/Users/jayan/Downloads/housing_data.csv")
n <- nrow(df)
df$Date_month <- seq(1:n)
long_df <- pivot_longer(df, colnames(df)[-c(1, 7)])
break_pt <- seq(1, n, len = 10)
trend_text <- "HPI increases with time" |>
  str_wrap(width = 30)

# Corrected ggplot code
long_df %>% 
  ggplot(mapping = aes(x = Date_month, y = value, color = name)) +
  geom_line() + 
  scale_x_continuous(breaks = break_pt, labels = df$Date[break_pt]) +
  
  # Place annotate functions inside ggplot()
  annotate(
    geom = "label",
    x = 20, y = 7e+05, # Adjust these coordinates based on your data
    label = trend_text,
    hjust = "left", color = "red"
  ) +
  annotate(
    geom = "segment",
    x = 20, y = 5e+05, xend = 85, yend = 8e+05, # Adjust these coordinates based on your data
    color = "red",
    arrow = arrow(type = "closed"))

