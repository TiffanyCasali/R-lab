
# Assigment 4
# Tiffany Casali
# Feb 19, 2013


# 0
print("Tiffany")
print("Casali")
print(1271030)

# 1
flights <- read.csv(file = "C:/Users/Tiff/Desktop/Econ 294A/flights.csv", header = TRUE, stringsAsFactors = FALSE)
planes <- read.csv(file = "C:/Users/Tiff/Desktop/Econ 294A/planes.csv", header = TRUE, stringsAsFactors = FALSE)
weather <- read.csv(file = "C:/Users/Tiff/Desktop/Econ 294A/weather.csv", header = TRUE, stringsAsFactors = FALSE)
airports <- read.csv(file = "C:/Users/Tiff/Desktop/Econ 294A/airports.csv", header = TRUE, stringsAsFactors = FALSE)

# 2
flights$date <- as.Date(flights$date)
weather$date <- as.Date(weather$date)

# 3
flights.2a <- subset(flights, dest == "SFO" | dest == "OAK")
nrow(flights.2a)
flights.2b <- subset(flights, dep_delay >= 60)
nrow(flights.2b)
flights.2c <- subset(flights, arr_delay > 2*dep_delay)
nrow(flights.2c)

# 4
install.packages("dplyr")
library(dplyr)

sub1 <- select(flights, ends_with("delay"))
sub2 <- select(flights,contains("delay"))
sub3 <- select(flights, arr_delay, dep_delay)

# 5
flights.5a <- arrange(flights, -dep_delay)
head(flights.5a, n=5)

flights.5b <- flights
flights.5b$dif <- flights$arr_delay-flights$dep_delay
flights.5b <- arrange(flights.5b, -dif)
head(flights.5b, n=5)

# 6
install.packages("magrittr")
library(magrittr)

flights <- flights %>%
  mutate(
    speed = dist/(time/60), 
    delta = dep_delay-arr_delay
  )

flights.6a <- arrange(flights, -speed)
head(flights.6a, n=5)
flights.6b <- arrange(flights, -delta)
head(flights.6b, n=5)
flights.6c <- arrange(flights, delta)
head(flights.6c, n=5)

# 7
flights.7a <- flights %>%
  group_by(carrier) %>%
  summarise(
    cancelled.flights = sum(cancelled, na.rm = T),
    total.flights = n(),
    percent.cancelled = sum(cancelled, na.rm = T)/n(),
    delta.min = min(delta, na.rm = T),
    delta.q1 = quantile(delta, 0.25, na.rm = T),
    delta.mean = mean(delta, na.rm = T),
    delta.median = median(delta, na.rm = T),
    delta.q3 = quantile(delta, 0.75, na.rm = T),
    delta.q90 = quantile(delta, 0.9, na.rm = T),
    delta.max = max(delta, na.rm = T)
  )

flights.7a <- arrange(flights.7a, desc(percent.cancelled))
summary(flights.7a)


day_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%
  group_by(date)%>%
    summarise(
      delay = mean(dep_delay),
      n=n()
    )

# 8
arrange(day_delay, date)
delay.8a <- day_delay %>%
  mutate(delay_today = delay,
         delay_yesterday = lag(delay, 1),
         delay_inc = delay_today - delay_yesterday)

delay.8a <- arrange(delay.8a, -delay_inc)
head(delay.8a, n=5)

# 9
dest_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%
  group_by(dest)%>%
    summarize(
      arr_delay = mean(arr_delay),
      n = n()
    )

airports <- airports %>%
  rename(dest=iata, name=airport)


df.9a <- left_join(dest_delay, airports, by=c("dest"="dest"))
df.9a <- arrange(df.9a, -arr_delay)
head(df.9a, n=5)

df.9b <- inner_join(dest_delay, airports, by=c("dest"="dest"))
nrow(df.9a)
nrow(df.9b)
print("The number of observations are close, but do not match exactly.")

df.9c <- right_join(dest_delay, airports, by=c("dest"="dest"))
nrow(df.9c)
print("There are NAs in in the arr_delay column, because there are more observations in airports than in dest_delay")

df.9d <- full_join(dest_delay, airports, by=c("dest"="dest"))
nrow(df.9d)
print("Again there are NAs in arr_delay, because airports has a different number of observations")

# 10
hourly_delay <- dplyr::filter(flights, !is.na(dep_delay))%>%
  group_by(date, hour)%>%
  summarise(
    delay = mean(dep_delay)
  )

hourly_delay$date <- as.Date(hourly_delay$date)
df.10a <- left_join(hourly_delay, weather, by=c("date"="date"))
