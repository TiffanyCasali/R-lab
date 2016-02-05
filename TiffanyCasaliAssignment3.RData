# 294A Assignment 3
# Tiffany Casali


# 0
print("Tiffany Casali")
print(1271030)
print("tcasali@ucsc.edu")


# 1
install.packages("foreign")
library(foreign)
df.ex <- read.dta(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)


# 2
install.packages("dplyr")
library(dplyr)
install.packages("magrittr")
library(magrittr)

require(dplyr)
df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & month == 12
  )
print(nrow(df.ex.2))

df.ex.2b <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month == 8 | month == 9)
  )
print(nrow(df.ex.2b))


# 3
df.ex.3a <- df.ex %>%
  arrange(year, month)


# 4
df.ex.4a <- df.ex %>%
  select(year:age)

df.ex.4b <- df.ex %>%
  select(year, month, starts_with("i"))

distinct(select(df.ex, state))


# 5
stndz <- function(x){(x - mean(x, na.rm = T))  /  sd(x, na.rm = T)}
nrmlz <- function(x){(x - min(x, na.rm = T))  /  (max(x, na.rm = T) - min(x, na.rm = T))}

df.ex.5a <- df.ex %>% 
  mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw)
  )

df.ex.5b <- df.ex %>%
  group_by(year, month) %>%
  mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw),
    count    = n()
  )


# 6
df.ex.6 <- df.ex %>%
  group_by(year, month, state) %>%
  summarise(
    rw_min = min(rw, na.rm = T),
    rw_q1 = quantile(rw, 0.25, na.rm = T),
    rw_mean = mean(rw, na.rm = T),
    rw_median = median(rw, na.rm = T),
    rw_q3 = quantile(rw, 0.75, na.rm = T),
    rw_max = max(rw, na.rm = T),
    count = n()
  )

val <- (max(df.ex.6$rw_mean))
value <- dplyr::filter(df.ex.6, rw_mean == val)
new_value <- select(value, year, month, state)
print(new_value)


# 7
df.ex.7 <- df.ex %>%
  arrange(year, month, desc(as.character(df$state)))
