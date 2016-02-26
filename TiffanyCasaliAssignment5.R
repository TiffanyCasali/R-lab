# Tiffany Casali
# Econ 294A
# February 26, 2016
# Assignment 5

library(ggplot2)

# Problem 1
# Part A
p1a <- ggplot(diamonds,
            aes(x=log(x*y*z), y=log(price)))

p1a + geom_point(aes(size = carat, alpha = 0.01, colour = clarity))

# Part B
p1b <- ggplot(diamonds,
              aes(x= carat, y=..density..)) + facet_wrap(~ cut)

p1b + geom_histogram(aes(fill=clarity))  + facet_grid(cut ~ .)

# Part C
p1c <- ggplot(diamonds,
              aes(x= cut, y=price))

p1c + geom_violin() + geom_jitter(alpha = 0.01)


# Question 2
library(foreign)
org_example <- read.dta("C:/Users/Tiff/Desktop/Econ 217/org_example.dta")

# Part A
library(dplyr)
library(magrittr)

org_example2 <- org_example %>%
  group_by(year, month) %>%
  summarize(
    rw.median = median(rw, na.rm = T),
    rw.q1 = quantile(rw, 0.25, na.rm = T),
    rw.q3 = quantile(rw, 0.75, na.rm = T),
    rw.d1 = quantile(rw, 0.1, na.rm = T),
    rw.d9 = quantile(rw, 0.9, na.rm = T)
    )

org_example2$date <- as.Date(paste(org_example2$month, org_example2$year, "01", sep= "."), format = "%m.%Y.%d")

p2a <- ggplot(org_example2,
              aes(x= date, y= rw.median))

p2a + geom_line() + 
  geom_ribbon(aes(ymin = rw.d1, ymax = rw.d9), alpha = 0.2) +
  geom_ribbon(aes(ymin = rw.q1, ymax = rw.q3), alpha = 0.5)

# Part B
org_example3 <- org_example %>%
  group_by(year, month, educ) %>%
  summarize(
    rw.median = median(rw, na.rm = T)
  )

org_example3$date <- as.Date(paste(org_example3$month, org_example3$year, "01", sep= "."), format = "%m.%Y.%d")

p2b <- ggplot(org_example3,
              aes(x= date, y= rw.median, group= educ))

p2b + geom_line(aes(color= educ))
