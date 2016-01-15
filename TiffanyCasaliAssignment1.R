# 294A Assignment 1
# Tiffany Casali


# Number zero
print('0.')
print('Tiffany Casali 1271030')


# Number one
print('1.')

library(foreign)
df.dta <- read.dta(file = 'https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta')
df.csv <- read.csv(file = 'https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv')
df.td <- read.table(file = 'https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt')
load(url('https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData'))
print('The name assigned to the R file is NHIS_2007_RData')

# Number two
format(object.size(df.dta), units = 'KB') # "452.3 Kb"
format(object.size(df.csv), units = 'KB') # "188.5 Kb"
format(object.size(df.td), units = 'KB')  # "506.4 Kb"
format(object.size(NHIS_2007_RData), units = 'KB')  # "188.5 Kb"

print('2.')
print('The .dta file is 452.3 Kb')
print('The .csv file is 188.5 Kb. This is tied for the smallest file.')
print('The .td file is 506.4 Kb. This is the largest file.')
print('The R file is 188.5 Kb also, and is tied with the .csv file for the smallest')
print('The variability in size can be accounted for due to the fact that different files are formattted differently. Some have additional spaces seperating the data, which takes up more room.')


# Number three
print('3.')

typeof(NHIS_2007_RData)
class(NHIS_2007_RData)
print('NHIS_2007_RData has a typeof of "list" and a class of "data.frame".')

length(NHIS_2007_RData)
dim(NHIS_2007_RData)
nrow(NHIS_2007_RData)
ncol(NHIS_2007_RData)
summary(NHIS_2007_RData)
print('The length of this dataset is 9 and the dimensions are 4785 by 9.')
print('It has 4785 rows and 9 colomns. The following is the summary.')
print(summary(NHIS_2007_RData))


# Number four
print('4.')
df <- read.dta(file = 'https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta')
print(str(df))
print('There are 1119754 observations and 30 variables.')

min(df$rw, na.rm = TRUE)
mean(df$rw, na.rm = TRUE)
median(df$rw, na.rm = TRUE)
max(df$rw, na.rm = TRUE)
quantile(df$rw, na.rm = TRUE)
print('The min is 1.814375, the mean is 19.81418, the median is 15.87578, and the max is 354.8014')
print('The 1st quantile is 10.704728 and the 3rd is 24.358696.')

sum(is.na(df$rw))
print('There are 521279 missing observations.')


# Number five
print('5.')

v <- c(1, 2, 3, 4, 5, 6, 7, 4, NULL, NA)
length(v)
print('The vector has a length of 9.')
print('The reported number of values does not match because one of our values is "NULL", which represents a function whose value is undefined')

mean(v, na = TRUE)
print('The mean is 4.')


# Number six
print('6.')

x <- matrix(c(1, 4, 7, 2, 5, 8, 3, 6, 9), nrow=3, ncol=3)
t(x)
eigen(x)

y <- matrix(c(1, 3, 2, 2, 2, 3, 3, 1, 0), nrow=3, ncol=3)
solve(y)
y %*% solve(y)
print('This new matrix is called the Identity Matrix.')


# Number seven
print('7.')

carat = c(5, 2, 0.5, 1.5, 5, NA, 3)
cut = c("fair", "good", "very good", "good", "fair", "Ideal", "fair")
clarity = c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", NA)
price = c(850, 450, 450, "NULL", 750, 980, 420)
diamonds = data.frame(carat, cut, clarity, price)

newdim <- as.numeric(as.character(diamonds$price))
mean(newdim, na.rm = TRUE)
print('The mean price is 650.')

s <- subset(diamonds, cut == "fair")
news <- as.numeric(as.character(s$price))
mean(news)
print('The mean price of cut "fair" is 673.3333')

n <- subset(diamonds, cut != "fair")
newn <- as.numeric(as.character(n$price))
mean(newn, na.rm = TRUE)
print('The mean price of "good", "very good", and "Ideal" is 626.6667')

m <- subset(diamonds, carat > 2 & cut == "very good" | cut == "Ideal")
print('There is only one observation that could meet these standards, with a price of 980. However, we do not actually know the number of carats, so we cannot say for sure what the median price is.')