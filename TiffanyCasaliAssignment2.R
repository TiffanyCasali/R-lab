# Econ 294A Assignment 2
# January 22, 2016


# Part 0
TiffanyCasaliAssignment2 <- list(
  firstName = "Tiffany",
  lastName = "Casali",
  email = "tcasali@ucsc.edu",
  studentID = 1271030
)


# Part 1
diamonds <- get(  
  load(
    file = url("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/diamonds.RData")
  )
)

TiffanyCasaliAssignment2$s1a <- nrow(diamonds)
TiffanyCasaliAssignment2$s1b <- ncol(diamonds)
TiffanyCasaliAssignment2$s1c <- names(diamonds)
TiffanyCasaliAssignment2$s1d <- summary(diamonds$price)


# Part 2
library(foreign)
df <- read.table(file = 'https://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt
', header = TRUE)

TiffanyCasaliAssignment2$s2a <- nrow(df)
TiffanyCasaliAssignment2$s2b <- ncol(df)
TiffanyCasaliAssignment2$s2c <- names(df)
TiffanyCasaliAssignment2$s2d <- mean(df$weight)
TiffanyCasaliAssignment2$s2e <- median(df$weight)

TiffanyCasaliAssignment2 <- col <- ifelse(test = df$weight >= 996, yes = NA, no = df$weight)
TiffanyCasaliAssignment2 <- hist(col)
TiffanyCasaliAssignment2 <- table(col)

TiffanyCasaliAssignment2$s2f <- mean(col, na.rm = TRUE)
TiffanyCasaliAssignment2$s2g <- median(col, na.rm = TRUE)

sub1 <- subset(df, SEX == 1 & weight < 900)
TiffanyCasaliAssignment2$s2i <- summary(sub1$weight)
sub2 <- subset(df, SEX == 2 & weight < 900)
TiffanyCasaliAssignment2$s2h <- summary(sub2$weight)


# Part 3
vec <- c(letters,LETTERS)
vec3a <- vec[seq(2, length(vec), 2)]
TiffanyCasaliAssignment2$s3a <- vec3a <- vec[seq(2, length(vec), 2)]
TiffanyCasaliAssignment2$s3b <- paste(vec[c(46, 9, 6)], collapse="")

arr <- array( c(letters,LETTERS), dim = c(3,3,3))
TiffanyCasaliAssignment2$s3c <- arr[,1,2]
TiffanyCasaliAssignment2$s3d <- arr[2,2,]
TiffanyCasaliAssignment2$s3e <- paste(arr[2,1,3], arr[3,3,1], arr[3,2,1], sep = '')

