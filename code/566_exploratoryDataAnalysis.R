### CSSS/Stat 566 Project: Exploratory Data Analysis
## Natalie Chisam, Rachel Flodin, Kaelan Yu
# 5/8/21
# This R script performs exploratory data analysis on
# a cleaned H1N1 data set from Kaggle, called "dat.csv"

rm(list = ls())

## 0. Preliminaries
library(ggplot2)
library(ggpubr)
library(grid)
library(plyr)
dat <- read.csv("dat.csv", header = TRUE)
dat <- dat[, -1] # exclude first column since just observation numbers

# just make sure data set looks good
head(dat, n = 3)
colnames(dat)
rownames(dat)
typeof(dat)

# check dimensions: n = 26707, k = 34
dim(dat)

## 1. Data Cleaning 

# remove missing values
dat <- na.omit(dat)

# sanity check for zero missing values
sum(is.na(dat)) # 0

# check dimensions again: n = 11794, k = 35
dim(dat)
n <- dim(dat)[1]
k <- dim(dat)[2]

# we removed 14913 rows! (over half of our data set)

## 2. Exploratory Data Analysis

# we have all categorical variables in the data set
# let's check the number of levels of each categorical variable
# we'll create a integer vector of length k to store the number of levels in each variable
num_levels <- integer(k)
for (i in 1:k) {
  num_levels[i] <- length(unique(dat[, i]))
}


# it looks like all the variables have between 2 and 5 levels
# we can use donut plots to visualize these categorical variables
make_donut <- function(i) {
  # data frame/summary stats (a, b)
  a <- count(as.factor(dat[, i]))
  b <- data.frame(
    category <- a[, 1],
    count <- a[, 2]
  )
  b$fraction = b$count / sum(b$count)
  b$ymax = cumsum(b$fraction)
  b$ymin <- c(0, head(b$ymax, n=-1))
  b$labelPosition <- (b$ymax + b$ymin)/2
  b$label <- paste0(b$category, "\n %: ", round(b$fraction * 100, 2))
  # donut plot (c) 
  c <- ggplot(b, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
    geom_rect() +
    geom_label(x=3.5, aes(y=labelPosition, label=label), size=3) +
    scale_fill_brewer(palette="Pastel1") +
    coord_polar(theta="y") +
    xlim(c(0, 4)) +
    theme_void()
  return(c)
}

# to store vector of plots
donut_vec <- vector(mode = "list", length = k)

for (i in 1:k) {
  donut_vec[[i]] <- make_donut(i)
}

# convert string of comma separated values into a list
# to pass into grid.arrange()
donutString <- ""
for (i in 1:(k-1)) {
  donutString <- paste0(donutString, "donut_vec[[", i, "]], ")
}
donutString <- paste0(donutString, "donut_vec[[", k, "]]")
donutList <- as.list(strsplit(donutString, ", "))

# generate multiple donut plots on the same page

# grid.arrange(donutList)