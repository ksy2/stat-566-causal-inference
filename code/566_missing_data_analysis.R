### CSSS/Stat 566 Project: Missing Data Analysis
## Natalie Chisam, Rachel Flodin, Kaelan Yu
# 5/28/21
# This R script compares the full data set and the cleaned data set (removing NA values)
# using summary statistics, histograms, and other quantitative methods of analysis.

# setwd("C:/Users/Kaelan/Desktop/stat-566-causal-inference/data")

rm(list = ls())

dat <- read.csv("dat.csv", header = TRUE)
dat <- dat[, -1]    # exclude first column since just observation numbers
dat_full <- dat     # full data set
dat <- na.omit(dat) # cleaned data set

# we only want to look at a reduced version of our 2 data sets containing the variables
# selected from BIC model selection

BIC_variables <- c("h1n1_vaccine", "h1n1_knowledge", "behavioral_large_gatherings",
                   "doctor_recc_h1n1", "doctor_recc_seasonal", "child_under_6_months", 
                   "health_worker", "health_insurance", "opinion_h1n1_vacc_effective", 
                   "opinion_h1n1_risk", "opinion_h1n1_sick_from_vacc", "sex", 
                   "marital_status", "seasonal_vaccine")

# reduced (BIC) data sets
dat_BIC_full <- dat_full[, BIC_variables]
dat_BIC <- dat[, BIC_variables]

summary(dat_BIC_full)
summary(dat_BIC)

# histograms of data sets
p <- dim(dat_BIC)[2]

pdf("566_barplots_BIC_datasets.pdf")
par(mfrow = c(2, 1))
for (i in 1:p) {
  lvl <- levels(as.factor(dat_BIC[, i]))
  k <- length(lvl)
  # full
  plot(as.factor(dat_BIC_full[,i]), main=paste(names(dat_BIC_full)[i],"(Full)"), names.arg=lvl, cex.names=1, 
       col = 1:k)  
  # cleaned
  plot(as.factor(dat_BIC[,i]), main=paste(names(dat_BIC)[i],"(Cleaned)"), names.arg=lvl, cex.names=1, 
       col = 1:k)

}
dev.off()