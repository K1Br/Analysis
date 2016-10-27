library("ggplot2")
library("dplyr")
library("plyr")
library("data.table")
setwd("~/R/HackTober")
df <- read.csv('WeightLoss.csv')


# Delete first column
df
df$x <- NULL


# Rename week columns
dfrename <- c("X" , "group","Weight Loss Week 1", "Weight loss week 2", "Weight loss week 3", "Self esteem week 1", "Self esteem week 2", "Self esteem week 3")
setnames(df, dfrename)
head(df,2)

# Create a function to rename "DietEx" values in group column
levels(df$group) <- c(levels(df$group), "Diet & Exercise")
df$group[df$group == "DietEx"] <- "Diet & Exercise"
df


