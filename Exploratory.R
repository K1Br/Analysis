
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
# Exploratory analysis of df
str(df)
summary(df)
head(df, 14)

range(df$`Self esteem week 1`)
range(df$`Weight loss week 3`)


#one way anova

summary(aov(formula = df$`Weight Loss Week 1` ~ df$group))

demo(colors)

ggplot(df, aes(x = group, y = Weight Loss Week 1)) +
  geom_boxplot(fill = "chocolate2", colour = "maroon3") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("weight loss")

TukeyHSD(aov(formula = df$`Weight Loss Week 1` ~ df$group))

# Regression analysis
fit <-lm(df$`Self esteem week 3` ~ df$`Weight Loss Week 1`, df$`Weight loss week 2`, df$`Weight loss week 3`, data=df)
summary(fit)

library(graphics)
barchart(Freq~`Weight Loss Week 1`|groups, data=df)

