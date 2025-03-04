# Question 1(a)

# Reading the dataset

nyt1 <- read.csv("~/Desktop/Datasets/all_nyt/nyt1.csv")
View(nyt1)
library(ggplot2)
a1 <- nyt1
b1 <- a1$Age
e1 <- a1$Clicks

# Calculating CTR for day1

CTR1 <- ifelse(a1$Impressions != 0, a1$Clicks/a1$Impressions, NA)
c1 <- cbind(a1, CTR1)
View(c1[1:50, ])

# Making age groups

age_groups <- cut(a1$Age, breaks = c(0, 18, 24, 34, 44, 54, 64, 120), labels = c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+"))
d1 <- cbind(c1, age_groups)

#Question 1(b)

# Making categories based on clicks

clicks_category<- cut(a1$Clicks, breaks = c(-0.1, 0, 1, 2, 3, 4, 5), labels = c("No_Clicks","Least_Clicks", "Low_Clicks", "Medium_Clicks", "High_Clicks", "Highest_Clicks"), na.rm = T)
g1 <- cbind(d1,clicks_category)
View(g1)

# Summary

summary(g1$Impressions, na.rm =T)

# The Impressions vary from 0-20, with median impressions = 5 and mean impressions = 5.007.

summary(g1$CTR1, na.rm = T)

# Mean CTR is 0.0185.

summary(g1$Age, na.rm = T)

# The age of all the users vary from 0-108 with median age = 31 and mean age = 29.48.

summary(g1$Gender == "0", na.rm = T)
summary(g1$Gender == "1", na.rm = T)

# Male members are greater than female members.

summary(g1$Clicks, na.rm =T)
summary(g1$clicks_category, na.rm =T)
sum(g1$Gender == "0")
sum(g1$Gender == "1")

# Male members are greater than female members.

sum(g1$Signed_In == "0")
sum(g1$Signed_In == "1")

# The members who have an account are more frequent users than the non-members.

# Plotting Impressions VS CTR for different age_groups of day1

plot(c1$Impressions[age_groups == "<18"], c1$CTR1[age_groups == "<18"], main = "Impressions vs CTR 1", xlab = "Impressions", ylab = "CTR", xlim = c(0, 21), ylim = c(0, 1), col = "red", las = 1)
plot(c1$Impressions[age_groups == "18-24"], c1$CTR1[age_groups == "18-24"], main = "Impressions vs CTR 2", xlab = "Impressions", ylab = "CTR", xlim = c(0, 21), ylim = c(0, 1), col = "blue", las = 1)
plot(c1$Impressions[age_groups == "25-34"], c1$CTR1[age_groups == "25-34"], main = "Impressions vs CTR 3",xlab = "Impressions", ylab = "CTR", xlim = c(0, 21), ylim = c(0, 1), col = "green", las = 1)
plot(c1$Impressions[age_groups == "35-44"], c1$CTR1[age_groups == "35-44"], main = "Impressions vs CTR 4",xlab = "Impressions", ylab = "CTR", xlim = c(0, 21), ylim = c(0, 1), col = "pink", las = 1)
plot(c1$Impressions[age_groups == "45-54"], c1$CTR1[age_groups == "45-54"], main = "Impressions vs CTR 5",  xlab = "Impressions", ylab = "CTR", xlim = c(0, 21), ylim = c(0, 1), col = "orange", las = 1)
plot(c1$Impressions[age_groups == "55-64"], c1$CTR1[age_groups == "55-64"], main = "Impressions vs CTR 6", xlab = "Impressions", ylab = "CTR", xlim = c(0, 21), ylim = c(0, 1), col = "yellow", las = 1)
plot(c1$Impressions[age_groups == "65+"], c1$CTR1[age_groups == "65+"], main = "Impressions vs CTR 7",vxlab = "Impressions", ylab = "CTR", xlim = c(0, 21), ylim = c(0, 1), las = 1)

# The plot for Impressions vs CTR for all age groups show that number of impressions and CTR have an inverse relationship for day1.

# Comparisions for Impressions, Clicks, Age and CTR between Male and Female

par(mfrow = c(1, 2))
boxplot(c1$Impressions[c1$Gender == "0"], main = "Female Impressions")
boxplot(c1$Impressions[c1$Gender == "1"], main = "Male Impressions")

# The boxplot of Impressions show that Females have higher median impressions than males.

boxplot(g1$Clicks[g1$Gender == "0"], main = "Female Clicks")
boxplot(g1$Clicks[g1$Gender == "1"], main = "Male Clicks")

# Females have higher clicks than males for day1

boxplot(g1$Age[g1$Gender == "0"], main = "Female Age")
boxplot(g1$Age[g1$Gender == "1"], main = "Male Age")

# The median age of males who visited the site is higher than median age of females who visit the site.

boxplot(g1$CTR1[g1$Gender == "0"], main = "CTR for Female")
boxplot(g1$CTR1[g1$Gender == "1"], main = "CTR for Male")

# The mean CTR for females is higher (almost double) than mean CTR for males.

par(mfrow = c(1, 1))

# Making the total count of Clicks, age_groups between members/non_members etc 

sum_clicks_1 <- sum(g1$clicks_category == "No_Clicks")
sum_clicks_2 <- sum(g1$clicks_category == "Least_Clicks")
sum_clicks_3 <- sum(g1$clicks_category == "Low_Clicks")
sum_clicks_4 <- sum(g1$clicks_category == "Medium_Clicks")
sum_clicks_5 <- sum(g1$clicks_category == "High_Clicks")
sum_clicks_6 <- sum(g1$clicks_category == "Highest_Clicks")

# The number of people in no_clicks category is highest followed by Least_Clicks, Low_Clicks, Medium_Clicks and Highest_Clicks.

sum_age_groups_1 <- sum(g1$age_groups == "<18", na.rm = T)
sum_age_groups_2 <- sum(g1$age_groups == "18-24", na.rm = T)
sum_age_groups_3 <- sum(g1$age_groups == "25-34", na.rm = T)
sum_age_groups_4 <- sum(g1$age_groups == "35-44", na.rm = T)
sum_age_groups_5 <- sum(g1$age_groups == "45-54", na.rm = T)
sum_age_groups_6 <- sum(g1$age_groups == "55-64", na.rm = T)
sum_age_groups_7 <- sum(g1$age_groups == "65+", na.rm = T)

#	The number of people in age group 35-44 is highest followed by 45-54, 25-34, 55-64, 18-24, 65+ and <18.

sum_members <- sum(g1$Signed_In == "0", na.rm = T)
sum_non_members <- sum(g1$Signed_In == "1", na.rm = T)
click_behaviour1 <- sum(g1$Clicks[g1$Signed_In == "0"], na.rm = T)
click_behaviour2 <- sum(g1$Clicks[g1$Signed_In == "1"], na.rm = T)

#	Non-members have lesser number of clicks than members.

impressions_behaviour1 <- sum(g1$Impressions[g1$Signed_In == "0"], na.rm = T)
impressions_behaviour2 <- sum(g1$Impressions[g1$Signed_In == "1"], na.rm = T)

# Non-members have lesser number of impressions than members.

CTR1_behaviour1 <- sum(g1$CTR1[g1$Gender == "0"], na.rm = T)
CTR1_behaviour2 <- sum(g1$CTR1[g1$Gender == "1"], na.rm = T)
signed_in_behaviour1 <- sum(g1$Signed_In[g1$Gender == "0"], na.rm = T)
signed_in_behaviour2 <- sum(g1$Signed_In[g1$Gender == "1"], na.rm = T)

# Plotting Impressions VS Clicks for Male and Female

par(mfrow = c(1, 2))
plot(g1$Impressions[g1$Gender == "0"], g1$Clicks[g1$Gender == "0"], main = "Impressions vs Clicks", xlab = "Impressions for Female", ylab = "Clicks for Female", las = 1)
plot(g1$Impressions[g1$Gender == "1"], g1$Clicks[g1$Gender == "1"], main = "Impressions vs Clicks", xlab = "Impressions for Male", ylab = "Clicks for Male", las = 1)
par(mfrow = c(1, 1))

# ggplots

library(ggplot2)
p1 <- ggplot(data = g1, aes(Impressions, fill = age_groups))
p1 + geom_histogram(binwidth = 0.5) + labs(title = "Histogram for Impressions")
p2 <- ggplot(data = g1, aes(Signed_In, fill = age_groups))
p2 + geom_histogram(binwidth = 0.5) + labs(title = "Histogram for Signed_in")
p3 <- ggplot(g1, aes(Clicks, fill = age_groups))
p3 + geom_histogram(binwidth = 0.5) + labs(title = "Histogram for Clicks")
p4 <- ggplot(g1, aes(Gender, fill = age_groups, fill = age_groups))
p4 + geom_histogram(binwidth = 0.5)
p5 <- ggplot(data = a1, aes(CTR1, fill = age_groups))
p5 + geom_histogram(binwidth = 0.1) + labs(title = "CTR distribution for day1")

#Question 1(c)

# Calculating various statistical parameters

median(g1$Clicks)
median(g1$Impressions)
max(g1$Gender)
var(g1$Impressions)
var(g1$CTR1)
var(g1$Signed_In)
var(g1$Age)
var(g1$Impressions)
sum(g1$Clicks == "0")
sum(g1$Clicks == "1")
sum(g1$Signed_In == "0")
sum(g1$Signed_In == "1")

# Reading the data for remaining 6 days

a2 <- read.csv("~/Desktop/Datasets/all_nyt/nyt2.csv")
a3 <- read.csv("~/Desktop/Datasets/all_nyt/nyt3.csv")
a4 <- read.csv("~/Desktop/Datasets/all_nyt/nyt4.csv")
a5 <- read.csv("~/Desktop/Datasets/all_nyt/nyt5.csv")
a6 <- read.csv("~/Desktop/Datasets/all_nyt/nyt6.csv")
a7 <- read.csv("~/Desktop/Datasets/all_nyt/nyt7.csv")

# Calculating parameters for remaining 6 days

b2 <- a2$Age
CTR2 <- ifelse(a2$Impressions != 0, a2$Clicks/a2$Impressions, NA)
c2 <- cbind(a2, CTR2)
b3 <- a3$Age
CTR3 <- ifelse(a3$Impressions != 0, a3$Clicks/a3$Impressions, NA)
c3 <- cbind(a3, CTR3)
b4 <- a4$Age
CTR4 <- ifelse(a4$Impressions != 0, a4$Clicks/a4$Impressions, NA)
c4 <- cbind(a4, CTR4)
b5 <- a5$Age
CTR5 <- ifelse(a5$Impressions != 0, a5$Clicks/a5$Impressions, NA)
c5 <- cbind(a5, CTR5)
b6 <- a6$Age
CTR6 <- ifelse(a6$Impressions != 0, a6$Clicks/a6$Impressions, NA)
c6 <- cbind(a6, CTR6)
b7 <- a7$Age
CTR7 <- ifelse(a7$Impressions != 0, a7$Clicks/a7$Impressions, NA)
c7 <- cbind(a7, CTR7)

# Making a variable which has CTR for 7 days

CTR_Net <- c(CTR1, CTR2, CTR3, CTR4, CTR5, CTR6, CTR7)
summary(CTR1)
summary(CTR2)
summary(CTR3)
summary(CTR4)
summary(CTR5)
summary(CTR6)
summary(CTR7)

# Calculating Statistical Parameters

summary(a2$Signed_In == "0")
summary(a2$Signed_In == "1")
summary(a3$Signed_In == "0")
summary(a3$Signed_In == "1")
summary(a4$Signed_In == "0")
summary(a4$Signed_In == "1")
summary(a5$Signed_In == "0")
summary(a5$Signed_In == "1")
summary(a6$Signed_In == "0")
summary(a6$Signed_In == "1")
summary(a7$Signed_In == "0")
summary(a7$Signed_In == "1")
summary(a2$Clicks)
summary(a3$Clicks)
summary(a4$Clicks)
summary(a5$Clicks)
summary(a6$Clicks)
summary(a7$Clicks)
summary(a2$Age)
summary(a3$Age)
summary(a4$Age)
summary(a5$Age)
summary(a6$Age)
summary(a7$Age)

# Making a seperate column for day categorization

a1$day <- "day1"
a2$day <- "day2"
a3$day <- "day3"
a4$day <- "day4"
a5$day <- "day5"
a6$day <- "day6"
a7$day <- "day7"

# Binding the rows to get data for 7 days

week <- rbind(a1, a2, a3, a4, a5, a6, a7)
View(week)

# Assignning levels for each day

levels(week$day) <- list('day1' = 'day1', 'day2' = 'day2', 'day3' = 'day3', 'day4' = 'day4', 'day5' = 'day5', 'day6' = 'day6', 'day7' = 'day7')

# Adding CTR column

week <- cbind(week, CTR_Net)

# Making comparisions for Clicks, CTR, Impressions across 7 days

clicks_comp <- aggregate(week$Clicks, by = list(week$day, week$Signed_In), FUN = median, na.rm = T)
View(clicks_comp)
CTR_comp <- aggregate(week$CTR, by = list(week$day, week$Signed_In), FUN = mean, na.rm = T)
View(CTR_comp)
Impressions_comp <- aggregate(week$Impressions, by = list(week$day, week$Signed_In), FUN = median, na.rm = T)
View(Impressions_comp)

# For all the 7 days, members have lesser mean clicks than non-members.
#	For all the 7 days, members have lesser mean CTR than non-members.
#	For all the 7 days, members and non-members have approximately same mean impressions.


