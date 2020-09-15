library(readxl)
IMDB_data <- read_excel("Desktop/Datasets/IMDB data.xlsm")
View(IMDB_data)

#Statistics

head(IMDB_data)
typeof(IMDB_data)
str(IMDB_data)
var(IMDB_data$Votes)
var(IMDB_data$`Runtime (Minutes)`)
var(IMDB_data$`Revenue (Millions)`)
summary(IMDB_data)
summary(IMDB_data$Year)
summary(IMDB_data$`Runtime (Minutes)`)
summary(IMDB_data$Rating)
summary(IMDB_data$Votes)
summary(IMDB_data$`Revenue (Millions)`)
summary(IMDB_data$Metascore)
sum(is.na(IMDB_data))
sum(is.na(IMDB_data$`Revenue (Millions)`))
sum(is.na(IMDB_data$Metascore))

#Making Categories

votes_categories <- cut(IMDB_data$Votes, breaks = c(60, 37000, 120000, 240000, 1800000), labels = c("Low_Votes", "Medium_Votes", "High_Votes", "Highest_Votes"))
Rating_categories <- cut(IMDB_data$Rating, breaks = c(0, 4, 7, 10), labels = c("Low_Ratings", "Medium Ratings", "High_Ratings"))
Revenue_categories <- cut(IMDB_data$`Revenue (Millions)`, breaks = c(0, 47, 113, 940), labels = c("Low_Revenue", "Medium_Revenue", "High_Revenue"))
Metascore_categories <- cut(IMDB_data$Metascore, breaks = c(10, 46, 60, 72, 100), labels = c("Lowest_Metascore", "Low_Metascore", "Medium_Metascore", "High_Metascore"))

#Binding into dataset

IMDB_categorized_data <- cbind(IMDB_data, votes_categories, Rating_categories, Revenue_categories, Metascore_categories)

#Plotting

plot(IMDB_categorized_data$Rank[votes_categories == "Low_Votes"], IMDB_categorized_data$Rating[votes_categories == "Low_Votes"], main = "Rank vs Ratings 1", xlab = "Rank", ylab = "Ratings", col = "red", las=1)

# It can be seen that a majority of movies lie in the rating range of 5-8. While on the other hand there is a large variation in rank of movies of approximately same rating. Perhaps we will have to explore the plot on more variables to find the reason for variation.

plot(IMDB_categorized_data$Rank[votes_categories == "Medium_Votes"], IMDB_categorized_data$Rating[votes_categories == "Medium_Votes"], main = "Rank vs Ratings 2", xlab = "Rank", ylab = "Ratings", las=1)

# The plot is more compressed for medium_votes range as ratings on Y-axis vary from 6-8 for a majority of movies. While on the other hand the disparity of rank remains the same.

plot(IMDB_categorized_data$Rank[votes_categories == "High_Votes"], IMDB_categorized_data$Rating[votes_categories == "High_Votes"], main = "Rank vs Ratings 3", xlab = "Rank", ylab = "Ratings", las=1)

# Like the plot for Medium_Votes, it can be seen that a majority of movies lie in the rating range of 6-8. While on the other hand there is a large variation in rank of movies of approximately same rating. Perhaps we will have to explore the plot on more variables to find the reason for variation.

plot(IMDB_categorized_data$Rank[votes_categories == "Highest_Votes"], IMDB_categorized_data$Rating[votes_categories == "Highest_Votes"], main = "Rank vs Ratings 4", xlab = "Rank", ylab = "Ratings", las=1)

# The plot is more compressed for highest_votes range as ratings on Y-axis vary from 6.5-8 for a majority of movies. Also it can be seen that there is a huge concentration of movies from rank 100-450.

plot(IMDB_categorized_data$Rank[Revenue_categories == "Low_Revenue"], IMDB_categorized_data$Rating[Revenue_categories == "Low_Revenue"], main = "Rank vs Ratings 5", xlab = "Rank", ylab = "Ratings", las=1)

# It can be seen that for low_revenue, the ratings vary for 5.5-8 for a majority of movies. But there is a vast divergence in terms of rank.

plot(IMDB_categorized_data$Rank[Revenue_categories == "Medium_Revenue"], IMDB_categorized_data$Rating[Revenue_categories == "Medium_Revenue"], main = "Rank vs Ratings 6", xlab = "Rank", ylab = "Ratings", las=1)

# The rating vary from 6-8 from a majority of movies while the rank varies from 10-1000.

plot(IMDB_categorized_data$Rank[Revenue_categories == "High_Revenue"], IMDB_categorized_data$Rating[Revenue_categories == "High_Revenue"], main = "Rank vs Ratings 7", xlab = "Rank", ylab = "Ratings", las=1)

# It can be seen that ratings vary from 6-8 and also there is a huge concentration of points from 0-400 rank. Hence we can conclude that movies which generate a high revenue generally have a rating above 6 and rank below 400 with some exceptions.

plot(IMDB_categorized_data$Rank[Metascore_categories == "Lowest_Metascore"], IMDB_categorized_data$Rating[Metascore_categories == "Lowest_Metascore"], main = "Rank vs Ratings 8", xlab = "Rank", ylab = "Ratings", las=1)

# It can be seen that for lowest_metascore categories ratings usually vary in the range of 5-7, while the rank is usually above 350. Hence we can conclude that metascore depends on rank as well as ratings for lowest_metascore categories.

plot(IMDB_categorized_data$Rank[Metascore_categories == "Low_Metascore"], IMDB_categorized_data$Rating[Metascore_categories == "Low_Metascore"], main = "Rank vs Ratings 9", xlab = "Rank", ylab = "Ratings", las=1)

# It can be seen that for low_metascore categories ratings usually vary in the range of 6-8, while the rank is varying from 10-1000.

plot(IMDB_categorized_data$Rank[Metascore_categories == "Medium_Metascore"], IMDB_categorized_data$Rating[Metascore_categories == "Medium_Metascore"], main = "Rank vs Ratings 10", xlab = "Rank", ylab = "Ratings", las=1)

# It can be seen that for medium_metascore categories ratings usually vary in the range of 6.5-8, while the rank has a large variance.

plot(IMDB_categorized_data$Rank[Metascore_categories == "High_Metascore"], IMDB_categorized_data$Rating[Metascore_categories == "High_Metascore"], main = "Rank vs Ratings 11", xlab = "Rank", ylab = "Ratings", las=1)

# It can be seen that for high_metascore categories ratings usually vary in the range of 7-8, while rank has a large variance.

plot(IMDB_data$Rank, IMDB_data$`Runtime (Minutes)`, main = "Rank vs Runtime", xlab = "Rank", ylab = "Runtime", las = 1)

# It can be concluded that a majority of movies have a runtime between 90-130 minutes.

plot(IMDB_data$Rank, log(IMDB_data$Votes), main = "Rank vs Votes", xlab = "Rank", ylab = "Votes", las = 1)

# It can be concluded that log(IMDB_data$votes) varies between 10-14 for a majority of movies that is there are movies of every rank category with a certain range in votes, so the number of votes cannot be a governing factor for rank.

plot(IMDB_categorized_data$`Revenue (Millions)`[Rating_categories == "Low_Ratings"], IMDB_categorized_data$Metascore[Rating_categories == "Low_Ratings"], main = "Revenue vs Metascore 1", xlab = "Revenue", ylab = "Metascore", las = 1)

# It can be seen that in low_rating categories majority of movies generate a revenue below 20 million dollars. Moreover their metascore is also below 55.

plot(IMDB_categorized_data$`Revenue (Millions)`[Rating_categories == "High_Ratings"], IMDB_categorized_data$Metascore[Rating_categories == "High_Ratings"], main = "Revenue vs Metascore 2", xlab = "Revenue", ylab = "Metascore", las = 1)

# It can be seen that for high_ratings category, movies have a metascore varying from 30 to 90 but a majority of movies have a generated revenue below 150 million dollars. So a high/low metascore in high rating categories cannot necessarily impy that it would earn above 150 million dollars.

plot(IMDB_categorized_data$Rating[Metascore_categories == "Low_Metascore"], IMDB_categorized_data$`Revenue (Millions)`[Metascore_categories == "Low_Metascore"], main = "Rating vs Revenue", xlab = "Rating", ylab = "Revenue", las = 1)

# It can be seen that for a low_metascore category, ratings are usually in between 5.5-7.5, while revenue is below 100 million dollars. It can be hence concluded that revenue generation can definitely affect metascore for a movie.

plot(IMDB_categorized_data$Rating[Revenue_categories == "Low_Revenue"], IMDB_categorized_data$Metascore[Revenue_categories == "Low_Revenue"], main = "Rating vs Metascore", xlab = "Rating", ylab = 'Metacore', las = 1)

# It can be said that as the ratings increase the metascore also increases for a majority of low_revenue category movies.

plot(IMDB_categorized_data$Rating, IMDB_categorized_data$`Runtime (Minutes)`, main = "Rating vs Runtime", xlab = "Rating", ylab = "Runtime", las = 1)

# It can be seen that for as runtime increases, ratings also increase in a certain range for a majority of movies.

#ggplots

library(ggplot2)
p1 <- ggplot(data = IMDB_data, aes(Rating))
p1 + geom_histogram(binwidth = 0.1) + labs(title = "Histogram for Rating")
p2 <- ggplot(data = IMDB_data, aes(Votes))
p2 + geom_histogram(binwidth = 50000) + labs(title = "Histogram for Votes")
p3 <- ggplot(data = IMDB_data, aes(Metascore))
p3 + geom_histogram(binwidth = 10) + labs(title = "Histogram for Metascore")

#qqplots

qqnorm(IMDB_categorized_data$Rating[Revenue_categories == "Low_Revenue"])
qqline(IMDB_categorized_data$Rating[Revenue_categories == "Low_Revenue"], col = "red")

# It can be seen that the data for ratings of low_revenue category movies have a normal distribution for a majority of dataset points.

qqplot(IMDB_categorized_data$Rating[Revenue_categories == "Low_Revenue"], IMDB_categorized_data$Metascore[Revenue_categories == "Low_Revenue"], xlab = "Ratings", ylab = "Metascore", col = "blue", las = 1)

# It can be seen that qqplot for Ratings vs Metascore shows that set points have a normal distribution.
