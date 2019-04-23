#Load Packages 

install.packages("ggplot2")
library(ggplot2)
install.packages("ggrepel")
library(ggrepel)
install.packages("ggthemes")
library(ggthemes)
install.packages("scales")
library(scales)
install.packages("dplyr")
library(dplyr)
install.packages("VIM")
library(VIM)
install.packages("data.table")
library(data.table)
install.packages("formattable")
library(formattable)
install.packages("plotly")
library(plotly)
install.packages("corrplot")
library(corrplot)
install.packages("GGally")
library(GGally)
install.packages("caret")
library(caret)
install.packages("car")
library(car)

#Read Data

movie_data <- read.csv("~/Desktop/movie_metadata.csv", header=TRUE)
str(movie_data)



# DATA EXPLORATION 


# Look for duplicates and delete them
sum(duplicated(movie_data))
movie_data <- movie_data[!duplicated(movie_data),]

# Tidy the movie title- garbage found before the actual name

library(stringr)
movie_data$movie_title <- gsub("Â", "", as.character(factor(movie_data$movie_title)))
str_trim(movie_data$movie_title, side = "right")

# Check all genres of the movies

head(movie_data$genres)

# Create a dataframe to store the substrings
genres.df <- as.data.frame(movie_data[,c("genres", "imdb_score")])

# Separate different genres 

genres.df$Action <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Action") 1 else 0)
genres.df$Action <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Action") 1 else 0)
genres.df$Adventure <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Adventure") 1 else 0)
genres.df$Animation <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Animation") 1 else 0)
genres.df$Biography <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Biography") 1 else 0)
genres.df$Comedy <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Comedy") 1 else 0)
genres.df$Crime <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Crime") 1 else 0)
genres.df$Documentary <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Documentary") 1 else 0)
genres.df$Drama <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Drama") 1 else 0)
genres.df$Family <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Family") 1 else 0)
genres.df$Fantasy <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Fantasy") 1 else 0)
genres.df$`Film-Noir` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Film-Noir") 1 else 0)
genres.df$History <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "History") 1 else 0)
genres.df$Horror <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Horror") 1 else 0)
genres.df$Musical <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Musical") 1 else 0)
genres.df$Mystery <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Mystery") 1 else 0)
genres.df$News <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "News") 1 else 0)
genres.df$Romance <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Romance") 1 else 0)
genres.df$`Sci-Fi` <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Sci-Fi") 1 else 0)
genres.df$Short <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Short") 1 else 0)
genres.df$Sport <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Sport") 1 else 0)
genres.df$Thriller <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Thriller") 1 else 0)
genres.df$War <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "War") 1 else 0)
genres.df$Western <- sapply(1:length(genres.df$genres), function(x) if (genres.df[x,1] %like% "Western") 1 else 0)

# Find the mean of imdb score for different genres

means <- rep(0,23)
for (i in 1:23) {
  means[i] <- mean(genres.df$imdb_score[genres.df[i+2]==1])
}

# Plot the Means

barplot(means, main = "Mean od the imdb scores for different genres")

# All means are in the range of 6-8, it can be assumed that not a lot of difference will be made to the 
# IMDB score if genres were present

movie_data <- subset(movie_data, select = -c(genres))

# Making sure Genres returns a NULL
str(movie_data$genres)

# Find Aggregate of NAs in all columns 

colSums(sapply(movie_data, is.na))

# Plotting a heat map to visualize the missing values 

missing.values <- aggr(movie_data, sortVars = T, prop = T, sortCombs = T, cex.lab = 1.5, cex.axis = .6, cex.numbers = 5, combined = F, gap = -.2)
    
# Gross and Budget have the highest amount of missing values but both of them are important factors in determing
# the IMDB score and thus we remove only the rows which have NA in them 

movie_data <- movie_data[!is.na(movie_data$gross),]
movie_data <- movie_data[!is.na(movie_data$budget),]

# Checking how much was our daat affected due to removing rows 

dim(movie_data)

# 23% of data removed, still consists of 3857 records for analysis 
# Rechecking the number of NAs

sum(complete.cases(movie_data))

colSums(sapply(movie_data, is.na))

# aspect ratio has the highest number of NAs, checking how important aspect ration in prediction is

table(movie_data$aspect_ratio)

# Replacing NAs in aspect ration with 0 

movie_data$aspect_ratio[is.na (movie_data$aspect_ratio)] <- 0

# Checking mean where aspect ratio is 1.85 and 2.35
mean(movie_data$imdb_score[movie_data$aspect_ratio == 1.85])

mean(movie_data$imdb_score[movie_data$aspect_ratio == 2.35])

# Checking mean where aspect ratio is not 1.85 and 2.35
mean(movie_data$imdb_score[movie_data$aspect_ratio != 1.85 & movie_data$aspect_ratio != 2.35])

# Observed: The mean in either of the cases isn't deviating much and it can be assumed tht removing this variable 
# will not affect our analysis

movie_data <- subset(movie_data, select = -c(aspect_ratio))

# Rechecking if the aspect ratio is still present or is NULL

str(movie_data$aspect_ratio)

# Replacing NAs and 0s in the Data 

# Replacing NA in facenumber_in_poster with the average of the column 

movie_data$facenumber_in_poster[is.na(movie_data$facenumber_in_poster)] <- round(mean(movie_data$facenumber_in_poster, na.rm = TRUE))

# Convert 0s in the data to NAs 
movie_data[,c(5,6,8,13,24,26)][movie_data[,c(5,6,8,13,24,26)] == 0] <- NA

# Replacing NA in num_critic_for_reviews with the average of the column 

movie_data$num_critic_for_reviews[is.na(movie_data$num_critic_for_reviews)] <- round(mean(movie_data$num_critic_for_reviews, na.rm = TRUE))

# Replacing NA in duration with the average of the column 

movie_data$duration[is.na(movie_data$duration)] <- round(mean(movie_data$duration, na.rm = TRUE))

# Replacing NA in director_facebook_likes with the average of the column 

movie_data$director_facebook_likes[is.na(movie_data$director_facebook_likes)] <- round(mean(movie_data$director_facebook_likes, na.rm = TRUE))

# Replacing NA in actor_3_facebook_likes with the average of the column 

movie_data$actor_3_facebook_likes[is.na(movie_data$actor_3_facebook_likes)] <- round(mean(movie_data$actor_3_facebook_likes, na.rm = TRUE))

# Replacing NA in actor_1_facebook_likes with the average of the column 

movie_data$actor_1_facebook_likes[is.na(movie_data$actor_1_facebook_likes)] <- round(mean(movie_data$actor_1_facebook_likes, na.rm = TRUE))

# Replacing NA in cast_total_facebook_likes with the average of the column 

movie_data$cast_total_facebook_likes[is.na(movie_data$cast_total_facebook_likes)] <- round(mean(movie_data$cast_total_facebook_likes, na.rm = TRUE))

# Replacing NA in actor_2_facebook_likes with the average of the column 

movie_data$actor_2_facebook_likes[is.na(movie_data$actor_2_facebook_likes)] <- round(mean(movie_data$actor_2_facebook_likes, na.rm = TRUE))

# Replacing NA in movie_facebook_likes with the average of the column 

movie_data$movie_facebook_likes[is.na(movie_data$movie_facebook_likes)] <- round(mean(movie_data$movie_facebook_likes, na.rm = TRUE))

# Finding the missing values in content rating  

table(movie_data$content_rating)

# Blanks are to be considered as missing values 

movie_data <- movie_data[!(movie_data$content_rating %in% ""),]

# Re-evaluating content ratings 
# M = GP = PG, X = NC-17. Replace M and GP with PG and replace X with NC-17

movie_data$content_rating
movie_data$content_rating[movie_data$content_rating == 'M']   <- 'PG' 
movie_data$content_rating[movie_data$content_rating == 'GP']  <- 'PG' 
movie_data$content_rating[movie_data$content_rating == 'X']   <- 'NC-17'

# Replace “Approved”, “Not Rated”, “Passed”, “Unrated” with the most common rating “R”

movie_data$content_rating[movie_data$content_rating == 'Approved']  <- 'R' 
movie_data$content_rating[movie_data$content_rating == 'Not Rated'] <- 'R' 
movie_data$content_rating[movie_data$content_rating == 'Passed']    <- 'R' 
movie_data$content_rating[movie_data$content_rating == 'Unrated']   <- 'R' 
movie_data$content_rating <- factor(movie_data$content_rating)

table(movie_data$content_rating)

# Creating 2 columns profit and percentage of return based on gross and budget 
movie_data<- movie_data %>% 
mutate(profit = gross - budget,
return_on_investment_perc = (profit/budget)*100)

# Checking if movie color is an influential factor towards it's score 

table(movie_data$color)

# It can be observed that the data in color is completely partial towards colored movies 
# and thus it is not an influential factor and we can remove it 
movie_data <- subset(movie_data, select = -c(color))

# Checking if color is removed from the data and returns a NULL value 
movie_data$color

# Checking if movie color is an influential factor towards it's score 

table(movie_data$language)

# It can be observed that the data in langauges is completely partial towards english movies 
# and thus it is not an influential factor and we can remove it 

movie_data <- subset(movie_data, select = -c(language))

# Checking if language is removed from the data and returns a NULL value 
movie_data$language

# Checking if the country the movie is produced in is an influential factor towards it's score 

table(movie_data$country)

# Approximately 79% movies are form the US, 8% from UK and 13% from other countries
# Thus we collectovely represent the movie locations as: US, UK, Others

levels(movie_data$country) <- c(levels(movie_data$country), "Others")
movie_data$country[(movie_data$country != 'USA')&(movie_data$country != 'UK')] <- 'Others' 
movie_data$country <- factor(movie_data$country)

# Checking if only 3 locations are available 

table(movie_data$country)





# DATA VISUALIZATION


# Histogram of movies released each year
ggplot(movie_data, aes(title_year)) +
  geom_bar() +
  labs(x = "Year movie release", y = "No. of movies released", title = "Histogram of no. of movies released each year") +
  theme(plot.title = element_text(hjust = 0.5))

# It can be seen that the movie boom came after 1980 and thus we represent the data only after 1980 

movie_data <- movie_data[movie_data$title_year >= 1980,]

# Visualizing top 20 movies based on profits in Million$

install.packages("ggrepel")
library(ggrepel)
movie_data %>%
  filter(title_year %in% c(2000:2016)) %>%
  arrange(desc(profit)) %>%
  top_n(20, profit) %>%
  ggplot(aes(x=budget/1000000, y=profit/1000000)) +
  geom_point() +
  geom_smooth() + 
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Budget in $million", y = "Profit in $million", title = "Top 10 Profitable Movies") +
  theme(plot.title = element_text(hjust = 0.5))

# Using profits and return on investment variables are criteria to find 20 most profitable movies 

movie_data %>%
  filter(budget > 100000) %>%
  mutate(profit = gross - budget,
         return_on_investment_perc = (profit/budget)*100) %>%
  arrange(desc(profit)) %>%
  top_n(20, profit) %>%
  ggplot(aes(x=budget/1000000, y = return_on_investment_perc)) + 
  geom_point(size = 2) + 
  geom_smooth(size = 1) + 
  geom_text_repel(aes(label = movie_title), size = 3) + 
  xlab("Budget in $million") + 
  ylab("Percent of Return on Investment") + 
  ggtitle("20 Most Profitable Movies based on their Return on Investment")


# Visualizing 20 top directors based on the highest IMDB scores 

install.packages("formattable")
library(formattable)
movie_data %>%
  group_by(director_name) %>%
  summarise(avg_imdb = mean(imdb_score)) %>%
  arrange(desc(avg_imdb)) %>%
  top_n(20, avg_imdb) %>%
  formattable(list(avg_imdb = color_bar("Red")), align = 'l')

# Plotting commerical success vs critical acclaim 

movie_data %>%
  top_n(20, profit) %>%
  ggplot(aes(x = imdb_score, y = gross/10^6, size = profit/10^6, color = content_rating)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 600)) + 
  geom_vline(aes(xintercept = 7.75)) + 
  geom_text_repel(aes(label = movie_title), size = 4) +
  xlab("IMDB Score") + 
  ylab("Gross Money earned in million$") + 
  ggtitle("Commercial Success Vs Critical Acclaim") +
  annotate("text", x = 8.5, y = 700, label = "High Ratings \n & High Gross") +
  theme(plot.title = element_text(hjust = 0.5))

# The above observation shows that there is hardly any correlation between critical acclaim and the movie's commercial success


# Visualizing relation between facebook likes and IMDB scores

library(plotly)
movie_data %>%
  plot_ly(x = ~movie_facebook_likes, y = ~imdb_score, color = ~content_rating , mode = "markers", 
          text = ~content_rating, alpha = 0.7, type = "scatter")

# Movies with high facebook likes can be seen to have higher IMDB score



# DATA PRE-PROCESSING


install.packages("data.table")
library(data.table)

# Find number of directors
sum(uniqueN(movie_data$director_name))

# Find number of actors
sum(uniqueN(movie_data[, c("actor_1_name", "actor_2_name", "actor_3_name")]))

# The names of the directors, actors 1 2 3 are so different that it will not contribute in predicting the score. 
# The plot keyword is too diverse to be used as a predictor 
# The movie IMDB link is redundant 

movie_data <- subset(movie_data, select = -c(director_name, actor_2_name, actor_1_name,
                                 movie_title, actor_3_name, plot_keywords, 
                                 movie_imdb_link))

# To avoid multicollinearity we remove the 2 previously added variables 

movie_data <- subset(movie_data, select = -c(profit, return_on_investment_perc))

# Plot heatmap of the entire data as of now 

ggcorr(movie_data, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

# Based on the heatmap, we can see some high correlations (>0.7) between predictors.
# The highest correlation value observed is 0.95 and we can see that actor_1_facebook_likes is highly correlated with the cast_total_facebook_likes
# and both actor2 and actor3 are also correlated to the total. 
# Thus we modify them into 2 variables: actor_1_facebook_likes and other_actors_facebook_likes.

movie_data$other_actors_facebook_likes <- movie_data$actor_2_facebook_likes + movie_data$actor_3_facebook_likes

# There is high correlations among num_voted_users, num_user_for_reviews and num_critic_for_reviews. 
# We want to keep num_voted_users and take the ratio of num_user_for_reviews and num_critic_for_reviews.

movie_data$critic_review_ratio <- movie_data$num_critic_for_reviews / movie_data$num_user_for_reviews

# Delete Columns 

movie_data <- subset(movie_data, select = -c(cast_total_facebook_likes, actor_2_facebook_likes, actor_3_facebook_likes,
                                 num_critic_for_reviews, num_user_for_reviews))


# Plotting heatmap to review post changes

ggcorr(movie_data, label = TRUE, label_round = 2, label_size = 4, size = 3, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

# No strong correlation of value greater than 0.7 observed

# The aim is to build a project wherein the model predicts whether the movie is good or bad. So bin the scores in four buckets: less than 4(Bad),
# 4-6(OK), 6-8(Good) and 8-10(Excellent)

movie_data$binned_score <- cut(movie_data$imdb_score, breaks = c(0,4,6,8,10))

# Rearranging the data and renaming the column to make it readable 

movie_data <- movie_data[,c(9,4,5,14,12,2,3,13,1,6,10,7,8,11,15)]
colnames(movie_data) <- c("budget", "gross", "user_vote", "critic_review_ratio",
                    "movie_fb", "director_fb", "actor1_fb", "other_actors_fb",
                    "duration", "face_number", "year", "country", "content",
                    "imdb_score", "binned_score")


# To apply models, spliting the data into training, validation and test sets with the ratio of 6:2:2
set.seed(45)
train.index <- sample(row.names(movie_data), dim(movie_data)[1]*0.6)
valid.index <- sample(setdiff(row.names(movie_data), train.index), dim(movie_data)[1]*0.2)
test.index <- setdiff(row.names(movie_data), union(train.index, valid.index))
train <- movie_data[train.index, ]
valid <- movie_data[valid.index, ]
test <- movie_data[test.index, ]



# IMPLEMNETATION OF ALGORITHMS 

# CLASSIFICATION TREE

# Implementing a full grown tree

library(rpart)
library(rpart.plot)
# Full grown tree
class.tree <- rpart(binned_score ~ . -imdb_score, data = train, method = "class")
## plot tree
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = 0) 


# Implementing Best pruned tree 

set.seed(51)
cv.ct <- rpart(binned_score ~ . -imdb_score, data = train, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)
printcp(cv.ct)
pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)


# Apply model on training set
tree.pred.train <- predict(pruned.ct, train, type = "class")
# Generate confusion matrix for training data
confusionMatrix(tree.pred.train, train$binned_score)

# Apply model on validation set
tree.pred.valid <- predict(pruned.ct, valid, type = "class")
# Generate confusion matrix for validation data
confusionMatrix(tree.pred.valid, valid$binned_score)

# Apply model on test set
tree.pred.test <- predict(pruned.ct, test, type = "class")
# Generate confusion matrix for test data
confusionMatrix(tree.pred.test, test$binned_score)


# K NEAREST NEIGHBOUR 

library(FNN)
# Using model.matrix() to create dummy variables for country and content
movie_data2 <- movie_data
movie_data2$country <- as.factor(movie_data2$country)
movie_data2$content <- as.factor(movie_data2$content)
movie_data2[,c("country_UK", "country_USA", "country_Others")] <- model.matrix( ~ country - 1, data = movie_data2)
movie_data2[,c("content_G", "content_NC17", "content_PG", "content_PG13", "content_R")] <- model.matrix( ~ content - 1, data = movie_data2)

# Select useful variables for future prediction
movie_data2 <- movie_data2[, c(1,2,3,4,5,6,7,8,9,10,11,16,17,18,19,20,21,22,23,15)]
# Partition the data into training and validation sets
set.seed(52)
train2 <- movie_data2[train.index, ]
valid2 <- movie_data2[valid.index, ]
test2 <- movie_data2[test.index, ]


# Initializing normalized training, validation, test data, complete data frames to originals
train2.norm <- train2
valid2.norm <- valid2
test2.norm <- test2
movie_data2.norm <- movie_data2

# Using preProcess() from the caret package to normalize predictors
norm.values <- preProcess(train2[, -20], method=c("center", "scale"))
train2.norm[, -20] <- predict(norm.values, train2[, -20])
valid2.norm[, -20] <- predict(norm.values, valid2[, -20])
test2.norm[, -20] <- predict(norm.values, test2[, -20])
movie_data2.norm[, -20] <- predict(norm.values, movie_data2[, -20])

# Finding the best K 

# Initialize a data frame with two columns: k, and accuracy

accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))

# Computing knn for different k on validation data

for(i in 1:20) {
  knn.pred <- knn(train2.norm[, -20], valid2.norm[, -20],
                  cl = train2.norm[, 20], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid2.norm[, 20])$overall[1]
}
accuracy.df


# Applying model on test set

knn.pred.test <- knn(train2.norm[, -20], test2.norm[, -20],
                     cl = train2.norm[, 20], k = 9)

# Generating confusion matrix for test data

accuracy <- confusionMatrix(knn.pred.test, test2.norm[, 20])$overall[1]
accuracy


# RANDOM FOREST

install.packages("randomForest")
library(randomForest)
set.seed(53)
rf <- randomForest(binned_score ~ . -imdb_score, data = train, mtry = 5)
# Show model error
plot(rf)
legend('topright', colnames(rf$err.rate), col=1:5, fill=1:5)


# Get importance
importance <- importance(rf)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Creating a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Using ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

install.packages("caret")
library(caret)
set.seed(632)
# apply model on validation set
rf.pred.valid <- predict(rf, valid)
# generate confusion matrix for validation data
confusionMatrix(rf.pred.valid, valid$binned_score)




