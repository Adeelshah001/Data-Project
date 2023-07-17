#All coding done in R
#viewing the data set
library(readxl)
Video_Games <- read_excel("Video_Games.xlsx")
View(Video_Games)
head(Video_Games)
tail(Video_Games)
str(Video_Games)
summary(Video_Games)
unique(Video_Games$Platform)
unique(Video_Games$Genre)

#sales for games released in certain year
sales_per_year <- aggregate(Video_Games$Global_Sales ~ Video_Games$Year_of_Release, data = Video_Games, sum)
head(sales_per_year)
#sales per developer/publisher
sales_per_dev <- aggregate(Global_Sales ~ Developer, data = Video_Games, sum)
sales_per_pub <- aggregate(Global_Sales ~ Publisher, data = Video_Games, sum)

#Sony Sales
Sony_Sales <- subset(Video_Games, Publisher == "Sony Computer Entertainment")
total_sony_sales <- sum(Sony_Sales$Global_Sales)
total_sony_sales

#Nintendo sales
Nin_Sales <- subset(Video_Games, Publisher == "Nintendo")
total_Nin_sales <- sum(Nin_Sales$Global_Sales)
total_Nin_sales

#Cleaning and prepping for analysis on global sales attribute
Video_Games2 <- Video_Games[, c("Name", "Platform", "Year_of_Release", "Genre", "Publisher", "Developer", "Global_Sales")]
Video_Games2 <- na.omit(Video_Games2)

#running cross validation on the prepped data set, using genre and platform to predict global sales***
library(caret)
selected_cols <- c("Genre", "Platform", "Global_Sales")
dataset_subset <- Video_Games2[selected_cols]
dataset_subset$Genre <- as.factor(dataset_subset$Genre)
dataset_subset$Platform <- as.factor(dataset_subset$Platform)
ctrl <- trainControl(method = "cv", 
                     number = 5,  
                     verboseIter = FALSE) 
model1 <- train(Global_Sales ~ Genre + Platform,  
               data = dataset_subset,          
               method = "lm",                    
               trControl = ctrl)                  
print(model1)
summary(model1)

#cross validation with added columns of publisher and developer
selected_cols2 <- c("Genre", "Platform", "Publisher", "Developer", "Global_Sales")
dataset_subset <- Video_Games2[selected_cols2]
dataset_subset$Genre <- as.factor(dataset_subset$Genre)
dataset_subset$Platform <- as.factor(dataset_subset$Platform)
ctrl <- trainControl(method = "cv",  
                     number = 5,   
                     verboseIter = FALSE)  
model2 <- train(Global_Sales ~ Genre + Platform + Developer + Publisher, 
               data = dataset_subset,            
               method = "lm",                     
               trControl = ctrl)                
print(model2)
summary(model2)

#performing ANOVA test on the "Global_Sales" column based on different genres
anova_result1 <- aov(Global_Sales ~ Genre, data = Video_Games2)
print(summary(anova_result1))
plot(anova_result1)
#ANOVA with the all relevant columns
anova_result2 <- aov(Global_Sales ~ Genre + Platform + Publisher + Developer, data = Video_Games2)
print(summary(anova_result2))

#creating another column to indicate games that appear on more than one platform
Video_Games3 <- Video_Games2
Video_Games3$Multiple_Platforms <- 0
game_counts <- table(Video_Games3$Name)
for (game_title in names(game_counts)) {
  if (game_counts[game_title] > 1) {
    Video_Games3$Multiple_Platforms[Video_Games3$Name == game_title] <- 1
  }
}
Video_Games3$Multiple_Platforms <- ifelse(Video_Games3$Multiple_Platforms == 1, "yes", "no")

#running decision tree with multiple platforms column and global sales
library(rpart)
library(rpart.plot)
Video_Games3$Multiple_Platforms <- as.factor(Video_Games3$Multiple_Platforms)
str(Video_Games3)
multiplat_model <- rpart(Multiple_Platforms ~ Global_Sales, data = Video_Games3, method = "class")
rpart.plot(multiplat_model)
multiplat_model
#this plot gives us an insight on multiplatform games and their sales performance. 

#genre per region 
global_genre <- aggregate(Global_Sales ~ Genre, data = Video_Games3, FUN = sum)
global_genre
sorted_global_genre <- global_genre[order(-global_genre$Global_Sales),]
sorted_global_genre

#barplot of genre by global sales
plot(sorted_global_genre$Genre, sorted_global_genre$Global_Sales)
data_bar <- sorted_global_genre$Global_Sales
names(data_bar) <- sorted_global_genre$Genre
data_bar
barplot(data_bar, main = "Global Sales by Genre")
#viewing the other genres per region
NA_genre <- aggregate(NA_Sales ~ Genre, data = Video_Games, FUN = sum)
NA_genre
sorted_NA_genre <- NA_genre[order(-NA_genre$NA_Sales),]
sorted_NA_genre
EU_genre <- aggregate(EU_Sales ~ Genre, data = Video_Games, FUN = sum)
EU_genre
sorted_EU_genre <- EU_genre[order(-EU_genre$EU_Sales),]
sorted_EU_genre
JP_genre <- aggregate(JP_Sales ~ Genre, data = Video_Games, FUN = sum)
JP_genre
sorted_JP_genre <- JP_genre[order(-JP_genre$JP_Sales),]
sorted_JP_genre
aggregated_sales <- aggregate(cbind(NA_Sales, EU_Sales, JP_Sales, Other_Sales) ~ Genre, data = Video_Games, sum)
print(aggregated_sales)
sorted_sales <- aggregated_sales[order(-aggregated_sales$NA_Sales, -aggregated_sales$EU_Sales, -aggregated_sales$JP_Sales, -aggregated_sales$Other_Sales), ]
sorted_sales
plot(sorted_sales$Genre)

#corelation matrix for sales regions
sales_data <- Video_Games[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")]
cor_matrix <- cor(sales_data)
print(cor_matrix)
heatmap(cor_matrix, 
        col = colorRampPalette(c("#FF0000", "white", "#0000FF"))(100),
        main = "Correlation Matrix",
        xlab = "Sales Columns",
        ylab = "Sales Columns",
        cex.main = 1.2,
        cex.axis = 1.2)

#multiple bar plots of regions sales
library(ggplot2)
sales_data_genre <- Video_Games[, c("Genre", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")]
sales_data_long <- tidyr::gather(sales_data_genre, Region, Sales, -Genre)
ggplot(sales_data_long, aes(x = Genre, y = Sales)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Region, scales = "free_y") +
  labs(x = "Genre", y = "Sales") +
  ggtitle("Region Sales by Genre") +
  theme_minimal()
#the disparity of Role-Playing genre in JP_Sales gives insighty as to the type of games preferred in that region.

#sales performance of genre by year
genre_sales_by_year <- aggregate(Global_Sales ~ Year_of_Release + Genre, data = Video_Games, FUN = sum)
ggplot(genre_sales_by_year, aes(x = Year_of_Release, y = Global_Sales, fill = Genre)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Year", y = "Global Sales", fill = "Genre") +
  ggtitle("Genre Sales by Year")

#genre sales in highest selling year
target_year <- 2008
yearly_sales <- subset(Video_Games, Year_of_Release == target_year)
ggplot(yearly_sales, aes(x = Genre, y = Global_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Genre", y = "Global Sales", title = paste("Genre Sales in", target_year))

#genre frequency
genre_frequency <- table(Video_Games$Genre)
genre_proportions <- prop.table(genre_frequency) * 100
labels <- paste(names(genre_proportions), "(", round(genre_proportions, 1), "%)", sep = "")
pie(genre_proportions, labels = labels, main = "Genre Frequency")

#Top Publishers global
publisher_sales <- aggregate(Global_Sales ~ Publisher, data = Video_Games3, sum)
publisher_sales <- publisher_sales[order(publisher_sales$Global_Sales, decreasing = TRUE), ]
top_10_publishers <- head(publisher_sales, 10)
barplot(top_10_publishers$Global_Sales, names.arg = top_10_publishers$Publisher, xlab = "Publisher", ylab = "Global Sales")

#Sales by user score
Video_Games$User_Score <- as.numeric(as.character(Video_Games$User_Score))
subset_df <- Video_Games[!is.na(Video_Games$User_Score), ]
sales_by_user_score <- aggregate(subset_df$Global_Sales, by = list(user_score = subset_df$User_Score), FUN = sum)
sales_by_user_score <- sales_by_user_score[order(sales_by_user_score$user_score, decreasing = TRUE), ]
barplot(sales_by_user_score$x, names.arg = sales_by_user_score$user_score,
        xlab = "User Score", ylab = "Sales",
        main = "Sales by User Score")

# Random Split + random forest algorithm
library(caret)
#install.packages("randomForest")
library(randomForest)
trainIndex <- createDataPartition(Video_Games3$Global_Sales, p = 0.7, list = FALSE)
train_data <- Video_Games3[trainIndex, ]
test_data <- Video_Games3[-trainIndex, ]
features <- train_data[, c("Genre", "Platform", "Year_of_Release", "Publisher")]
target <- train_data$Global_Sales
num_trees <- 100
rf_model <- randomForest(features, target, ntree = num_trees)
test_features <- test_data[, c("Genre", "Platform", "Year_of_Release", "Publisher")]
predictions <- predict(rf_model, newdata = test_features)
actual_sales <- test_data$Global_Sales
accuracy <- mean(predictions == actual_sales)
print(paste("Accuracy:", accuracy))
#random forest is not usable for this data set. 

#linear Regression Algorithm
#encoding the categorical variables for linear regression model.
genre_encoded <- model.matrix(~ Genre - 1, data = Video_Games2)
platform_encoded <- model.matrix(~ Platform - 1, data = Video_Games2)
year_encoded <- model.matrix(~ Year_of_Release - 1, data = Video_Games2)
publisher_encoded <- model.matrix(~ Publisher - 1, data = Video_Games2)
encoded_df <- cbind(genre_encoded, platform_encoded, year_encoded, publisher_encoded)
response <- Video_Games2$Global_Sales
model <- lm(response ~ ., data = as.data.frame(encoded_df))
summary(model)
#the residuals do not follow a normal distribution. 

#SVM - Support Vector Machines Algorithm
#install.packages("e1071")
library(e1071)
library(caret)
formula <- as.formula("~ Genre + Platform + Year_of_Release + Publisher")
encoded_data <- model.matrix(formula, data = Video_Games3)
encoded_data <- cbind(encoded_data, Global_Sales = Video_Games3$Global_Sales)
encoded_data <- as.data.frame(encoded_data)
train_indices <- sample(nrow(encoded_data), round(0.7 * nrow(encoded_data)))
train_data <- encoded_data[train_indices, ]
test_data <- encoded_data[-train_indices, ]
svm_model <- svm(Global_Sales ~ ., data = train_data)
svm_predictions <- predict(svm_model, newdata = test_data)
confusion_matrix <- table(svm_predictions, test_data$Global_Sales)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)
#SVM does not perform well with this dataset either. 

