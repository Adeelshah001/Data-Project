# Data-Project
Data Analytics Project on Video Game Sales Dataset Updated -Extra Feat by IBRAHIM MUHAMMAD NAEEM
Abstract: 
"What are the key factors influencing the sales performance of video games, and how can they be leveraged to optimize marketing strategies within the industry?" The purpose of this project is to answer this question and gain insights in the dataset I have chosen. Video games have become a significant source of revenue and data in modern society, as they engage consumers in interactive digital experiences. Understanding the dynamics of video game sales, including factors such as game types, genres, and regional popularity, is crucial for assessing their widespread impact. This data-driven project aims to develop a predictive model for video game sales using the "Video Game Sales Dataset Updated - Extra Feat" by IBRAHIM MUHAMMAD NAEEM on Kaggle. A Dataset with 16 columns and over 11,000 values, the dependent variables analyzed in this project will be the regional sales columns. By analyzing this comprehensive dataset, which includes sales information for various games across platforms and studios, we seek to identify key influencing factors that contribute to overall sales.
My analysis will focus on predictive analysis and correlation analysis to uncover patterns and relationships between attributes such as genre, platform, year of release, critic scores, and user scores. To ensure data quality, a thorough data preprocessing phase will be conducted, involving cleaning, handling missing values, identifying outliers, and applying necessary transformations. Exploratory data analysis techniques will then be employed to gain deeper insights into sales patterns. The findings from this project will provide valuable insights for video game developers and publishers, helping them make informed decisions about game releases. In conclusion, this data analytics project will contribute to the understanding of factors influencing video game sales through preprocessing, exploratory data analysis, correlation analysis, and predictive modeling.

Coding is done in R

#Loading in the dataset.
install.packages("dplyr")
library(dplyr)
library(readxl)
Video_Games <- read_excel("Video_Games.xlsx")
View(Video_Games)

#Viewing the Summary Statistics.
head(Video_Games)
tail(Video_Games)
str(Video_Games)
summary(Video_Games)
summary(Video_Games$Global_Sales)
mean_sales <- mean(Video_Games$Global_Sales)
median_sales <- median(Video_Games$Global_Sales)
max_sales <- max(Video_Games$Global_Sales)
min_sales <- min(Video_Games$Global_Sales)
variance_sales <- var(Video_Games$Global_Sales)
sd_sales <- sd(Video_Games$Global_Sales)
cat("Mean Sales: ", mean_sales, "\n")
cat("Median Sales: ", median_sales, "\n")
cat("Maximum Sales: ", max_sales, "\n")
cat("Minimum Sales: ", min_sales, "\n")
cat("Variance of Sales: ", variance_sales, "\n")
cat("Standard Deviation of Sales: ", sd_sales, "\n")

#Viewing min/max observations.
max_sales_observation <- Video_Games[Video_Games$Global_Sales == max_sales, ]
print(max_sales_observation)
min_sales_observation <- Video_Games[Video_Games$Global_Sales == min_sales, ]
print(min_sales_observation)



