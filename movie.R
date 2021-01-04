# set working directory
setwd("C:/Users/ok/iCloudDrive/KMU/blog/201223 linear regression")
movie <- read.csv("movie_new2.csv")
head(movie)
summary(movie)

#remove the first column(movie titles)
movie_data <- movie[,-1]

#remove the column including N/A
movie_data <- na.omit(movie_data)

#In the original dataset, switch the data format for analyzing
#remove the comma in the numeric data
movie_data$전국스크린수 <- as.numeric(movie_data$전국스크린수)
movie_data$전국매출액 <- as.numeric(movie_data$전국매출액)
movie_data$전국관객수 <- as.numeric( movie_data$전국관객수)
movie_data$상영횟수 <- as.numeric(movie_data$상영횟수)
movie_data$관람객평점 <- as.numeric(movie_data$관람객평점)
movie_data$X15세 <- as.factor(movie_data$X15세)
movie_data$X12세 <- as.factor(movie_data$X12세)
movie_data$메이저제작사 <- as.factor(movie_data$메이저제작사)
movie_data$메이저배급사 <- as.factor(movie_data$메이저배급사)
movie_data$독립여부 <- as.factor(movie_data$독립여부)
movie_data$스타파워 <- as.factor(movie_data$스타파워)
movie_data$감독파워 <- as.factor(movie_data$감독파워)
movie_data$청불 <- as.factor(movie_data$청불)
movie_data$전체 <- as.factor(movie_data$전체)

#check the type of features
str(movie_data)

#summary of the data
summary(movie_data)
sort(movie_data$전국스크린수, decreasing=T)
movie_data$전국스크린수
#descriptive statistic
hist(movie_data$전국매출액)
hist(movie_data$전국스크린수)
hist(movie_data$전국관객수)
hist(movie_data$상영횟수)
hist(movie_data$관람객평점)
hist(movie_data$평론가평점)
hist(movie_data$네티즌평점)

#scatter plot matrix
pairs(movie_data)

#fit the data with multi-linear regression model
fit <- lm(전국매출액~., data=movie_data)
summary(fit)


# Normalization scaling with new function
# make the range of the data between 0 and 1
normalize <- function(x) {
  return((x-min(x)) / (max(x)-min(x)))
}
movie_data$전국스크린수 <- normalize(movie_data$전국스크린수)
movie_data$전국매출액 <- normalize(movie_data$전국매출액)
movie_data$전국관객수 <- normalize(movie_data$전국관객수)
movie_data$상영횟수 <- normalize(movie_data$상영횟수)
movie_data$관람객평점 <- normalize(movie_data$관람객평점)
movie_data$평론가평점 <- normalize(movie_data$평론가평점)
movie_data$네티즌평점 <- normalize(movie_data$네티즌평점)

fit <- lm(전국매출액~., data=movie_data)
summary(fit)

# select only continuous variables
fit <- lm(전국매출액~ 전국관객수 + 전국스크린수 + 상영횟수 +
                 관람객평점 + 평론가평점 + 네티즌평점, data=movie_data)
summary(fit)

# check scatter plot with 매출액 and 관객수
plot(x=movie_data$전국매출액, y=movie_data$전국관객수)

# remove 전국관객수 variable and select continuous variables
fit <- lm(전국매출액~ 전국스크린수 + 상영횟수 +
                 관람객평점 + 평론가평점 + 네티즌평점, data=movie_data)
summary(fit)

# as I thought, 전국관객수 variable affect so much on the model
# So, I select all variables except 전국관객수 variable 
#movie_data_nopeople <- movie_data[,-5]
fit <- lm(전국매출액~.,data=movie_data_nopeople)
summary(fit)

# backward removal for variables selection
step(fit, direction = 'backward')

# fit again with selected variables
fit <- lm(전국매출액 ~ X15세 + 독립여부 + 메이저배급사 + 메이저제작사 +
                 스타파워 + 전국스크린수 + 상영횟수, data = movie_data_nopeople)
summary(fit)

movie_data$전국관객수 == movie_data$전국매출액

#anova(fit)
#drop1(fit,test = "F")
