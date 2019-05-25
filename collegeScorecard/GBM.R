# Jason Dean
# March 14, 2017
# This script builds a gradient boosted regression model for prediction of student median incomes
# Data is pulled from the SQLite database created by the SQLiteDB.R script.
# Please see the .rmd, .html, or jasontdean.com for additional information and details

# ______Model 1______

# pull earnings data from the database for model building
earnings <- dbSendQuery(college.db, "SELECT HIGHDEG as [highestDegree], STABBR as [state], COSTT4_A as [averageCost], AVGFACSAL as [avgFacultySalary], SAT_AVG as [AverageSAT], MD_EARN_WNE_P10 as [MedianEarnings] FROM collegeData")
earnings  <- fetch(earnings)

# format as numeric
earnings$averageCost <- as.numeric(earnings$averageCost)
earnings$avgFacultySalary <- as.numeric(earnings$avgFacultySalary)
earnings$AverageSAT <- as.numeric(earnings$AverageSAT)
earnings$MedianEarnings <- as.numeric(earnings$MedianEarnings)
earnings$state <- as.factor(earnings$state)
earnings$highestDegree <- as.numeric(earnings$highestDegree)

# convert 'highest degree' number to name
degrees <- data.frame(number = c(0,1,2,3,4), type = c("Non-degree-granting", "Certificate degree", "Associate degree", "Bachelor's degree", "Graduate Degree"))
earnings$highestDegree <- degrees$type[match(earnings$highestDegree, degrees$number)]
earnings$highestDegree <- as.factor(earnings$highestDegree)

# remove missing data
earnings <- earnings[complete.cases(earnings),]

# split data into test and training sets
splitter <- sample(c("train", "test"), nrow(earnings), replace=TRUE, prob=c(0.7,0.3))
earnings.train <- earnings[splitter == 'train',]
earnings.test <- earnings[splitter == 'test',]

# create a grid of gradient boost hyperparameters
grid = expand.grid(.n.trees=seq(1,201, by=20), .interaction.depth=seq(1,4, by=1), .shrinkage=c(.001,.01,.1), .n.minobsinnode=10)

# create control object for training and hyperparameter tune using the training data
control = trainControl(method="LOOCV")
earnings.gbm.train = train(MedianEarnings~., data=earnings.train, method="gbm", trControl=control, tuneGrid=grid)

# create gbm model using parameter values from hypertuning
earnings.gbm = gbm(MedianEarnings~., data=earnings.train, n.trees=141, interaction.depth=2, shrinkage=0.1, distribution="gaussian")

# make predictions on test data and plot the results
earnings.test.predict = predict(earnings.gbm, newdata=earnings.test, n.trees=141)
plot(earnings.test$MedianEarnings, earnings.test.predict, xlim=c(20000,70000), ylim=c(20000,70000), xlab=("MedianEarnings"), ylab=("PredictedEarnings"))
abline(coef=c(0,1))

# calculate RMSE for model1
rmse.1 <- (sum((earnings.test$MedianEarnings - earnings.test.predict)^2)/nrow(earnings.test))^0.5
rmse.1

# calculate R squared
ss.res <- sum((earnings.test$MedianEarnings - earnings.test.predict)^2)
ss.tot <- sum((earnings.test$MedianEarnings - mean(earnings.test$MedianEarnings))^2)
r.squared.1 = 1 - ss.res/ss.tot
r.squared.1


# ______Model 2______

# pull earnings and degree data from the database
earnings <- dbSendQuery(college.db, "SELECT PCIP01, PCIP03, PCIP04, PCIP05, PCIP09, PCIP10, PCIP11, PCIP12, PCIP13, PCIP14, PCIP15, PCIP16, PCIP19, PCIP22, PCIP23, PCIP24, PCIP25, PCIP26, PCIP27, PCIP29, PCIP30, PCIP31, PCIP38, PCIP39, PCIP40, PCIP41, PCIP42, PCIP43, PCIP44, PCIP45, PCIP46, PCIP47, PCIP48, PCIP49, PCIP50, PCIP51, PCIP52, PCIP54, HIGHDEG as [highestDegree], STABBR as [state], COSTT4_A as [averageCost], AVGFACSAL as [avgFacultySalary], SAT_AVG as [AverageSAT], MD_EARN_WNE_P10 as [MedianEarnings] FROM collegeData")
earnings  <- fetch(earnings)

# remove missing data and format as numeric
earnings$averageCost <- as.numeric(earnings$averageCost)
earnings$avgFacultySalary <- as.numeric(earnings$avgFacultySalary)
earnings$AverageSAT <- as.numeric(earnings$AverageSAT)
earnings$MedianEarnings <- as.numeric(earnings$MedianEarnings)
earnings$state <- as.factor(earnings$state)
earnings$highestDegree <- as.numeric(earnings$highestDegree)

# convert 'highest degree' number to name
degrees <- data.frame(number = c(0,1,2,3,4), type = c("Non-degree-granting", "Certificate degree", "Associate degree", "Bachelor's degree", "Graduate Degree"))
earnings$highestDegree <- degrees$type[match(earnings$highestDegree, degrees$number)]
earnings$highestDegree <- as.factor(earnings$highestDegree)

# convert degree percentage data to numeric
earnings[,1:38] <- sapply(earnings[,1:38], as.numeric)

# remove missing data
earnings <- earnings[complete.cases(earnings),]

# split data into test and training sets
splitter <- sample(c("train", "test"), nrow(earnings), replace=TRUE, prob=c(0.7,0.3))
earnings.train <- earnings[splitter == 'train',]
earnings.test <- earnings[splitter == 'test',]

# hyperparameter tuning
grid = expand.grid(.n.trees=seq(1,201, by=20), .interaction.depth=seq(1,4, by=1), .shrinkage=c(.001,.01,.1), .n.minobsinnode=10)
control = trainControl(method="LOOCV")
earnings.gbm.train = train(MedianEarnings~., data=earnings.train, method="gbm", trControl=control, tuneGrid=grid)

# create gbm model using parameter values from hypertuning
earnings.gbm = gbm(MedianEarnings~., data=earnings.train, n.trees=141, interaction.depth=4, shrinkage=0.1, distribution="gaussian")

# make predictions on test data and plot the results
earnings.test.predict = predict(earnings.gbm, newdata=earnings.test, n.trees=141)
plot(earnings.test$MedianEarnings, earnings.test.predict, xlim=c(20000,70000), ylim=c(20000,70000), xlab=("MedianEarnings"), ylab=("PredictedEarnings"))
abline(coef=c(0,1))

# calculate R squared of model 2
ss.res <- sum((earnings.test$MedianEarnings - earnings.test.predict)^2)
ss.tot <- sum((earnings.test$MedianEarnings - mean(earnings.test$MedianEarnings))^2)
r.squared.2 = 1 - ss.res/ss.tot
r.squared.2

# rmse
rmse.2 <- (sum((earnings.test$MedianEarnings - earnings.test.predict)^2)/nrow(earnings.test))^0.5
rmse.2

# ___________Compare Two Models___________

# create table of R-squared and RMSE values for Model 1 and Model 2
results.table <- data.frame(type = c("Model1", "Model2 (+ degree info)"), Rsquared = c(r.squared.1, r.squared.2), RMSE = c(rmse.1, rmse.2))
kable(head(results.table), format="html", align = 'c',digits = 2)
