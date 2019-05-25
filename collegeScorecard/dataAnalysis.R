# Jason Dean
# March 14, 2017
# This script runs exploratory data analysis of the college scorecard data from 2015-2016.
# Note that data is pulled from the database created via SQLiteDB.R
# Please refer to the .rmd file in this repository or jasontdean.com for more information

# _____COST ANALYSIS_____

# pull college cost and location data from the db and convert to numeric
costLoc <- dbSendQuery(college.db, "SELECT COSTT4_A, LATITUDE, LONGITUDE FROM collegeData")
costLoc <- fetch(costLoc)
title <- c('AverageCost', 'LATITUDE', 'LONGITUDE')
colnames(costLoc) <- title
costLoc$AverageCost <- as.numeric(costLoc$AverageCost)
costLoc$LATITUDE <- as.numeric(costLoc$LATITUDE)
costLoc$LONGITUDE <- as.numeric(costLoc$LONGITUDE)

# get a map of the USA from gogle
map<-get_map(location='united states', zoom=4, maptype = "terrain", source='google',color='color')

# plot the lat/long institution data over the google map
ggmap(map, extent="device") + geom_point(aes(x=LONGITUDE, y=LATITUDE), data=costLoc, alpha=.25, na.rm = T, col="dodgerblue3") 

# pull average cost and name data from the db
costNames <- dbSendQuery(college.db, "SELECT COSTT4_A as [AverageCost], INSTNM as [Name] FROM collegeData")
costNames <- fetch(costNames)
costNames$AverageCost <- as.numeric(costNames$AverageCost)

# plot the name and cost data for the top ten most expensive schools
costNames <- costNames[order(costNames$AverageCost, decreasing = TRUE),]
pricey <- costNames[1:10,]
pricey$Name <- factor(pricey$Name, levels=pricey$Name)
ggplot(pricey, aes(x=Name, y=AverageCost)) + geom_bar(stat = "identity", fill='lightblue', colour='black') + coord_flip() + ggtitle("Top Ten Most Expensive Schools") + xlab('')

# plot the name and cost data for the top ten least expensive schools
costNames <- costNames[order(costNames$AverageCost, decreasing = FALSE),]
notpricey <- costNames[1:10,]
notpricey$Name <- factor(notpricey$Name, levels=notpricey$Name)
ggplot(notpricey, aes(x=Name, y=AverageCost)) + geom_bar(stat = "identity", fill='lightblue', colour='black') + coord_flip() + ggtitle("Top Ten Cheapest Schools") + xlab('')

# pull average cost and state data from the db
costState <- dbSendQuery(college.db, "SELECT AVG(COSTT4_A), STABBR FROM collegeData GROUP BY STABBR")
costState <- fetch(costState)
costState <- costState[order(costState$`AVG(COSTT4_A)`),]

# summary statistics for the average cost of attendence
summary(costState$`AVG(COSTT4_A)`)

# convert state abbreviations to full names and plot the average cost over the google USA map
st.codes<-data.frame(state=as.factor(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA","HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS",  "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")), full=as.factor(c("alaska","alabama","arkansas","arizona","california","colorado", "connecticut","district of columbia","delaware","florida","georgia", "hawaii", "iowa","idaho", "illinois", "indiana", "kansas", "kentucky", "louisiana","massachusetts","maryland","maine","michigan","minnesota","missouri","mississippi","montana","north carolina","north dakota", "nebraska","new hampshire","new jersey", "new mexico","nevada", "new york","ohio","oklahoma","oregon","pennsylvania","puerto rico", "rhode island","south carolina","south dakota","tennessee","texas",  "utah",  "virginia", "vermont", "washington", "wisconsin", "west virginia", "wyoming")))

costState$region <- st.codes$full[match(costState$STABBR, st.codes$state)]
states <- map_data("state")
map.df <- merge(states,costState, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]
map.df$AverageCost <- map.df$`AVG(COSTT4_A)`

ggplot(map.df, aes(x=long,y=lat,group=group)) + geom_polygon(aes(fill=AverageCost)) + geom_path()+ scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90") + coord_map()

# pull cost, admission rate, average faculty salary, and average incoming SAT score from the DB
costAux <- dbSendQuery(college.db, "SELECT COSTT4_A, ADM_RATE, AVGFACSAL, SAT_AVG FROM collegeData")
costAux  <- fetch(costAux)
title <- c("AverageCost", "AdmissionRate", "FacultySalary", "AverageSAT")
colnames(costAux) <- title

# remove NULL and NA containing entries and convert to numerics
costAux <- costAux[costAux$AverageCost != 'NULL',]
costAux <- costAux[costAux$AverageCost != 'NA',]
costAux <- costAux[costAux$FacultySalary != 'NULL',]
costAux <- costAux[costAux$FacultySalary != 'NA',]
costAux <- costAux[costAux$AdmissionRate != 'NULL',]
costAux <- costAux[costAux$AdmissionRate != 'NA',]
costAux <- costAux[costAux$AverageSAT != 'NULL',]
costAux <- costAux[costAux$AverageSAT != 'NA',]

costAux$AverageCost <- as.numeric(costAux$AverageCost)
costAux$FacultySalary <- as.numeric(costAux$FacultySalary)
costAux$AdmissionRate <- as.numeric(costAux$AdmissionRate)
costAux$AverageSAT <- as.numeric(costAux$AverageSAT)

# plot average cost of attendance vs. average faculty salary
ggplot(costAux, aes(x=AverageCost, y=FacultySalary)) + geom_point(alpha=0.5) + theme_bw() + geom_smooth(method=lm, se=FALSE) + ggtitle("Faculty Salary vs. Average Cost")

# plot average cost of attendance vs. admission rate
ggplot(costAux, aes(x=AverageCost, y=AdmissionRate)) + geom_point(alpha=0.5) + theme_bw() + geom_smooth(method=lm, se=FALSE) + ggtitle("Admission Rate vs. Average Cost")

# plot average SAT score vs. admission rate
ggplot(costAux, aes(x=AverageCost, y=AverageSAT)) + geom_point(alpha=0.5) + theme_bw() + geom_smooth(method=lm, se=FALSE) + ggtitle("Average SAT vs. Average Cost")

# _____DEBT ANALYSIS_____

# pull debt data from collegeData database
debtName <- dbSendQuery(college.db, "SELECT GRAD_DEBT_MDN as [completeDebt], INSTNM as [Name] FROM collegeData")
debtName <- fetch(debtName)
debtName$completeDebt <- as.numeric(debtName$completeDebt)

# plot the schools with the highest associated debt
debtName <- debtName[order(debtName$completeDebt, decreasing = TRUE),]
pricey <- debtName[1:10,]
pricey$Name <- factor(pricey$Name, levels=pricey$Name)
ggplot(pricey, aes(x=Name, y=completeDebt)) + geom_bar(stat = "identity", fill='lightblue', colour='black') + coord_flip() + ggtitle("Top Ten Schools For Most Debt") + xlab('')

# pull debt data from collegeData database, remove missing values, and convert to numeric
debtLoc <- dbSendQuery(college.db, "SELECT GRAD_DEBT_MDN, WDRAW_DEBT_MDN FROM collegeData")
debtLoc <- fetch(debtLoc)
title <- c('completeDebt', 'wdrawDebt')
colnames(debtLoc) <- title
debtLoc$completeDebt <- as.numeric(debtLoc$completeDebt)
debtLoc$wdrawDebt <- as.numeric(debtLoc$wdrawDebt)
debtLoc <- debtLoc[!is.na(debtLoc$completeDebt),]
debtLoc <- debtLoc[!is.na(debtLoc$wdrawDebt),]
summary(debtLoc)

# format data for plotting and plot debt of those that withdraw and those that complete their education
finishDebt <- as.data.frame(debtLoc$completeDebt)
finishDebt$type <- 'complete'
withdrawDebt <- as.data.frame(debtLoc$wdrawDebt)
withdrawDebt$type <- 'withdraw'
names <- c('debt', 'type')
colnames(finishDebt) <- names
colnames(withdrawDebt) <- names
debt <- rbind(withdrawDebt, finishDebt)
ggplot(debt, aes(debt, fill=type)) + geom_density(alpha=0.5, aes(y=..scaled..))

# pull data and intstitution type from collegeData database, convert to numeric, and remove missing data
debtPP <- dbSendQuery(college.db, "SELECT GRAD_DEBT_MDN, WDRAW_DEBT_MDN, CONTROL FROM collegeData")
debtPP <- fetch(debtPP)
title <- c('completeDebt', 'wdrawDebt', 'prvtPublic')
colnames(debtPP) <- title
debtPP$completeDebt <- as.numeric(debtPP$completeDebt)
debtPP$wdrawDebt <- as.numeric(debtPP$wdrawDebt)
debtPP <- debtPP[!is.na(debtPP$completeDebt),]
debtPP <- debtPP[!is.na(debtPP$wdrawDebt),]
debtPP <- debtPP[!is.na(debtPP$prvtPublic),]

# calculate the mean debt for the different type of institutions
names <- data.frame(num = c(1,2,3), type = c("public", "private nonprofit", "private for-profit"))
debtPP$prvtPublic <- as.numeric(debtPP$prvtPublic)
debtPP$type <- names$type[match(debtPP$prvtPublic, names$num)]
tabl <- debtPP %>% group_by(type) %>% summarise(completeDebt = mean(completeDebt), wdrawDebt = mean(wdrawDebt))
kable(tabl, format="html", align = 'c')

# create two separate dfs - one for containing data for private non-profit schools and one for the other two types
names$num <- c("public / private for-profit", "private nonprofit", "public / private for-profit")
debtPP$type <- names$num[match(debtPP$type, names$type)]
debtPP.ppfp <- debtPP %>% filter(debtPP$type == 'public / private for-profit')
debtPP.pnp <- debtPP %>% filter(debtPP$type == 'private nonprofit')

# format data for plotting for public / private for-profit debt and plot
finishDebt.ppfp <- as.data.frame(debtPP.ppfp$completeDebt)
finishDebt.ppfp$type <- 'complete'
withdrawDebt.ppfp <- as.data.frame(debtPP.ppfp$wdrawDebt)
withdrawDebt.ppfp$type <- 'withdraw'
names <- c('debt', 'type')
colnames(finishDebt.ppfp) <- names
colnames(withdrawDebt.ppfp) <- names
debt.ppfp <- rbind(finishDebt.ppfp, withdrawDebt.ppfp)
ggplot(debt.ppfp, aes(debt, fill=type)) + geom_density(alpha=0.5, aes(y=..scaled..)) + ggtitle('Debt:  Public and Private for-profit Institutions')

# format data for plotting for Private non-profit debt and plot
finishDebt.pnp <- as.data.frame(debtPP.pnp$completeDebt)
finishDebt.pnp$type <- 'complete'
withdrawDebt.pnp <- as.data.frame(debtPP.pnp$wdrawDebt)
withdrawDebt.pnp$type <- 'withdraw'
names <- c('debt', 'type')
colnames(finishDebt.pnp) <- names
colnames(withdrawDebt.pnp) <- names
debt.pnp <- rbind(finishDebt.pnp, withdrawDebt.pnp)
ggplot(debt.pnp, aes(debt, fill=type)) + geom_density(alpha=0.5, aes(y=..scaled..)) + ggtitle('Debt:  Private non-profit')

# pull debt and cost data from collegeData database
debtCost <- dbSendQuery(college.db, "SELECT GRAD_DEBT_MDN, COSTT4_A FROM collegeData")
debtCost <- fetch(debtCost)
title <- c('completeDebt', 'AverageCost')
colnames(debtCost) <- title
debtCost$completeDebt <- as.numeric(debtCost$completeDebt)
debtCost$AverageCost <- as.numeric(debtCost$AverageCost)
debtCost <- debtCost[!is.na(debtCost$completeDebt),]
debtCost <- debtCost[!is.na(debtCost$AverageCost),]

# plot completed debt vs cost
ggplot(debtCost, aes(x=AverageCost, y=completeDebt)) + geom_point(alpha=0.5) + theme_bw() + geom_smooth(method=lm, se=FALSE)

# _____FUTURE EARNINGS ANALYSIS_____
# pull median earnings and name data from collegeData database
earnName <- dbSendQuery(college.db, "SELECT MD_EARN_WNE_P10 as [medianEarnings], INSTNM as [Name] FROM collegeData")
earnName <- fetch(earnName)
earnName$medianEarnings <- as.numeric(earnName$medianEarnings)

# plot earnings data for those that completed their education
earnName <- earnName[order(earnName$medianEarnings, decreasing = TRUE),]
rich <- earnName[1:10,]
rich$Name <- factor(rich$Name, levels=rich$Name)
ggplot(rich, aes(x=Name, y=medianEarnings)) + geom_bar(stat = "identity", fill='lightblue', colour='black') + coord_flip() + ggtitle("Top Ten Highest 10yr Median Earnings") + xlab('')

# pull median earnings and state data from the db
earnState <- dbSendQuery(college.db, "SELECT AVG(MD_EARN_WNE_P10), STABBR FROM collegeData GROUP BY STABBR")
earnState <- fetch(earnState)
title <- c('medianEarnings', 'State')
colnames(earnState) <- title
earnState$medianEarnings <- as.numeric(earnState$medianEarnings)

# convert state abbreviations to full names and plot the median earnings over the google USA map
earnState$region <- st.codes$full[match(earnState$State, st.codes$state)]
states <- map_data("state")
map.df <- merge(states,earnState, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]

ggplot(map.df, aes(x=long,y=lat,group=group)) + geom_polygon(aes(fill=medianEarnings)) + geom_path()+ scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90") + coord_map()

# pull median earnings and cost data from the db
earnState <- dbSendQuery(college.db, "SELECT MD_EARN_WNE_P10, COSTT4_A FROM collegeData")
earnState <- fetch(earnState)
title <- c('medianEarnings', 'AverageCost')
colnames(earnState) <- title
earnState$medianEarnings <- as.numeric(earnState$medianEarnings)
earnState$AverageCost <- as.numeric(earnState$AverageCost)
earnState <- earnState[!is.na(earnState$medianEarnings),]
earnState <- earnState[!is.na(earnState$AverageCost),]

# plot the median earnings vs. the cost of attending
ggplot(earnState, aes(x=AverageCost, y=medianEarnings)) + geom_point(alpha=0.5) + theme_bw() + geom_smooth(method=lm, SE=TRUE) + ylim(0,75000)

# create a linear regression model of future earnings and average cost
earnings <- lm(medianEarnings ~ AverageCost, data=earnState)

# plot residuals of the linear regression model
hist(earnings$residuals, breaks=50)
qqnorm(earnings$residuals)
qqline(earnings$residuals)

# create a linear regression of log(median earnings) vs cost of attendance 
earnState$log <- log(earnState$medianEarnings)
earnings <- lm(log ~ AverageCost, data=earnState)

# plot residuals
hist(earnings$residuals, breaks=50)
qqnorm(earnings$residuals)
qqline(earnings$residuals)

# summary of linear model
summary(earnings)

# pull median earnings and highest offered degree data and degree from the db
earnDegree <- dbSendQuery(college.db, "SELECT AVG(MD_EARN_WNE_P10) as [medianEarnings], HIGHDEG FROM collegeData GROUP BY HIGHDEG")
earnDegree <- fetch(earnDegree)
earnDegree$type <- c("Non-degree-granting", "Certificate degree", "Associate degree", "Bachelor's degree", "Graduate Degree")
earnDegree$medianEarnings <- as.numeric(earnDegree$medianEarnings)
earnDegree$type <- factor(earnDegree$type, levels=earnDegree$type)

# plot average median earnings grouped by highest offered degree
ggplot(data=earnDegree, aes(x=type, y=medianEarnings)) + geom_bar(stat = "identity", fill='lightblue', colour='black') + xlab("Degree Type") + ylab("Average Median Earnings") + theme_bw()

# pull median earnings and completeDebt from the db
earnDebt <- dbSendQuery(college.db, "SELECT MD_EARN_WNE_P10 as [medianEarnings], GRAD_DEBT_MDN as [completeDebt] FROM collegeData")
earnDebt <- fetch(earnDebt)
earnDebt$medianEarnings <- as.numeric(earnDebt$medianEarnings)
earnDebt$completeDebt <- as.numeric(earnDebt$completeDebt)
earnDebt <- earnDebt[!is.na(earnDebt$medianEarnings),]
earnDebt <- earnDebt[!is.na(earnDebt$completeDebt),]
earnDebt$log <- log(earnDebt$medianEarnings)

# plot the median earnings vs. complete debt
ggplot(earnDebt, aes(x=completeDebt, y=log)) + geom_point(alpha=0.5) + theme_bw() + geom_smooth(method=lm, SE=FALSE) + ylab("log Median Earnings")

# evaluate linear relationship between log(medianEarnings) and completeDebt
fit <- lm(log ~ completeDebt, earnDebt)
hist(fit$residuals,breaks=50)
qqnorm(fit$residuals)
qqline(fit$residuals)
summary(fit)

# pull admission rate, average SAT score, median wage, and average faculty salary from the DB
costSmarts <- dbSendQuery(college.db, "SELECT ADM_RATE as [AdmissionRate], SAT_AVG as [AverageSAT], MD_EARN_WNE_P10 as [MedianEarnings], AVGFACSAL as [avgFacultySalary] FROM collegeData")
costSmarts  <- fetch(costSmarts)

# remove NULL and NA containing entries and convert to numeric
costSmarts <- costSmarts[costSmarts$AdmissionRate != 'NULL',]
costSmarts <- costSmarts[costSmarts$AdmissionRate != 'NA',]
costSmarts <- costSmarts[costSmarts$AverageSAT != 'NULL',]
costSmarts <- costSmarts[costSmarts$AverageSAT != 'NA',]
costSmarts <- costSmarts[costSmarts$MedianEarnings != 'NULL',]
costSmarts <- costSmarts[costSmarts$MedianEarnings != 'NA',]
costSmarts <- costSmarts[costSmarts$avgFacultySalary != 'NULL',]
costSmarts <- costSmarts[costSmarts$avgFacultySalary != 'NA',]

costSmarts$AdmissionRate <- as.numeric(costSmarts$AdmissionRate)
costSmarts$MedianEarnings <- as.numeric(costSmarts$MedianEarnings)
costSmarts$AverageSAT <- as.numeric(costSmarts$AverageSAT)
costSmarts$avgFacultySalary <- as.numeric(costSmarts$avgFacultySalary)

# plot the data
ggplot(costSmarts, aes(x=MedianEarnings, y=AdmissionRate)) + geom_point(alpha=0.5) + theme_bw() + geom_smooth(method=lm, SE=FALSE) + ggtitle('Admission Rate vs. Median Earnings')

ggplot(costSmarts, aes(x=MedianEarnings, y=AverageSAT)) + geom_point(alpha=0.5) + theme_bw() + geom_smooth(method=lm, SE=FALSE) + ggtitle('Average SAT vs. Median Earnings')

ggplot(costSmarts, aes(x=MedianEarnings, y=avgFacultySalary)) + geom_point(alpha=0.5) + theme_bw() + geom_smooth(method=lm, SE=FALSE) + ggtitle('Average Faculty Salary vs. Median Earnings')
