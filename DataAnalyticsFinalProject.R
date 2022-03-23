## Read in data
setwd("C:\\Users\\macmahoni\\Documents\\School\\Data Analytics\\Final Project")
bm <- read.csv("marathon_results_2017.csv")
head(bm)





## Shows official time data before converting (first - last place)
bm$Official.Time[1]
bm$Official.Time[26410]





## Function converts official time to minutes
time_in_mins <- function(time){
	hr <- substring(time, 1, 1)
	min <- substring(time, 3, 4)
	sec <- substring(time, 6, 7)
	x <- (as.numeric(hr)*60) + as.numeric(min) + (as.numeric(sec)/60)
	return(x)
}
## Demonstrates use of function
time_in_mins(bm$Official.Time[1])
## Use of function to get average time
mean(time_in_mins(bm$Official.Time))





## Function reverts official time from minutes back to 00:00:00
revert_time <- function(time){
	hr <- time %/% 60
	time <- time %% 60
	min <- floor(time)
	time <- time - floor(time)
	sec <- time*60
	x <- sprintf("%02.0f:%02.0f:%02.0f", hr, min, sec)
	return(x)
}
## Demonstrates use of function
revert_time(mean(time_in_mins(bm$Official.Time)))





## Random sample from population
library(dplyr) 
samp <- sample_n(bm, 1000)
head(samp)
mean(time_in_mins(samp$Official.Time))

## Used to write random sample to csv (already have sample written to csv)
##write.csv(samp,"C:\\Users\\macmahoni\\Documents\\School\\Data Analytics\\Final Project\\samp.csv", row.names = FALSE)





## load previously sampled data
samp <- read.csv("samp.csv")
head(samp, 20)





## descriptive statistics of population data
head(bm)

## summary of finish time
(s <- summary(time_in_mins(bm$Official.Time)))
revert_time(s)

## mean pace
(m <- mean(time_in_mins(bm$Pace)))
revert_time(m)

## number of men & women
male <- subset(bm, M.F == 'M')
nrow(male)
female <- subset(bm, M.F == 'F')
nrow(female)

## number of runners from USA
usa <- subset(bm, Country == "USA")
nrow(usa)
## international runners
nrow(bm) - nrow(usa)

## summary of age of runners
summary(bm$Age)





## Sample analysis

## IQR of finish times
(iqr <- IQR(time_in_mins(samp$Official.Time)))

## Number of rows
nrow(samp)

## Average time for age 30
thirty <- subset(samp, samp$Age == 30)
nrow(thirty)
revert_time(mean(time_in_mins(thirty$Official.Time)))

## Ratio of half to full time
half <- time_in_mins(samp$Half)
full <- time_in_mins(samp$Official.Time)
ratio <- half/full
plot(half, ratio)

## Coeffecient of Variation
(cv <- sd(time_in_mins(samp$Official.Time))/mean(time_in_mins(samp$Official.Time)))

## Plot age of runners
hist(samp$Age, main="Age of Runners", xLab="Age", ylim=c(0, 200), xlim=c(0,100), col="yellow")

## Boxplot of time for age 30 compared to age 40
thirty <- subset(samp, samp$Age == 30)
nrow(thirty)
fourty <- subset(samp, samp$Age == 40)
nrow(fourty)
boxplot(time_in_mins(thirty$Official.Time), time_in_mins(fourty$Official.Time), horizontal = TRUE,
notch = TRUE, col = "red", main = "Boxplot", xlab = "Official Time", ylab = "Age")

## Skewness (positive value shows right skewness)
library(e1071)
skewness(time_in_mins(samp$Official.Time))
## Density plot demonstrates this
plot(density(time_in_mins(samp$Official.Time)))



##
## BEGIN TESTS
##





## One sample two sided test
## H0: mu = 238
## H1: mu != 238
## alpha = .05
## P-value approach
alpha <- .05 
(re <- t.test(time_in_mins(samp$Official.Time), mu=238, alt = "two.sided", conf.level = 0.95))
re$p.value < alpha

## REPLICATE P-VALUE
n <- length(time_in_mins(samp$Official.Time))
2*pt(re$statistic, df = n-1, lower.tail=T)

## CV Approach (Crtical value is inside non-rejection region, therefore fail to reject)
##Graph non-rejection region and t statistic
lb.t = qt(0.025, n-1)
ub.t = qt(0.975, n-1)
##plot for critical value approach
plot(function(x) dt(x, n-1),
xlim=c(-4, 4), main=paste("t: Mean =", 0, ", SD_MU =", 1),xlab="X",ylab="")
segments(lb.t, 0, lb.t, dt(lb.t, n-1))
segments(ub.t, 0, ub.t, dt(ub.t, n-1))
cord.x <- c(lb.t,seq(lb.t,ub.t,0.01),ub.t)
cord.y <- c(0,dt(seq(lb.t, ub.t, 0.01), n-1),0)
polygon(cord.x,cord.y,col="yellow")
text(-4, dt(0, n-1), paste("Prob =", round(pt(ub.t, n-1)-pt(lb.t, n-1), 2)), pos = 4)
arrows(re$statistic, 0.2, re$statistic, 0)

## CI Approach (238 falls within the confidence interval, therfore fail to reject)
re$conf.int





## Two sample one sided test
## H0: mu_m <= mu_f (male, female)
## H1: mu_m > mu_f
## alpha = .05
## CI Approach
male <- subset(samp, samp$M.F == 'M')
female <- subset(samp, samp$M.F == 'F')

## Descriptive of subsets
nrow(male)
nrow(female)
sd(time_in_mins(male$Official.Time))
sd(time_in_mins(female$Official.Time))

(re <- t.test(time_in_mins(male$Official.Time), time_in_mins(female$Official.Time), var.equal = TRUE, alt = "greater"))
re$conf.int

##P-value approach (Fail to reject, p-value is not less than alpha)
re$p.value < .05

##REPLICATE P-VALUE 
df <- length(time_in_mins(male$Official.Time) - 1) + length(time_in_mins(female$Official.Time) - 1)
pt(re$statistic, df, lower.tail=FALSE)

##CV approach 
par(mfrow=c(1,1), pty="m")
lb.t = -4
ub.t = qt(0.95, df)

plot(function(x) dt(x, df), xlim=c(-10,4),
main=paste("t: Mean = ", 0, ", SD_MU = ", 1), xlab = "X", ylab = "")
segments(lb.t, 0, lb.t, dt(lb.t, df))
segments(ub.t, 0, ub.t, dt(ub.t, df))
cord.x <- c(lb.t, seq(lb.t, ub.t, 0.01), ub.t)
cord.y <- c(0, dt(seq(lb.t, ub.t, 0.01), df), 0)
polygon(cord.x, cord.y, col = "blue")
text(-4, dt(0, df), paste("Prob = ", round(pt(ub.t, df)-pt(lb.t, df), 2)), pos = 4)
arrows(re$statistic, 0.2, re$statistic, 0) 


## ANOVA test
## H0: mu_y = mu_o = mu_m (young, old, middle age)
## H1: Not H0
## alpha = .05
##young = 18-40, middle = 40-62, old = 62, 84

young <- subset(samp, samp$Age >= 18 & samp$Age < 40)
middle <- subset(samp, samp$Age >= 40 & samp$Age < 62)
old <- subset(samp, samp$Age >= 62 & samp$Age < 84)


##amount in each category
nrow(young)
nrow(middle)
nrow(old)

##adding ID
id <- c(rep(1, 379))
young <- cbind(young, id)

id <- c(rep(2, 575))
middle <- cbind(middle, id)

id <- c(rep(3, 46))
old <- cbind(old, id)


##binding subsets together 
AGES <- rbind(young, middle, old)

##test (Reject, p-value is less than alpha)
AGES$id <- as.factor(AGES$id)
anova(lm(time_in_mins(AGES$Official.Time) ~ AGES$id, AGES))

##which groups are different
pairwise.t.test(time_in_mins(AGES$Official.Time), AGES$id)
tapply(time_in_mins(AGES$Official.Time), AGES$id, mean)
##All three groups are different from each other
