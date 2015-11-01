# Employment and wage estimates
# from http://www.bls.gov/oes/current/oes_nat.htm

data <- read.csv("national_M2014_dl.csv")

tot_emp <- as.vector.factor(data$TOT_EMP[-1])
hour_mean <- as.vector.factor(data$H_MEAN[-1])


# remove commas and asterisks
tot_emp <- gsub(",", "", tot_emp)
hour_mean <- gsub(",", "", hour_mean)
hour_mean <- gsub("\\*", "NA", hour_mean)

tot_emp <- as.numeric(tot_emp)
hour_mean <- as.numeric(hour_mean)

# log transform, b/c much of the hourly mean data is skewed at low employment levels,
# which potentially means employment is low in general
log_tot_emp <- log(tot_emp)


plot(log_tot_emp, hour_mean, pch=16, cex=0.5, xlab="log(total # employed per job)", ylab="hourly mean salary (US $)",
     main="Hourly wage per number of people \n employed per job")
# this plot shows a lot of low-paying jobs and fewer high-paying jobs




## New jobs created per presidential cycle
# from https://en.wikipedia.org/wiki/Jobs_created_during_U.S._presidential_terms#cite_note-2

jobs <- read.csv("jobs made.csv")

jobs$Ave.annual.increase <- gsub("%", "", jobs$Ave.annual.increase)

years <- jobs$Term.years
timesteps <- 1:24
increase <- jobs$Ave.annual.increase


years_2 <- years[-3]
timesteps_2 <- 1:23
increase_2 <- increase[-3]  # remove 1929-1933 of Hoover's term when increase was -5


fit <- lm(increase_2 ~ timesteps_2)  # regression shows job creation decreasing
B0 <- coef(fit)[1]  # intercept = 3.7
B1 <- coef(fit)[2]  # slope = -0.123

start_years <- substring(years_2, 1, 4)
start_years_2 <- start_years[seq(1, length(start_years), 2)]  # every other year

plot.default(timesteps_2, increase_2, xaxt="n", xlab="Start of Term Year", ylab="% Increase", main="Job Growth",
             pch=16)
axis(side=1, at=seq(1, 23, by=2), labels=start_years_2, las=2, cex.axis=0.8)  # make own axis
abline(B0, B1)



# plot including very low decrease during Hoover's term
fit2 <- lm(increase ~ timesteps)
plot(timesteps, increase, xlab="Time Steps", ylab="% increase", main="Job Growth", pch=16)
abline(coef(fit2)[1], coef(fit2)[2])
