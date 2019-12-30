# load the library
library(tidyverse)
library(fitdistrplus)

# load the data
#This is a comment
suicides <- read.csv('master.csv', header = TRUE, sep = ',')
suicides

#Question
#CI of mean number of male suicides in the year 1988
#Number of suicides committed by male in the year 1988
suicides_1988_male <- filter(suicides, suicides$year == 1988 & suicides$sex == 'male')
suicide_male <- suicides_1988_male %>%
  dplyr::select(country, suicides.100k.pop)%>%
  group_by(country) %>%
  summarise(Total_suicides = round(sum(suicides.100k.pop)))

#Plotting histogram 
ggplot(suicide_male, aes(x= suicide_male$Total_suicides)) + geom_histogram(color = "black", fill = 'steelblue')
fitdist_n <- fitdist(suicide_male$Total_suicides, "norm")
summary(fitdist_n)

#The Cullen and Frey graph
descdist(suicide_male$Total_suicides)
par(mfrow=c(2,2))
plot.legend <- c("normal") 
fit_n <- fitdist(suicide_male$Total_suicides, "norm") 
summary(fit_n)
denscomp(list(fit_n), legendtext = plot.legend, xlab = 'Total suicides') 
cdfcomp (list(fit_n), legendtext = plot.legend, xlab = 'Total suicides') 
qqcomp (list(fit_n), legendtext = plot.legend, xlab = 'Total suicides') 
ppcomp (list(fit_n), legendtext = plot.legend, xlab = 'Total suicides')


#Performing t test to test the hypothesis
pop_mean <- mean(suicide_male$Total_suicides)
suicide_male_sample <- sample_n(suicide_male, 15)
Tcalc <- stats::t.test(suicide_male_sample$Total_suicides, mu = pop_mean, alternative = "two.sided")
tvalue <- qt(0.025 , 14)

m = 0
std = 1

funcShaded <- function(x, lower_bound, upper_bound) {
  y = dnorm(x, mean = m, sd = std)
  y[x > lower_bound & x < upper_bound] <- NA
  return(y)
}

ggplot(data.frame(x = c(m - (5*std), m + (5*std))), aes(x = x)) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = std)) + 
  stat_function(fun = funcShaded, args = list(lower_bound = tvalue, upper_bound = -tvalue), 
                geom = "area", fill = "red", alpha = .2) + 
  scale_x_continuous(breaks = c(m - (3*std), m + (3*std), m, tvalue, -tvalue))+
  theme_minimal()+ theme(axis.text.y = element_blank())

#Drawing the graph
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Rejection Region of a Two-Sided Test',
      yaxs = 'i',
      xlab = 't-statistic',
      ylab = '',
      lwd = 2,
      axes = 'F')


# add x-axis
axis(1, 
     at = c(-4, 0, tvalue,Tcalc["statistic"],0, -tvalue, 4), 
     padj = 0.5,
     labels = c('-4', 0, "T0.025", "Tcalc", '0', "T0.025", 4)
)

# shade rejection region in left tail
polygon(x = c(-4, seq(-4, tvalue, 0.01), tvalue),
        y = c(0, dnorm(seq(-4, tvalue, 0.01)), 0), 
        col = 'darkred')

polygon(x = c(-tvalue, seq(-tvalue, 4, 0.01), 4),
        y = c(0, dnorm(seq(-tvalue, 4, 0.01)), 0), 
        col = 'darkred')

#Question 2

#Confidence Interval for the mean number of suicides in the year 1988
sample_mean <- mean(suicide_male_sample$Total_suicides)
sample_sd <- sd(suicide_male_sample$Total_suicides)
n <- length(suicide_male_sample$Total_suicides)
e <- qt(0.975, n-1) * (sample_sd/sqrt(n))
lower_limit <- sample_mean - e
upper_limit <- sample_mean + e
lower_limit 
upper_limit
tv <- qt(0.975, n-1) 

m = 99.866
std = 25

funcShaded <- function(x, lower_bound, upper_bound) {
  y = dnorm(x, mean = m, sd = std)
  y[x > lower_bound & x < upper_bound] <- NA
  return(y)
}

ggplot(data.frame(x = c(m - (5*std), m + (5*std))), aes(x = x)) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = std)) + 
  stat_function(fun = funcShaded, args = list(lower_bound = 59.86064, upper_bound = 139.8727), 
                geom = "area", fill = "red", alpha = .2) + 
  scale_x_continuous(breaks = c(m - (3*std), m + (3*std), m, 59.86064, 139.8727))





#QUESTION 3

#Difference in proportion 75+ years between male and female suicides in year 2007
#Male suicides in year 2000 to 2016
suicides_2000s_male <- filter(suicides, suicides$year >= 2000 & suicides$year <= 2016 & suicides$sex == 'male')
#Female suicides in year 2000 to 2016
suicides_2000s_female <- filter(suicides, suicides$year >= 2000 & suicides$year <= 2016 & suicides$sex == 'female')


#Making it into a normal distribution
suicide_male_dist <- suicides_2000s_male %>%
  dplyr::select(country, age, suicides.100k.pop) %>%
  group_by(country, age) %>%
  summarise(Total_suicides = round(sum(suicides.100k.pop)))%>% 
  filter(Total_suicides > 300 & Total_suicides < 500)

ggplot(suicide_male_dist, aes(x= suicide_male_dist$Total_suicides)) + geom_histogram(color = "black", fill = 'steelblue')
descdist(suicide_male_dist$Total_suicides)
par(mfrow=c(1,1))
plot.legend <- c("normal") 
fit_norm_m <- fitdist(suicide_male_dist$Total_suicides, "norm") 
summary(fit_norm_m)
denscomp(list(fit_norm_m), legendtext = plot.legend, xlab = 'Total suicides', xlegend = 'topleft') 
cdfcomp (list(fit_norm_m), legendtext = plot.legend, xlab = 'Total suicides') 
qqcomp (list(fit_norm_m), legendtext = plot.legend, xlab = 'Total suicides') 
ppcomp (list(fit_norm_m), legendtext = plot.legend, xlab = 'Total suicides')


#Making female data into normal distribution
suicide_female_dist <- suicides_2000s_female %>%
  dplyr::select(country, age, suicides.100k.pop) %>%
  group_by(country, age) %>%
  summarise(Total_suicides = round(sum(suicides.100k.pop)))%>%
  filter(Total_suicides>100 & Total_suicides < 200)

ggplot(suicide_female_dist, aes(x= suicide_female_dist$Total_suicides)) + geom_histogram(color = "black", fill = 'steelblue')
descdist(suicide_female_dist$Total_suicides)
fit_norm_f <- fitdist(suicide_female_dist$Total_suicides, "norm") 
summary(fit_norm)
denscomp(list(fit_norm_f), legendtext = plot.legend, xlab = 'Total suicides', xlegend = 'topleft') 
cdfcomp (list(fit_norm_f), legendtext = plot.legend, xlab = 'Total suicides') 
qqcomp (list(fit_norm_f), legendtext = plot.legend, xlab = 'Total suicides') 
ppcomp (list(fit_norm_f), legendtext = plot.legend, xlab = 'Total suicides')

#p1 = Total number of female suicides age 75+/Total number of female suicides
sample1 <- sample(nrow(suicide_female_dist), size = 15)
sample_data_female <- suicide_female_dist[sample1,]
plus_75_female <- sample_data_female %>%
  filter(age == '75+ years')
p1 <- sum(plus_75_female$Total_suicides)/sum(sample_data_female$Total_suicides)


#p2 = Total number of male suicides age 75+/Total number of male suicides
sample2 <- sample(nrow(suicide_male_dist), size = 15)
sample_data_male <- suicide_male_dist[sample2,]
plus_75_male <- sample_data_male %>%
  filter(age == '75+ years')
p2 <- sum(plus_75_male$Total_suicides)/sum(sample_data_male$Total_suicides)

df = nrow(sample_data_male) + nrow(sample_data_female) - 2
q2 <- 1 - p1
q1 <- 1 - p2
error <- qt(0.975, df = df) * sqrt(((p1*q1)/nrow(sample_data_female)) + ((p2*q2)/nrow(sample_data_male)))
left <- (p1 - p2) - error
right <- (p1-p2) + error

left
right

tvalue1 <- qt(0.10, df = df)

#Drawing the graph
curve(dnorm(x),
      xlim = c(-4, 4),
      main = 'Rejection Region of a Two-Sided Test',
      yaxs = 'i',
      xlab = 't-statistic',
      ylab = '',
      lwd = 2,
      axes = 'F')


# add x-axis
axis(1, 
     at = c(-4, tvalue1,-0.210, -tvalue1, 4), 
     padj = 0.5,
     labels = c('',"-0.0231" ,"-0.210", "-0.398",'')
)

# shade rejection region in left tail
polygon(x = c(-4, seq(-4, tvalue1, 0.01), tvalue1),
        y = c(0, dnorm(seq(-4, tvalue1, 0.01)), 0), 
        col = 'darkred')

polygon(x = c(-tvalue1, seq(-tvalue1, 4, 0.01), 4),
        y = c(0, dnorm(seq(-tvalue1, 4, 0.01)), 0), 
        col = 'darkred')


#QUESTION 4 

#CI of difference of means Unequal variances
sample_suicide_female <- sample(nrow(suicide_female_dist), size = 15)
sample_suicide_male <- sample(nrow(suicide_male_dist), size = 15)
ssf_data <- suicide_female_dist[sample_suicide_female,]
ssm_data <- suicide_male_dist[sample_suicide_male,]
ssf_mean <- mean(sample_suicide_female)
ssm_mean <- mean(sample_suicide_male)
ssf_sd<- sd(sample_suicide_female)
ssm_sd<- sd(sample_suicide_male)
err <- qt(0.975, 14) * sqrt((ssf_sd^2/15)+(ssm_sd^2/15))
mean_diff <- ssf_mean - ssm_mean
lower_lim <- mean_diff - err
upper_lim <- mean_diff + err
lower_lim
upper_lim


m = -8.533
std = 10

funcShaded <- function(x, lower_bound, upper_bound) {
  y = dnorm(x, mean = m, sd = std)
  y[x > lower_bound & x < upper_bound] <- NA
  return(y)
}

ggplot(data.frame(x = c(m - (5*std), m + (5*std))), aes(x = x)) + 
  stat_function(fun = dnorm, args = list(mean = m, sd = std)) + 
  stat_function(fun = funcShaded, args = list(lower_bound = -27.941, upper_bound = 10.874), 
                geom = "area", fill = "red", alpha = .2) + 
  scale_x_continuous(breaks = c(m - (3*std), m + (3*std), m, -27.941, 10.874))+
  theme_minimal()+ theme(axis.text.y = element_blank())

dev.off()



