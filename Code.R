#  data input  (growth rate of real GDP (GDPr), PIT PD in the groups of credit rating (X1 ... X9))
#  в into a variable (input) to convert  it into a form suitable for processing and checking  correctness 
# I:
input <-read.csv("input_data.csv")
library(dplyr)
dplyr::glimpse(input)

# To assess visually, it is advisable to plot graphs of variables
# (growth rate of real GDP (GDPr), PIT PD for 9 group of credit rating as an example)
# II:
dates<-seq(as.Date("2016/1/1"), as.Date("2018/10/1"), by = "quarter")
plot(dates, input$X9, type="o", main="DR 9 group",
ylab="Default rate",xlab="Time", col="blue")
plot(dates, input$GDPr, type="o", main="GDP growth rate",
ylab="Percent",xlab="Time", col="blue")

# Construction of histograms of time series (growth rate of real GDP (GDPr), PIT PD for 9 group of credit rating as an example) 
# III:
library(ggplot2)
hist.adverts <-ggplot(input, aes(input$X9))+
geom_histogram(aes (y = ..density..))+theme_bw()+
labs(x = "DR 9", y = "Density")
hist.adverts
hist.adverts + stat_function(fun = dnorm, args = list(mean = mean(input$X9, na.rm = TRUE),
sd = sd(input$X9, na.rm = TRUE)), color  = 'red')

hist.adverts <-ggplot(input, aes(input$GDPr))+
geom_histogram(aes (y = ..density..))+theme_bw()+
labs(x = "GDPr", y = "Density")
hist.adverts
hist.adverts + stat_function(fun = dnorm, args = list(mean = mean(input$GDPr, na.rm = TRUE),
sd = sd(input$GDPr, na.rm = TRUE)), color  = 'red')


# KPSS-testing
# IV:
library(tseries)
kpss.test(input$X9, null = "Trend", lshort = TRUE)
kpss.test(input$GDPr, null = "Trend", lshort = TRUE)

# The construction of linear regression model (growth rate of real GDP (GDPr), PIT PD for 9 group of credit rating as an example) 
# V:
fit <-lm(X9~GDPr, data=input)
summary(fit)
confint(fit)

# Least Squares Assumptions check
# VI:
library(gvlma)
summary(gvlma(fit))

# Durbin-Watson test
# VII:
library(car) 
durbinWatsonTest(fit)

# Breusch-Godfrey test
# VIII:
library(lmtest)
bg3 <-bgtest(fit, order = 3)
bg3
bg4 <-bgtest(fit, order = 4)
bg4

# ACF and PACF calculation and testing
# IX:
res <- residuals(fit)
Res
acf(res) 
pacf(res)

