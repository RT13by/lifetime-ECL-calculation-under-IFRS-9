# Ввод исходных данных (темпа прироста реального ВВП (GDPr), PIT PD в разрезе групп кредитного рейтинга (Х1…Х9)) 
# в переменную (input) посредством R ,приведения их в пригодный
# для обработки вид и проверки их корректности
# I:
input <-read.csv("input_data.csv")
library(dplyr)
dplyr::glimpse(input)

# Для визуальной оценки целесообразно построение графиков переменных 
#(уровня реального ВВП и PIT PD на примере 9 группы кредитного рейтинга)
# II:
dates<-seq(as.Date("2016/1/1"), as.Date("2018/10/1"), by = "quarter")
plot(dates, input$X9, type="o", main="DR 9 group",
ylab="Default rate",xlab="Time", col="blue")
plot(dates, input$GDPr, type="o", main="GDP growth rate",
ylab="Percent",xlab="Time", col="blue")

# Построение гистограмм временных рядов уровня реального  ВВП и PIT PD на примере 9 группы кредитного рейтинга
# III:
library(ggplot2)
hist.adverts <-ggplot(input, aes(input$X9))+
geom_histogram(aes (y = ..density..))+theme_bw()+
labs(x = "DR 9", y = "Плотность")
hist.adverts
hist.adverts + stat_function(fun = dnorm, args = list(mean = mean(input$X9, na.rm = TRUE),
sd = sd(input$X9, na.rm = TRUE)), color  = 'red')

hist.adverts <-ggplot(input, aes(input$GDPr))+
geom_histogram(aes (y = ..density..))+theme_bw()+
labs(x = "GDPr", y = "Плотность")
hist.adverts
hist.adverts + stat_function(fun = dnorm, args = list(mean = mean(input$GDPr, na.rm = TRUE),
sd = sd(input$GDPr, na.rm = TRUE)), color  = 'red')


# KPSS-тест
# IV:
library(tseries)
kpss.test(input$X9, null = "Trend", lshort = TRUE)
kpss.test(input$GDPr, null = "Trend", lshort = TRUE)

# Расчет линейной регрессии осуществлен на примере 9 группы кредитного рейтинга
# V:
fit <-lm(X9~GDPr, data=input)
summary(fit)
confint(fit)

# Оценка допущений метода наименьших квадратов 
# VI:
library(gvlma)
summary(gvlma(fit))

# Тест Дарбина-Уотсона
# VII:
library(car) 
durbinWatsonTest(fit)

# Тест Бройша-Годфри
# VIII:
library(lmtest)
bg3 <-bgtest(fit, order = 3)
bg3
bg4 <-bgtest(fit, order = 4)
bg4

# Построение графиков АКФ и ПАКФ
# IX:
res <- residuals(fit)
Res
acf(res) 
pacf(res)

