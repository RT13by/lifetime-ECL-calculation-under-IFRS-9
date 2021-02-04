# Ââîä èñõîäíûõ äàííûõ (òåìïà ïðèðîñòà ðåàëüíîãî ÂÂÏ (GDPr), PIT PD â ðàçðåçå ãðóïï êðåäèòíîãî ðåéòèíãà (Õ1…Õ9)) 
# â ïåðåìåííóþ (input) ïîñðåäñòâîì R ,ïðèâåäåíèÿ èõ â ïðèãîäíûé
# äëÿ îáðàáîòêè âèä è ïðîâåðêè èõ êîððåêòíîñòè
# I:
input <-read.csv("input_data.csv")
library(dplyr)
dplyr::glimpse(input)

# Äëÿ âèçóàëüíîé îöåíêè öåëåñîîáðàçíî ïîñòðîåíèå ãðàôèêîâ ïåðåìåííûõ 
#(óðîâíÿ ðåàëüíîãî ÂÂÏ è PIT PD íà ïðèìåðå 9 ãðóïïû êðåäèòíîãî ðåéòèíãà)
# II:
dates<-seq(as.Date("2016/1/1"), as.Date("2018/10/1"), by = "quarter")
plot(dates, input$X9, type="o", main="DR 9 group",
ylab="Default rate",xlab="Time", col="blue")
plot(dates, input$GDPr, type="o", main="GDP growth rate",
ylab="Percent",xlab="Time", col="blue")

# Ïîñòðîåíèå ãèñòîãðàìì âðåìåííûõ ðÿäîâ óðîâíÿ ðåàëüíîãî  ÂÂÏ è PIT PD íà ïðèìåðå 9 ãðóïïû êðåäèòíîãî ðåéòèíãà
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


# KPSS-òåñò
# IV:
library(tseries)
kpss.test(input$X9, null = "Trend", lshort = TRUE)
kpss.test(input$GDPr, null = "Trend", lshort = TRUE)

# Ðàñ÷åò ëèíåéíîé ðåãðåññèè îñóùåñòâëåí íà ïðèìåðå 9 ãðóïïû êðåäèòíîãî ðåéòèíãà
# V:
fit <-lm(X9~GDPr, data=input)
summary(fit)
confint(fit)

# Îöåíêà äîïóùåíèé ìåòîäà íàèìåíüøèõ êâàäðàòîâ 
# VI:
library(gvlma)
summary(gvlma(fit))

# Òåñò Äàðáèíà-Óîòñîíà
# VII:
library(car) 
durbinWatsonTest(fit)

# Òåñò Áðîéøà-Ãîäôðè
# VIII:
library(lmtest)
bg3 <-bgtest(fit, order = 3)
bg3
bg4 <-bgtest(fit, order = 4)
bg4

# Ïîñòðîåíèå ãðàôèêîâ ÀÊÔ è ÏÀÊÔ
# IX:
res <- residuals(fit)
Res
acf(res) 
pacf(res)

