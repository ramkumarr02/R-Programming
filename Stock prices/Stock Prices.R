Boeing = read.csv("BoeingStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
GE = read.csv("GEStock.csv")
IBM = read.csv("IBMStock.csv")
PG = read.csv("ProcterGambleStock.csv")

Boeing$Date = as.Date(Boeing$Date,"%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date,"%m/%d/%y")
GE$Date = as.Date(GE$Date,"%m/%d/%y")
IBM$Date = as.Date(IBM$Date,"%m/%d/%y")
PG$Date = as.Date(PG$Date,"%m/%d/%y")


str(Boeing)
str(CocaCola)
str(IBM)
str(GE)
str(PG)

summary(Boeing)
summary(CocaCola)
summary(IBM)
summary(GE)
summary(PG)

sd(PG$StockPrice)
plot(CocaCola$Date, CocaCola$StockPrice, type = "l")

CocaCola$Date[max(CocaCola$StockPrice)]
max(CocaCola$StockPrice)

sort(tapply(CocaCola$StockPrice, CocaCola$Date, max),decreasing = TRUE)
sort(tapply(CocaCola$StockPrice, CocaCola$Date, max))


plot(CocaCola$Date, CocaCola$StockPrice, type = "l", lwd =2, col = "RED", xlim = c(as.Date("1980-01-01"),as.Date("1984-01-01")))
lines(PG$Date,PG$StockPrice, col = "Blue", lwd = 2)
abline(v=as.Date("1983-01-01"), lwd = 0.1, lty = 2)

plot(CocaCola$Date, CocaCola$StockPrice, type = "l", lwd =2, col = "RED")
lines(PG$Date,PG$StockPrice, col = "Blue", lwd = 2)
abline(v=as.Date("1983-01-01"), lwd = 0.1, lty = 2)

