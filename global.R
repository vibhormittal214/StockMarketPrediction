library(lubridate)
library(caret)
library(forecast)
library(TTR)
library(DMwR)
library(neuralnet)
library(forecast)
library(quantmod)
getdata<- function()
{
  
}

getPrices<- function()
{
  if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
  library(BatchGetSymbols)
  
  # set dates
  first.date <- "2015-01-01"
  last.date <- "2018-05-06"
  
  # set tickers
  tickers <- c('FB')
  
  l.out <- BatchGetSymbols(tickers = tickers, 
                           first.date = first.date,
                           last.date = last.date, 
                           cache.folder = file.path(tempdir(), 
                                                    'BGS_Cache') ) # cache in tempdir()
  df2<-l.out$df.tickers[c('ref.date','price.open','price.high','price.low','price.close','price.adjusted','volume')]
  colnames(df2)<-c("Date","Open","High","Low","Close","Adj.Close","Volume")
  prices<-df2
  priceszz <- prices$Close
  cur_dev <- dev.cur()
  ggsave("plot1.png",plot.ts(priceszz))
  dev.set(cur_dev)
  prices_diff1<-diff(priceszz,differences = 1)
  cur_dev <- dev.cur()
  ggsave("plot2.png",plot.ts(prices_diff1))
  dev.set(cur_dev)
  prices_diff2<-diff(priceszz,differences = 2)
  cur_dev <- dev.cur()
  ggsave("plot3.png",plot.ts(prices_diff2))
  dev.set(cur_dev)
  pmodel1<-arima(priceszz,order = c(1,0,0))
  pmodel2<-arima(priceszz,order = c(1,0,1))
  pmodel3<-arima(priceszz,order = c(2,0,0))
  pmodel4<-arima(priceszz,order = c(0,0,1))
  pmodel5<-arima(priceszz,order = c(0,0,2))
  pmodel6<-arima(priceszz,order = c(1,1,0))
  pmodel7<-arima(priceszz,order = c(0,1,0))
  pmodel8<-arima(priceszz,order = c(2,1,0))
  pmodel9<-arima(priceszz,order = c(2,1,1))
  pmodel10<-arima(priceszz,order = c(2,1,2))
  pmodel_auto_arima<-auto.arima(priceszz)
  print(Box.test(pmodel_auto_arima$residuals,lag=1,type = "Ljung-Box"))
  print(Box.test(pmodel1$residuals,lag=1,type = "Ljung-Box"))
  print(Box.test(pmodel2$residuals,lag=1,type = "Ljung-Box"))
  print(Box.test(pmodel3$residuals,lag=1,type = "Ljung-Box"))
  print(Box.test(pmodel4$residuals,lag=1,type = "Ljung-Box"))
  print(Box.test(pmodel5$residuals,lag=1,type = "Ljung-Box"))
  print(Box.test(pmodel6$residuals,lag=1,type = "Ljung-Box"))
  print(Box.test(pmodel7$residuals,lag=1,type = "Ljung-Box"))
  print(Box.test(pmodel8$residuals,lag=1,type = "Ljung-Box"))
  print(Box.test(pmodel9$residuals,lag=1,type = "Ljung-Box"))
  print(Box.test(pmodel10$residuals,lag=1,type = "Ljung-Box"))
  print(pmodel_auto_arima)
  
  model_forecast<-forecast(pmodel_auto_arima,h=5)
  model_forecast1<-forecast(pmodel1,h=5)
  model_forecast2<-forecast(pmodel2,h=5)
  model_forecast3<-forecast(pmodel3,h=5)
  model_forecast4<-forecast(pmodel4,h=5)
  model_forecast5<-forecast(pmodel5,h=5)
  model_forecast6<-forecast(pmodel6,h=5)
  model_forecast7<-forecast(pmodel7,h=5)
  model_forecast8<-forecast(pmodel8,h=5)
  model_forecast9<-forecast(pmodel9,h=5)
  model_forecast10<-forecast(pmodel10,h=5)
  print(accuracy(model_forecast1))
  print(accuracy(model_forecast2))
  print(accuracy(model_forecast3))
  print(accuracy(model_forecast4))
  print(accuracy(model_forecast5))
  print(accuracy(model_forecast6))
  print(accuracy(model_forecast7))
  print(accuracy(model_forecast8))
  print(accuracy(model_forecast9))
  print(accuracy(model_forecast10))
  print(model_forecast)
  predicts1 <- as.data.frame(cbind(as.Date(as.data.frame(rbind(as.Date(tail(prices,1)$Date)+1, 
                                                              as.Date(tail(prices,1)$Date)+2,
                                                              as.Date(tail(prices,1)$Date)+3,
                                                              as.Date(tail(prices,1)$Date)+4,
                                                              as.Date(tail(prices,1)$Date)+5))$V1,origin = "1970-01-01"),as.data.frame(model_forecast)
  ))
  predicts1<-predicts1[,c(1,5)]
  colnames(predicts1)<-c("Date","Close")
  actuals1 <- prices[, c("Date", "Close")]
  predicts1<-rbind(tail(actuals1,1),predicts1)
  predicts1$Date <- as.Date( predicts1$Date, '%m/%d/%Y')
  require(ggplot2)
  print(ggplot( data = predicts1, aes( Date, Close )) + geom_line() )
  # 3. Close the file
  jpeg("plot4.jpg")
  # 2. Create the plot
  plot(model_forecast)
  # 3. Close the file
  dev.off()
  print(accuracy(model_forecast))
  prices$Date<-as.Date(prices$Date)
  today<-Sys.Date()
  prices <- prices[prices$Date >=today-180,]
  prevrows <- function(data,n) {sapply(1:n,function(x) c(rep(NA,x),head(data,-x)))}
  
  prices$Day <- as.numeric(as.POSIXlt(prices$Date)$wday)
  prices$PrevClose <- prevrows(prices$Close,1)
  prices$Change <- prices$Close - prices$PrevClose
  prices$Movement<-ifelse(prices$Change > 0 ,1 , -1)
  prices$SMA50<-SMA(prices$Close, n=50)
  prices$SMA10<-SMA(prices$Close, n=10)
  prices$SMA5<-SMA(prices$Close, n=5)
  prices$SMA3<-SMA(prices$Close, n=3)
  prices$EMA50<-EMA(prices$Close, n=50)
  prices$EMA10<-EMA(prices$Close, n=10)
  prices$EMA5<-EMA(prices$Close, n=5)
  prices$EMA3<-EMA(prices$Close, n=3)
  prices$RSI50<-RSI(prices$Close, n=50)
  prices$RSI10<-RSI(prices$Close, n=10)
  prices$RSI5<-RSI(prices$Close, n=5)
  prices$RSI3<-RSI(prices$Close, n=3)
  prices$VOLEMA50<-EMA(prices$Volume, n=50)
  prices$VOLEMA10<-EMA(prices$Volume, n=10)
  prices$VOLEMA5<-EMA(prices$Volume, n=5)
  prices$VOLEMA3<-EMA(prices$Volume, n=3)
  prices$OBV<-OBV(prices$Close, prices$Volume)
  HLC<-as.matrix(cbind(prices$High, prices$Low, prices$Close))
  prices$ATR<-ATR(HLC, n = 14)[,1]
  
  prices$MACD<-as.data.frame((MACD(prices$Close)))$macd
  
  prices<-prices[ which(complete.cases(prices)==TRUE),]
  training = prices[prices$Date < as.Date(today-30),]
  trainNN<-as.data.frame(cbind(training$Close,training$PrevClose,training$Day,training$SMA50,training$SMA10,training$SMA5,training$SMA3,training$EMA50,training$EMA10,training$EMA5,training$EMA3,training$RSI50,training$MACD,training$OBV,training$Change
  ))
  trainNN<-trainNN[ which(complete.cases(trainNN)==TRUE),]
  trainNN<-cbind(trainNN$V1, trainNN$V2
                 , prevrows(trainNN$V3,1)
                 , prevrows(trainNN$V4,1)
                 , prevrows(trainNN$V5,1)
                 , prevrows(trainNN$V6,1)
                 , prevrows(trainNN$V7,1)
                 , prevrows(trainNN$V8,1)
                 , prevrows(trainNN$V9,1)
                 , prevrows(trainNN$V10,1)
                 , prevrows(trainNN$V11,1)
                 , prevrows(trainNN$V12,1)
                 , prevrows(trainNN$V13,1)
                 , prevrows(trainNN$V14,1)
                 , prevrows(trainNN$V15,1)
  )
  trainNN<-trainNN[ which(complete.cases(trainNN)==TRUE),]
  trainNNScaled<-as.data.frame(scale(lag(as.matrix(trainNN),1)))
  n <- names(trainNNScaled)
  f <- as.formula(paste("V1 ~", paste(n[!n %in% "V1"], collapse = " + ")))
  set.seed(12345)
  net <- neuralnet(f,data=trainNNScaled, hidden=2, threshold=.01, rep=1)
  testing = prices[prices$Date >= as.Date(today-30),]
  lastTrainingDay<-as.data.frame(tail(trainNN,1))
  testNN<-rbind(lastTrainingDay,as.data.frame(cbind(testing$Close,testing$PrevClose,testing$Day,testing$SMA50,testing$SMA10,testing$SMA5,testing$SMA3,testing$EMA50,testing$EMA10,testing$EMA5,testing$EMA3,testing$RSI50,testing$MACD,testing$OBV,testing$Change
  )))
  
  nextwday <- function(dy) {
    ifelse(dy==5, 1, dy<-dy+1)
  }
  testNN<-cbind(testNN$V1, testNN$V2
                , prevrows(testNN$V3,1)
                , prevrows(testNN$V4,1)
                , prevrows(testNN$V5,1)
                , prevrows(testNN$V6,1)
                , prevrows(testNN$V7,1)
                , prevrows(testNN$V8,1)
                , prevrows(testNN$V9,1)
                , prevrows(testNN$V10,1)
                , prevrows(testNN$V11,1)
                , prevrows(testNN$V12,1)
                , prevrows(testNN$V13,1)
                , prevrows(testNN$V14,1)
                , prevrows(testNN$V15,1)
  )
  testNN<-testNN[ which(complete.cases(testNN)==TRUE),]
  lastDay<-as.data.frame(tail(testing,1))
  nextDay<-as.data.frame(cbind(0,lastDay$Close,nextwday(as.data.frame(tail(testNN,1))$V3),lastDay$SMA50,lastDay$SMA10,lastDay$SMA5,lastDay$SMA3,lastDay$EMA50,lastDay$EMA10,lastDay$EMA5,lastDay$EMA3,lastDay$RSI50,lastDay$MACD,lastDay$OBV,lastDay$Change
  ))
  testNN<-rbind(testNN,nextDay)
  
  for (i in 1:5){
    testNNScale <- scale(testNN, center = attr(scale(trainNN), 'scaled:center'), scale = attr(scale(trainNN), 'scaled:scale'))
    
    net.results <- compute(net, as.data.frame(testNNScale)[-1])
    results<-unscale(as.matrix(net.results$net.result)[,1], testNNScale)
    
    if(i==1){
      cleanoutput <- cbind(testNN$V1,as.data.frame(results), abs(testNN$V1-as.data.frame(results))/testNN$V1*100)
      colnames(cleanoutput) <- c("Expected Output","Neural Net Output", "Error %")
      print(cleanoutput)
      
      print(t<-sum(cleanoutput[which(cleanoutput$"Error %" < 100),]$"Error %")/(nrow(cleanoutput)-1))
    }
    
    testNN[nrow(testNN),1]<-tail(as.data.frame(results),1)
    p<-rbind(trainNN,testNN)
    SMA50<-tail(SMA(p$V1, n=50),1)
    SMA10<-tail(SMA(p$V1, n=10),1)
    SMA5<-tail(SMA(p$V1, n=5),1)
    SMA3<-tail(SMA(p$V1, n=3),1)
    EMA50<-tail(EMA(p$V1, n=50),1)
    EMA10<-tail(EMA(p$V1, n=10),1)
    EMA5<-tail(EMA(p$V1, n=5),1)
    EMA3<-tail(EMA(p$V1, n=3),1)
    RSI50<-tail(RSI(p$V1, n=50),1)
    MACD<-tail(as.data.frame((MACD(p$V1)))$macd,1)
    OBV<-tail(p$V14,1)
    Change<-tail(as.data.frame(results),1)-head(tail(as.data.frame(results),2),1)
    
    nextDay<-as.data.frame(cbind(0,tail(as.data.frame(results),1),nextwday(as.data.frame(tail(testNN,1))$V3),SMA50,SMA10,SMA5,SMA3,EMA50,EMA10,EMA5,EMA3,RSI50,MACD,OBV,Change
    ))
    
    names(nextDay) <- names(testNN)
    testNN<-rbind(testNN,nextDay)
  }
  testNN<-testNN[-nrow(testNN),]
  predicts <- as.data.frame(cbind(as.Date(as.data.frame(rbind(as.Date(tail(prices,1)$Date+1), 
                                                              as.Date(tail(prices,1)$Date)+2,
                                                              as.Date(tail(prices,1)$Date)+3,
                                                              as.Date(tail(prices,1)$Date)+4,
                                                              as.Date(tail(prices,1)$Date)+5))$V1,origin = "1970-01-01"),as.data.frame(tail(testNN,5)$V1)
  ))
  colnames(predicts) <- c("Date", "Close")
  actuals <- prices[, c("Date", "Close")]
  prices<-rbind(actuals,predicts)
  print(prices)
  print(t)
}

