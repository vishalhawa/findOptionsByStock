
###############################################################################
##To download all Options for a Stock and check for IV ("Cheapest Option")
#############################################################################


library("quantmod")
library("RQuantLib")
library("data.table")
library(XML)

calculateGreeks<-function(OS,stkPrice,dailyVolatility,rf =0.03,dy=0){
  
  if(is.na(dy)) dy=0
  daysToMaturity=as.numeric(as.Date(gsub("*\\D[[:digit:]]*$","",gsub(pattern="(^[[:alpha:]]+)","",OS)),format = "%y%m%d")-Sys.Date())
  yrs =daysToMaturity/365
  XP = as.numeric(gsub(pattern="(^[[:alpha:]]+)\\d{6}[[:alpha:]]","",OS))/1000
  if( (gsub("[[:digit:]]*$","",gsub(pattern="(^[[:alpha:]]+)\\d{6}","",OS)))=="C") ty="call"  else ty="put"
  op = EuropeanOption(type = ty,underlying = stkPrice,strike =XP ,dividendYield = dy,riskFreeRate =rf ,maturity =yrs ,volatility =dailyVolatility*sqrt(250) )
  op$theta = op$theta/365
  return (op)
}



fetchOptions<-function(stock,exp=NULL){
  # return(getOptionChain(Symbols=stock,Exp=exp))
  opList=tryCatch(getOptionChain(Symbols=stock,Exp=exp), error=function(x){list(symbol=list(dat=data.frame( Strike=NA,  Last=NA,   Chg=NA,   Bid=NA,   Ask=NA, Vol=NA,   OI=NA))); })
  # options<-do.call(rbind,lapply(getOptionChain(Symbols=stock,Exp=exp), function(x) do.call(rbind, x))) 
  options<-do.call(rbind,lapply(opList, function(x) do.call(rbind, x))) 
  options$OS<-gsub("^.*\\.","",rownames(options)) # option symbols
  options$stock <- gsub(pattern="[[:digit:]]*\\D[[:digit:]]*$","",options$OS)
  
  return (options)
}

filterOptions<-function(options){
  options[options$OI<10,] <- NA 
}

IV<-function(optionSymbol,price,stock,stockPrice,dy=0,vlty){
  daysToMaturity=as.numeric(as.Date(gsub("*\\D[[:digit:]]*$","",gsub(pattern="(^[[:alpha:]]+)","",optionSymbol)),format = "%y%m%d")-Sys.Date())
  yrs =daysToMaturity/365
  
  XP = as.numeric(gsub(pattern="(^[[:alpha:]]+)\\d{6}[[:alpha:]]","",optionSymbol))/1000
  if(gsub("[[:digit:]]*$","",gsub(pattern="(^[[:alpha:]]+)\\d{6}","",optionSymbol))=="C") ty="call" else ty="put"
  rf=getQuote("^TYX")$Last/100
  iv=tryCatch(EuropeanOptionImpliedVolatility(type = ty,value = price,underlying = stockPrice ,strike=XP,dividendYield=dy ,riskFreeRate= rf,maturity =yrs,volatility = vlty*sqrt(260)), error=function(x)NA)
  greek = calculateGreeks(optionSymbol,XP,vlty,rf,dy)
  return(data.frame(IV=iv[1],delta=round(greek$delta,2),optionPrice=round(greek$value,2)))
}



processSymbol<-function(sym,startDate=Sys.Date()-365,endDate=Sys.Date()){

stockPrice = getQuote(sym)$Last
if(stockPrice=="N/A") {print("Incorrect Symbol"); quit()}
dividends = getDividends(sym,from = startDate, to = endDate,env=NULL,warnings = FALSE)
dy = sum(dividends)/stockPrice
prices = OHLC(getSymbols(sym,from = startDate, to = endDate,env=NULL,warnings = FALSE))
vlty20 = volatility(prices,n=20)/sqrt(260) # this is daily volatility of 20 days
v20 = as.vector(vlty20[length(vlty20)])

options<-fetchOptions(sym)
options$volatility20 =v20
options$stockPrice = stockPrice
options$changeP = round(100*(stockPrice/as.vector(Cl(prices))[nrow(prices)]-1),2)

if(nrow(options)>1){
  cat("processing...",sym)
  options$marketPrice =  options$Bid/2+options$Ask/2
  options$spread =  round((options$Ask-options$Bid)/options$marketPrice,2)
  
  # options[options$Strike+marketPrice>stockPrice,]  ##Parity filter - may not be needed
 options= options[grepl("C[[:digit:]]{8}$",options$OS) & (options$Strike+options$marketPrice>stockPrice),]
 options =  rbind(options,options[grepl("P[[:digit:]]{8}$",options$OS) & (options$Strike<options$marketPrice+stockPrice),])
  
  df=mapply(options$OS,FUN = IV,price=options$marketPrice,stock=options$stock,stockPrice=stockPrice,dy=dy,vlty=v20)
options$IV=as.numeric(t(df)[,"IV"])
options$delta=as.numeric(t(df)[,"delta"])
options$optionPrice=as.numeric(t(df)[,"optionPrice"])

# filterOptions(options)
options = options[complete.cases(options),]
options$OptionPriceRatio = round(options$marketPrice/options$optionPrice,2)

options = as.data.table(options)

print("Cheap Options...")
print(head(options[order(options$IV),c("OI","OS","stock","marketPrice","IV","delta","OptionPriceRatio")]))
print("Expensive Options...")
print(tail(options[order(options$IV),c("OI","OS","stock","marketPrice","IV","delta","OptionPriceRatio")]))

}
return(options)
}

# # #################   Diver ############################
 # processSymbol("ngl")
# 
# cat("Enter Stock : ")
# sym <- readLines(file("stdin"),n=1)
# print(sym);

# IV(options$OS[124],options$marketPrice[124],options$stock[124],stockPrice)
# for(n in 1:length(options$OS)){
#   print(n)
#   IV(options$OS[n],options$marketPrice[n],options$stock[n],stockPrice)
# 
# }
 

