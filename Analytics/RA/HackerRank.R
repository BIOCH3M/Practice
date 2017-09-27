
most_elastic_market <- function (data) {
  f <- file("/Users/asurin/Documents/GITHUB_PUBLIC/HackerRank/input000.txt")
  on.exit(close(f))
  
  raw_data <- strsplit(readLines(f), ",")
  col_names <- raw_data[[1]]
  df <- data.frame(raw_data[2:5], stringsAsFactors=FALSE)
  names(df) <- col_names
  df$day_num <- as.integer(df$day_num)
  df$num_sales <- as.integer(df$num_sales)
  df$price <- as.numeric(df$price)
  
  ##Split data by market into different data frames
  atl.df<-subset(df, market=="Atlanta")
  dal.df<-subset(df, market=="Dallas")
  orl.df<-subset(df, market=="Orlando")
  chi.df<-subset(df, market=="Chicago")
  sea.df<-subset(df, market=="Seattle")
  
  ##Run Regressions of Sales on Price for each market and find coeff
  atl.reg<-lm(num_sales~ price,data=atl.df )
  atl.reg.c<-atl.reg$coefficients[2]
  dal.reg<-lm(num_sales~ price,data=dal.df )
  dal.reg.c<-atl.reg$coefficients[2]
  orl.reg<-lm(num_sales~ price,data=orl.df )
  orl.reg.c<-atl.reg$coefficients[2]
  chi.reg<-lm(num_sales~ price,data=chi.df )
  chi.reg.c<-atl.reg$coefficients[2]
  sea.reg<-lm(num_sales~ price,data=sea.df )
  sea.reg.c<-atl.reg$coefficients[2]
  
  ##Calculate Price Elasticity for each market
  
  ##PE=B*(P/Q)
  #B=coef
  #P=historical average price
  atl.p<-mean(atl.df$price)
  dal.p<-mean(dal.df$price)
  orl.p<-mean(orl.df$price)
  chi.p<-mean(chi.df$price)
  sea.p<-mean(sea.df$price)
  #Q=historical average sales
  atl.q<-mean(atl.df$num_sales)
  dal.q<-mean(dal.df$num_sales)
  orl.q<-mean(orl.df$num_sales)
  chi.q<-mean(chi.df$num_sales)
  sea.q<-mean(sea.df$num_sales)
  
  ###Price Elasticity
  atl.pe<-atl.reg.c*(atl.p/atl.q)
  dal.pe<-dal.reg.c*(dal.p/dal.q)
  orl.pe<-orl.reg.c*(orl.p/orl.q)
  chi.pe<-chi.reg.c*(chi.p/chi.q)
  sea.pe<-sea.reg.c*(sea.p/sea.q)
  
  #Look at Significance:
  atl.pv<-summary(atl.reg)$coefficients[2,4]
  dal.pv<-summary(dal.reg)$coefficients[2,4]
  orl.pv<-summary(orl.reg)$coefficients[2,4]
  chi.pv<-summary(chi.reg)$coefficients[2,4]
  sea.pv<-summary(sea.reg)$coefficients[2,4]
  
  ###Now compare values and decide if need to use default elasticity=-0.5
  Market<-c("Atlanta", "Dallas", "Orlando", "Chicago", "Seattle")
  PE<-c(atl.pe, dal.pe, orl.pe, chi.pe, sea.pe)
  Sig<-c(atl.pv,dal.pv,orl.pv,chi.pv,sea.pv)
  final.df<-data.frame(Market=Market,PE=as.numeric(PE),PE.Sig=ifelse(Sig<=.05,"Yes", "No"))
  #Change PE for failed sig
  final.df[4,2]<--0.05
  final.df[5,2]<--0.05
  #order and select the only first city
  o.df<-final.df[order(final.df$PE),]
  answer<-unlist(as.character(o.df[1,1]))
  return(answer)
}
most_elastic_market(data)


