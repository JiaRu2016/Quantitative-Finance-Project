# 量化金融项目
# By: JiaRu, jiaru2014@126.com
# R version 3.2.3
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Date: 2016-06-22
# 
# Version 1.3
# ===================================================================
# 交易策略：
# 1. Sell信号有两种：
#   - 上涨5%平仓 _Profit_
#   - 下跌2%止损  _Stop_
#   - return低于10%的quantile  _Sell_
# 2. 卖出不能在同一天。T + 1
# 3. 买入信号 (return > 90% quantile) * (Volumn > 70% quantile) 交叉项
# 4. 无视开盘的买入信号
# 5. 可以多次买入。分批建仓 eg. 每次买入使用10%的资金

rm(list = ls())
require(data.table)
require(magrittr)
require(assertthat)
require(ggplot2)

load("dt.Rdata")

stock_set <- c("000001")


Trade <- function(
  stock,   # a data.table / stock name, chr,  eg. "000001" 
  insample,  # chr, length == 2, eg. c("2013-01-01", "2013-06-31")
  outsample,  
  Profit.rate = 0.05, Sell.rt.q = 0.1,  # Sell 信号 __or__
  Stop.rate = 0.02,                  # Stop 信号
  Buy.rt.q = 0.9, Buy.vol.q = 0.7,   # Buy 信号 __and__
  # k = TRUE or int 10,     # 可多次买入。可设置分几批建仓
  Tp1 = TRUE   # T + 1, 限制当日不能卖出 pass
)
{
  # 1.确认参数 ------------------------------------------
  insample <- as.Date(insample)
  outsample <- as.Date(outsample)
  
  assert_that(
    #  stock参数：available, scalar
    # stock %in% stock_set,
    # is.string(stock),
    
    # insample outsample 参数：时间段顺序，长度
    insample[1] < insample[2],
    outsample[1] < outsample[2],
    insample[2] <= outsample[1],
    length(insample) == 2,
    length(outsample) == 2
  )
  
  # 2. 得到 insample data 计算 quantile -------------------
  dt <- stock   #  输入方式待定
  dt_in <- dt[TDate >= insample[1] & TDate <= insample[2]]
  
  Buy.vol <- quantile(dt_in$MinTq, Buy.vol.q)
  Buy.rt <- quantile(dt_in$return, Buy.rt.q)
  Sell.rt <- quantile(dt_in$return, Sell.rt.q)
  
  # 3. trade  -------------------------------------------
  dt_out <- dt[TDate >= outsample[1] & TDate <= outsample[2]]
  dt_trade <- dt_out
  
  
  # `book` is to record trade history. Initialize...
  book <- data.table(index = integer(0),
                     date_time = as.POSIXct(character(0)),
                     position = integer(0),
                     Buy = integer(0),
                     profit.point = numeric(0),
                     stop.point = numeric(0),
                     Sell = integer(0),
                     Profit = integer(0),
                     Profit.w = integer(0),  
                     Stop = integer(0),
                     Stop.w = integer(0))  
  j <- 1L  # index of `book``
  pst <- 0L  # positon
  for (i in 1:nrow(dt_trade)){
    L <- dt_trade[i, ] # L stands for "line"
    
    # (1) Buy or Sell ?
    if( L$return > Buy.rt & L$MinTime != "0930") {
      # Buy
      pst <- pst + 1L
      book_loop <- data.table(index = j, 
                              date_time = L$DateTime,
                              position = pst,
                              Buy = 1,
                              profit.point = L$EndPrc * (1 + Profit.rate),
                              stop.point = L$EndPrc * (1 - Stop.rate))
      #browser()
      book <- rbind(book, book_loop, fill = TRUE)
      j <- j + 1L
    } else if (L$return < Sell.rt) {
      # Sell 
      # 这里先不管 T+1 循环完了再改 group by TDate ....
      book_loop <- data.table(index = j,
                              date_time = L$DateTime,
                              position = 0,
                              Sell = -pst)
      book <- rbind(book, book_loop, fill = TRUE)
      pst <- 0L
      j <- j + 1L
    } else {
      # pass
    }
    
    # (2) Profit-taking
    # idx <- book[L$EndPrc > book$profit.point, index] 
    # 
    # (3) Stop-loss
    
    
  }
  
  
  return(book)
}

a <- Trade(
  dt,   # a data.table / stock name, chr,  eg. "000001" 
  insample = c("2016-01-04", "2016-04-29"),
  outsample = c("2016-05-01", "2016-05-15")  )

a
