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

require(data.table)
require(magrittr)
require(assertthat)
require(ggplot2)
require(rlist)
require(stringr)


Trade <- function(stock,  # data.table
                  insample,  # 
                  outsample,
                  Profit.rate = 0.05,  # 5% Profit-taking
                  Stop.rate = 0.02,  # 2% Stop-loss
                  Sell.rt.q = 0.1,  # return < 10% quantile, Sell
                  Buy.rt.q = 0.9,  # (return > 90% quantile)&(vol > 70% quantile), Buy
                  Buy.vol.q = 0.7,
                  Tp1 = TRUE   # T + 1 pass..
                  
)
{
  # 1.确认参数 ------------------------------------------
  insample <- as.Date(insample)
  outsample <- as.Date(outsample)
  
  #browser()
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
                     Stop.w = integer(0), 
                     Note = character(0))
  book.wait <- book  # 买入当天不能交易，等待第二天交易的 waiting list
  j <- 1L  # index of `book``
  pst <- 0L  # positon
  pool <- list() # 未平仓的交易集合。Data structure like "Set" 
  
  for (i in 1:nrow(dt_trade)){
    L <- dt_trade[i, ] # L stands for "line"
    
    # (0) 每一天的开始，先交易昨天的 waiting list 
    # if (j== 44) {
    #   browser()
    # }
    if (L$MinTime == "0930" & nrow(book.wait) > 0) {
      
      wn <-
        book.wait$Note[nrow(book.wait)] %>% 
        str_extract("\\d{1,3}") %>% 
        as.integer() %>% 
        sum()
      pst <- pst - wn
      
      pool <- pool[-which(pool %in% book.wait$index)] # 把要平仓的拿出pool集合
      book.wait[, `:=`(index = 0L,
                       date_time = L$DateTime,
                       position = pst,
                       Sell = -nrow(wait_sell),
                       Profit = Profit - 1L,   # 0 - 1 = -1
                       Stop = Stop - 1L,       # NA - 1 = NA
                       Note = "yesterday")       
                ] 
      
      book <- rbind(book, tail(book.wait, 1), fill = TRUE)
      book.wait <- book.wait[!(1:nrow(book.wait))] #删掉所有行，保留结构
    }
    
    # (1) Profit-taking
    # 找出book中触及到profit-taking的交易的index
    idx <- book[L$EndPrc > book$profit.point, index] 
    idx <- idx[idx %in% pool]
    if (length(idx) > 0){ # 如果有的话。。。
      
      for (idx_loop in idx){ # 遍历所有触及Profit-taking点的index
        if(book[index == idx_loop, date_time] < L$DateTime){ 
          #要Profit的一笔交易__不是__当天买入的
          pst <- pst - 1L
          pool <- pool[-which(pool %in% idx_loop)]
          book_loop <- data.table(index = j,
                                  date_time = L$DateTime, 
                                  position = pst,
                                  Profit = -1L,
                                  Profit.w = idx_loop) # 'w' stands for 'which'
          book <- rbind(book, book_loop, fill = TRUE)
          j <- j + 1L
          
        } else if(book[index == idx_loop, date_time] == L$DateTime){ 
          # 要Profit的一笔交易__是__当天买入的
          # pst <- pst - 0L
          # pool <- Pass...
          book_loop <- data.table(index = j,
                                  date_time = L$DateTime, 
                                  position = pst, # 
                                  Profit = 0L,  # 不能实际Profit，只添一条记录
                                  Profit.w = idx_loop,  
                                  Note = "Wait to profit")
          book <- rbind(book, book_loop, fill = TRUE)
          j <- j + 1L
          
          # 加入 waiting list
          book.wait <- rbind(book.wait, book_loop, fill = TRUE)
          
        } else {
          warning("foo")
        }
        
      } # End for
    } 
    
    
    # (2) Stop-loss。
    # 找出book中触及到Stop-loss的交易的index
    idx <- book[L$EndPrc < book$stop.point, index] 
    idx <- idx[idx %in% pool]
    if (length(idx) > 0){ # 如果有的话。。。
      
      for (idx_loop in idx){ # 遍历所有触及Stop-loss点的index
        if(book[index == idx_loop, date_time] < L$DateTime){ 
          #要Stop的一笔交易__不是__当天买入的
          pst <- pst - 1L
          pool <- pool[-which(pool %in% idx_loop)]
          book_loop <- data.table(index = j,
                                  date_time = L$DateTime, 
                                  position = pst,
                                  Stop = -1L,
                                  Stop.w = idx_loop) 
          book <- rbind(book, book_loop, fill = TRUE)
          j <- j + 1L
          
        } else if(book[index == idx_loop, date_time] == L$DateTime){ 
          # 要Stop的一笔交易__是__当天买入的
          # pst <- pst - 0L
          # pool Pass...
          book_loop <- data.table(index = j,
                                  date_time = L$DateTime, 
                                  position = pst, # 
                                  Stop = 0L,  # 不能实际Stop，只添一条记录
                                  Stop.w = idx_loop,  
                                  Note = "Wait to stop")
          book <- rbind(book, book_loop, fill = TRUE)
          j <- j + 1L
          
          # 加入 waiting list
          book.wait <- rbind(book.wait, book_loop, fill = TRUE)
          
        } else {
          warning("foo_foo")
        }
        
      } # End for
    } 
    
    # (3) Buy or Sell ?
    if( L$return > Buy.rt & L$MinTime != "0930") {
      # Buy
      pst <- pst + 1L
      pool <- c(pool, j)  # 加入pool集合
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
      to_sell <- book[index %in% pool]  # 找出book中尚未平仓的所有记录. length=0,1,2...
      now_sell <- to_sell[as.Date(date_time) < L$TDate]
      wait_sell <- to_sell[as.Date(date_time) == L$TDate]
      
      # now_sell
      pst <- pst - nrow(now_sell)
      book_loop <- data.table(index = j,
                              date_time = L$DateTime,
                              position = pst,
                              Sell = -nrow(now_sell))
      book <- rbind(book, book_loop, fill = TRUE)
      if(j == 44) {
        browser()
      }
      pool <- pool[!(pool %in% now_sell$index)]
      j <- j + 1L
      
      # wait_sell
      book_loop <- data.table(index = j,
                              date_time = L$DateTime, 
                              position = pst, # 
                              Sell = 0L,  # 不能实际Sell，只添一条记录
                              Note = paste0("Wait to Sell", -nrow(wait_sell)))
      book <- rbind(book, book_loop, fill = TRUE)
      j <- j + 1L
      book.wait <- rbind(book.wait, book_loop, fill = TRUE)
      
    } else {
      # pass
    }
    

    
    if (i%%100 == 0) message(round(i/nrow(dt_trade)*100), "%")
  }
  
  book[, `:=`(profit.point = NULL, stop.point = NULL)]
  attr(book, "out_sample_range") <- outsample
  
  return(book)
}


# 单元测试
# # debug(Trade)
# a <- Trade(
#   dt,   # data.table
#   insample = c("2014-01-01", "2015-04-01"),  # 样本内时间范围
#   outsample = c("2015-04-01", "2015-04-30"),  # 样本外时间范围
#   # 注意时间顺序，否则会报错
#   Profit.rate = 0.05,  # 5% Profit-taking
#   Stop.rate = 0.02,  # 2% Stop-loss
#   Sell.rt.q = 0.001,  # return < 0.1% quantile, Sell
#   Buy.rt.q = 0.9,  # (return > 90% quantile)&(vol > 70% quantile), Buy
#   Buy.vol.q = 0.7
# )
# a
