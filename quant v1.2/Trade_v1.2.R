# 设计交易策略
# By: JiaRu, jiaru2014@126.com
# R version 3.2.3
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Date: 2016-06-16
# 
# Version 1.2
# 做一个简单的交易策略：
# 在out-sample里面找出那些 80% quantile之外的时间,标记出来,
# 利用这些信息来设计buy or sell的signal.
# 对单个股票而言，利用一定时间的样本数据得到一分钟成交量和回报率的联合分布,
# 然后在out-of-sample 阶段,寻找那些奇异点,
# 即超过90% 或者80%的quantile 的时间点，标出买入信号和卖出信号.
# 分析这些时间点对应的下一分钟（或其他形式的时间段）的回报率
# 买入之后，5%收益率就卖出，2%亏损就进行止损，但是当日不能操作
# ===================================================================


rm(list = ls())

require(data.table)  
require(magrittr)  
require(ggplot2)
require(assertthat)  
require(stringr)  

load("dt.Rdata") # 为了方便这里就直接load Rdata格式的数据了。

Trade <- function(
  dt, 
  quantile = 0.9,  # 奇异点定义为 90% quantile 
  sell = 0.05,  # 5%收益率就卖出
  stop = 0.02,  # 2%亏损就进行止损
  when = c("2015-01-05", "2015-12-31", "2016-01-04", "2016-01-29")
)
{
  # 定义样本内/样本外数据 --------------------------------------
  when <- as.Date(when)
  dt_in <- dt[TDate >= when[1] & TDate <= when[2]]
  dt_out <- dt[TDate >= when[3] & TDate <= when[4]]
  
  # 找奇异点 --------------------------------------
  #（此处只看reutrn,暂不考虑return & Volumn 联合分布）
  # 由于return以0为中心大致呈正态分布，ie. 对称的，奇异点定义为上下10% quantile
  # 如果是看Volumn呢？ Volumn 大致呈对数正态分布。都是正的，不对称。？？？
  q1 <- (1 - quantile) / 2
  q2 <- 1 - q1
  q <- quantile(dt_in$return, probs = c(q1, q2))
  q_up <- q[2]
  q_down <- q[1]
  
  # 找出买入信号
  buypoint_index <- which(dt_out$return < q_down | dt_out$return > q_up)
  dt_buysignal <- dt_out[buypoint_index][, .(DateTime, Signal = "BuyPoint")]
  dt_trade <- merge(dt_out, dt_buysignal, by = "DateTime", all = TRUE)
  dt_trade[is.na(dt_trade)] <- ""  # 把Signal中的NA换成空字符串，因为NA不能做“==”运算
  
  # 交易 ------------------------------------------------
  # 交易流程：
  # 按时间顺序遍历整个out sample, 
  # 遇到一个买入信号时，如果没有持仓，就（全仓）买入 
  # 接下来如果上涨到5%就卖出获利了结，如果跌破2%就止损
  # 持仓期间如果再遇到买入信号则跳过。
  
  state <- 0  # 0表示没有持仓，1表示满仓
  sellpoint <- NaN  # 获利平仓点
  stoppoint <- NaN  # 止损点
  for (i in 1:nrow(dt_trade)) {
    
    if (state == 1) { # 有仓位。。。
      
      if (dt_trade[i, EndPrc] > sellpoint) {
        dt_trade[i, Close := "Sell"]  # 若涨到sellpoint, 卖出获利（sell）
        state <- 0
      } else if (dt_trade[i, EndPrc] < stoppoint) {
        dt_trade[i, Close := "Stop"]  # 若跌到stoppoint, 止损（stop）
        state <- 0
      } else {dt_trade[i, Close := ""]}
      
    } else {  # (state == 0) # 没有仓位。。。
      
      if (dt_trade[i, Signal == "BuyPoint"]) {
        dt_trade[i, Buy := "Buy"]
        state <- 1
        buyprice <- dt_trade[i, EndPrc]
        sellpoint <- buyprice * (1 + sell)
        stoppoint <- buyprice * (1 - stop)
      }
      
    }
    message("Looping ", round(i/nrow(dt_trade)*100), " % ...") # 进度条
  }
  dt_trade[is.na(dt_trade)] <- ""  # 把Signal中的NA换成空字符串
  
  # 计算总收益率
  stoptime <- nrow(dt_trade[Close == "Stop"])
  selltime <- nrow(dt_trade[Close == "Sell"])
  totalreturn <- selltime * sell - stoptime * stop
  message("Total return is ", totalreturn, " Sell: ", selltime, " Stop: ", stoptime)  
  
  return(dt_trade)
}


# 测试 ======================================================
# 以下参数可以修改：
dt_trade <- Trade(
  dt, 
  quantile = 0.9,
  sell = 0.04,
  stop = 0.02,
  when = c("2013-06-01", "2014-12-31", "2015-01-01", "2015-02-01") 
)

dt_trade[, index := seq_along(DateTime)]

g <- ggplot(dt_trade, aes(x = index, y = EndPrc)) +
  geom_line() + 
  geom_point(data = dt_trade[Buy == "Buy"], col = "blue") + 
  geom_text(data = dt_trade[Buy == "Buy"], aes(label = "Buy"), col = "blue", vjust = "inward", hjust = "inward") + 
  geom_point(data = dt_trade[Close == "Sell"], col = "green") + 
  geom_text(data = dt_trade[Close == "Sell"], aes(label = "Sell"), col = "green", vjust = "inward", hjust = "inward") + 
  geom_point(data = dt_trade[Close == "Stop"], col = "red") + 
  geom_text(data = dt_trade[Close == "Stop"], aes(label = "Stop"), col = "red", vjust = "inward", hjust = "inward") + 
  xlab("date index")
g

# 列出交易记录：
dt_trade[
  Buy=="Buy" | Close == "Sell" | Close == "Stop", 
  .(DateTime, return, Buy, Close)
  ]
