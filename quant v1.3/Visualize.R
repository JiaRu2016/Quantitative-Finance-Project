# 量化金融项目
# By: JiaRu, jiaru2014@126.com
# R version 3.2.3
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Date: 2016-06-22
# 
# Version 1.3
# ===================================================================
# 可视化
# 
# 输入 tradebook(with attr: out-sample period) dt
# 输出 ggplot object 
# layers:
# 1. EndPrc
# 2. Buy / Sell signal
# 3. Profit / Stop
# 4. Volumn


Visualize <- function(tradebook, dt) 
{
  # merge
  date_range <- attr(tradebook, "out_sample_range")  # date range
  dt_out <- dt[TDate >= date_range[1] & TDate <= date_range[2]]
  
  trade_plot <- melt(
    tradebook,
    id.vars = c("date_time"),
    measure.vars = c("Buy", "Sell", "Profit", "Stop")
  ) 
  trade_plot <- trade_plot[!is.na(value)]
  
  # 从 dt_out 中添加 EndPrc 
  trade_plot_2 <- merge(
    x = trade_plot,
    y = dt_out[, .(DateTime ,EndPrc)],
    by.x = "date_time",
    by.y = "DateTime",
    all.x = TRUE,
    all.y = TRUE
  )
  
  # 因为x是datetime，存在大量非交易时间，
  # 需要把x做一个转换，不然画出来图很难看
  trade_plot_2[, timeindex := seq_along(date_time)]
  # 绘图参数
  b <- 20  # 20个break
  m <- round(nrow(trade_plot_2) / b)
  
  # 开始画图
  g1 <- 
    ggplot(data = trade_plot_2, aes(x = timeindex, y = EndPrc)) + 
    geom_line() +
    geom_point(data = trade_plot_2[!is.na(variable)], 
               aes(colour = variable), 
               alpha = 0.5) +
    scale_x_continuous(
      breaks = trade_plot_2[timeindex %% m == 0, timeindex], 
      labels = trade_plot_2[timeindex %% m == 0, format(date_time, format = "%m/%d")]
    ) +
    ggtitle("Trade") +
    theme_bw()
  
  return(g1)
}

# # 单元测试
# Visalize(tradebook, dt) 





