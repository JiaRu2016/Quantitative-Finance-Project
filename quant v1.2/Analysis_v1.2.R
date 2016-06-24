# 分析股票交易数据
# By: JiaRu, jiaru2014@126.com
# R version 3.2.3
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Date: 2016-06-16
# 
# Version 1.2
# 1. 样本内/外起止的日期作为函数参数，可调节
# 2. 原始数据不用excel了，直接从csv文件读取。
# ===================================================================

rm(list = ls())

require(data.table)  
require(magrittr)  # 只是为了用 `%>%`
require(ggplot2)
require(assertthat)  # a tool for Defensive Programming
require(stringr)  # 文字处理


# 1. 读文件 ===========================================

# 函数ReadStockFile() 
# Argus: 
#   stock_num: 股票代码，要加引号！！！
# Return: 
#   a data.table of 5 columns
#     type分别为：股票代码 chr, 日期 chr, 时间 chr, 价格 num, 成交量 int 

ReadStockFile <- function(stock_num)
{
  # 检查参数 ---------------------
  assert_that(stock_num %in% stock_list)
  
  # 读文件 ------------------------
  tryCatch(
    {
      dt <- read.csv(
        file.path("csvStockFiles", paste0(stock_num, ".csv")),
        header = TRUE,
        sep = ";",
        colClasses = c(rep("character", 3), "numeric", "integer")
      ) %>% as.data.table()
      message("读文件成功：", stock_num)
      
    },
    error = function(e) {message("读取文件出错！"); e}
  )
  
  
  # 整理数据格式 ----------------------------
  dt[, DateTime := as.POSIXct(paste(TDate, MinTime), format = "%Y%m%d %H%M", tz = "PRC")]
  dt[, TDate := as.Date(TDate, "%Y%m%d")]
  dt[, return := c(NA, diff(EndPrc)/EndPrc[-.N])]  # 计算收益率
  message("整理数据格式完毕！")
  
  setkey(dt, "DateTime")
  
  return(dt)
}



# 2. 检查数据是否有异常  ======================================
# 函数CheckStockData()
# Arugs:
#   dt: data.table from last step
# Return:
#   cleaned data.table

CheckStockData <- function(dt) 
{
  # 看有没有重复的行 ----------------------------
  tryCatch(
    {
      if (uniqueN(dt) == nrow(dt)){
        message("没有重复行。")
      } else {
        dt <- unique(dt)
        message("数据集中有重复行，已删除重复行。")
      }
    },
    error = function(e) message("检查是否有重复行时出错。已跳过此步骤。")
  )
  
  # 看每一天时间是否齐全。----------------------------
  # 正常来说每天应有 4*60+1 = 241 行
  tryCatch(
    {
      checktime <- dt[, .N, by = TDate][N != 241]
      if (nrow(checktime) == 0) {
        message("每个交易日的交易信息都是完整的！")
      } else {
        message("以下交易日缺少部分交易信息：")
        print(checktime)
      }
    },
    error = function(e) message("检查每天时间是否齐全时出错。跳过。")
  )
  
  return(dt)
}




# 3. 分析  ======================================
# 函数AnalysisDis()分析样本内/外收益率or成交量的分布
# Argus：
#   dt: 清理好的股票数据。
#   what: "return" or "MinTq"
#   when = c(in_start, in_end, out_start, out_end): 样本内/外起止点。
#      写成可直接转化成日期格式的形式：“2015-01-05”
#      长度可以是3或4

AnalysisDis <- function(dt, what = "return", 
    when = c("2015-01-05", "2015-12-31", "2016-01-04", "2016-01-29"))
{
  # 检查参数 --------------------------
  when <- as.Date(when)
  assert_that(
    what %in% c("return", "MinTq") && length(what) == 1,
    length(when) == 4,
    when[1] < when[2] && when[2] <= when[3] && when[3] < when[4]
  )
  # 取样本内/外数据 --------------------
  # 这里对于成交量(all >= 0) 取对数
  # 对于retrun, 先取绝对值再取对数。
  # 
  dt_in <- 
    dt[TDate >= when[1] & TDate <= when[2], what, with = FALSE] %>% 
    unlist(use.names = FALSE) %>%
    abs() %>%
    log()
  dt_out <- 
    dt[TDate >= when[3] & TDate <= when[4], what, with = FALSE] %>% 
    unlist(use.names = FALSE) %>%
    abs() %>%
    log()
  
  dt_in_g <<- dt_in  # just for debugging
  dt_out_g <<- dt_out
  
  # 画图：样本内/外retrun/Vol分布---------------------
  # 这里只用R中的基础绘图系统 boxplot()
  # 暂时先不用histogram,因为根据不同的数据要调整坐标轴，很难写成函数
  
  boxplot(dt_in, dt_out, 
          xlab = "in sample         out of sample",
          main = sprintf("distribution of log(abs( %s )) of stock %s", what, stock_num),
          outline = FALSE)
  
  return(0)
}

# 4. 测试 ===================================================

# 注：
# 文件夹“csvStockFiles“里面是合并好的股票数据csv文件。
# 每个股票只有一个文件，是我手工合并的（excel里复制粘贴），再另存为csv格式
# 暂时只做了了一个股票，作为测试，后续可以添加更多。。

# 列出数据集中所有的股票代码 
stock_list <- 
  list.files("csvStockFiles") %>% 
  str_replace(pattern = "\\.csv", replacement = "")
cat("以下是数据集中所有可分析的股票代码：", stock_list, sep = "\n")

# 以"000001SH"为例，测试
stock_num <- "000001SH"
dt <- ReadStockFile(stock_num)
dt <- CheckStockData(dt)

# 以下参数可以改：
AnalysisDis(dt, what = "return", 
            when = c("2014-01-01", "2016-05-01", "2016-05-02", "2016-05-31"))
AnalysisDis(dt, what = "MinTq",
            when = c("2014-01-01", "2016-05-01", "2016-05-02", "2016-05-31"))

# 把数据dt存成Rdata格式，方便后面使用。
# save(dt, file = "dt.Rdata")





