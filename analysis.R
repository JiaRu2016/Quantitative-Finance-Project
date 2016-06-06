
# 分析股票交易数据
# Author: JiaRu 
# Email: jiaru2014@126.com
# Version 1.0

# 1. 读文件 ====================================================
# 每个股票由三个文件组成：
# 顺序是:
# “股票代码.xls
# ”股票代码.part1.xls“
# ”股票代码part2.xls”
# 将这三个数据框rbind()就得到一只股票的全部交易数据 
# 以000001SH（上证综合）为例： 2013-05-30 ~ 2015-08-21
# 我个人比较偏好data.table方式。
# Note：excel直接读取莫名少一列。。。google好久没有解决方案，只好手动在excel中调整一下数据格式，再read_excel()

rm(list = ls())
require(readxl) # 读excel文件用
require(data.table)  
require(magrittr)  # 只是为了用 `%>%`
require(ggplot2)

stock_num <- "000001SH"  # 此处以000001SH（上证综合）为例，可以改。

fseq <- .Platform$file.sep
file_name <- paste0("JP_stock_file", fseq, stock_num, ".xls")
file_name1 <- paste0("JP_stock_file", fseq, stock_num, ".part1", ".xls")
file_name2 <- paste0("JP_stock_file", fseq, stock_num, ".part2", ".xls")

df <- read_excel(file_name, col_types = rep("text", 5))
df1 <- read_excel(file_name1, col_types = rep("text", 5)) 
df2 <- read_excel(file_name2, col_types = rep("text", 5))

dt <- rbind(df, df1, df2) %>% as.data.table()  

# 2. 整理数据格式，检查有无错误值 ====================================================

# 看有没有重复的行。没有
uniqueN(dt) == nrow(dt) # [1] TRUE

# 看每一天时间是否齐全。
# 股票交易时间：上午时段9:30-11:30，下午时段13:00-15:00
# 所以正常来说每天应该有 4*60+1 = 241 个一分钟（行）
checktime <- dt[, .N, by = TDate][N != 241]
checktime

# 发现有两天时间不齐全：
# TDate   N
# 1: 20140715 240
# 2: 20150821 240
# 进一步查找发现少的两行分别是：
# 2014-07-15 11:22
# 2015-08-21 13:13
# 正好是三个文件衔接的地方。


# 转换日期时间
dt[, DateTime := as.POSIXct(paste(TDate, MinTime), format = "%Y%m%d %H%M", tz = "PRC")]
dt[, TDate := as.Date(TDate, "%Y%m%d")]  

# EndPrc 和 MinTq 转换成数字格式
dt[, EndPrc := as.numeric(EndPrc)]
dt[, MinTq := as.integer(MinTq)]

# 计算收益率
dt[, return := c(NA, diff(EndPrc)/EndPrc[-.N])]

# 3. 研究收益率分布  ====================================================
# 区分in-sample和out-sample, 看一下in sample的收益率分布，
# 再看out of sample 的收益率处于in sample收益率分布的哪个位置。

# 以"2016-05-01"为界，区分in-sample和out-of-sample
# 也可以用其他方法分界：个数or比例

bp_time <- as.Date("2016-05-01")  #以日期为界
bp_num <- 100L  # 100个
bp_prop <- 1e-4  # 千分之一

dt_in <- dt[TDate <= bp_time][-1]  # in sample
dt_out <- dt[TDate > bp_time]  # out of sample

# 看一下return的分布
summary(dt_in$return)
quantile(dt_in$return, probs = seq(0, 1, 0.1)) 

# 画return的直方图
c1 <- ggplot(data = dt_in, aes(x = return)) +
  geom_histogram(binwidth = 1e-4) +
  coord_cartesian(xlim = c(-1e-2, 1e-2)) +
  ggtitle(paste(stock_num, "in the sample"))
c1 

# 还是画return的直方图，调整一下x坐标轴：
c2 <- ggplot(data = dt_in, aes(x = return)) +
  geom_histogram(binwidth = 1e-4) +
  coord_cartesian(xlim = c(-2e-3, 2e-3)) +
  ggtitle(paste(stock_num, "in the sample 2"))
c2

# 然后看一下out of sample 的 return 在什么位置：
c2 + geom_rug(
  data = dt_out, aes(x = return),
  col = "red", alpha = 0.1
) + ggtitle(
  paste(stock_num, "in the sample & out of sample", sep = "__")
)


