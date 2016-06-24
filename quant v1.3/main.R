# 量化金融项目
# By: JiaRu, jiaru2014@126.com
# R version 3.2.3
# Platform: x86_64-apple-darwin13.4.0 (64-bit)
# Date: 2016-06-22
# 
# Version 1.3
# ===================================================================
# 主程序

rm(list = ls())

load("dt.Rdata")  # 000001 stock. class data.table 
source("Trade.R")
source("Visualize.R")

tradebook <- Trade(
  dt,   # data.table
  insample = c("2014-01-01", "2015-04-01"),
  outsample = c("2015-04-01", "2015-04-30"),
  Profit.rate = 0.05,
  Stop.rate = 0.02,
  Sell.rt.q = 0.001,
  Buy.rt.q = 0.9,
  Buy.vol.q = 0.7
)

Visualize(tradebook, dt)