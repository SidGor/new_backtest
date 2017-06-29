


###############################new Backtest##################
rm(list = ls())


library(quantex)
library(data.table)
library(pipeR)
library(ggplot2)
library(zoo)
library(rlist)

#product_names <- c("ag","al","au","bu","CF","cs","cu","FG","hc","i","j","jd",
#"jm","l","m","OI","p","pb","pp","rb","RM","ru","SR","TA","y","zn")

product_id           <- "rb"
vm                   <- products[[product_id]]$volume_multiple
slippage             <- 2 * products[[product_id]]$price_tick
fee_rate             <- products[[product_id]]$cost_ratio
fee_rate_fix         <- products[[product_id]]$cost_const

start_date           <- 20100101                     ##########  DATA  ########  
end_date             <- 20161230                     ########  LOADING  ####### 
frequency            <- "5m"

N1                   <- 200
N2                   <- 200

P4                   <- 2
loss_rate            <- 0.04
ATR_rate             <- 0.3
#wait_time            <- 5                          #bars,which means 30 minutes
#sav_date             <- 0
#adj_count            <- 0
position             <- 0L
vec.position         <- vector()
vec.fee              <- vector()
vec.closed_profit    <- vector()
vec.position_profit  <- vector()
closed_profit        <- 0
position_profit      <- 0
fee                  <- 0
sav_stop             <- NA_real_
loss_price           <- NA_real_
high_since_entry     <- NA_real_
low_since_entry      <- NA_real_
enter_price          <- NA_real_
loc                  <- NA_real_
save_low             <- NA_real_
save_high             <- NA_real_

out                  <- list()
trades               <- list()
###############################################################################


######################data#####################################################

data <- as.data.table(readRDS(sprintf("/home/lisida/raw_data/data_%s_freq_%s.rds",product_id,frequency))[[1]])

data <- data[trading_day >= start_date & trading_day <= end_date,]


cdt  <- data[, -c("turnover", "open_interest", "ticks", "rollover")]

cdt[,  number := c(1:nrow(data))]

cdt[,  long_price := (rollapplyr(high, width = N1, FUN = max,
                                 fill = NA) %>>% shift(, n = 1L, type = "lag")) ]

cdt[,  lag_long_price := shift(long_price, n = P4 + 1, type = "lag")]

cdt[,  short_price := (rollapplyr(low, width = N1, FUN = min,
                                  fill = NA) %>>% shift(, n = 1L, type = "lag"))]

cdt[,  lag_short_price := shift(short_price, n = P4 + 1, type = "lag")]

cdt[,  ATR_Hbound := (rollapplyr(high, width = N2, FUN = max,
                                 fill = NA) %>>% shift(, n = 1L, type = "lag")) ]

cdt[,  ATR_Lbound := (rollapplyr(low, width = N2, FUN = min,
                                 fill = NA) %>>% shift(, n = 1L, type = "lag")) ]

cdt[,  ATR := (ATR_Hbound - ATR_Lbound) ]


vec.position = rep(0,nrow(cdt))
vec.fee      = rep(0,nrow(cdt))
vec.closed_profit    = rep(0,nrow(cdt))
vec.position_profit  = rep(0,nrow(cdt))
###############################################################################


##############################generate long sig################################



cdt[, long.sig.1 := shift(high > long_price, n = P4 + 1, type = "lag") ]
cdt[, long.sig.2 := high > shift(long_price, n = P4 + 1, type = "lag")]

long.sigs <- na.fill(cdt$long.sig.1,0) * na.fill(cdt$long.sig.2,0)


cdt[, short.sig.1 := shift(low < short_price, n = P4 + 1, type = "lag") ]
cdt[, short.sig.2 := low < shift(short_price, n = P4 + 1, type = "lag")]

short.sigs <- na.fill(cdt$short.sig.1,0) * na.fill(cdt$short.sig.2,0)

sig.vec <- long.sigs - short.sigs          #空仓表示为负

sig.judg <- sig.vec != 0

sig.pos  <- c(1:length(sig.vec))[sig.judg]


#######################################################################################

jump = 0

#循环取得关键向量
for (f in 1:(length(sig.pos))){
# for (f in 1:2988){
  if (jump == 0){
    start.point = sig.pos[f]
  }  
  
  if(is.na(sig.pos[f+1])){
    temp.cdt <- cdt[start.point : sig.pos[f]]
  }else{
    temp.cdt <- cdt[start.point : sig.pos[f+1],]
  }
    




#判定止损

if (sig.vec[sig.pos[f]] == 1L) {
  
  temp.cdt[, cumhigh := shift(cummax(high), n = 1L, type = "lag")]
  temp.cdt[, stop := cumhigh - ATR * ATR_rate]
  temp.cdt[, stop.t := na.fill(low < stop, FALSE)]
  leave_price = max(temp.cdt[loc]$open, temp.cdt[loc]$stop) + slippage
  #保存enter信息  
  # if (f == 1){
  #   enter_date = temp.cdt[1]$trading_day
  #   enter_ts   = temp.cdt[1]$ts
  #   enter_price = min(temp.cdt[1]$open, temp.cdt[1]$lag_short_price) - slippage
  # }
  # else if (sig.vec[sig.pos[f]] != vec.position[sig.pos[f-1]]){  #判断是开仓的情况下再存
  #   enter_date = temp.cdt[1]$trading_day
  #   enter_ts   = temp.cdt[1]$ts
  #   enter_price = min(temp.cdt[1]$open, temp.cdt[1]$lag_short_price) - slippage
  # }
  if(any(temp.cdt$stop.t) == TRUE){
    loc = match(TRUE,temp.cdt$stop.t)
    # 交易登记
    trade_out <- list(
      enter_date = temp.cdt[1]$trading_day,
      enter_ts   = temp.cdt[1]$ts,
      enter_price = max(temp.cdt[1]$open, temp.cdt[1]$lag_long_price) + slippage,
      leave_date = temp.cdt[loc]$trading_day,
      leave_ts   = temp.cdt[loc]$ts,
      leave_price = min(temp.cdt[loc]$open, temp.cdt[loc]$stop) - slippage,
      side = sig.vec[sig.pos[f]],
      commission = (min(temp.cdt[loc]$open, temp.cdt[loc]$stop) - slippage + max(temp.cdt[1]$open, temp.cdt[1]$lag_short_price) + slippage) * vm * fee_rate + 2 * fee_rate_fix
    ) 
    
    trades <- list.append(trades, trade_out)
    enter_date = NA_real_
    enter_price = NA_real_
    enter_ts    = NA_real_
    jump = 0

    
    
    #修改position
    range.start <- temp.cdt$number[1]
    range.end <- temp.cdt$number[loc]
    vec.position[range.start:(range.end-1)] = sig.vec[sig.pos[f]]
    
    #添加closed_profit
    vec.closed_profit[range.end:length(vec.closed_profit)] = vec.closed_profit[range.start] + 
      vm * (trade_out$leave_price - trade_out$enter_price) * trade_out$side
    
    #添加 position_profit
    vec.position_profit[range.start:range.end - 1] = 
      (cdt$close[range.start:range.end - 1] - trade_out$enter_price) * trade_out$side * vm
    
    rm(trade_out)
    # 重置状态变量      
    
    # vec.position[sig.pos[f] : temp.cdt[loc]$number] = sig.vec[sig.pos[f]]
    # vec.fee[sig.pos[f] : (temp.cdt[loc]$number - 1)] = vec.fee[sig.pos[f] -1] +  trade_out$enter_price * vm * fee_rate + fee_rate_fix #改写fee
    # vec.fee[temp.cdt[loc]$number : length(vec.fee)] = vec.fee[temp.cdt[loc]$number - 1] + trade_out$leave_price * vm * fee_rate + fee_rate_fix #改写fee
    # vec.closed_profit[temp.cdt[loc]$number : length(vec.closed_profit)] = vec.closed_profit[temp.cdt[loc]$number -1] + trade_out$side * (trade_out$leave_price - trade_out$enter_price)*vm
    # vec.position_profit[sig.pos[f] : (temp.cdt[loc]$number - 1)] = vm * trade_out$side * (cdt$close[sig.pos[f] : (temp.cdt[loc]$number - 1)] - trade_out$enter_price)
    # rm(trade_out)
   }else{
    jump = 1  
    if(is.na(sig.pos[f+1])){
      range.start <- temp.cdt$number[1]
      vec.position[range.start:last(temp.cdt$number)] = sig.vec[sig.pos[f]]
    }
  }
  
}else if (sig.vec[sig.pos[f]] == -1L){
  
  temp.cdt[, cumlow := shift(cummin(low), n = 1L, type = "lag")]
  temp.cdt[, stop := cumlow + ATR * ATR_rate]
  temp.cdt[, stop.t := na.fill(high > stop, FALSE)]
  leave_price = max(temp.cdt[loc]$open, temp.cdt[loc]$stop) + slippage
  
  #保存enter信息  
  # if (f == 1){
  #   enter_date = temp.cdt[1]$trading_day
  #   enter_ts   = temp.cdt[1]$ts
  #   enter_price = min(temp.cdt[1]$open, temp.cdt[1]$lag_short_price) - slippage
  # }
  # else if (sig.vec[sig.pos[f]] != vec.position[sig.pos[f-1]]){  #判断是开仓的情况下再存
  #   enter_date = temp.cdt[1]$trading_day
  #   enter_ts   = temp.cdt[1]$ts
  #   enter_price = min(temp.cdt[1]$open, temp.cdt[1]$lag_short_price) - slippage
  # }
  if(any(temp.cdt$stop.t) == TRUE){
    
    loc = match(TRUE,temp.cdt$stop.t)
    # 交易登记
    trade_out <- list(
      enter_date = temp.cdt[1]$trading_day,
      enter_ts   = temp.cdt[1]$ts,
      enter_price = min(temp.cdt[1]$open, temp.cdt[1]$lag_short_price) - slippage,
      leave_date = temp.cdt[loc]$trading_day,
      leave_ts   = temp.cdt[loc]$ts,
      leave_price = max(temp.cdt[loc]$open, temp.cdt[loc]$stop) + slippage,
      side = sig.vec[sig.pos[f]],
      commission = (max(temp.cdt[loc]$open, temp.cdt[loc]$stop) + slippage + min(temp.cdt[1]$open, temp.cdt[1]$lag_short_price) - slippage) * vm * fee_rate + 2 * fee_rate_fix
    ) 
    
    trades <- list.append(trades, trade_out)
    enter_date = NA_real_
    enter_price = NA_real_
    enter_ts    = NA_real_
    jump        = 0
    
    # 重置状态变量      
    # vec.position[sig.pos[f] : temp.cdt[loc]$number] = sig.vec[sig.pos[f]]
    # vec.fee[sig.pos[f] : (temp.cdt[loc]$number - 1)] = vec.fee[sig.pos[f] -1] +  trade_out$enter_price * vm * fee_rate + fee_rate_fix #改写fee
    # vec.fee[temp.cdt[loc]$number : length(vec.fee)] = vec.fee[temp.cdt[loc]$number - 1] + trade_out$leave_price * vm * fee_rate + fee_rate_fix #改写fee
    # vec.closed_profit[temp.cdt[loc]$number : length(vec.closed_profit)] = vec.closed_profit[temp.cdt[loc]$number -1] + trade_out$side * (trade_out$leave_price - trade_out$enter_price)*vm
    # vec.position_profit[sig.pos[f] : (temp.cdt[loc]$number - 1)] = vm * trade_out$side * (cdt$close[sig.pos[f] : (temp.cdt[loc]$number - 1)] - trade_out$enter_price)
    # rm(trade_out)
    
    #添加position_profit
    range.start <- temp.cdt$number[1]
    range.end <- temp.cdt$number[loc]
    vec.position[range.start:(range.end-1)] = sig.vec[sig.pos[f]]
    #添加closed_profit
    vec.closed_profit[range.end:length(vec.closed_profit)] = vec.closed_profit[range.start] + 
      vm * (trade_out$leave_price - trade_out$enter_price) * trade_out$side
    
    #添加 position_profit
    vec.position_profit[range.start:range.end - 1] = 
      (cdt$close[range.start:range.end - 1] - trade_out$enter_price) * trade_out$side * vm
    
    rm(trade_out)
  }else {
    jump = 1
    if(is.na(sig.pos[f+1])){
      range.start <- temp.cdt$number[1]
      vec.position[range.start:last(temp.cdt$number)] = sig.vec[sig.pos[f]]
    }
    
  }
  
}# end of else if(sig.vec:...)
# testing ：temp.cdt [, stop.t := c(FALSE,TRUE,TRUE,FALSE)]


# end of else if(sig.vec:...)
# testing ：temp.cdt [, stop.t := c(FALSE,TRUE,TRUE,FALSE)]


rm(temp.cdt)

} #end of for (f in 1:length(sig.pos))


###############################################################################

##########################generate trades_dt##################################

trades_dt <- list.stack(trades, data.table = TRUE)

out_dt <- data.table(
  date = cdt$trading_day,
  ts  = cdt$ts,
  position = vec.position,
  position_profit = vec.position_profit,
  closed_profit = vec.closed_profit,
  fee = vec.fee,
  close = cdt$close,
  market_value = cdt$close * vm
)

out_dt <- out_dt[N1 + 1:nrow(out_dt)]

out_dt[, net_profit := closed_profit + position_profit - fee]
out_dt[, profit := c(0,diff(net_profit))] 
out_dt[, cum_profit := cumsum(profit)]
out_dt[, cummax_cum_profit := cummax(cum_profit)]
out_dt[, drawdown := cum_profit - cummax_cum_profit]

plot(out_dt$net_profit, type = "l")
#########################functions#############################################


