library(readr)
library(dplyr)
library(roll)
library(data.table)
library(fBasics)
library(StatMeasures)
library(reshape2)

# 1. Construct VWRETD

setwd("~/MFE/Term3/431 Quantitative Asset Management/HW3")
CRSP_Stocks = data.table(read_csv("CRSP_Stocks.csv"))
CRSP_Stocks$date = as.Date(as.character(CRSP_Stocks$date), format="%Y%m%d")
CRSP_Stocks$RET = as.numeric(CRSP_Stocks$RET)

# Details of Information present in the file:
# PERMNO	Permanent Company Number
# date	Date
# SHRCD	Share Code
# EXCHCD	Exchange Code
# RET	Holding Period Return
# DLRET	Delisting Return
# PRC	Price (-ve price indicates share not traded that day)
# SHROUT	Number of Shares Outstanding (in thousands)

PS3_Q1 = function(Input){
     
     # Filterning universe of stocks and cleaning data
     Input = Input[((Input$SHRCD == 10) | (Input$SHRCD == 11)),]
     Input = Input[(Input$EXCHCD == 1 | Input$EXCHCD == 2 | Input$EXCHCD == 3),]
     Input = Input[!is.na(Input$PERMNO),]
     
     Input$lag_Mkt_Cap = lag(abs(Input$PRC) * Input$SHROUT)/1000
     Input = Input[!is.na(Input$lag_Mkt_Cap),]
     Input$RET[is.na(Input$RET)] = 0; Input$DLRET[is.na(Input$DLRET)] = 0
     Input$Ret = log(1+Input$RET) + log(1+Input$DLRET)
     
     Input = Input %>% group_by(PERMNO) %>% mutate(Ranking_Ret = roll_sum(x = matrix(Ret), width = 13, min_obs = 13, na_restore = NA) - roll_sum(x = matrix(Ret), width = 2, min_obs = 2, na_restore = NA))
     Input = Input %>% group_by(PERMNO) %>% mutate(P13 = lag(PRC,13))
     Input = Input[!is.na(Input$Ranking_Ret) & !is.na(Input$P13),]
     Input$Year = as.integer(format(Input$date, "%Y"))
     Input$Month = as.integer(format(Input$date, "%m"))
     Input = Input[(Input$Year >= 1927) & (Input$Year <= 2018),]
     
     Input[,c("Year", "Month", "PERMNO", "EXCHCD", "lag_Mkt_Cap", "Ret", "Ranking_Ret")]
}

CRSP_Stocks_Momentum = PS3_Q1(CRSP_Stocks)


###############################################################################

# 2. Monthly Momentum Portfolio Decile

PS3_Q2 = function(Input){
     
     Input$date = Input$Year*100 + Input$Month
     Input = Input[order(Input$date, Input$EXCHCD),]
     Input = Input %>% group_by(date) %>% mutate(DM_decile = decile(Ranking_Ret))
     Input = Input %>% group_by(date, EXCHCD) %>% mutate(KRF_decile = decile(Ranking_Ret))
     
     D = data.frame("date" = unique(Input$date))
     D = cbind(D, matrix(NA, ncol=11, nrow = nrow(D)))
     
     for(i in 1:nrow(D)){
          X = Input[(Input$date == D$date[i]) & (Input$EXCHCD ==1),]
          D[i,-1] = as.numeric(quantile(X$Ranking_Ret, c(0:10)/10))
     }
     
     Input = merge(Input, D, by="date")
     Input$KRF_decile = apply(matrix(rep(Input$Ranking_Ret, each=11), ncol=11, byrow=TRUE) > Input[,c((ncol(Input)-10):ncol(Input))], 1, sum)
     
     Input[,c("Year", "Month", "PERMNO", "lag_Mkt_Cap", "Ret", "DM_decile", "KRF_decile")]
}

CRSP_Stocks_Momentum_decile = PS3_Q2(CRSP_Stocks_Momentum)

###############################################################################

# 3. Monthly Momentum Portfolio Decile Returns

FF_mkt = read.csv("F-F_Research_Data_Factors.CSV", skip =3, stringsAsFactors = FALSE)
FF_mkt = data.frame(FF_mkt[c(1:1112),])
FF_mkt = as.data.frame(sapply(FF_mkt, as.numeric))
colnames(FF_mkt) = c("date", "Market_minus_Rf", "SMB", "HML", "Rf")
FF_mkt$Year = as.integer(FF_mkt$date/100)
FF_mkt$Month = as.integer(FF_mkt$date %% 100)
FF_mkt = FF_mkt[,c("Year", "Month", "Market_minus_Rf","SMB", "HML", "Rf")]

PS3_Q3 = function(Input_Mom, Input_FF){
     
     Input_Mom$date = Input_Mom$Year*100 + Input_Mom$Month
     Input_FF$date = Input_FF$Year*100 + Input_FF$Month
     Input = merge(Input_Mom, Input_FF[,c(-1,-2)], by="date")
     
     Output_DM = summarise(group_by(Input_Mom, date, DM_decile),
                           lag_MCap = sum(lag_Mkt_Cap),
                           DM_Ret = sum(lag_Mkt_Cap*Ret)/lag_MCap)
     Output_KRF = summarise(group_by(Input_Mom, date, KRF_decile),
                            lag_MCap = sum(lag_Mkt_Cap),
                            KRF_Ret = sum(lag_Mkt_Cap*Ret)/lag_MCap)
     Output_DM$C = Output_DM$date*100 + Output_DM$DM_decile
     Output_KRF$C = Output_KRF$date*100 + Output_KRF$KRF_decile
     Out = merge(Output_DM, Output_KRF[,-1], by="C")[-1]
     Out = merge(Out, Input_FF[,c("date","Rf")], by="date")
     
     colnames(Out)[2] = "decile"
     Out$Year = as.integer(Out$date/100)
     Out$Month = as.integer(Out$date %% 100)
     
     Out[,c("Year", "Month", "decile", "DM_Ret", "KRF_Ret", "Rf")]
}

CRSP_Stocks_Momentum_returns = PS3_Q3(CRSP_Stocks_Momentum_decile, FF_mkt)


###############################################################################

# 4. Table 1

PS3_Q4 = function(Input){
     
     Output = summarise(group_by(Input, decile),
                        r_rf = mean(DM_Ret - Rf/100)*12,
                        sd = sd(DM_Ret - Rf/100)*sqrt(12),
                        SR = r_rf/sd,
                        sk = skewness(DM_Ret)
     )
     WML = Input[Input$decile == 10, "DM_Ret"] - Input[Input$decile == 1, "DM_Ret"]
     Output = data.frame(t(Output[,-1]))
     colnames(Output) = paste("Decile", 1:10, sep = " ")
     Output$WML = Output$`Decile 10` - Output$`Decile 1`
     Output$WML[2] = sd(WML)*sqrt(12)
     Output$WML[3] = Output$WML[1]/Output$WML[2]
     WML1 = log(1+WML + Input[Input$decile == 10, "Rf"]/100)
     Output$WML[4] = skewness(WML1)
     
     Output
}

CRSP_Stocks_Momentum_table = PS3_Q4(CRSP_Stocks_Momentum_returns)



###############################################################################

# 5. Correlation

DM_returns = read_table2("m_m_pt_tot.txt", col_names = FALSE)
DM_returns = DM_returns[,c(1:3)]
colnames(DM_returns) = c("date", "decile", "DM_Ret")
DM_returns$Year = as.integer(DM_returns$date/10000)
DM_returns$Month = as.integer(DM_returns$date/100) - DM_returns$Year*100
DM_returns = DM_returns[,c("Year", "Month", "decile", "DM_Ret")]

KRF_returns = read.csv("10_Portfolios_Prior_12_2.CSV", skip=10, stringsAsFactors = FALSE)[1:1107,]
colnames(KRF_returns) = c("date", c(1:10))
KRF_returns$Year = as.integer(as.integer(KRF_returns$date)/100)
KRF_returns$Month = as.integer(KRF_returns$date) %% 100
KRF_returns = melt(KRF_returns[,-1], id = c("Year", "Month"))
colnames(KRF_returns) = c("Year","Month","decile", "KRF_Ret")
KRF_returns$KRF_Ret = as.integer(KRF_returns$KRF_Ret)/100

PS3_Q5 = function(Input_Mom, Input_DM, Input_KRF){
     
     Input_Mom$date_decile = Input_Mom$Year*10000 + Input_Mom$Month*100 + Input_Mom$decile
     Input_DM$date_decile = Input_DM$Year*10000 + Input_DM$Month*100 + Input_DM$decile
     Input_KRF$date_decile = Input_KRF$Year*10000 + Input_KRF$Month*100 + Input_KRF$decile
     
     Input = merge(merge(Input_Mom, Input_DM, by="date_decile"), 
                   Input_KRF, by="date_decile")
     
     Output = summarise(group_by(Input, decile),
                        DM_correlation = cor(DM_Ret.x, DM_Ret.y),
                        KRF_correlation = cor(KRF_Ret.x, KRF_Ret.y))
     WML = Input[Input$decile == 10,] - Input[Input$decile == 1,]
     Output[11,] = c("WML", cor(WML$DM_Ret.x, WML$DM_Ret.y), cor(WML$KRF_Ret.x, WML$KRF_Ret.y))
     
     Output = data.frame(t(Output[,-1]))
     colnames(Output) = c(paste("Decile", 1:10, sep = " "), "WML")
     
     Output
}

CRSP_Stocks_Momentum_correlation = PS3_Q5(CRSP_Stocks_Momentum_returns, DM_returns, KRF_returns)
