library(readr)
library(dplyr, quietly = TRUE)
library(fBasics)

setwd("~/MFE/Term3/431 Quantitative Asset Management/HW2")
CRSP_Bonds = read.csv("CRSP_Bonds.csv")[,-1]
CRSP_Bonds$MCALDT = as.Date(CRSP_Bonds$MCALDT, format="%m/%d/%Y")
Monthly_CRSP_Stocks = read.csv("Monthly_CRSP_Stocks.csv")

# Details of Information present in the file:
# KYCRSPID     Bond ID
# MCALDT       Date
# TMRETNUA     Monthly Unadjusted Return
# TMTOTOUT     Total Amount Outstanding (in millions of dollars)

# 1. Bond Market Return Index

PS2_Q1 = function(Input){
     
     # Cleaning the Input file
     Input = mutate(group_by(Input, KYCRSPID), MCap_lag = lag(TMTOTOUT))
     Input$MCap_lag[is.na(Input$MCap_lag)] = Input$TMTOTOUT[is.na(Input$MCap_lag)]
     Input = Input[!(Input$TMRETNUA == -99),]
     Input$MCap_lag[is.na(Input$MCap_lag)] = 0
     
     # Filling in Output values
     Output = summarise(group_by(Input, MCALDT), 
                        Bond_lag_MV = sum(MCap_lag), 
                        Bond_Ew_Ret = mean(TMRETNUA), 
                        Bond_Vw_Ret = sum(MCap_lag*TMRETNUA)/Bond_lag_MV)
     Output$Year = as.integer(format(Output$MCALDT, "%Y"))
     Output$Month = as.integer(format(Output$MCALDT, "%m"))
     
     Output[,c("Year", "Month", "Bond_lag_MV", "Bond_Ew_Ret", "Bond_Vw_Ret")]
}

Monthly_CRSP_Bonds = PS2_Q1(CRSP_Bonds)

####################################################################

# 2. Aggregate stock, bond, and riskless datatables

Monthly_CRSP_Riskless = read.csv("Monthly_CRSP_Riskless.csv")
Monthly_CRSP_Riskless$caldt = as.Date(as.character(Monthly_CRSP_Riskless$caldt), format="%Y%m%d")
Monthly_CRSP_Riskless = Monthly_CRSP_Riskless[!is.na(Monthly_CRSP_Riskless$t30ret),]

PS2_Q2 = function(Stock, Bond, Riskless){
     Stock$date = Stock$Year*100 + Stock$Month
     Bond$date = Bond$Year*100 + Bond$Month
     Riskless$date = as.integer(format(Riskless$caldt, "%Y"))*100 + as.integer(format(Riskless$caldt, "%m"))
     Output = merge(merge(
          Stock[,c("Year", "Month", "Stock_lag_MV", "Stock_Vw_Ret","date")],
          Bond[,c("Bond_lag_MV", "Bond_Vw_Ret", "date")], by="date"),
          Riskless[,c("t30ret", "t90ret", "date")], by="date")[,-1]
     Output$Stock_Excess_Vw_Ret = Output$Stock_Vw_Ret - Output$t90ret
     Output$Bond_Excess_Vw_Ret = Output$Bond_Vw_Ret - Output$t90ret
     
     Output[,c("Year", "Month", "Stock_lag_MV", "Stock_Excess_Vw_Ret", "Bond_lag_MV", "Bond_Excess_Vw_Ret")]

}

Monthly_CRSP_Universe = PS2_Q2(Monthly_CRSP_Stocks, Monthly_CRSP_Bonds, Monthly_CRSP_Riskless)

#######################################################################

# 3. unlevered and levered risk-parity portfolio returns

PS2_Q3 = function(Input){
     
     # Creating an Output File
     Output = Input[,c("Year","Month", "Stock_Excess_Vw_Ret", "Bond_Excess_Vw_Ret")]
     Output$Excess_Vw_Ret = (Input$Stock_lag_MV*Input$Stock_Excess_Vw_Ret + Input$Bond_lag_MV*Input$Bond_Excess_Vw_Ret)/(Input$Stock_lag_MV+Input$Bond_lag_MV)
     Output$Excess_60_40_Ret = 0.6*Input$Stock_Excess_Vw_Ret + 0.4*Input$Bond_Excess_Vw_Ret
     
     Output$Stock_inverse_sigma_hat = NA
     Output$Bond_inverse_sigma_hat = NA
     
     for (i in 1:nrow(Input)){
          Output$Stock_inverse_sigma_hat[i] = 1/sd(Input$Stock_Excess_Vw_Ret[max(1,i-36):max(1,i-1)])
          Output$Bond_inverse_sigma_hat[i] = 1/sd(Input$Bond_Excess_Vw_Ret[max(1,i-36):max(1,i-1)])
     }
     
     # Putting the third value in first 2 rows
     Output[is.na(Output$Stock_inverse_sigma_hat),c(ncol(Output)-1, ncol(Output))] = Output[sum(is.na(Output$Stock_inverse_sigma_hat))+1,c(ncol(Output)-1, ncol(Output))]
     
     Output$Unlevered_k = 1/(Output$Stock_inverse_sigma_hat + Output$Bond_inverse_sigma_hat)
     Output$Excess_Unlevered_RP_Ret = Output$Unlevered_k*(Output$Stock_inverse_sigma_hat*Output$Stock_Excess_Vw_Ret + Output$Bond_inverse_sigma_hat*Output$Bond_Excess_Vw_Ret)
     
     Input$Levered_Port = Output$Stock_inverse_sigma_hat*Input$Stock_Excess_Vw_Ret + Output$Bond_inverse_sigma_hat*Input$Bond_Excess_Vw_Ret
     
     Output$Levered_k = sd(Output$Excess_Vw_Ret)/sd(Input$Levered_Port)
     Output$Excess_Levered_RP_Ret = Output$Levered_k*Input$Levered_Port
     
     Output
}

Port_Rets = PS2_Q3(Monthly_CRSP_Universe)

#######################################################################

# 4. Panel A

PS2_Q4 = function(Input){
     
     Col.N = c("Stock_Excess_Vw_Ret", "Bond_Excess_Vw_Ret", "Excess_Vw_Ret", "Excess_60_40_Ret", "Excess_Unlevered_RP_Ret", "Excess_Levered_RP_Ret")
     
     # Segregating data till June 2010
     Input = Input[(Input$Year*100 + Input$Month <= 201006),Col.N]
     
     Output = data.frame("Excess_Return" = apply(Input, 2, mean))*12
     Output$Volatility = apply(Input, 2, sd)*sqrt(12)
     Output$t_stat_of_Excess_Return = (Output$Excess_Return/12)/(Output$Volatility/sqrt(12))*sqrt(nrow(Input))
     
     Output$Sharpe_Ratio = Output$Excess_Return/Output$Volatility
     # Output$Skewness = apply(((Input - matrix(apply(Input, 2, mean), nrow=1))/(Output$Volatility/sqrt(12)))^3, 2, mean)
     # Output$Excess_Kurtosis = apply(((Input - t(Output$Excess_Return/12))/(Output$Volatility*sqrt(12)))^4, 2, mean) - 3
     Output$Skewness = colSkewness(Input)
     Output$Excess_Kurtosis = colKurtosis(Input)
     
     Output
}

Table2 = PS2_Q4(Port_Rets)
