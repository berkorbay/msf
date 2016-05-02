#Author: Berk Orbay 

#This code snippet is irrelevant to the model, just to clean the workspace
#rm(list=ls(all=TRUE))

#European Option Pricing with Black and Scholes
BS_EOPT<-function(s0,K,r,T,sig,callOrPut,div){
	d1<- (log(s0/K) + (r - div + sig^2/2)*T)/(sig*sqrt(T))
	d2<- d1 - sig*sqrt(T)
	if(callOrPut=="call"){
		return(s0*exp(-div*T)*pnorm(d1) - K*exp(-r*T)*pnorm(d2))
	}else{
		return(K*exp(-r*T)*pnorm(-d2) - s0*exp(-div*T)*pnorm(-d1))
	}
}

BS_Bulk<-function(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset,data_year,hist_vol_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/"){
	option_specs<-read.csv(paste0(input_path,"Asset Options/",underlying_asset,"_",data_year,"_options_filtered_",filter_suffix,".csv"),header=TRUE)
	asset_data<-read.csv(paste0(input_path,"Asset Prices/",underlying_asset,"_daily_processed_histvol.csv"),header=TRUE)
	VolStream<-asset_data[match(option_specs$DataDate,asset_data$Date),paste0("HistVol",hist_vol_period,"y")]
	BS_prices<-mapply(BS_EOPT,s0=option_specs$UnderlyingPrice,K=option_specs$Strike,T=option_specs$NetMaturity/252,sig=VolStream*sqrt(252),callOrPut=option_specs$Type,r=option_specs$RiskFreeRate,div=option_specs$Dividend.Yield)
	price_output<-data.frame(option_specs[,c(1,2,4,5,7,8,9,14,20,23,24,25)],BS.HV2y=round(BS_prices,2),HV2y=VolStream*sqrt(252))
	write.table(price_output,paste0(output_path,underlying_asset,"_",data_year,"_BS_data_withdiv_",hist_vol_period,"y.csv"),row.names=FALSE,sep=",")	
}



for(i in 2013:2009){
	print(i)
	BS_Bulk(filter_suffix="A12",underlying_asset="SPX",data_year=i,hist_vol_period=5)
	BS_Bulk(filter_suffix="A12",underlying_asset="SPX",data_year=i,hist_vol_period=2)
	BS_Bulk(filter_suffix="A12",underlying_asset="SPX",data_year=i,hist_vol_period=3)

}
# BS_Bulk(filter_suffix="A12",underlying_asset="SPX",data_year=2013,hist_vol_period=2)
# BS_Bulk(filter_suffix="A12",underlying_asset="SPX",data_year=2012,hist_vol_period=2)
# BS_Bulk(filter_suffix="A12",underlying_asset="SPX",data_year=2011,hist_vol_period=2)
# BS_Bulk(filter_suffix="A12",underlying_asset="SPX",data_year=2010,hist_vol_period=2)

setwd("~/Documents/")
options(repos="http://cran.rstudio.com/")
install.packages("RND")
library(RND)

input_path="~/Dropbox/PhD_Workshop/Input Files/"
filter_suffix="A12"
underlying_asset="SPX"
data_year=2013
hist_vol_period=2
output_path="~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/"

option_specs<-read.csv(paste0(input_path,"Asset Options/",underlying_asset,"_",data_year,"_options_filtered_",filter_suffix,".csv"),header=TRUE)
asset_data<-read.csv(paste0(input_path,"Asset Prices/",underlying_asset,"_daily_processed_histvol.csv"),header=TRUE)


#### IV calculation
only_calls<-which(option_specs$Type == "call")

qw<-18396
qw<-only_calls[10]
option_specs[qw,]
print("Net Maturity Example")
compute.implied.volatility(option_specs$RiskFreeRate[qw],option_specs$NetMaturity[qw]/252,option_specs$UnderlyingPrice[qw],option_specs$Strike[qw],option_specs$Dividend.Yield[qw],option_specs$Last[qw],0,3)
print("Raw Maturity Example")
compute.implied.volatility(option_specs$RiskFreeRate[qw],option_specs$Maturity[qw]/365,option_specs$UnderlyingPrice[qw],option_specs$Strike[qw],option_specs$Dividend.Yield[qw],option_specs$Last[qw],0,3)
print("From Data")
option_specs$IV[qw]

#####

option_day<-"2013-04-19"
option_subset<- option_specs %>%
					filter(DataDate == option_day)

maturity_spec<-table(option_subset$NetMaturity)
qa<-6


option_subset<-option_subset %>%
					filter(NetMaturity == as.numeric(names(maturity_spec)[qa]))

call_subset<-option_subset %>%
					filter(Type=="call")

put_subset<-option_subset %>%
					filter(Type=="put")

bsm_params<-extract.bsm.density(r=option_subset$RiskFreeRate[1],y=option_subset$Dividend.Yield[1],te=as.numeric(names(maturity_spec)[qa])/252,s0=option_subset$UnderlyingPrice[1],
	market.calls=call_subset$Last,call.strikes=call_subset$Strike,
	market.puts=put_subset$Last,put.strikes=put_subset$Strike)

# actual.mu     = log(s0) + ( r - y - 0.5 * sigma^2) * te
#   actual.zeta   = sigma * sqrt(te)


mu<-bsm_params$mu
zeta<-bsm_params$zeta

	# d1<- (log(s0/K) + (r - div + sig^2/2)*T)/(sig*sqrt(T))
	# d2<- d1 - sig*sqrt(T)

results_path<-"~/Dropbox/PhD_Workshop/Output Files/"

myresults<-read.table(paste0(results_path,"Black_Scholes/SPX_2013_BS_data_withdiv_5y.csv"),sep=",",header=TRUE)

resulted_options<-myresults %>%
	filter(DataDate == option_day & NetMaturity == as.numeric(names(maturity_spec)[qa]))


for(i in 1:dim(option_subset)[1]){
	print(i)
	print(paste0("Moneyness is: ",option_subset$UnderlyingPrice[1]/option_subset$Strike[i]," ",option_subset$Type[i]))
	print(round(RND_BSM(s0=option_subset$UnderlyingPrice[1],K=option_subset$Strike[i],r=option_subset$RiskFreeRate[1],div=option_subset$Dividend.Yield[1],
		T=as.numeric(names(maturity_spec)[qa])/252,callOrPut=option_subset$Type[i],mu=mu,zeta=zeta),2))
	print("Estimate from historical prices")
	print(resulted_options[i,"BS.HV2y"])

}

RND_BSM(1500,1500,0,0,"call",0.5,7,0.2)

RND_BSM<-function(s0,K,r,div,callOrPut,T,mu,zeta){
	print("Estimate from option prices")
	d2 <- (mu-log(s0)+log(s0/K))/zeta
	d1 <- d2 + zeta
	if(callOrPut=="call"){
		s0*exp(-div*T)*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
	}else{
		K*exp(-r*T)*pnorm(-d2) - s0*exp(-div*T)*pnorm(-d1)
	}
}



#######Unsuccessful risk free rate and dividend rate extraction
library(dplyr)

option_types<-option_specs %>%
	filter(DataDate =="2013-04-19") %>%
	arrange(Strike,NetMaturity) %>%
	group_by(Strike,NetMaturity)

group_type<-NULL
for(i in 1:length(group_size(option_types))){
	if(group_size(option_types)[i]==2){
		group_type<-c(group_type,rep(2,2))
	}else{
		group_type<-c(group_type,1)
	}
}

option_types<- cbind(option_types,Groups=group_type)
option_types <- option_types %>% filter(Groups==2 & NetMaturity== 44)


option.c<- option_types %>%
	filter(Type=="call" & DataDate =="2013-04-19") %>%
	arrange(desc(Last)) %>%
	select(Call=Last)

option.p<- option_types %>%
	filter(Type=="put" & DataDate =="2013-04-19") %>%
	arrange(Last) %>%
	select(Put=Last)

option.s<- option_types %>%
	filter(Type=="call" & DataDate =="2013-04-19") %>%
	arrange(Strike) %>%
	select(Strike)

s0<-option_types %>% 
	filter(DataDate=="2013-04-19") %>%
	select(UnderlyingPrice)

s0<-unlist(s0[1,])

te<-44/252

extract.rates(unlist(option.c$Call),unlist(option.p$Put),unlist(s0),unlist(option.s),unlist(te))

####Extracting rates result in heavy negative rf and dividend rates FAIL

