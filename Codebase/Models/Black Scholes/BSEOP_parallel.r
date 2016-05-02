#Author: Berk Orbay 

#This code snippet is irrelevant to the model, just to clean the workspace
#rm(list=ls(all=TRUE))

main_path <- "~/Dropbox/PhD_Workshop/"
source(paste0(main_path,"Codebase/00_run_this_first_option_calculations.r"))


# #European Option Pricing with Black and Scholes
# BS_EOPT<-function(s0,K,r,T,sig,callOrPut,div){
# 	d1<- (log(s0/K) + (r - div + sig^2/2)*T)/(sig*sqrt(T))
# 	d2<- d1 - sig*sqrt(T)
# 	if(callOrPut=="call"){
# 		return(s0*exp(-div*T)*pnorm(d1) - K*exp(-r*T)*pnorm(d2))
# 	}else{
# 		return(K*exp(-r*T)*pnorm(-d2) - s0*exp(-div*T)*pnorm(-d1))
# 	}
# }

# BS_Bulk<-function(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset,data_year,hist_vol_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/",voltype="yearly"){
# 	option_specs<-read.csv(paste0(input_path,"Asset Options/",underlying_asset,"_",data_year,"_options_filtered_",filter_suffix,".csv"),header=TRUE)
# 	asset_data<-read.csv(paste0(input_path,"Asset Prices/",underlying_asset,"_daily_processed_histvol.csv"),header=TRUE)
# 	VolStream<-asset_data[match(option_specs$DataDate,asset_data$Date),paste0("HistVol",hist_vol_period,"y")]
# 	VolStream<-VolStream*ifelse(voltype=="yearly",1,sqrt(252))
# 	BS_prices<-mapply(BS_EOPT,s0=option_specs$UnderlyingPrice,K=option_specs$Strike,T=option_specs$NetMaturity/252,sig=VolStream,callOrPut=option_specs$Type,r=option_specs$RiskFreeRate,div=option_specs$Dividend.Yield)
# 	price_output<-data.frame(option_specs[,c(1,2,4,5,7,8,9,14,20,23,24,25)],BS.HV2y=round(BS_prices,2),HV2y=VolStream*sqrt(252))
# 	write.table(price_output,paste0(output_path,underlying_asset,"_",data_year,"_BS_data_withdiv_",hist_vol_period,"y.csv"),row.names=FALSE,sep=",")	
# }


BS_EOPT_with_delta<-function(s0,K,r,T,sig,callOrPut,div){
	T<- max(T/252,1/252)
	d1<- (log(s0/K) + (r - div + sig^2/2)*T)/(sig*sqrt(T))
	d2<- d1 - sig*sqrt(T)
	if(callOrPut=="call"){
		BS_prices_raw <- s0*exp(-div*T)*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
		BS_Delta_raw <- pnorm(d1)
	}else{
		BS_prices_raw <- K*exp(-r*T)*pnorm(-d2) - s0*exp(-div*T)*pnorm(-d1)
		BS_Delta_raw <- -pnorm(-d1)
	}
	return(data.frame(BS_prices_raw,BS_Delta_raw))
}


# input_path="Input Files/"
# filter_suffix="C12"
# underlying_asset<-"SPX"
# data_year<-2008
# parameter_period=2
# sym=TRUE
# output_path="~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/"
# progressOutput=TRUE



BS_Delta_Bulk<-function(input_path="Input Files/",filter_suffix="B12",underlying_asset,data_year,parameter_period=2,output_path="Output Files/Black_Scholes/",progressOutput=TRUE){

	option_input_path<-paste0(main_path,input_path,"Asset Position Raw/")	
	asset_input_path<-paste0(main_path,input_path,"Asset Prices/")
	dividend_input_path<-paste0(main_path,input_path,"Asset Specials/")
	rates_data_path<-paste0(main_path,input_path,"Other/")
	output_path<-paste0(main_path,output_path)

	load(paste0(option_input_path,underlying_asset,"_",data_year,"_price_delta_raw.RData")) #This function brings delta_data

	asset_data<-read.csv(paste0(asset_input_path,underlying_asset,"_daily_processed_histvol.csv"),header=TRUE) %>%
						tbl_df %>%
						mutate(DataDate=as.Date(Date,format=ifelse(nchar(as.character(Date))==8,"%d/%m/%y","%Y-%m-%d"))) %>% 
						select(DataDate,HistVol=contains(paste0(parameter_period,"y"))) 

	if(underlying_asset=="SPX"){
		asset_data<- asset_data %>% mutate(HistVol=HistVol*sqrt(252))
	}


	colnames(asset_data)<-gsub(paste0(parameter_period,"y"),"",colnames(asset_data))

	option_specs <- delta_data %>%
						left_join(.,asset_data,by="DataDate")

	mdply_data <- option_specs %>%
						ungroup %>%
						select(s0=UnderlyingPrice,
							    K=Strike,
							   T=NetMaturity,
							     r=RiskFreeRate,
							   div=Dividend.Yield,
							   callOrPut=Type,
							   sig=contains("HistVol")
							   ) 

	price_delta<-mdply(mdply_data,.fun=BS_EOPT_with_delta,.inform=FALSE,.parallel=TRUE) %>% 
						select(BS_prices_raw,BS_Delta_raw) %>%
						mutate(BS_prices=ifelse(BS_prices_raw < 0,0,BS_prices_raw)) %>%
						mutate(BS_Delta=ifelse(BS_Delta_raw > 1,1,BS_Delta_raw),BS_Delta=ifelse(BS_Delta_raw < -1,-1,BS_Delta_raw)) %>%
						cbind(option_specs,.) %>% 
						tbl_df  %>% 
						select(
								OptionSymbol,
								DataDate,
								Type,
								UnderlyingPrice,
								Strike,
								Moneyness,
								Time_Moneyness,
								NetMaturity,
								Last,
								BS_prices,
								BS_Delta,
								RealExpiration,
								ExpirationPrice,
								ExpirationMoneyness,
								RiskFreeRate,
								Dividend.Yield,
								Maturity,
								HistVol,
								IV,
								DataDelta,
								BS_prices_raw,
								BS_Delta_raw
								)

	save(price_delta,file=paste0(output_path,underlying_asset,"_",data_year,"_BS_data_withdiv_",parameter_period,"y_with_delta.RData"))

}


for(i in 2008:2013){
	BS_Delta_Bulk(filter_suffix="C12",underlying_asset="NDX",data_year=i,parameter_period=2,progressOutput=TRUE)
	BS_Delta_Bulk(filter_suffix="C12",underlying_asset="NDX",data_year=i,parameter_period=5,progressOutput=TRUE)
}

for(i in 2008:2013){
	BS_Delta_Bulk(filter_suffix="C12",underlying_asset="SPX",data_year=i,parameter_period=2,progressOutput=TRUE)
	BS_Delta_Bulk(filter_suffix="C12",underlying_asset="SPX",data_year=i,parameter_period=5,progressOutput=TRUE)
}

# BS_Delta_Bulk(filter_suffix="B12",underlying_asset="SPX",data_year=2010,parameter_period=5,progressOutput=TRUE)

# for(i in 2013:2008){
# 	print(i)
# 	print("1 year lookback starting")
# 	BS_Bulk(filter_suffix="A12",underlying_asset="NDX",data_year=i,hist_vol_period=1,voltype="yearly")
# 	print("2 years lookback starting")
# 	BS_Bulk(filter_suffix="A12",underlying_asset="NDX",data_year=i,hist_vol_period=2,voltype="yearly")
# 	print("3 years lookback starting")
# 	BS_Bulk(filter_suffix="A12",underlying_asset="NDX",data_year=i,hist_vol_period=3,voltype="yearly")
# 	print("5 years lookback starting")
# 	BS_Bulk(filter_suffix="A12",underlying_asset="NDX",data_year=i,hist_vol_period=5,voltype="yearly")

# }
# BS_Bulk(filter_suffix="A12",underlying_asset="SPX",data_year=2013,hist_vol_period=2)
# BS_Bulk(filter_suffix="A12",underlying_asset="SPX",data_year=2012,hist_vol_period=2)
# BS_Bulk(filter_suffix="A12",underlying_asset="SPX",data_year=2011,hist_vol_period=2)
# BS_Bulk(filter_suffix="A12",underlying_asset="SPX",data_year=2010,hist_vol_period=2)

# underlying_asset<-"SPX"
# data_year<-2008
# lookback

# those_files<- dir(paste0("~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/"),full.names=TRUE)

# for(i in 1:length(those_files)){
# 	print(those_files[i])
# 	load(those_files[i])
# 	price_delta %>% filter(!is.na(Last)) %>% mutate(overunder=ifelse(Last<BS_prices,"over","under"),cheapy=ifelse(Last>quantile(Last,0.25,na.rm=TRUE),ifelse(Last>quantile(Last,0.75,na.rm=TRUE),"expensive","moderate"),"cheap"),APE=abs(Last-BS_prices),ARPE=APE/Last) %>% group_by(Type,overunder,cheapy) %>% summarise(count=n(),avg_APE=mean(APE),median_APE=median(APE),max_APE=max(APE),avg_ARPE=mean(ARPE),median_ARPE=median(ARPE),max_ARPE=max(ARPE)) %>% print()
# }

# load(paste0("~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/",underlying_asset,"_",data_year,"_BS_data_withdiv_",lookback,"y_with_delta.RData"))


