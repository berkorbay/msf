
# find_trading_days<-function(asset_data,initial_date,expiration_date){
# 	print(initial_date)
# 	print(expiration_date)
# 	asset_csection<-asset_data %>% filter(DataDate>initial_date & DataDate <= expiration_date)
# 	print(asset_csection,n=50)
# 	print(nrow(asset_csection))
# 	nrow(asset_csection)
# }

main_path <- "~/Dropbox/PhD_Workshop/"
source(paste0(main_path,"Codebase/00_run_this_first_option_calculations.r"))

expand_dates<-function(option_symbol,start_date,end_date,real_exp,asset_prices,option_data){

	option_csection <- asset_prices %>% 
							filter(DataDate >= start_date & DataDate <= real_exp) %>% 
							arrange(DataDate)

	option_csection <- option_data %>% 
							filter(OptionSymbol == option_symbol) %>%
							select(DataDate,Last,Moneyness) %>% 
							left_join(option_csection,.,by="DataDate") %>%
							mutate(NetMaturity=nrow(.)-1) %>%
							ungroup %>% 
							filter(DataDate <= end_date)

	option_csection <- option_data %>% 
							filter(OptionSymbol == option_symbol) %>% 
							slice(1) %>%
							select(OptionSymbol,Type,RealExpiration,Strike,ExpirationPrice,ExpirationMoneyness) %>%
							cbind(option_csection,.) %>%
							mutate(NetMaturity=NetMaturity-c(0,cumsum(rep(1,nrow(.)-1))),
									Maturity=as.numeric(RealExpiration-DataDate))

	return(option_csection)
}


add_risk_free_rates<-function(the_date,Maturity,rf_data,maturity_periods){
	yield_vector <- rf_data %>% filter(DataDate == the_date) %>% select(-DataDate) %>% unlist
	while(length(yield_vector) == 0){
		the_date <- as.Date(as.numeric(the_date)-1,origin="1970-01-01")
		yield_vector <- rf_data %>% filter(DataDate == the_date) %>% select(-DataDate) %>% unlist
	}

	fspline<-splinefun(maturity_periods,yield_vector)
	return(fspline(Maturity))
}

prepare_optiondelta<-function(input_path="Input Files/",filter_suffix="C12",underlying_asset,data_year,progressOutput=TRUE){

	option_input_path<-paste0(main_path,input_path,"Asset Options/")
	asset_input_path<-paste0(main_path,input_path,"Asset Prices/")
	dividend_input_path<-paste0(main_path,input_path,"Asset Specials/")
	rates_data_path<-paste0(main_path,input_path,"Other/")
	output_path<-paste0(main_path,input_path,"Asset Position Raw/")

	# option_data<-read.csv(paste0(option_input_path,underlying_asset,"_",data_year,"_options_filtered_",filter_suffix,".csv")) %>% tbl_df
	load(paste0(option_input_path,underlying_asset,"_",data_year,"_options_filtered_",filter_suffix,".RData")) 

	option_data<- processed_data %>% 
					tbl_df %>%
					select(DataDate,OptionSymbol,RealExpiration,Type,Last,Strike,Moneyness,ExpirationPrice,ExpirationMoneyness,Maturity,IV,DataDelta=Delta)						

	terminal_date <- option_data %>% select(DataDate) %>% summarise(max(DataDate)) %>% unlist %>% as.Date(.,origin="1970-01-01")

	load(paste0(asset_input_path,underlying_asset,"_","daily_processed.RData"))

	

	# asset_data<-read.csv(paste0(asset_input_path,underlying_asset,"_","daily_processed.csv")) %>% tbl_df

	asset_data <- asset_price_data %>% filter(DataDate > "2000-01-01" & !is.na(Dividend.Yield)) %>% 
					select(DataDate,UnderlyingPrice,Dividend.Yield)

					# mutate(DataDate=as.Date(Date)) %>% 
					# filter(DataDate > "2000-01-01") %>%
					# select(UnderlyingPrice=Adj.Close,DataDate)
					# select(UnderlyingPrice=Adj.Close,Dividend.Yield,matches(paste0(parameter_period,"y")),DataDate)

	# max_date<- max(as.Date("2015-08-22"),asset_data$DataDate)

	# dividend_data<-read.csv(paste0(dividend_input_path,underlying_asset,"_","dividend_yield.csv")) %>% 
	# 				tbl_df %>%
	# 				mutate(DataDate=as.Date(Date,format="%d/%m/%y"),DataDate=as.Date(ifelse(DataDate > max_date,as.Date(paste0("19",substring(as.character(DataDate),3))),DataDate),origin="1970-01-01")) %>%
	# 				# mutate(DataDate=as.Date(Date,format="%d/%m/%y"),DataYear=as.numeric(format(DataDate,"%Y")),DataYear=ifelse(DataYear > 2014,DataYear-40,DataYear)) %>%
	# 				select(Dividend.Yield,DataDate)


	# asset_data<-left_join(asset_data,dividend_data)

	if(is.na(asset_data$Dividend.Yield[nrow(asset_data)])){
		asset_data$Dividend.Yield[nrow(asset_data)] <- rev(asset_data$Dividend.Yield[!is.na(asset_data$Dividend.Yield)])[1]
	}
	while(any(is.na(asset_data$Dividend.Yield))){
		asset_data$Dividend.Yield<-ifelse(is.na(asset_data$Dividend.Yield),c(asset_data$Dividend.Yield[-1],0),asset_data$Dividend.Yield)
	}


	# colnames(asset_data) <- sub(paste0(parameter_period,"y"),"",colnames(asset_data))

	# symbol_data <- option_data %>% 
	# 					arrange(OptionSymbol,DataDate) %>% 
	# 					distinct(OptionSymbol) %>% 
	# 					select(OptionSymbol,Type,Strike,DataDate,RealExpiration) %>%
	# 					mutate(EndDate=pmin(RealExpiration,terminal_date)) %>%
	# 					slice(1:3)

	symbol_data <- option_data %>% 
						arrange(OptionSymbol,DataDate) %>% 
						group_by(OptionSymbol) %>%
						summarise(StartDate=min(DataDate),EndDate=max(RealExpiration),number_of_contracts=n()) %>% 
						ungroup %>%
						mutate(RealExp=EndDate,EndDate=pmin(as.Date(EndDate,origin="1970-01-01"),terminal_date)) %>% 
						filter(number_of_contracts > 1)

	mdply_data <- symbol_data %>%	
					select(-number_of_contracts) %>%
					rename(option_symbol=OptionSymbol,start_date=StartDate,end_date=EndDate,real_exp=RealExp)
					# rename(option_symbol=OptionSymbol,start_date=StartDate,end_date=EndDate)


	load(paste0(rates_data_path,"TreasuryYields.RData"))
	# rf_yields<-read.csv(paste0(rates_data_path,"TreasuryYields.csv"),header=TRUE) %>% 
	# 			tbl_df %>%
	# treasury_rates <- treasury_rates %>% mutate(Date=as.Date(Date,format="%d/%m/%y"))
	periods<-as.numeric(sub("X","",colnames(treasury_rates)[-1]))*30


	delta_data<-mdply(mdply_data,.fun=expand_dates,asset_prices=asset_data,option_data=option_data,.progress="text",.parallel = TRUE) %>% 
				tbl_df %>% 
				select(-option_symbol,-start_date,-end_date,-real_exp) %>%
				rowwise %>% mutate(RiskFreeRate=add_risk_free_rates(DataDate,Maturity,treasury_rates,periods)) %>%
				mutate(Moneyness=UnderlyingPrice/Strike,Time_Moneyness=Moneyness*exp(RiskFreeRate*NetMaturity/252))

	delta_data <- option_data %>% select(DataDate,OptionSymbol,IV,DataDelta) %>% left_join(delta_data,.,by=c("DataDate","OptionSymbol")) %>% 
	select(OptionSymbol,DataDate,Type,NetMaturity,Strike,Last,Moneyness,Maturity,UnderlyingPrice,RealExpiration,Time_Moneyness,ExpirationPrice,ExpirationMoneyness,Dividend.Yield,RiskFreeRate,IV,DataDelta)

	# write.table(as.data.frame(delta_data),paste0(output_path,underlying_asset,"_",data_year,"_price_delta_raw.csv"),sep=",",row.names=FALSE)

	save(delta_data,file=paste0(output_path,underlying_asset,"_",data_year,"_price_delta_raw.RData"))

}


for(i in 2008:2013){
	print(paste0("Starting ",i))
	prepare_optiondelta(input_path="Input Files/",filter_suffix="C12",underlying_asset="NDX",data_year=i,progressOutput=TRUE)
}

for(i in 2008:2013){
	print(paste0("Starting ",i))
	prepare_optiondelta(input_path="Input Files/",filter_suffix="C12",underlying_asset="SPX",data_year=i,progressOutput=TRUE)
}



# delta_data %>% mutate(mecheck=Maturity/NetMaturity) %>% filter(mecheck  > 7/4.7) %>% arrange(desc(mecheck))
	
# mdply_data %>% filter(option_symbol=="NDX090117C01150000")

# option_symbol<-"SPX080719C01200000"
# start_date<-as.Date("2008-06-24")
# end_date<-as.Date("2008-07-18")
# real_exp<-as.Date("2008-07-18")




