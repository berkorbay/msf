#Data restructuring for C12

main_path <- "~/Dropbox/PhD_Workshop/"
source(paste0(main_path,"Codebase/00_run_this_first_option_calculations.r"))

find_real_expiration<-function(exp_data,date_data){

	unique_expirations<-as.Date(unique(exp_data$Expiration))
	real_expirations<-unique_expirations

	if(length(date_data)<10 | length(exp_data)<1){
		stop("Check the data")
	}

	repeat{
		check_all<-real_expirations %in% date_data
		real_expirations[!check_all]<-as.Date(real_expirations[!check_all])-1
		if(all(check_all)){
			break
		}
	}

	expiration_summary<-data.frame(Expiration=unique_expirations,RealExpiration=real_expirations)

	exp_data %>% left_join(.,expiration_summary,by="Expiration")

}

add_risk_free_rates<-function(DataDate,Maturity,rf_data,maturity_periods){
	yield_vector <- rf_data %>% filter(Date == DataDate) %>% select(-Date) %>% unlist
	while(length(yield_vector) == 0){
		DataDate <- as.Date(as.numeric(DataDate)-1,origin="1970-01-01")
		yield_vector <- rf_data %>% filter(Date == DataDate) %>% select(-Date) %>% unlist
	}

	fspline<-splinefun(maturity_periods,yield_vector)
	return(fspline(Maturity))
}

# underlying_asset<-"SPX"
# data_year<-2010

findNetMaturity<-function(init_date,end_date,mid_dates){

	sum(mid_dates <= end_date & mid_dates > init_date)


}


process_data_v2<-function(input_path="Input Files/",options_path="Asset Options/",prices_path="Asset Prices/",rates_path="Other/",underlying_asset,data_year){

	load(paste0(main_path,input_path,options_path,underlying_asset,"_",data_year,"_options",".RData"))

	processed_data <- raw_data %>% 
							distinct(OptionSymbol,DataDate) %>%
							filter(Exchange=="*") %>%
							rename(OldSymbol=OptionSymbol) %>%
							mutate(OptionSymbol=paste0(underlying_asset,substr(OldSymbol,4,length(OldSymbol)))) %>%
							distinct(OptionSymbol,DataDate) %>%
							select(-AKA,-UnderlyingPrice,-Exchange)

	load(paste0(main_path,input_path,prices_path,underlying_asset,"_daily_processed",".RData"))

	asset_price_data <-  asset_price_data %>%
							select(-Open,-High,-Low,-Close,-Volume,-log_returns)

	processed_data <- processed_data %>%
							find_real_expiration(.,asset_price_data$DataDate) %>%
							left_join(.,asset_price_data,by="DataDate") %>%
							left_join(.,select(asset_price_data,DataDate,ExpirationPrice=UnderlyingPrice),by=c("RealExpiration"="DataDate")) %>%
							mutate(Maturity=as.numeric(RealExpiration-DataDate),
									Moneyness=UnderlyingPrice/Strike,
									ExpirationMoneyness=ExpirationPrice/Strike) %>%
							# rowwise() %>%
							# mutate(NetMaturity=findNetMaturity(DataDate,Expiration,asset_price_data$DataDate)) %>%
							filter(Last >= 0.05 &
								   Volume >= 100 &
								   Moneyness >= 0.5 &
								   Moneyness <= 1.5 &
								   Maturity <= 365 &
								   Maturity >= 7 &
								   IV <= 0.7 &
								   IV > 0.01 &
								   !((Type=="call" & Last < (UnderlyingPrice - Strike)*1) | (Type=="put" & Last < 1*(Strike - UnderlyingPrice)))) %>%
							arrange(OptionSymbol,DataDate) %>%
							# group_by(OptionSymbol) %>%
							# mutate(OptionCount=n(),OccurenceRank=1:n()) %>%
							# ungroup %>% 
							group_by(DataDate,Maturity,Type) %>% 
							arrange(desc(Strike)) %>% 
							mutate(diff=Last/lag(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lag(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
							# arrange(desc(Strike)) %>% 
							# select(OptionSymbol,DataDate,Maturity,Type,Strike,Last,diff,diff2) %>% 

							mutate(mark=ifelse((Type=="call" & diff < 1)|(Type=="put" & diff > 1),"Mark",""),remove_row=ifelse((Type=="call" & diff < 1 & diff2 < 1)|(Type=="put" & diff > 1 & diff2 > 1),TRUE,FALSE)) %>%
							filter(!remove_row) %>%
							mutate(diff=Last/lag(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lag(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
							mutate(mark=ifelse((Type=="call" & diff < 1)|(Type=="put" & diff > 1),"Mark",""),remove_row=ifelse((Type=="call" & diff < 1 & diff2 < 1)|(Type=="put" & diff > 1 & diff2 > 1),TRUE,FALSE)) %>%
							filter(!remove_row) %>%
							mutate(diff=Last/lag(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lag(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
							mutate(mark=ifelse((Type=="call" & diff < 1)|(Type=="put" & diff > 1),"Mark",""),remove_row=ifelse((Type=="call" & diff < 1 & diff2 < 1)|(Type=="put" & diff > 1 & diff2 > 1),TRUE,FALSE)) %>%
							filter(!remove_row) %>%
							mutate(diff=Last/lag(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lag(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
							mutate(mark=ifelse((Type=="call" & diff < 1)|(Type=="put" & diff > 1),"Mark",""),remove_row=ifelse((Type=="call" & diff < 1 & diff2 < 1)|(Type=="put" & diff > 1 & diff2 > 1),TRUE,FALSE)) %>%
							filter(!remove_row) %>%
							 # filter(DataDate=="2010-02-05",Maturity==14,Type=="put") %>% 

							mutate(diff=Last/lead(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lead(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
							mutate(mark=ifelse((Type=="call" & diff > 1)|(Type=="put" & diff < 1),"Mark",""),remove_row=ifelse((Type=="call" & diff > 1 & diff2 > 1)|(Type=="put" & diff < 1 & diff2 < 1),TRUE,FALSE)) %>%
							filter(!remove_row) %>%
							mutate(diff=Last/lead(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lead(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
							mutate(mark=ifelse((Type=="call" & diff > 1)|(Type=="put" & diff < 1),"Mark",""),remove_row=ifelse((Type=="call" & diff > 1 & diff2 > 1)|(Type=="put" & diff < 1 & diff2 < 1),TRUE,FALSE)) %>%
							filter(!remove_row) %>%
							mutate(diff=Last/lead(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lead(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
							mutate(mark=ifelse((Type=="call" & diff > 1)|(Type=="put" & diff < 1),"Mark",""),remove_row=ifelse((Type=="call" & diff > 1 & diff2 > 1)|(Type=="put" & diff < 1 & diff2 < 1),TRUE,FALSE)) %>%
							filter(!remove_row) %>%
							mutate(diff=Last/lead(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lead(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
							mutate(mark=ifelse((Type=="call" & diff > 1)|(Type=="put" & diff < 1),"Mark",""),remove_row=ifelse((Type=="call" & diff > 1 & diff2 > 1)|(Type=="put" & diff < 1 & diff2 < 1),TRUE,FALSE)) %>%
							filter(!remove_row) %>%
							ungroup %>%

							group_by(DataDate,Strike,Type) %>% 
							arrange((Maturity)) %>% 
							mutate(diff=Last/lag(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lag(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
							mutate(mark=ifelse((diff < 1),"Mark",""),remove_row=ifelse((diff < 1 & diff2 < 1),TRUE,FALSE)) %>%
							filter(!remove_row) %>%
							ungroup %>%

							select(	OptionSymbol,
									DataDate,
									UnderlyingPrice,
									Strike,
									Type,
									Last,
									Moneyness,
									Maturity,
									RealExpiration,
									ExpirationPrice,
									ExpirationMoneyness,
									# OptionCount,
									# OccurenceRank,
									IV,
									Delta,
									Bid,
									Ask,
									Volume,
									OpenInterest,
									Gamma,
									Theta,
									Vega,
									Expiration,
									# Exchange,
									OldSymbol,
									UnderlyingSymbol
									)

	save(processed_data,file=paste0(main_path,input_path,"Asset Options/",underlying_asset,"_",data_year,"_options_filtered_C12.RData"))
}

for(i in 2008:2013){
	print(paste0("Preparing NDX ",i))
	process_data_v2(underlying_asset="NDX",data_year=i)
	print(paste0("Preparing SPX ",i))
	process_data_v2(underlying_asset="SPX",data_year=i)
}
i<-2013



# processed_data2 %>% 
# group_by(DataDate,Maturity,Type) %>% 
# arrange(desc(Strike)) %>% 
# mutate(diff=Last/lag(Last,1,default=NA),strikediff=Strike/lag(Strike,1,default=NA),compdiff=diff*strikediff,compdiff1=lead(compdiff,1),compdiff2=lag(compdiff,1)) %>% 
# mutate(alert=ifelse(!is.na(diff) & ((Type=="call" & diff < 1)|(Type=="put" & diff > 1)),"Mark",""),alert2=lead(alert,1)) %>% 
# # filter(alert=="Mark") %>% 


# # select(DataDate,Maturity,Type)


# # processed_data2 %>% 
# filter(DataDate=="2010-01-04",Maturity==11,Type=="put") %>% 
# arrange(desc(Strike)) %>% 
# select(OptionSymbol,DataDate,Maturity,Type,Strike,Last,diff,strikediff,compdiff,compdiff1,compdiff2,alert,alert2) %>% 
# # mutate(diff=Last-lag(Last,1,default=NA)) %>% 
# mutate(remove_row=ifelse((Type=="call" & ((alert=="Mark" & compdiff > compdiff2)|(alert2=="Mark" & compdiff > compdiff1)))|(Type=="put" & ((alert=="Mark" & compdiff < compdiff2)|(alert2=="Mark" & compdiff < compdiff1))),"This","")) %>%
# print(n=50)


# # processed_data2 %>% 
# # group_by(DataDate,Maturity,Type) %>% 
# # arrange(desc(Strike)) %>% 
# # mutate(diff=Last/lag(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lag(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
# # # arrange(desc(Strike)) %>% 
# # # select(OptionSymbol,DataDate,Maturity,Type,Strike,Last,diff,diff2) %>% 

# # mutate(mark=ifelse((Type=="call" & diff < 1)|(Type=="put" & diff > 1),"Mark",""),remove_row=ifelse((Type=="call" & diff < 1 & diff2 < 1)|(Type=="put" & diff > 1 & diff2 > 1),TRUE,FALSE)) %>%
# # filter(!remove_row) %>%
# # mutate(diff=Last/lag(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lag(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
# # mutate(mark=ifelse((Type=="call" & diff < 1)|(Type=="put" & diff > 1),"Mark",""),remove_row=ifelse((Type=="call" & diff < 1 & diff2 < 1)|(Type=="put" & diff > 1 & diff2 > 1),TRUE,FALSE)) %>%
# # filter(!remove_row) %>%
# # mutate(diff=Last/lag(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lag(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
# # mutate(mark=ifelse((Type=="call" & diff < 1)|(Type=="put" & diff > 1),"Mark",""),remove_row=ifelse((Type=="call" & diff < 1 & diff2 < 1)|(Type=="put" & diff > 1 & diff2 > 1),TRUE,FALSE)) %>%
# # filter(!remove_row) %>%
# # mutate(diff=Last/lag(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lag(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
# # mutate(mark=ifelse((Type=="call" & diff < 1)|(Type=="put" & diff > 1),"Mark",""),remove_row=ifelse((Type=="call" & diff < 1 & diff2 < 1)|(Type=="put" & diff > 1 & diff2 > 1),TRUE,FALSE)) %>%
# # filter(!remove_row) %>%
# #  # filter(DataDate=="2010-02-05",Maturity==14,Type=="put") %>% 

# # mutate(diff=Last/lead(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lead(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
# # mutate(mark=ifelse((Type=="call" & diff > 1)|(Type=="put" & diff < 1),"Mark",""),remove_row=ifelse((Type=="call" & diff > 1 & diff2 > 1)|(Type=="put" & diff < 1 & diff2 < 1),TRUE,FALSE)) %>%
# # filter(!remove_row) %>%
# # mutate(diff=Last/lead(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lead(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
# # mutate(mark=ifelse((Type=="call" & diff > 1)|(Type=="put" & diff < 1),"Mark",""),remove_row=ifelse((Type=="call" & diff > 1 & diff2 > 1)|(Type=="put" & diff < 1 & diff2 < 1),TRUE,FALSE)) %>%
# # filter(!remove_row) %>%
# # mutate(diff=Last/lead(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lead(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
# # mutate(mark=ifelse((Type=="call" & diff > 1)|(Type=="put" & diff < 1),"Mark",""),remove_row=ifelse((Type=="call" & diff > 1 & diff2 > 1)|(Type=="put" & diff < 1 & diff2 < 1),TRUE,FALSE)) %>%
# # filter(!remove_row) %>%
# # mutate(diff=Last/lead(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lead(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
# # mutate(mark=ifelse((Type=="call" & diff > 1)|(Type=="put" & diff < 1),"Mark",""),remove_row=ifelse((Type=="call" & diff > 1 & diff2 > 1)|(Type=="put" & diff < 1 & diff2 < 1),TRUE,FALSE)) %>%
# # filter(!remove_row) %>%

# print(n=70)



# processed_data2 %>% 
# group_by(DataDate,Strike,Type) %>% 
# # summarise(count=n()) %>% group_by(count) %>% summarise(cnt=n())
# arrange((Maturity)) %>% 
# mutate(diff=Last/lag(Last,1,default=NA),diff=ifelse(is.na(diff),1,diff),diff2=Last/lag(Last,2,default=NA),diff2=ifelse(is.na(diff2),1,diff2)) %>%
# mutate(mark=ifelse((diff < 1),"Mark",""),remove_row=ifelse((diff < 1 & diff2 < 1),TRUE,FALSE)) %>%
# filter(!remove_row) 





# rates_path<-"Other/"
# prices_path<-"Asset Prices/"
# input_path<-"Input Files/"
# underlying_asset <- "SPX"
# data_year <- 2013


# processed_data2 %>% group_by(DataDate,Maturity,Type) %>% arrange((Strike)) %>% mutate(diff=(Last-lag(Last,1))/Last) %>% filter(!is.na(diff) & ((diff>0 & Type=="call")|(diff<0 & Type=="put"))) %>% group_by(Type) %>% filter(abs(diff)>0) %>% summarise(quantile(abs(diff),0.9))

# processed_data2 %>% group_by(DataDate,Maturity,Type) %>% arrange((Strike)) %>% mutate(diff=(Last-lag(Last,1))/Last) %>% filter(diff > 1 & Type == "call")

# processed_data2 %>% group_by(DataDate,Maturity,Type) %>% arrange((Strike)) %>% mutate(diff=(Last-lag(Last,1))/Last)  %>% filter(DataDate=="2010-08-11",Maturity==37,Type=="call") %>% print(n=100)


# processed_data2 %>% group_by(DataDate,Maturity,Type) %>% arrange(desc(Strike)) %>% mutate(diff=Last-lag(Last,1)) %>% select(OptionSymbol, DataDate,Maturity,Type,Strike,Last,diff) %>% print(n=250)  %>% filter(diff<0,Type=="call")

# processed_data2 %>% group_by(DataDate,Maturity,Type,Strike) %>% summarise(count=n()) %>% ungroup %>% filter(count > 1)
#  processed_data2 %>% filter(DataDate=="2010-01-05",Maturity==10,Type=="call")

