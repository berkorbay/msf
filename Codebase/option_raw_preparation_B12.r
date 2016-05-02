#Data restructuring for B12

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

process_data_v2<-function(input_path="Input Files/",prices_path="Asset Prices/",rates_path="Other/",underlying_asset,data_year){

	raw_data<-read.csv(paste0(main_path,input_path,"Asset Options/",underlying_asset,"_",data_year,"_options",".csv"),header=TRUE) %>% tbl_df
	processed_data <- raw_data %>% 
							distinct(OptionSymbol,DataDate) %>%
							select(-OptionExt,-AKA,-UnderlyingPrice) %>%
							mutate(Expiration=as.Date(Expiration,format=ifelse(nchar(as.character(Expiration))==8,"%m/%d/%y","%m/%d/%Y")),
								   DataDate=as.Date(DataDate,format=ifelse(nchar(as.character(Expiration))==8,"%m/%d/%y","%m/%d/%Y")))

	asset_price_data<-read.csv(paste0(main_path,input_path,prices_path,underlying_asset,"_daily_processed",".csv"),header=TRUE) %>% 
						tbl_df %>%
						mutate(DataDate=as.Date(Date),
								UnderlyingPrice=round(Adj.Close,2)) %>% 
						select(-Date,-Open,-High,-Low,-Close)

	processed_data <- processed_data %>%
							find_real_expiration(.,asset_price_data$DataDate) %>%
							left_join(.,select(asset_price_data,DataDate,UnderlyingPrice),by="DataDate") %>%
							left_join(.,select(asset_price_data,DataDate,ExpirationPrice=UnderlyingPrice),by=c("RealExpiration"="DataDate")) %>%
							mutate(Maturity=as.numeric(RealExpiration-DataDate),
									Moneyness=UnderlyingPrice/Strike,
									ExpirationMoneyness=ExpirationPrice/Strike) %>%
							filter(Last >= 0.05 &
								   Volume >= 100 &
								   Moneyness >= 0.5 &
								   Moneyness <= 1.5 &
								   Maturity <= 365 &
								   !((Type=="call" & Last < (UnderlyingPrice - Strike)*0.95) | (Type=="put" & Last < 0.95*(Strike - UnderlyingPrice)))) %>%
							arrange(OptionSymbol,DataDate) %>%
							group_by(OptionSymbol) %>%
							mutate(OptionCount=n(),OccurenceRank=1:n()) %>%
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
									OptionCount,
									OccurenceRank,
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
									Exchange,
									UnderlyingSymbol
									)

	save(processed_data,file=paste0(main_path,input_path,"Asset Options/",underlying_asset,"_",data_year,"_options_filtered_B12.RData"))
}

for(i in 2008:2013){
	print(paste0("Preparing NDX ",i))
	process_data_v2(underlying_asset="NDX",data_year=i)
	print(paste0("Preparing SPX ",i))
	process_data_v2(underlying_asset="SPX",data_year=i)
}
i<-2013

rates_path<-"Other/"
prices_path<-"Asset Prices/"
input_path<-"Input Files/"
underlying_asset <- "SPX"
data_year <- 2013


