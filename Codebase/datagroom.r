#Author: Berk Orbay 

#This code snippet is irrelevant to the model, just to clean the workspace
# rm(list=ls(all=TRUE))

#This is data grooming code for option data.

main_path<-"~/Dropbox/PhD_Workshop/"

###############################
# Extracting single symbol from daily aggregate options data
###############################


data_restruct<-function(data_path="~/local_projects/HDALL/",data_type="options",data_year,underlying_asset,output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE){
	required_files<-dir(data_path,pattern=paste0(data_type,"_",data_year),recursive=TRUE)
	#Progress output is a convenience to the user about what is going on with the progress of the code
	if(progressOutput){
		print(paste0(data_year," ",underlying_asset))
		#Setup the progress bar
		progBar <- txtProgressBar(min = 0, max = length(required_files), style = 3)
		setTxtProgressBar(progBar, 0)
		#Report the number of files to be parsed
		print(paste0("Number of files: ",length(required_files)))
		#Keep time elapsed for this code (it takes an average of an hour per symbol per year, less for previous years)
		start_time<-Sys.time()
		print(paste0("Starting at ",start_time,". Calculating estimated time."))
	}

	#Initialize the output table by taking the first file in the list and extracting the required rows with the desired symbol and writing it to a table
	table1<-read.csv(paste0(data_path,required_files[1]),header=TRUE)
	#table1[table1$UnderlyingSymbol==underlying_asset,] #Note to self - Rogue line?
	write.table(table1[table1$UnderlyingSymbol==underlying_asset,],paste0(output_path,paste(c(underlying_asset,data_year,data_type),collapse="_"),".csv"),sep=",",append=FALSE,row.names=FALSE)	

	#Update the progress and report ETA 
	if(progressOutput){
		setTxtProgressBar(progBar, 1)
		print(paste0("Estimated time to finish: ",round(difftime(Sys.time(),start_time,units="mins")*(length(required_files)-1),2)," minutes."))
	}

	#Repeat the procedure for every file in the list
	for(i in 2:length(required_files)){
		table1<-read.csv(paste0(data_path,required_files[i]),header=TRUE)
		table1[table1$UnderlyingSymbol==underlying_asset,]
		write.table(table1[table1$UnderlyingSymbol==underlying_asset,],paste0(output_path,paste(c(underlying_asset,data_year,data_type),collapse="_"),".csv"),sep=",",append=TRUE,row.names=FALSE,col.names=FALSE)
		#Update the progress and report updated ETA 
		if(progressOutput && i%%5){	
			setTxtProgressBar(progBar, i+1)
			print(paste0("Elapsed time: ",round(difftime(Sys.time(),start_time,units="mins"),2)))
			print(paste0("Updated ETF: ",round(difftime(Sys.time(),start_time,units="mins")*(length(required_files)-i)/i,2)," minutes."))		
		}
	}
	#Gracefully report exit
	if(progressOutput){
		print("That would be all, thank you.")
	}
}

#Remove the comment from the line below to run the code
#data_restruct(data_path="~/local_projects/HDALL/",data_type="options",data_year="2003",underlying_asset="MSFT",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE)

###############################
# Processing raw options data
###############################

#Here in this function we are taking the calendar days and find the next and previous trading days
convert_to_trading_day<-function(dates_to_convert,conversion="previous",days_path="~/Dropbox/PhD_Workshop/Input Files/Other/"){
	#Here we create a class to convert the raw data to date
	setClass("tradeDayFormat")
	setAs("character","tradeDayFormat", function(from) as.Date(from, format="%d/%m/%y") )
	#Read the data from the file and convert to R date format
	#Data contains the calendar and for each day the next and previous trading days
	trade_days<-read.csv(paste0(days_path,"trade_days.csv"),colClasses=rep("tradeDayFormat",3),header=TRUE)

	#Find the places of the dates to convert to trading days
	conv_places<-match(dates_to_convert,trade_days$regular_day)
	#If anything is wrong spit out a warning as undefined dates
	if(any(is.na(conv_places))){
		print("Undefined dates!")
	}
	#If everything is in order return the specified trading days (previous or next) or spit out a warning if neither is specified.
	if(conversion=="previous"){
		fixed_days<-trade_days$prev_trade_day[conv_places]
		fixed_days[is.na(conv_places)]<-dates_to_convert[is.na(conv_places)]
		return(fixed_days)
	}else if(conversion=="next"){
		fixed_days<-trade_days$next_trade_day[conv_places]
		fixed_days[is.na(conv_places)]<-dates_to_convert[is.na(conv_places)]
		return(fixed_days)
	}else{
		print("Conversion not understood?!")
	}
}

#This code is to find number of trading days within the given maturities. It takes the current date of the option and expiration date,
#then finds the number of trading days between the two date points
find_maturity<-function(expirations,inceptions,days_path="~/Dropbox/PhD_Workshop/Input Files/Other/"){
	#Here we create a class to convert the raw data to date
	setClass("tradeDayFormat")
	setAs("character","tradeDayFormat", function(from) as.Date(from, format="%d/%m/%y"))
	trade_days<-read.csv(paste0(days_path,"trade_days.csv"),colClasses=rep("tradeDayFormat",3),header=TRUE)
	#Find the previous trade days, no option of previous or next trading days
	#This is mostly because of trading of the option ceases in the previous trading day not the next
	real_trade_days<-unique(trade_days$prev_trade_day[max(expirations)>=trade_days$prev_trade_day & min(inceptions)<trade_days$prev_trade_day])
	rtmatrix<-t(matrix(rep(real_trade_days,length(expirations)),nrow=length(real_trade_days)))
	#Find the number of trading days as the real maturities
	RealMaturities<-rowSums(expirations>=rtmatrix & rtmatrix>inceptions)
	#The trading days file has limited data on the future (somewhere to the end of 2015 I suppose)
	#So if any option has longer maturity (i.e. LEAPS) report them and set the maturity to 9999 days 
	#in order to filter them in the next processing function
	if(any(expirations>max(trade_days$prev_trade_day))){
		print("Beyond Max Maturities")
		RealMaturities[expirations>max(trade_days$prev_trade_day)]<-9999
	}
	return(RealMaturities)
}

#This is the cleaning of the raw option specs data
process_data<-function(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_year,underlying_asset,output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE){
	#The progress can be tracked step by step with the progressOutput = TRUE
	if(progressOutput){
		print("Reading the file.")
	}	
	raw_data<-read.csv(paste0(data_path,underlying_asset,"_",data_year,"_",data_type,".csv"),header=TRUE)
	#Here is the standardization of dates
	if(progressOutput){
		print("Correcting the dates.")
	}
	raw_data$Expiration<-as.Date(raw_data$Expiration,format=ifelse(nchar(as.character(raw_data$Expiration))==8,"%m/%d/%y","%m/%d/%Y"))
	raw_data$DataDate<-as.Date(raw_data$DataDate,format=ifelse(nchar(as.character(raw_data$DataDate))==8,"%m/%d/%y","%m/%d/%Y"))
	if(progressOutput){
		print("Calculating moneyness.")
	}
	#Calculate the moneyness by S0/K to the fifth decimal point
	Moneyness<-round(raw_data$UnderlyingPrice/raw_data$Strike,5)
	#Find the closest previous trading day of the expiration if the expiration date is not a trading day
	ConvExpiration<-convert_to_trading_day(unique(raw_data$Expiration),conversion="previous")
	RealExpiration<-ConvExpiration[match(raw_data$Expiration,unique(raw_data$Expiration))]
	#Find the maturity of the option in terms of days (not trading days)
	Maturity<-raw_data$Expiration-raw_data$DataDate
	if(progressOutput){
		print("Finding net maturity.")
	}	
	#Find the maturity of the option in terms of trading days
	NetMaturity<-find_maturity(expirations=RealExpiration,inceptions=raw_data$DataDate)
	#Bind them all in a data frame
	matured_data<-cbind(raw_data,Moneyness,RealExpiration,Maturity,NetMaturity)
	#Write them all in the file
	write.table(matured_data,paste0(data_path,underlying_asset,"_",data_year,"_",data_type,"_processed.csv"),sep=",",append=FALSE,row.names=FALSE)
	if(progressOutput){
		print("That would be all, thank you.")
	}
}

#Remove the comment from the line below to run the code
#process_data(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_year="2010",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE)


###############################
# Filtering processed options data
###############################

#This function is to import the adjusted closing price of the option at the time of expiration
find_ST<-function(inception_dates,expiration_dates,asset_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",splits_path="~/Dropbox/PhD_Workshop/Input Files/Asset Specials/",data_type="daily_processed",underlying_asset){
	asset_data<-read.csv(paste0(asset_path,underlying_asset,"_",data_type,".csv"),header=TRUE)
	ST<-asset_data$Close[match(expiration_dates,as.Date(asset_data$Date))]
	#This is to amend the splits
	if(paste0(underlying_asset,"_stock_splits.csv")%in%dir(splits_path)){
		splits_data<-read.csv(paste0(splits_path,underlying_asset,"_stock_splits.csv"),header=TRUE)
		splits_data$Date<-as.Date(splits_data$Date,format="%d/%m/%y")
		for(i in 1:nrow(splits_data)){
			ST[splits_data$Date[i]>=inception_dates&splits_data$Date[i]<expiration_dates]<-ST[splits_data$Date[i]>=inception_dates&splits_data$Date[i]<expiration_dates]*splits_data$new_split[i]/splits_data$old_split[i]
		}
	}
	#If the expiration date is after the most recent price data date (not necessarily today) imply it with -1 
	ST[is.na(ST)]<- -1
	ST
}

#This function is to find the realized historical volatility during the lifetime of the option. It is used as to compare with IV.
find_RV<-function(inception_dates,expiration_dates,asset_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily_processed",underlying_asset){
	asset_data<-read.csv(paste0(asset_path,underlying_asset,"_",data_type,".csv"),header=TRUE)
	exp_places<-match(expiration_dates,as.Date(asset_data$Date))
	inc_places<-match(convert_to_trading_day(inception_dates+1,conversion="next"),as.Date(asset_data$Date))
	mapply(function(x,y,returns) ifelse(is.na(x),0,sd(returns[x:y])*sqrt(252)),x=exp_places,y=inc_places,MoreArgs=list(returns=asset_data$log_returns))
}


#Using STs and strike proces of the option specs, the real payoff at the time of expiration is calculated with this function
find_payoff<-function(final_prices,special_dividends,strike_prices,contract_type){
	realized_payoff<-final_prices-(strike_prices-special_dividends)
	realized_payoff[contract_type=="put"]<- -realized_payoff[contract_type=="put"]
	realized_payoff<-pmax(realized_payoff,0)
	realized_payoff[final_prices == -1]<- -1
	return(realized_payoff)
}

append_risk_free_rate<-function(rates_data_path="~/Dropbox/PhD_Workshop/Input Files/Other/",inception_dates,expiration_dates){
	risk_free_table<-read.csv(paste0(rates_data_path,"TreasuryYields.csv"),header=TRUE)
	risk_free_table$Date<-as.Date(risk_free_table$Date,format="%d/%m/%y")
	periods<-as.numeric(gsub("X","",colnames(risk_free_table)[-1]))/12
	data_uniques<-unique(inception_dates)
	adjusted_risk_free_rates<-rep(0,length(inception_dates))
	intervals<-as.numeric(expiration_dates-inception_dates)/365
	for(i in 1:length(data_uniques)){
		rf_fun<-splinefun(periods,risk_free_table[ifelse(any(data_uniques[i]==risk_free_table$Date),match(data_uniques[i],risk_free_table$Date),match(data_uniques[i+1],risk_free_table$Date)),-1])
		adjusted_risk_free_rates[data_uniques[i]==inception_dates]<-rf_fun(intervals[data_uniques[i]==inception_dates])
	}
	return(adjusted_risk_free_rates)
}

#ps: Nasdaq or NYSE or CBOE is not closed on Veterans' Day and Columbus Day and closed on Good Friday unlike Government 
#http://www.investopedia.com/ask/answers/06/stockexchangeclosed.asp

append_dividend_yield<-function(dividend_data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",underlying_asset,inception_dates){
	dividend_table<-read.csv(paste0(dividend_data_path,underlying_asset,"_daily_processed.csv"),header=TRUE)
	dividend_table$Date<-as.Date(dividend_table$Date)
	if(any(is.na(match(inception_dates,dividend_table$Date)))){
		print("ERRRRORRRRR at Append Dividend Yield!!!!!! CHECK IMMEDIATELY!!!!")
	}
	return(dividend_table$Dividend.Yield[match(inception_dates,dividend_table$Date)])
}


get_special_dividends<-function(special_dividend_data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Specials/",underlying_asset,inception_dates,expiration_dates){
	special_dividend<-rep(0,length(inception_dates))
	if(file.exists(paste0(special_dividend_data_path,underlying_asset,"_special_dividends.csv"))){
		dividend_table<-read.csv(paste0(special_dividend_data_path,underlying_asset,"_special_dividends.csv"),header=TRUE)
		dividend_table$Date<-as.Date(dividend_table$Date,format="%d/%m/%y")
		for(i in 1:nrow(dividend_table)){
			chosen<-dividend_table$Date[i]>inception_dates & dividend_table$Date[i]<=expiration_dates
			special_dividend[chosen]<-special_dividend[chosen] + dividend_table$Special_Dividends[i]
		}
	}
	return(special_dividend)
}


#Here we filter the data according to the specifications we require for a healthy analysis
filter_rule_A1<-function(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_suffix="_processed",data_year,underlying_asset,output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE){
	if(progressOutput){
		print("Reading the file...")
	}
	#Take the processed but not filtered data
	unfiltered_data<-read.csv(paste0(data_path,underlying_asset,"_",data_year,"_",data_type,data_suffix,".csv"),header=TRUE)
	if(progressOutput){
		print("Filtering...")
	}
	#Removing an unnecessary column
	filtered_data<-unfiltered_data[,-5]
	#Filter options with volume less than 100
	filtered_data<-filtered_data[filtered_data[,"Volume"]>=100,]
	#Filter options with price less than or equal to 0.05
	filtered_data<-filtered_data[filtered_data[,"Last"]>0.05,]
	#Filter options with moneyness S0/K outside 0.25 and 4 
	filtered_data<-filtered_data[filtered_data[,"Moneyness"]>=0.25 & filtered_data[,"Moneyness"]<=4,]
	#Filter options with maturity outside the range
	filtered_data<-filtered_data[filtered_data[,"NetMaturity"]!=9999,]
	#Filter options with maturity less than a week and more than 2 years (731 days)
	filtered_data<-filtered_data[filtered_data[,"Maturity"]>=7 & filtered_data[,"Maturity"]<=731,]
	#Find final closing prices of the underlyings
	ExpirationPrices<-find_ST(inception_dates=as.Date(filtered_data$DataDate),expiration_dates=as.Date(filtered_data$RealExpiration),data_type="daily_processed",underlying_asset=underlying_asset)
	#Find special dividend adjustments
	SpecialDividend<-get_special_dividends(underlying_asset=underlying_asset,inception_dates=as.Date(filtered_data$DataDate),expiration_dates=as.Date(filtered_data$RealExpiration))
	#Calculate the realized payoffs
	ExpirationPayoff<-find_payoff(ExpirationPrices,SpecialDividend,filtered_data$Strike,filtered_data$Type)
	#Calculate the nominal price difference 
	PDAbsolute<- ExpirationPayoff - filtered_data$Last
	PDAbsolute[ExpirationPrices == -1]<- -1
	#Calculate the ratio of the realized payoff to option price of the data
	PDRatio<- ExpirationPayoff / filtered_data$Last
	PDRatio[ExpirationPrices == -1]<- -1
	RiskFreeRate<-append_risk_free_rate(inception_dates=as.Date(filtered_data$DataDate),expiration_dates=as.Date(filtered_data$RealExpiration))
	Dividend.Yield<-append_dividend_yield(underlying_asset=underlying_asset,inception_dates=as.Date(filtered_data$DataDate))
	RV<-find_RV(inception_dates=as.Date(filtered_data$DataDate),expiration_dates=as.Date(filtered_data$RealExpiration),data_type="daily_processed",underlying_asset=underlying_asset)
	filtered_data<-cbind(filtered_data,ExpirationPrices,ExpirationPayoff,SpecialDividend,PDAbsolute,PDRatio,RiskFreeRate,Dividend.Yield,RV)
	#Write them all in the file
	write.table(filtered_data,paste0(data_path,underlying_asset,"_",data_year,"_",data_type,"_filtered_A12.csv"),sep=",",append=FALSE,row.names=FALSE)

	if(progressOutput){
		print("That would be all, thank you.")
	}	
}

#filter_rule_A1(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_suffix="_processed",data_year="2012",underlying_asset="MSFT",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE)

# process_data(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_year="2013",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE)
# filter_rule_A1(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_suffix="_processed",data_year="2013",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE)
# process_data(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_year="2012",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE)
# filter_rule_A1(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_suffix="_processed",data_year="2012",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE)
# process_data(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_year="2011",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE)
# filter_rule_A1(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_suffix="_processed",data_year="2011",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE)
# process_data(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_year="2010",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE)
# filter_rule_A1(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",data_type="options",data_suffix="_processed",data_year="2010",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Options/",progressOutput=TRUE)


###############################
# Processing asset price data
###############################

#This function is specific to process_asset_price_data
#It fetches the dividend yields for their respective assets
get_dividend_yield<-function(dividend_path="~/Dropbox/PhD_Workshop/Input Files/Asset Specials/",underlying_asset,main_table,discreteDividend=TRUE){
#discreteDividend == TRUE means it gets the discrete dividends and converts them to a dividend yield. Otherwise take the yields and match the dates
	main_table<-cbind(main_table,0)
	colnames(main_table)[ncol(main_table)]<-"Dividend.Yield"
	if(!discreteDividend){
		dividend_table<-read.table(paste0(dividend_path,underlying_asset,"_dividend_yield.csv"),sep=",",header=TRUE)
		dividend_table$Date<-as.Date(dividend_table$Date,format=ifelse(nchar(as.character(dividend_table$Date))==8,"%d/%m/%y","%d/%m/%Y"))
		dividend_table$Date<-as.Date(ifelse(dividend_table$Date>Sys.Date(),format(dividend_table$Date,"19%y-%m-%d"),format(dividend_table$Date,"%Y-%m-%d")))
		#Since ex-dividend date starts with the open and we are dealing with closing prices we take ex-dividend date as the day before the ex-dividend date
		dividend_table$Date<-dividend_table$Date-1
		for(i in 1:nrow(main_table)){
			main_table$Dividend.Yield[i]<-rev(dividend_table$Dividend.Yield[main_table$Date[i]<=dividend_table$Date])[1]
		}		
	}else{
		dividend_table<-read.table(paste0(dividend_path,underlying_asset,"_dividends.csv"),sep=",",header=TRUE)
		dividend_table$Date<-as.Date(dividend_table$Date,format=ifelse(nchar(as.character(dividend_table$Date))==8,"%d/%m/%y","%d/%m/%Y"))
		dividend_table$Date<-as.Date(ifelse(dividend_table$Date>Sys.Date(),format(dividend_table$Date,"19%y-%m-%d"),format(dividend_table$Date,"%Y-%m-%d")))
		dividend_table$Date<-dividend_table$Date-1
		for(i in 1:nrow(main_table)){
			year_later<-seq(main_table$Date[i],length=2,by="1 year")[2]-1
			quarterlies<-dividend_table[dividend_table$Date>=main_table$Date[i] & dividend_table$Date<=year_later,"Dividends"]
			#This is for estimation of the recent dates which a year ahead of is yet to come
			#It assumes quarterly dividends
			if(sum(dividend_table$Date>=main_table$Date[i])<4){
				if(length(quarterlies)==0){
					#If no dividend info (very recent data) take the quadruple of the last quarterly dividend 
					quarterlies<-4*(dividend_table[1,"Dividends"])
				}else{
					#If less than 4 dividends, complete them to 4 by taking the respective ratio (e.g. for 3 available data points it is 4/3) 
					quarterlies<-4/length(quarterlies)*sum(quarterlies)
				}
			}
			#Sum the dividends in a year and divide by the current value of the asset price
			main_table[i,"Dividend.Yield"]<-sum(quarterlies)/main_table[i,"Close"]
		}
	}
	return(main_table)
}

process_asset_price_data<-function(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily",underlying_asset,output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",progressOutput=TRUE,discreteDividend=TRUE){
	if(progressOutput){
		print("Reading the file.")
	}	
	raw_data<-read.csv(paste0(data_path,underlying_asset,"_",data_type,".csv"),header=TRUE)
	raw_data$Date<-as.Date(raw_data$Date)
	log_returns<-c(log(raw_data$Adj.Close[-length(raw_data$Adj.Close)]/raw_data$Adj.Close[-1]),0)
	processed_data<-cbind(raw_data,log_returns)
	if(progressOutput){
		print("Getting dividend info.")
	}	
	processed_data<-get_dividend_yield(underlying_asset=underlying_asset,main_table=processed_data,discreteDividend=discreteDividend)
	write.table(processed_data,paste0(data_path,underlying_asset,"_",data_type,"_processed.csv"),sep=",",append=FALSE,row.names=FALSE)
	if(progressOutput){
		print("That would be all, thank you.")
	}		
}

#process_asset_price_data(underlying_asset="MSFT",discreteDividend=FALSE)

calc_hist_vol<-function(dates,log_returns,years_back){
	time_machine<-as.Date(paste0(as.numeric(substr(dates,1,4))-years_back,substr(dates,5,10)))
	feb29s<-which(is.na(time_machine))
	time_machine[feb29s]<-as.Date(paste0(as.numeric(substr(dates[feb29s],1,4))-years_back,substr(dates[feb29s]-1,5,10)))
	places<-match(time_machine,dates)
	while(time_machine[min(which(is.na(places)))]>=min(dates)){
		time_machine[which(is.na(places))]<-time_machine[which(is.na(places))]-1
		places<-match(time_machine,dates)
	}
	places<-places-1
	mapply(function(x,y,returns) ifelse(is.na(y),-1,sd(returns[x:y])*sqrt(252)),x=1:length(places),y=places,MoreArgs=list(returns=log_returns))
}


add_hist_vols<-function(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily",underlying_asset,output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",progressOutput=TRUE){
	if(progressOutput){
		print("Reading the file.")
	}	
	raw_data<-read.csv(paste0(data_path,underlying_asset,"_",data_type,"_processed.csv"),header=TRUE)
	raw_data$Date<-as.Date(raw_data$Date)
	HistVol1y<-calc_hist_vol(raw_data$Date,raw_data$log_returns,1)
	HistVol2y<-calc_hist_vol(raw_data$Date,raw_data$log_returns,2)
	HistVol3y<-calc_hist_vol(raw_data$Date,raw_data$log_returns,3)
	HistVol5y<-calc_hist_vol(raw_data$Date,raw_data$log_returns,5)
	HistVol10y<-calc_hist_vol(raw_data$Date,raw_data$log_returns,10)
	enhanced_data<-data.frame(raw_data,HistVol1y,HistVol2y,HistVol3y,HistVol5y,HistVol10y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_histvol.csv"),sep=",",append=FALSE,row.names=FALSE)
	if(progressOutput){
		print("That would be all, thank you.")
	}		
}
#add_hist_vols(underlying_asset="SPX")


