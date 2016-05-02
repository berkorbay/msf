main_path <- "~/Dropbox/PhD_Workshop/"
source(paste0(main_path,"Codebase/00_run_this_first_option_calculations.r"))

# load("/Users/berkorbay/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/SPX_2010_HN_data_withdiv_asym_5y_with_delta.RData")

take_cumulative_position<-function(the_df){
	naked_position<-the_df$naked_position
	position_df<-data.frame(index=1:length(naked_position),naked_position)

	last_position <- rep(0,length(naked_position))
	last_position[rev(which(naked_position!=0))[1]]<- -naked_position[rev(which(naked_position!=0))[1]]
	end_position <- c(rep(0,length(naked_position)-1),-naked_position[rev(which(naked_position!=0))[1]])

	position_df %>% 
			filter(naked_position != 0) %>% 
			mutate(cumulative_position=naked_position - lag(naked_position,default=0)) %>%
			left_join(position_df,.,by=c("index","naked_position")) %>%
			mutate(cumulative_position=ifelse(is.na(cumulative_position),0,cumulative_position),
					net_position=cumsum(cumulative_position)) %>%
			cbind(.,last_position,end_position) %>% 
			mutate(net_position_to_last=net_position+cumsum(last_position)-last_position) %>%
			select(-index,-naked_position) %>%
			cbind(the_df,.)

}

# model_abbr<-"BS"
# parameter_period=5
# sym=TRUE
# underlying_asset="SPX"
# data_year<-2010


convert_to_error_and_trading_data<-function(model_abbr,parameter_period,sym,underlying_asset,data_year){

	input_path<-paste0(main_path,"Output Files/")
		output_path<-paste0(main_path,"Output Files/Trading_and_Error/")

	if(model_abbr=="BS"){
		input_path<-paste0(input_path,"Black_Scholes/")
		sym_abbr<-""
	}else if(model_abbr=="HN"){
		input_path<-paste0(input_path,"HestonNandi_GARCH/")
		if(sym==TRUE){
			sym_abbr<-"symm_"
		}else{
			sym_abbr<-"asym_"
		}
	}else{
		stop("Wrong model abbreviation")
	}

	# load(paste0("/Users/berkorbay/Dropbox/PhD_Workshop/Output Files/Black_Scholes/SPX_2010_BS_data_withdiv_5y_with_delta.RData"))
	load(paste0(input_path,underlying_asset,"_",data_year,"_",model_abbr,"_data_withdiv_",sym_abbr,parameter_period,"y_with_delta.RData"))

	trade_error<- price_delta %>%
					rename_(model_price=paste0(model_abbr,"_prices"),model_delta=paste0(model_abbr,"_Delta")) %>%
					# rename(model_price=BS_prices,model_delta=BS_Delta) %>%
					mutate(model_price=round(model_price,2),model_delta=ifelse(Type=="call",-1,1)*abs(round(ifelse((Type=="call" & model_delta>0) | (Type=="put" & model_delta<0),model_delta,0),4))) %>%
					mutate(naked_position=ifelse(is.na(Last),0,ifelse(model_price >= Last,1,-1))) %>% 
					ddply(.,.(OptionSymbol),.fun=take_cumulative_position,.parallel=TRUE) %>% 
					tbl_df %>% ungroup %>%
					group_by(OptionSymbol) %>% 
					arrange(DataDate) %>%
					mutate(delta_change= (model_delta*net_position - lag(model_delta*net_position,default=0) + end_position*model_delta)) %>% 
					# mutate(delta_change= ifelse(Type=="call",-1,1)*(model_delta*net_position - lag(model_delta*net_position,default=0) + end_position*model_delta), cs=round(cumsum(delta_change),4)) %>% 
					ungroup %>%
					mutate(naked_pnl= (naked_position)*(pmax(0,ifelse(Type=="call",1,-1)*(ExpirationPrice-Strike))*exp(-RiskFreeRate*NetMaturity/252)-Last),
							delta_pnl= net_position*model_delta*(ExpirationPrice*exp(-RiskFreeRate*NetMaturity/252)-UnderlyingPrice),
							hedge_and_forget_pnl=naked_pnl+delta_pnl) %>% 
					mutate(RPE= round((model_price-Last)/Last,4),
							ARPE= abs(RPE),
							PE = (model_price-Last),
							APE=abs(PE),
							SE=PE^2) %>% 
					group_by(OptionSymbol) %>%
					mutate(naked_cumulative_pnl=(cumulative_position+last_position)*(-Last),naked_cumulative_pnl=ifelse(is.na(naked_cumulative_pnl),0,naked_cumulative_pnl)) %>%
					mutate(delta_change_to_last= abs(net_position_to_last)*(model_delta*net_position_to_last - lag(model_delta*net_position_to_last,default=0) + last_position*model_delta)) %>% 
					mutate(delta_change_pnl_to_last=delta_change_to_last*(-UnderlyingPrice),
							daily_pnl_to_last=naked_cumulative_pnl+delta_change_pnl_to_last,
							cumulative_pnl_to_last=abs(net_position_to_last)*cumsum(daily_pnl_to_last)) %>%
					ungroup

	save(trade_error,file=paste0(output_path,underlying_asset,"_",data_year,"_",model_abbr,"_data_withdiv_",sym_abbr,parameter_period,"y_trade_and_error.RData"))
}

for(j in c("NDX","SPX")){
	print(j)
	for(i in 2008:2013){
	print(i)
	convert_to_error_and_trading_data(model_abbr="BS",parameter_period=2,sym=TRUE,underlying_asset=j,data_year=i)
	convert_to_error_and_trading_data(model_abbr="BS",parameter_period=5,sym=TRUE,underlying_asset=j,data_year=i)
	# convert_to_error_and_trading_data(model_abbr="HN",parameter_period=2,sym=TRUE,underlying_asset=j,data_year=i)
	# convert_to_error_and_trading_data(model_abbr="HN",parameter_period=5,sym=TRUE,underlying_asset=j,data_year=i)
	# convert_to_error_and_trading_data(model_abbr="HN",parameter_period=2,sym=FALSE,underlying_asset=j,data_year=i)
	# convert_to_error_and_trading_data(model_abbr="HN",parameter_period=5,sym=FALSE,underlying_asset=j,data_year=i)
	}
}



# convert_to_error_and_trading_data(model_abbr="HN",parameter_period=5,sym=FALSE,underlying_asset="SPX",data_year=2011)
j<-"NDX"
	for(i in 2008:2013){
	print(i)
	convert_to_error_and_trading_data(model_abbr="BS",parameter_period=2,sym=TRUE,underlying_asset=j,data_year=i)
	convert_to_error_and_trading_data(model_abbr="BS",parameter_period=5,sym=TRUE,underlying_asset=j,data_year=i)
}
