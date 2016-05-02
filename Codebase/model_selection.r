main_path <- "~/Dropbox/PhD_Workshop/"
source(paste0(main_path,"Codebase/00_run_this_first_option_calculations.r"))

if(!("party" %in% rownames(installed.packages())))
	install.packages("party")

library(party)

party_rules<-ctree_control(minbucket=7)

#same as trading.r
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


predict_clusters<-function(the_df,lookback=1,help_df,trim_expiration=FALSE){

	year_month<-the_df %>% select(year_month) %>% slice(1) %>% unlist

	training_start<-as.Date(paste0(as.numeric(substr(year_month,1,4))-lookback,"-",substr(year_month,6,7),"-","01"))
	training_end <- as.Date(paste0(year_month,"-01"))

	if(trim_expiration){
		training_df <- help_df %>% filter(DataDate >= training_start & RealExpiration < training_end) %>% filter(!is.na(Response))
	}else{
		training_df <- help_df %>% filter(DataDate >= training_start & DataDate < training_end) %>% filter(!is.na(Response))
	}

	#Tree model fit
	cluster_learn<-ctree(Response ~ NetMaturity + Moneyness + Type, data=training_df, controls=party_rules)
	#Find nodes and node averages
	cluster_nodes<-predict(cluster_learn,type="node")
	node_averages<-aggregate(training_df$Response,by=list(cluster_nodes),"mean")
	colnames(node_averages)<-c("node","prediction_value")
	
	the_df %>% 
			cbind(.,node=predict(cluster_learn, newdata=.,type="node")) %>% 
			left_join(.,node_averages,by="node") %>%
			select(-node) %>% tbl_df
}


get_error_predictions<-function(underlying_asset,data_year,model_name,sym_abbr,parameter_period,error_type,lookback=1){

	load(paste0(main_path,"Output Files/Trading_and_Error/",underlying_asset,"_",data_year,"_",model_name,"_data_withdiv_",sym_abbr,parameter_period,"y_trade_and_error.RData"))
	predict_df<-trade_error 

	if(lookback > 0){
		for(i in 1:lookback){
			load(paste0(main_path,"Output Files/Trading_and_Error/",underlying_asset,"_",data_year-i,"_",model_name,"_data_withdiv_",sym_abbr,parameter_period,"y_trade_and_error.RData"))
			predict_df<-rbind(predict_df,trade_error) 
		}
	}else{
		stop("Lookback problematic.")
	}

	predict_df <- predict_df %>%
					rename_(Response=error_type) %>% 
					select(OptionSymbol,DataDate,Moneyness,NetMaturity,Type,Response,RealExpiration) %>% 				
					mutate(Type=as.factor(Type),year_month=format(DataDate,"%Y-%m")) 

	if(error_type %in% c("naked_pnl","hedge_and_forget_pnl")){
		future_outcome<-TRUE
	}else{
		future_outcome<-FALSE
	}

	set.seed(201601)

	predict_df %>%
		filter(DataDate >= as.Date(paste0(data_year,"-01-01"))) %>%
		ddply(.,.(year_month),.fun=predict_clusters,lookback=lookback,help_df=predict_df,trim_expiration=future_outcome,.inform=FALSE,.parallel=TRUE) %>% 
		# rename_(paste0(model_name,"_",sym_abbr,parameter_period,"y")="prediction_value") %>%
		tbl_df %>% 
		select(-Response,-year_month) %>%
		ungroup

}

model_selection<-function(underlying_asset,the_year,contenders,error_type="naked_pnl",error_obj="max",ms_code="5_models_v01",lookback=1){

	ms_data<-get_error_predictions(underlying_asset=underlying_asset,
											data_year=the_year,
											model_name=contenders$model_name[1],
											sym_abbr=contenders$sym_abbr[1],
											parameter_period=contenders$parameter_period[1],
											error_type=error_type,
											lookback=lookback) 	

	colnames(ms_data)<-gsub("prediction_value",paste0(contenders$model_name[1],"_",contenders$sym_abbr[1],contenders$parameter_period[1],"y"),colnames(ms_data))

	for(i in 2:nrow(contenders)){

		ms_data <- get_error_predictions(underlying_asset=underlying_asset,
											data_year=the_year,
											model_name=contenders$model_name[i],
											sym_abbr=contenders$sym_abbr[i],
											parameter_period=contenders$parameter_period[i],
											error_type=error_type,
											lookback=lookback) %>% 
						select(prediction_value) %>% 
						cbind(ms_data,.)

		colnames(ms_data)<-gsub("prediction_value",paste0(contenders$model_name[i],"_",contenders$sym_abbr[i],contenders$parameter_period[i],"y"),colnames(ms_data))
				
	}

	selection_raw_data <-	ms_data %>% 
								select(-(OptionSymbol:Type),-RealExpiration)

	if(error_obj=="max"){
		selection_raw_data <- 	selection_raw_data %>% 
							mutate(selection_index=max.col(.)) %>% select(selection_index) 
	}else if(error_obj=="min_abs"){
		selection_raw_data <- selection_raw_data %>%
							mutate_each(.,funs(neg_abs= (-1)*abs(.))) %>%
							ungroup %>%
							mutate(selection_index=max.col(.)) %>% select(selection_index) 
	}else{
		stop("Wrong error objective!")
	}

	selection_raw_data <- selection_raw_data %>% 
			cbind(ms_data,.) %>%
			select(OptionSymbol,DataDate,selection_index) %>%
			tbl_df

	contender_index_data <- selection_raw_data %>% 
								filter(selection_index == 1) 

	load(paste0(main_path,"Output Files/Trading_and_Error/",underlying_asset,"_",the_year,"_",contenders$model_name[1],"_data_withdiv_",contenders$sym_abbr[1],contenders$parameter_period[1],"y_trade_and_error.RData"))

	if(contenders$model_name[1]=="BS"){

			trade_error <- trade_error %>% select(-HistVol,-contains("BS_"))

	}else if(contenders$model_name[1]=="HN"){

			trade_error <- trade_error %>% select(-(lambda:gamma),-contains("HN_"))

	}

	ms_trade_error <- left_join(contender_index_data,trade_error,by=c("OptionSymbol","DataDate"))

	for(i in 2:nrow(contenders)){

		contender_index_data <- selection_raw_data %>% 
									filter(selection_index == i) 

		load(paste0(main_path,"Output Files/Trading_and_Error/",underlying_asset,"_",the_year,"_",contenders$model_name[i],"_data_withdiv_",contenders$sym_abbr[i],contenders$parameter_period[i],"y_trade_and_error.RData"))

		if(contenders$model_name[i]=="BS"){
				trade_error <- trade_error %>% select(-HistVol,-contains("BS_"))
		}else if(contenders$model_name[i]=="HN"){
				trade_error <- trade_error %>% select(-(lambda:gamma),-contains("HN_"))
		}
		ms_trade_error <- left_join(contender_index_data,trade_error,by=c("OptionSymbol","DataDate")) %>% rbind(ms_trade_error,.)
	}

	contender_names<- contenders %>% mutate(parameter_period=paste0(parameter_period,"y")) %>% apply(.,1,paste0,collapse="_") %>% gsub("_$","",.) %>% data.frame(models=.) %>% mutate(selection_index=1:5)

	ms_trade_error <- ms_trade_error %>% arrange(OptionSymbol,DataDate) %>% left_join(.,contender_names,by="selection_index") %>% select(-selection_index)

	ms_trade_error <- ms_trade_error %>% select(OptionSymbol:Maturity,models,naked_position) %>% 
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

	save(ms_trade_error,file=paste0(main_path,"Output Files/Trading_and_Error/",underlying_asset,"_",the_year,"_","MS_data_withdiv_",error_obj,"_",error_type,"_",ms_code,"_",lookback,"y_lookback_trade_and_error.RData"))

}


# underlying_asset<-"SPX"
# the_year<-that_year
# lookback<-1

# underlying_asset="SPX"
# that_year=the_year=2013
# contenders=contenders
# error_type="ARPE"
# error_obj="min_abs"
# ms_code="5_models_v01"
# lookback=2


contenders<-data.frame(model_name=c("BS","BS","HN","HN","HN"),parameter_period=c(2,5,2,2,5),sym_abbr=c("","","symm_","asym_","asym_"))

for(that_year in 2009:2013){
	print(that_year)
	print("max")
	model_selection(underlying_asset="SPX",the_year=that_year,contenders=contenders,error_type="naked_pnl",error_obj="max",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="naked_pnl",error_obj="max",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="SPX",the_year=that_year,contenders=contenders,error_type="hedge_and_forget_pnl",error_obj="max",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="hedge_and_forget_pnl",error_obj="max",ms_code="5_models_v01",lookback=1)
	print("min_abs")
	model_selection(underlying_asset="SPX",the_year=that_year,contenders=contenders,error_type="naked_pnl",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="naked_pnl",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="SPX",the_year=that_year,contenders=contenders,error_type="hedge_and_forget_pnl",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="hedge_and_forget_pnl",error_obj="min_abs",ms_code="5_models_v01",lookback=1)

}

for(that_year in 2009:2013){
	print(that_year)
	print("min_abs")
	model_selection(underlying_asset="SPX",the_year=that_year,contenders=contenders,error_type="ARPE",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="ARPE",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="SPX",the_year=that_year,contenders=contenders,error_type="RPE",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="RPE",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="SPX",the_year=that_year,contenders=contenders,error_type="APE",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="APE",error_obj="min_abs",ms_code="5_models_v01",lookback=1)

}





for(that_year in 2009:2013){
	print(that_year)
	print("max")
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="naked_pnl",error_obj="max",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="hedge_and_forget_pnl",error_obj="max",ms_code="5_models_v01",lookback=1)
	print("min_abs efficiency")
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="naked_pnl",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="hedge_and_forget_pnl",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	print("min_abs")
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="ARPE",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="RPE",error_obj="min_abs",ms_code="5_models_v01",lookback=1)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="APE",error_obj="min_abs",ms_code="5_models_v01",lookback=1)

}


for(that_year in 2010:2013){
	print(that_year)
	print("max")
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="naked_pnl",error_obj="max",ms_code="5_models_v01",lookback=2)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="hedge_and_forget_pnl",error_obj="max",ms_code="5_models_v01",lookback=2)
	print("min_abs efficiency")
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="naked_pnl",error_obj="min_abs",ms_code="5_models_v01",lookback=2)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="hedge_and_forget_pnl",error_obj="min_abs",ms_code="5_models_v01",lookback=2)
	print("min_abs")
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="ARPE",error_obj="min_abs",ms_code="5_models_v01",lookback=2)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="RPE",error_obj="min_abs",ms_code="5_models_v01",lookback=2)
	model_selection(underlying_asset="NDX",the_year=that_year,contenders=contenders,error_type="APE",error_obj="min_abs",ms_code="5_models_v01",lookback=2)

}

