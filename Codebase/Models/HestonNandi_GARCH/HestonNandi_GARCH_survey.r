#Heston-Nandi runs for "Survey Paper"

source("~/Dropbox/PhD_Workshop/Codebase/Models/HestonNandi_GARCH/HestonNandi_GARCH.r")

HNBulk_modified<-function(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset,data_year,parameter_period=2,sym=TRUE,output_path="~/Dropbox/PhD_Workshop/Output Files/Survey_Results/HestonNandi_GARCH/",progressOutput=TRUE,output_suffix="000000",r_choice=0,div_choice=0){
	option_specs<-read.csv(paste0(input_path,"Asset Options/",underlying_asset,"_",data_year,"_options_filtered_",filter_suffix,".csv"),header=TRUE)
	asset_data<-read.csv(paste0(input_path,"Asset Prices/",underlying_asset,"_daily_processed_HN_",ifelse(sym,"symmetric","nonsymmetric"),".csv"),header=TRUE)
	asset_data$Date<-as.Date(asset_data$Date,format=ifelse(nchar(as.character(asset_data$Date))==8,"%d/%m/%y","%Y-%m-%d"))
	option_specs$DataDate<-as.Date(option_specs$DataDate)
#Quick hack to fix
	param_data<-asset_data[match(option_specs$DataDate,asset_data$Date),paste0(c("lambda", "omega", "alpha", "beta", "gamma"),parameter_period,"y")]
#	param_data<-asset_data[match(option_specs$DataDate,asset_data$Date),paste0(c("lambda", "omega", "alpha", "beta", "gamma"),parameter_period,"y")]
	colnames(param_data)<-c("lambda", "omega", "alpha", "beta", "gamma")
	if(r_choice==0 && div_choice==0){
		print("Risk free rate is spline and dividend is continuous")
		HN_prices<-mapply(HNGarchEOPT,s0=option_specs$UnderlyingPrice,K=option_specs$Strike,time_to_maturity=option_specs$NetMaturity,callOrPut=option_specs$Type,lambda=param_data$lambda, omega = param_data$omega, alpha = param_data$alpha,
							beta = param_data$beta, gamma = param_data$gamma, rowCount=1:nrow(option_specs), r=option_specs$RiskFreeRate, div=option_specs$Dividend.Yield, MoreArgs=list(progressOutput=TRUE))
	}else if(r_choice==0 && div_choice==1){
		print("Risk free rate is spline and dividend is ignored")
		HN_prices<-mapply(HNGarchEOPT,s0=(option_specs$UnderlyingPrice*(1-option_specs$Dividend.Yield*option_specs$NetMaturity/252)),K=option_specs$Strike,time_to_maturity=option_specs$NetMaturity,callOrPut=option_specs$Type,lambda=param_data$lambda, omega = param_data$omega, alpha = param_data$alpha,
							beta = param_data$beta, gamma = param_data$gamma, rowCount=1:nrow(option_specs), r=option_specs$RiskFreeRate, MoreArgs=list(progressOutput=TRUE,div=0))
	}else if(r_choice==0 && div_choice==2){
		print("Risk free rate is spline and dividend is ignored")
		HN_prices<-mapply(HNGarchEOPT,s0=option_specs$UnderlyingPrice,K=option_specs$Strike,time_to_maturity=option_specs$NetMaturity,callOrPut=option_specs$Type,lambda=param_data$lambda, omega = param_data$omega, alpha = param_data$alpha,
							beta = param_data$beta, gamma = param_data$gamma, rowCount=1:nrow(option_specs), r=option_specs$RiskFreeRate, MoreArgs=list(progressOutput=TRUE,div=0))
	}else if(r_choice==2 && div_choice==0){
		print("Risk free rate is fixed at 2% yearly and dividend is continuous")
		HN_prices<-mapply(HNGarchEOPT,s0=option_specs$UnderlyingPrice,K=option_specs$Strike,time_to_maturity=option_specs$NetMaturity,callOrPut=option_specs$Type,lambda=param_data$lambda, omega = param_data$omega, alpha = param_data$alpha,
							beta = param_data$beta, gamma = param_data$gamma, rowCount=1:nrow(option_specs), div=option_specs$Dividend.Yield, MoreArgs=list(progressOutput=TRUE, r=0.02/252))
	}else if(r_choice==2 && div_choice==1){
		print("Risk free rate is fixed at 2% yearly and dividend is continuous")
		HN_prices<-mapply(HNGarchEOPT,s0=(option_specs$UnderlyingPrice*(1-option_specs$Dividend.Yield*option_specs$NetMaturity/252)),K=option_specs$Strike,time_to_maturity=option_specs$NetMaturity,callOrPut=option_specs$Type,lambda=param_data$lambda, omega = param_data$omega, alpha = param_data$alpha,
							beta = param_data$beta, gamma = param_data$gamma, rowCount=1:nrow(option_specs), MoreArgs=list(progressOutput=TRUE, r=0.02/252, div=0))
	}else if(r_choice==2 && div_choice==2){
		print("Risk free rate is fixed at 2% yearly and dividend is continuous")
		HN_prices<-mapply(HNGarchEOPT,s0=option_specs$UnderlyingPrice,K=option_specs$Strike,time_to_maturity=option_specs$NetMaturity,callOrPut=option_specs$Type,lambda=param_data$lambda, omega = param_data$omega, alpha = param_data$alpha,
							beta = param_data$beta, gamma = param_data$gamma, rowCount=1:nrow(option_specs), MoreArgs=list(progressOutput=TRUE, r=0.02/252, div=0))
	}else{
		break("No proper option selection, aborting.")
	}
	HN_nonnegative<-HN_prices
	HN_nonnegative[HN_nonnegative<0]<-0
	price_output<-data.frame(option_specs[,c(1,2,4,5,7,8,9,14,20,23,24,25)],HN_prices=HN_nonnegative, HN_raw=HN_prices,param_data)
	write.table(price_output,paste0(output_path,underlying_asset,"_",data_year,"_HN_data_withdiv_",ifelse(sym,"symm","asym"),"_",parameter_period,"y_",output_suffix,".csv"),row.names=FALSE,sep=",")	
}

for(i in 2013:2009){
	HNBulk_modified(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=i,parameter_period=5,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/Survey_Results/HestonNandi_GARCH/",progressOutput=TRUE,output_suffix="000000",r_choice=0,div_choice=0)
}

for(i in 2013:2009){
	HNBulk_modified(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=i,parameter_period=5,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/Survey_Results/HestonNandi_GARCH/",progressOutput=TRUE,output_suffix="010000",r_choice=0,div_choice=1)
}

for(i in 2013:2009){
	HNBulk_modified(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=i,parameter_period=5,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/Survey_Results/HestonNandi_GARCH/",progressOutput=TRUE,output_suffix="020000",r_choice=0,div_choice=2)
}

for(i in 2013:2009){
	HNBulk_modified(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=i,parameter_period=5,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/Survey_Results/HestonNandi_GARCH/",progressOutput=TRUE,output_suffix="002000",r_choice=2,div_choice=0)
}

for(i in 2013:2009){
	HNBulk_modified(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=i,parameter_period=5,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/Survey_Results/HestonNandi_GARCH/",progressOutput=TRUE,output_suffix="012000",r_choice=2,div_choice=1)
}

for(i in 2013:2009){
	HNBulk_modified(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=i,parameter_period=5,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/Survey_Results/HestonNandi_GARCH/",progressOutput=TRUE,output_suffix="022000",r_choice=2,div_choice=2)
}

#cases 000000

#dividend rate is digit 2
#0: dividend continuous
#1: dividend discount from S0
#2: dividend ignore


#risk free rate is digit 3
#0: risk-free spline
#1: risk-free fixed on a single Yield
#2: risk-free fixed on a value (%2 yearly)


#000000
#data cleaning
#dividend
#risk-free
#inference
#extra_slot
#extra_slot
