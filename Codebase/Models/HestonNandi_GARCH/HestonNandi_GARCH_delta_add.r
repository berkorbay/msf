main_path <- "~/Dropbox/PhD_Workshop/"
source(paste0(main_path,"Codebase/00_run_this_first_option_calculations.r"))

calculate_HN_parameters<-function(dates,log_returns,years_back,sym){
	time_machine<-as.Date(paste0(as.numeric(substr(dates,1,4))-years_back,substr(dates,5,10)))
	feb29s<-which(is.na(time_machine))
	time_machine[feb29s]<-as.Date(paste0(as.numeric(substr(dates[feb29s],1,4))-years_back,substr(dates[feb29s]-1,5,10)))
	places<-match(time_machine,dates)
	while(time_machine[min(which(is.na(places)))]>=min(dates)){
		time_machine[which(is.na(places))]<-time_machine[which(is.na(places))]-1
		places<-match(time_machine,dates)
	}
	places<-places-1
	errorResponse<-setNames(rep(-1,5),c("lambda","omega","alpha","beta","gamma"))
	print("Starting")
	mapply(function(x,y,returns,sym) if(is.na(y)){errorResponse}else{if(x%%10==0) print(paste(x,"out of", length(places))); hngarchFit(returns[x:y],symmetric=sym)$estimate},x=1:length(places),y=places,MoreArgs=list(returns=log_returns,sym=sym))	
}


HNParameters<-function(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily",underlying_asset,sym=TRUE,output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",progressOutput=TRUE){
	if(progressOutput){
		print("Reading the file.")
	}	
	raw_data<-read.csv(paste0(data_path,underlying_asset,"_",data_type,"_processed.csv"),header=TRUE)
	raw_data$Date<-as.Date(raw_data$Date)
	if(progressOutput){
		print("Getting 1 year parameters.")
	}	
	HN1y<-t(calculate_HN_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=1,sym=sym))
	colnames(HN1y)<-paste0(colnames(HN1y),1,"y")
	enhanced_data<-data.frame(raw_data,HN1y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_HN_",ifelse(sym,"symmetric","nonsymmetric"),".csv"),sep=",",append=FALSE,row.names=FALSE)

	if(progressOutput){
		print("Getting 2 years parameters.")
	}	
	HN2y<-t(calculate_HN_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=2,sym=sym))
	colnames(HN2y)<-paste0(colnames(HN2y),2,"y")
	enhanced_data<-data.frame(enhanced_data,HN2y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_HN_",ifelse(sym,"symmetric","nonsymmetric"),".csv"),sep=",",append=FALSE,row.names=FALSE)

	if(progressOutput){
		print("Getting 3 years parameters.")
	}	
	HN3y<-t(calculate_HN_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=3,sym=sym))
	colnames(HN3y)<-paste0(colnames(HN3y),3,"y")
	enhanced_data<-data.frame(enhanced_data,HN3y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_HN_",ifelse(sym,"symmetric","nonsymmetric"),".csv"),sep=",",append=FALSE,row.names=FALSE)

	if(progressOutput){
		print("Getting 5 years parameters.")
	}	
	HN5y<-t(calculate_HN_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=5,sym=sym))
	colnames(HN5y)<-paste0(colnames(HN5y),5,"y")
	enhanced_data<-data.frame(enhanced_data,HN5y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_HN_",ifelse(sym,"symmetric","nonsymmetric"),".csv"),sep=",",append=FALSE,row.names=FALSE)

	# if(progressOutput){
	# 	print("Getting 10 years parameters.")
	# }	
	# HN10y<-t(calculate_HN_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=10,sym=sym))
	# colnames(HN10y)<-paste0(colnames(HN10y),years_back,"y")
	# enhanced_data<-data.frame(enhanced_data,HN10y)
	# write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_HN_",ifelse(sym,"symmetric","nonsymmetric"),".csv"),sep=",",append=FALSE,row.names=FALSE)

}
#HNParameters(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily",underlying_asset="NDX",sym=TRUE,output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",progressOutput=TRUE)

#This function is a modified function for fOptions
.fstarHN<-function (phi, const, model, S, X, Time.inDays, r.daily, div.yield) 
{
    lambda = -1/2
    omega = model$omega
    alpha = model$alpha
    gamma = model$gamma + model$lambda + 1/2
    beta = model$beta
    sigma2 = (omega + alpha)/(1 - beta - alpha * gamma^2)
    cphi0 = phi * complex(real = 0, imaginary = 1)
    cphi = cphi0 + const
    a = cphi * (r.daily - div.yield)
    b = lambda * cphi + cphi * cphi/2
    for (i in 2:Time.inDays) {
        a = a + cphi *(r.daily - div.yield) + b * omega - log(1 - 2 * alpha * 
            b)/2
        b = cphi * (lambda + gamma) - gamma^2/2 + beta * b + 
            0.5 * (cphi - gamma)^2/(1 - 2 * alpha * b)
    }
    f = Re(exp(-cphi0 * log(X) + cphi * log(S) + a + b * sigma2)/cphi0)/pi
    f
}

.fdeltaHN<-function (phi, const, model, S, X, Time.inDays, r.daily, div.yield) 
{
    cphi0 = phi * complex(real = 0, imaginary = 1)
    cphi = cphi0 + const
    fdelta = cphi * .fHN(phi, const, model, S, X, Time.inDays, 
        (r.daily-div.yield))/S
    Re(fdelta)
}

.fgammaHN<-function (phi, const, model, S, X, Time.inDays, r.daily, div.yield) 
{
    cphi0 = phi * complex(real = 0, imaginary = 1)
    cphi = cphi0 + const
    fgamma = cphi * (cphi - 1) * .fHN(phi, const, model, S, X, 
        Time.inDays, r.daily - div.yield)/S^2
    Re(fgamma)
}

HNGGreeks<-function(Selection = c("Delta", "Gamma"), TypeFlag = c("c","p"), model, S, X, Time.inDays, r.daily, div.yield) 
{
    Selection = Selection[1]
    TypeFlag = TypeFlag[1]
    if (Selection == "Delta") {
        delta1 = integrate(.fdeltaHN, 0, Inf, const = 1, model = model, 
            S = S, X = X, Time.inDays = Time.inDays, r.daily = r.daily, div.yield=div.yield ,stop.on.error=FALSE)
        if (is.null(delta1$value)) 
            delta1$value = delta1$integral
        delta2 = integrate(.fdeltaHN, 0, Inf, const = 0, model = model, 
            S = S, X = X, Time.inDays = Time.inDays, r.daily = r.daily, div.yield=div.yield ,stop.on.error=FALSE)
        if (is.null(delta2$value)) 
            delta2$value = delta2$integral
        greek = 1/2 + exp(-r.daily * Time.inDays) * delta1$value - 
            X * exp(-r.daily * Time.inDays) * delta2$value
        if (TypeFlag == "p") 
            greek = greek - 1
    }
    if (Selection == "Gamma") {
        gamma1 = integrate(.fgammaHN, 0, Inf, const = 1, model = model, 
            S = S, X = X, Time.inDays = Time.inDays, r.daily = r.daily, div.yield=div.yield ,stop.on.error=FALSE)
        if (is.null(gamma1$value)) 
            gamma1$value = gamma1$integral
        gamma2 = integrate(.fgammaHN, 0, Inf, const = 0, model = model, 
            S = S, X = X, Time.inDays = Time.inDays, r.daily = r.daily, div.yield=div.yield ,stop.on.error=FALSE)
        if (is.null(gamma2$value)) 
            gamma2$value = gamma2$integral
        greek = put.gamma = exp(-r.daily * Time.inDays) * gamma1$value - 
            X * exp(-r.daily * Time.inDays) * gamma2$value
    }
    greek
}

#Since row 10629 gives error on "probably divergent message" the HNGOption is modified
#All the integrals are given stop.on.error=FALSE property
HNGOption<-function (TypeFlag = c("c", "p"), model, S, X, Time.inDays, r.daily, div.yield) 
{
    TypeFlag = TypeFlag[1]
    #The changed function (1/2)
    call1 = integrate(.fstarHN, 0, Inf, const = 1, model = model, 
        S = S, X = X, Time.inDays = Time.inDays, r.daily = r.daily, div.yield=div.yield ,stop.on.error=FALSE)
    if (is.null(call1$value)) 
        call1$value = call1$integral
# print(call1$value)
    #The changed function (2/2)
    call2 = integrate(.fstarHN, 0, Inf, const = 0, model = model, 
        S = S, X = X, Time.inDays = Time.inDays, r.daily = r.daily, div.yield=div.yield, stop.on.error=FALSE)
# print(call2$value)
    if (is.null(call2$value)) 
        call2$value = call2$integral
    call.price = S/2 + exp(-(r.daily - div.yield) * Time.inDays) * call1$value - 
        X * exp(-r.daily * Time.inDays) * (1/2 + call2$value)
# print(call.price)
    price = NA
    if (TypeFlag == "c") 
        price = call.price
    if (TypeFlag == "p") 
        price = call.price + X * exp(-r.daily * Time.inDays) - S
    option = list(price = price, call = match.call())
    class(option) = "option"
    option
}

HNGarchEOPT<-function(s0,K,time_to_maturity,r,div,callOrPut,lambda,omega,alpha,beta,gamma,rowCount,progressOutput=TRUE){
	if(progressOutput && runif(1) > 0.9995){
		print(paste0(rowCount," Still working..."))
	}	
 	#	fitobject<-hngarchFit(returns,symmetric=sym)
	if(callOrPut=="call"){
		callput<-"c"		
	}else if(callOrPut=="put"){
		callput<-"p"
	}else{
		stop("Call Put is not working!!!!!!")
	}
	priceobject<-HNGOption(callput,model=list(lambda =lambda, omega = omega, alpha = alpha, beta = beta, gamma = gamma),s0,K,time_to_maturity,r/252,div/252)
	# if(progressOutput){
	# 	print(paste0("Price is ",priceobject$price))
	# }		
	return(priceobject$price)
}

HNBulk<-function(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset,data_year,parameter_period=2,sym=TRUE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE){
	option_specs<-read.csv(paste0(input_path,"Asset Options/",underlying_asset,"_",data_year,"_options_filtered_",filter_suffix,".csv"),header=TRUE)
	asset_data<-read.csv(paste0(input_path,"Asset Prices/",underlying_asset,"_daily_processed_HN_",ifelse(sym,"symmetric","nonsymmetric"),".csv"),header=TRUE)
	asset_data$Date<-as.Date(asset_data$Date,format=ifelse(nchar(as.character(asset_data$Date))==8,"%d/%m/%y","%Y-%m-%d"))
	option_specs$DataDate<-as.Date(option_specs$DataDate)
	#Quick hack to fix
	param_data<-asset_data[match(option_specs$DataDate,asset_data$Date),paste0(c("lambda", "omega", "alpha", "beta", "gamma"),parameter_period,"y")]
	#	param_data<-asset_data[match(option_specs$DataDate,asset_data$Date),paste0(c("lambda", "omega", "alpha", "beta", "gamma"),parameter_period,"y")]
	colnames(param_data)<-c("lambda", "omega", "alpha", "beta", "gamma")
	HN_prices<-mapply(HNGarchEOPT,s0=option_specs$UnderlyingPrice,K=option_specs$Strike,time_to_maturity=option_specs$NetMaturity,callOrPut=option_specs$Type,lambda=param_data$lambda, omega = param_data$omega, alpha = param_data$alpha,
						beta = param_data$beta, gamma = param_data$gamma, rowCount=1:nrow(option_specs), r=option_specs$RiskFreeRate, div=option_specs$Dividend.Yield, MoreArgs=list(progressOutput=TRUE))
	HN_nonnegative<-HN_prices
	HN_nonnegative[HN_nonnegative<0]<-0
	price_output<-data.frame(option_specs[,c(1,2,4,5,7,8,9,14,20,23,24,25)],HN_prices=HN_nonnegative, HN_raw=HN_prices,param_data)
	write.table(price_output,paste0(output_path,underlying_asset,"_",data_year,"_HN_data_withdiv_",ifelse(sym,"symm","asym"),"_",parameter_period,"y.csv"),row.names=FALSE,sep=",")	
}

HNGarchEOPT_Greeks<-function(s0,K,time_to_maturity,r,div,callOrPut,lambda,omega,alpha,beta,gamma,rowCount,progressOutput=TRUE){
	if(progressOutput && runif(1) > 0.9995){
		print(paste0(rowCount," Still working..."))
	}	
	#fitobject<-hngarchFit(returns,symmetric=sym)
	if(callOrPut=="call"){
		callput<-"c"		
	}else if(callOrPut=="put"){
		callput<-"p"
	}else{
		stop("Call Put is not working!!!!!!")
	}
	delta_value<-HNGGreeks(Selection="Delta",TypeFlag=callput,model=list(lambda =lambda, omega = omega, alpha = alpha, beta = beta, gamma = gamma),S=s0,X=K,Time.inDays=time_to_maturity,r.daily=r/252,div.yield=div/252)
	# if(progressOutput){
	# 	print(paste0("Price is ",priceobject$price))
	# }		
	return(delta_value)
}


# input_path="Input Files/"
# filter_suffix="B12"
# underlying_asset<-"NDX"
# data_year<-2009
# parameter_period=2
# sym=TRUE
# output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/"
# progressOutput=TRUE


HN_Delta_Bulk<-function(input_path="Input Files/",filter_suffix="B12",underlying_asset,data_year,parameter_period=2,sym=TRUE,output_path="Output Files/HestonNandi_GARCH/",progressOutput=TRUE){

	option_input_path<-paste0(main_path,input_path,"Asset Position Raw/")	
	asset_input_path<-paste0(main_path,input_path,"Asset Prices/")
	dividend_input_path<-paste0(main_path,input_path,"Asset Specials/")
	rates_data_path<-paste0(main_path,input_path,"Other/")
	output_path<-paste0(main_path,output_path)

	# option_data<-read.csv(paste0(option_input_path,underlying_asset,"_",data_year,"_options_filtered_",filter_suffix,".csv")) %>% tbl_df
	load(paste0(option_input_path,underlying_asset,"_",data_year,"_price_delta_raw.RData")) #This function brings delta_data

	asset_data<-read.csv(paste0(asset_input_path,underlying_asset,"_daily_processed_HN_",ifelse(sym,"symmetric","nonsymmetric"),".csv"),header=TRUE) %>%
						tbl_df %>%
						mutate(DataDate=as.Date(Date,format=ifelse(nchar(as.character(Date))==8,"%d/%m/%y","%Y-%m-%d"))) %>% 
						select(DataDate,contains(paste0(parameter_period,"y")))

	colnames(asset_data)<-gsub(paste0(parameter_period,"y"),"",colnames(asset_data))

	# price_specs <- read.csv(paste0(output_path,underlying_asset,"_",data_year,"_HN_data_withdiv_",ifelse(sym,"symm","asym"),"_",parameter_period,"y.csv"),header=TRUE) %>% 
	# 					tbl_df %>%
	# 					mutate(DataDate=as.Date(DataDate)) %>% 
	# 					select(OptionSymbol,DataDate,contains("_prices"))


	option_specs <- delta_data %>%
						left_join(.,asset_data,by="DataDate")

	# option_specs <- read.csv(paste0(input_path,"Asset Position Raw/",underlying_asset,"_",data_year,"_price_delta_raw.csv"),header=TRUE) %>% 
	# 					tbl_df	%>%
	# 					mutate(DataDate=as.Date(DataDate)) %>%
	# 					left_join(.,price_specs,by=c("OptionSymbol","DataDate")) %>%
	# 					left_join(.,asset_data,by="DataDate")

	mdply_data <- option_specs %>%
						ungroup %>%
						mutate(rowCount=1:nrow(.)) %>%
						select(s0=UnderlyingPrice,
							    K=Strike,
							   time_to_maturity=NetMaturity,
							     r=RiskFreeRate,
							   div=Dividend.Yield,
							   callOrPut=Type,
							   lambda=contains("lambda"),
							    omega=contains("omega"),
							    alpha=contains("alpha"),
							     beta=contains("beta"),
							    gamma=contains("gamma"),
							   rowCount)

	price_table<- mdply(mdply_data,.fun=HNGarchEOPT,.inform=FALSE,.parallel=TRUE) %>% 
						select(HN_prices_raw=V1) %>%
						mutate(HN_prices=ifelse(HN_prices_raw < 0,0,HN_prices_raw)) %>%
						cbind(option_specs,.) %>% 
						tbl_df 

										   
	price_delta<- mdply(mdply_data,.fun=HNGarchEOPT_Greeks,.inform=FALSE,.parallel=TRUE) %>% 
						select(HN_Delta_raw=V1) %>%
						mutate(HN_Delta=ifelse(HN_Delta_raw > 1,1,HN_Delta_raw),HN_Delta=ifelse(HN_Delta < -1,-1,HN_Delta)) %>%
						cbind(price_table,.) %>% 
						tbl_df %>% 
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
								HN_prices,
								HN_Delta,
								RealExpiration,
								ExpirationPrice,
								ExpirationMoneyness,
								RiskFreeRate,
								Dividend.Yield,
								Maturity,
								lambda,
								omega,
								alpha,
								beta,
								gamma,
								IV,
								DataDelta,
								HN_prices_raw,
								HN_Delta_raw
								)

	# delta_table <- delta_table %>% mutate(HN_Delta=ifelse(HN_Delta_raw > 1,1,HN_Delta_raw),HN_Delta=ifelse(HN_Delta < -1,-1,HN_Delta))


	# price_specs <- option_specs %>% select(DataDate,OptionSymbol,Maturity,RiskFreeRate,Dividend.Yield) %>% left_join(price_specs,.)

	# next_date_df<-option_specs %>% select(DataDate) %>% distinct(DataDate) %>% arrange(DataDate)
	# next_date <- c(as.Date(unlist(next_date_df),origin = "1970-01-01")[-1],NA)
	# # next_date <- c(as.Date(unlist(next_date_df),origin = "1970-01-01")[-1],
	# # 				as.Date(read.csv(paste0(input_path,"Asset Options/",underlying_asset,"_",data_year+1,"_options_filtered_",filter_suffix,".csv"),header=TRUE) %>% 
	# # 					tbl_df %>% transmute(DataDate=as.Date(DataDate)) %>% distinct(DataDate) %>% arrange(DataDate) %>% slice(1) %>% unlist,origin = "1970-01-01"))
	# names(next_date) <- NULL
	# next_date_df<-cbind(next_date_df,data.frame(NextDate=next_date))

	# price_specs <- price_specs %>% left_join(.,next_date_df) 

	# price_specs <- asset_data %>% select(NextDate=Date,UnderlyingPrice_NextDate=Close) %>% left_join(price_specs,.)

	# price_specs<- price_specs %>% select(NextDate=DataDate,OptionSymbol,Last_NextDate=Last) %>% left_join(price_specs,.)

	# HN_delta<-mapply(HNGarchEOPT_Greeks,s0=price_specs$UnderlyingPrice,K=price_specs$Strike,time_to_maturity=price_specs$NetMaturity,callOrPut=price_specs$Type,lambda=price_specs$lambda, omega = price_specs$omega, alpha = price_specs$alpha,
	# 						beta = price_specs$beta, gamma = price_specs$gamma, rowCount=1:nrow(price_specs), r=price_specs$RiskFreeRate, div=price_specs$Dividend.Yield, MoreArgs=list(progressOutput=TRUE))

	save(price_delta,file=paste0(output_path,underlying_asset,"_",data_year,"_HN_data_withdiv_",ifelse(sym,"symm","asym"),"_",parameter_period,"y_with_delta.RData"))
	# write.table(data.frame(price_delta),paste0(output_path,underlying_asset,"_",data_year,"_HN_data_withdiv_",ifelse(sym,"symm","asym"),"_",parameter_period,"y_with_delta.csv"),row.names=FALSE,sep=",")	

}

# HN_Delta_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=2010,parameter_period=2,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	


for(i in 2008:2013){
	print(i)
	print("2 year ASYM")
	HN_Delta_Bulk(filter_suffix="C12",underlying_asset="NDX",data_year=i,parameter_period=2,sym=FALSE,progressOutput=TRUE)	
	print("5 year ASYM")
	HN_Delta_Bulk(filter_suffix="C12",underlying_asset="NDX",data_year=i,parameter_period=5,sym=FALSE,progressOutput=TRUE)	
	print("2 year SYM")
	HN_Delta_Bulk(filter_suffix="C12",underlying_asset="NDX",data_year=i,parameter_period=2,sym=TRUE,progressOutput=TRUE)	
	# HN_Delta_Bulk(filter_suffix="C12",underlying_asset="NDX",data_year=i,parameter_period=5,sym=TRUE,progressOutput=TRUE)	
}

for(i in 2008:2013){
	print(i)
	print("2 year ASYM")
	HN_Delta_Bulk(filter_suffix="C12",underlying_asset="SPX",data_year=i,parameter_period=2,sym=FALSE,progressOutput=TRUE)	
	print("5 year ASYM")
	HN_Delta_Bulk(filter_suffix="C12",underlying_asset="SPX",data_year=i,parameter_period=5,sym=FALSE,progressOutput=TRUE)	
	print("2 year SYM")
	HN_Delta_Bulk(filter_suffix="C12",underlying_asset="SPX",data_year=i,parameter_period=2,sym=TRUE,progressOutput=TRUE)	
	# HN_Delta_Bulk(filter_suffix="C12",underlying_asset="SPX",data_year=i,parameter_period=5,sym=TRUE,progressOutput=TRUE)	
}

# for(i in 2013:2010){
# 	print(i)
# 	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=2,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
# 	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=5,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
# 	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=2,sym=TRUE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
# 	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=5,sym=TRUE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
# }

those_files<- dir(paste0("~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/"),full.names=TRUE)

for(i in 1:length(those_files)){
	print(i)
	print(those_files[i])
	load(those_files[i])
	price_delta %>% 
		filter(!is.na(Last)) %>% 
		mutate(	overunder=ifelse(Last<HN_prices,"over","under"),
				cheapy=ifelse(Last>quantile(Last,0.25,na.rm=TRUE),ifelse(Last>quantile(Last,0.75,na.rm=TRUE),"expensive","moderate"),"cheap"),
				APE=abs(Last-HN_prices),
				ARPE=APE/Last) %>% 
		group_by(Type,overunder,cheapy) %>% 
		summarise(count=n(),avg_APE=mean(APE),median_APE=median(APE),max_APE=max(APE),avg_ARPE=mean(ARPE),median_ARPE=median(ARPE),max_ARPE=max(ARPE)) %>% 
		print()
}


i<-27
load(those_files[i])
price_delta %>% filter(abs(Last-HN_prices)/Last > 500)
 


for(i in 1:length(those_files)){

	print(i)
	print(those_files[i])
	load(those_files[i])

	# price_delta <- price_delta %>% rename(HN_prices=BS_prices)

	ppp <- 
	price_delta %>%
		ungroup %>%
		filter(!is.na(Last)) %>%
		group_by(DataDate,Maturity,Type) %>% 
		arrange(desc(Strike)) %>% 
		mutate(diff=round(HN_prices-lag(HN_prices,1,default=NA),2),diff=ifelse(is.na(diff),0,diff),diff2=round(HN_prices-lead(HN_prices,1,default=NA),2),diff2=ifelse(is.na(diff2),0,diff2)) %>%
		# arrange(desc(Strike)) %>% 
		# select(OptionSymbol,DataDate,Maturity,Type,Strike,Last,diff,diff2) %>% 

		mutate(mark=ifelse((Type=="call" & diff > 0 & diff2 > 0)|(Type=="put" & diff > 0 & diff2 >0),"Mark","")) %>%	
		ungroup %>% filter(mark=="Mark")

	if(nrow(ppp) == 0){
		print(paste0("Problem rows zero. Skipping."))
		save(price_delta,file=those_files[i])
		next
	}

	ppp2<- ppp %>% rowwise %>%
	# price_delta %>% filter(Type=="put", NetMaturity==19, DataDate =="2008-07-21") %>% slice(8) %>%
	 mutate(HN_price_new=HNGarchEOPT(
			s0=UnderlyingPrice,
			K=Strike+ifelse(Type=="call",0.1,-0.1),
			time_to_maturity=NetMaturity,
			r=RiskFreeRate,
			div=Dividend.Yield,
			callOrPut=Type,
			lambda=lambda,
			omega=omega,
			alpha=alpha,
			beta=beta,
			gamma=gamma,
			rowCount=0,progressOutput=TRUE),
	 HN_delta_new=HNGarchEOPT_Greeks(
			s0=UnderlyingPrice,
			K=Strike+ifelse(Type=="call",0.1,-0.1),
			time_to_maturity=NetMaturity,
			r=RiskFreeRate,
			div=Dividend.Yield,
			callOrPut=Type,
			lambda=lambda,
			omega=omega,
			alpha=alpha,
			beta=beta,
			gamma=gamma,
			rowCount=0,progressOutput=TRUE)) %>%
			ungroup %>%
			mutate(HN_price_new=pmax(HN_price_new,0)) %>%
			mutate(HN_delta_new=ifelse(HN_delta_new > 1,1,HN_delta_new),HN_delta_new=ifelse(HN_delta_new < -1,-1,HN_delta_new)) %>%
			select(OptionSymbol,DataDate,HN_price_new,HN_delta_new)

	print(paste0("Problem rows: ",nrow(ppp2)))

	price_delta <-price_delta %>% 
					ungroup %>%
					left_join(.,ppp2,by= c("OptionSymbol", "DataDate")) %>% 
					mutate(HN_prices=ifelse(is.na(HN_price_new),HN_prices,HN_price_new)) %>%
					mutate(HN_Delta=ifelse(is.na(HN_delta_new),HN_Delta,HN_delta_new)) %>%
					select(-HN_price_new,-HN_delta_new)

	save(price_delta,file=those_files[i])
}

#### Below is irrelevant


ppp %>% slice(23)

ppp



price_delta %>% filter(DataDate=="2010-02-09" & !is.na(Last) & NetMaturity==153 & Type=="call") %>% mutate(HN_prices=round(HN_prices,4)) %>% select(OptionSymbol,DataDate,UnderlyingPrice,Type,Strike,NetMaturity,RiskFreeRate,Last,HN_prices,Dividend.Yield,ExpirationPrice) %>% print(n=26)

ppp


price_delta %>% filter(abs(Last-HN_prices) > 75)
price_delta %>% filter(DataDate=="2013-11-07" & !is.na(Last) & NetMaturity==6 & Type=="call") %>% mutate(HN_prices=round(HN_prices,2)) %>% select(OptionSymbol,DataDate,UnderlyingPrice,Type,Strike,NetMaturity,RiskFreeRate,Last,HN_prices,Dividend.Yield,ExpirationPrice) %>% print(n=26)

price_delta %>% filter(DataDate=="2013-10-21" & NetMaturity==19 & Type=="put" & !is.na(Last)) %>% mutate(HN_prices=round(HN_prices,2)) %>% select(OptionSymbol,DataDate,UnderlyingPrice,Type,Strike,NetMaturity,RiskFreeRate,Last,HN_prices,Dividend.Yield,ExpirationPrice) %>% print(n=26)

SPX131116P01450000

price_delta %>% filter(DataDate=="2013-06-19" & Type=="put" & NetMaturity==21) 

price_delta %>% filter(DataDate=="2013-10-21" & Type=="put" & NetMaturity==19 ) 

ddd<- price_delta %>% filter(OptionSymbol=="SPX091219P00800000") %>% select(DataDate,Type,UnderlyingPrice,Strike,Maturity,Last,HN_prices,ExpirationPrice) %>% print(n=244)
ggplot(ddd,aes(x=DataDate))+ geom_line(aes(y=UnderlyingPrice/10),color="black")  + geom_line(aes(y=Last),color="red") + geom_line(aes(y=HN_prices),color="blue") 


  call   1825          18  0.20     61.63

HNGarchEOPT(s0=1692.39,K=1825,time_to_maturity=18,r=0.0002265689,div=0.0202,callOrPut="call",lambda=3.180698,omega=3.540293e-224,alpha=0.000007138843,beta=0.898154,gamma=-71.38064,rowCount,progressOutput=TRUE)
HNGOption(TypeFlag = "c", model=list(lambda=3.180698,omega=3.540293e-224,alpha=0.000007138843,beta=0.898154,gamma=-71.38064), S=1692.39, X=1825, Time.inDays=18, r.daily=0.0002265689/252, div.yield=0.0202/252)

TypeFlag = "c"
model=list(lambda=3.180698,omega=3.540293e-224,alpha=0.000007138843,beta=0.898154,gamma=-71.38064)
S=1692.39
X=1825
Time.inDays=18
r.daily=0.0002265689/252
div.yield=0.0202/252

quartz()
X_val<-1825.5
val<-0
for(i in 0:50){
val[i+1]<-.fstarHN(phi=i, const=0, model=list(lambda=3.180698,omega=3.540293e-224,alpha=0.000007138843,beta=0.898154,gamma=-71.38064), S=1692.39, X=X_val, Time.inDays=18, r.daily=0.0002265689/252, div.yield=0.0202/252)
}
plot(0:50,val)
title(X_val)

val<-0
for(X_val in 1750:1950){
print(X_val)
val[X_val-1749]<-HNGOption(TypeFlag = "c", model=list(lambda=3.180698,omega=3.540293e-224,alpha=0.000007138843,beta=0.898154,gamma=-71.38064), S=1692, X=1825, Time.inDays=18, r.daily=0.0002265689/252, div.yield=0.0202/252)
}
plot(1750:1950,val)


HNGOption(TypeFlag = "c", model=list(lambda=3.180698,omega=3.540293e-224,alpha=0.000007138843,beta=0.898154,gamma=-71.38064), S=1692.39, X=1825, Time.inDays=18, r.daily=0.0002265689/252, div.yield=0/252)

HNGOption(TypeFlag = "p", model=list(lambda=1.340732,omega=0,alpha=0.000005839549,beta=0.883917,gamma=-103.934), S=1494.82, X=1375, Time.inDays=35, r.daily=0.0006768879/252, div.yield=0.0213/252)


price_delta %>% filter(OptionSymbol=="SPX130316P01375000" & DataDate == "2013-01-24")

# input_path="~/Dropbox/PhD_Workshop/Input Files/"
# filter_suffix="A12"
# underlying_asset="NDX"
# data_year=2010
# parameter_period=2
# sym=TRUE
# output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/"
# progressOutput=TRUE




# prsptemp <-price_specs %>% slice(100:110) %>% mutate(HNGtype=ifelse(Type=="call","c","p"))

# mdply(prsptemp,.fun=HNGGreeks,Selection="Delta",TypeFlag="c",model=list(lambda =lambda, omega = omega, alpha = alpha, beta = beta, gamma = gamma),
						# S=UnderlyingPrice,X=Strike,Time.inDays=NetMaturity,r.daily=RiskFreeRate,div.yield=Dividend.Yield,.progress = "text")

# mapply(HNGarchEOPT,s0=option_specs$UnderlyingPrice,K=option_specs$Strike,time_to_maturity=option_specs$NetMaturity,callOrPut=option_specs$Type,lambda=param_data$lambda, omega = param_data$omega, alpha = param_data$alpha,
						# beta = param_data$beta, gamma = param_data$gamma, rowCount=1:nrow(option_specs), r=option_specs$RiskFreeRate, div=option_specs$Dividend.Yield, MoreArgs=list(progressOutput=TRUE))



# HNGGreeks(Selection = "Delta", TypeFlag = "p", model=list(lambda=-0.036112224,omega=1.96e-161,alpha=2.7851e-05,beta=0.913406538,gamma=-14.94133746), S=1977.83, X=1475, Time.inDays=34, r.daily=0.001605654/252, div.yield=0.007827887/252)
# HNGGreeks(Selection = "Delta", TypeFlag = "p", model=list(lambda=-0.036112224,omega=1.96e-161,alpha=2.7851e-05,beta=0.912809985,gamma=-12.88287853), S=1995.65, X=1500, Time.inDays=48, r.daily=0.00150414/252, div.yield=0.007627641/252)
# HNGarchEOPT_Greeks(s0=1977.83,K=1475,time_to_maturity=34,r=0.001605654,div=0.007827887,callOrPut="put",lambda=-0.036112224,omega=0,alpha=2.7851e-05,beta=0.913406538,gamma=0,rowCount=156,progressOutput=TRUE)



# for(i in 2013:2010){
# 	print(i)
# 	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=2,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
# 	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=5,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
# 	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=2,sym=TRUE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
# 	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=5,sym=TRUE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
# }

# if(activate){
# 	HNGarchEOPT(returns,s0=adjusted_closing_prices[1,"Adj.Close"],K=adjusted_closing_prices[1,"Adj.Close"],ttm=126,ydays=252,r=0,callTRUEputFALSE=FALSE,sym=TRUE)
# }




# mlply(prsptemp,HNGarchEOPT_Greeks(s0=UnderlyingPrice,K=Strike,time_to_maturity=NetMaturity,callOrPut=HNGtype,lambda=lambda, omega = omega, alpha = alpha,
# 						beta = beta, gamma = gamma, r=RiskFreeRate, div=Dividend.Yield),.progress = "text")

# mlply(.data=prsptemp,.fun=HNGGreeks(TypeFlag=HNGtype,model=list(lambda =lambda, omega = omega, alpha = alpha, beta = beta, gamma = gamma),S=UnderlyingPrice,X=Strike,Time.inDays=NetMaturity,r.daily=RiskFreeRate/252,div.yield=Dividend.Yield/252),Selection="Delta")

# blabla<-function(x,y){

# 	x + y$a + y$b
# }

# mlply(data.frame(meh=1:3,deh=(4:6)*10,geh=(7:9)*100),.fun=blabla(x=meh,y=list(a=deh,b=geh)))


# HNGGreeks<-function(Selection = "Delta", TypeFlag = c("c","p"), model, S, X, Time.inDays, r.daily, div.yield)


# HNGarchEOPT_Greeks(s0=1981.95,K=1800,time_to_maturity=33,r=0.0016/252,div=0.007/252,callOrPut="call",lambda=-0.3324211,omega=3.79e-19,alpha=0.0000285,beta=0.91885,gamma=0)
# HNGGreeks(s0=1981.95,K=1800,time_to_maturity=33,r=0.0016/252,div=0.007/252,callOrPut="call",lambda=-0.3324211,omega=3.79e-19,alpha=0.0000285,beta=0.91885,gamma=0)
# HNGGreeks(Selection = "Delta", TypeFlag = "c", model=list(lambda=-0.3324211,omega=3.79e-19,alpha=0.0000285,beta=0.91885,gamma=0), S=1981, X=1800, Time.inDays=33, r.daily=0.0001, div.yield=0.00006)
