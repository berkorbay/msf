#Author: Berk Orbay 

#This code snippet is irrelevant to the model, just to clean the workspace
#rm(list=ls(all=TRUE))

#This is the R implementation of the Heston Nandi GARCH option pricing method


#Install the required package fOptions
if(!("fOptions" %in% rownames(installed.packages())))
	install.packages("fOptions", repos = "http://cran.pau.edu.tr/")

library(fOptions)

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
    #The changed function (2/2)
    call2 = integrate(.fstarHN, 0, Inf, const = 0, model = model, 
        S = S, X = X, Time.inDays = Time.inDays, r.daily = r.daily, div.yield=div.yield, stop.on.error=FALSE)
    if (is.null(call2$value)) 
        call2$value = call2$integral
    call.price = S/2 + exp(-(r.daily - div.yield) * Time.inDays) * call1$value - 
        X * exp(-r.daily * Time.inDays) * (1/2 + call2$value)
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

for(i in 2013:2010){
	print(i)
	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=2,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=5,sym=FALSE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=2,sym=TRUE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
	HNBulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="NDX",data_year=i,parameter_period=5,sym=TRUE,output_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",progressOutput=TRUE)	
}

# if(activate){
# 	HNGarchEOPT(returns,s0=adjusted_closing_prices[1,"Adj.Close"],K=adjusted_closing_prices[1,"Adj.Close"],ttm=126,ydays=252,r=0,callTRUEputFALSE=FALSE,sym=TRUE)
# }


