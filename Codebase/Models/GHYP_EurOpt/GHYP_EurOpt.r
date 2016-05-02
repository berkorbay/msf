#Author: Berk Orbay 
#This code snippet is irrelevant to the model, just to clean the workspace
#rm(list=ls(all=TRUE))

#This is the R implementation of the European option pricing method with Generalized - Hyperbolic distribution with Either Esscher Transform 
#or Mean Correcting Martingale Measure
#
#This is the re-make of the Levy process models for bulk calculations

#Remove predetermined seed if exists
if(exists(".Random.seed"))
	rm(.Random.seed, envir=globalenv())


#Install the required packages
if(!("ghyp" %in% rownames(installed.packages())))
	install.packages("ghyp")
if(!("Runuran" %in% rownames(installed.packages())))
	install.packages("Runuran")
if(!("fBasics" %in% rownames(installed.packages())))
	install.packages("fBasics")
if(!("ggplot2" %in% rownames(installed.packages())))
	install.packages("ggplot2")


library(ghyp)
library(Runuran)
library(fBasics)
library(ggplot2)


fit.ghypuv.wrapper<-function(returns,start_index,end_index,sym,check_freq=1){
	if(runif(1)>max(1-check_freq,0)) print(end_index)
	errorResponse<-setNames(c(rep(-1,5),0),c("alpha","beta","delta","mu","lambda","error.code"))
	if(is.na(start_index)){
		errorResponse
	}else{
		tryCatch(fit.set<-fit.ghypuv(returns[start_index:end_index], symmetric=sym,silent=TRUE,control=list(maxit=1000)),error=function(e){warning(conditionMessage(e));return(errorResponse)})
		if(exists("fit.set")){
			c(unlist(coef(fit.set, type="alpha.delta")),error.code=fit.set@error.code)
		}else{
			errorResponse
		}
	}
}

calculate_GHYP_parameters<-function(dates,log_returns,years_back,sym){
	time_machine<-as.Date(paste0(as.numeric(substr(dates,1,4))-years_back,substr(dates,5,10)))
	feb29s<-which(is.na(time_machine))
	time_machine[feb29s]<-as.Date(paste0(as.numeric(substr(dates[feb29s],1,4))-years_back,substr(dates[feb29s]-1,5,10)))
	places<-match(time_machine,dates)
	while(time_machine[min(which(is.na(places)))]>=min(dates)){
		time_machine[which(is.na(places))]<-time_machine[which(is.na(places))]-1
		places<-match(time_machine,dates)
	}
	places<-places-1
	print(paste0("Starting GHYP parameter inference."))
#	t(mapply(fit.ghypuv.wrapper,start_index=1:length(places),end_index=places,MoreArgs=list(returns=log_returns,sym=sym)))
	t(mapply(fit.ghypuv.wrapper,start_index=places,end_index=1:length(places),MoreArgs=list(returns=log_returns,sym=sym)))
}

GHYP_Parameters<-function(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily",underlying_asset,output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",progressOutput=TRUE,sym=TRUE){
	if(progressOutput){
		print(paste0("Reading the file: ",underlying_asset))
	}	
	raw_data<-read.csv(paste0(data_path,underlying_asset,"_",data_type,"_processed.csv"),header=TRUE)
	raw_data<-raw_data[1:min(7500,nrow(raw_data)),]
	raw_data$Date<-as.Date(raw_data$Date)
	if(progressOutput){
		print("Getting 1 year parameters.")
	}	
	GHYP1y<-calculate_GHYP_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=1,sym=sym)
	colnames(GHYP1y)<-paste0(colnames(GHYP1y),1,"y")
	enhanced_data<-data.frame(raw_data,GHYP1y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP_w_error_",ifelse(sym,"symmetric","asymmetric"),".csv"),sep=",",append=FALSE,row.names=FALSE)


	if(progressOutput){
		print("Getting 2 years parameters.")
	}	
	GHYP2y<-calculate_GHYP_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=2,sym=sym)
	colnames(GHYP2y)<-paste0(colnames(GHYP2y),2,"y")
	enhanced_data<-data.frame(enhanced_data,GHYP2y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP_w_error_",ifelse(sym,"symmetric","asymmetric"),".csv"),sep=",",append=FALSE,row.names=FALSE)

	if(progressOutput){
		print("Getting 3 years parameters.")
	}	
	GHYP3y<-calculate_GHYP_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=3,sym=sym)
	colnames(GHYP3y)<-paste0(colnames(GHYP3y),3,"y")
	enhanced_data<-data.frame(enhanced_data,GHYP3y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP_w_error_",ifelse(sym,"symmetric","asymmetric"),".csv"),sep=",",append=FALSE,row.names=FALSE)

	if(progressOutput){
		print("Getting 5 years parameters.")
	}	
	GHYP5y<-calculate_GHYP_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=5,sym=sym)
	colnames(GHYP5y)<-paste0(colnames(GHYP5y),5,"y")
	enhanced_data<-data.frame(enhanced_data,GHYP5y)
	# if(progressOutput){
	# 	print("Getting 10 years parameters.")
	# }	
	# HN10y<-t(calculate_HN_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=10,sym=sym))
	# colnames(HN10y)<-paste0(colnames(HN10y),years_back,"y")
	# enhanced_data<-data.frame(enhanced_data,HN10y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP_w_error_",ifelse(sym,"symmetric","asymmetric"),".csv"),sep=",",append=FALSE,row.names=FALSE)

}

# GHYP_Parameters(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",progressOutput=TRUE,sym=TRUE)


# updateStatus("@berkorbay GHYP parameters are done with symmetric")

# GHYP_Parameters(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",progressOutput=TRUE,sym=FALSE)
# library(twitteR)
# #options("httr_oauth_cache" = FALSE)
# setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
# updateStatus("@berkorbay Starting coding")

#This function takes account of the parameter with error codes greater than 0 and replaces them with the most recent parameter set from the past with no error code
correct_GHYP_errors<-function(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily",underlying_asset,output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",progressOutput=TRUE,sym=TRUE){
	if(progressOutput){
	print("Reading the file.")
	}	
	raw_data<-read.csv(paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP_w_error_",ifelse(sym,"symmetric","asymmetric"),".csv"),header=TRUE)
	raw_data$Date<-as.Date(raw_data$Date)
	#See how many years_back are calculated
	calculated_years_back<-as.numeric(substr(colnames(raw_data)[grep("error.code",colnames(raw_data))],11,11))
	#Initiate the new data frame
	fixed_data<-raw_data[,c("Date","Open","High","Low","Close","Volume","Adj.Close","log_returns","Dividend.Yield")]

	for(i in calculated_years_back){
		er_code<-paste0("error.code",i,"y")
		#Get the indices of parameter sets without error
		pure_index<-which(raw_data[,er_code]==0)
		#This function is to find the most recent index values of the parameter sets with error code
		get_pure<-function(count){
			if(raw_data[count,er_code]>0){
				return(min(pure_index[pure_index>count]))
			}else{
				return(count)
			}
		}
		#Take the 
		assign(paste0("fixed",i,"y"),mapply(get_pure,count=1:nrow(raw_data)))
		fixed_data<-cbind(fixed_data,raw_data[get(paste0("fixed",i,"y")),paste0(c("alpha","delta","beta","mu","lambda"),i,"y")])
	}
	write.table(fixed_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP_",ifelse(sym,"symmetric","asymmetric"),".csv"),sep=",",append=FALSE,row.names=FALSE)
}


correct_GHYP_errors(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily",underlying_asset="SPX",output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",progressOutput=TRUE,sym=FALSE)



GHYP_Moment_Generating_Function<-function(x,alpha,beta,delta,mu,lambda){
	exp(x*mu)*((alpha^2-beta^2)/(alpha^2-(beta+x)^2))^(lambda/2)*besselK(delta*sqrt(alpha^2-(beta+x)^2),lambda)/besselK(delta*sqrt(alpha^2-beta^2),lambda)
}

get_theta<-function(params.alpha,params.beta,params.delta,params.mu,params.lambda,r,divid){
	f<-function(x){
		r - divid - log(GHYP_Moment_Generating_Function(x+1,alpha=params.alpha,beta=params.beta,delta=params.delta,mu=params.mu,lambda=params.lambda)/
		GHYP_Moment_Generating_Function(x,alpha=params.alpha,beta=params.beta,delta=params.delta,mu=params.mu,lambda=params.lambda))
	}
	tryCatch(uniroot(f,c(-(params.alpha+params.beta)+0.00001,(params.alpha-params.beta-1)-0.00001))$root,error=function(e){warning(conditionMessage(e));NA})	
}


#Here we simulate the risk neutral process 
GHYP_EOP_Simulation<-function(s0,K,ttm,r,option_type,param.alpha,param.beta,param.delta,param.mu,param.lambda,report_index,progressOutput_freq,n){
	if(runif(1)<progressOutput_freq && report_index > 0) print(paste0("Running iteration ",report_index))
	#If something is wrong, abort
	if(is.na(sum(c(param.alpha,param.beta,param.delta,param.mu,param.lambda)))){
			result<-rep(-1,4)
			names(result)<-c("Price","SError","Lower","Upper")
			return(result)
	}	

	#Find the inverse distribution
	#Limited lower and upper bound to [-10,7] because actually the movement is between percentage returns of 0.000045 and 1096.633
	inverse_distribution<-pinv.new(pdf=dgh,alpha=param.alpha,beta=param.beta,delta=param.delta,mu=param.mu,lambda=param.lambda,lb=-10,ub=7)	
	#Initialize the price chain 
	St_GH<-s0*exp(rowSums(matrix(uq(inverse_distribution,runif(n*ttm)),nrow=n)))

	if(option_type=="call"){
		price<-exp(-r*ttm/252)*pmax(St_GH-K,0)		
	}else{
		price<-exp(-r*ttm/252)*pmax(K-St_GH,0)		
	}

	result<-c(mean(price),1.96*sd(price)/sqrt(n),mean(price)-1.96*sd(price)/sqrt(n),mean(price)+1.96*sd(price)/sqrt(n))
	names(result)<-c("Price","SError","Lower","Upper")
	
	return(result)
		
}


GHYP_Bulk<-function(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset,data_year,n=5000,martingale="Esscher",parameter_period=1,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="onebyone",symmetric=FALSE){

	if(all(martingale != c("Esscher","MCMM"))){
		print("Wrong martingale choice. It should either be Esscher or MCMM")
		return
	}

	if(progressOutput){
		print("Getting the data.")
	}	

	#Get the input data
	option_specs<-read.csv(paste0(input_path,"Asset Options/",underlying_asset,"_",data_year,"_options_filtered_",filter_suffix,".csv"),header=TRUE)
######### Sampling code start
	# set.seed(21101928)
	# sampling_ids<-sample(1:nrow(option_specs),2000)
	# print(sampling_ids[1:10])
	# option_specs_original<-option_specs
	# option_specs<-option_specs[sampling_ids,]
######### Sampling code end

	asset_data<-read.csv(paste0(input_path,"Asset Prices/",underlying_asset,"_daily_processed_GHYP_",ifelse(symmetric,"symmetric","asymmetric"),".csv"),header=TRUE)
	#Extract the required parameter space from the asset data
	parameter_space<-asset_data[match(option_specs$DataDate,asset_data$Date),paste0(c("lambda","alpha","delta","beta","mu"),parameter_period,"y")]
	#as.Date
	option_specs$DataDate<-as.Date(option_specs$DataDate)
	option_specs$Expiration<-as.Date(option_specs$Expiration)
	option_specs$RealExpiration<-as.Date(option_specs$RealExpiration)
	asset_data$Date<-as.Date(asset_data$Date)
	#Delta should not be zero because then MCMM gives error
	parameter_space[parameter_space[,paste0("delta",parameter_period,"y")]==0,paste0("delta",parameter_period,"y")]<-1.943135e-13
	#Bulk calculate Esscher parameter
	if(martingale=="Esscher"){
		if(progressOutput){
			print("Calculating Esscher Theta.")		
		}	
		assign(paste0("Esscher_theta",parameter_period,"y"),mapply(get_theta,params.alpha=parameter_space$alpha,params.beta=parameter_space$beta,params.delta=parameter_space$delta,params.mu=parameter_space$mu,params.lambda=parameter_space$lambda,r=option_specs$RiskFreeRate/252,divid=option_specs$Dividend.Yield/252))
	}
	#Delta with zero value causes problems in GHYP Moment GF so it is set to a very small number (min of deltas greater than zero)
	if(martingale=="MCMM"){
		if(progressOutput){
			print("Calculating MCMM_mu.")
		}	

		#The MCMM transformation is mu - r - log(MGF(1)) is done by first calculating MGF(1) for each row and then by completing the operation
		assign(paste0("MCMM_mu_",parameter_period,"y"),mapply(GHYP_Moment_Generating_Function,alpha=parameter_space$alpha,beta=parameter_space$beta,delta=parameter_space$delta,mu=parameter_space$mu,lambda=parameter_space$lambda,MoreArgs=list(x=1)))
		assign(paste0("MCMM_mu_",parameter_period,"y"),parameter_space[,paste0("mu",parameter_period,"y")] + option_specs$RiskFreeRate/252 - option_specs$Dividend.Yield/252 - log(get(paste0("MCMM_mu_",parameter_period,"y"))))
		MCMM_mu_P<-get(paste0("MCMM_mu_",parameter_period,"y"))
	}	

	if(progressOutput){
	print("Calculating prices in bulk.")
	}	
	if(methodology=="onebyone"){
		GHYP_prices<-mapply(GHYP_EOP_Simulation,
							s0=option_specs$UnderlyingPrice,
							K=option_specs$Strike,
							ttm=option_specs$NetMaturity,
							r=option_specs$RiskFreeRate/252,
							option_type=as.character(option_specs$Type),											
							param.alpha=parameter_space[,paste0(c("alpha"),parameter_period,"y")],
							param.beta=parameter_space[,paste0(c("beta"),parameter_period,"y")] + if(martingale=="Esscher"){get(paste0("Esscher_theta",parameter_period,"y"))}else{ 0 },
							param.delta=parameter_space[,paste0(c("delta"),parameter_period,"y")],
							param.mu=if(martingale=="MCMM"){get(paste0("MCMM_mu_",parameter_period,"y"))}else{parameter_space[,paste0(c("mu"),parameter_period,"y")]},
							param.lambda=parameter_space[,paste0(c("lambda"),parameter_period,"y")],
							report_index=1:nrow(option_specs),
							MoreArgs=list(n=n,progressOutput_freq=0.01))

		return(t(GHYP_prices))
	}else{
		##This is the alternative way
	
		if(progressOutput){
			print("Getting random matrix.")
		}		
		randgen<-matrix(runif(max(option_specs$NetMaturity)*n),nrow=n)
	
	#	colnames(randgen)<-unique(convert_to_trading_day(as.Date(min(option_specs$DataDate):max(option_specs$RealExpiration),origin="1970-01-01")))
		result<-data.frame(Price=numeric(),SError=numeric(),Lower=numeric(),Upper=numeric())
	
		for(i in unique(option_specs$DataDate)){
			print(as.Date(i,origin="1970-01-01"))
			i_index<-which(option_specs$DataDate%in%i)
			#This piece of code is to ensure dividend yield and risk-free rate to be determined same as the longest maturity option
			i_index<-i_index[order(option_specs$NetMaturity[i_index],decreasing = TRUE)]
			s0<-option_specs$UnderlyingPrice[i_index][1]
			if(any(is.na(parameter_space[i_index[1],]))){
				result[i_index,]<- -1
				next
			}
			if(martingale == "Esscher"){
				if(is.na(get(paste0("Esscher_theta",parameter_period,"y"))[i_index[1]])){
					result[i_index,]<- -1
					next
				}
				inverse_distribution<-pinv.new(pdf=dgh,alpha=parameter_space[i_index[1],paste0(c("alpha"),parameter_period,"y")],beta=parameter_space[i_index[1],paste0(c("beta"),parameter_period,"y")] + get(paste0("Esscher_theta",parameter_period,"y"))[i_index[1]],
												delta=parameter_space[i_index[1],paste0(c("delta"),parameter_period,"y")],mu=parameter_space[i_index[1],paste0(c("mu"),parameter_period,"y")],
												lambda=parameter_space[i_index[1],paste0(c("lambda"),parameter_period,"y")],lb=-10,ub=7)
			}else if(martingale == "MCMM"){
				if(is.na(get(paste0("MCMM_mu_",parameter_period,"y"))[i_index[1]])){
					result[i_index,]<- -1
					next
				}
				inverse_distribution<-pinv.new(pdf=dgh,alpha=parameter_space[i_index[1],paste0(c("alpha"),parameter_period,"y")],beta=parameter_space[i_index[1],paste0(c("beta"),parameter_period,"y")],
												delta=parameter_space[i_index[1],paste0(c("delta"),parameter_period,"y")],mu=get(paste0("MCMM_mu_",parameter_period,"y"))[i_index[1]],
												lambda=parameter_space[i_index[1],paste0(c("lambda"),parameter_period,"y")],lb=-10,ub=7)
			}else{ stop("Wrong Martingale") }
			r<-option_specs$RiskFreeRate[i_index[1]]/252
	
	
			# for(j in unique(option_specs$NetMaturity[i_index])){
			# 	j_index<-which(option_specs$NetMaturity%in%j)
			# 	j_index<-j_index[na.omit(unique(match(i_index,j_index)))]
			# 	St_GH<-St_GH*exp(rowSums(matrix(uq(inverse_distribution,randgen[,1:j]),nrow=nrow(randgen))))
	
			# }
	
			for(j in i_index){
				St_GH<-s0*exp(rowSums(matrix(uq(inverse_distribution,randgen[,1:option_specs$NetMaturity[j]]),nrow=nrow(randgen))))
	
				if(option_specs$Type[j]=="call"){
					price<-exp(-r*option_specs$NetMaturity[j])*pmax(St_GH-option_specs$Strike[j],0)		
				}else{
					price<-exp(-r*option_specs$NetMaturity[j])*pmax(option_specs$Strike[j]-St_GH,0)		
				}
	
				result[j,]<-c(mean(price),1.96*sd(price)/sqrt(n),mean(price)-1.96*sd(price)/sqrt(n),mean(price)+1.96*sd(price)/sqrt(n))
				
									
	
			}
		}


		if(martingale=="Esscher"){
			colnames(result)<-c("LE_prices","LE_SError","LE_lower","Le_upper")
			price_output<-data.frame(option_specs[,c(1,2,4,5,7,8,9,14,20,23,24,25)],result,parameter_space,Esscher_theta=get(paste0("Esscher_theta",parameter_period,"y")))
		}else if(martingale=="MCMM"){
			colnames(result)<-c("LM_prices","LM_SError","LM_lower","LM_upper")
			price_output<-data.frame(option_specs[,c(1,2,4,5,7,8,9,14,20,23,24,25)],result,parameter_space,MCMM_mu=MCMM_mu_P)
		}
		write.table(price_output,paste0(output_path,underlying_asset,"_",data_year,"_Levy_GHYP_data_withdiv_",martingale,"_",n,"_iteration_",parameter_period,"y_",ifelse(symmetric,"symmetric","asymmetric"),".csv"),row.names=FALSE,sep=",")
		return("Success!")
		# return(result)
	}
}


#####Runs start here

#####2013 symmetric

	for(j in c(TRUE,FALSE)){
		for(i in 2013:2009){
			for(k in c("Esscher","MCMM")){
		#		if(j && i == 2013 && k =="Esscher")
		#			next
				print(paste0("Calculating ",ifelse(j,"symmetric","asymmetric")," ",k," from ",i))
				GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=i,n=50000,
						martingale=k,parameter_period=5,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=j)
			}
		}
	}


	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2013,n=50000,
				martingale="MCMM",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=TRUE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))

###### 2012

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=50000,
				martingale="Esscher",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=TRUE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=50000,
				martingale="MCMM",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=TRUE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))

###### 2011

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2011,n=50000,
				martingale="Esscher",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=TRUE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2011,n=50000,
				martingale="MCMM",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=TRUE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))

###### 2010

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2010,n=50000,
				martingale="Esscher",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=TRUE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2010,n=50000,
				martingale="MCMM",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=TRUE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


###### 2009

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2009,n=50000,
				martingale="Esscher",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=TRUE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2009,n=50000,
				martingale="MCMM",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=TRUE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


#####2013 non-symmetric

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2013,n=50000,
				martingale="Esscher",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=FALSE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2013,n=50000,
				martingale="MCMM",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=FALSE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))

###### 2012

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=50000,
				martingale="Esscher",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=FALSE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=50000,
				martingale="MCMM",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=FALSE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))

###### 2011

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2011,n=50000,
				martingale="Esscher",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=FALSE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2011,n=50000,
				martingale="MCMM",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=FALSE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))

###### 2010

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2010,n=50000,
				martingale="Esscher",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=FALSE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2010,n=50000,
				martingale="MCMM",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=FALSE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


###### 2009

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2009,n=50000,
				martingale="Esscher",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=FALSE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))


	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts at ",format(Sys.time(), "%H:%M")))

	GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2009,n=50000,
				martingale="MCMM",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative",symmetric=FALSE)

	options(httr_oauth_cache=FALSE)
	setup_twitter_oauth("eLcRN1XLcIR3gP4oGDEGV4MHO","6VV2O9cqFCNq9pxWB8fA23ClOHktZnpAjLThp4MdWf4WjjPaVS","2338643988-zzpqBVthBo64jKSKbsNidxOellltZ154cGqMxoh","wXICF8JLnAMztR24lRiAvT7c0l6L82BPO5SDfqA9hsfV4")
	updateStatus(paste0("@berkorbay GHYP Simulation 2y Esscher starts finished at ",format(Sys.time(), "%H:%M")))




# 2000 contracts n = 5000

t1<-system.time(my_prices<-GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=5000,
						martingale="Esscher",parameter_period=1,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="onebyone"))

#    user  system elapsed 
# 487.275   6.785 491.232 


t2<-system.time(my_prices_2<-


	)




 #   user  system elapsed 
 # 93.885   6.172  99.438 

t3<-system.time(my_prices_MCMM<-GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=5000,
						martingale="MCMM",parameter_period=1,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="onebyone"))

#    user  system elapsed 
# 476.940   6.689 480.769 


t4<-system.time(my_prices_MCMM_2<-GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=5000,
						martingale="MCMM",parameter_period=1,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="iterative"))

 #   user  system elapsed 
 # 92.911   6.218  98.560 
