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


fit.ghypuv.wrapper<-function(returns,start_index,end_index,sym,check_freq=0){
	if(runif(1)>max(1-check_freq,0)) print(end_index)
	errorResponse<-setNames(c(rep(-1,5),0),c("alpha","beta","delta","mu","lambda","error.code"))
	if(is.na(start_index)){
		errorResponse
	}else{
		fit.set<-fit.ghypuv(returns[start_index:end_index], symmetric=sym,silent=TRUE,control=list(maxit=1000))
		c(unlist(coef(fit.set, type="alpha.delta")),error.code=fit.set@error.code)
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
	print(paste0("Starting ",underlying_asset," GHYP parameter inference."))
#	t(mapply(fit.ghypuv.wrapper,start_index=1:length(places),end_index=places,MoreArgs=list(returns=log_returns,sym=sym)))
	t(mapply(fit.ghypuv.wrapper,start_index=places,end_index=1:length(places),MoreArgs=list(returns=log_returns,sym=sym,check_freq=1)))
}

GHYP_Parameters<-function(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily",underlying_asset,output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",progressOutput=TRUE,sym=TRUE){
	if(progressOutput){
		print("Reading the file.")
	}	
	raw_data<-read.csv(paste0(data_path,underlying_asset,"_",data_type,"_processed.csv"),header=TRUE)
	raw_data$Date<-as.Date(raw_data$Date)
	if(progressOutput){
		print("Getting 1 year parameters.")
	}	
	GHYP1y<-calculate_GHYP_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=1,sym=sym)
	colnames(GHYP1y)<-paste0(colnames(GHYP1y),1,"y")
	enhanced_data<-data.frame(raw_data,GHYP1y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP_w_error.csv"),sep=",",append=FALSE,row.names=FALSE)


	if(progressOutput){
		print("Getting 2 years parameters.")
	}	
	GHYP2y<-calculate_GHYP_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=2,sym=sym)
	colnames(GHYP2y)<-paste0(colnames(GHYP2y),2,"y")
	enhanced_data<-data.frame(enhanced_data,GHYP2y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP_w_error.csv"),sep=",",append=FALSE,row.names=FALSE)

	if(progressOutput){
		print("Getting 3 years parameters.")
	}	
	GHYP3y<-t(calculate_GHYP_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=3))
	colnames(GHYP3y)<-paste0(colnames(GHYP3y),3,"y")
	enhanced_data<-data.frame(enhanced_data,GHYP3y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP_w_error.csv"),sep=",",append=FALSE,row.names=FALSE)

	if(progressOutput){
		print("Getting 5 years parameters.")
	}	
	GHYP5y<-t(calculate_GHYP_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=5))
	colnames(GHYP5y)<-paste0(colnames(GHYP5y),5,"y")
	enhanced_data<-data.frame(enhanced_data,GHYP5y)
	# if(progressOutput){
	# 	print("Getting 10 years parameters.")
	# }	
	# HN10y<-t(calculate_HN_parameters(dates=raw_data$Date,log_returns=raw_data$log_returns,years_back=10,sym=sym))
	# colnames(HN10y)<-paste0(colnames(HN10y),years_back,"y")
	# enhanced_data<-data.frame(enhanced_data,HN10y)
	write.table(enhanced_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP_w_error.csv"),sep=",",append=FALSE,row.names=FALSE)

}

#This function takes account of the parameter with error codes greater than 0 and replaces them with the most recent parameter set from the past with no error code
correct_GHYP_errors<-function(data_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",data_type="daily",underlying_asset,output_path="~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",progressOutput=TRUE,sym=TRUE){
	if(progressOutput){
	print("Reading the file.")
	}	
	raw_data<-read.csv(paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP_w_errors.csv"),header=TRUE)
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
	write.table(fixed_data,paste0(data_path,underlying_asset,"_",data_type,"_processed_GHYP.csv"),sep=",",append=FALSE,row.names=FALSE)
}


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


GHYP_Bulk<-function(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset,data_year,n=5000,martingale="Esscher",parameter_period=1,output_path="~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/",progressOutput=TRUE,methodology="onebyone"){

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

	asset_data<-read.csv(paste0(input_path,"Asset Prices/",underlying_asset,"_daily_processed_GHYP.csv"),header=TRUE)
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
	if(progressOutput && martingale=="Esscher"){
		print("Calculating Esscher Theta.")		
		assign(paste0("Esscher_theta",parameter_period,"y"),mapply(get_theta,params.alpha=parameter_space$alpha,params.beta=parameter_space$beta,params.delta=parameter_space$delta,params.mu=parameter_space$mu,params.lambda=parameter_space$lambda,r=option_specs$RiskFreeRate/252,divid=option_specs$Dividend.Yield/252))
	}
	#Delta with zero value causes problems in GHYP Moment GF so it is set to a very small number (min of deltas greater than zero)
	if(progressOutput && martingale=="MCMM"){
		print("Calculating MCMM_mu.")
		#The MCMM transformation is mu - r - log(MGF(1)) is done by first calculating MGF(1) for each row and then by completing the operation
		assign(paste0("MCMM_mu",parameter_period,"y"),mapply(GHYP_Moment_Generating_Function,alpha=parameter_space$alpha,beta=parameter_space$beta,delta=parameter_space$delta,mu=parameter_space$mu,lambda=parameter_space$lambda,MoreArgs=list(x=1)))
		assign(paste0("MCMM_mu",parameter_period,"y"),parameter_space[,paste0("mu",parameter_period,"y")] + option_specs$RiskFreeRate/252 - option_specs$Dividend.Yield/252 - log(get(paste0("MCMM_mu",parameter_period,"y"))))
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
							param.mu=if(martingale=="MCMM"){get(paste0("MCMM_mu",parameter_period,"y"))}else{parameter_space[,paste0(c("mu"),parameter_period,"y")]},
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
				if(is.na(get(paste0("MCMM_mu",parameter_period,"y"))[i_index[1]])){
					result[i_index,]<- -1
					next
				}
				inverse_distribution<-pinv.new(pdf=dgh,alpha=parameter_space[i_index[1],paste0(c("alpha"),parameter_period,"y")],beta=parameter_space[i_index[1],paste0(c("beta"),parameter_period,"y")],
												delta=parameter_space[i_index[1],paste0(c("delta"),parameter_period,"y")],mu=get(paste0("MCMM_mu",parameter_period,"y"))[i_index[1]],
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
	
		return(result)
	}
}

# 2000 contracts n = 5000

t1<-system.time(my_prices<-GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=5000,
						martingale="Esscher",parameter_period=1,output_path="~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/",progressOutput=TRUE,methodology="onebyone"))

#    user  system elapsed 
# 487.275   6.785 491.232 


t2<-system.time(my_prices_2<-GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=5000,
						martingale="Esscher",parameter_period=1,output_path="~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/",progressOutput=TRUE,methodology="iterative"))

 #   user  system elapsed 
 # 93.885   6.172  99.438 

t3<-system.time(my_prices_MCMM<-GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=5000,
						martingale="MCMM",parameter_period=1,output_path="~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/",progressOutput=TRUE,methodology="onebyone"))

#    user  system elapsed 
# 476.940   6.689 480.769 


t4<-system.time(my_prices_MCMM_2<-GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=5000,
						martingale="MCMM",parameter_period=1,output_path="~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/",progressOutput=TRUE,methodology="iterative"))

 #   user  system elapsed 
 # 92.911   6.218  98.560 


####
# Excess Code
####

option_specs<-read.csv(paste0(input_path,"Asset Options/",underlying_asset,"_",data_year,"_options_filtered_",filter_suffix,".csv"),header=TRUE)
######### Sampling code start
set.seed(21101928)
sampling_ids<-sample(1:nrow(option_specs),2000)

option_specs<-option_specs[sampling_ids,]

asset_data<-read.csv(paste0(input_path,"Asset Prices/",underlying_asset,"_daily_processed_GHYP.csv"),header=TRUE)
#Extract the required parameter space from the asset data
parameter_space<-asset_data[match(option_specs$DataDate,asset_data$Date),paste0(c("lambda","alpha","delta","beta","mu"),parameter_period,"y")]
#as.Date
option_specs$DataDate<-as.Date(option_specs$DataDate)
option_specs$Expiration<-as.Date(option_specs$Expiration)
option_specs$RealExpiration<-as.Date(option_specs$RealExpiration)
asset_data$Date<-as.Date(asset_data$Date)


my_all_prices<-cbind(my_prices,my_prices_2,my_prices_MCMM,my_prices_MCMM_2)
colnames(my_all_prices)<-c(paste0("E1_",c("Price","SError","Lower","Upper")),paste0("E2_",c("Price","SError","Lower","Upper")),paste0("M1_",c("Price","SError","Lower","Upper")),paste0("M2_",c("Price","SError","Lower","Upper")))
big_output<-cbind(sampling_ids,option_specs[,c("UnderlyingPrice","OptionSymbol","Type","Expiration","DataDate","Strike","IV","NetMaturity","Last","ExpirationPrices","ExpirationPayoff","RV","RiskFreeRate","Dividend.Yield")],parameter_space,my_all_prices)

write.table(big_output,"~/Dropbox/PhD_Workshop/Output Files/GHYP_Europt/comparisontest.csv",row.names=FALSE,sep=",",col.names=TRUE)




#########

tr1<-system.time(real_test_Esscher<-GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=0.4*10^6,
						martingale="Esscher",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/",progressOutput=TRUE,methodology="iterative"))

price_output<-data.frame(option_specs[, c("UnderlyingSymbol","UnderlyingPrice","OptionSymbol","Type","DataDate","Strike","Last","IV","Moneyness","NetMaturity","ExpirationPrices","ExpirationPayoff")],
GHYP.Esscher.2y=real_test_Esscher[,"Price"],real_test_Esscher[,-1],Esscher.theta.2y=get(paste0("Esscher_theta",parameter_period,"y")),parameter_space)

write.table(price_output,paste0(output_path,underlying_asset,"_",data_year,"_GHYP_test",martingale,".csv"),row.names=FALSE,sep=",")



tr2<-system.time(real_test_MCMM<-GHYP_Bulk(input_path="~/Dropbox/PhD_Workshop/Input Files/",filter_suffix="A12",underlying_asset="SPX",data_year=2012,n=0.4*10^6,
						martingale="MCMM",parameter_period=2,output_path="~/Dropbox/PhD_Workshop/Output Files/Black_Scholes/",progressOutput=TRUE,methodology="iterative"))

price_output<-data.frame(option_specs[, c("UnderlyingSymbol","UnderlyingPrice","OptionSymbol","Type","DataDate","Strike","Last","IV","Moneyness","NetMaturity","ExpirationPrices","ExpirationPayoff")],
GHYP.MCMM.2y=real_test_MCMM[,"Price"],real_test_MCMM[,-1],MCMM.mu.2y=get(paste0("MCMM_mu",parameter_period,"y")),parameter_space)

write.table(price_output,paste0(output_path,underlying_asset,"_",data_year,"_GHYP_test",martingale,".csv"),row.names=FALSE,sep=",")



#############
# Plotting functions
#############

asset_data<-read.csv("~/Dropbox/PhD_Workshop/Input Files/Asset Prices/SPX_daily_processed_GHYP_w_errors.csv",header=T)
log_returns<-asset_data$log_returns
error.codes<-asset_data$error.code1y
sym<-TRUE

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



calculate_places<-function(dates,log_returns,years_back,sym){
	time_machine<-as.Date(paste0(as.numeric(substr(dates,1,4))-years_back,substr(dates,5,10)))
	feb29s<-which(is.na(time_machine))
	time_machine[feb29s]<-as.Date(paste0(as.numeric(substr(dates[feb29s],1,4))-years_back,substr(dates[feb29s]-1,5,10)))
	places<-match(time_machine,dates)
	while(time_machine[min(which(is.na(places)))]>=min(dates)){
		time_machine[which(is.na(places))]<-time_machine[which(is.na(places))]-1
		places<-match(time_machine,dates)
	}
	places<-places-1
	return(places)
}
places<-calculate_places(as.Date(asset_data[,"Date"]),log_returns,1,TRUE)


# Error code 1
# [1]  1276  1280  1281  6423  9359  9396  9666 14125 14130 14132 14133 14141 14142 14147 14148
# [16] 14164 14166 14169 14172 14174 14182 14184 14185 14190 14195 14203 14211 14214

# Error code 10
# [1] 2078 9184 9292 9294 9301 9304 9348 9624 9685

# Error code 100, rest is on the bottom of this section
#   [1]  2073  2074  2077  2079  2080  2081  2082  2083  2085  2091  2092  2093  2094  2095  2096
#  [16]  2097  2098  2099  2109  2110  2113  2114  2115  2116  2118  2119  2120  2121  2137  2138
#  [31]  2146  2151  2152  2155  2156  2157  2160  2162  2163  2166  2167  2179  2180  2181  2182

#2074 
# Error in optim(theta, negloglik, hessian = se, pdf = pdf, tmp.data = data,  : 
#   non-finite finite-difference value [1]

#9292 no error but code 10

#14164 singularity error code 1

# 800 eiffel problem

# 14164 

plot_pdf_cdf<-function(myind,places,sym=TRUE){
	myret<-sort(log_returns[places[myind]:myind])
	#plot(sort(myret),dgh(myret[order(myret)],hhh$estimate[1],hhh$estimate[2],hhh$estimate[3],hhh$estimate[4],hhh$estimate[5]),type="l")

	hhh2<-coef(fit.ghypuv(log_returns[places[myind]:myind], symmetric=sym, silent=TRUE, control=list(maxit=2000)), type="alpha.delta")
	#plot(sort(myret),dgh(myret[order(myret)],hhh2$alpha,hhh2$beta,hhh2$delta,hhh2$mu,hhh2$lambda),type="l")

	hhh3<-density(myret)

	my_x<-unique(myret)

	benchy<-data.frame(returns=my_x,#home=dgh(myret,hhh$estimate[1],hhh$estimate[2],hhh$estimate[3],hhh$estimate[4],hhh$estimate[5]),
			ghyp=dgh(my_x,hhh2$alpha,hhh2$beta,hhh2$delta,hhh2$mu,hhh2$lambda),
			empirical=approx(hhh3$x,hhh3$y,my_x)$y, #, ghyp.def=dgh(myret,hhh4$alpha+0.00001,hhh4$beta,hhh4$delta,hhh4$mu,hhh4$lambda)
			normal=dnorm(my_x,mean(myret),sd(myret))
			)

	pl1<-ggplot(data=benchy,aes(x=my_x)) + geom_line(aes(y=ghyp, color="GHYP")) + geom_line(aes(y=empirical, color="Empirical")) + geom_line(aes(y=normal, color="Normal")) + theme(legend.position="none")# + labs(color = paste0("pdf: ",myind)) #+ geom_line(aes(y=ghyp.def, color="GHYP.def"))


	##This is for CDF

	hhh4<-ecdf(myret)


	benchy<-data.frame(returns=my_x,#home=dgh(myret,hhh$estimate[1],hhh$estimate[2],hhh$estimate[3],hhh$estimate[4],hhh$estimate[5]),
			ghyp=pgh(my_x,hhh2$alpha,hhh2$beta,hhh2$delta,hhh2$mu,hhh2$lambda),
			empirical=hhh4(my_x), #, ghyp.def=dgh(myret,hhh4$alpha+0.00001,hhh4$beta,hhh4$delta,hhh4$mu,hhh4$lambda)
			normal=pnorm(my_x,mean(myret),sd(myret))
			)

	pl2<-ggplot(data=benchy,aes(x=my_x)) + geom_line(aes(y=ghyp, color="GHYP")) + geom_line(aes(y=empirical, color="Empirical")) + geom_line(aes(y=normal, color="Normal")) + labs(color = myind) #+ geom_line(aes(y=ghyp.def, color="GHYP.def"))

	multiplot(pl1,pl2,cols=2)
}

plot_pdf_cdf(9292,places)

myind<-9292

# [46]  2183  2184  2185  2186  2188  2190  2191  2192  2196  2198  2204  2205  2207  2211  2212
# [61]  2213  2214  2216  2217  2219  2221  2223  2224  2226  2227  2230  2233  2234  2235  2238
# [76]  2239  2241  2242  2243  2244  2247  2249  2250  2251  2252  2254  2255  2257  2258  2259
# [91]  2260  2261  2264  2270  2272  2273  2274  2278  2279  2284  2285  2288  2290  2292  2293
# [106]  2294  2300  2302  2305  2306  2310  2313  2315  2316  2319  2321  2322  2323  2324  2325
# [121]  2333  2335  2336  2338  2339  2341  2357  2362  2363  2365  2367  2368  2370  2371  2378
# [136]  2386  2387  2391  2398  2403  2407  2412  2414  2416  2420  2421  2428  2434  2437  2439
# [151]  2468  2472  2489  2491  2492  2493  2500  2509  2516  2518  2519  2523  2525  2527  3612
# [166]  3619  3624  3626  3627  3653  7577  7582  7612  8298  8311  9089  9091  9092  9093  9095
# [181]  9098  9099  9100  9105  9106  9107  9108  9125  9127  9128  9134  9170  9172  9173  9177
# [196]  9179  9180  9181  9187  9189  9191  9192  9193  9194  9195  9196  9198  9200  9201  9202
# [211]  9203  9204  9205  9217  9218  9219  9225  9226  9227  9228  9232  9233  9235  9236  9237
# [226]  9239  9241  9242  9243  9244  9245  9248  9249  9251  9253  9254  9259  9262  9264  9265
# [241]  9269  9272  9273  9276  9280  9281  9287  9293  9297  9300  9302  9303  9305  9308  9312
# [256]  9314  9315  9316  9317  9318  9319  9320  9321  9322  9325  9328  9329  9330  9331  9333
# [271]  9334  9335  9336  9340  9343  9345  9346  9347  9351  9352  9353  9361  9362  9365  9366
# [286]  9372  9376  9377  9381  9408  9409  9410  9411  9412  9413  9415  9416  9425  9428  9429
# [301]  9435  9437  9439  9440  9441  9442  9443  9444  9449  9451  9452  9454  9456  9457  9458
# [316]  9459  9461  9463  9464  9465  9466  9467  9468  9470  9472  9473  9475  9476  9477  9478
# [331]  9479  9481  9484  9485  9488  9489  9491  9496  9503  9505  9507  9508  9509  9510  9511
# [346]  9515  9516  9517  9519  9520  9521  9522  9523  9524  9525  9526  9527  9528  9529  9530
# [361]  9531  9532  9533  9534  9535  9536  9537  9538  9539  9540  9541  9542  9543  9544  9545
# [376]  9546  9547  9548  9549  9550  9551  9552  9553  9554  9555  9556  9557  9559  9562  9563
# [391]  9564  9565  9566  9567  9568  9569  9570  9571  9572  9573  9574  9575  9576  9577  9578
# [406]  9580  9581  9582  9583  9584  9585  9586  9587  9589  9590  9592  9593  9594  9595  9596
# [421]  9597  9598  9599  9600  9601  9602  9603  9604  9605  9607  9608  9611  9612  9613  9614
# [436]  9615  9617  9618  9619  9620  9622  9623  9626  9627  9628  9629  9631  9632  9633  9634
# [451]  9635  9636  9637  9639  9640  9641  9642  9643  9644  9645  9647  9648  9649  9650  9651
# [466]  9652  9653  9655  9656  9657  9658  9659  9660  9661  9662  9663  9664  9665  9667  9668
# [481]  9669  9670  9672  9673  9674  9675  9677  9678  9679  9680  9681  9682  9683  9684  9686
# [496] 10110 10195 10196 10198 10221 10224 10227 10228 10234 10235 10236 10249 10327 10329 10331
# [511] 10334 10336 10337 10338 10339 10341 10342 10344 10345 10348 10350 10354 10359 10361 10362
# [526] 10363 10364 10370 10375 10376 10377 10378 10379 10380 10381 10382 10383 10384 10385 10386
# [541] 10387 10388 10389 10390 10391 10392 10393 10394 10395 10396 10397 10398 10399 10400 10401
# [556] 10402 10403 10404 10406 10407 10413 10414 10415 10416 10417 10418 10422 10423 10424 10426
# [571] 10427 10431 10432 10433 10434 10435 10436 10444 11141 11142 11143 11144 11145 11151 12687





if(activate){
	print("Activated")
	GHYP_EOP_Simulation(returns,n=10^4,r=0,T_time=180,ydays=360,s0=adjusted_closing_prices[1,"Adj.Close"],K=adjusted_closing_prices[1,"Adj.Close"],call_Or_Put=T,martingale=martingale)		
}


#############
# Old Code for single instance
#############




#Turn on activate to TRUE if you want for standalone use
activate<-FALSE

if(activate){
	path<-"~/Dropbox/PhD Who/PhDWorks/"
	adjusted_closing_prices<-read.table(paste0(path,"Input Data/AAPL_f_20111001_t_20131001_daily.csv"),sep=",",header=TRUE)
	returns<-adjusted_closing_prices[,"Adj.Close"]
	returns<-rev(log(returns[-length(returns)]/returns[-1]))
	martingale<-"Esscher"
#	martingale<-"MCMM"
}

#Install the required packages
if(!("ghyp" %in% rownames(installed.packages())))
	install.packages("ghyp")
if(!("Runuran" %in% rownames(installed.packages())))
	install.packages("Runuran")
if(!("fBasics" %in% rownames(installed.packages())))
	install.packages("fBasics")

library(ghyp)
library(Runuran)
library(fBasics)


#This is the MGF of the GHYP distribution
GHYP_Moment_Generating_Function<-function(x,alpha,beta,delta,mu,lambda){
	exp(x*mu)*((alpha^2-beta^2)/(alpha^2-(beta+x)^2))^(lambda/2)*besselK(delta*sqrt(alpha^2-(beta+x)^2),lambda)/besselK(delta*sqrt(alpha^2-beta^2),lambda)
}

#Here we simulate the risk neutral process 
GHYP_EOP_Simulation<-function(returns,n=5000,r=0,T_time=180,ydays=360,s0=100,K=90,call_Or_Put=T,martingale="Esscher"){
	ttm<-T_time
	if(martingale==1){
		martingale<-"Esscher"
	}else if(martingale==2){
		martingale<-"MCMM"
	}
	print(martingale)
#Infer the parameters using the fit.ghypuv of the ghyp package
	params<-coef(fit.ghypuv(returns, silent=TRUE, control=list(maxit=2000)), type="alpha.delta")
#These three functions below helps to sample from the GHYP process using the inverse of the function

	if(martingale=="Esscher"){
		#Find the risk neutrality parameter theta using the MGF of GHYP 
		f<-function(x){
			r - log(GHYP_Moment_Generating_Function(x+1,alpha=params$alpha,beta=params$beta,delta=params$delta,mu=params$mu,lambda=params$lambda)/
			GHYP_Moment_Generating_Function(x,alpha=params$alpha,beta=params$beta,delta=params$delta,mu=params$mu,lambda=params$lambda))
		}
		theta<-tryCatch(uniroot(f,c(-(params$alpha+params$beta)+0.00001,(params$alpha-params$beta-1)-0.00001))$root,error=function(e){warning(conditionMessage(e));NA})
		if(is.na(theta)){
			print("Esscher transform does not exist for these parameters.")
			result<-rep(0,4)
			names(result)<-c("Price","SError","Lower","Upper")
			return(result)
		}
		ghyp_risk_neutral_pdf<-function(x) dgh(x,alpha=params$alpha,beta=theta + params$beta,delta=params$delta,mu=params$mu,lambda=params$lambda)
	}else if(martingale=="MCMM"){
		mu_new <- params$mu - r - log(GHYP_Moment_Generating_Function(1,alpha=params$alpha,beta=params$beta,delta=params$delta,mu=params$mu,lambda=params$lambda))
		ghyp_risk_neutral_pdf<-function(x) dgh(x,alpha=params$alpha,beta=params$beta,delta=params$delta,mu=mu_new,lambda=params$lambda)
	}else{
			print("Wrong martingale.")
			result<-rep(0,4)
			names(result)<-c("Price","SError","Lower","Upper")
			return(result)
	}

	inverse_distribution<-pinv.new(pdf=ghyp_risk_neutral_pdf,lb=-Inf,ub=Inf)	
	St_GH<-s0*exp(uq(inverse_distribution,runif(n)))
#Aaand simulate	
	for(i in 2:ttm){
		St_GH<-St_GH*exp(uq(inverse_distribution,runif(n)))
	}
	if(call_Or_Put==T)
		price<-exp(-r*ttm/ydays)*pmax(St_GH-K,0)
	else
		price<-exp(-r*ttm/ydays)*pmax(K-St_GH,0)

	result<-c(mean(price),1.96*sd(price)/sqrt(n),mean(price)-1.96*sd(price)/sqrt(n),mean(price)+1.96*sd(price)/sqrt(n))
	names(result)<-c("Price","SError","Lower","Upper")
	
	return(result)
		
}

if(activate){
	print("Activated")
	GHYP_EOP_Simulation(returns,n=10^4,r=0,T_time=180,ydays=360,s0=adjusted_closing_prices[1,"Adj.Close"],K=adjusted_closing_prices[1,"Adj.Close"],call_Or_Put=T,martingale=martingale)		
}


