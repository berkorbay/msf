#Author: Berk Orbay 

#This code snippet is irrelevant to the model, just to clean the workspace
rm(list=ls(all=TRUE))

#This is the R implementation of the Duan GARCH option pricing method 
#Source Article: 011001 The GARCH Option Pricing Model - Duan (1995).pdf

algorithm_name<-"GARCH_Duan_1995"

timestamp<-function(xtime){
	format(xtime,"%Y%m%d_%H%M%S_")
}

runtime<-function(now,later){
	timesec<-as.numeric(difftime(later,now,units="secs"))
	difsec<-timesec%%60
	timemin<-(timesec-difsec)/60
	difmin<-timemin%%60
	timehr<-(timemin-difmin)/60
	difhr<-timehr%%24
	timeday<-(timehr-difhr)/24
	return(paste0(timeday, " days ", difhr, " hours ", difmin, " minutes ", difsec, " seconds"))
}

outputsummary_GARCH_Duan_1995<-function(thestamp,suffix_name){
	file_name<-paste0(output_path,thestamp,suffix_name,".txt")
	cat("Start Time:", format(start_time), "\n", file=file_name, append = FALSE)
	cat("End Time:", format(end_time), "\n", file=file_name, append = TRUE)
	cat("Elapsed Time:", runtime(start_time,end_time), "\n", file=file_name, append = TRUE)
	cat("Elapsed Time in seconds:", as.numeric(difftime(end_time,start_time,units="secs")), "\n", file=file_name, append = TRUE)	
}
outputsummary_GARCH_Duan_1995(timestamp(end_time),paste0(algorithm_name,"_trial")


#TRUE if you want for standalone use
standalone<-TRUE
full_verbose<-TRUE

if(standalone){
	start_time<-Sys.time()
	if(full_verbose){
		print(paste0("Start time: ", start_time))
	}
	#The path part
	main_path<-"~/Dropbox/PhD_Workshop/"
	input_path<-paste0(main_path,"Input Files/")
	output_path<-paste0(main_path,"Output Files/",algorithm_name,"/")
	model_path<-paste0(main_path,"Models/",algorithm_name,"/")

	#Here we take the adjusted closing prices from some source and compute the returns
	adjusted_closing_prices<-read.table(paste0(input_path,"AAPL_f_20111001_t_20131001_daily.csv"),sep=",",header=TRUE)
	returns<-adjusted_closing_prices[,"Adj.Close"]
	returns<-rev(log(returns[-length(returns)]/returns[-1]))
}

#This is the GARCH process used to filter the parameter set that gives the highest likelihood for the sample
Normal_GARCH_Duan_P<-function(parameters,returns,r,tracey=1){
#omega is the long term variance
	omega<-parameters[1]
#alpha is the error term coefficient of GARCH(1,1)
	alpha<-parameters[2]
#beta is the previous volatility coefficient of GARCH(1,1)
	beta<-parameters[3]
#lambda is the price of the unit risk
	lambda<-parameters[4]
#sig2 is the initial variance value of the , later it will be used to 
	sig2<-var(returns)
#	sig2<-omega/(1-alpha-beta)
#initial error term is set to zero
	err<- 0
#set the initial log-likelihood,this is left deliberately negative of the log-likelihood function 
#because the optimization function is set to minimize
#These are the constraints imposed by Duan and GARCH fundamentals, failure to comply results in a nasty way
	if(alpha + beta > 0.995 || abs(lambda) >= sqrt((1 - alpha - beta)/alpha)){
		llh<-1e+10
		return(llh)
	}	


	llh<- 0
	n<-length(returns)
	for(i in 2:(n+1)){
		sig2[i]<- omega + alpha*err[i-1]^2 + beta*sig2[i-1]
		err[i] <- (returns[i-1] - r - lambda*sqrt(sig2[i]) + 0.5*sig2[i])
		if(dnorm(err[i],0,1)>1 || is.na(err[i])){
			print(paste0("Error problem: ",dnorm(err[i],0,1)/sqrt(sig2[i])," Iteration: ", i))
			print(parameters)
			return(-Inf)			
		}
		llh <- llh - log(dnorm(err[i],0,sqrt(sig2[i])))
	}
#print(paste("Sigma:",mean(sig2),"Error:",mean(err),mean(dnorm(err,0,sqrt(sig2)))))
#print(paste("Volatility:",mean(sig2)))
#This code is relevant with the optimization process. 	
#It is to prevent llh be too big or too small for the glitches in the nlminb minimization function.
	if(is.na(llh)){
		llh<- 1e+10
		return(llh)
	}
	if(llh == Inf){
		llh<- 1e+10	
		return(llh)
	}
#Return the log likelihood value
	llh
}

#This is the main function to find optimal GARCH parameters for the given sample
Normal_GARCH_Duan_Parameter_Inference<-function(ignition,returns,r=0,tracey=1){
	print("Hello This is Inference")
	print(returns[1:5])
#	print(paste("Initial Parameters: ",ignition,"Returns NA: ",any(is.na(returns)),"risk-free rate: ",r))
#Setting optimization thresholds
	low<-rep(1e-6,4)
	high<-c(0.4,0.995,0.995,10)
#nlminb is used to find fitting parameter values, using the Normal_GARCH_Duan_P function
	params<-nlminb(start=ignition,objective=Normal_GARCH_Duan_P, returns=returns,r=r,lower=low,upper=high,control=list(x.tol=1e-11,trace=tracey))
	params
}


Normal_GARCH_Duan_Q<-function(returns,n,s0,K,T_time,ydays,r,call_True_Put_False,tracey=1){
	GARCH_parameters<-Normal_GARCH_Duan_Parameter_Inference(c(0.000015,0.19,0.72,0.007),returns,r,tracey)$par
	print(GARCH_parameters)
	if(tracey) print("Welcome")
#omega is the long term variance
	omega<-GARCH_parameters[1]
#alpha is the error term coefficient of GARCH(1,1)
	alpha<-GARCH_parameters[2]
#beta is the previous volatility coefficient of GARCH(1,1)
	beta<-GARCH_parameters[3]
#lambda is the price of the unit risk
	lambda<-GARCH_parameters[4]
#sig2 is the initial variance value of the , later it will be used to 
	sig2<-var(returns)
#	sig2<- omega / (1 - alpha*(1+lambda^2) - beta)
#initial error term is set to zero
	err<- 0
#set the initial log-likelihood,this is left deliberately negative of the log-likelihood function 
#because the optimization function is set to minimize
print(paste(omega,alpha,beta,lambda,sig2,err))
#Start the simulation	
	for(i in 1:n){
#Initialize the simulation vector
		simpath<-s0
#Notification of the process
		if(tracey && i %% 1000 == 0){print(paste0("Iteration ",i))}
#Simulate the process as shown in the paper
		for(j in 2:T_time){
			if(tracey && j == T_time-1 && i %% 1000 == 0){print(paste("Sig2 at ",j," ",sig2[j]))}
			sig2[j]<-omega + alpha*(err[j-1] - lambda*sqrt(sig2[j-1]))^2 + beta*sig2[j-1]
			err[j]<-sqrt(sig2[j])*rnorm(1)
			simpath<-simpath*exp(r - 0.5*sig2[j] + err[j])		
		}
#Since R should first create a variable or a vector for i == 1 we create sT
		if(i == 1)
			sT<-simpath
		else
			sT[i]<-simpath

		if(tracey && i %% 1000 == 0){print(paste0("Final S ",sT[i]))}

	}
#Is our option a put or a call?	
	if(call_True_Put_False){
		resultvec<-pmax(sT-K,0)*exp(-r*T_time/ydays)
	}else{
		resultvec<-pmax(K-sT,0)*exp(-r*T_time/ydays)
	}

#Option price, standard error and lower and upper bounds for 95% confidence interval
	result<-c(mean(resultvec),1.96*sd(resultvec)/sqrt(n),mean(resultvec)-1.96*sd(resultvec)/sqrt(n),mean(resultvec)+1.96*sd(resultvec)/sqrt(n))
	names(result)<-c("Price","SError","Lower","Upper")
	print(result)
	return(result)
}

if(standalone){
	Normal_GARCH_Duan_Q(returns=returns,n=10^4,s0=adjusted_closing_prices[1,"Adj.Close"],K=adjusted_closing_prices[1,"Adj.Close"],T_time=180,ydays=360,r=0,call_True_Put_False=TRUE,tracey=1)
	if(full_verbose){
		end_time<-Sys.time()
		print(paste0("End time: ", end_time))
		print(paste0("Elapsed time: ", end_time - start_time))
	}
}
