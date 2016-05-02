#Author: Berk Orbay 

#This code snippet is irrelevant to the model, just to clean the workspace
rm(list=ls(all=TRUE))

#This is the R implementation of the Duan's GARCH option pricing method with Markov Chains  
#Source Article: 000000 American Option Pricing under GARCH by a Markov chain approximation - Duan and Simonato (2001).pdf

algorithm_name<-"GARCH_MC_Duan"

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

outputsummary_GARCH_Duan_MC<-function(thestamp,suffix_name){
	file_name<-paste0(output_path,thestamp,suffix_name,".txt")
	cat("Start Time:", format(start_time), "\n", file=file_name, append = FALSE)
	cat("End Time:", format(end_time), "\n", file=file_name, append = TRUE)
	cat("Elapsed Time:", runtime(start_time,end_time), "\n", file=file_name, append = TRUE)
	cat("Elapsed Time in seconds:", as.numeric(difftime(end_time,start_time,units="secs")), "\n", file=file_name, append = TRUE)	
}
outputsummary_GARCH_Duan_MC(timestamp(end_time),paste0(algorithm_name,"_trial")


#TRUE if you want for standalone use
standalone<-TRUE
full_verbose<-TRUE

GARCH_DUAN_MC_Payoff<-function(opt_type,K,S0,r,ttm,price_vector,h_star,n_length){
		if(opt_type=="AP"){
			return(pmax(K - exp((r-0.5*h_star)*ttm+state_matrix[,"price"]),0))
		}else if(opt_type=="EP"){
			return(pmax(K - exp((r-0.5*h_star)*ttm+state_matrix[,"price"]),0))
		}else{
			print("Wrong option type. Returning the default payoff function American Put")
			return(pmax(K - exp((r-0.5*h_star)*ttm+state_matrix[,"price"]),0))
		}
}


rm(list=ls(all=TRUE))

GARCH_DUAN_MC_Q<-function(days_per_year,time_step,ttm,r_annualized,K,S0,m_length,n_length,garch_parameters,tao=90,constant_volatility=FALSE){
	#### Code Specific Part####
	#days_per_year is the number of days in a year default 365
	#ttm is the time to maturity in days  default 90
	#r is the risk free rate default 0.05 annualized convert it to daily
	#K is the strike price default 50
	#S0 is the initial stock price default 50
	#m_length is the length of the log-prices vector
	#n_length is the length of the log-variance vector
	#tao is the weight to determine q1_star default 3 months 90 days

	if(constant_volatility&&(n_length!=1)){
		print(paste("If volatility is constant, then n_length should be one. Converting it to one."))
		n_length<-1
	}

	#r<-log(1+r_annualized)/days_per_year
	r<-r_annualized*time_step/days_per_year

	stm<-ttm/time_step
	if(stm%%1>0){
		print(paste("Time step is not an integer divisor of time to maturity. New time to maturity is "),round(stm,0)*time_step)
		stm<-round(stm,0)
	}
	#garch_omega is the long term variance aka beta_0 in the paper, default 10^(-5) 
	#garch_beta is the weight of the previous variance term aka beta_1 in the paper, 0.8
	#garch_alpha is the weight of the previous error term aka beta_2 in the paper, 0.1
	#garch_theta is the leverage parameter aka theta in the paper, 0.3
	#garch_lambda is the risk premium aka lambda in the paper, 0.2

	garch_omega<-garch_parameters$omega

	if(constant_volatility&&(garch_parameters$beta+garch_parameters$alpha >0)){
		print(paste("If volatility is constant, then garch beta and alpha should be zero. Converting them to zero."))
		garch_beta<-0
		garch_alpha<-0 		
	}else{
		garch_beta<-garch_parameters$beta
		garch_alpha<-garch_parameters$alpha 		
	}

	garch_theta<-garch_parameters$theta
	garch_lambda<-garch_parameters$lambda


	#Internal checks for m_length and n_length
	if(is.integer(m_length/2)){
		m_length<-m_length+1
		print(paste("Length of log-price vector should be an odd number. Automatically fixing it to ", m_length))
	}

	if((!constant_volatility)&&is.integer(n_length/2)){
		n_length<-n_length+1
		print(paste("Length of log-price vector should be an odd number. Automatically fixing it to ", n_length))
	}

	#h_1 is the stationary variance of P
	h_1<-garch_omega*(1-garch_beta-garch_alpha*(1+(garch_theta)^2))^(-1)
	#h_star is the stationary variance of Q
	h_star<-garch_omega*(1-garch_beta-garch_alpha*(1+(garch_theta+garch_lambda)^2))^(-1)

	#delta_p and I_p determines the range of the log-prices
	delta_p<-2+log(log(m_length))
	garch_v<-garch_beta+garch_alpha*(1+(garch_theta+garch_lambda)^2)
	I_p<-delta_p*sqrt(sum(h_1*garch_v^((1:stm)-1)+garch_omega*(1-garch_v^((1:stm)-1))/(1-garch_v)))

	#logprice_p is the log-price states aka p(i) in the paper
	logprice_p<-seq(log(S0)-I_p,log(S0)+I_p,length.out=m_length)

	#q1_star is the central value of the log-variance vector
	q1_star<-log((tao-min(tao,stm))/tao*h_1+min(tao,stm)/tao*h_star)

	if(constant_volatility){
		logvar_q<-q1_star
	}else{
		#delta_q and I_q determines the range of the log-variances
		delta_q<-2+log(log(n_length))
		garch_u<-(garch_alpha^2*(3+6*(garch_theta+garch_lambda)^2+(garch_theta+garch_lambda)^4)+
			2*garch_beta*garch_alpha*(1+(garch_theta+garch_lambda)^2)+garch_beta^2)
		eQhT<-h_1*garch_v^(stm-1)+garch_omega*(1-garch_v^(stm-1))/(1-garch_v)
		eQhT_2<-(h_1^2*garch_u^(stm-1)+2*garch_omega*h_1*(garch_v*(garch_u^(stm-1)-garch_v^(stm-1))/(garch_u-garch_v))+
			garch_omega^2*((1-garch_u^(stm-1))/(1-garch_u)+2*garch_v/(garch_u-garch_v)*((1-garch_u^(stm-1))/(1-garch_u)-
			(1-garch_v^(stm-1))/(1-garch_v))))
		sigma_h<-sqrt(eQhT_2-eQhT^2)
		I_q<-log(h_1+delta_q*sigma_h)-log(h_1)

		#logvar_q is the log-variance states aka q(i) in the paper
		logvar_q<-seq(q1_star-I_q,q1_star+I_q,length.out=n_length)
	}

	#logprice_c is the interval boundaries for each log-price state aka c(i) in the paper
	#length of stock_c is m+1
	logprice_c<-c(-Inf,(logprice_p[-1]+logprice_p[-m_length])/2,Inf)

	if(constant_volatility){
		logvar_d<-logvar_q
	}else{
		#logvar_d is the interval boundaries for each log-variance state aka d(i) in the paper
		#length of logvar_d is n+1
		logvar_d<-c(-Inf,(logvar_q[-1]+logvar_q[-n_length])/2,Inf)
	}

	state_matrix<-expand.grid(logprice_p,logvar_q)
	colnames(state_matrix)<-c("price","var")

	#transition_matrix is the transition matrix
#	transition_matrix<-matrix(0,ncol=m_length*max(1,n_length),nrow=m_length*max(1,n_length))
	ck_pi<-t(matrix(logprice_c,ncol=(m_length)*max(1,n_length),nrow=(m_length+1)*max(1,n_length)))-logprice_p

	if(constant_volatility){
		vol_filter<-1
	}else{
		next_vols<-log(garch_omega+garch_beta*exp(logvar_q)+garch_alpha*(t(price_tmat)-price_tmat+0.5*(exp(logvar_q)-h_star)
			-(garch_theta+garch_lambda)*exp(logvar_q/2))^2)

		vol_filter<-t((t(next_vols)<sort(rep(logvar_d[-1],m_length)))*(t(next_vols)>sort(rep(logvar_d[-(n_length+1)],m_length))))
	}


	l_ijk_mat<-pnorm((ck_pi+0.5*(exp(state_matrix[,"var"])-h_star))/sqrt(exp(state_matrix[,"var"])))
#	l_ijk_mat_upper<-(t(logprice_c[-1]-t(price_tmat))-0.5*(exp(state_matrix[,"var"])-h_star))/sqrt(exp(state_matrix[,"var"]))
#	cdf_values<-pnorm(l_ijk_mat)
#	transition_probabilities<-cdf_values[,-1]-cdf_values[,-(m_length+1)]
	transition_probabilities<-l_ijk_mat[,-1]-l_ijk_mat[,-ncol(l_ijk_mat)]
#	transition_probabilities<-matrix(transition_probabilities,ncol=m_length*max(1,n_length),nrow=m_length*max(1,n_length))
	transition_matrix<-vol_filter*transition_probabilities
	#opVal is the option value at iteration t
	opVal<-pmax(K - exp((r-0.5*h_star)*stm+state_matrix[,"price"]),0)
	if(stm>=1){
		for(t in (stm):1){
			opVal<-pmax(K - exp((r-0.5*h_star)*(t-1)+state_matrix[,"price"]),exp(-r)*(transition_matrix%*%opVal))
#			opVal<-exp(-r)*(transition_matrix%*%opVal)
		}
	}




	if(constant_volatility){
		opPrice<-opVal[ceiling(m_length/2)]
	}else{
		opVal<-opVal[round(state_matrix[,"price"],5)==round(log(S0),5)]

		# if(any(log(h_1)==logvar_q)){
		# 	opPrice<-opVal[ceiling(n_length/2)]
		# }else{
		# 	lower_j<-order(abs(logvar_q-log(h_1)))[1]
		# 	if(logvar_q[lower_j]>=log(h_1)){
		# 		lower_j<-lower_j+1
		# 	}
		# 	opPrice<-(logvar_q[lower_j+1]-log(h_1))/(logvar_q[lower_j+1]-logvar_q[lower_j])*opVal[lower_j]+
		# 				(log(h_1)-logvar_q[lower_j])/(logvar_q[lower_j+1]-logvar_q[lower_j])*opVal[lower_j+1]
		# }

		#This is the paper's original version of locating the true S0-variance position by interpolation
		if(any(log(h_1)==logvar_d)){
			opPrice<-opVal[ceiling(n_length/2)]
		}else{
			lower_j<-order(abs(logvar_d-log(h_1)))[1]
			if(logvar_q[lower_j]>=log(h_1)){
				lower_j<-lower_j+1
			}
			opPrice<-(logvar_d[lower_j+1]-log(h_1))/(logvar_d[lower_j+1]-logvar_d[lower_j])*opVal[lower_j]+
						(log(h_1)-logvar_d[lower_j])/(logvar_d[lower_j+1]-logvar_d[lower_j])*opVal[lower_j+1]
		}

	}	
	return(opPrice)
}


#garch_pars<-list(omega=10^(-5),beta=0.8,alpha=0.1,theta=0.3,lambda=0.2)
garch_pars<-list(omega=(0.2)^2*(1/365),beta=0.8,alpha=0.1,theta=0.3,lambda=0.2)

GARCH_DUAN_MC_Q(days_per_year=365,time_step=1,ttm=90,r_annualized=0.05,K=50,S0=50,m_length=51,n_length=1,tao=90,garch_parameters=garch_pars,constant_volatility=TRUE)


days_per_year=365
ttm=90
time_step=1
r_annualized=0.05
K=50
S0=50
m_length=51
n_length=1
tao=90
garch_parameters=garch_pars
constant_volatility=TRUE
#