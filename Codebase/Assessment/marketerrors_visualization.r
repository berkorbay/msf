#This is the code for graphical and detailed analysis of option price estimates.
#Warning! Code pieces are not independent!

packy<-"ggplot2"
if(!(packy %in% rownames(installed.packages()))){
	install.packages(packy)
}else{
	update.packages(packy)
}
library(ggplot2)

packy<-"MASS"
if(!(packy %in% rownames(installed.packages()))){
	install.packages(packy)
}else{
	update.packages(packy)
}
library(MASS)

packy<-"dplyr"
if(!("dplyr" %in% rownames(installed.packages()))){
	install.packages("dplyr")
}else{
	update.packages("dplyr")
}
library(dplyr)


packy<-"reshape2"
if(!(packy %in% rownames(installed.packages()))){
	install.packages(packy)
}else{
	update.packages(packy)
}
library(reshape2)

packy<-"scales"
if(!(packy %in% rownames(installed.packages()))){
	install.packages(packy)
}else{
	update.packages(packy)
}
library(scales)

packy<-"gridExtra"
if(!(packy %in% rownames(installed.packages()))){
	install.packages(packy)
}else{
	update.packages(packy)
}
library(gridExtra)

packy<-"gtable"
if(!(packy %in% rownames(installed.packages()))){
	install.packages(packy)
}else{
	update.packages(packy)
}
library(gtable)



kde2d.weighted <- function (x, y, w, h, n = 25, lims = c(range(x), range(y))) {
  nx <- length(x)
  if (length(y) != nx) 
    stop("data vectors must be the same length")
  if (length(w) != nx & length(w) != 1)
    stop("weight vectors must be 1 or length of data")
  gx <- seq(lims[1], lims[2], length = n) # gridpoints x
  gy <- seq(lims[3], lims[4], length = n) # gridpoints y
  if (missing(h)) 
    h <- c(bandwidth.nrd(x), bandwidth.nrd(y));
  if (missing(w)) 
    w <- numeric(nx)+1;
  h <- h/4
  ax <- outer(gx, x, "-")/h[1] # distance of each point to each grid point in x-direction
  ay <- outer(gy, y, "-")/h[2] # distance of each point to each grid point in y-direction
  z <- (matrix(rep(w,n), nrow=n, ncol=nx, byrow=TRUE)*matrix(dnorm(ax), n, nx)) %*% t(matrix(dnorm(ay), n, nx))/(sum(w) * h[1] * h[2]) # z is the density
  return(list(x = gx, y = gy, z = z))
}



results_path<-"~/Dropbox/PhD_Workshop/Output Files/"

mydata<-read.table(paste0(results_path,"HestonNandi_GARCH/SPX_2007_HN_data_withdiv_asym_5y.csv"),sep=",",header=TRUE)


################
#Calculate error terms
################

mydata<- mydata %>%
			mutate(arpe = abs(HN_prices-Last)/Last*100, #Absolute Relative Percentage Error
					rpe = (HN_prices-Last)/Last*100) #Relative Percentage Error

################
################


################
#Heatmap Using Densities
#Working but not so well
################

dens<-kde2d.weighted(mydata$Moneyness,mydata$NetMaturity,mydata$arpe,n=200)
dfdens <- data.frame(expand.grid(x=dens$x, y=dens$y), z=as.vector(dens$z))

ggplot(mydata, aes(x = Moneyness, y = NetMaturity)) + xlim(0.75,1.25) + ylim(0,100) +
    geom_point(size=0.5) + #+ xlim(0.5, 6) + ylim(40, 110) +
	geom_tile(aes(x=x, y=y,fill=z),data= dfdens, alpha=0.8) + scale_fill_gradient(breaks=seq(0.1,1,length.out=10))
	# scale_fill_gradient(low="green", high="red")

################
################



################
#Aggregating into summary tables
################
provide_summary_table<-function(mydata,moneyness_cuts=c(-Inf,0.94,0.97,1,1.03,1.06,Inf),maturity_cuts=c(0,60,180,Inf)){

	mydata %>%
		dplyr::select(Moneyness, NetMaturity, arpe,rpe) %>% #select only required columns dplyr:: is because of conflict with MASS::select
		mutate(Monbreaks=cut(Moneyness,moneyness_cuts,right=FALSE), Matbreaks=cut(NetMaturity,maturity_cuts,right=FALSE)) %>% #cluster with breaks
		dcast(Monbreaks ~ Matbreaks,mean,value.var="arpe",margins=c("Matbreaks","Monbreaks")) #make the summary table

}
################
################

new_moncuts<-c(-Inf,0.8,0.9,1,1.1,1.2,Inf)
new_matcuts<-c(0,0.25*252,0.5*252,252,Inf)
sumdata<-provide_summary_table(mydata,new_moncuts,new_matcuts)
rownames(sumdata)<-sumdata[,1]
sumdata<-t(sumdata[,-1])

################
#Heatmap with real values (grey areas are missing data)
################

# new_moncuts<-c(-Inf,seq(0.94,3,length.out=5),Inf)
# new_matcuts<-c(0,seq(60,200,length.out=5),Inf)
# sumdata<-provide_summary_table(mydata,new_moncuts,new_matcuts)

new_moncuts<-seq(0.25,4,length.out=10)
new_matcuts<-seq(0,550,length.out=10)
sumdata<-provide_summary_table(mydata,new_moncuts,new_matcuts)
sumdatalong<-sumdata[-nrow(sumdata),-ncol(sumdata)]
sumdatalong[,1]<-(new_moncuts[-1]+new_moncuts[-length(new_moncuts)])/2
colnames(sumdatalong)[-1]<-(new_matcuts[-1]+new_matcuts[-length(new_matcuts)])/2

# sumdatalong<-sumdata[-nrow(sumdata),-ncol(sumdata)]

# sumdatalong[,1]<-c(new_moncuts[-c(1,length(new_moncuts))],max(mydata$Moneyness))
# colnames(sumdatalong)[-1]<-c(new_matcuts[-c(1,length(new_matcuts))],max(mydata$NetMaturity))

sumdatalong_plot_data<-melt(sumdatalong,id.vars="Monbreaks",,value.name="arpe")
sumdatalong_plot_data$variable<-as.numeric(as.character(sumdatalong_plot_data$variable))

ggplot(sumdatalong_plot_data, aes(x = Monbreaks, y = variable)) + 
	geom_tile(aes(x=Monbreaks, y=variable,fill=arpe),data= sumdatalong_plot_data, alpha=0.8) + 
	ylab("Maturity") + xlab("Moneyness") +
	scale_fill_gradient2(name="ARPE",midpoint=1000, low="green",high="red", mid="blue")
################
################



################
#Back-to-back Bar Charts 1: Call-Put Counts
################


mydata$DataDate<-as.Date(mydata$DataDate)

mychanges<- mydata %>%
				mutate(Month=format(DataDate,"%m")) %>%
				group_by(Month,Type) %>%
				summarise(count=n())

ggplot(data=mychanges,aes(x=Month, y = count)) + 
	geom_bar(data=subset(mychanges, Type == "call"), aes(y=count,fill=Type),stat="identity") +
	geom_bar(data=subset(mychanges, Type == "put"), aes(y=-count,fill=Type),stat="identity") + #aes(,subset = .(Type == "call"), y = count), fill=Type, stat = "identity") + 
	#geom_bar(subset = .(Type == "put"), aes(y = -count), stat = "identity") +
	geom_hline(yintercept = 0, colour = "grey90", size=2) +
	xlab("Month") + scale_y_continuous("Put - Call",labels=trans_format("abs",format=NULL))


################
#Back-to-back Bar Charts 2: Call-Put & Short Long Counts
################


mydata$DataDate<-as.Date(mydata$DataDate)

mychanges<- mydata %>%
				mutate(Month=format(DataDate,"%m"),Decision=ifelse(Last<=HN_prices,"long","short")) %>%
				group_by(Month,Type,Decision) %>%
				summarise(count=n())

ggplot(data=mychanges,aes(x=Month, y = count)) + 
	geom_bar(data=subset(mychanges, Decision == "long"), aes(y=count,fill=Type),stat="identity") +
	geom_bar(data=subset(mychanges, Decision == "short"), aes(y=-count,fill=Type),stat="identity") + #aes(,subset = .(Type == "call"), y = count), fill=Type, stat = "identity") + 
	#geom_bar(subset = .(Type == "put"), aes(y = -count), stat = "identity") +
	geom_hline(yintercept = 0, colour = "grey90", size=2) +
	xlab("Month") + scale_y_continuous("Short - Long",labels=trans_format("abs",format=NULL),breaks=seq(-ceiling(max(mychanges$count)/250)*250,ceiling(max(mychanges$count)/250)*250,by=250))


################
################

################
#Back-to-back Bar Charts 3: Call-Put & Short Long Amounts
################


mydata$DataDate<-as.Date(mydata$DataDate)

mychanges<- mydata %>%
				mutate(Month=format(DataDate,"%m"),Decision=ifelse(Last<=HN_prices,"long","short")) %>%
				group_by(Month,Type,Decision) %>%
				summarise(total=sum(Last))

ggplot(data=mychanges,aes(x=Month, y = count)) + 
	geom_bar(data=subset(mychanges, Decision == "long"), aes(y=total,fill=Type),stat="identity") +
	geom_bar(data=subset(mychanges, Decision == "short"), aes(y=-total,fill=Type),stat="identity") + #aes(,subset = .(Type == "call"), y = count), fill=Type, stat = "identity") + 
	#geom_bar(subset = .(Type == "put"), aes(y = -count), stat = "identity") +
	geom_hline(yintercept = 0, colour = "grey90", size=2) +
	xlab("Month") + scale_y_continuous("Short - Long",labels=trans_format("abs",format=NULL)#,
		#breaks=seq(-ceiling(max(mychanges$count)/250)*250,ceiling(max(mychanges$count)/250)*250,by=250))
	)

################
################


################
#Back-to-back Bar Charts 4: Profit-loss
################


mydata$DataDate<-as.Date(mydata$DataDate)

mychanges<- mydata %>%
				mutate(Month=format(DataDate,"%m"),Decision=ifelse(Last<=HN_prices,"long","short"),Outcome=(ExpirationPayoff-Last)*ifelse(Last<=HN_prices,1,-1),PoL=ifelse(Outcome>0,"profit","loss")) %>%
				group_by(Month,Type,PoL) %>%
				summarise(total=abs(sum(Outcome)))

mypnl<- mydata %>%
			mutate(Month=format(DataDate,"%m"),Decision=ifelse(Last<=HN_prices,"long","short"),Outcome=(ExpirationPayoff-Last)*ifelse(Last<=HN_prices,1,-1),PoL=ifelse(Outcome>0,"profit","loss")) %>%
			group_by(Month) %>%
			summarise(total=sum(Outcome)) %>%
			mutate(balance=cumsum(total))

ggplot(data=mychanges,aes(x=Month,y=total)) + 
	geom_bar(data=subset(mychanges, PoL == "profit"), aes(x=Month,y=total,fill=Type),stat="identity") +
	geom_bar(data=subset(mychanges, PoL == "loss"), aes(x=Month,y=-total,fill=Type),stat="identity") + 
	geom_hline(yintercept = 0, colour = "grey90", size=0.5) +
	geom_line(data=mypnl,aes(x=Month,y=total,group=1,color="Monthly"),size=1.1) + 
	geom_line(data=mypnl,aes(x=Month,y=balance,group=1,color="Cumulative"),size=1.1) + 
	scale_fill_manual(name="Type",values=c(call="#3D4C53", put="#E6772E")) +
	scale_color_manual(name="Balance",values=c(Cumulative="#4DB3B3",Monthly="#F2C249")) +
	xlab("Month") + scale_y_continuous("Profit-Loss",labels=trans_format("abs",format=NULL)#,breaks=unique(c(seq(-ceiling(max(subset(mychanges, PoL == "loss")$total)/10000)*10000,0,by=10000),seq(0,ceiling(max(mychanges$total)/10000)*10000,by=10000)))
#	breaks=seq(-ceiling(max(mychanges$total)/250)*250,ceiling(max(mychanges$total)/250)*250,by=250)
	)

################
################

################
# Linear Regression Model
################

results_path<-"~/Dropbox/PhD_Workshop/Output Files/Survey_Results/"


myrawdata<-read.table("~/Dropbox/PhD_Workshop/Input Files/Asset Options/SPX_2011_options_filtered_A12.csv",sep=",",header=TRUE)


mydata<-read.table(paste0(results_path,"HestonNandi_GARCH/SPX_2011_HN_data_withdiv_asym_5y_012000.csv"),sep=",",header=TRUE)
mydata$DataDate<-as.Date(mydata$DataDate)
mydata<- mydata %>%
			mutate(arpe = abs(Last-HN_prices)/Last*100, #Absolute Relative Percentage Error
					rpe = (Last-HN_prices)/Last*100,
					se =  (Last-HN_prices)^2,#Relative Percentage Error
					Outcome=(ExpirationPayoff-Last)*ifelse(Last<=HN_prices,1,-1)) 

mydata<- mydata %>%
			filter(NetMaturity <= 252 & Moneyness > 0.9 & Moneyness < 1.1 & Type == "call")

merged_table<-inner_join(mydata,myrawdata)

myresult<-lm(Outcome ~ se + NetMaturity + Moneyness,data=merged_table)

myresult2<-lm(Outcome ~ se,data=merged_table)

################
################

################
# ATM movement
################
options(dplyr.width = Inf)
# atmdata <- mydata %>%
# 			mutate(atm=abs(Moneyness-1)) %>%
# 			filter(Type=="call") %>%
# 			arrange(DataDate,atm) %>%
# 			group_by(DataDate) 

# atmsummary<- atmdata %>%
# 			summarise(minDiff=min(atm))

# atm_join<-inner_join(atmdata,atmsummary)

# atm_join<- atm_join %>%
# 				filter(atm==minDiff) %>%
# 				mutate(priceperc=Last/UnderlyingPrice,Matbreaks=cut(NetMaturity,c(0,60,180,Inf),right=FALSE))


# ggplot(data=atm_join, aes(x=DataDate,y=priceperc)) + geom_point(aes(color=Matbreaks),size=4)

get_atm_error_graph<-function(results_path="~/Dropbox/PhD_Workshop/Output Files/",results_model_path="Black_Scholes/",underlying_asset="SPX",data_year=2012,progressOutput=TRUE,model_specs="BS_data_withdiv_2y",call_or_put="call",error_type="rpe",price_column="BS.HV2y",save_graphs=TRUE,save_path="~/Dropbox/PhD_Workshop/Graphs/Error Graphs/"){

	mydata<-read.table(paste0(results_path,results_model_path,underlying_asset,"_",data_year,"_",model_specs,".csv"),sep=",",header=TRUE)
	#mydata<-read.table(paste0(results_path,"HestonNandi_GARCH/SPX_2011_HN_data_withdiv_asym_5y.csv"),sep=",",header=TRUE)
	mydata$DataDate<-as.Date(mydata$DataDate)
	#Options Input Data
	myrawdata<-read.table(paste0("~/Dropbox/PhD_Workshop/Input Files/Asset Options/",underlying_asset,"_",data_year,"_options_filtered_A12.csv"),sep=",",header=TRUE)
	myrawdata$DataDate<-as.Date(myrawdata$DataDate)
	#Asset Price Data
	asset_process<-read.table(paste0("~/Dropbox/PhD_Workshop/Input Files/Asset Prices/",underlying_asset,"_daily_processed.csv"),sep=",",header=TRUE)
	asset_process$Date<-as.Date(asset_process$Date)



	myexpr<- substitute(mydata<-mydata %>% mutate(arpe = abs(Last-price_column)/Last*100,rpe = (price_column-Last)/Last*100),list(price_column = as.name(price_column)))
	eval(myexpr)

	atmdata <- mydata %>%
				mutate(atm=abs(Moneyness-1)) %>%
				filter(Type==call_or_put) %>%
				arrange(DataDate,atm) %>%
				group_by(DataDate) 


	atmsummary<- atmdata %>%
				summarise(minDiff=min(atm))

	print("All Good So Far")
	#Join two data sets
	atm_join<-inner_join(atmdata,atmsummary)
	atm_join$DataDate<-as.Date(atm_join$DataDate)

	#Join with Option Input Data
	dupnames<-table(c(colnames(atm_join),colnames(myrawdata)))
	dupnames<-names(dupnames[dupnames>1])
	atm_join<-left_join(atm_join,myrawdata,by=dupnames)
	atm_join$RealExpiration<-as.Date(atm_join$RealExpiration)

	#Find the most ATM contracts for each day. There might be more than one (same strike price, different maturities)
	#Calculate contract price over underlying price

	myexpr<-substitute(atm_join<- atm_join %>%
					filter(atm==minDiff,RealExpiration<=max(asset_process$Date)) %>%
					mutate(priceperc=Last/UnderlyingPrice,Matbreaks=cut(NetMaturity,c(0,22,60,180,Inf),right=FALSE),Outcome=(ExpirationPayoff-Last)*ifelse(Last<=price_column,1,-1),PoL=ifelse(Outcome>0,"profit","loss")),list(price_column = as.name(price_column)))
	eval(myexpr)

	theme_opt<-theme(title=element_text(size=8),axis.title=element_text(size=8),axis.text=element_text(size=8),legend.text=element_text(size=8),legend.key.size=unit(10,"points"))

	error_date<-ggplot(data=atm_join, aes_string(x="DataDate",y=error_type)) + geom_point(aes(color=Matbreaks,shape=PoL),size=1.5) + theme(legend.position="left",legend.box = "vertical", legend.direction = "vertical") + labs(title = paste0(underlying_asset," ",data_year," ",model_specs," ",call_or_put)) + theme_opt

	if(save_graphs){
		ggsave(paste0(save_path,underlying_asset,"_",data_year,"_",model_specs,"_",call_or_put,"_",error_type,"_type_errors_from_data_date.png"),error_date,width=6, height=3)
	}

	error_expiration<-ggplot(data=atm_join, aes_string(x="RealExpiration",y=error_type)) + geom_point(aes(color=Matbreaks,shape=PoL),size=1.5) + theme(legend.position="left",legend.box = "vertical", legend.direction = "vertical") + labs(title = paste0(underlying_asset," ",data_year," ",model_specs," ",call_or_put)) + theme_opt
	if(save_graphs){
		ggsave(paste0(save_path,underlying_asset,"_",data_year,"_",model_specs,"_",call_or_put,"_",error_type,"_type_errors_from_expiration_date.png"),error_expiration,width=6, height=3)
	}

	error_maturity<-ggplot(data=atm_join, aes_string(x="NetMaturity",y=error_type)) + geom_point(aes(color=Matbreaks,shape=PoL),size=2) + theme(legend.position="top",legend.box = "horizontal", legend.direction = "horizontal") + labs(title = paste0(underlying_asset," ",data_year," ",model_specs," ",call_or_put)) + theme_opt
	if(save_graphs){
		ggsave(paste0(save_path,underlying_asset,"_",data_year,"_",model_specs,"_",call_or_put,"_",error_type,"_type_errors_from_maturity.png"),error_maturity,width=6, height=3)
	}

	error_graph<-arrangeGrob(error_date,error_expiration + theme(legend.position="none"),ncol=2)

	if(save_graphs){
		ggsave(paste0(save_path,underlying_asset,"_",data_year,"_",model_specs,"_",call_or_put,"_",error_type,"_type_errors_aggregate.png"),error_graph,width=8, height=2)
	}
	#error_graph


	################
	atm_strikes<-distinct(select(atm_join,Strike)) #Strike prices for ATM options
	ap_limited<-filter(asset_process,Date>=min(mydata$DataDate) & Date<=max(mydata$DataDate)) #Limit the asset price time interval to considered assets' contract dates
	ap_limited_exp<-filter(asset_process,Date>=min(atm_join$RealExpiration) & Date<=max(atm_join$RealExpiration)) #Limit the asset price time interval to considered assets' expirations 
	#Plots of asset price process and strike prices
	y_axis_limits<-c(min(ap_limited$Adj.Close,ap_limited_exp$Adj.Close),max(ap_limited$Adj.Close,ap_limited_exp$Adj.Close))
	#atm_process<-ggplot(data=ap_limited,aes(x=Date,y=Adj.Close)) + geom_point(data=atm_join,aes(x=DataDate, y=Strike)) + geom_abline(data=atm_strikes,aes(intercept=Strike,slope=0)) + geom_line() + scale_y_continuous(limits=y_axis_limits)
	atm_process<-ggplot(data=ap_limited,aes(x=Date,y=Adj.Close)) + geom_point(data=atm_join,aes(x=DataDate, y=Strike)) + geom_line() + theme_opt # + scale_y_continuous(limits=y_axis_limits)
	atm_process_exp<-ggplot(data=ap_limited_exp,aes(x=Date,y=Adj.Close)) + geom_point(data=atm_join,aes(x=RealExpiration, y=Strike))  + geom_line() + scale_y_continuous(limits=y_axis_limits) + theme_opt # + geom_abline(data=atm_strikes,aes(intercept=Strike,slope=0))
	#quartz()
	quartet_graph<-arrangeGrob(error_date,error_expiration+ theme(legend.position="none"),atm_process,atm_process_exp,ncol=2)
	################
	if(save_graphs){
		ggsave(paste0(save_path,underlying_asset,"_",data_year,"_",model_specs,"_",call_or_put,"_",error_type,"_type_errors_quartet.png"),quartet_graph,width=8, height=4)
	}


}
#get_atm_error_graph()
for(j in c("call","put")){
for(k in c("arpe","rpe")){
for(i in 2013:2009){
	get_atm_error_graph(results_path="~/Dropbox/PhD_Workshop/Output Files/",results_model_path="Black_Scholes/",underlying_asset="NDX",data_year=i,progressOutput=TRUE,model_specs="BS_data_withdiv_5y",call_or_put=j,error_type=k,price_column="BS.HV2y",save_graphs=TRUE,save_path="~/Dropbox/PhD_Workshop/Graphs/Error Graphs/")

}}}

for(j in c("call","put")){
for(k in c("arpe","rpe")){
for(i in 2013:2009){
	get_atm_error_graph(results_path="~/Dropbox/PhD_Workshop/Output Files/",results_model_path="Black_Scholes/",underlying_asset="NDX",data_year=i,progressOutput=TRUE,model_specs="BS_data_withdiv_2y",call_or_put=j,error_type=k,price_column="BS.HV2y",save_graphs=TRUE,save_path="~/Dropbox/PhD_Workshop/Graphs/Error Graphs/")

}}}


for(j in c("call","put")){
for(k in c("arpe","rpe")){
for(i in 2013:2009){
	get_atm_error_graph(results_path="~/Dropbox/PhD_Workshop/Output Files/",results_model_path="HestonNandi_GARCH/",underlying_asset="NDX",data_year=i,progressOutput=TRUE,model_specs="HN_data_withdiv_symm_5y",call_or_put=j,error_type=k,price_column="HN_prices",save_graphs=TRUE,save_path="~/Dropbox/PhD_Workshop/Graphs/Error Graphs/")

}}}

for(j in c("call","put")){
for(k in c("arpe","rpe")){
for(i in 2013:2009){
	get_atm_error_graph(results_path="~/Dropbox/PhD_Workshop/Output Files/",results_model_path="HestonNandi_GARCH/",underlying_asset="NDX",data_year=i,progressOutput=TRUE,model_specs="HN_data_withdiv_symm_2y",call_or_put=j,error_type=k,price_column="HN_prices",save_graphs=TRUE,save_path="~/Dropbox/PhD_Workshop/Graphs/Error Graphs/")

}}}

for(j in c("call","put")){
for(k in c("arpe","rpe")){
for(i in 2013:2009){
	get_atm_error_graph(results_path="~/Dropbox/PhD_Workshop/Output Files/",results_model_path="HestonNandi_GARCH/",underlying_asset="NDX",data_year=i,progressOutput=TRUE,model_specs="HN_data_withdiv_asym_5y",call_or_put=j,error_type=k,price_column="HN_prices",save_graphs=TRUE,save_path="~/Dropbox/PhD_Workshop/Graphs/Error Graphs/")

}}}



HestonNandi_GARCH

#I/O operations
#Model Price Estimates Data
# results_path<-"Dropbox/PhD_Workshop/Output Files/Survey_Results/"
# mydata<-read.table(paste0(results_path,"HestonNandi_GARCH/SPX_2010_HN_data_withdiv_asym_5y_012000.csv"),sep=",",header=TRUE)
results_path<-"~/Dropbox/PhD_Workshop/Output Files/"
mydata<-read.table(paste0(results_path,"Black_Scholes/SPX_2011_BS_data_withdiv_2y.csv"),sep=",",header=TRUE)
#mydata<-read.table(paste0(results_path,"HestonNandi_GARCH/SPX_2011_HN_data_withdiv_asym_5y.csv"),sep=",",header=TRUE)
mydata$DataDate<-as.Date(mydata$DataDate)
#Options Input Data
myrawdata<-read.table("~/Dropbox/PhD_Workshop/Input Files/Asset Options/SPX_2011_options_filtered_A12.csv",sep=",",header=TRUE)
myrawdata$DataDate<-as.Date(myrawdata$DataDate)
#Asset Price Data
asset_process<-read.table(paste0("~/Dropbox/PhD_Workshop/Input Files/Asset Prices/SPX_daily_processed.csv"),sep=",",header=TRUE)
asset_process$Date<-as.Date(asset_process$Date)

#Calculate ARPE and RPE
# mydata<- mydata %>%
# 			mutate(arpe = abs(Last-HN_prices)/Last*100, #Absolute Relative Percentage Error
# 					rpe = (HN_prices-Last)/Last*100) #Relative Percentage Error
mydata<- mydata %>%
			mutate(arpe = abs(Last-BS.HV2y)/Last*100, #Absolute Relative Percentage Error
					rpe = (BS.HV2y-Last)/Last*100) #Relative Percentage Error

#Calculate ATMness abs(S0/K-1), filter by type, order by date and ATMness and group by date

call_or_put<-"call"
atmdata <- mydata %>%
			mutate(atm=abs(Moneyness-1)) %>%
			filter(Type==call_or_put) %>%
			arrange(DataDate,atm) %>%
			group_by(DataDate) 

# itmdata <- mydata %>%
# 			filter(Type=="call" & Moneyness > 1.2 & Moneyness < 1.5) %>%
# 			arrange(DataDate,Moneyness,NetMaturity)
# select(itmdata,DataDate,Moneyness,NetMaturity,Last)

#Find best (minimum) ATMness of each group
atmsummary<- atmdata %>%
			summarise(minDiff=min(atm))

#Join two data sets
atm_join<-inner_join(atmdata,atmsummary)
atm_join$DataDate<-as.Date(atm_join$DataDate)

#Join with Option Input Data
dupnames<-table(c(colnames(atm_join),colnames(myrawdata)))
dupnames<-names(dupnames[dupnames>1])
atm_join<-left_join(atm_join,myrawdata,by=dupnames)
atm_join$RealExpiration<-as.Date(atm_join$RealExpiration)

#Find the most ATM contracts for each day. There might be more than one (same strike price, different maturities)
#Calculate contract price over underlying price

atm_join<- atm_join %>%
				filter(atm==minDiff,RealExpiration<=max(asset_process$Date)) %>%
				mutate(priceperc=Last/UnderlyingPrice,Matbreaks=cut(NetMaturity,c(0,22,60,180,Inf),right=FALSE),Outcome=(ExpirationPayoff-Last)*ifelse(Last<=BS.HV2y,1,-1),PoL=ifelse(Outcome>0,"profit","loss"))


arpe_date<-ggplot(data=atm_join, aes(x=DataDate,y=rpe)) + geom_point(aes(color=Matbreaks,shape=PoL),size=1.5) + theme(legend.position="left",legend.box = "vertical", legend.direction = "vertical")
arpe_expiration<-ggplot(data=atm_join, aes(x=RealExpiration,y=rpe)) + geom_point(aes(color=Matbreaks,shape=PoL),size=1.5) + theme(legend.position="none")
arpe_maturity<-ggplot(data=atm_join, aes(x=NetMaturity,y=rpe)) + geom_point(aes(color=Matbreaks,shape=PoL),size=2) + theme(legend.position="top",legend.box = "horizontal", legend.direction = "horizontal")
grid.arrange(arpe_date,arpe_expiration,ncol=2)



arpe_maturity<-ggplot(data=atm_join, aes_string(x="NetMaturity",y="rpe")) + geom_point(aes(color=as.factor(format(DataDate,"%m"))),size=1.5) + 
						theme(legend.position="left",legend.box = "vertical", legend.direction = "vertical") +
						scale_color_discrete(name="Month")



####Excess RPE P&L 
atm_money<-atm_join %>%
			filter(rpe>100) %>%
			select(Outcome,PoL)

sum(atm_money$Outcome) #find pnl



#####
#Code for logreturns
#####
ap_limited_ma<-filter(asset_process,Date>=min(mydata$DataDate-6) & Date<=max(mydata$DataDate)) #Limit the asset price time interval to considered assets' contract dates

logma5<-NULL
for(i in 1:nrow(ap_limited_ma)){
	logma5[i]<-mean(ap_limited_ma$log_returns[i:(i+4)])	
}

ap_limited_ma<-cbind(ap_limited_ma,logma5)
ap_limited_ma<-ap_limited_ma[-((nrow(ap_limited_ma)-3):nrow(ap_limited_ma)),]

ggplot(ap_limited_ma,aes(x=Date,y=logma5)) + geom_line() #plot the log returns




#hebele<-atm_join %>% mutate(Month=as.factor(format(DataDate,"%m"))) %>% select(DataDate,Month)

#as.factor(as.POSIXct(atm_join$DataDate[1])$mon

#join_g<-ggplot(data=atm_join, aes(x=RealExpiration,y=priceperc)) + geom_point(aes(color=Matbreaks,shape=PoL),size=4) + theme(legend.position="top")

################
atm_strikes<-distinct(select(atm_join,Strike)) #Strike prices for ATM options
ap_limited<-filter(asset_process,Date>=min(mydata$DataDate) & Date<=max(mydata$DataDate)) #Limit the asset price time interval to considered assets' contract dates
ap_limited_exp<-filter(asset_process,Date>=min(atm_join$RealExpiration) & Date<=max(atm_join$RealExpiration)) #Limit the asset price time interval to considered assets' expirations 
#Plots of asset price process and strike prices
y_axis_limits<-c(min(ap_limited$Adj.Close,ap_limited_exp$Adj.Close),max(ap_limited$Adj.Close,ap_limited_exp$Adj.Close))
#atm_process<-ggplot(data=ap_limited,aes(x=Date,y=Adj.Close)) + geom_point(data=atm_join,aes(x=DataDate, y=Strike)) + geom_abline(data=atm_strikes,aes(intercept=Strike,slope=0)) + geom_line() + scale_y_continuous(limits=y_axis_limits)
atm_process<-ggplot(data=ap_limited,aes(x=Date,y=Adj.Close)) + geom_point(data=atm_join,aes(x=DataDate, y=Strike)) + geom_line() # + scale_y_continuous(limits=y_axis_limits)
atm_process_exp<-ggplot(data=ap_limited_exp,aes(x=Date,y=Adj.Close)) + geom_point(data=atm_join,aes(x=RealExpiration, y=Strike))  + geom_line() + scale_y_continuous(limits=y_axis_limits)# + geom_abline(data=atm_strikes,aes(intercept=Strike,slope=0))
#quartz()
grid.arrange(arpe_date,arpe_expiration,atm_process,atm_process_exp,ncol=2)
################


#Join with Option Input Data
dupnames2<-table(c(colnames(mydata),colnames(myrawdata)))
dupnames2<-names(dupnames2[dupnames2>1])

single_contracts<- inner_join(mydata,myrawdata,by=dupnames2)
single_contracts$DataDate<-as.Date(single_contracts$DataDate)
single_contracts$RealExpiration<-as.Date(single_contracts$RealExpiration)


single_contracts<- single_contracts %>%
						arrange(OptionSymbol,DataDate) %>%
						group_by(OptionSymbol)

single_contracts_arpe<-summarise(single_contracts,arpe=mean(arpe),NMaturity=max(NetMaturity),count=n())

arrange(single_contracts_arpe,desc(count),desc(arpe))

one_contract<- single_contracts %>%
				filter(OptionSymbol=="SPX111217C01400000")


contract_price_plot<- ggplot(data=one_contract,aes(x=DataDate)) + geom_line(aes(y=Last,color="Market")) + geom_line(aes(y=HN_prices,color="model")) + 
	geom_vline(xintercept=as.numeric(one_contract$RealExpiration[1]))

ap_limited_one<-filter(asset_process,Date>=min(one_contract$DataDate) & Date<=one_contract$RealExpiration[1]) #Limit the asset price time interval to considered assets' contract dates
one_contract_y_lims<-c(min(one_contract$Strike[1],ap_limited_one$Adj.Close),max(one_contract$Strike[1],ap_limited_one$Adj.Close))
asset_process_plot<-ggplot(data=ap_limited_one,aes(x=Date,y=Adj.Close)) + geom_abline(data=one_contract,aes(intercept=Strike[1],slope=0,color="Strike")) + geom_line(aes(color="Asset Price")) + 
	scale_y_continuous(limits=one_contract_y_lims) + geom_vline(xintercept=as.numeric(one_contract$RealExpiration[1]))
arpe_one_plot<-ggplot(data=one_contract,aes(x=DataDate)) + geom_line(aes(y=arpe,color="ARPE")) + 
	geom_vline(xintercept=as.numeric(one_contract$RealExpiration[1]))
#grid.arrange(contract_price_plot,asset_process_plot,arpe_one_plot)

grid.draw(rbind(ggplotGrob(asset_process_plot),ggplotGrob(contract_price_plot), ggplotGrob(arpe_one_plot), size="first"))

######
#Some filtering stuff and then some other
# filter(atm_join,NetMaturity > 100 & priceperc<0.05)
# filter(mydata, DataDate>="2009-06-01" & DataDate<="2009-06-10" & Strike==945)
# process_g<-ggplot(data=ap_limited,aes(x=Date,y=Adj.Close)) + geom_line()
# grid.arrange(join_g,process_g)

######






################

# mysummary<-mydata[sample(1:nrow(mydata),30),]

# EVI2_veg <- ggplot() + geom_blank() + 
#     ggtitle("EVI2 for reference-data in Azraq (Jordan)") +
#     ylab("EVI2") + xlab("month") +
#     theme_bw(base_size = 12, base_family = "Times New Roman") +
#     geom_smooth(aes(x=Date, y=Vegetable_mean, ymin=Vegetable_min, 
#         ymax=Vegetable_max, color="Vegetable", fill="Vegetable"),
#         data=Grouped_Croptypes_EVI2, stat="identity") +
#     geom_line(aes(x=Date, y=Tomato, color="Tomato"), data=Sample_EVI2_A_SPOT) +
#     scale_fill_manual(name="Min-Max-Range and Mean \nof specific Croptypes",
#         values=c(Vegetable="#008B00", Tomato="#FFFFFF")) +
#     scale_color_manual(name="Min-Max-Range and Mean \nof specific Croptypes",
#         values=c(Vegetable="#008B00",Tomato="#CD4F39")) 
# EVI2_veg

# mydata %>%
# 	filter(Moneyness>3.5)



