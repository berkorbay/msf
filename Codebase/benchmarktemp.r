
if(!("party" %in% rownames(installed.packages())))
	install.packages("party")
#install.packages("party")
library(party)

########

#This is an attempt to build together a benchmarking environment

find_predictions<-function(path_info,input_path,underlying_asset,predict_year,parameter_period,method,benchmark,refresh_period="monthly",output_path,progressOutput=TRUE){
	party_rules<-ctree_control(minbucket=7)
	if(progressOutput){
		print(paste0("Starting finding error predictions of ",path_info[,"file_name"]))
	}
	market_data<-read.csv(paste0("~/Dropbox/PhD_Workshop/Input Files/Asset Options/",underlying_asset,"_",predict_year-1,"_options_filtered_A12.csv"),header=TRUE)
	if(parameter_period > 1){
		for(back_time in 2:parameter_period){
			market_data<-rbind(market_data,read.csv(paste0("~/Dropbox/PhD_Workshop/Input Files/Asset Options/",underlying_asset,"_",predict_year-back_time,"_options_filtered_A12.csv"),header=TRUE))
		}
	}
	market_data<-market_data[market_data$ExpirationPrices != -1,]
	market_data$DataDate<-as.Date(market_data$DataDate)	
	market_data$RealExpiration<-as.Date(market_data$RealExpiration)

	if(parameter_period < 1){
		stop("Parameter period should be one or more, preferably less than 5.")
	}
	training_data<-read.csv(paste0(input_path,path_info[,"model_path"],"/",underlying_asset,"_",predict_year-1,"_",path_info[,"file_name"],".csv"),header=TRUE)
	if(parameter_period > 1){
		for(back_time in 2:parameter_period){
			training_data<-rbind(training_data,read.csv(paste0(input_path,path_info[,"model_path"],"/",underlying_asset,"_",predict_year-back_time,"_",path_info[,"file_name"],".csv"),header=TRUE))
		}
	}
	training_data<-training_data[training_data$ExpirationPrices != -1,]
	training_data$DataDate<- as.Date(training_data$DataDate)	
	if(nrow(training_data) != nrow(market_data)){
		stop("Market data and training data are not compatible!!!!")
	}

	if(benchmark == "market"){
		if(method == "arpe"){
			est_error<-abs(training_data$Last-training_data[,path_info[,"price_column"]])/training_data$Last
		}else if(method == "mse"){
			est_error<-(training_data$Last-training_data[,path_info[,"price_column"]])^2
		}else{
			stop("Wrong method chosen. We have only a limited number of options.")
		}
	}else if(benchmark == "realizations"){
		if(method == "pnl"){
			est_error<- ifelse(training_data[,path_info[,"price_column"]]>market_data$Last,-1,1)*market_data$PDAbsolute
		}else{
			stop("Wrong method chosen. We have only a limited number of options.")			
		}
	}else{
		stop("Choose a proper benchmark! Either market or realizations.")
	}

	prediction_data<-read.csv(paste0(input_path,path_info[,"model_path"],"/",underlying_asset,"_",predict_year,"_",path_info[,"file_name"],".csv"),header=TRUE)
	prediction_data<-prediction_data[prediction_data$ExpirationPrices != -1,]
	prediction_data$DataDate<- as.Date(prediction_data$DataDate)	

	if(refresh_period == "yearly"){
		if(benchmark=="realizations"){
			pass_index<-which(market_data$RealExpiration < as.Date(paste0(predict_year,"-01-01")))
			est_error<-est_error[pass_index]
			training_data<-training_data[pass_index,]
		}
		cluster_data<-data.frame(ErrorValues=est_error,NetMaturity=training_data$NetMaturity,Moneyness=training_data$Moneyness)
		cluster_learn<-ctree(ErrorValues ~ NetMaturity + Moneyness, data=cluster_data, controls=party_rules)
		cluster_nodes<-predict(cluster_learn,type="node")
		write.table(cbind(training_data,cluster_data$ErrorValues,cluster_nodes),paste0(output_path,underlying_asset,"_",predict_year,"_",method,"_",benchmark,"_",path_info[,"file_name"],"_with_lookback_",parameter_period,"y_errordata.csv"),sep=",",row.names=FALSE,append=FALSE)
		node_averages<-aggregate(cluster_data$ErrorValues,by=list(cluster_nodes),"mean")
		colnames(node_averages)<-c("node","average")
		predict_nodes<-predict(cluster_learn, newdata=data.frame(NetMaturity=prediction_data$NetMaturity, Moneyness=prediction_data$Moneyness),type="node")
		predicted_errors<-node_averages[match(predict_nodes,node_averages$node),"average"]
		if(progressOutput){
			print("Done")
		}
		return(predicted_errors)
	}else if(refresh_period == "monthly"){
		predict_nodes<-rep(-1,nrow(prediction_data))
		predicted_errors<-rep(-1,nrow(prediction_data))
		for(i in 1:12){
			if(benchmark=="realizations"){
				prediction_index <- which(as.Date(ifelse(i<12,paste0(predict_year,"-",i+1,"-01"),paste0(predict_year+1,"-01-01"))) > prediction_data$DataDate & prediction_data$DataDate >= as.Date(paste0(predict_year,"-",i,"-01")))
				training_index <- which(as.Date(paste0(predict_year,"-",i,"-01")) > training_data$DataDate & training_data$DataDate >= as.Date(paste0(predict_year - parameter_period,"-",i,"-01")) & market_data$RealExpiration < as.Date(paste0(predict_year,"-",i,"-01")))
			}else{
				prediction_index <- which(as.Date(ifelse(i<12,paste0(predict_year,"-",i+1,"-01"),paste0(predict_year+1,"-01-01"))) > prediction_data$DataDate & prediction_data$DataDate >= as.Date(paste0(predict_year,"-",i,"-01")))
				training_index <- which(as.Date(paste0(predict_year,"-",i,"-01")) > training_data$DataDate & training_data$DataDate >= as.Date(paste0(predict_year - parameter_period,"-",i,"-01")))
			}
			cluster_data<-data.frame(ErrorValues=est_error[training_index],NetMaturity=training_data$NetMaturity[training_index],Moneyness=training_data$Moneyness[training_index])
			cluster_learn<-ctree(ErrorValues ~ NetMaturity + Moneyness, data=cluster_data, controls=party_rules)
			cluster_nodes<-predict(cluster_learn,type="node")
			# write.table(cbind(training_data,cluster_data$ErrorValues,cluster_nodes),paste0(output_path,underlying_asset,"_",predict_year,"_",method,"_",benchmark,"_",path_info[,"file_name"],"_with_lookback_",parameter_period,"y_errordata.csv"),sep=",",row.names=FALSE,append=FALSE)
			node_averages<-aggregate(cluster_data$ErrorValues,by=list(cluster_nodes),"mean")
			colnames(node_averages)<-c("node","average")
			predict_nodes[prediction_index]<-predict(cluster_learn, newdata=data.frame(NetMaturity=prediction_data$NetMaturity[prediction_index], Moneyness=prediction_data$Moneyness[prediction_index]),type="node")
			predicted_errors[prediction_index]<-node_averages[match(predict_nodes[prediction_index],node_averages$node),"average"]
		}
		if(any(predict_nodes < 0)){
			stop("Some months are missing in prediction")
		}
		if(progressOutput){
			print("Done")
		}
		return(predicted_errors)
	}else{
		stop("Refresh period is not chosen properly.")
	}

}

table_benchmark_summary<-function(pnl_data,contenders){
	benchmark_summary<-rep(0,nrow(contenders)+2)
	names(benchmark_summary)<-c("selection",contenders[,"file_name"],"market_long")
	#Long Calls
	pnl_filter<-pnl_data[pnl_data$Type == "call",]
	benchmark_summary<-rbind(benchmark_summary,"Long Calls" = c(colSums((pnl_filter[,c("selection",contenders[,"file_name"])]+pnl_filter[,"PDAbsolute"]) != 0),nrow(pnl_filter)))
	#Short Calls
	benchmark_summary<-rbind(benchmark_summary,"Short Calls" = c(colSums((pnl_filter[,c("selection",contenders[,"file_name"])]+pnl_filter[,"PDAbsolute"]) == 0),0))		
	#Long Puts
	pnl_filter<-pnl_data[pnl_data$Type == "put",]
	benchmark_summary<-rbind(benchmark_summary,"Long Puts" = c(colSums((pnl_filter[,c("selection",contenders[,"file_name"])]+pnl_filter[,"PDAbsolute"]) != 0),nrow(pnl_filter)))
	#Short Puts
	benchmark_summary<-rbind(benchmark_summary,"Short Puts" = c(colSums((pnl_filter[,c("selection",contenders[,"file_name"])]+pnl_filter[,"PDAbsolute"]) == 0),0))		
	#Total Longs
	benchmark_summary<-rbind(benchmark_summary,"Total Longs" = colSums(benchmark_summary[c("Long Calls","Long Puts"),]))
	#Total Shorts
	benchmark_summary<-rbind(benchmark_summary,"Total Shorts" = colSums(benchmark_summary[c("Short Calls","Short Puts"),]))
	#Total Long Investment
	benchmark_summary<-rbind(benchmark_summary,"Total Long Investment" = c(colSums(pnl_data$Last*((pnl_data[,c("selection",contenders[,"file_name"])]+pnl_data[,"PDAbsolute"]) != 0)),sum(pnl_data$Last)))
	#Total Short Capital
	benchmark_summary<-rbind(benchmark_summary,"Total Short Capital" = c(colSums(pnl_data$Last*((pnl_data[,c("selection",contenders[,"file_name"])]+pnl_data[,"PDAbsolute"]) == 0)),0))
	#Total Nonnegative Contracts
	benchmark_summary<-rbind(benchmark_summary,"Total Nonnegative Contracts" = colSums(pnl_data[,c("selection",contenders[,"file_name"],"market_long")] >= 0))
	#Total Profit
	benchmark_summary<-rbind(benchmark_summary,"Total Profit" = colSums((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] >= 0)*(pnl_data[,c("selection",contenders[,"file_name"],"market_long")])))
	#Total Negative Contracts
	benchmark_summary<-rbind(benchmark_summary,"Total Negative Contracts" = colSums(pnl_data[,c("selection",contenders[,"file_name"],"market_long")] < 0))
	#Total Loss
	benchmark_summary<-rbind(benchmark_summary,"Total Loss" = colSums((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] < 0)*(pnl_data[,c("selection",contenders[,"file_name"],"market_long")])))
	#PnL
	benchmark_summary<-rbind(benchmark_summary,"PnL" = colSums(benchmark_summary[c("Total Profit","Total Loss"),]))
	#Total Consensus
	benchmark_summary<-rbind(benchmark_summary,"Total Consensus" = c(rep(sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1),nrow(contenders)+1),sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1 & pnl_data[,"market_long"] >= 0)))
	#Total Wins
	benchmark_summary<-rbind(benchmark_summary,
		"Total Wins" = colSums((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] >= 0) & (rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) < 1 & rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) > 0)))
	benchmark_summary["Total Wins","market_long"]<-benchmark_summary["Total Wins","market_long"] + sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0 & pnl_data[,"market_long"] >= 0)
	#Total Losses
	benchmark_summary<-rbind(benchmark_summary,
		"Total Losses" = colSums((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] < 0) & (rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) < 1 & rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) > 0)))
	benchmark_summary["Total Losses","market_long"] <- benchmark_summary["Total Losses","market_long"] + sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1 & pnl_data[,"market_long"] < 0)
	#Total Doom
	benchmark_summary<-rbind(benchmark_summary,"Total Doom" = c(rep(sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0),nrow(contenders)+1),sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0 & pnl_data[,"market_long"] < 0))) 
	#Total Consensus Dollars
	benchmark_summary<-rbind(benchmark_summary,"Total Consensus Dollars" = c(rep(sum((rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1)*pnl_data[,"selection"]),nrow(contenders)+1),sum(pnl_data[,"market_long"]*(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1 & pnl_data[,"market_long"] >= 0))))
	#Total Wins Dollars
	benchmark_summary<-rbind(benchmark_summary,
		"Total Wins Dollars" = colSums(pnl_data[,c("selection",contenders[,"file_name"],"market_long")]*((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] >= 0) & (rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) < 1 & rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) > 0))))
	benchmark_summary["Total Wins Dollars","market_long"]<-benchmark_summary["Total Wins Dollars","market_long"] + sum(pnl_data[,"market_long"]*(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0 & pnl_data[,"market_long"] >= 0))
	#Total Losses
	benchmark_summary<-rbind(benchmark_summary,
		"Total Losses Dollars" = colSums(pnl_data[,c("selection",contenders[,"file_name"],"market_long")]*((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] < 0) & (rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) < 1 & rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) > 0))))
	benchmark_summary["Total Losses Dollars","market_long"]<-benchmark_summary["Total Losses Dollars","market_long"] + sum(pnl_data[,"market_long"]*(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1 & pnl_data[,"market_long"] < 0))
	#Total Doom Dollars
	benchmark_summary<-rbind(benchmark_summary,"Total Doom Dollars" = c(rep(sum((rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0)*pnl_data[,"selection"]),nrow(contenders)+1),sum(pnl_data[,"market_long"]*(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0 & pnl_data[,"market_long"] < 0))))

	return(benchmark_summary[-1,])
}

benchmark_mondrian<-function(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="arpe",benchmark="market",underlying_asset,predict_year=2013,parameter_period=2,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE){
	if(length(seeds) != nrow(contenders)){
		stop("Seeds are not equal to contenders")
	}
	
	market_prices<-read.csv(paste0("~/Dropbox/PhD_Workshop/Input Files/Asset Options/",underlying_asset,"_",predict_year,"_options_filtered_A12.csv"),header=TRUE)

	if(progressOutput){
		print("Predicting Errors")
	}


	for(i in 1:nrow(contenders)){
		set.seed(seeds[i])
		if(i == 1){
			error_sets<-find_predictions(path_info=contenders[i,1:3] , input_path=input_path,underlying_asset=underlying_asset,predict_year=predict_year,parameter_period=parameter_period,method=method,benchmark=benchmark,refresh_period=refresh_period,output_path=output_path,progressOutput=progressOutput)
			price_file<-read.csv(paste0(input_path,contenders[i,"model_path"],"/",underlying_asset,"_",predict_year,"_",contenders[i,"file_name"],".csv"),header=TRUE)
			price_info<-price_file[,contenders[i,"price_column"]]
		}
		else{
			error_sets<-cbind(error_sets,find_predictions(path_info=contenders[i,1:3] , input_path=input_path,underlying_asset=underlying_asset,predict_year=predict_year,parameter_period=parameter_period,method=method,benchmark=benchmark,refresh_period=refresh_period,output_path=output_path,progressOutput=progressOutput))
			price_file<-read.csv(paste0(input_path,contenders[i,"model_path"],"/",underlying_asset,"_",predict_year,"_",contenders[i,"file_name"],".csv"),header=TRUE)
			price_info<-cbind(price_info,price_file[,contenders[i,"price_column"]])
		}
	}

	colnames(error_sets)<-contenders[,"file_name"]

	if(progressOutput){
		print("Writing Error Data")
	}

	write.table(error_sets,paste0(output_path,underlying_asset,"_",predict_year,"_",method,"_with_",benchmark,"_",parameter_period,"y_lookback_errors.csv"),sep=",",row.names=FALSE,append=FALSE)

	price_info<-price_info[market_prices$ExpirationPrices != -1,]
	selections<-max.col(-error_sets)
	selections_prices<-rep(0,length(selections))
	for(i in 1:nrow(contenders)){
		selections_prices[selections==i]<-price_info[selections==i,i]
	}

	market_prices<-market_prices[market_prices$ExpirationPrices != -1,]

	price_info<-cbind(selections_prices,price_info,market_prices$Last)
	colnames(price_info)<-c("selection",contenders[,"file_name"],"market_prices")

	if(progressOutput){
		print("Writing Price Data")
	}

	write.table(cbind(market_prices[,c("UnderlyingPrice","OptionSymbol","Type","DataDate","Expiration","RealExpiration","Moneyness","NetMaturity","Strike","ExpirationPrices","Last","ExpirationPayoff","PDAbsolute")],
		price_info),paste0(output_path,underlying_asset,"_",predict_year,"_",method,"_with_",benchmark,"_",parameter_period,"y_lookback_",refresh_period,"_refresh_price_info.csv"),sep=",",row.names=FALSE,append=FALSE)

	# return(price_info)

	pnl_info<-ifelse(price_info[,"selection"]>market_prices$Last,1,-1)*market_prices$PDAbsolute
	for(i in 1:nrow(contenders)){
		pnl_info<-cbind(pnl_info,ifelse(price_info[,i+1]>market_prices$Last,1,-1)*market_prices$PDAbsolute)
	}
	pnl_info<-cbind(pnl_info,market_prices$PDAbsolute)
	colnames(pnl_info)<-c("selection",contenders[,"file_name"],"market_long")

	if(progressOutput){
		print("Writing PnL Data")
	}

	pnl_data<-cbind(market_prices[,c("UnderlyingPrice","OptionSymbol","Type","DataDate","Expiration","RealExpiration","Moneyness","NetMaturity","Strike","ExpirationPrices","Last","ExpirationPayoff","PDAbsolute")],
		pnl_info)
	write.table(pnl_data,paste0(output_path,underlying_asset,"_",predict_year,"_",method,"_with_",benchmark,"_",parameter_period,"y_lookback_",refresh_period,"_refresh_pnl.csv"),sep=",",row.names=FALSE,append=FALSE)

	if(progressOutput){
		print("Writing Benchmark Summary")
	}
	write.table(table_benchmark_summary(pnl_data,contenders),paste0(input_path,"Benchmarks/Summaries/",underlying_asset,"_",predict_year,"_",method,"_with_",benchmark,"_",parameter_period,"y_lookback_",refresh_period,"_refresh_benchmark_summary.csv"),sep=",",row.names=TRUE,col.names=NA,append=FALSE)

	return(pnl_data)

}



# pnl_tots<-NULL
for(prediction.date in 2013:2010){
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="mse",benchmark="market",underlying_asset="NDX",predict_year=prediction.date,parameter_period=1,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="mse",benchmark="market",underlying_asset="NDX",predict_year=prediction.date,parameter_period=2,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="mse",benchmark="market",underlying_asset="NDX",predict_year=prediction.date,parameter_period=1,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="mse",benchmark="market",underlying_asset="NDX",predict_year=prediction.date,parameter_period=2,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	# pnl_tots<-rbind(pnl_tots,cbind(prediction.date,pnl_vals))
	#print(colSums(pnl_vals))
}

for(prediction.date in 2013:2010){
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="arpe",benchmark="market",underlying_asset="NDX",predict_year=prediction.date,parameter_period=1,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="arpe",benchmark="market",underlying_asset="NDX",predict_year=prediction.date,parameter_period=2,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="arpe",benchmark="market",underlying_asset="NDX",predict_year=prediction.date,parameter_period=1,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="arpe",benchmark="market",underlying_asset="NDX",predict_year=prediction.date,parameter_period=2,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	# pnl_tots<-rbind(pnl_tots,cbind(prediction.date,pnl_vals))
	#print(colSums(pnl_vals))
}

for(prediction.date in 2013:2010){
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="pnl",benchmark="realizations",underlying_asset="NDX",predict_year=prediction.date,parameter_period=1,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="pnl",benchmark="realizations",underlying_asset="NDX",predict_year=prediction.date,parameter_period=2,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="pnl",benchmark="realizations",underlying_asset="NDX",predict_year=prediction.date,parameter_period=1,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="pnl",benchmark="realizations",underlying_asset="NDX",predict_year=prediction.date,parameter_period=2,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
	# pnl_tots<-rbind(pnl_tots,cbind(prediction.date,pnl_vals))
	#print(colSums(pnl_vals))
}


# print(colSums(pnl_tots[,-1]))


pnl_summaries<-function(only_read=FALSE,year_from=2013,year_to=2011,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="pnl",benchmark="realizations",underlying_asset="SPX",parameter_period=1,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE){

	#Calculate PnLs
	pnl_tots<-NULL
	for(prediction.date in year_from:year_to){
		if(only_read){
			pnl_vals<-read.csv(paste0(output_path,underlying_asset,"_",prediction.date,"_",method,"_with_",benchmark,"_",parameter_period,"y_lookback_",refresh_period,"_refresh_benchmark_summary.csv"),header=TRUE)
			pnl_tots<-pnl_tots + pnl_vals
		}else{
			if(progressOutput){
				print(paste0("Starting ",prediction.date," benchmarks."))
			}
			pnl_vals<-benchmark_mondrian(contenders,seeds,input_path=input_path, method=method,benchmark=benchmark,underlying_asset=underlying_asset,predict_year=prediction.date,parameter_period=parameter_period,refresh_period=refresh_period,output_path=output_path,progressOutput=progressOutput)			
			pnl_tots<-rbind(pnl_tots,pnl_vals)
		}
	}

	if(progressOutput){
		print("Getting Aggregate Summmary")
	}

	write.table(table_benchmark_summary(pnl_tots,contenders),paste0(input_path,"Benchmarks/Summaries/",underlying_asset,"_from_",year_from,"_to_",year_to,"_",method,"_with_",benchmark,"_",parameter_period,"y_lookback_",refresh_period,"_refresh_benchmark_summary_aggregate.csv"),sep=",",row.names=TRUE,col.names=NA,append=FALSE)

	# #Get Summaries
	# for(prediction.date in year_from:year_to){
	# 	pnl_data<-read.csv(paste0(output_path,underlying_asset,prediction.date,"_",method,"_with_",benchmark,"_",parameter_period,"y_lookback_",refresh_period,"_refresh_pnl.csv"),header=TRUE)
	# 	#Long Calls
	# 	pnl_filter<-pnl_data[pnl_data$Type == "call",]
	# 	benchmark_summary<-rbind(benchmark_summary,"Long Calls" = c(colSums((pnl_filter[,c("selection",contenders[,"file_name"])]+pnl_filter[,"PDAbsolute"]) != 0),nrow(pnl_filter)))
	# 	#Short Calls
	# 	benchmark_summary<-rbind(benchmark_summary,"Short Calls" = c(colSums((pnl_filter[,c("selection",contenders[,"file_name"])]+pnl_filter[,"PDAbsolute"]) == 0),0))		
	# 	#Long Puts
	# 	pnl_filter<-pnl_data[pnl_data$Type == "put",]
	# 	benchmark_summary<-rbind(benchmark_summary,"Long Puts" = c(colSums((pnl_filter[,c("selection",contenders[,"file_name"])]+pnl_filter[,"PDAbsolute"]) != 0),nrow(pnl_filter)))
	# 	#Short Puts
	# 	benchmark_summary<-rbind(benchmark_summary,"Short Puts" = c(colSums((pnl_filter[,c("selection",contenders[,"file_name"])]+pnl_filter[,"PDAbsolute"]) == 0),0))		
	# 	#Total Longs
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Longs" = colSums(benchmark_summary[c("Long Calls","Long Puts"),]))
	# 	#Total Shorts
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Shorts" = colSums(benchmark_summary[c("Short Calls","Short Puts"),]))
	# 	#Total Long Investment
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Long Investment" = c(colSums(pnl_data$Last*((pnl_data[,c("selection",contenders[,"file_name"])]+pnl_data[,"PDAbsolute"]) != 0)),sum(pnl_data$Last)))
	# 	#Total Short Capital
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Short Capital" = c(colSums(pnl_data$Last*((pnl_data[,c("selection",contenders[,"file_name"])]+pnl_data[,"PDAbsolute"]) == 0)),0))
	# 	#Total Nonnegative Contracts
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Nonnegative Contracts" = colSums(pnl_data[,c("selection",contenders[,"file_name"],"market_long")] >= 0))
	# 	#Total Profit
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Profit" = colSums((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] >= 0)*(pnl_data[,c("selection",contenders[,"file_name"],"market_long")])))
	# 	#Total Negative Contracts
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Negative Contracts" = colSums(pnl_data[,c("selection",contenders[,"file_name"],"market_long")] < 0))
	# 	#Total Loss
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Loss" = colSums((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] < 0)*(pnl_data[,c("selection",contenders[,"file_name"],"market_long")])))
	# 	benchmark_summary<-benchmark_summary[c(2:nrow(benchmark_summary),1),]
	# 	#Total Consensus
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Consensus" = c(rep(sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1),nrow(contenders)+1),sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1 & pnl_data[,"market_long"] >= 0)))
	# 	#Total Wins
	# 	benchmark_summary<-rbind(benchmark_summary,
	# 		"Total Wins" = colSums((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] >= 0) & (rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) < 1 & rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) > 0)))
	# 	benchmark_summary["Total Wins","market_long"]<-benchmark_summary["Total Wins","market_long"] + sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0 & pnl_data[,"market_long"] >= 0)
	# 	#Total Losses
	# 	benchmark_summary<-rbind(benchmark_summary,
	# 		"Total Losses" = colSums((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] < 0) & (rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) < 1 & rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) > 0)))
	# 	benchmark_summary["Total Losses","market_long"]<-benchmark_summary["Total Losses","market_long"] + sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1 & pnl_data[,"market_long"] < 0)
	# 	#Total Doom
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Doom" = c(rep(sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0),nrow(contenders)+1),sum(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0 & pnl_data[,"market_long"] < 0)))

	# 	#Total Consensus Dollars
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Consensus Dollars" = c(rep(sum((rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1)*pnl_data[,"selection"]),nrow(contenders)+1),sum(pnl_data[,"market_long"]*(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1 & pnl_data[,"market_long"] >= 0))))
	# 	#Total Wins Dollars
	# 	benchmark_summary<-rbind(benchmark_summary,
	# 		"Total Wins Dollars" = colSums(pnl_data[,c("selection",contenders[,"file_name"],"market_long")]*((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] >= 0) & (rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) < 1 & rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) > 0))))
	# 	benchmark_summary["Total Wins Dollars","market_long"]<-benchmark_summary["Total Wins Dollars","market_long"] + sum(pnl_data[,"market_long"]*(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0 & pnl_data[,"market_long"] >= 0))
	# 	#Total Losses
	# 	benchmark_summary<-rbind(benchmark_summary,
	# 		"Total Losses Dollars" = colSums(pnl_data[,c("selection",contenders[,"file_name"],"market_long")]*((pnl_data[,c("selection",contenders[,"file_name"],"market_long")] < 0) & (rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) < 1 & rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) > 0))))
	# 	benchmark_summary["Total Losses Dollars","market_long"]<-benchmark_summary["Total Losses Dollars","market_long"] + sum(pnl_data[,"market_long"]*(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 1 & pnl_data[,"market_long"] < 0))
	# 	#Total Doom Dollars
	# 	benchmark_summary<-rbind(benchmark_summary,"Total Doom Dollars" = c(rep(sum((rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0)*pnl_data[,"selection"]),nrow(contenders)+1),sum(pnl_data[,"market_long"]*(rowSums(pnl_data[,contenders[,"file_name"]] >= 0)/nrow(contenders) == 0 & pnl_data[,"market_long"] < 0))))


	# }


}


# contenders<-data.frame(model_path=c("HestonNandi_GARCH","HestonNandi_GARCH","HestonNandi_GARCH","Black_Scholes","Black_Scholes","GHYP_Europt","GHYP_Europt","GHYP_Europt","GHYP_Europt","GHYP_Europt","GHYP_Europt","GHYP_Europt","GHYP_Europt"),
# 					   file_name=c("HN_data_withdiv_symm_2y","HN_data_withdiv_symm_5y","HN_data_withdiv_asym_5y","BS_data_withdiv_2y","BS_data_withdiv_5y",
# 					   	"Levy_GHYP_data_withdiv_Esscher_50000_iteration_2y_asymmetric","Levy_GHYP_data_withdiv_MCMM_50000_iteration_2y_asymmetric","Levy_GHYP_data_withdiv_Esscher_50000_iteration_2y_symmetric","Levy_GHYP_data_withdiv_MCMM_50000_iteration_2y_symmetric",
# 					   	"Levy_GHYP_data_withdiv_Esscher_50000_iteration_5y_asymmetric","Levy_GHYP_data_withdiv_MCMM_50000_iteration_5y_asymmetric","Levy_GHYP_data_withdiv_Esscher_50000_iteration_5y_symmetric","Levy_GHYP_data_withdiv_MCMM_50000_iteration_5y_symmetric"),
# 					   price_column=c("HN_prices","HN_prices","HN_prices","BS.HV2y","BS.HV2y","LE_prices","LM_prices","LE_prices","LM_prices","LE_prices","LM_prices","LE_prices","LM_prices"),
# 					   stringsAsFactors=FALSE)

# contenders<-data.frame(model_path=c("HestonNandi_GARCH","HestonNandi_GARCH","HestonNandi_GARCH","HestonNandi_GARCH","HestonNandi_GARCH","HestonNandi_GARCH"),
# 					   	file_name=c("HN_data_withdiv_asym_5y_000000","HN_data_withdiv_asym_5y_002000","HN_data_withdiv_asym_5y_010000","HN_data_withdiv_asym_5y_012000","HN_data_withdiv_asym_5y_020000","HN_data_withdiv_asym_5y_022000"),
# 					   	price_column=c("HN_prices","HN_prices","HN_prices","HN_prices","HN_prices","HN_prices"),
# 					   	stringsAsFactors=FALSE)


contenders<-data.frame(model_path=c("HestonNandi_GARCH","HestonNandi_GARCH","HestonNandi_GARCH","Black_Scholes","Black_Scholes"),
					   file_name=c("HN_data_withdiv_symm_2y","HN_data_withdiv_symm_5y","HN_data_withdiv_asym_5y","BS_data_withdiv_2y","BS_data_withdiv_5y"),
   					   price_column=c("HN_prices","HN_prices","HN_prices","BS.HV2y","BS.HV2y"),
					   stringsAsFactors=FALSE)


#seeds<-c(7061414,4111447,515153,3504592,959323,872692,489137,506416,977659,927887,327129,274429,129331)
seeds<-c(7061414,4111447,515153,3504592,959323)

for(par_per in 1:2){
	for(ref_per in c("monthly","yearly")){
		for(met_name in c("pnl","arpe","mse")){

			pnl_summaries(only_read=FALSE,year_from=2013,year_to=2010,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method=met_name,benchmark=ifelse(met_name=="pnl","realizations","market"),underlying_asset="NDX",parameter_period=par_per,refresh_period=ref_per,output_path="~/Dropbox/PhD_Workshop/Output Files/Survey_Results/Benchmarks/Mondrian/",progressOutput=TRUE)

			pnl_summaries(only_read=FALSE,year_from=2013,year_to=2011,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method=met_name,benchmark=ifelse(met_name=="pnl","realizations","market"),underlying_asset="NDX",parameter_period=par_per,refresh_period=ref_per,output_path="~/Dropbox/PhD_Workshop/Output Files/Survey_Results/Benchmarks/Mondrian/",progressOutput=TRUE)


		}
	}
}


pnl_summaries(only_read=FALSE,year_from=2013,year_to=2010,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="pnl",benchmark="realizations",underlying_asset="NDX",parameter_period=1,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Survey_Results/Benchmarks/Mondrian/",progressOutput=TRUE)

pnl_summaries(only_read=FALSE,year_from=2013,year_to=2011,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="pnl",benchmark="realizations",underlying_asset="NDX",parameter_period=1,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE)

pnl_summaries(only_read=FALSE,year_from=2013,year_to=2010,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="pnl",benchmark="realizations",underlying_asset="NDX",parameter_period=2,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Survey_Results/Benchmarks/Mondrian/",progressOutput=TRUE)

pnl_summaries(only_read=FALSE,year_from=2013,year_to=2011,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="pnl",benchmark="realizations",underlying_asset="NDX",parameter_period=2,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE)

pnl_summaries(only_read=FALSE,year_from=2013,year_to=2010,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="arpe",benchmark="market",underlying_asset="NDX",parameter_period=1,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE)

pnl_summaries(only_read=FALSE,year_from=2013,year_to=2011,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="arpe",benchmark="market",underlying_asset="NDX",parameter_period=1,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE)

pnl_summaries(only_read=FALSE,year_from=2013,year_to=2010,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="arpe",benchmark="market",underlying_asset="NDX",parameter_period=2,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE)

pnl_summaries(only_read=FALSE,year_from=2013,year_to=2011,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="arpe",benchmark="market",underlying_asset="NDX",parameter_period=2,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE)

pnl_summaries(only_read=FALSE,year_from=2013,year_to=2010,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="mse",benchmark="market",underlying_asset="NDX",parameter_period=1,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE)

pnl_summaries(only_read=FALSE,year_from=2013,year_to=2011,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="mse",benchmark="market",underlying_asset="NDX",parameter_period=2,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE)

pnl_summaries(only_read=FALSE,year_from=2013,year_to=2010,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="mse",benchmark="market",underlying_asset="NDX",parameter_period=1,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE)

pnl_summaries(only_read=FALSE,year_from=2013,year_to=2011,contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="mse",benchmark="market",underlying_asset="NDX",parameter_period=2,refresh_period="yearly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/",progressOutput=TRUE)


# pnl_2013<-benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="pnl",benchmark="realizations",underlying_asset="SPX",predict_year=2013,parameter_period=2,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
# pnl_2012<-benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="mse",benchmark="market",underlying_asset="SPX",predict_year=2012,parameter_period=2,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")
# pnl_2011<-benchmark_mondrian(contenders,seeds,input_path="~/Dropbox/PhD_Workshop/Output Files/", method="mse",benchmark="market",underlying_asset="SPX",predict_year=2011,parameter_period=2,refresh_period="monthly",output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/")

# colSums(pnl_2013)
# colSums(pnl_2012)
# colSums(pnl_2011)

# colSums(pnl_2013) + colSums(pnl_2012) + colSums(pnl_2011)

########


# #This is the temporary code for classification and benchmarking of models
# data_path<-"~/Dropbox/PhD_Workshop/Output Files/"
# #data_path<-"~/Dropbox/PhD_Workshop/Output Files/"

# #Set the asset name
# underlying_asset<-"SPX"

# #Set the data year to be predicted
# data_year<-2013

# #Set random seed for replication
# set.seed(7061414)

# #Import training data from the previous two years
# hn_training<-read.csv(paste0(data_path,"HestonNandi_GARCH/",underlying_asset,"_",data_year-1,"_HN_data.csv"),header=TRUE)
# hn_training<-rbind(hn_training,read.csv(paste0(data_path,"HestonNandi_GARCH/",underlying_asset,"_",data_year-2,"_HN_data.csv"),header=TRUE))
# #120874 rows including far future options

# #Calculate ARPE from the market prices
# hn_arpe<-abs(hn_training$Last-hn_training$HN_prices)/hn_training$Last

# #Calculate AE from expiration payoff. Formula might be wrong
# #hn_arpe<-abs(hn_training$ExpirationPayoff-hn_training$HN_prices)

# #Set Conditional Inference Tree rules 
# party_rules<-ctree_control(minbucket=0.005*nrow(hn_training))
# #The rules below are for demonstration only
# #party_rules<-ctree_control(minbucket=0.20*nrow(hn_training),maxdepth=2)

# #Compose the data frame required for classification
# hn_required<-data.frame(arpe=hn_arpe,NetMaturity=hn_training$NetMaturity,Moneyness=hn_training$Moneyness)
# #Use the conditional inference tree to create nodes
# hn_classification<-ctree(arpe ~ NetMaturity + Moneyness, data=hn_required, controls=party_rules)
# #Add nodes to the data frame
# hn_required<-data.frame(hn_required,nodes=predict(hn_classification,type="node"))
# #Calculate node averages
# hn_node_averages<-aggregate(hn_required$arpe,by=list(hn_required$nodes),"mean")
# colnames(hn_node_averages)<-c("node","average")
# #Get the prediction year's data
# hn_predict<-read.csv(paste0(data_path,"HestonNandi_GARCH/",underlying_asset,"_",data_year,"_HN_data.csv"),header=TRUE)
# #Assign nodes for different maturity-moneyness combinations
# hn_predict_nodes<-predict(hn_classification, newdata=data.frame(NetMaturity=hn_predict$NetMaturity, Moneyness=hn_predict$Moneyness),type="node")
# #Predict error performance of the new nodes with the previous data
# hn_predict_performance<-hn_node_averages[match(hn_predict_nodes,hn_node_averages$node),"average"]

# #Repeat for
# #BS
# set.seed(4141607)

# bs_training<-read.csv(paste0(data_path,"Black_Scholes/",underlying_asset,"_",data_year-1,"_BS_data.csv"),header=TRUE)
# bs_training<-rbind(bs_training,read.csv(paste0(data_path,"Black_Scholes/",underlying_asset,"_",data_year-2,"_BS_data.csv"),header=TRUE))
# #120874 rows including far future options
# party_rules<-ctree_control(minbucket=0.005*nrow(bs_training))
# #party_rules<-ctree_control(minbucket=0.2*nrow(bs_training),maxdepth=2)

# bs_arpe<-abs(bs_training$Last-bs_training$BS.HV2y)/bs_training$Last

# #bs_arpe<-abs(bs_training$ExpirationPayoff-bs_training$BS.HV2y)

# bs_required<-data.frame(arpe=bs_arpe,NetMaturity=bs_training$NetMaturity,Moneyness=bs_training$Moneyness)
# bs_classification<-ctree(arpe ~ NetMaturity + Moneyness, data=bs_required)

# bs_required<-data.frame(bs_required,nodes=predict(bs_classification,type="node"))
# bs_node_averages<-aggregate(bs_required$arpe,by=list(bs_required$nodes),"mean")
# colnames(bs_node_averages)<-c("node","average")

# bs_predict<-read.csv(paste0(data_path,"Black_Scholes/",underlying_asset,"_",data_year,"_BS_data.csv"),header=TRUE)

# bs_predict_nodes<-predict(bs_classification, newdata=data.frame(NetMaturity=bs_predict$NetMaturity, Moneyness=bs_predict$Moneyness),type="node")
# bs_predict_performance<-bs_node_averages[match(bs_predict_nodes,bs_node_averages$node),"average"]

# HN_or_BS<-ifelse(max.col(cbind(-hn_predict_performance,-bs_predict_performance))==1,"HN","BS")
# BetPrices<-rep(0,length(HN_or_BS))
# BetPrices[HN_or_BS=="HN"]<-hn_predict$HN_prices[HN_or_BS=="HN"]
# BetPrices[HN_or_BS=="BS"]<-bs_predict$BS.HV2y[HN_or_BS=="BS"]

# option_data<-read.csv(paste0("~/Dropbox/PhD_Workshop/Input Files/Asset Options/SPX_",data_year,"_options_filtered_A12.csv"),header=TRUE)

# #ifelse(option_data$Last<BetPrices,1,-1)*option_data$PDAbsolute

# final_df<-data.frame(OptionSymbol=option_data$OptionSymbol, Type=option_data$Type, UnderlyingPrice=option_data$UnderlyingPrice, RealExpiration=option_data$RealExpiration, NetMaturity=option_data$NetMaturity,
# 	Strike=option_data$Strike, Last=option_data$Last, ExpirationPrices=option_data$ExpirationPrices, Moneyness=option_data$Moneyness , ExpirationPayoff=option_data$ExpirationPayoff,
# 	PDAbsolute=option_data$PDAbsolute, HN_prices=hn_predict$HN_prices, HN_payoff=ifelse(option_data$Last<hn_predict$HN_prices,1,-1)*option_data$PDAbsolute, BS_prices=bs_predict$BS.HV2y,
# 	BS_payoff=ifelse(option_data$Last<bs_predict$BS.HV2y,1,-1)*option_data$PDAbsolute, HN_or_BS=HN_or_BS, Bet_prices=BetPrices, Bet_payoff=ifelse(option_data$Last<BetPrices,1,-1)*option_data$PDAbsolute
# 	 )

# final_df<-final_df[(final_df$ExpirationPrices != -1),]

# write.table(final_df,paste0(data_path,"Benchmarks/",underlying_asset,"_",data_year,"_HN_vs_BS_benchmark_2710_marketarpe.csv"),row.names=FALSE,sep=",")

# #####

# nrow(final_df)
# sum(final_df$HN_payoff>=0 & final_df$BS_payoff>=0)
# sum(final_df$HN_payoff<0 & final_df$BS_payoff>=0)
# sum(final_df$HN_payoff>=0 & final_df$BS_payoff<0)
# sum(final_df$HN_payoff<0 & final_df$BS_payoff<0)

# sum(final_df$HN_payoff[final_df$HN_payoff>=0 & final_df$BS_payoff>=0])
# sum(final_df$HN_payoff[final_df$HN_payoff<0 & final_df$BS_payoff>=0])
# sum(final_df$HN_payoff[final_df$HN_payoff>=0 & final_df$BS_payoff<0])
# sum(final_df$HN_payoff[final_df$HN_payoff<0 & final_df$BS_payoff<0])

# sum(final_df$BS_payoff[final_df$HN_payoff>=0 & final_df$BS_payoff>=0])
# sum(final_df$BS_payoff[final_df$HN_payoff<0 & final_df$BS_payoff>=0])
# sum(final_df$BS_payoff[final_df$HN_payoff>=0 & final_df$BS_payoff<0])
# sum(final_df$BS_payoff[final_df$HN_payoff<0 & final_df$BS_payoff<0])

# sum(final_df$Bet_payoff[final_df$Bet_payoff >= 0 & final_df$HN_payoff>=0 & final_df$BS_payoff>=0])
# sum(final_df$Bet_payoff[final_df$Bet_payoff >= 0 & final_df$HN_payoff<0 & final_df$BS_payoff>=0])
# sum(final_df$Bet_payoff[final_df$Bet_payoff >= 0 & final_df$HN_payoff>=0 & final_df$BS_payoff<0])
# sum(final_df$Bet_payoff[final_df$Bet_payoff >= 0 & final_df$HN_payoff<0 & final_df$BS_payoff<0])

# sum(final_df$Bet_payoff[final_df$Bet_payoff < 0 & final_df$HN_payoff>=0 & final_df$BS_payoff>=0])
# sum(final_df$Bet_payoff[final_df$Bet_payoff < 0 & final_df$HN_payoff<0 & final_df$BS_payoff>=0])
# sum(final_df$Bet_payoff[final_df$Bet_payoff < 0 & final_df$HN_payoff>=0 & final_df$BS_payoff<0])
# sum(final_df$Bet_payoff[final_df$Bet_payoff < 0 & final_df$HN_payoff<0 & final_df$BS_payoff<0])

# #####

# benchmark_table<-data.frame(Heston_Nandi=numeric(),Black_Scholes=numeric(),Model_Selection=numeric(),All_Long=numeric())

# #Long Calls
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_prices >= final_df$Last & final_df$Type == "call"),
# 	sum(final_df$BS_prices >= final_df$Last & final_df$Type == "call"),
# 	sum(final_df$Bet_prices >= final_df$Last & final_df$Type == "call"),
# 	sum(final_df$Type == "call")
# 	)
# 	)

# #Short Calls
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_prices < final_df$Last & final_df$Type == "call"),
# 	sum(final_df$BS_prices < final_df$Last & final_df$Type == "call"),
# 	sum(final_df$Bet_prices < final_df$Last & final_df$Type == "call"),
# 	0
# 	)
# 	)

# #Long Puts
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_prices >= final_df$Last & final_df$Type == "put"),
# 	sum(final_df$BS_prices >= final_df$Last & final_df$Type == "put"),
# 	sum(final_df$Bet_prices >= final_df$Last & final_df$Type == "put"),
# 	sum(final_df$Type == "put")
# 	)
# 	)

# #Short Puts
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_prices < final_df$Last & final_df$Type == "put"),
# 	sum(final_df$BS_prices < final_df$Last & final_df$Type == "put"),
# 	sum(final_df$Bet_prices < final_df$Last & final_df$Type == "put"),
# 	0
# 	)
# 	)

# #Total Longs
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_prices >= final_df$Last),
# 	sum(final_df$BS_prices >= final_df$Last),
# 	sum(final_df$Bet_prices >= final_df$Last),
# 	nrow(final_df)
# 	)
# 	)

# #Total Long Capital
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$Last[final_df$HN_prices >= final_df$Last]),
# 	sum(final_df$Last[final_df$BS_prices >= final_df$Last]),
# 	sum(final_df$Last[final_df$Bet_prices >= final_df$Last]),
# 	sum(final_df$Last)
# 	)
# 	)

# #Total Shorts
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_prices < final_df$Last),
# 	sum(final_df$BS_prices < final_df$Last),
# 	sum(final_df$Bet_prices < final_df$Last),
# 	0
# 	)
# 	)

# #Total Short Capital
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$Last[final_df$HN_prices < final_df$Last]),
# 	sum(final_df$Last[final_df$BS_prices < final_df$Last]),
# 	sum(final_df$Last[final_df$Bet_prices < final_df$Last]),
# 	0
# 	)
# 	)

# #Total positive contracts
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_payoff>=0),
# 	sum(final_df$BS_payoff>=0),
# 	sum(final_df$Bet_payoff>=0),
# 	sum(final_df$PDAbsolute>=0)
# 	)
# 	)

# #Total negative contracts
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_payoff<0),
# 	sum(final_df$BS_payoff<0),
# 	sum(final_df$Bet_payoff<0),
# 	sum(final_df$PDAbsolute<0)
# 	)
# 	)

# #Total profit 
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_payoff[final_df$HN_payoff>=0]),
# 	sum(final_df$BS_payoff[final_df$BS_payoff>=0]),
# 	sum(final_df$Bet_payoff[final_df$Bet_payoff>=0]),
# 	sum(final_df$PDAbsolute[final_df$PDAbsolute>=0])
# 	)
# 	)

# #Total loss 
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_payoff[final_df$HN_payoff<0]),
# 	sum(final_df$BS_payoff[final_df$BS_payoff<0]),
# 	sum(final_df$Bet_payoff[final_df$Bet_payoff<0]),
# 	sum(final_df$PDAbsolute[final_df$PDAbsolute<0])
# 	)
# 	)

# #P&L (Balance)
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_payoff[final_df$HN_payoff>=0]) + sum(final_df$HN_payoff[final_df$HN_payoff<0]),
# 	sum(final_df$BS_payoff[final_df$BS_payoff>=0]) + sum(final_df$BS_payoff[final_df$BS_payoff<0]),
# 	sum(final_df$Bet_payoff[final_df$Bet_payoff>=0]) + sum(final_df$Bet_payoff[final_df$Bet_payoff<0]),
# 	sum(final_df$PDAbsolute[final_df$PDAbsolute>=0]) + sum(final_df$PDAbsolute[final_df$PDAbsolute<0])
# 	)
# 	)

# #Wins - Contract
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_payoff>=0 & final_df$BS_payoff<0),
# 	sum(final_df$HN_payoff<0 & final_df$BS_payoff>=0),
# 	sum(final_df$Bet_payoff >= 0 & final_df$HN_payoff<0 & final_df$BS_payoff>=0) + sum(final_df$Bet_payoff >= 0 & final_df$HN_payoff>=0 & final_df$BS_payoff<0),
# 	NA
# 	)
# 	)

# #Losses - Contract
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_payoff<0 & final_df$BS_payoff>=0),
# 	sum(final_df$HN_payoff>=0 & final_df$BS_payoff<0),
# 	sum(final_df$Bet_payoff < 0 & final_df$HN_payoff<0 & final_df$BS_payoff>=0) + sum(final_df$Bet_payoff < 0 & final_df$HN_payoff>=0 & final_df$BS_payoff<0),
# 	NA
# 	)
# 	)

# #Wins - Dollars
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_payoff[final_df$HN_payoff>=0 & final_df$BS_payoff<0]),
# 	sum(final_df$BS_payoff[final_df$HN_payoff<0 & final_df$BS_payoff>=0]),
# 	sum(final_df$Bet_payoff[final_df$Bet_payoff >= 0 & final_df$HN_payoff<0 & final_df$BS_payoff>=0]) + sum(final_df$Bet_payoff[final_df$Bet_payoff >= 0 & final_df$HN_payoff>=0 & final_df$BS_payoff<0]),
# 	NA
# 	)
# 	)

# #Losses - Dollars
# benchmark_table<-rbind(benchmark_table,c(
# 	sum(final_df$HN_payoff[final_df$HN_payoff<0 & final_df$BS_payoff>=0]),
# 	sum(final_df$BS_payoff[final_df$HN_payoff>=0 & final_df$BS_payoff<0]),
# 	sum(final_df$Bet_payoff[final_df$Bet_payoff < 0 & final_df$HN_payoff<0 & final_df$BS_payoff>=0]) + sum(final_df$Bet_payoff[final_df$Bet_payoff < 0 & final_df$HN_payoff>=0 & final_df$BS_payoff<0]),
# 	NA
# 	)
# 	)

# colnames(benchmark_table)<-c("Heston Nandi", "Black Scholes", "Model Selection", "All Long")
# rownames(benchmark_table)<-c(
# 	"Long Calls - Contracts",
# 	"Short Calls - Contracts",
# 	"Long Puts - Contracts",
# 	"Short Puts - Contracts",
# 	"Total Longs - Contracts",
# 	"Total Longs - Dollars",
# 	"Total Shorts - Contracts",
# 	"Total Shorts - Dollars",
# 	"Total Nonnegative - Contracts",
# 	"Total Negative - Contracts",
# 	"Total Profit - Dollars",
# 	"Total Loss - Dollars",
# 	"PnL (Balance) - Dollars",
# 	"Wins - Contracts",
# 	"Losses - Contracts",
# 	"Wins - Dollars",
# 	"Losses - Dollars"
# 	)


# write.table(benchmark_table,paste0(data_path,"Benchmarks/",underlying_asset,"_",data_year,"_HN_vs_BS_benchmark_2710_marketarpe_summary.csv"),row.names=TRUE,sep=",")
# #####

# ####

# ggplot(bs_required,aes(x=NetMaturity,y=Moneyness,fill=arpe))+geom_tile()


# qplot(x=bs_required$NetMaturity,y=bs_required$Moneyness,geom="tile",fill=bs_required$arpe)

# ##


# hn_dectree<-tree(arpe ~ NetMaturity + Moneyness,data=hn_required)

# plot(hn_dectree)



########
### Plot P&L distribution
########

if(!("ggplot2" %in% rownames(installed.packages())))
	install.packages("ggplot2")

library(ggplot2)

a2013_results<-read.table("~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/SPX_2013_pnl_with_realizations_1y_lookback_monthly_refresh_pnl.csv",header=TRUE,sep=",")
a2012_results<-read.table("~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/SPX_2012_pnl_with_realizations_1y_lookback_monthly_refresh_pnl.csv",header=TRUE,sep=",")
a2011_results<-read.table("~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/SPX_2011_pnl_with_realizations_1y_lookback_monthly_refresh_pnl.csv",header=TRUE,sep=",")
a2010_results<-read.table("~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/SPX_2010_pnl_with_realizations_1y_lookback_monthly_refresh_pnl.csv",header=TRUE,sep=",")
# HN_data_withdiv_asym_5y
select_density<-density(c(a2013_results$selection,a2012_results$selection,a2011_results$selection,a2010_results$selection))

select_density_selection<-density(c(a2013_results$selection,a2012_results$selection,a2011_results$selection,a2010_results$selection))
select_density_HN_A_5y<-density(c(a2013_results$HN_data_withdiv_asym_5y,a2012_results$HN_data_withdiv_asym_5y,a2011_results$HN_data_withdiv_asym_5y,a2010_results$HN_data_withdiv_asym_5y))
select_density_BS_5y<-density(c(a2013_results$BS_data_withdiv_5y,a2012_results$BS_data_withdiv_5y,a2011_results$BS_data_withdiv_5y,a2010_results$BS_data_withdiv_5y))
select_density_Levy_S_M_2y<-density(c(a2013_results$Levy_GHYP_data_withdiv_MCMM_50000_iteration_2y_symmetric,a2012_results$Levy_GHYP_data_withdiv_MCMM_50000_iteration_2y_symmetric,a2011_results$Levy_GHYP_data_withdiv_MCMM_50000_iteration_2y_symmetric,a2010_results$Levy_GHYP_data_withdiv_MCMM_50000_iteration_2y_symmetric))
select_density_marketlong<-density(c(a2013_results$market_long,a2012_results$market_long,a2011_results$market_long,a2010_results$market_long))

my_x<-seq(min(c(select_density_selection$x,select_density_HN_A_5y$x,select_density_BS_5y$x,select_density_Levy_S_M_2y$x)),max(c(select_density_selection$x,select_density_HN_A_5y$x,select_density_BS_5y$x,select_density_Levy_S_M_2y$x)),length.out=1000)

my_x<-seq(-50,50,length.out=1000)

selection_y<-approx(select_density_selection$x,select_density_selection$y,my_x)$y
HN_A_5y_y<-approx(select_density_HN_A_5y$x,select_density_HN_A_5y$y,my_x)$y
BS_5y_y<-approx(select_density_BS_5y$x,select_density_BS_5y$y,my_x)$y
Levy_S_M_2y_y<-approx(select_density_Levy_S_M_2y$x,select_density_Levy_S_M_2y$y,my_x)$y
marketlong_y<-approx(select_density_marketlong$x,select_density_marketlong$y,my_x)$y


benchy<-data.frame(pnl=my_x,selection=selection_y,HN_A_5y=HN_A_5y_y,BS_5y=BS_5y_y,Levy_M_S_2y=Levy_S_M_2y_y,marketlong=marketlong_y)

pl2<-ggplot(data=benchy,aes(x=pnl)) + geom_line(aes(y=selection_y, color="selection")) + geom_line(aes(y=HN_A_5y, color="HN_A_5y")) + geom_line(aes(y=Levy_M_S_2y, color="Levy_M_S_2y")) + geom_line(aes(y=BS_5y, color="BS_5y"))+ geom_line(aes(y=marketlong, color="marketlong")) + labs(color = "Models") + xlab("Net Profit") + ylab("Density") #+ geom_line(aes(y=ghyp.def, color="GHYP.def"))


ggsave("~/Dropbox/PhD_Workshop/Reports/Progress Report 3 - 20150120/images/pnldensities_models_large.png",pl2,width=8,height=5)

plot(select_density)
#select_density<-density(some_results$selection)
my_x<-seq(-100,100,length.out=1000)
my_y<-approx(marketlong_y$x,marketlong_y$y,my_x)$y

plot(my_x,my_y)