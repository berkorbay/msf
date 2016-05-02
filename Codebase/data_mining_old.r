#This is the code for data mining pricing errors

## These codes are to calculate rankings of 2015

#### WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
#### THIS CODE IS EXTREMELY FRAGILE AND POSSIBLY WILL NOT WORK ON YOUR COMPUTER
#### BLAME THE CREATORS OF R twitteR package

rm(list=ls(all=TRUE)) #remove all items in the working environment
gc() #garbage collection. clears space and memory.
options(repos="http://cran.rstudio.com/") #Set default repo as RStudio
options(dplyr.width = Inf) #See all dplyr data table columns
options(scipen = 7) #decimal display limit
options("httr_oauth_cache"=TRUE) #TwitteR cache option
options(browser="/usr/bin/open -a '/applications/Google Chrome.app'") #Default Browser as Chrome
options(java.parameters = "-Xmx8g") #For xlsx package java memory allowance to 8gb
#Remove predetermined seed if exists
if(exists(".Random.seed")) #If there is a seed, remove it
	rm(.Random.seed, envir=globalenv())


#Install the required packages
if(!("dplyr" %in% rownames(installed.packages())))
	install.packages("dplyr") #dplyr for data manipulation

if(!("ggplot2" %in% rownames(installed.packages())))
	install.packages("ggplot2") #ggplot2 for charts

if(!("reshape2" %in% rownames(installed.packages())))
	install.packages("reshape2") #reshape2 for cast and melt functions

if(!("png" %in% rownames(installed.packages())))
	install.packages("png") #png for adding logos to ggplot2 graphs

if(!("gridExtra" %in% rownames(installed.packages())))
	install.packages("gridExtra") #gridExtra for binding multiple plot elements

if(!("xlsx" %in% rownames(installed.packages())))
	install.packages("xlsx") #xlsx for xlsx inputs and outputs

if(!("readxl" %in% rownames(installed.packages())))
	install.packages("readxl") #readxl for faster xlsx inputs

if(!("e1071" %in% rownames(installed.packages())))
	install.packages("e1071") #e1071 for data mining algorithms

if(!("party" %in% rownames(installed.packages())))
	install.packages("party") #party for conditional inference trees

if(!("partykit" %in% rownames(installed.packages())))
	install.packages("partykit") #party on steroids?

if(!("Formula" %in% rownames(installed.packages())))
	install.packages("Formula") #Formula for some formula notation

if(!("rpart" %in% rownames(installed.packages())))
	install.packages("rpart")

if(!("lazyeval" %in% rownames(installed.packages())))
	install.packages("lazyeval") #lasy eval for non standard expressions

if(!("NbClust" %in% rownames(installed.packages())))
	install.packages("NbClust") #NbClust for automatic optimal k-means cluster size detection

if(!("RColorBrewer" %in% rownames(installed.packages())))
	install.packages("RColorBrewer")

if(!("scales" %in% rownames(installed.packages())))
	install.packages("scales") 

if(!("stringr" %in% rownames(installed.packages())))
	install.packages("stringr") 

library(dplyr)
library(ggplot2)
library(reshape2)
library(png)
library(gridExtra)
library(xlsx)
library(readxl)
library(e1071)
library(party)
library(partykit)
library(Formula)
library(rpart)
library(lazyeval)
library(NbClust)
library(RColorBrewer)
library(scales)
library(stringr)


use_seeds<-TRUE #Whether to use the predetermined seeds

take_section<-function(training_start_date,training_end_date,prediction_end_date,data_file,local_input_path,local_input_file,output_path,section_code,price_column,
						moneyness_limits=c(-Inf,Inf),maturity_limits=c(-Inf,Inf),progressOutput=TRUE){

	section_data<-read.table(paste0(local_input_path,local_input_file),sep=",",header=TRUE)
	section_data$DataDate<-as.Date(section_data$DataDate)
	section_data <- section_data %>% 
						filter(DataDate<=prediction_end_date & DataDate>=training_start_date) %>% 
						filter(((Type=="call" & Last > UnderlyingPrice - Strike) | (Type=="put" & Last < UnderlyingPrice + Strike))) %>% #basic arbitrage conditions 
						filter(Moneyness >= moneyness_limits[1] & Moneyness <= moneyness_limits[2] & NetMaturity >= maturity_limits[1] & NetMaturity <= maturity_limits[2]) %>%
						mutate(DataType=ifelse(DataDate>training_end_date,"Prediction","Training")) %>%
						select(DataDate,OptionSymbol,Type,UnderlyingPrice,Strike,Last,Moneyness,NetMaturity,ExpirationPrices,ExpirationPayoff,ModelEstimate=contains(price_column),DataType) %>%
						mutate(ARPE_market=abs(ModelEstimate-Last)/Last,RPE_market=(ModelEstimate-Last)/Last,MSE_market=(ModelEstimate-Last)^2) %>%
						arrange(DataDate,Type,OptionSymbol)

	write.table(data.frame(section_data),paste0(output_path,section_code,"_",local_input_file),sep=",",row.names=FALSE,append = FALSE)
}

svm_learn<-function(input_data,response,covariates,random_seed_needed=use_seeds){
	if(random_seed_needed){
		set.seed(1437764655)
	}

	training_data<-filter(input_data,DataType=="Training")
	svm_formula<- as.formula(paste0(response," ~ ",paste(covariates,collapse=" + ")))
	svm_model<-svm(svm_formula,training_data)
	svm_predict<-predict(svm_model,input_data)
	svm_predict

}

kmeans_learn<-function(input_data,response,covariates,random_seed_needed=use_seeds){

	if(random_seed_needed){
		set.seed(1437764655)
	}


	dots<-lapply(c(response,covariates),as.symbol)
	training_data<- input_data %>% 
						filter(DataType=="Training") %>% 
						select_(.dots=dots) %>%
						as.matrix()


	number_of_clusters<-NbClust(training_data,min.nc = 9,max.nc=15,method="kmeans",index="kl")$Best.nc[1]
	kmeans_model<-kmeans(training_data,number_of_clusters,nstart=25,iter.max=10^6)

	prediction_data<- input_data %>% 
						select_(.dots=dots) %>%
						as.matrix()


	prediction_distances<-as.matrix(dist(rbind(kmeans_model$centers,prediction_data)))[-(1:number_of_clusters),(1:number_of_clusters)]

#	prediction_distances<-as.matrix(dist(rbind(kmeans_model$centers[,-1],prediction_data[,-1])))[-(1:number_of_clusters),(1:number_of_clusters)]
	kmeans_predict<-kmeans_model$centers[apply(prediction_distances,1,which.min),response]

	kmeans_predict
}

manual_learn<-function(input_data,response,covariates,breaks,random_seed_needed=use_seeds){

	if(random_seed_needed){
		set.seed(1437764655)
	}

	for(i in 1:length(covariates)){
		if(i!=1){
			break_data<-cbind(break_data,cut(unlist(input_data[,covariates[i]]),breaks[[covariates[i]]],right=FALSE))
		}else{
			break_data<-data.frame(cut(unlist(input_data[,covariates[i]]),breaks[[covariates[i]]],right=FALSE))
		}
	}					
	colnames(break_data)<-paste0(covariates,"_break")

	input_data<-cbind(input_data,break_data)

	dots<-lapply(colnames(break_data),as.symbol)
	cluster_averages <- input_data %>% 
							filter(DataType=="Training") %>%
							group_by_(.dots=dots) %>%
							summarise_each(funs(mean),matches(response)) %>%
							rename_(.dots=setNames(response,"ErrorEstimate"))

	manual_predict <- input_data %>% 
							left_join(.,cluster_averages,by=c(paste0(covariates,"_break"))) %>%
							select(ErrorEstimate) %>%
							unlist()

	names(manual_predict)<-NULL
	manual_predict

}

cit_learn<-function(input_data,response,covariates,random_seed_needed=use_seeds){

	if(random_seed_needed){
		set.seed(1437764655)
	}

	cit_formula<- as.formula(paste0(response," ~ ",paste(covariates,collapse=" + ")))
	training_data <- filter(input_data,DataType=="Training")
	cit_model<-ctree(cit_formula, data=training_data)
	cit_predict<-predict(cit_model,newdata=input_data)

	cit_predict
}


dtree_learn<-function(input_data,response,covariates,random_seed_needed=use_seeds){

	if(random_seed_needed){
		set.seed(1437764655)
	}

	dtree_formula<- as.formula(paste0(response," ~ ",paste(covariates,collapse=" + ")))
	training_data <- filter(input_data,DataType=="Training")
	dtree_model<-rpart(dtree_formula, data=training_data)
	dtree_predict<-predict(dtree_model,newdata=input_data)

	dtree_predict
}

analyze_section<-function(local_input_path,section_code,local_input_file,output_path,covariates=c("Moneyness","NetMaturity"),number_of_clusters=9,progressOutput=TRUE){

	analysis_data<-read.table(paste0(local_input_path,section_code,"_",local_input_file),sep=",",header=TRUE)
	analysis_data$DataDate<-as.Date(analysis_data$DataDate)

	if(progressOutput){
		print("Starting with puts.")
	}

	if(progressOutput){
		print("Starting SVM.")
	}


	prediction_data_put <- analysis_data %>% 
								filter(Type=="put") %>%
								cbind(.,SVM_ARPE=svm_learn(.,"ARPE_market",covariates)) %>%
								cbind(.,SVM_RPE=svm_learn(.,"RPE_market",covariates)) %>%
								cbind(.,SVM_MSE=svm_learn(.,"MSE_market",covariates)) %>%
								tbl_df()

	if(progressOutput){
		print("Starting KMeans.")
	}

	prediction_data_put<- prediction_data_put %>%
								cbind(.,kmeans_ARPE=kmeans_learn(.,"ARPE_market",covariates)) %>%
								cbind(.,kmeans_RPE=kmeans_learn(.,"RPE_market",covariates)) %>%
								cbind(.,kmeans_MSE=kmeans_learn(.,"MSE_market",covariates)) %>%
								tbl_df()



	if(progressOutput){
		print("Starting Manual partitioning 1.")
	}

	manual_name<-"manual1"
	manual_covariate_breaks<-list(Moneyness=c(0.89,0.94, 0.97, 1.00, 1.03, 1.06,1.11),NetMaturity=c(0,42,126,253))


	prediction_data_put<- prediction_data_put %>%
								cbind(.,manual_ARPE=manual_learn(.,"ARPE_market",covariates,manual_covariate_breaks)) %>%
								cbind(.,manual_RPE=manual_learn(.,"RPE_market",covariates,manual_covariate_breaks)) %>%
								cbind(.,manual_MSE=manual_learn(.,"MSE_market",covariates,manual_covariate_breaks)) %>%
								rename_(.dots=setNames(paste0("manual_",c("ARPE","RPE","MSE")),paste0(manual_name,"_",c("ARPE","RPE","MSE")))) %>%
								tbl_df()

	if(progressOutput){
		print("Starting Manual partitioning 2.")
	}

	manual_name<-"manual2"
	manual_covariate_breaks<-list(Moneyness=c(0.89,0.92, 0.95, 0.98, 1.02, 1.05, 1.08,1.11),NetMaturity=c(0,50,100,253))


	prediction_data_put<- prediction_data_put %>%
								cbind(.,manual_ARPE=manual_learn(.,"ARPE_market",covariates,manual_covariate_breaks)) %>%
								cbind(.,manual_RPE=manual_learn(.,"RPE_market",covariates,manual_covariate_breaks)) %>%
								cbind(.,manual_MSE=manual_learn(.,"MSE_market",covariates,manual_covariate_breaks)) %>%
								rename_(.dots=setNames(paste0("manual_",c("ARPE","RPE","MSE")),paste0(manual_name,"_",c("ARPE","RPE","MSE")))) %>%
								tbl_df()


	if(progressOutput){
		print("Starting CIT.")
	}


	prediction_data_put<- prediction_data_put %>%
								cbind(.,cit_ARPE=cit_learn(.,"ARPE_market",covariates)) %>%
								cbind(.,cit_RPE=cit_learn(.,"RPE_market",covariates)) %>%
								cbind(.,cit_MSE=cit_learn(.,"MSE_market",covariates)) %>%
								tbl_df()

	if(progressOutput){
		print("Starting Decision Trees.")
	}

	prediction_data_put<- prediction_data_put %>%
								cbind(.,dtree_ARPE=dtree_learn(.,"ARPE_market",covariates)) %>%
								cbind(.,dtree_RPE=dtree_learn(.,"RPE_market",covariates)) %>%
								cbind(.,dtree_MSE=dtree_learn(.,"MSE_market",covariates)) %>%
								tbl_df()


	#Now calls

	if(progressOutput){
		print("Starting with calls.")
	}

	if(progressOutput){
		print("Starting SVM.")
	}


	prediction_data_call <- analysis_data %>% 
								filter(Type=="call") %>%
								cbind(.,SVM_ARPE=svm_learn(.,"ARPE_market",covariates)) %>%
								cbind(.,SVM_RPE=svm_learn(.,"RPE_market",covariates)) %>%
								cbind(.,SVM_MSE=svm_learn(.,"MSE_market",covariates)) %>%
								tbl_df()

	if(progressOutput){
		print("Starting KMeans.")
	}

	prediction_data_call<- prediction_data_call %>%
								cbind(.,kmeans_ARPE=kmeans_learn(.,"ARPE_market",covariates)) %>%
								cbind(.,kmeans_RPE=kmeans_learn(.,"RPE_market",covariates)) %>%
								cbind(.,kmeans_MSE=kmeans_learn(.,"MSE_market",covariates)) %>%
								tbl_df()


	if(progressOutput){
		print("Starting Manual partitioning 1.")
	}

	manual_name<-"manual1"
	manual_covariate_breaks<-list(Moneyness=c(0.89,0.94, 0.97, 1.00, 1.03, 1.06,1.11),NetMaturity=c(0,42,126,253))


	prediction_data_call<- prediction_data_call %>%
								cbind(.,manual_ARPE=manual_learn(.,"ARPE_market",covariates,manual_covariate_breaks)) %>%
								cbind(.,manual_RPE=manual_learn(.,"RPE_market",covariates,manual_covariate_breaks)) %>%
								cbind(.,manual_MSE=manual_learn(.,"MSE_market",covariates,manual_covariate_breaks)) %>%
								rename_(.dots=setNames(paste0("manual_",c("ARPE","RPE","MSE")),paste0(manual_name,"_",c("ARPE","RPE","MSE")))) %>%
								tbl_df()

	if(progressOutput){
		print("Starting Manual partitioning 2.")
	}

	manual_name<-"manual2"
	manual_covariate_breaks<-list(Moneyness=c(0.89,0.92, 0.95, 0.98, 1.02, 1.05, 1.08,1.11),NetMaturity=c(0,50,100,253))


	prediction_data_call<- prediction_data_call %>%
								cbind(.,manual_ARPE=manual_learn(.,"ARPE_market",covariates,manual_covariate_breaks)) %>%
								cbind(.,manual_RPE=manual_learn(.,"RPE_market",covariates,manual_covariate_breaks)) %>%
								cbind(.,manual_MSE=manual_learn(.,"MSE_market",covariates,manual_covariate_breaks)) %>%
								rename_(.dots=setNames(paste0("manual_",c("ARPE","RPE","MSE")),paste0(manual_name,"_",c("ARPE","RPE","MSE")))) %>%
								tbl_df()



	if(progressOutput){
		print("Starting CIT.")
	}

	prediction_data_call<- prediction_data_call %>%
								cbind(.,cit_ARPE=cit_learn(.,"ARPE_market",covariates)) %>%
								cbind(.,cit_RPE=cit_learn(.,"RPE_market",covariates)) %>%
								cbind(.,cit_MSE=cit_learn(.,"MSE_market",covariates)) %>%
								tbl_df()

	if(progressOutput){
		print("Starting Decision Trees.")
	}

	prediction_data_call<- prediction_data_call %>%
								cbind(.,dtree_ARPE=dtree_learn(.,"ARPE_market",covariates)) %>%
								cbind(.,dtree_RPE=dtree_learn(.,"RPE_market",covariates)) %>%
								cbind(.,dtree_MSE=dtree_learn(.,"MSE_market",covariates)) %>%
								tbl_df()

	if(progressOutput){
		print("Bind call and puts and write to table.")
	}


	prediction_data <- prediction_data_put %>%
							rbind(.,prediction_data_call) %>%
							filter(rowSums(is.na(.))==0) %>%
							arrange(DataDate,Type,OptionSymbol) %>%
							as.data.frame()

	write.table(prediction_data,paste0(output_path,section_code,"_",gsub(".csv","",local_input_file),"_predictions.csv"),sep=",",row.names=FALSE,append=FALSE)

}

analyze_errors<-function(local_input_path,section_code,local_input_file,output_path,error_types=c("ARPE","RPE","MSE"),progressOutput=TRUE){

	if(progressOutput){
		print("Starting with the errors.")
	}

	prediction_data<-read.table(paste0(local_input_path,section_code,"_",gsub(".csv","",local_input_file),"_predictions.csv"),sep=",",header=TRUE)
	prediction_data$DataDate<-as.Date(prediction_data$DataDate)

	# prediction_data[,"ARPE_market"][prediction_data[,"ARPE_market"]==0]<-min(prediction_data[,"ARPE_market"][prediction_data[,"ARPE_market"]>0])
	# prediction_data[,"MSE_market"][prediction_data[,"MSE_market"]==0]<-min(prediction_data[,"MSE_market"][prediction_data[,"MSE_market"]>0])
	# prediction_data[,"RPE_market"][prediction_data[,"RPE_market"]==0]<-prediction_data[,"RPE_market"][which.min(abs(prediction_data[,"RPE_market"][prediction_data[,"RPE_market"]!=0]))]

	error_data <- prediction_data


	for(i in 1:length(error_types)){
		error_data <- error_data %>% 
						select(-contains(paste0("_",error_types[i])))
	}
	error_data<-tbl_df(error_data)

	if(progressOutput){
		print("Calculating errors.")
	}


	for(i in 1:ncol(prediction_data)){
		for(j in 1:length(error_types)){
			if(length(grep(paste0("_",error_types[j]),colnames(prediction_data)[i]))>0){
				error_data <- prediction_data %>% 
								transmute_(PredictionError=interp(~ abs((x-y)/x),.values=list(x=as.name(colnames(prediction_data)[i]),y=as.name(paste0(error_types[j],"_market"))))) %>% 
								cbind(error_data,.) %>%
								rename_(.dots=setNames("PredictionError",paste0(colnames(prediction_data)[i],"_errors"))) %>%
								tbl_df()
			}
		}		
	}

	if(progressOutput){
		print("Writing error table.")
	}

	write.table(error_data,paste0(output_path,section_code,"_",gsub(".csv","",local_input_file),"_errors.csv"),sep=",",row.names=FALSE,append=FALSE)

}

error_summarization<-function(error_input_path,error_input_file,section_code,progressOutput=TRUE){

	if(progressOutput){
		print("Starting with the error summarization.")
	}


	error_data<-read.table(paste0(error_input_path,section_code,"_",gsub(".csv","",error_input_file),"_errors.csv"),sep=",",header=TRUE)

	error_summary<-error_data %>% select(DataType,contains("errors")) %>% group_by(DataType) %>% summarise_each(funs(mean,median)) %>% slice(2:1) %>% 
					 select(-DataType) %>% t() %>% as.data.frame()

	colnames(error_summary)<-c("Training","Prediction")

	error_types<-data.frame(rname=rownames(error_summary)) %>% 
		mutate(error_type=ifelse(grepl("MSE",rname),"MSE",ifelse(grepl("ARPE",rname),"ARPE","RPE")),mean_median=ifelse(grepl("median",rname),"median","mean")) 
	error_summary<-cbind(error_summary,error_types) %>% 
		group_by(error_type,mean_median) %>% mutate(Training_rank=rank(Training),Prediction_rank=rank(Prediction))
	error_summary$rname <- as.character(error_summary$rname)
	error_summary<-error_summary %>% rowwise() %>% mutate(model_name=substr(rname,1,regexpr("_",rname)[1]-1)) %>% select(-rname) %>% cbind(select(error_types,rname),.)



	if(progressOutput){
		print("Writing error summary table.")
	}


	write.xlsx2(data.frame(error_summary),path.expand(paste0(error_input_path,section_code,"_",gsub(".csv","",error_input_file),"_error_summary.xlsx")),sep=",",row.names=FALSE,append=FALSE)




}


full_suite_analysis<-function(take_action=TRUE,analyze_action=TRUE,error_action=TRUE,summarize_action=TRUE,training_start,training_end,prediction_end,
								local_input_path,local_input_file,section_output_path,analysis_output_path,section_code,price_column,
								moneyness_limits,maturity_limits,covariates=c("Moneyness","NetMaturity"),error_types=c("ARPE","RPE","MSE"),progressOutput=TRUE){

	if(progressOutput){
		print(local_input_file)
	}


	if(take_action){
		if(progressOutput){
			print("Proceeding to Data Sectioning.")
		}

		 
		take_section(training_start_date=training_start,training_end_date=training_end,prediction_end_date=prediction_end,
					local_input_path=local_input_path,
					local_input_file=local_input_file,
					output_path=section_output_path,
					section_code=section_code,
					price_column=price_column,
					moneyness_limits=moneyness_limits,
					maturity_limits=maturity_limits,
					progressOutput=TRUE)

		if(progressOutput){
			print("Completed Data Sectioning.")
		}

	}

	if(analyze_action){
		if(progressOutput){
			print("Proceeding to analysis.")
		}

		analyze_section(local_input_path=section_output_path,
						section_code=section_code,
						local_input_file=local_input_file,
						output_path=analysis_output_path,
						covariates=covariates,
						progressOutput=TRUE)

		if(progressOutput){
			print("Completed analysis.")
		}

	}


	if(error_action){

		if(progressOutput){
			print("Proceeding to errors.")
		}

		analyze_errors(local_input_path=analysis_output_path,
						section_code=section_code,
						local_input_file=local_input_file,
						output_path=analysis_output_path,
						error_types=error_types,
						progressOutput=TRUE)		

		if(progressOutput){
			print("Completed errors.")
		}


	}

	if(summarize_action){

		if(progressOutput){
			print("Proceeding to error summarization.")
		}

		error_summarization(error_input_path=analysis_output_path,
						error_input_file=local_input_file,
						section_code=section_code,
						progressOutput=TRUE)		

		if(progressOutput){
			print("Completed error summarization.")
		}


	}


	if(progressOutput){
		print("All Done.")
		print("============")
	}


}

# full_suite_analysis(take_action=TRUE,analyze_action=TRUE,error_action=TRUE,
# 			training_start="2011-04-01",training_end="2011-04-30",prediction_end="2011-05-10",
# 			local_input_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",
# 			local_input_file="SPX_2011_HN_data_withdiv_asym_5y.csv",
# 			section_output_path="~/Dropbox/PhD_Workshop/DataMining/RawInputs/",
# 			analysis_output_path="~/Dropbox/PhD_Workshop/DataMining/Analyzed/",
# 			section_code="AA001",
# 			price_column="HN_prices",
# 			covariates=c("Moneyness","NetMaturity"),
# 			error_types=c("ARPE","RPE","MSE"),
# 			progressOutput=TRUE)

# full_suite_analysis(take_action=TRUE,analyze_action=TRUE,error_action=TRUE,
# 			training_start="2013-07-01",training_end="2013-07-30",prediction_end="2013-08-09",
# 			local_input_path="~/Dropbox/PhD_Workshop/Output Files/HestonNandi_GARCH/",
# 			local_input_file="SPX_2013_HN_data_withdiv_asym_5y.csv",
# 			section_output_path="~/Dropbox/PhD_Workshop/DataMining/RawInputs/",
# 			analysis_output_path="~/Dropbox/PhD_Workshop/DataMining/Analyzed/",
# 			section_code="AA001",
# 			price_column="HN_prices",
# 			covariates=c("Moneyness","NetMaturity"),
# 			error_types=c("ARPE","RPE","MSE"),
# 			progressOutput=TRUE)


input_table<-read_excel("~/Dropbox/PhD_Workshop/DataMining/to_be_analyzed.xlsx") %>% filter(grepl("AA",section_code))
input_table$training_start<-as.Date(input_table$training_start)
input_table$training_end<-as.Date(input_table$training_end)
input_table$prediction_end<-as.Date(input_table$prediction_end)

for(i in 1:nrow(input_table)){

	print(paste0("Starting set ",i," of ",nrow(input_table)))
	timer_start<-Sys.time()

	full_suite_analysis(take_action=TRUE,analyze_action=TRUE,error_action=TRUE,summarize_action=TRUE,
			training_start=input_table$training_start[i],training_end=input_table$training_end[i],prediction_end=input_table$prediction_end[i],
			local_input_path=input_table$local_input_path[i],
			local_input_file=input_table$local_input_file[i],
			section_output_path="~/Dropbox/PhD_Workshop/DataMining/RawInputs/",
			analysis_output_path="~/Dropbox/PhD_Workshop/DataMining/Analyzed/",
			section_code=input_table$section_code[i],
			price_column=input_table$price_column[i],
			moneyness_limits=c(0.9,1.1),
			maturity_limits=c(6,252),
			covariates=c("Moneyness","NetMaturity"),
			error_types=c("ARPE","RPE","MSE"),
			progressOutput=TRUE)

	print(paste0("Elapsed time for this set is ",Sys.time()-timer_start))

}


input_table<-read_excel("~/Dropbox/PhD_Workshop/DataMining/to_be_analyzed.xlsx") %>% filter(grepl("AA",section_code))
input_table$training_start<-as.Date(input_table$training_start)
input_table$training_end<-as.Date(input_table$training_end)
input_table$prediction_end<-as.Date(input_table$prediction_end)

# for(i in 1:nrow(input_table)){

# 	paste0("~/Dropbox/PhD_Workshop/DataMining/Analyzed/",input_table$section_code[i],"_",local_input_file=input_table$local_input_file[i])
# 	dir("~/Dropbox/PhD_Workshop/DataMining/Analyzed/")
# }



###This is actually production code which makes summary files
section_code_init<-"AA0"
# the_model<-"HN_data_withdiv_asym_5y"
the_model<-"BS_data_withdiv_2y"
error_files<-data.frame(file_names=dir("~/Dropbox/PhD_Workshop/DataMining/Analyzed/")) %>% 
				filter(grepl("error_summary",file_names) & grepl(the_model,file_names) & grepl(section_code_init,file_names) & !grepl("~",file_names))

for(i in 1:nrow(error_files)){

	summary_table<-read_excel(paste0("~/Dropbox/PhD_Workshop/DataMining/Analyzed/",as.character(error_files[i,]))) 

	if(i == 1){
		average_table<-select(summary_table,Training,Prediction)
		rownames(average_table)<-unlist(summary_table[,1])
	}else{
		average_table<-average_table + select(summary_table,Training,Prediction)
	}

}
average_table<-average_table/nrow(error_files)
average_table<-cbind(average_table,select(summary_table,error_type,mean_median))

average_table<-average_table %>% group_by(error_type,mean_median) %>% mutate(Training_rank=rank(Training),Prediction_rank=rank(Prediction)) %>% 
		cbind(summary_table[,1],.)

colnames(average_table)[1]<-"model_full"

average_table<- average_table %>% rowwise() %>% mutate(model_name=substr(model_full,1,regexpr("_",model_full)[1]-1)) %>% arrange(mean_median,model_name) %>% print(n=50)

write.xlsx2(data.frame(average_table),path.expand(paste0("~/Dropbox/PhD_Workshop/DataMining/Analyzed/",section_code_init,"_",the_model,"_aggregate_summary.xlsx")),row.names=FALSE,append=FALSE)




# error_input_path<-"~/Dropbox/PhD_Workshop/DataMining/Analyzed/"
# i<-7
# error_files<-dir(error_input_path)[grep("_errors",dir(error_input_path))]
# section_codes<-mapply(substr,error_files,MoreArgs=list(start=1,stop=5))
# error_files<-mapply(substr,error_files,MoreArgs=list(start=7,stop=nchar(error_files)))
# error_files<-gsub("_errors.csv","",error_files)

# for(i in 1:length(error_files)){

# 	error_summarization(error_input_path,error_files[i],section_codes[i])

# }

# seq(0.9,1.1,length.out=6)
# 42 124


error_data_hn <- read.table(paste0("~/Dropbox/PhD_Workshop/DataMining/Analyzed/","AA001_SPX_2013_HN_data_withdiv_asym_5y_predictions.csv"),sep=",",header=TRUE)
error_data_bs <- read.table(paste0("~/Dropbox/PhD_Workshop/DataMining/Analyzed/","AA001_SPX_2013_BS_data_withdiv_2y_predictions.csv"),sep=",",header=TRUE)
error_data_hn <- error_data_hn %>% filter(Type=="call" & ARPE_market <= quantile(error_data_hn$ARPE_market,0.85) & ARPE_market <= quantile(error_data_hn$manual1_ARPE,0.85) & ARPE_market <= quantile(error_data_hn$cit_ARPE,0.85)) 
error_data_bs <- error_data_bs %>% filter(Type=="call" & ARPE_market <= quantile(error_data_bs$ARPE_market,0.85) & ARPE_market <= quantile(error_data_bs$manual1_ARPE,0.85) & ARPE_market <= quantile(error_data_bs$cit_ARPE,0.85)) 

error_vector <- (c(error_data_bs$ARPE_market,error_data_hn$ARPE_market,error_data_hn$manual1_ARPE,error_data_hn$cit_ARPE,error_data_bs$manual1_ARPE,error_data_bs$cit_ARPE))
error_color_values<-c(quantile(rescale(error_vector),0),quantile(rescale(error_vector),0.25),quantile(rescale(error_vector),0.75),quantile(rescale(error_vector),1))


instructions<-	scale_color_gradientn(colours=c("#71B280","#134E5E","#FFC500","#C21500"),values=error_color_values,name="Error",limits=c(min(error_vector),max(error_vector))) #+ scale_shape_discrete(name="Data Type")
shape_setup<- scale_shape_discrete(name="Data Type",labels=c("In-sample","Out-of-sample"))
theme_text_setup<-theme(title=element_text(size=6),axis.title.y=element_text(size=8),axis.title.x=element_text(size=8),axis.text.x=element_text(size=6),axis.text.y=element_text(size=6),legend.title=element_text(size=6),legend.text=element_text(size=6))
subs<-sample(1:nrow(error_data_hn),1000)
r1<-ggplot(data=error_data_bs,aes(x=Moneyness,y=NetMaturity)) + geom_point(aes_string(colour="ARPE_market",shape="DataType"),alpha=0.6) + instructions + ggtitle("Black Scholes Realized ARPE Errors") + theme_text_setup + shape_setup
#quartz()
r2<-ggplot(data=error_data_hn,aes(x=Moneyness,y=NetMaturity)) + geom_point(aes_string(colour="ARPE_market",shape="DataType"),alpha=0.6) + instructions + ggtitle("Heston Nandi GARCH Realized ARPE Errors") + theme_text_setup + shape_setup

cit1<-ggplot(data=error_data_bs,aes(x=Moneyness,y=NetMaturity)) + geom_point(aes_string(colour="cit_ARPE",shape="DataType"),alpha=0.6) + instructions  + ggtitle("Black Scholes CIT ARPE Predictions") + theme_text_setup + shape_setup
#quartz()
cit2<-ggplot(data=error_data_hn,aes(x=Moneyness,y=NetMaturity)) + geom_point(aes_string(colour="cit_ARPE",shape="DataType"),alpha=0.6) + instructions  + ggtitle("Heston Nandi GARCH CIT ARPE Predictions") + theme_text_setup + shape_setup

man1<-ggplot(data=error_data_bs,aes(x=Moneyness,y=NetMaturity)) + geom_point(aes_string(colour="manual1_ARPE",shape="DataType"),alpha=0.6) + instructions  + ggtitle("Black Scholes Manual ARPE Predictions") + theme_text_setup + shape_setup
#quartz()
man2<-ggplot(data=error_data_hn,aes(x=Moneyness,y=NetMaturity)) + geom_point(aes_string(colour="manual1_ARPE",shape="DataType"),alpha=0.6) + instructions  + ggtitle("Heston Nandi GARCH Manual ARPE Predictions") + theme_text_setup + shape_setup

	
#						theme(title=element_text(size=8),axis.title.y=element_text(size=8),axis.title.x=element_text(size=8)),axis.text.y=element_text(size=8)) +


# sub_error_data<-subset(error_data,Moneyness<=1.1 & Moneyness>=0.9 & NetMaturity<=252 & NetMaturity>=5) %>% filter(manual1_ARPE_errors<1000 & DataType =="Prediction" & Type =="call") %>% arrange((manual1_ARPE_errors))
# break_values<-c(0,quantile(sub_error_data$manual1_ARPE_errors,0.25),quantile(sub_error_data$manual1_ARPE_errors,0.5),quantile(sub_error_data$manual1_ARPE_errors,0.75),quantile(sub_error_data$manual1_ARPE_errors,1))/max(sub_error_data$manual1_ARPE_errors)

# instructions<-	scale_color_gradientn(colours=c("#71B280","#134E5E","#ffc500","#c21500"),values=break_values,name="ARPE",limits=c(min(sub_error_data$manual1_ARPE_errors),max(sub_error_data$manual1_ARPE_errors))) 

# a1<-ggplot(data=sub_error_data,aes(x=Moneyness,y=NetMaturity)) + geom_point(aes_string(color="manual1_ARPE_errors",shape="DataType"),alpha=0.6) + instructions
# a2<-ggplot(data=sub_error_data,aes(x=Moneyness,y=NetMaturity)) + geom_point(aes_string(color="cit_ARPE_errors",shape="DataType"),alpha=0.6) + instructions
# 	scale_color_gradientn(colours=c("#71B280","#134E5E","#ffc500","#c21500"),values=break_values,name="ARPE",limits=c(min(sub_error_data$manual1_ARPE_errors),max(sub_error_data$manual1_ARPE_errors)))



grid_arrange_shared_legend <- function(...) {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom",legend.box="horizontal"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    arrangeGrob(
        do.call(arrangeGrob, lapply(plots, function(x)
            x + theme(legend.position="none"))),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight))

}
ahah<-grid_arrange_shared_legend(r1,r2,cit1,cit2,man1,man2)

ggsave("~/Documents/ARPE_errors.jpg",ahah,height=9,width=6)

ggplot(data=error_data,aes(x=Moneyness,y=NetMaturity)) + geom_point(aes_string(color="cit_ARPE_errors"),alpha=0.5) + 
	scale_color_gradient(low="red",high="green",limits=c(quantile(error_data$cit_RPE_errors,0.01),quantile(error_data$cit_RPE_errors,0.99)))

ggplot(data=error_data,aes(x=Moneyness,y=NetMaturity)) + geom_point(aes_string(color="cit_ARPE_errors"),alpha=0.5) + 
	scale_color_gradient(low="red",high="green",limits=c(quantile(error_data$cit_RPE_errors,0.01),quantile(error_data$cit_RPE_errors,0.99)))


quantile(sub_error_data$manual1_MSE_errors,0.99)



#This part is to return summary tables

results_main_path<-"~/Dropbox/PhD_Workshop/DataMining/"

summary_raw<-read_excel(paste0(results_main_path,"Analyzed Kmeans Altered/AA0_BS_data_withdiv_2y_aggregate_summary.xlsx"))
#summary_raw<-read_excel(paste0(results_main_path,"Analyzed Kmeans Clairvoyant/AA0_BS_data_withdiv_2y_aggregate_summary_cv.xlsx"))

summary_good <-summary_raw %>%
			filter(mean_median=="median") %>%
			arrange(model_name) %>%
			select(model_name,error_type,Training,Prediction) %>%
			melt(id.vars=c("model_name","error_type"),variable.name="ErrorPhase",value.name="Error") %>%
			mutate(model_name=toupper(model_name),Error=round(Error,3)) %>%
			dcast(formula=model_name ~ ErrorPhase + error_type, value.var="Error")

ssg<-xtable(summary_good)
digits(ssg)<-3
print(ssg)

