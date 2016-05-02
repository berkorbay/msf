main_path <- "~/Dropbox/PhD_Workshop/"
source(paste0(main_path,"Codebase/00_run_this_first_option_calculations.r"))

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

suppressMessages(library(e1071))
suppressMessages(library(party))
suppressMessages(library(partykit))
suppressMessages(library(Formula))
suppressMessages(library(rpart))
suppressMessages(library(lazyeval))
suppressMessages(library(NbClust))
suppressMessages(library(RColorBrewer))
suppressMessages(library(scales))
suppressMessages(library(stringr))

get_data_from_time_interval<-function(prediction_first_date,underlying_asset,training_back_months,prediction_forward_months,model_and_parameter,moneyness_interval,maturity_interval){

	training_data_years<-as.numeric(format(prediction_first_date,"%Y"))

	training_period_start<-as.numeric(format(prediction_first_date,"%m"))-training_back_months
	if(training_period_start <=0){
		training_period_start <- training_period_start + 12
		training_data_years<- c(training_data_years,training_data_years-1)
	}
	training_start_date<-as.Date(paste0(min(training_data_years),"-",ifelse(training_period_start<10,paste0("0",training_period_start),training_period_start),"-",format(prediction_first_date,"%d")))

	prediction_period_end<-as.numeric(format(prediction_first_date,"%m"))+prediction_forward_months
	if(prediction_period_end > 12){
		prediction_period_end <- prediction_period_end - 12
		training_data_years<- c(training_data_years,training_data_years+1)
	}
	prediction_period_end<-as.Date(paste0(max(training_data_years),"-",ifelse(prediction_period_end<10,paste0("0",prediction_period_end),prediction_period_end),"-",format(prediction_first_date,"%d")))

	load(paste0(main_path,"Output Files/Trading_and_Error/",underlying_asset,"_",training_data_years[1],"_",model_and_parameter,"_trade_and_error.RData"))
	data_set <- trade_error %>% filter(!is.na(Last)) %>% select(DataDate,OptionSymbol,Type,Strike,Moneyness,Maturity,NetMaturity,Last,model_price,RPE,ARPE,APE)

	if(length(training_data_years)>1){
		for(yrs in 2:length(training_data_years)){
			load(paste0(main_path,"Output Files/Trading_and_Error/",underlying_asset,"_",training_data_years[yrs],"_",model_and_parameter,"_trade_and_error.RData"))
			data_set <- trade_error %>% filter(!is.na(Last)) %>% select(DataDate,OptionSymbol,Type,Strike,Moneyness,Maturity,NetMaturity,Last,model_price,RPE,ARPE,APE) %>%
							rbind(data_set,.)
		}
	}
	data_set <- data_set %>% filter(DataDate >= training_start_date & DataDate < prediction_period_end & Moneyness <= moneyness_interval[2] & Moneyness >= moneyness_interval[1] & Maturity >= maturity_interval[1] & Maturity <= maturity_interval[2]) %>%
		filter(Last>=0.05 & model_price>0.01) %>% mutate(TrainPred=ifelse(DataDate>=prediction_first_date,"Prediction","Training"))
	return(data_set)
}

rescale_error<-function(errors,mederror=NA){
	if(is.na(mederror)){
		ifelse(errors<=median(errors),(errors-min(errors))/(median(errors)-min(errors))*0.5,(errors-median(errors))/(max(errors)-median(errors))*0.5+0.5)
	}else{
		ifelse(errors<=mederror,(errors-min(errors))/(mederror-min(errors))*0.5,(errors-mederror)/(max(errors)-mederror)*0.5+0.5)
	}
}

get_the_cluster<-function(Mon_sc,Mat_sc,center_matrix){

	which.min(colSums((t(center_matrix)-c(Mon_sc,Mat_sc))^2))

}

kmeans_learn<-function(raw_data,CallPut="call",error_type="ARPE",randseed=0,moneyness_interval,maturity_interval){

	raw_data<-raw_data %>% 
				filter(Type==CallPut)  %>% 
				mutate_(Moneyness_scaled=interp(~((Moneyness-min(moneyness_interval))/(max(moneyness_interval)-min(moneyness_interval))*100)),
						Maturity_scaled=interp(~((Maturity-min(maturity_interval))/(max(maturity_interval)-min(maturity_interval))*100)))

	training_data <- raw_data %>% 
						filter(TrainPred=="Training") %>% 
						mutate_(Error_scaled=interp(~(rescale_error(var,mederror=NA)*100), var = as.name(error_type)))

	# mederror <- training_data %>% summarise_(med=interp(~median(var),var=as.name(error_type)))

	training_matrix<- training_data %>%
						select(Error_scaled,Moneyness_scaled,Maturity_scaled) %>%
						as.matrix

	n_cluster<-min(round(nrow(training_matrix)/100),200)

	if(randseed>0){
		set.seed(randseed)
	}

	kmeans_training<-kmeans(training_matrix,centers=n_cluster,nstart=round(n_cluster/5),iter.max=10^6)

	kmeans_centers<-kmeans_training$centers[,c("Moneyness_scaled","Maturity_scaled")]

	kmeans_center_errors<- training_data %>% 
								mutate(clusters=kmeans_training$cluster) %>% 
								group_by(clusters) %>% 
								summarise_(model_estimate=interp(~mean(var,na.rm=TRUE),var=as.name(error_type)))

	result_data <- raw_data %>% 
						rowwise() %>%
						mutate(clusters=get_the_cluster(Moneyness_scaled,Maturity_scaled,kmeans_centers)) %>% 
						ungroup() %>%
						left_join(.,kmeans_center_errors,by="clusters") %>%
						mutate_(MAPE=interp(~(abs(var-model_estimate)/pmax(abs(var),0.01)),var=as.name(error_type)))

	summary<- result_data %>% group_by(TrainPred) %>% summarise(MAPE=mean(MAPE,na.rm=TRUE)) %>% select(MAPE) %>% unlist
	names(summary)<-c("Prediction","Training")

	return(list(result_data=result_data,summary=summary))
}

dm_learn<-function(raw_data,model_name="svm",CallPut="call",error_type,randseed=0){

	if(randseed>0){
		set.seed(randseed)
	}
	raw_data<-filter(raw_data,Type==CallPut)
	# training_data<-filter(raw_data,TrainPred=="Training")
	the_formula<- as.formula(paste0(error_type," ~ ","Moneyness + Maturity"))
	if(model_name=="svm"){
		the_model<-svm(formula=the_formula,data=raw_data,subset=(raw_data$TrainPred=="Training"),kernel="radial",scale=FALSE)
	}else if(model_name=="cit"){
		the_model<-ctree(the_formula, data=raw_data,subset=(raw_data$TrainPred=="Training"))
	}else if(model_name=="dt"){
		the_model<-rpart(the_formula, data=raw_data,subset=(raw_data$TrainPred=="Training"))
	}else{
		stop("Wrong model name!")
	}
	result_data <- raw_data %>% 
	 	mutate(model_estimate=predict(the_model,.)) %>%
		mutate_(MAPE=interp(~(abs(var-model_estimate)/pmax(abs(var),0.01)),var=as.name(error_type)))

	summary<- result_data %>% group_by(TrainPred) %>% summarise(MAPE=mean(MAPE,na.rm=TRUE)) %>% select(MAPE) %>% unlist
	names(summary)<-c("Prediction","Training")

	return(list(result_data=result_data,summary=summary))
}

manual_learn<-function(raw_data,CallPut,error_type,monbreaks,matbreaks){

	# raw_data<-tp_data
	raw_data <- raw_data %>% filter(Type==CallPut) %>%
					mutate(Moncuts=cut(Moneyness,monbreaks),
							Matcuts=cut(Maturity,matbreaks))

	manual_centers <- raw_data %>% 
						filter(TrainPred=="Training") %>% 
						group_by(Moncuts,Matcuts) %>% 
						summarise_(model_estimate=interp(~mean(var,na.rm=TRUE),var=as.name(error_type)))

	result_data <-	raw_data %>%
					  	left_join(.,manual_centers,by=c("Moncuts","Matcuts")) %>%
					  	mutate_(MAPE=interp(~(abs(var-model_estimate)/pmax(abs(var),0.01)),var=as.name(error_type)))

	summary<- result_data %>% group_by(TrainPred) %>% summarise(MAPE=mean(MAPE,na.rm=TRUE)) %>% select(MAPE) %>% unlist
	names(summary)<-c("Prediction","Training")

	return(list(result_data=result_data,summary=summary))
}

analyze_cross_section<-function(prediction_first_date,underlying_asset,training_back_months,prediction_forward_months,model_and_parameter,moneyness_interval,maturity_interval,the_seed=0,print_report=TRUE){

	tp_data <- get_data_from_time_interval(prediction_first_date=prediction_first_date,underlying_asset=underlying_asset,training_back_months=training_back_months,prediction_forward_months=prediction_forward_months,model_and_parameter=model_and_parameter,moneyness_interval,maturity_interval) %>% arrange(DataDate,OptionSymbol)

	summary_table<-matrix(0,ncol=6,nrow=6)
	rownames(summary_table)<-c("K-Means","SVM","CIT","DT","Manual 1","Manual 2")
	colnames(summary_table)<-c(paste0(c("call_"),c("ARPE","RPE","APE")),paste0(c("put_"),c("ARPE","RPE","APE")))
	is_st<-summary_table
	os_st<-summary_table

	for(errt in c("ARPE","RPE","APE")){
		for(cp in c("call","put")){
			# print(paste(errt,cp))

			# print("kmeans")
			the_list<-kmeans_learn(tp_data,CallPut=cp,error_type=errt,randseed=the_seed,moneyness_interval=moneyness_interval,maturity_interval=maturity_interval)
			error_table<-the_list$result_data %>% rename(kmeans_cluster=clusters,kmeans_estimate=model_estimate,kmeans_MAPE=MAPE)
			is_st["K-Means",paste0(cp,"_",errt)]<-the_list$summary["Training"]
			os_st["K-Means",paste0(cp,"_",errt)]<-the_list$summary["Prediction"]

			# print("svm")
			the_list<-dm_learn(raw_data=tp_data,model_name="svm",CallPut=cp,error_type=errt,randseed=the_seed)
			error_table <- the_list$result_data %>% 
								select(DataDate,OptionSymbol,model_estimate,MAPE) %>% 
								rename(svm_estimate=model_estimate,svm_MAPE=MAPE) %>% 
								left_join(error_table,.,by=c("OptionSymbol","DataDate"))
			is_st["SVM",paste0(cp,"_",errt)]<-the_list$summary["Training"]
			os_st["SVM",paste0(cp,"_",errt)]<-the_list$summary["Prediction"]

			# print("cit")
			the_list<-dm_learn(raw_data=tp_data,model_name="cit",CallPut=cp,error_type=errt,randseed=the_seed)
			error_table <- the_list$result_data %>% 
								select(DataDate,OptionSymbol,model_estimate,MAPE) %>% 
								rename(cit_estimate=model_estimate,cit_MAPE=MAPE) %>% 
								left_join(error_table,.,by=c("OptionSymbol","DataDate"))
			is_st["CIT",paste0(cp,"_",errt)]<-the_list$summary["Training"]
			os_st["CIT",paste0(cp,"_",errt)]<-the_list$summary["Prediction"]

			# print("dt")
			the_list<-dm_learn(raw_data=tp_data,model_name="dt",CallPut=cp,error_type=errt,randseed=the_seed) #64342
			error_table <- the_list$result_data %>% 
								select(DataDate,OptionSymbol,model_estimate,MAPE) %>% 
								rename(dt_estimate=model_estimate,dt_MAPE=MAPE) %>% 
								left_join(error_table,.,by=c("OptionSymbol","DataDate"))
			is_st["DT",paste0(cp,"_",errt)]<-the_list$summary["Training"]
			os_st["DT",paste0(cp,"_",errt)]<-the_list$summary["Prediction"]

			# print("manual1")
			the_list<-manual_learn(raw_data=tp_data,CallPut=cp,error_type=errt,monbreaks=c(0.899,0.94,0.97,1.00,1.03,1.06,1.101),matbreaks=c(0,60,180,366))
			error_table <- the_list$result_data %>% 
								select(DataDate,OptionSymbol,model_estimate,MAPE) %>% 
								rename(manual1_estimate=model_estimate,manual1_MAPE=MAPE) %>% 
								left_join(error_table,.,by=c("OptionSymbol","DataDate"))
			is_st["Manual 1",paste0(cp,"_",errt)]<-the_list$summary["Training"]
			os_st["Manual 1",paste0(cp,"_",errt)]<-the_list$summary["Prediction"]
		
			# print("manual2")
			the_list<-manual_learn(raw_data=tp_data,CallPut=cp,error_type=errt,monbreaks=c(0.899,0.92,0.95,0.98,1.02,1.05,1.08,1.101),matbreaks=c(0,70,120,366))
			error_table <- the_list$result_data %>% 
								select(DataDate,OptionSymbol,model_estimate,MAPE) %>% 
								rename(manual2_estimate=model_estimate,manual2_MAPE=MAPE) %>% 
								left_join(error_table,.,by=c("OptionSymbol","DataDate"))
			is_st["Manual 2",paste0(cp,"_",errt)]<-the_list$summary["Training"]
			os_st["Manual 2",paste0(cp,"_",errt)]<-the_list$summary["Prediction"]

			if(cp=="call"){
				error_data<-error_table
			}else{
				error_data <- rbind(error_data,error_table)
				save(error_data,file=paste0(main_path,"Output Files/Clustering Chapter/",underlying_asset,"_",gsub("-","_",prediction_first_date),"_tr_",training_back_months,"_pr_",prediction_forward_months,"_",model_and_parameter,"_",errt,".RData"))
			}
		}
	}

	summary_tables=list(ins=is_st,outs=os_st)	

	colnames(is_st)<-paste0("ins_",colnames(is_st))
	colnames(os_st)<-paste0("outs_",colnames(os_st))

	if(print_report){

		print("In-sample Results")
		print("================")
		print(t(t(table(rownames(is_st)[data.frame(is_st) %>% summarise_each(funs(which.min)) %>% unlist]))))

		print("Out-of-sample Results")
		print("================")
		print(t(t(table(rownames(os_st)[data.frame(os_st) %>% summarise_each(funs(which.min)) %>% unlist]))))
	}

	write.xlsx2(cbind(is_st,os_st),path.expand(paste0(main_path,"Output Files/Clustering Chapter/",underlying_asset,"_",gsub("-","_",prediction_first_date),"_tr_",training_back_months,"_pr_",prediction_forward_months,"_",model_and_parameter,"_summary_table.xlsx")))

	return(summary_tables)

}

get_generalized_results<-function(cut_dates,the_model,the_asset,date_set="v1",print_report=TRUE){
	print(cut_dates[1])
		cv_data<-analyze_cross_section(prediction_first_date=cut_dates[1],underlying_asset=the_asset,training_back_months=6,prediction_forward_months=1,model_and_parameter=the_model,moneyness_interval=c(0.9,1.1),maturity_interval=c(7,365),the_seed=53423,print_report=print_report)
		cv_is<-cv_data$ins
		cv_os<-cv_data$outs

	for(i in 2:length(cut_dates)){
	print(cut_dates[i])
		cv_data<-analyze_cross_section(prediction_first_date=cut_dates[i],underlying_asset=the_asset,training_back_months=6,prediction_forward_months=1,model_and_parameter=the_model,moneyness_interval=c(0.9,1.1),maturity_interval=c(7,365),the_seed=53423,print_report=print_report)
		cv_is<-cv_is+cv_data$ins
		cv_os<-cv_os+cv_data$outs
	}

	cv_tables<-list(ins=cv_is/length(cut_dates),outs=cv_os/length(cut_dates))

	colnames(cv_is)<-paste0("ins_",colnames(cv_is))
	colnames(cv_os)<-paste0("outs_",colnames(cv_os))

	if(print_report){

		print("In-sample Results")
		print("================")
		print(t(t(table(rownames(cv_is)[data.frame(cv_is) %>% summarise_each(funs(which.min)) %>% unlist]))))

		print("Out-of-sample Results")
		print("================")
		print(t(t(table(rownames(cv_os)[data.frame(cv_os) %>% summarise_each(funs(which.min)) %>% unlist]))))
	}

	write.xlsx2(cbind(cv_is,cv_os)/length(cut_dates),path.expand(paste0(main_path,"Output Files/Clustering Chapter/",the_asset,"_",the_model,"_cross_validation_",date_set,"_.xlsx")))

	return(cv_tables)

}

# cut_dates_v1<-sort(as.Date(c("2009-03-09","2009-12-05","2010-01-20","2010-10-01","2011-07-06","2011-09-15","2012-05-06","2012-11-11","2013-02-04","2013-08-16")))

# my_cv<-get_generalized_results(cut_dates=cut_dates_v1,the_model="HN_data_withdiv_asym_5y",the_asset="SPX",print_report=TRUE)

# my_cv<-get_generalized_results(cut_dates=cut_dates_v1,the_model="BS_data_withdiv_2y",the_asset="SPX",print_report=TRUE)


######## Below code is for table generation

cluster_results_path<-paste0(main_path,"Output Files/Clustering Chapter/")

the_model_abbr<-"HN"

summary_table_paths<-data.frame(filenames=dir(cluster_results_path)) %>% filter(grepl("summary_table",filenames) & grepl(the_model_abbr,filenames))

st_data<-read_excel(paste0(cluster_results_path,summary_table_paths[1,1]))
colnames(st_data)[1]<-"model_name"

model_names <- st_data %>% mutate(model_name=gsub("Manual","Static",model_name)) %>% select(1) %>% unlist

change_name<-function(indexes,mod_names){
	mod_names[indexes]
}

st_table<-st_data %>% summarise_each(funs(which.min),-1) %>% mutate_each(funs(change_name(.,mod_names=model_names)))

for(i in 2:nrow(summary_table_paths)){
	st_table<-read_excel(paste0(cluster_results_path,summary_table_paths[i,1])) %>% 
					summarise_each(funs(which.min),-1) %>% 
					mutate_each(funs(change_name(.,mod_names=model_names))) %>%
					rbind(st_table,.)

}

rownames(st_table)<-paste("DS",1:10)


library(xtable)
print(xtable(st_table %>% select(contains("ins_"))),include.rownames=TRUE)
print(xtable(st_table %>% select(contains("outs_"))),include.rownames=TRUE)


summary_table_paths<-data.frame(filenames=dir(cluster_results_path)) %>% filter(grepl("cross",filenames) & grepl(the_model_abbr,filenames))

st_data<-read_excel(paste0(cluster_results_path,summary_table_paths[1,1])) %>% select(-1)
rownames(st_data)<-model_names

print(xtable(st_data %>% select(contains("ins_"))),include.rownames=TRUE)

print(xtable(st_data %>% select(contains("outs_"))),include.rownames=TRUE)

st_table %>% 
	melt(id.vars=NULL) %>% 
	rowwise %>% 
	mutate(inout=substr(variable,1,3)) %>% 
	ungroup %>% 
	group_by(value) %>% 
	summarise(count=n()) %>% 
	# ungroup %>% 
	# group_by(inout) %>% 
	arrange(desc(count))


####These tables are used in Clustering Chapter Algorithm vs Static and CIT vs Static Calculations

output_path<-"~/Dropbox/PhD_Workshop/Output Files/Clustering Chapter/"

file_list<-dir(output_path,pattern="BS_data_withdiv_2y_summary_table.xlsx")

# file_list<-dir(output_path,pattern="HN_data_withdiv_asym_5y_summary_table.xlsx")


analysis_one<-data.frame(model=character(),outs_call_ARPE=numeric(),outs_call_RPE=numeric(),outs_call_APE=numeric(),outs_put_ARPE=numeric(),outs_put_RPE=numeric(),outs_put_APE=numeric())

for(i in 1:length(file_list)){

	the_data<-read_excel(paste0(output_path,file_list[i]))
	colnames(the_data)[1]<-"model"

	analysis_one<-
		the_data %>% 
		select(model,starts_with("outs")) %>% 
		mutate(model_type=c(rep("DM",4),rep("Man",2))) %>% 
		group_by(model_type) %>% 
		summarise_each(funs(min),-model_type,-model) %>% 
		ungroup %>%
		mutate_each(funs(100*round(1-(./lead(.,1)),4)),-model_type) %>%
		slice(1) %>%
		mutate_each(funs(paste0(.,"%")),-model_type) %>%
		mutate(model_type=paste0("DS",i)) %>%
		rename(model=model_type) %>%
		rbind(analysis_one,.)

}

print(xtable(analysis_one),include.rownames=FALSE)

analysis_two<-data.frame(ds=character(),model=character(),outs_call_ARPE=numeric(),outs_call_RPE=numeric(),outs_call_APE=numeric(),outs_put_ARPE=numeric(),outs_put_RPE=numeric(),outs_put_APE=numeric())
for(i in 1:length(file_list)){

the_data<-read_excel(paste0(output_path,file_list[i]))
colnames(the_data)[1]<-"model"

analysis_two<-
	the_data %>% 
	select(model,starts_with("outs")) %>% 
	filter(model %in% c("CIT","Manual 1","Manual 2")) %>%
	mutate_each(funs(100*round(1-(first(.)/.),4)),-model) %>%
	slice(-1) %>%
	mutate_each(funs(paste0(.,"%")),-model) %>%
	mutate(model=gsub("Manual","Static",model)) %>%
	mutate(ds=ifelse(grepl("1",model),paste0("DS ",i),"")) %>%
	select(ds,model,starts_with("outs")) %>%
		rbind(analysis_two,.)
}
print(xtable(analysis_two),include.rownames=FALSE)


# rbind(analysis_one,analysis_two)



cluster_results_path<-paste0(main_path,"Output Files/Clustering Chapter/")

the_model_abbr<-"BS"

med_table<-data.frame(Type=c("call","call","put","put"),TrainPred=c("Training","Prediction","Training","Prediction")) %>% tbl_df
error_type<-c("ARPE","RPE","APE")

for(j in 1:length(error_type)){
summary_table_paths<-data.frame(filenames=dir(cluster_results_path,full.names=TRUE)) %>% filter(grepl(paste0("_",error_type[j]),filenames) & grepl(the_model_abbr,filenames))

	for(i in 1:nrow(summary_table_paths)){
	load(summary_table_paths$filenames[i])

	med_table<-
	error_data %>% 
	group_by(Type,TrainPred) %>% 
	summarise_each(funs(mean),contains("_MAPE")) %>% 
	# summarise_each(funs(quantile(.,0.9,na.rm=TRUE)),contains("_MAPE")) %>% 
	ungroup %>% 
	melt(.,id.var=c("Type","TrainPred")) %>% 
	tbl_df %>%
	group_by(Type,TrainPred) %>% 
	summarise(ddd=model_names[which.min(value)]) %>%
	left_join(med_table,.,by=c("Type", "TrainPred")) 

	colnames(med_table)<-gsub("ddd",paste0("DS",i,"_",error_type[j]),colnames(med_table))
	}

}


med_table %>% melt(.,id.var=c("Type","TrainPred")) %>% group_by(Type,TrainPred,value) %>% summarise(count=n()) %>% ungroup %>% arrange(value,Type,TrainPred)

med_table %>% melt(.,id.var=c("Type","TrainPred")) %>% group_by(value) %>% summarise(count=n()) %>% ungroup %>% arrange(desc(count))


