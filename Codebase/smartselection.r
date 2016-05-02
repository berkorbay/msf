#This code is to amend irrationalities in model selection process

## These codes are to spit out descriptive statistics from OSYM
rm(list=ls(all=TRUE))
gc()
options(repos="http://cran.rstudio.com/") #Set default repo
options(dplyr.width = Inf) #See all dplyr data table columns
options(scipen = 7)

#Remove predetermined seed if exists
if(exists(".Random.seed"))
	rm(.Random.seed, envir=globalenv())


#Install the required packages
if(!("dplyr" %in% rownames(installed.packages())))
	install.packages("dplyr")


if(!("ggplot2" %in% rownames(installed.packages())))
	install.packages("ggplot2")


if(!("reshape2" %in% rownames(installed.packages())))
	install.packages("reshape2")

if(!("png" %in% rownames(installed.packages())))
	install.packages("png")

if(!("gridExtra" %in% rownames(installed.packages())))
	install.packages("gridExtra")

if(!("xlsx" %in% rownames(installed.packages())))
	install.packages("xlsx")


#library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(png)
library(gridExtra)
library(xlsx)

find_final_decision<-function(x){
	y<-rep(0,length(x))
	y[rev(which(x!=0))[1]]<-ifelse(x[rev(which(x!=0))[1]]>0,1,-1)
	return(y)
}



#The path to model selection info
selection_path<-"~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/"

#List the files and filter for model selection prices
selection_info<-dir(selection_path)
selection_info<-selection_info[grep("refresh_price_info",selection_info)]

smart_selection<-function(selection_file,model_filenames,output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/Smart Selection/"){
	price_info_table<-read.table(paste0(selection_path,selection_file),sep=",",header=TRUE)
	price_info_table$DataDate<-as.Date(price_info_table$DataDate)
	#Keep indices (a simple trick to keep location)
	price_info_table<-price_info_table %>% mutate(row_index=1:nrow(price_info_table))
	ls_info_table<-price_info_table
	pnl_info_table<-price_info_table



	pnl_info_table[,model_filenames]<-0
	ls_info_table[,model_filenames]<-0
	temp_table<-ls_info_table[,model_filenames]

	ls_info_table[,model_filenames]<-price_info_table[,model_filenames]>=price_info_table[,"Last"]
	temp_table<-ls_info_table[,model_filenames]*1+(!ls_info_table[,model_filenames])*(-1)
	ls_info_table[,model_filenames]<-temp_table

	#Find the PnL counterpart
	# selection_pnl<-gsub("price_info","pnl",selection_info)
	# pnl_info_table<-read.table(paste0(selection_path,selection_pnl[1]),sep=",",header=TRUE)
	# pnl_info_table$DataDate<-as.Date(pnl_info_table$DataDate)
	# #Prepare smart selection column
	# pnl_info_table<-pnl_info_table %>% mutate(smart_selection=0)

	#Get the link 
	contracts_list<- unique(price_info_table$OptionSymbol)
	#316
	# pnl_final<-rep(0,length(model_filenames))
	# names(pnl_final)<-model_filenames
	for(i in contracts_list){
		contract_csection<-ls_info_table %>% 
								filter(OptionSymbol == i) %>%
								arrange(DataDate)

		ls_decision<-contract_csection[,model_filenames]
		ls_decision<-rbind(ls_decision[1,],(ls_decision[-1,]-ls_decision[-nrow(ls_decision),]))
		payoff_decision<-ls_decision %>% mutate_each(funs(find_final_decision))
		ls_info_table[contract_csection$row_index,model_filenames]<-ls_decision+0.1*payoff_decision
		pnl_info_table[contract_csection$row_index,model_filenames]<-payoff_decision*contract_csection$ExpirationPayoff - ls_decision*contract_csection$Last
		# This code is for end of the contract line decisions
		# ls_decision[length(ls_decision)]<-ls_decision[length(ls_decision)]-sum(ls_decision)
	}

	ls_info_table <- ls_info_table %>% select(-market_prices,-row_index)
	pnl_info_table <- pnl_info_table %>% select(-market_prices,-row_index)

	write.table(ls_info_table,paste0(output_path,gsub(".csv","",selection_file),"_smart_longshort",".csv"),sep=",",row.names=FALSE)
	write.table(pnl_info_table,paste0(output_path,gsub(".csv","",selection_file),"_smart_pnl",".csv"),sep=",",row.names=FALSE)

}

model_filenames<-c("selection","HN_data_withdiv_symm_2y","HN_data_withdiv_symm_5y","HN_data_withdiv_asym_5y","BS_data_withdiv_2y","BS_data_withdiv_5y")

model_filenames_large<-c("Levy_GHYP_data_withdiv_Esscher_50000_iteration_2y_asymmetric","Levy_GHYP_data_withdiv_MCMM_50000_iteration_2y_asymmetric","Levy_GHYP_data_withdiv_Esscher_50000_iteration_2y_symmetric","Levy_GHYP_data_withdiv_MCMM_50000_iteration_2y_symmetric","Levy_GHYP_data_withdiv_Esscher_50000_iteration_5y_asymmetric","Levy_GHYP_data_withdiv_MCMM_50000_iteration_5y_asymmetric","Levy_GHYP_data_withdiv_Esscher_50000_iteration_5y_symmetric","Levy_GHYP_data_withdiv_MCMM_50000_iteration_5y_symmetric")

for(i in selection_info){
	print(i)
	if(any(grepl("SPX",i))){
		# smart_selection(i,c(model_filenames))
		smart_selection(i,c(model_filenames,model_filenames_large))

	}else{
		# smart_selection(i)

	}
}


smart_summaries<-function(great_table,model_filenames){



}



output_path="~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Mondrian/Smart Selection/"

output_files<-dir(output_path)

output_files<-gsub("_smart_pnl.csv","",output_files)
output_files<-gsub("_smart_longshort.csv","",output_files)
# length(output_files)
output_files<-unique(output_files)

for(i in 1:length(output_files)){

	a1<-read.csv(paste0(output_path,output_files[i],"_smart_pnl.csv"),sep=",",header=TRUE)
	a2<-read.csv(paste0(output_path,output_files[i],"_smart_longshort.csv"),sep=",",header=TRUE)

	contract_data<-round(a2[,(which(colnames(a2)=="PDAbsolute")+1):ncol(a2)])
	pnl_data<-a1[,(which(colnames(a1)=="PDAbsolute")+1):ncol(a1)]

	long_calls<-colSums((a2$Type=="call")*contract_data*(contract_data>0))
	long_puts<-colSums((a2$Type=="put")*contract_data*(contract_data>0))
	total_longs<-long_calls+long_puts
	summary_table<-rbind(long_calls,long_puts,total_longs)

	short_calls<-colSums((a2$Type=="call")*abs(contract_data)*(contract_data<0))
	short_puts<-colSums((a2$Type=="put")*abs(contract_data)*(contract_data<0))
	total_shorts<-short_calls+short_puts
	summary_table<-rbind(summary_table,short_calls,short_puts,total_shorts)


	neutral_calls<-colSums((a2$Type=="call")*(contract_data==0))
	neutral_puts<-colSums((a2$Type=="put")*(contract_data==0))
	total_neutrals<-neutral_calls+neutral_puts
	summary_table<-rbind(summary_table,neutral_calls,neutral_puts,total_neutrals)

	total_long_investment<-colSums(a2$Last*contract_data*(contract_data>0))
	total_short_capital<-colSums(a2$Last*abs(contract_data)*(contract_data<0))
	total_neutral_value<-colSums(a2$Last*(contract_data==0))
	summary_table<-rbind(summary_table,total_long_investment,total_short_capital,total_neutral_value)

	total_nonnegative_contracts<-colSums((pnl_data>=0)*abs(contract_data))
	total_profit<-colSums((pnl_data>=0)*pnl_data)
	total_negative_contracts<-colSums((pnl_data<0)*abs(contract_data))
	total_loss<-colSums((pnl_data<0)*pnl_data)
	PnL<-colSums(pnl_data)
	summary_table<-rbind(summary_table,total_nonnegative_contracts,total_profit,total_negative_contracts,total_loss,PnL)

	consensus_contracts <- rep(sum(rowSums(pnl_data>0) == ncol(summary_table)),ncol(summary_table))
	all_neutral_contracts <- rep(sum(rowSums(abs(contract_data)) == 0),ncol(summary_table))
	doom_contracts <- rep(sum(rowSums(pnl_data<0) == ncol(summary_table)),ncol(summary_table))
	wins_contracts<- colSums((pnl_data>0) * (rowSums(pnl_data>0) < ncol(summary_table)))
	neutral_contracts<-colSums((contract_data==0) * (rowSums(contract_data==0) < ncol(summary_table)))
	losses_contracts<- colSums((pnl_data<0) * (rowSums(pnl_data<0) < ncol(summary_table)))

	summary_table<-rbind(summary_table,consensus_contracts,all_neutral_contracts,doom_contracts,wins_contracts,neutral_contracts,losses_contracts)

	consensus_contracts_dollars <- rep(sum(a2$Last*(rowSums(pnl_data>0) == ncol(summary_table))),ncol(summary_table))
	all_neutral_contracts_dollars <- rep(sum(a2$Last*(rowSums(abs(contract_data)) == 0)),ncol(summary_table))
	doom_contracts_dollars <- rep(sum(a2$Last*(rowSums(pnl_data<0) == ncol(summary_table))),ncol(summary_table))
	wins_contracts_dollars <- colSums(pnl_data*(pnl_data>0) * (rowSums(pnl_data>0) < ncol(summary_table)))
	neutral_contracts_dollars <- colSums(a2$Last*(contract_data==0) * (rowSums(contract_data==0) < ncol(summary_table)))
	losses_contracts_dollars <- colSums(pnl_data*(pnl_data<0) * (rowSums(pnl_data<0) < ncol(summary_table)))

	summary_table<-rbind(summary_table,consensus_contracts_dollars,all_neutral_contracts_dollars,doom_contracts_dollars,wins_contracts_dollars,neutral_contracts_dollars,losses_contracts_dollars)

	rownames(summary_table)<-c("Long Calls",
								"Long Puts",
								"Total Longs",
								"Short Calls",
								"Short Puts",
								"Total Shorts",
								"Neutral Calls",
								"Neutral Puts",
								"Total Neutrals",
								"Total Long Investment",
								"Total Short Capital",
								"Total Neutral Value",
								"Total Nonnegative Contracts",
								"Total Profit",
								"Total Negative Contracts",
								"Total Loss",
								"PnL",
								"Total Consensus",
								"All Neutral",
								"Total Doom",
								"Total Wins",
								"Total Individual Neutrals",
								"Total Losses",
								"Total Consensus Dollars",
								"All Neutral Dollars",
								"Total Doom Dollars",
								"Total Wins Dollars",
								"Total Individual Neutrals Dollars",
								"Total Losses Dollars")


	write.table(summary_table,paste0("~/Dropbox/PhD_Workshop/Output Files/","Benchmarks/Summaries/Smart Selection/",gsub("_price_info","",output_files[i]),"_benchmark_summary_smart.csv"),sep=",",row.names=TRUE,col.names=NA,append=FALSE)

}


to_be_summarized<-dir("~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Summaries/Smart Selection/")
to_be_summarized<-to_be_summarized[-grep("aggregate",to_be_summarized)]
to_be_summarized<-gsub("_2010","_datehere",to_be_summarized)
to_be_summarized<-gsub("_2011","_datehere",to_be_summarized)
to_be_summarized<-gsub("_2012","_datehere",to_be_summarized)
to_be_summarized<-gsub("_2013","_datehere",to_be_summarized)
to_be_summarized<-unique(to_be_summarized)

for(i in 1:length(to_be_summarized)){
	start_year<-0
	print(to_be_summarized[i])
	for(years in 2010:2013){
		if(gsub("datehere",years,to_be_summarized[i]) %in% dir("~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Summaries/Smart Selection/")){
			the_data<-read.csv(paste0("~/Dropbox/PhD_Workshop/Output Files/Benchmarks/Summaries/Smart Selection/",gsub("datehere",years,to_be_summarized[i])),sep=",",row.names=1)
			if(start_year == 0){
				start_year<-years
			}
			if(years>start_year){
				sum_data<-sum_data+the_data
			}else{
				sum_data<-the_data
			}
		}
	}
	output_file_name<-gsub("datehere","from_2013_to_2010",to_be_summarized[i])
	output_file_name<-gsub("_benchmark_summary_smart.csv","_benchmark_summary_aggregate_smart.csv",output_file_name)
	
	write.table(sum_data,paste0("~/Dropbox/PhD_Workshop/Output Files/","Benchmarks/Summaries/Smart Selection/",output_file_name),sep=",",row.names=TRUE,col.names=NA,append=FALSE)
	print(start_year)

}


asset_data<-read.csv("~/Dropbox/PhD_Workshop/Input Files/Asset Prices/NDX_daily_processed.csv",sep=",",header=TRUE)
asset_data$Date<-as.Date(asset_data$Date)
asset_plot()

first_data<- a2 %>% filter(OptionSymbol == "NDX101218P02000000") %>% arrange(DataDate) %>% mutate(selection=round(selection)) %>% select(DataDate,selection)
first_data$DataDate<-as.Date(first_data$DataDate)
max_maturity<- a2 %>% filter(OptionSymbol == "NDX101218P02000000") %>% select(RealExpiration)
max_maturity<-as.Date(unlist(max_maturity)[1])

plot_data<-data.frame(Dates=seq(min(as.Date(first_data$DataDate)),max_maturity,by=1),position=0)

plot_data<-left_join(plot_data,first_data,by=c("Dates" = "DataDate"))

plot_data$selection[is.na(plot_data$selection)]<-0

plot_data<-plot_data %>% mutate(position=cumsum(selection))

plot_data$selection[which(plot_data$selection ==0)]<-NA

plot_position<- ggplot(data=plot_data,aes(x=Dates)) + geom_line(aes(y=position,color="Position")) + geom_point(aes(y=selection,color="decision")) + xlab("") + scale_color_discrete("") + theme(legend.position="bottom")


second_data<- a1 %>% filter(OptionSymbol == "NDX101218P02000000") %>% arrange(DataDate) %>% select(DataDate,selection,Last)
second_data$DataDate<-as.Date(first_data$DataDate)

pnl_plot_data<-data.frame(Dates=seq(min(as.Date(first_data$DataDate)),max_maturity,by=1),position=0)

pnl_plot_data<-left_join(pnl_plot_data,second_data,by=c("Dates" = "DataDate"))

pnl_plot_data[is.na(pnl_plot_data)]<-0

pnl_plot_data<-pnl_plot_data %>% mutate(position=cumsum(selection))
pnl_plot_data$selection[which(pnl_plot_data$selection ==0)]<-NA

plot_pnl<- ggplot(data=pnl_plot_data,aes(x=Dates)) + geom_point(aes(y=selection,color="Decision Cost")) + 
											geom_line(aes(y=position,color="Cumulative PnL")) + 
											geom_line(aes(y=Last,color="Option Price")) +
											ylab("Dollars") + xlab("") +
											scale_color_discrete("") + 
											theme(legend.position="bottom")


asset_data<-read.csv("~/Dropbox/PhD_Workshop/Input Files/Asset Prices/NDX_daily_processed.csv",sep=",",header=TRUE)
asset_data$Date<-as.Date(asset_data$Date)
asset_data<- asset_data %>% filter(Date >= min(as.Date(first_data$DataDate)) & Date <= max_maturity)
asset_plot <- ggplot(data=asset_data,aes(x=Date)) + geom_line(aes(y=Close)) + 
geom_abline(aes(intercept=2000,slope=0),color="red") + xlab("")

plot_full<-arrangeGrob(plot_position,plot_pnl,asset_plot,ncol=1) 
ggsave("~/Documents/smart_selection_plot.png",plot_full,width=8,height=11)

#Total longs round(table), pmax(0,x) - might not work on df level, total longs reverse is shorts

#Total neutrals if ls info table == 0

#Long calls, short calls, long puts, short puts, neutral calls, neutral puts

#Long investment, short capital

#positive contracts
#profits
#negative contracts
#loss
#pnl
#consensus
#wins
#losses
#doom
#consensus dollars
#wins dollars
#losses dollars
#doom dollars


