main_path <- "~/Dropbox/PhD_Workshop/"
source(paste0(main_path,"Codebase/00_run_this_first_option_calculations.r"))

# underlying_asset="SPX"
# data_year=2013
# benchmark_alias="b001"



# table(c(nrow(ms_trade_error),
# nrow(trade_error),
# nrow(position_table),
# nrow(cumulative_position_table),
# nrow(delta_table),
# nrow(pnl_table),
# nrow(delta_pnl_table),
# nrow(hf_pnl_table),
# nrow(dhe_table),
# nrow(arpe_table),
# nrow(rpe_table),
# nrow(ape_table)))

get_summary<-function(contenders,ms_contenders,underlying_asset,data_year,benchmark_alias){

	load(paste0(main_path,"Output Files/Trading_and_Error/",underlying_asset,"_",data_year,"_","MS_data_withdiv_",ms_contenders$training_error_obj[1],"_",ms_contenders$training_error_type[1],"_",ms_contenders$ms_code[1],"_",ms_contenders$lookback[1],"y_lookback_trade_and_error.RData"))

	position_table <- ms_trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,Type,Last,naked_position)
	colnames(position_table) <- gsub("naked_position",ms_contenders$alias[1],colnames(position_table))

	cumulative_position_table <- ms_trade_error  %>% filter(!is.na(Last)) %>% mutate(cumulative_position=cumulative_position+last_position) %>% select(OptionSymbol,DataDate,Type,Last,cumulative_position)
	colnames(cumulative_position_table) <- gsub("cumulative_position",ms_contenders$alias[1],colnames(cumulative_position_table))

	delta_table <- ms_trade_error %>% select(OptionSymbol,DataDate,Type,model_delta)
	colnames(delta_table) <- gsub("model_delta",ms_contenders$alias[1],colnames(delta_table))

	pnl_table <- ms_trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,naked_pnl)
	colnames(pnl_table) <- gsub("naked_pnl",ms_contenders$alias[1],colnames(pnl_table))

	delta_pnl_table <- ms_trade_error %>% select(OptionSymbol,DataDate,Type,delta_pnl)
	colnames(delta_pnl_table) <- gsub("delta_pnl",ms_contenders$alias[1],colnames(delta_pnl_table))

	hf_pnl_table <- ms_trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,Type,hedge_and_forget_pnl)
	colnames(hf_pnl_table) <- gsub("hedge_and_forget_pnl",ms_contenders$alias[1],colnames(hf_pnl_table))

	dhe_table <- ms_trade_error %>% filter(last_position!=0) %>% select(OptionSymbol,DataDate,Type,cumulative_pnl_to_last)
	colnames(dhe_table) <- gsub("cumulative_pnl_to_last",ms_contenders$alias[1],colnames(dhe_table))

	arpe_table <- ms_trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,Type,ARPE)
	colnames(arpe_table) <- gsub("ARPE",ms_contenders$alias[1],colnames(arpe_table))

	rpe_table <- ms_trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,Type,RPE)
	colnames(rpe_table) <- gsub("RPE",ms_contenders$alias[1],colnames(rpe_table))

	ape_table <- ms_trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,Type,APE)
	colnames(ape_table) <- gsub("APE",ms_contenders$alias[1],colnames(ape_table))

	if(nrow(ms_contenders)>1){
		for(i in 2:nrow(ms_contenders)){

			load(paste0(main_path,"Output Files/Trading_and_Error/",underlying_asset,"_",data_year,"_","MS_data_withdiv_",ms_contenders$training_error_obj[i],"_",ms_contenders$training_error_type[i],"_",ms_contenders$ms_code[i],"_",ms_contenders$lookback[i],"y_lookback_trade_and_error.RData"))

			position_table <- ms_trade_error %>% select(OptionSymbol,DataDate,naked_position) %>% left_join(position_table,.,by=c("OptionSymbol","DataDate"))
			colnames(position_table) <- gsub("naked_position",ms_contenders$alias[i],colnames(position_table))

			cumulative_position_table <- ms_trade_error %>% mutate(cumulative_position=cumulative_position+last_position) %>% select(OptionSymbol,DataDate,cumulative_position) %>% left_join(cumulative_position_table,.,by=c("OptionSymbol","DataDate"))
			colnames(cumulative_position_table) <- gsub("cumulative_position",ms_contenders$alias[i],colnames(cumulative_position_table)) 

			delta_table <- ms_trade_error %>% select(OptionSymbol,DataDate,model_delta) %>% left_join(delta_table,.,by=c("OptionSymbol","DataDate"))
			colnames(delta_table) <- gsub("model_delta",ms_contenders$alias[i],colnames(delta_table))

			pnl_table <- ms_trade_error %>% select(OptionSymbol,DataDate,naked_pnl) %>% left_join(pnl_table,.,by=c("OptionSymbol","DataDate"))
			colnames(pnl_table) <- gsub("naked_pnl",ms_contenders$alias[i],colnames(pnl_table))

			delta_pnl_table <- ms_trade_error %>% select(OptionSymbol,DataDate,delta_pnl) %>% left_join(delta_pnl_table,.,by=c("OptionSymbol","DataDate"))
			colnames(delta_pnl_table) <- gsub("delta_pnl",ms_contenders$alias[i],colnames(delta_pnl_table))

			hf_pnl_table <- ms_trade_error %>% select(OptionSymbol,DataDate,hedge_and_forget_pnl) %>% left_join(hf_pnl_table,.,by=c("OptionSymbol","DataDate"))
			colnames(hf_pnl_table) <- gsub("hedge_and_forget_pnl",ms_contenders$alias[i],colnames(hf_pnl_table))

			dhe_table <- ms_trade_error %>% filter(last_position!=0) %>% select(OptionSymbol,DataDate,cumulative_pnl_to_last) %>% left_join(dhe_table,.,by=c("OptionSymbol","DataDate"))
			colnames(dhe_table) <- gsub("cumulative_pnl_to_last",ms_contenders$alias[i],colnames(dhe_table))

			arpe_table <- ms_trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,ARPE) %>% left_join(arpe_table,.,by=c("OptionSymbol","DataDate"))
			colnames(arpe_table) <- gsub("ARPE",ms_contenders$alias[i],colnames(arpe_table))

			rpe_table <- ms_trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,RPE) %>% left_join(rpe_table,.,by=c("OptionSymbol","DataDate"))
			colnames(rpe_table) <- gsub("RPE",ms_contenders$alias[i],colnames(rpe_table))

			ape_table <- ms_trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,APE) %>% left_join(ape_table,.,by=c("OptionSymbol","DataDate"))
			colnames(ape_table) <- gsub("APE",ms_contenders$alias[i],colnames(ape_table))						
		}
	}


	contender_names<- contenders %>% mutate(parameter_period=paste0(parameter_period,"y")) %>% apply(.,1,paste0,collapse="_") %>% gsub("_$","",.) 
	for(i in 1:nrow(contenders)){
		#Get the RData File
		load(paste0(main_path,"Output Files/Trading_and_Error/",underlying_asset,"_",data_year,"_",contenders$model_name[i],"_data_withdiv_",contenders$sym_abbr[i],contenders$parameter_period[i],"y_trade_and_error.RData"))

		position_table <- trade_error %>% select(OptionSymbol,DataDate,naked_position) %>% left_join(position_table,.,by=c("OptionSymbol","DataDate"))
		colnames(position_table) <- gsub("naked_position",contender_names[i],colnames(position_table))

		cumulative_position_table <- trade_error %>% mutate(cumulative_position=cumulative_position+last_position) %>% select(OptionSymbol,DataDate,cumulative_position) %>% left_join(cumulative_position_table,.,by=c("OptionSymbol","DataDate"))
		colnames(cumulative_position_table) <- gsub("cumulative_position",contender_names[i],colnames(cumulative_position_table)) 

		delta_table <- trade_error %>% select(OptionSymbol,DataDate,model_delta) %>% left_join(delta_table,.,by=c("OptionSymbol","DataDate"))
		colnames(delta_table) <- gsub("model_delta",contender_names[i],colnames(delta_table))

		pnl_table <- trade_error %>% select(OptionSymbol,DataDate,naked_pnl) %>% left_join(pnl_table,.,by=c("OptionSymbol","DataDate"))
		colnames(pnl_table) <- gsub("naked_pnl",contender_names[i],colnames(pnl_table))

		delta_pnl_table <- trade_error %>% select(OptionSymbol,DataDate,delta_pnl) %>% left_join(delta_pnl_table,.,by=c("OptionSymbol","DataDate"))
		colnames(delta_pnl_table) <- gsub("delta_pnl",contender_names[i],colnames(delta_pnl_table))

		hf_pnl_table <- trade_error %>% select(OptionSymbol,DataDate,hedge_and_forget_pnl) %>% left_join(hf_pnl_table,.,by=c("OptionSymbol","DataDate"))
		colnames(hf_pnl_table) <- gsub("hedge_and_forget_pnl",contender_names[i],colnames(hf_pnl_table))

		dhe_table <- trade_error %>% filter(last_position!=0) %>% select(OptionSymbol,DataDate,cumulative_pnl_to_last) %>% left_join(dhe_table,.,by=c("OptionSymbol","DataDate"))
		colnames(dhe_table) <- gsub("cumulative_pnl_to_last",contender_names[i],colnames(dhe_table))

		arpe_table <- trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,ARPE) %>% left_join(arpe_table,.,by=c("OptionSymbol","DataDate"))
		colnames(arpe_table) <- gsub("ARPE",contender_names[i],colnames(arpe_table))

		rpe_table <- trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,RPE) %>% left_join(rpe_table,.,by=c("OptionSymbol","DataDate"))
		colnames(rpe_table) <- gsub("RPE",contender_names[i],colnames(rpe_table))

		ape_table <- trade_error %>% filter(!is.na(Last)) %>% select(OptionSymbol,DataDate,APE) %>% left_join(ape_table,.,by=c("OptionSymbol","DataDate"))
		colnames(ape_table) <- gsub("APE",contender_names[i],colnames(ape_table))	
	}

	#Long Calls
	position_summary_table <- position_table %>% summarise_each(funs(sum(.==1 & Type=="call")),-OptionSymbol,-DataDate,-Type,-Last) 
	#Short Calls
	position_summary_table <- position_table %>% summarise_each(funs(sum(.==-1 & Type=="call")),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(position_summary_table,.)
	#Long Puts
	position_summary_table <- position_table %>% summarise_each(funs(sum(.==1 & Type=="put")),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(position_summary_table,.)
	#Short Puts
	position_summary_table <- position_table %>% summarise_each(funs(sum(.==-1 & Type=="put")),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(position_summary_table,.)
	#Total Longs
	position_summary_table <- position_table %>% summarise_each(funs(sum(.==1)),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(position_summary_table,.)
	#Total Shorts
	position_summary_table <- position_table %>% summarise_each(funs(sum(.==-1)),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(position_summary_table,.)
	#Total Long Investment
	position_summary_table <- position_table %>% summarise_each(funs(sum((.==1)*Last)),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(position_summary_table,.)
	#Total Short Capital
	position_summary_table <- position_table %>% summarise_each(funs(sum((.==-1)*Last)),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(position_summary_table,.)

	rm(position_table)
	gc()

	#Total Non-negative Contracts
	position_summary_table <- pnl_table %>% summarise_each(funs(sum(.>=0)),-OptionSymbol,-DataDate) %>% rbind(position_summary_table,.)
	#Total Profit
	position_summary_table <- pnl_table %>% summarise_each(funs(sum((.>=0)*.)),-OptionSymbol,-DataDate) %>% rbind(position_summary_table,.)
	#Total Negative Contracts
	position_summary_table <- pnl_table %>% summarise_each(funs(sum(.<0)),-OptionSymbol,-DataDate) %>% rbind(position_summary_table,.)
	#Total Loss
	position_summary_table <- pnl_table %>% summarise_each(funs(sum((.<0)*.)),-OptionSymbol,-DataDate) %>% rbind(position_summary_table,.)
	#P&L
	position_summary_table <- pnl_table %>% summarise_each(funs(sum(.)),-OptionSymbol,-DataDate) %>% rbind(position_summary_table,.)

	rownames(position_summary_table)<-c("Long Calls",
								"Short Calls",
								"Long Puts",
								"Short Puts",
								"Total Longs",
								"Total Shorts",
								"Total Naked Long Investment",
								"Total Naked Short Capital",
								"Total Naked Nonnegative Contracts",
								"Total Naked Profit",
								"Total Naked Negative Contracts",
								"Total Naked Loss",
								"Naked P&L")

	#Calls Change to Long
	cumulative_position_summary_table <- cumulative_position_table %>% summarise_each(funs(sum(.==2 & Type=="call")),-OptionSymbol,-DataDate,-Type,-Last) 
	#Calls Change to Short
	cumulative_position_summary_table <- cumulative_position_table %>% summarise_each(funs(sum(.==-2 & Type=="call")),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(cumulative_position_summary_table,.)
	#Puts Change to Long
	cumulative_position_summary_table <- cumulative_position_table %>% summarise_each(funs(sum(.==2 & Type=="put")),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(cumulative_position_summary_table,.)
	#Puts Change to Short
	cumulative_position_summary_table <- cumulative_position_table %>% summarise_each(funs(sum(.==-2 & Type=="put")),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(cumulative_position_summary_table,.)
	#Total Calls Position Change
	cumulative_position_summary_table <- cumulative_position_table %>% summarise_each(funs(sum(abs(.)==2 & Type=="call")),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(cumulative_position_summary_table,.)
	#Total Puts Position Change
	cumulative_position_summary_table <- cumulative_position_table %>% summarise_each(funs(sum(abs(.)==2 & Type=="put")),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(cumulative_position_summary_table,.)
	#Total Change to Long
	cumulative_position_summary_table <- cumulative_position_table %>% summarise_each(funs(sum(.==2)),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(cumulative_position_summary_table,.)
	#Total Change to Short
	cumulative_position_summary_table <- cumulative_position_table %>% summarise_each(funs(sum(.==-2)),-OptionSymbol,-DataDate,-Type,-Last) %>% rbind(cumulative_position_summary_table,.)

	rm(cumulative_position_table)
	gc()

	rownames(cumulative_position_summary_table)<-c("Calls Change to Long",
								"Calls Change to Short",
								"Puts Change to Long",
								"Puts Change to Short",
								"Total Calls Position Change",
								"Total Puts Position Change",
								"Total Change to Long",
								"Total Change to Short")

	#Calls Median Delta
	delta_summary_table <- delta_table %>% filter(Type=="call") %>% summarise_each(funs(median(abs(.))),-OptionSymbol,-DataDate,-Type) 
	#Puts Median Delta
	delta_summary_table <- delta_table %>% filter(Type=="put") %>% summarise_each(funs(median(abs(.))),-OptionSymbol,-DataDate,-Type) %>% rbind(delta_summary_table,.)
	#Total Median Delta
	delta_summary_table <- delta_table %>% summarise_each(funs(median(abs(.))),-OptionSymbol,-DataDate,-Type) %>% rbind(delta_summary_table,.)

	rm(delta_table)
	gc()

	rownames(delta_summary_table)<-c("Calls Median Delta",
								"Puts Median Delta",
								"Aggregate Median Delta")


	#Show profits and loss indicators
	pnl_summary <- pnl_table %>% mutate_each(funs(ifelse(.>=0,1,0)),-OptionSymbol,-DataDate) %>% mutate(consensus=rowSums(.[-(1:2)])) 
	gc()
	#Total Consensus
	pnl_summary_table <- pnl_summary %>% summarise_each(funs(sum(.==1 & consensus == nrow(contenders)+1)),-OptionSymbol,-DataDate,-consensus)
	gc()
	#Total Wins
	pnl_summary_table <- pnl_summary %>% summarise_each(funs(sum(.==1 & consensus > 0 & consensus < nrow(contenders)+1)),-OptionSymbol,-DataDate,-consensus) %>%	rbind(pnl_summary_table,.)
	gc()
	#Total Losses
	pnl_summary_table <- pnl_summary %>% summarise_each(funs(sum(.==0 & consensus > 0 & consensus < nrow(contenders)+1)),-OptionSymbol,-DataDate,-consensus) %>%	rbind(pnl_summary_table,.)
	gc()
	#Total Doom
	pnl_summary_table <- pnl_summary %>% summarise_each(funs(sum(.==0 & consensus == 0)),-OptionSymbol,-DataDate,-consensus) %>% rbind(pnl_summary_table,.)
	gc()

	
	#Add position consensus stuff
	pnl_summary<- pnl_summary %>% select(OptionSymbol,DataDate,consensus) %>% arrange(OptionSymbol,DataDate)
	gc()
	pnl_table <- pnl_table %>% left_join(.,pnl_summary,by=c("OptionSymbol","DataDate")) 

	rm(pnl_summary)
	gc()

	#Total Consensus Dollars
	pnl_summary_table <- pnl_table %>% summarise_each(funs(sum(.*(.>=0 & consensus==nrow(contenders)+1))),-OptionSymbol,-DataDate,-consensus) %>% rbind(pnl_summary_table,.)
	#Total Wins Dollars
	pnl_summary_table <- pnl_table %>% summarise_each(funs(sum(.*(.>=0 & consensus >0 & consensus < nrow(contenders)+1))),-OptionSymbol,-DataDate,-consensus) %>% rbind(pnl_summary_table,.)
	#Total Losses Dollars
	pnl_summary_table <- pnl_table %>% summarise_each(funs(sum(.*(.<0 & consensus >0 & consensus < nrow(contenders)+1))),-OptionSymbol,-DataDate,-consensus) %>% rbind(pnl_summary_table,.)
	#Total Doom Dollars
	pnl_summary_table <- pnl_table %>% summarise_each(funs(sum(.*(.<0 & consensus == 0))),-OptionSymbol,-DataDate,-consensus) %>% rbind(pnl_summary_table,.) 

	rm(pnl_table)
	gc()

	rownames(pnl_summary_table)<-c(	"Total Naked Consensus",
									"Total Naked Wins",
									"Total Naked Losses",
									"Total Naked Doom",
									"Total Naked Consensus Dollars",
									"Total Naked Wins Dollars",
									"Total Naked Losses Dollars",
									"Total Naked Doom Dollars")

	#Delta Call Profits
	delta_pnl_summary_table <- delta_pnl_table %>% filter(Type=="call") %>% summarise_each(funs(sum((.>=0)*.)),-OptionSymbol,-DataDate,-Type) 
	#Delta Call Losses
	delta_pnl_summary_table <- delta_pnl_table %>% filter(Type=="call") %>% summarise_each(funs(sum((.<0)*.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(delta_pnl_summary_table,.)
	#Delta Put Profits
	delta_pnl_summary_table <- delta_pnl_table %>% filter(Type=="put") %>% summarise_each(funs(sum((.>=0)*.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(delta_pnl_summary_table,.)
	#Delta Put Losses
	delta_pnl_summary_table <- delta_pnl_table %>% filter(Type=="put") %>% summarise_each(funs(sum((.<0)*.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(delta_pnl_summary_table,.)
	#Delta Total Profits
	delta_pnl_summary_table <- delta_pnl_table %>% summarise_each(funs(sum((.>=0)*.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(delta_pnl_summary_table,.)
	#Delta Total Losses
	delta_pnl_summary_table <- delta_pnl_table %>% summarise_each(funs(sum((.<0)*.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(delta_pnl_summary_table,.)
	#Delta P&L
	delta_pnl_summary_table <- delta_pnl_table %>% summarise_each(funs(sum(.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(delta_pnl_summary_table,.)

	rm(delta_pnl_table)
	gc()

	rownames(delta_pnl_summary_table)<-c(	"Delta Call Profits",
									"Delta Call Losses",
									"Delta Put Profits",
									"Delta Put Losses",
									"Delta Total Profits",
									"Delta Total Losses",
									"Delta P&L")

	#HF Call Profits
	hf_pnl_summary_table <- hf_pnl_table %>% filter(Type=="call") %>% summarise_each(funs(sum((.>=0)*.)),-OptionSymbol,-DataDate,-Type) 
	#HF Call Losses
	hf_pnl_summary_table <- hf_pnl_table %>% filter(Type=="call") %>% summarise_each(funs(sum((.<0)*.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(hf_pnl_summary_table,.)
	#HF Put Profits
	hf_pnl_summary_table <- hf_pnl_table %>% filter(Type=="put") %>% summarise_each(funs(sum((.>=0)*.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(hf_pnl_summary_table,.)
	#HF Put Losses
	hf_pnl_summary_table <- hf_pnl_table %>% filter(Type=="put") %>% summarise_each(funs(sum((.<0)*.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(hf_pnl_summary_table,.)
	#HF Total Profits
	hf_pnl_summary_table <- hf_pnl_table %>% summarise_each(funs(sum((.>=0)*.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(hf_pnl_summary_table,.)
	#HF Total Losses
	hf_pnl_summary_table <- hf_pnl_table %>% summarise_each(funs(sum((.<0)*.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(hf_pnl_summary_table,.)
	#HF P&L
	hf_pnl_summary_table <- hf_pnl_table %>% summarise_each(funs(sum(.)),-OptionSymbol,-DataDate,-Type)  %>% rbind(hf_pnl_summary_table,.)

	rm(hf_pnl_table)
	gc()

	rownames(hf_pnl_summary_table)<-c(	"HF Call Profits",
									"HF Call Losses",
									"HF Put Profits",
									"HF Put Losses",
									"HF Total Profits",
									"HF Total Losses",
									"HF P&L")

	#Total DHE
	dhe_summary_table <- dhe_table %>% summarise_each(funs(sum(.)),-OptionSymbol,-DataDate,-Type) 
	dhe_summary_table <- dhe_table %>% summarise_each(funs(mean(.)),-OptionSymbol,-DataDate,-Type) %>% rbind(dhe_summary_table,.)
	dhe_summary_table <- dhe_table %>% summarise_each(funs(sd(.)),-OptionSymbol,-DataDate,-Type) %>% rbind(dhe_summary_table,.)
	dhe_summary_table <- dhe_table %>% summarise_each(funs(median(.)),-OptionSymbol,-DataDate,-Type) %>% rbind(dhe_summary_table,.)
	rownames(dhe_summary_table)<-c("Total DHE",
									"Mean DHE",
									"SD DHE",
									"Median DHE")

	rm(dhe_table)
	gc()

	pricing_error_summary_table <- arpe_table %>% summarise_each(funs(mean(.)),-OptionSymbol,-DataDate,-Type)
	pricing_error_summary_table <- rpe_table %>% summarise_each(funs(mean(.)),-OptionSymbol,-DataDate,-Type) %>% rbind(pricing_error_summary_table,.)
	pricing_error_summary_table <- ape_table %>% summarise_each(funs(mean(.)),-OptionSymbol,-DataDate,-Type) %>% rbind(pricing_error_summary_table,.)

	rownames(pricing_error_summary_table)<-c("ARPE",
									"RPE",
									"APE")

	benchmark_table<- rbind(position_summary_table,
							cumulative_position_summary_table,
							delta_summary_table,
							pnl_summary_table,
							delta_pnl_summary_table,
							hf_pnl_summary_table,
							dhe_summary_table,
							pricing_error_summary_table)


	benchmark_table <- as.data.frame(benchmark_table)

	save(benchmark_table,file=paste0(main_path,"Output Files/Benchmarks/benchmark_summary_",underlying_asset,"_",data_year,"_benchmark_",benchmark_alias,"_",nrow(ms_contenders),"_ms_",nrow(contenders),"_ind.RData"))
	write.xlsx2(benchmark_table,path.expand(paste0(main_path,"Output Files/Benchmarks/benchmark_summary_",underlying_asset,"_",data_year,"_benchmark_",benchmark_alias,"_",nrow(ms_contenders),"_ms_",nrow(contenders),"_ind.xlsx")),row.names=TRUE)
}


ms_contenders<-ms_table<-data.frame(training_error_type=c("naked_pnl","hedge_and_forget_pnl","naked_pnl","hedge_and_forget_pnl","ARPE","RPE","APE"),
									training_error_obj=c("min_abs","min_abs","max","max","min_abs","min_abs","min_abs"),
									ms_code=c("5_models_v01","5_models_v01","5_models_v01","5_models_v01","5_models_v01","5_models_v01","5_models_v01"),
									# lookback=rep(2,7),
									# alias=c("naked_minabs_2y_v01","hf_minabs_2y_v01","naked_max_2y_v01","hf_max_2y_v01","arpe_minabs_2y_v01","rpe_minabs_2y_v01","ape_minabs_2y_v01"))
									lookback=rep(1,7),
									alias=c("naked_minabs_1y_v01","hf_minabs_1y_v01","naked_max_1y_v01","hf_max_1y_v01","arpe_minabs_1y_v01","rpe_minabs_1y_v01","ape_minabs_1y_v01"))

contenders<-data.frame(model_name=c("BS","BS","HN","HN","HN"),parameter_period=c(2,5,2,2,5),sym_abbr=c("","","symm_","asym_","asym_"))

for(years in 2009:2013){
	print(years)
	get_summary(contenders,ms_contenders,underlying_asset="SPX",data_year=years,benchmark_alias="b001")
	get_summary(contenders,ms_contenders,underlying_asset="NDX",data_year=years,benchmark_alias="b001")

}


ms_contenders<-ms_table<-data.frame(training_error_type=c("naked_pnl","hedge_and_forget_pnl","naked_pnl","hedge_and_forget_pnl","ARPE","RPE","APE"),
									training_error_obj=c("min_abs","min_abs","max","max","min_abs","min_abs","min_abs"),
									ms_code=c("5_models_v01","5_models_v01","5_models_v01","5_models_v01","5_models_v01","5_models_v01","5_models_v01"),
									lookback=rep(2,7),
									alias=c("naked_minabs_2y_v01","hf_minabs_2y_v01","naked_max_2y_v01","hf_max_2y_v01","arpe_minabs_2y_v01","rpe_minabs_2y_v01","ape_minabs_2y_v01"))
									# lookback=rep(1,7),
									# alias=c("naked_minabs_1y_v01","hf_minabs_1y_v01","naked_max_1y_v01","hf_max_1y_v01","arpe_minabs_1y_v01","rpe_minabs_1y_v01","ape_minabs_1y_v01"))

contenders<-data.frame(model_name=c("BS","BS","HN","HN","HN"),parameter_period=c(2,5,2,2,5),sym_abbr=c("","","symm_","asym_","asym_"))

for(years in 2010:2013){
	print(years)
	get_summary(contenders,ms_contenders,underlying_asset="SPX",data_year=years,benchmark_alias="b002")
	get_summary(contenders,ms_contenders,underlying_asset="NDX",data_year=years,benchmark_alias="b002")

}





