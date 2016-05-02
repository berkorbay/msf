#This is the control node for the r functions
#It is supposed to bring many different functions under one control page


data_raw_to_finalized<-function(main_path="~/Dropbox/PhD_Workshop/",finalizeWhat="both",data_year=2013,underlying_asset="SPX",progressOutput=TRUE,restruct.data=TRUE,process.data=TRUE,filter.data=TRUE,discreteDividend=TRUE,process.asset=TRUE,include.histvols=TRUE){
	source(paste0(main_path,"Codebase/datagroom.r"))

	if(finalizeWhat=="option"||finalizeWhat=="options"||finalizeWhat=="both"){
		#Load the required functions
		if(restruct.data){
			print("Starting restructuring data")
			data_restruct(data_path="~/local_projects/HDALL/",data_type="options",data_year=data_year,underlying_asset=underlying_asset,output_path=paste0(main_path,"Input Files/Asset Options/"),progressOutput=progressOutput)
		}
		if(process.data){
			print("Starting processing data")
			process_data(data_path=paste0(main_path,"Input Files/Asset Options/"),data_type="options",data_year=data_year,underlying_asset=underlying_asset,output_path=paste0(main_path,"Input Files/Asset Options/"),progressOutput=progressOutput)
		}
		if(filter.data){
			print("Starting filtering data")
			filter_rule_A1(data_path=paste0(main_path,"Input Files/Asset Options/"),data_type="options",data_suffix="_processed",data_year=data_year,underlying_asset=underlying_asset,output_path=paste0(main_path,"Input Files/Asset Options/"),progressOutput=progressOutput)
		}		
	}else{
		print("Wrong finalizeWhat input. It should either be option, asset, or both")
	}

	if(finalizeWhat=="asset"||finalizeWhat=="stock"||finalizeWhat=="both"){
		if(process.asset){
			process_asset_price_data(underlying_asset=underlying_asset,discreteDividend=discreteDividend)
		}
		if(include.histvols){
			add_hist_vols(underlying_asset=underlying_asset)
		}
	}
}


data_raw_to_finalized(main_path="~/Dropbox/PhD_Workshop/",finalizeWhat="option",underlying_asset="SPX",data_year=2007,progressOutput=TRUE,restruct.data=TRUE,process.data=TRUE,filter.data=TRUE,discreteDividend=FALSE,process.asset=TRUE,include.histvols=TRUE)
data_raw_to_finalized(main_path="~/Dropbox/PhD_Workshop/",finalizeWhat="option",underlying_asset="SPX",data_year=2006,progressOutput=TRUE,restruct.data=TRUE,process.data=TRUE,filter.data=TRUE,discreteDividend=FALSE,process.asset=TRUE,include.histvols=TRUE)

data_raw_to_finalized(main_path="~/Dropbox/PhD_Workshop/",finalizeWhat="option",underlying_asset="NDX",data_year=2013,progressOutput=TRUE,restruct.data=TRUE,process.data=TRUE,filter.data=TRUE,discreteDividend=FALSE,process.asset=TRUE,include.histvols=TRUE)
data_raw_to_finalized(main_path="~/Dropbox/PhD_Workshop/",finalizeWhat="option",underlying_asset="NDX",data_year=2012,progressOutput=TRUE,restruct.data=TRUE,process.data=TRUE,filter.data=TRUE,discreteDividend=FALSE,process.asset=TRUE,include.histvols=TRUE)
data_raw_to_finalized(main_path="~/Dropbox/PhD_Workshop/",finalizeWhat="option",underlying_asset="NDX",data_year=2011,progressOutput=TRUE,restruct.data=TRUE,process.data=TRUE,filter.data=TRUE,discreteDividend=FALSE,process.asset=TRUE,include.histvols=TRUE)
data_raw_to_finalized(main_path="~/Dropbox/PhD_Workshop/",finalizeWhat="option",underlying_asset="NDX",data_year=2010,progressOutput=TRUE,restruct.data=TRUE,process.data=TRUE,filter.data=TRUE,discreteDividend=FALSE,process.asset=TRUE,include.histvols=TRUE)
data_raw_to_finalized(main_path="~/Dropbox/PhD_Workshop/",finalizeWhat="option",underlying_asset="NDX",data_year=2009,progressOutput=TRUE,restruct.data=TRUE,process.data=TRUE,filter.data=TRUE,discreteDividend=FALSE,process.asset=TRUE,include.histvols=TRUE)
data_raw_to_finalized(main_path="~/Dropbox/PhD_Workshop/",finalizeWhat="option",underlying_asset="NDX",data_year=2008,progressOutput=TRUE,restruct.data=TRUE,process.data=TRUE,filter.data=TRUE,discreteDividend=FALSE,process.asset=TRUE,include.histvols=TRUE)
