#Data restructuring for B12

main_path <- "~/Dropbox/PhD_Workshop/"
source(paste0(main_path,"Codebase/00_run_this_first_option_calculations.r"))


#Don't run this again
input_path="Input Files/"
prices_path="Asset Prices/"
rates_path="Other/"

option_csv_files<- dir(paste0(main_path,input_path,"Asset Options/"),pattern="options.csv",full.names=TRUE)

for(i in 1:length(option_csv_files)){

	print(option_csv_files[i])
	raw_data<-read.csv(option_csv_files[i],header=TRUE) %>% tbl_df %>%
			select(-OptionExt) %>%
			mutate(Expiration=as.Date(Expiration,format=ifelse(nchar(as.character(Expiration))==8,"%m/%d/%y","%m/%d/%Y")),
				   DataDate=as.Date(DataDate,format=ifelse(nchar(as.character(DataDate))==8,"%m/%d/%y","%m/%d/%Y")))

	save(raw_data,file=gsub(".csv",".RData",option_csv_files[i]))

	raw_data %>% summarise(min_datadate=min(DataDate),max_datadate=max(DataDate),min_expiration=min(Expiration),max_expiration=max(Expiration)) %>% print

}


asset_csv_files<- dir(paste0(main_path,input_path,prices_path),pattern="_daily_processed.csv",full.names=TRUE)
i<-3
asset_price_data <- read.csv(asset_csv_files[i],header=TRUE) %>% tbl_df %>%
						mutate(DataDate=as.Date(Date),
								UnderlyingPrice=round(Adj.Close,2)) %>% 
						select(DataDate,UnderlyingPrice,Open:Dividend.Yield) %>%
						select(-Adj.Close)
save(asset_price_data,file=gsub(".csv",".RData",asset_csv_files[i]))


treasury_rates<- read.csv("~/Dropbox/PhD_Workshop/Input Files/Other/TreasuryYields.csv",header=TRUE) %>% tbl_df %>% 
					mutate(DataDate=as.Date(Date,format=ifelse(nchar(as.character(Date))==8,"%d/%m/%y","%d/%m/%Y"))) %>%
					select(DataDate,X1:X360) %>% 
					mutate(X1=ifelse(is.na(X1),X3,X1),X240=ifelse(is.na(X240),X120,X240),X360=ifelse(is.na(X360),X240,X360))

save(treasury_rates,file="~/Dropbox/PhD_Workshop/Input Files/Other/TreasuryYields.RData")

