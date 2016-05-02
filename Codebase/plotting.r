
library(lattice)
library(ggplot2)
library(akima)
library(rgl)
main_path<-"~/Dropbox/PhD_Workshop/"

option_path<-"Input Files/Asset Options/SPX_2012_options_filtered_A12.csv"

options(stringsAsFactors=FALSE)

option_table<-read.table(paste0(main_path,option_path),sep=",",header=TRUE)

chosen<-as.Date(option_table$DataDate)=="2012-04-02"

opt_2<-option_table[chosen,]
opt_2<-subset(opt_2,Type=="call")



#wireframe(IV ~ Moneyness * as.numeric(NetMaturity), data=opt_2,aspect = c(1, 1),light.source = c(10,10,10),scales = list(arrows=FALSE, col="black", font=10, tck=0.5))

#######
s<-interp(unlist(opt_2$NetMaturity), unlist(opt_2$Moneyness), unlist(opt_2$IV), xo=sort(unlist(opt_2$NetMaturity)), 
	yo=sort(unlist(opt_2$Moneyness)), duplicate="mean",linear=TRUE)

s$z[is.na(s$z)]<-0

wireframe(as.matrix(s$z),row.values=s$x,column.values=s$y,aspect = c(1, 1),light.source = c(10,10,10))
######

# sq<-as.data.frame(s)

# surface3d(s$x,s$y,s$z*100,ambient="purple")
bs_path<-"Output Files/Black_Scholes/SPX_2012_BS_data.csv"
bs_table<-read.table(paste0(main_path,bs_path),sep=",",header=TRUE)

opt_3<-data.frame(option_table,BS.HV2y=bs_table$BS.HV2y)

opt_3<-subset(opt_3,Type=="call",)

opt_3<-opt_3[order(abs(opt_3$Moneyness-1)),]

opt_3<-opt_3[!duplicated(as.Date(opt_3$DataDate)),]

opt_3<-opt_3[order(as.Date(opt_3$DataDate)),]

ggplot(data=opt_3,aes(as.Date(DataDate))) + geom_line(aes(y=Last, color="Last")) + geom_line(aes(y=BS.HV2y, color="BS.HV2y")) + geom_point(aes(y=Last, color="Last")) + geom_point(aes(y=BS.HV2y, color="BS.HV2y"))

# opt_3<-opt_3[(opt_3$OptionSymbol=="SPX120317C01315000"),]
# opt_3$DataDate<-as.Date(opt_3$DataDate)
# ggplot(opt_3, aes(x=DataDate, y=Last)+geom_line())

hn_path<-"Output Files/HestonNandi_GARCH/SPX_2012_HN_data.csv"
hn_table<-read.table(paste0(main_path,hn_path),sep=",",header=TRUE)

opt_4<-data.frame(option_table,BS.HV2y=bs_table$BS.HV2y,HN_prices=hn_table$HN_prices)

opt_4<-subset(opt_4,Type=="call",)

opt_4<-subset(opt_4, Maturity<=100 & Maturity>=80)

opt_4<-opt_4[order(abs(opt_4$Moneyness-0.8)),]

opt_4<-opt_4[!duplicated(as.Date(opt_4$DataDate)),]

opt_4<-opt_4[order(as.Date(opt_4$DataDate)),]

ggplot(data=opt_5,aes(as.Date(DataDate))) + geom_line(aes(y=Last, color="Last")) + geom_line(aes(y=BS.HV2y, color="BS.HV2y")) + geom_line(aes(y=HN_prices, color="HN_prices")) + geom_line(aes(y=ExpirationPayoff, color="ExpirationPayoff")) + 
	geom_point(aes(y=Last, color="Last")) + geom_point(aes(y=BS.HV2y, color="BS.HV2y")) + geom_point(aes(y=HN_prices, color="HN_prices")) + geom_point(aes(y=ExpirationPayoff, color="ExpirationPayoff")) +
	theme(#axis.line=element_blank(),
      #axis.text.x=element_blank(),
      #axis.text.y=element_blank(),
      #axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) + labs(color = "Source")
      #legend.position="none",
      #panel.background=element_blank(),
      #panel.border=element_blank(),
      #panel.grid.major=element_blank(),
      #panel.grid.minor=element_blank(),
      #plot.background=element_blank()
 

asset_table<-read.table(paste0(main_path,"Input Files/Asset Prices/SPX_daily_processed.csv"),header=TRUE,sep=",")	
asset_table$Date<-as.Date(asset_table$Date)
asset_subset<-subset(asset_table,Date>="2012-01-01" & Date<"2013-01-01")
ggplot(data=asset_subset,aes(x=as.numeric(Date))) + geom_line(aes(y=Close, color="Close")) + geom_point(aes(y=Close, color="Close")) + theme(legend.position="none") + 
	scale_x_continuous(breaks = as.numeric(seq(as.Date("2012-01-01"),as.Date("2013-01-01"),by="month")), labels = format(seq(as.Date("2012-01-01"),as.Date("2013-01-01"),by="month"), format = "%Y-%m"))


ggplot(data=opt_4,aes(as.Date(DataDate))) + geom_line(aes(y=Last/Last, color="Last")) + geom_line(aes(y=BS.HV2y/Last, color="BS.HV2y")) + geom_line(aes(y=HN_prices/Last, color="HN_prices")) + 
	geom_point(aes(y=Last/Last, color="Last")) + geom_point(aes(y=BS.HV2y/Last, color="BS.HV2y")) + geom_point(aes(y=HN_prices/Last, color="HN_prices")) +
	theme(#axis.line=element_blank(),
      #axis.text.x=element_blank(),
      #axis.text.y=element_blank(),
      #axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) + labs(color = "Source")
      #legend.position="none",
      #panel.background=element_blank(),
      #panel.border=element_blank(),
      #panel.grid.major=element_blank(),
      #panel.grid.minor=element_blank(),
      #plot.background=element_blank()

opt_5<-opt_4[!duplicated(opt_4$OptionSymbol),]


ggplot(data=opt_5,aes(as.Date(DataDate))) + geom_line(aes(y=Last, color="Last")) + geom_line(aes(y=BS.HV2y, color="BS.HV2y")) + geom_line(aes(y=ExpirationPayoff, color="ExpirationPayoff")) + 
	geom_point(aes(y=Last, color="Last")) + geom_point(aes(y=BS.HV2y, color="BS.HV2y")) + geom_point(aes(y=ExpirationPayoff, color="ExpirationPayoff")) +
	theme(#axis.line=element_blank(),
      #axis.text.x=element_blank(),
      #axis.text.y=element_blank(),
      #axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) + labs(color = "Source")
      #legend.position="none",
      #panel.background=element_blank(),
      #panel.border=element_blank(),
      #panel.grid.major=element_blank(),
      #panel.grid.minor=element_blank(),
      #plot.background=element_blank()


