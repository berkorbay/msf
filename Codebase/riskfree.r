#
###
#########
#DELETE THIS BEFORE PUBLISHING!!!!!
#And the one in the code
#My quandl auth token Wp59hYhH8drdSbyrmZ2J
#########
###
#



if(!"Quandl"%in%installed.packages()){
	install.packages("Quandl", repos="http://cran.rstudio.com/")
}
#Call quandl
#library(Quandl)

#This is to install from github
install.packages("devtools", repos="http://cran.rstudio.com/")
library(devtools)
install_github('R-package','Quandl')
library(Quandl)


#
###
#########
#DELETE THIS BEFORE PUBLISHING!!!!!
Quandl.auth("Wp59hYhH8drdSbyrmZ2J") #My quandl auth token Wp59hYhH8drdSbyrmZ2J
#########
###
# 
TreasuryYields<-Quandl("USTREASURY/YIELD", trim_start="1990-01-02", trim_end="2014-07-17", authcode="Wp59hYhH8drdSbyrmZ2J", type="raw")
TreasuryYields[,-1]<-TreasuryYields[,-1]/100

rf_fun<-splinefun(periods<-c(1/12,3/12,6/12,1,2,3,5,7,10,20,30),TreasuryYields[25,-1])

#This code is to export TreasuryYields to a csv file
#The colnames are in terms of months
colnames(TreasuryYields)<-c("Date",12*c(1/12,3/12,6/12,1,2,3,5,7,10,20,30))
write.table(TreasuryYields,"~/Dropbox/PhD_Workshop/Input Files/Other/TreasuryYields.csv",row.names=FALSE,sep=",")