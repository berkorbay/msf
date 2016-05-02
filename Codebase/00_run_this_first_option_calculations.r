#R Code for option raw header
#Start this first

rm(list=ls(all=TRUE))
gc()
options(stringsAsFactors=FALSE)
options(repos="http://cran.rstudio.com/") #Set default repo
options(dplyr.width = Inf) #See all dplyr data table columns
options(scipen = 7)

#Remove predetermined seed if exists
if(exists(".Random.seed"))
	rm(.Random.seed, envir=globalenv())

if(!("devtools" %in% rownames(installed.packages())))
	install.packages("devtools")

library(devtools)

#Install the required packages
if(!("fOptions" %in% rownames(installed.packages())))
	install.packages("fOptions")

if(!("foreach" %in% rownames(installed.packages())))
	install.packages("foreach")

if(!("doMC" %in% rownames(installed.packages())))
	install.packages("doMC")

if(!("tidyr" %in% rownames(installed.packages())))
	install.packages("tidyr")

if(!("plyr" %in% rownames(installed.packages())))
	install.packages("plyr")

if(!("dplyr" %in% rownames(installed.packages())))
	install.packages("dplyr")

if(!("ggplot2" %in% rownames(installed.packages()))){
	# install.packages("ggplot2")
	install_github("hadley/ggplot2")
}

if(!("reshape2" %in% rownames(installed.packages())))
	install.packages("reshape2")

if(!("png" %in% rownames(installed.packages())))
	install.packages("png")

if(!("grid" %in% rownames(installed.packages())))
	install.packages("grid")

if(!("gridExtra" %in% rownames(installed.packages())))
	install.packages("gridExtra")

if(!("xlsx" %in% rownames(installed.packages())))
	install.packages("xlsx")

if(!("readxl" %in% rownames(installed.packages())))
	install.packages("readxl")

if(!("RecordLinkage" %in% rownames(installed.packages())))
	install.packages("RecordLinkage")

suppressMessages(library(fOptions))
suppressMessages(library(foreach))
suppressMessages(library(doMC))
suppressMessages(library(tidyr))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))
suppressMessages(library(png))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(xlsx))
suppressMessages(library(readxl))
registerDoMC(cores=4)

main_path <- "~/Dropbox/PhD_Workshop/"
