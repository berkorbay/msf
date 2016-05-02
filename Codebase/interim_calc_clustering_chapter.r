

#Interim calculations for clustering_chapter.r

test_set<-dir("~/Dropbox/PhD_Workshop/Output\ Files/Clustering\ Chapter/",pattern="HN_data_withdiv_asym_5y_RPE.RData",full.names=TRUE)
test_set<-dir("~/Dropbox/PhD_Workshop/Output\ Files/Clustering\ Chapter/",pattern="BS_data_withdiv_2y_RPE.RData",full.names=TRUE)

load(test_set[1])

error_data %>% mutate(cit_misses=ifelse(cit_MAPE > quantile(cit_MAPE,0.9),"Miss","Hit")) %>% filter(cit_misses=="Miss" & cit_MAPE > manual2_MAPE) %>% arrange(desc(cit_MAPE))

cit_graph<- data.frame(index=1:nrow(error_data),error=sort(error_data$cit_MAPE)) %>% tbl_df
m2_graph<- data.frame(index=1:nrow(error_data),error=sort(error_data$manual2_MAPE)) %>% tbl_df

ggplot() + geom_line(data=cit_graph,aes(x=index,y=error), color="#fe7900") + geom_line(data=m2_graph,aes(x=index,y=error), color="#ff0000")

ggplot(error_data %>% mutate(rn=row_number()),aes(x=rn)) + geom_point(aes(y=cit_MAPE), color="#fe7900",shape=2) + geom_point(aes(y=manual2_MAPE), color="#ff0000")


ggplot(error_data %>% mutate(rn=row_number()),aes(x=rn)) + geom_point(aes(y=RPE,shape=Type))



ggplot(error_data %>% mutate(rn=row_number()),aes(x=rn,color=Type)) + geom_point(aes(y=cit_MAPE),shape=2) + geom_point(aes(y=manual2_MAPE))

load(test_set[6])


san<- error_data %>% arrange(desc(cit_MAPE)) %>% select(DataDate:TrainPred,cit_estimate,cit_MAPE,manual2_estimate,manual2_MAPE) %>% mutate(diffs=ifelse(cit_MAPE<manual2_MAPE,"Better","Worse"))

if(any(is.na(error_data))){print("NA alert!")}

san %>% group_by(Type,TrainPred) %>% summarise(cit_median=median(cit_MAPE),cit_mean=mean(cit_MAPE),m2_median=median(manual2_MAPE,na.rm=TRUE),m2_mean=mean(manual2_MAPE,na.rm=TRUE))

error_data %>% filter(is.na(manual2_estimate))