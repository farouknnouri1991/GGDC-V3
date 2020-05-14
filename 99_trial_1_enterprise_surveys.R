#___________________________________________________5- The case of Morocco:enterprise surveys_______________________________________________________11/mars/2020
rm(list=ls())
#_______________________________________________________________________________________________________________________________________________________________

library(haven); library(rlist); library(sjlabelled)

path<-"Inputs/9-WB firms survey 2003_2019"
files<-c("Morocco-2007--full-data-.dta", "Morocco-2013-full-data.dta" ,"Morocco-2019-full-data_.dta", "Morocco_2004_2007_panel.dta")
l<-as.list(paste0(path,"/", files)) %>% lapply(read_dta)
names(l)<-c("y2007","y2013","y2019", "y2004_2007")
var.labels<-l %>% lapply(get_label)
val.labets<-l %>% lapply(get_labels)
#We have labels for 2013, 2019 , not for 2004-2007
#workflow: mamke labels for 2004,2007 data

y2019<-l[["y2019"]]
a<-get_label(y2019) %>% as.data.frame()
table(y2019$a4b)
get_labels(y2019$a4b)


l %>% lapply(nrow)

y2007<-l$y2007
table(y2007$h18a)

table(y2007$j7a)#% de la fraude
summary(y2007$j7a)

table(y2007$j1a)#% tribunaux impartiaux


hist(y2007$n2a)
table (y2007$e31)

table (y2007$j12)#refus= oui








