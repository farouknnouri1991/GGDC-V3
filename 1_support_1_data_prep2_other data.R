#________________________________________________DATA PREARATION2____________________________________________________________________
                            #1- gdp
                            #2- infrastructure 
                            #3- 

#_______________________________________________________________________________________________________________________________________________________________
library(tidyverse)
rm(list = ls())
#II-Other national data----

#1-GDP_ppp----
    
    GDP_ppp<-read.csv2("Inputs/GDP_PPP.csv", stringsAsFactors = F) %>% select(-c(1, 3:4), -X) %>% mutate(Variable="gdp_ppp") %>% gather(Year, Value, X1960:X2019)
      
    GDP_pc_ppp<-read.csv2("Inputs/GDP_PC_PPP.csv", stringsAsFactors = F)%>% select(-c(1, 3:4), -X) %>%  mutate(Variable="gdp_pc_ppp") %>%  gather(Year, Value, X1960:X2019)
  
    GDP<-rbind(GDP_ppp,GDP_pc_ppp)  ; rm(GDP_pc_ppp, GDP_ppp)
    
    GDP$Year<-as.numeric(sub(x=GDP$Year, pattern="X", replacement= "")); names(GDP)[1]<-"Area"
    GDP$Value[GDP$Value==""]<-NA;GDP$Value<-as.numeric(GDP$Value)
    
    GDP<-filter(GDP, !is.na(Value))
  saveRDS(GDP, "Outputs/dta/GDP.RDS"); 

    
    
#2-Infrastructure stock (canning 1965-2005)----
rm(list=ls())
l<-list.files("Inputs/5-Canning-Infra_stock/csv") #%>% as.list() 

infra<-paste0("Inputs/5-Canning-Infra_stock/csv/", l) %>% lapply(read.csv2, stringsAsFactors = F) 

  #infra<-infra %>% lapply(FUN=function(x) gather(x, Year, Value,  5:ncol(x)))
codes<-infra[[2]]
correct<-infra[[1]]
infra<-infra[-c(1, 2)] %>% lapply(FUN=function(x) gather(x, Variable, Value,  3:ncol(x)))

infra %>% lapply(names)

infra<-infra%>% reduce(rbind) %>% rename(Area=country) %>% 
filter(!is.na(Value), Value!="", Value!=" ")

infra$year<-as.numeric(infra$year)

infra$Value<-as.numeric(infra$Value)

codes$Area<-toupper(codes$Area)
infra$Area<-toupper(infra$Area)

#detect bad names
#areas<-merge(infra, codes, all.x=T) %>% select(Area, Area_code) %>% unique()
#areas %>% write.csv2("Inputs/5-Canning-Infra_stock/csv/codes2.csv")

#correct names

infra<-merge(infra, correct, all.x=T)

infra$Area[infra$Area%in%correct$Area & !is.na(infra$Area_correct)]<-infra$Area_correct[infra$Area%in%correct$Area & !is.na(infra$Area_correct)]

#merge with codes
infra<-merge(codes, infra, all.y=T) %>% select(-Area_correct)
infra %>% select(Area, Area_code) %>% filter(is.na(Area_code)) %>% unique()

#keep only long series "egcmerge", "pavedmerge", "railmerge", "telemerge"

vars<-c( "egcmerge", "pavedmerge", "railmerge", "telemerge")
infra<-filter(infra, merge )
vars<-data.frame(short=c("elec_gen_cap", "pav_road", "rail", "telephones"),
                 Variable=sort(vars))

infra<-merge(vars, infra) %>% select(-Variable, -Area) %>% rename(Variable="short", Area=Area_code) 

#save
saveRDS(infra, "Outputs/dta/dta.infrastructure_canning.RDS")
write.csv2(infra, "Outputs/dta/dta.infrastructure_canning.csv")



