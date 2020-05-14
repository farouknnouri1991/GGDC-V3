rm(list=ls()); 
library(sjlabelled); library(haven); library(stargazer)
PPP_Conv<-read.csv2("Inputs/Conversion/PPP_Conv.csv",stringsAsFactors = F) %>% gather(key="Year", value="PPP_WB", 5: 32) %>%
      mutate(Year= str_replace(Year,"X", "") %>% as.integer()) %>% select(-c(1, 3:4)) %>% filter(!is.na(PPP_WB)) %>% rename(Area=1)
 
penn<-read_dta("Inputs/PennTables/pwt91.dta")
variables<-tibble(labs=get_label(penn),names=names(penn))

ppp<-penn %>% select( countrycode, year,cgdpe,rgdpna, pl_gdpo, rgdpe, pl_con) %>% 
  mutate(rgdpe/rgdpna, 1/pl_gdpo, 1/pl_con) #%>% filter()
ppp<-merge(ppp, PPP_Conv, by.x=c("countrycode", "year"), by.y=c("Area", "Year"), all.x=T)

ppp%>% write.csv2("PPP_trials.csv")

variables  %>% write.csv2("penn.csv")



plot((rgdp$ppp), rgdp$PPP)

class(penn)
summary(penn)
