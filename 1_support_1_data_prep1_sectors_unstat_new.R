#____________________________________________________NEW DATA from UNSTAT: nama project____________________________________________________
library(tidyverse)

#nama<-read.csv2("https://unstats.un.org/unsd/amaapi/api/file/6", header = F)

nama<-read.csv2("Inputs/11-UNSTAT/GDPconstant-USD-countries-2.csv", stringsAsFactors = F) %>% select(-1)

nama<-nama %>% gather(Year, Value, X1970:X2018) %>% 
  mutate(Year=as.numeric(sub(Year, pattern="X",replacement = "")),
         
         Value=as.numeric(gsub(Value, pattern=" ",replacement = ""))
         ) 

codes<-read.csv2("Inputs/11-UNSTAT/codes_nama.csv", stringsAsFactors = F)

nama<-merge(nama, codes) %>% select(-IndicatorName) %>% rename(Area=Country)

