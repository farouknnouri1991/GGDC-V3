#________________________________________________DATA PREARATION____________________________________________________________________
                            #1- Clearing Data (GGDC+EASD) and transform to PPP
                            #2- Calculate Real-Nominal sectoral Productivities 
                            #3- Calculating shares in (emp, va, pty, ptyq)
                                #- Shares are ot given in % in orther to not atlerate coming calculus 
                                #- 
#_______________________________________________________________________________________________________________________________________________________________

rm(list=ls()); 
library(tidyverse); library(foreign)
#_______________________________________________________________________________________________________________________________________________________________

#I-Sectroral Data (GGDC, EASD)----
#1- Cleaning Data-Transform to PPP----

  #GGDC
    GGDC<-read.dta("Inputs/1-GGDC-EASD/10sd_jan15.dta" ) %>% rename(Area=1) %>% select(-Region); 
    
    GGDC[GGDC$Area=="MOR",1]<-"MAR"; GGDC[GGDC$Area=="DEW",1]<-"DEU"
    
    indic<-c("VA_Q05","VA_Q10","VA_Q91"); 
    cols=c("Area","Regioncode", "Year", "Sector","PPP", "base_year", "Variable", "Value" )#pour ordonner les cols
    
    VAQ_base<-GGDC %>% filter( Variable %in% indic) %>% select(Area, Year, Variable) %>% unique() %>% mutate(base_year=Variable)
    
    GGDC_LC<-merge(GGDC, VAQ_base, all=T) %>% mutate(PPP="LC", DB="GGDC") %>% gather(key=Sector, value= Value, 5:15) %>% #select(cols) %>% 
      filter(!is.na(Value)) 
    
    GGDC_LC[GGDC_LC$Variable %in% indic, "Variable"]<-"VAQ"
    #z<-unique(GGDC_LC[-9]) #its unique
    sectors<-GGDC_LC %>% select(Sector) %>% unique() %>% filter(Sector!="SUM") #%>% as.vector()##important!
    rm(indic,  GGDC, VAQ_base)
  
  
  #EASD
    
    EASD_VA<-read.csv2("Inputs/1-GGDC-EASD/EASD-2018-VA.csv", stringsAsFactors = F) %>% select(-Unit, -Country) %>% rename(Year=1, Area=2) 
    EASD_EMP<-read.csv2("Inputs/1-GGDC-EASD/EASD-2018-EMP.csv", stringsAsFactors = F) %>% select( -Country) %>% rename(Year=1, Area=2) %>% 
      mutate( Variable="EMP") %>% select(names(EASD_VA))
    
    EASD<-rbind(EASD_VA, EASD_EMP) %>% mutate(Regioncode="SSA",  PPP="LC", DB="EASD") %>% gather(Sector, Value, 4:15) %>% mutate(Value=as.numeric(Value)) ; 
    EASD[EASD$Variable=="VA_Q05","Variable"]<-"VAQ"; EASD[EASD$Variable=="VAQ","base_year"]<-"VA_Q05"
    #z<-unique(EASD[-9]) #its unique
    rm(EASD_EMP, EASD_VA)
  
 
    
  #linking the two
    sect<-c("AGR", "FIRE", "CON", "DWE", "GOV", "MAN", "MIN", "OTH", "SUM", "WRT", "TRA","PU")
    dictionnary<-data.frame(Sector=EASD$Sector %>% unique %>% sort, 
                            ggdc=sect)
    
    #transform: FIRE=FIRE+DWE
    EASD<-merge(EASD, dictionnary) %>% mutate(Sector=ggdc) %>% select(-ggdc) %>% spread(Sector, Value) %>% 
      mutate(FIRE=FIRE+DWE) %>% 
      select(-DWE) %>% gather(Sector, Value, sect[sect!="DWE"])
    
    #rbind
    dta<-rbind(GGDC_LC, EASD %>% filter(Variable!="VA_P"))# %>% unique()
    #z<-dta[-9] %>% unique()#its unique
    
    #extract sectoral PPP coversion rates
    #PPP_conv_easd<-EASD %>% filter(Variable=="VA_P")#FAALSE THIS IS PRICE DEFLATOR 2005=1
    
    attributes(dta)$sectors<-dictionnary
    rm(EASD, GGDC_LC)
  
  
#PPP conversion---- 

    PPP_Conv<-read.csv2("Inputs/99-Conversion/PPP_Conv.csv",stringsAsFactors = F) %>% gather(key="Year", value="PPP", 5: 32) %>%
      mutate(Year= str_replace(Year,"X", "") %>% as.integer()) %>% select(-c(1, 3:4)) %>% filter(!is.na(PPP)) %>% rename(Area=1) 
    
    PPP_Conv$PPP[PPP_Conv$Area=="BRA" & PPP_Conv$Year<=1994]<-NA
    
    #z<-PPP_Conv[-3] %>% unique()#also unique

    
    VA_PPP<-dta %>%filter(Variable!="EMP") %>% select(-PPP) %>% 
      merge( PPP_Conv, by=c('Area','Year')) %>% 
      mutate(Value=Value/PPP, PPP="PPP") %>% filter(!is.na(Value))
    
    #z<-VA_PPP[-8] %>% unique()#also unique
    
    #c est là qu'il faut repliquer emploi meme pour PPP=PPP
    EMP_PPP<-dta %>% filter(Variable=="EMP") %>% mutate(PPP="PPP")
    
    dta_PPP<-rbind(VA_PPP, EMP_PPP) %>% filter(!is.na(Value)) #%>% unique
    #z<-dta[-c(9, 5)] %>% unique()#also unique
                            
    rm(PPP_Conv,VA_PPP, cols, EMP_PPP)

#To us$ conversion----
    #USD conversion 

    USD_Conv<-read.csv2("Inputs/99-Conversion/exchange_rates.csv",stringsAsFactors = F) %>% select(-X) %>% gather(key="Year", value="USD", 5: 64) %>%
      mutate(Year= str_replace(Year,"X", "") %>% as.integer()) %>% select(-c(1, 3:4)) %>% filter(!is.na(USD)) %>% rename(Area=1) 
    
    #prob in some euro countries
    euro<-subset(USD_Conv, Area=="EMU") %>% select(-Area) %>% filter(USD!="")#european moneraty union
    eurocountries<-merge(USD_Conv,unique(dta[c(1,3,4)]) )%>% filter(USD=="", Regioncode=="EUR" )  %>% 
      select(-USD)
    USD_euro<-merge(euro,eurocountries) %>% select(-Regioncode)
    USD_Conv<-filter(USD_Conv, USD!="") %>% rbind(USD_euro)
    
    USD_Conv$USD<-as.numeric(USD_Conv$USD)
    rm(USD_euro, eurocountries, euro)
    
    
    #z<-USD_Conv[-3] %>% unique()#also unique

    
    VA_USD<-dta %>%filter(Variable!="EMP", PPP=="LC") %>% select(-PPP) %>% 
      merge( USD_Conv, by=c('Area','Year')) %>% 
      mutate(Value=Value/USD, PPP="USD") %>% filter(!is.na(Value)) %>% select(-USD)
    
    #z<-VA_USD[-8] %>% unique()#also unique
    
    #c est là qu'il faut repliquer emploi meme pour USD=USD
    EMP_USD<-dta %>% filter(Variable=="EMP", PPP=="LC") %>% mutate(PPP="USD")
    dta_USD<-rbind(VA_USD, EMP_USD) %>% filter(!is.na(Value)) #%>% unique
    #z<-dta[-c(9, 5)] %>% unique()#also unique
                            
    rm(USD_Conv,VA_USD,EMP_USD)
    
    
    
    
#2- Calculating sectoral real-nominal productivities----
    
    dta<-rbind(dta, dta_PPP, dta_USD)
    
    dta<-dta %>% select(-base_year) %>% spread(Variable, Value)%>%
      mutate(pty=VA/EMP, ptyq=VAQ/EMP) 
    
    dta<-dta%>% gather(key=Variable, value=Value, EMP, VA,VAQ,pty,ptyq) %>% 
      filter(!is.na(Value), is.finite(Value))
    
    #z<-dta[-8] %>% unique()#also unique
    
#3- Calculating shares in (emp, va, pty, ptyq)-----
    
  dta_shares<-dta %>% spread(Sector, Value) %>% select(1:6, c(sectors$Sector, "SUM")) %>% #il faut remettre total à la fin pour ne pas le modifier avant de tout diviser par lui
    mutate_at(7:17, ~./SUM) %>%
    gather(Sector, Value, 7:17) %>% mutate(Variable=paste0(Variable,"_share")) %>% 
    filter(!is.na(Value), !is.nan(Value)) #%>% mutate(Value=round(100*Value, 0)) #control reussi: que la somme des parts =1
  
  #z<-dta_shares[-8] %>% unique()#also unique                               
  
  dta<-rbind(dta,dta_shares)#use shares for decomposition of growth equations 1 & 2 in Rodrik framework
  
  #z<-dta[-8] %>% unique()
  
  rm(dta_shares, sectors, sect)

#4- Saving final data----
  dta<-filter(dta,!(Year<1960&Area=="NLD"))
  sectors<-read.csv2("Inputs/1-GGDc-EASD/sectors_def.csv");attributes(dta)$sectors<-merge(dictionnary, sectors)
  
  saveRDS(dta, "Outputs/dta/dta.RDS") 

#__________________________________________________________________________________________________________________________________________________________________________________________________________________________________

