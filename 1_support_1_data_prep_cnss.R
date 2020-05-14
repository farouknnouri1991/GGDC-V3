#________________________________________________DATA PREARATION____________________________________________________________________
                        #1- reading csv data, save it to RDS
                        
#_______________________________________________________________________________________________________________________________________________________________
rm(list=ls()); 
library(tidyverse); 
library(foreign)
#_______________________________________________________________________________________________________________________________________________________________
sessionInfo()

cnss1115<-read.csv2("Inputs/3-Tarek/CNSS/DS 2011-2015 annuel.csv", stringsAsFactors = F, encoding = "latin1")

saveRDS(cnss1115, "Inputs/3-Tarek/CNSS/cnss11_15.RDS")

cnss0610<-read.csv2("Inputs/3-Tarek/CNSS/DS 2006-2010 annuel.csv", stringsAsFactors = F , encoding = "latin1")

saveRDS(cnss0610, "Inputs/3-Tarek/CNSS/cnss06_10.RDS")

cnss16<-read.csv2("Inputs/3-Tarek/CNSS/2016 DS.csv",stringsAsFactors = F, encoding = "UTF-8")

#saveRDS(cnss16, "Inputs/3-Tarek/CNSS/cnss16.RDS")

cnss17<-read.csv2("Inputs/3-Tarek/CNSS/2017 DS.csv", stringsAsFactors = F, encoding = "UTF-8")

cnss1617<-rbind(cnss16, cnss17)

cnss1617$libelle_groupe_br_activite<-iconv(cnss1617$libelle_groupe_br_activite, from="UTF-8", to="latin1")
cnss1617$libelle_branche_activite  <-iconv(cnss1617$libelle_branche_activite,   from="UTF-8", to="latin1")
cnss1617$libelle_activite          <-iconv(cnss1617$libelle_activite,           from="UTF-8", to="latin1")

cnss1617$ville<-iconv(cnss1617$ville,           from="UTF-8", to="latin1")
cnss1617$forme_juridique<-iconv(cnss1617$forme_juridique,           from="UTF-8", to="latin1")
saveRDS(cnss1617, "Inputs/3-Tarek/CNSS/cnss1617.RDS")


#____________combine data----
cnss1115<-readRDS(("Inputs/3-Tarek/CNSS/cnss11_15.RDS"))
cnss0610<-readRDS(("Inputs/3-Tarek/CNSS/cnss06_10.RDS"))
cnss1617<-readRDS(("Inputs/3-Tarek/CNSS/cnss1617.RDS"))
names(cnss1115)<-tolower((names(cnss1115)))
names(cnss0610)<-tolower((names(cnss0610)))

cnss<-rbind(cnss0610, cnss1115, cnss1617)
cnss<-rbind(cnss0610, cnss1115, cnss1617)

#rename:
a<-names(cnss)
cnss<-cnss %>% rename(year=annee_declaration,
                      id_firm=code_affilie, firm_aff=date_affiliation, firm_crea=date_creation,juri=forme_juridique,
                      act_gr=libelle_groupe_br_activite, act_gr_=code_group_br_activite, 
                      act_br=libelle_branche_activite, act_br_=code_branche_activite, 
                      act=libelle_activite,act_=code_activte,
                      id_wrk=code_immatricule, wrk_im=date_immatriculation,wrk_dec= periode_premiere_declaration, birth=date_naissance,
                      wage_an=salaire_annuel, months=nb_mois_declare,days= nbr_jours_annuel, nation=nationalite
                      ) %>% select(-c("code_direction", "direction","code_agence","agence"))

cnss<-cnss %>% mutate(wage_day=wage_an/days) %>% filter(!is.nan(wage_day))

b<-names(cnss)

attributes(cnss)$var.labels<- data.frame(short=b, long=a)
cnss %>% saveRDS("Inputs/3-Tarek/CNSS/cnss.RDS")

