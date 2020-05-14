#___________________________________________________5- The case of Morocco:CNSS_______________________________________________________11/mars/2020
                            
#_______________________________________________________________________________________________________________________________________________________________

rm(list=ls())
library(tidyverse); library(sjlabelled); library(plotly)

#1- data import and prepa----

cnss <-readRDS(("Inputs/3-Tarek/CNSS/cnss.RDS")) %>% filter(!is.nan(wage_day))
cnss$year<-as.numeric(cnss$year)
attributes(cnss)$.rows<-NULL

#1- summarizing data 31 millions!!

#by activities
group_activities<-cnss %>% group_by(act_gr, year ) %>% summarize(
  wage_an_mean=mean(wage_an), wage_day_mean=mean(wage_day),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F"),
  q1=quantile(wage_day, 0.2),q2=quantile(wage_day, 0.4),q3=quantile(wage_day, 0.6),q4=quantile(wage_day, 0.8)
   )

branch_activities<-cnss %>% group_by(act_gr, act_br, year ) %>% summarize(
  wage_an_mean=mean(wage_an), wage_day_mean=mean(wage_day),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F") ,
  q1=quantile(wage_day, 0.2),q2=quantile(wage_day, 0.4),q3=quantile(wage_day, 0.6),q4=quantile(wage_day, 0.8)
  )%>% 
  gather(q, value, q1:q4)

activities<-cnss %>% group_by(act_gr, act_br,act, year ) %>% summarize(
  wage_an_mean=mean(wage_an), wage_day_mean=mean(wage_day),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F"),
  q1=quantile(wage_day, 0.2),q2=quantile(wage_day, 0.4),q3=quantile(wage_day, 0.6),q4=quantile(wage_day, 0.8)
  )

group_activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_groups.RDS")
branch_activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_branches.RDS")
activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_activities.RDS")


#firm level data
firms<-cnss %>% group_by(act_gr, act_br,act,id_firm, year, juri, firm_crea, firm_aff ) %>% summarize(
  wage_an_mean=mean(wage_an), wage_day_mean=mean(wage_day),nb_wrk=n(),female=sum(sexe=="F"),
  q1=quantile(wage_day, 0.2),q2=quantile(wage_day, 0.4),q3=quantile(wage_day, 0.6),q4=quantile(wage_day, 0.8)
  )

firms<-firms  %>%arrange(-nb_wrk) %>% group_by(year, act_gr, act_br, act) %>% 
  mutate(big=row_number(id_firm)) 

firms<-firms %>% group_by(id_firm) %>% arrange(year)%>%mutate(ylast=last(year), yfirst=first(year))


firms %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_firms.RDS")

#firms_city data
firms_city<-cnss %>% group_by(act_gr, act_br,act,id_firm, year, juri, firm_crea, firm_aff, ville ) %>% summarize(
  wage_an_mean=mean(wage_an), wage_day_mean=mean(wage_day),nb_wrk=n(),female=sum(sexe=="F"),
  q1=quantile(wage_day, 0.2),q2=quantile(wage_day, 0.4),q3=quantile(wage_day, 0.6),q4=quantile(wage_day, 0.8)
  )

firms_city<-firms_city %>% group_by(id_firm) %>% arrange(year)%>%mutate(ylast=last(year), yfirst=first(year))

firms_city %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_firms_city.RDS")


#by cities
villes<-cnss %>% group_by(ville, year ) %>% summarize(wage_an=mean(wage_an), wage_day_mean=mean(wage_day), nb_wrk=n() , nb_firms=n_distinct(id_firm),female=sum(sexe=="F")) %>% 
  gather(variable, value, 3:5) %>% filter(!is.na(value))
villes<-ungroup(villes)

villes_ch<-villes %>% group_by(ville, variable) %>% arrange(year) %>% 
  summarize(gr=(dplyr::last(value)-dplyr::first(value))/dplyr::first(value)
              
            ) %>% spread(variable, gr)


city_group_activities<-cnss %>% group_by(ville, act_gr, year ) %>% summarize(
  wage_an_mean=mean(wage_an), wage_day_mean=mean(wage_day),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F"),
  q1=quantile(wage_day, 0.2),q2=quantile(wage_day, 0.4),q3=quantile(wage_day, 0.6),q4=quantile(wage_day, 0.8)
   )

city_branch_activities<-cnss %>% group_by(ville, act_gr, act_br, year ) %>% summarize(
  wage_an_mean=mean(wage_an), wage_day_mean=mean(wage_day),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F") ,
  q1=quantile(wage_day, 0.2),q2=quantile(wage_day, 0.4),q3=quantile(wage_day, 0.6),q4=quantile(wage_day, 0.8)
  )

city_activities<-cnss %>% group_by(ville, act_gr, act_br,act, year ) %>% summarize(
  wage_an_mean=mean(wage_an), wage_day_mean=mean(wage_day),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F"),
  q1=quantile(wage_day, 0.2),q2=quantile(wage_day, 0.4),q3=quantile(wage_day, 0.6),q4=quantile(wage_day, 0.8)
  )

city_group_activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_city_groups.RDS")
city_branch_activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_city_branches.RDS")
city_activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_city_activities.RDS")

#concentration: 

concentration_act<-firms %>% group_by(year, act_gr, act_br, act) %>% 
  summarize(big_five=sum(nb_wrk[big<=5], na.rm=T)/sum(nb_wrk, na.rm = T)*100)

concentration_act %>% saveRDS("Inputs/3-Tarek/CNSS/concenration_bigfive.RDS")

#










