#__________________________________________________________Recensement Economique _______________________________________________________11/mars/2020
rm(list=ls())


#read the file into character voctor
library(pdftools)
ecocensus<-pdf_text("econcensus.pdf")

#table 12.2: repartition emploi/sex-branche et CSP
table12.2<-ecocensus[90:94]

lines <- table12.2 %>%  lapply(str_split, "\n", simplify = TRUE)
lines<-lines %>% reduce(c)

lines2<-lines[c(8:36,44: 82, 92:131, 140:180)] %>% gsub(pattern="\r",replacement = "")
A<-lines2 %>% gsub(pattern="\\s{2, }", replacement = "_")
A<-A %>% str_split(pattern="_")

comp<-logical(); twolines<-logical()

for (i in 3: length(A)-1){
  twolines[i]<-length(A[[i]])==12 & A[[i]][1]=="" & length(A[[i-1]])==1 & length(A[[i+1]])==1  
}

sum(twolines,na.rm=T)
l1<-A[which(twolines==T)-1] %>% reduce(rbind)
l2<-A[which(twolines==T)] %>% reduce(rbind)
l3<-A[which(twolines==T)+1] %>% reduce(rbind)

l2[,1]<-paste(l1, l3)

a1<-l2 %>% as.tibble()
a1$indic<-which(twolines==T)

for (i in 1: length(A)){
  comp[i]<-length(A[[i]])==12 & A[[i]][1]!=""
}  

sum(comp, na.rm = T)  
a2<-A[comp] %>% reduce(rbind) %>% as.tibble() 
a2$indic<-which(comp==T)

rest<-c(which(twolines==T)-1, which(twolines==T), which(twolines==T)+1, which(comp==T) )
rest<-A[-rest]# je ne me casse pasla tete 
#un seul sect qui rest 
rest<-lines[20]


census<-rbind(a1,a2) %>% arrange(indic) 
colnames(census)<-c("branch", "patrons", "salar","salar_f","apprenti", 
                      "apprenti_f", "aid", "aid_f", "oth", "oth_f", "tot", "tot_f", "indic") 
str(census)

census[, 2:12]<- census[, 2:12]%>% apply( MARGIN = 2,  gsub, pattern = " ", replacement = "") %>% apply(MARGIN = 2, as.numeric)
str(census)

census<-census%>% as.data.frame() %>% 
  mutate(apprenti_share=apprenti/tot, apprenti_f_share=apprenti_f/apprenti)

#Table 14
#Ets-effectifs/secteur et date de création
table14<-ecocensus[98]


lines <- table14 %>%  lapply(str_split, "\n", simplify = TRUE)
lines<-lines %>% reduce(rbind)

rural<-lines[c(9:21)] %>% gsub(pattern="\\s{2, }", replacement = "_") %>%  str_split(pattern="_")
rural[2:13]<-rural[2:13] %>% lapply(function(x) x[2:12])
rural<-rural %>% reduce(rbind) %>% as.tibble()%>% mutate(milieu="rural")



urban<-lines[c(28:40)]%>% gsub(pattern="\\s{2, }", replacement = "_") %>%  str_split(pattern="_")
urban[2:13]<-urban[2:13] %>% lapply(function(x) x[2:12])
urban<-urban %>% reduce(rbind) %>% as.tibble() %>% mutate(milieu="urban")

table14<-rbind(rural, urban)

names(table14)<-c("year", "industry_est", "industry_emp", "btp_est", "btp_emp", 
                  "commerce_est", "commerce_emp", "service_est", "service_emp", "total_est", "total_emp", "milieu")
