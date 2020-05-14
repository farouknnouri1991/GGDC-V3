#________________________________________________CODE FOR FUNCTIONS____________________________________________________________________
                            #1- Data Operating: changes, growths, etc ==>dta_op_f
                            #2- Growth decomposition (Eq3)            ==>decomp_f: for each sector the between and within componants/ also aggregate between and within 
                            #3- Clustering Function                   ==>kmeans_f
                            #4- Growth acceleration identifier        ==>

#_______________________________________________________________________________________________________________________________________________________________

rm(list=ls()); cat("All shares are given in %, hence ")
library(tidyverse); library(RColorBrewer)
#_______________________________________________________________________________________________________________________________________________________________


#dta<-readRDS("Outputs/dta.RDS")

#1: Data operating: calculating changes ----

dta_op_f<-function(data, group, first, last){
  
  #group<-sym(group)
  data %>% group_by_at(group) %>% filter(Year%in%c(first, last)) %>% arrange(Year) %>%
    summarize(first=first(Value), first_yr=first(Year),
              last=last(Value)  , last_yr=last(Year),
              change=last-first , growth=change/first
              ) %>% 
    filter(first_yr!=last_yr) %>% select(-first_yr,-last_yr) %>% 
    gather(operation, Value, c("last","first","change", "growth")) %>% filter(!is.na(Value))
  
  } 


group_vars<-c("DB","Area","Regioncode", "PPP", "Sector", "Variable") #Essential for dta_op_f

#2- Decomposition (Eq3) :as a function ----

decomp_f<-function(first, last, group, data)
  {
    
  group_vars<-group
    
    #0-calculate changes----
    dta_op<-dta_op_f(data=data, group=group_vars, first = first, last=last) %>% ungroup()#calculer les deltas
    
    decomp<-dta_op %>%
      filter(Variable %in% c("EMP_share", "ptyq", "pty"),operation %in% c("last", "change", "first")) %>%
      
      mutate(Variable=paste0(Variable,"_", operation)) %>% select(-operation) %>% spread(Variable, Value) %>%
      
    
    #I- calculer les composantes conformement à la formule/ SECTEUR----
      
    mutate(within_ptyq=EMP_share_first*ptyq_change,
           between_ptyq=ptyq_last*EMP_share_change,
           
           within_pty=EMP_share_first*pty_change,
           between_pty=pty_last*EMP_share_change
           ) 
    
    decomp<-decomp %>% gather(Variable, Value, length(group_vars):length(decomp)) %>%
      
      filter(Sector!="SUM" |(Sector=="SUM" & 
             ! Variable %in% c("within_ptyq","between_ptyq","within_pty","between_pty" ) ) )
  
  #Calculer la contribution totale de chaque composante/TOTAL
  decomp_tot<-decomp %>% filter(Sector!="SUM") %>% spread(Variable, Value) %>% 
    group_by(DB, Area,Regioncode, PPP) %>% 
    summarize(
              between_pty=sum(between_pty, na.rm=T),
              within_pty=sum(within_pty, na.rm=T),
              D_pty=sum(between_pty+within_pty, na.rm=T),
                
              between_ptyq=sum(between_ptyq, na.rm=T),
              within_ptyq=sum(within_ptyq, na.rm=T), 
              D_ptyq=sum(between_ptyq+within_ptyq, na.rm=T)#just for check..yes it is different from total pty change (as the decomposition is just an approximation)
                      ) %>% ungroup() #Controle3 Reussi:
          
    #Keep Relevant vars and tidy 
    indic_decomp<-c("EMP_share_change", "pty_change", "ptyq_change", "within_pty", "between_pty","within_ptyq","between_ptyq","D_pty", "D_ptyq")
    decomp<-decomp %>% filter(Variable%in% indic_decomp) %>% ungroup(); rm(indic_decomp)
    
    decomp_tot<-decomp_tot %>% gather(Variable, Value, (length(group_vars)-1):ncol(decomp_tot)) %>% 
      as.data.frame()%>% mutate(Sector="SUM")
  
    #bring together decomp and decomp tot: en considérant tot un secteur=summation.of.sectors GDP
    decomp<-rbind(decomp, decomp_tot) #%>% filter(!is.na(Value))
    decomp
    
    #note on method
    cat(" 
        1. for the Real Sectors: 
        
        within_ptyq=EMP_share_first*ptyq_change,
        between_ptyq=ptyq_last*EMP_share_change,
        
        
        2. for the SUM sector:
        between_ptyq=sum(between_ptyq, na.rm=T),
        within_ptyq=sum(within_ptyq, na.rm=T), 
        D_ptyq=sum(between_ptyq+within_ptyq, na.rm=T)
        
        3.D_pty is little different from total pty change (as the decomposition is just an approximation)

        4. Between and within have both reasons change whith PPP: both evolve VAs, however sectro diff in within shall be the same for PPP and LC
        "
          )
    
    
  decomp  
    
  }#END decomp_f
  
#s<-decomp_f(data = dta, first = 1960, last=2000, group = group_vars )

#2-2 a function tha decompose many times by a number of years entered as agrument "by"

decomp_n_f<-function (y0, yn, by){
 
  output.vars<- c("within_ptyq","between_ptyq","EMP_share_change","ptyq_change", "D_ptyq")
  maxyear=2012
  
  if (yn>2012) cat("please enter yn<2012")
  
  else{
  a<-seq(y0, yn, by=by)
  l<-as.list(rep(0, length(a))); names(l)<-as.character(a)
  
  
  for(i in 1: length(l))
    {
    p<-decomp_f(first=a[i], last=min(a[i]+by-1,maxyear), group_vars, data=dta) %>%
    filter( Variable %in% output.vars ) %>% 
    spread(Variable, Value) 
  
  p$decade<-a[i]
  
  l[[as.character(a[i])]]<-p
  print(i)
  }
  
  
  
  decomp_sect_dec<-l %>% reduce(rbind) %>% gather(variable, value, output.vars) %>% 
    ungroup() %>% filter(!is.na(value)) 
  }
}


#making colors (10)+bonus ;)
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(10)



#3- A CLUSTERING FUNCTION----


kmeans_f<-function(Data, VARIABLE,  n)
  
    {
      #data must have "Area", "Year", "Variable", a "Sector", and a "Value" columns 
      Data<-Data; variable<-VARIABLE
      if (!variable%in%unique(Data$Variable)) cat(paste(variable, "is not a Variable in the dataset"))
      else{
      
      #filter on the variable of clustering(befor goig to sectors)
      Data<-filter(Data, Variable==variable)
      
      nsectors<-length(unique(Data$Sector))
      cols<-(ncol(Data)-1): (ncol(Data)-2+nsectors)
      
      #Spread to have sectors as variables you gone to cluster on (sinon you have only one var)
      Data_w<-spread(Data, Sector, Value) %>% na.omit()
      
      #run the kmeans appropriately (na.omit up)
      km<-kmeans(Data_w[,cols], centers = n)
      
      #prepare the output:
          # 
          #a summary cluster , 
          #the kmeans output as list, and data transformed with the columns who have been into kmeans function
      
      Data_w<-data.frame( cluster=km$cluster, Data_w)
      
      #Clusters<-data.frame(Data_w[c("Area", "Year")], cluster=km$cluster)
      
      l<-list(DATA=list(Data_w=Data_w, cols=cols),
           Kmeans=km
           #Clusters=Clusters
           )
      }#end else
      cat("1. data is spread along the the sectors by clusterig variable 
              the incomplete cases are omitted, and clusters are given
          2.  output ass a list containing: 
              i. the data cleaned as in 1. with clusters and the specific columns inpued into kmeans function; 
              ii. the kmeans output Kmeans
          ")
      l
      }
    
    #L<-kmeans_f(Data=dta, VARIABLE="EMp_share" , n=4 )
    #L<-kmeans_f(Data=dta, VARIABLE="EMP_share" , n=4 )
rm(L)

  
  

  
  
#4- Identifier les accelerations:-----

#dta<-readRDS("Outputs/dta.RDS")   

#Data=filter(dta, Sector=="SUM", Variable=="ptyq", PPP=="LC", DB=="GGDC");
#Year="Year"; Country="Area"; Variable="Value"; period=3; v=0.02; a = 0.05
 
Acceleration_function=function(Data, Country, Year, Variable, period, v, a){#takes only one var by country //must choose the additional vars to add all conditons or some of them
  
  #getting the max of the previos years
  dta2<-Data; var<-Variable
  Large<-dta2[,c(Country, Year, Variable )] %>% spread(key=Year, value=Variable)
  
  Max<-as.data.frame(matrix(nrow=nrow(Large),ncol=ncol(Large)))
  
  names(Max)<-names(Large)
  
  Max[Country]<-Large[Country]
  
  for (i in 2:ncol(Large)){
    Max[,i]<-apply(Large[,2:i, drop=FALSE], MARGIN=1, FUN=max, na.rm=TRUE)
  }
  
  Max<-Max %>% gather(key="Year", value="Maximum_previous",2: ncol(Max))
  
  dta2<-merge(dta2,Max)
  
  #All the conditions: 
  dta.panel<-pdata.frame(dta2, index=c(Country, Year), row.names = TRUE)
  
  c<-dta.panel%>%mutate(VAW_grw=(lead(dta.panel[[Variable]],k=3)-get(Variable))/get(Variable))#VA growth rate 3years foreward
  
  c<-pdata.frame(c, index=c(Country, Year), row.names = TRUE)#when mutate we lose teh panel structure so I need to remind R of the structure I want
  c<-c%>%mutate(D_VAW_grw= c$VAW_grw -lag(c$VAW_grw,k=period), #difference in growth rates (+-3years)
                acceleration=D_VAW_grw>a,#growth has accelerated (we can imagine other conditions like mean(grwth rates after)>mean(grwth rates before)
                rapid=VAW_grw> v, #growth is rapid this year (calculated along the next three years)
                maxpost=lead(c[[Variable]], k=period)>Maximum_previous, #the level of VA post accelaration period exceeds the levels of pre-acceleration period
                conditions=acceleration&rapid&maxpost,
                Year=as.numeric(as.character(Year))) 
  c
  }
  
  #t<-Acceleration_function(Data=filter(dta, Sector=="SUM", Variable=="VA", Sector=="AGR", PPP=="LC", DB=="GGDC"), 
   #                        Year="Year", Country="Area", 
    #                        Variable="Value", period=3, v=0.02, a = 0.05)

    
    
    
#-----------------------------------------------------------FRAGMENTS OF CODE-------------------------------------------------------------


#Controle3 Reussi:
  #controle<-decomp_f(group=group_vars, first=2000, last=2001) 
  #controle2<-controle%>% filter(Variable %in% c("ptyq_change", "D_ptyq"), Sector=="SUM") %>% spread(Variable, Value) %>% mutate(d=(ptyq_change-D_ptyq)/D_ptyq)
  #plot(controle2$ptyq_change, controle2$D_ptyq)


