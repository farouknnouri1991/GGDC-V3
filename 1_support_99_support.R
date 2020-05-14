lagged_fun<-function(i){
GGDC_lg %>% group_by(Area,Region.code, Region, PPP, Sector, Variable) %>% arrange(Year) %>%
  mutate(lag_1=dplyr::lag(Value,1), 
         diff_1=Value-lag_1,
         g_1=diff_1/lag_1
        ) %>% gather(time_op, Value, c("Value","lag_1","diff_1", "g_1")) %>% filter(!is.na(Value))
}
lagged_fun(1)






#II-Summary Statistics####
    
    #1- Table 1####
        
        Table1<-function(DB, Start, Final){
             
                  G<-gather(DB,key="Sector",value="Sectoral_PDVTY", 4:14)          
                  
                  
                  Stats_Final_Year<-G %>% 
                        filter(Year==Final & !is.na(Sectoral_PDVTY) ) %>%  ###WANARI ASH DART FIA HAD les na wlad 97AB
                        group_by(Country.code) %>% 
                          summarise(
                                    Overall_Productivty=last(Sectoral_PDVTY),##last in order to get productivity of summation sector which I could not get using Sectoral_PDVTY[Sector= or == "Summation..."]
                                                                                                        
                                    Coefficient_of_Variation= 100*sd(log(Sectoral_PDVTY[1:10]),na.rm=TRUE)/mean(log(Sectoral_PDVTY[1:10]),na.rm=TRUE),
                                    
                                    Sector_of_lowest_Productivity=Sector[which.min(Sectoral_PDVTY)],
                                    Lowest_Productivity=min(Sectoral_PDVTY), 
                                    
                                    Sector_of_highest_Productivity=Sector[which.max(Sectoral_PDVTY)],
                                    Highest_Productivity=max(Sectoral_PDVTY)                                                                    
                                    )
                  
                  #can do better for getting overall productivity
                  Overall_Productivty<- G[G$Year==Final & G$Sector=="Summation.of.sector.GDP",] #then merge it 
                  
                  Coumpound_Growth_Rate<- G %>% group_by(Country.code) %>% 
                                                summarize(Coumpound_Growth_Rate=(mean(Sectoral_PDVTY[G$Year==Final], na.rm=TRUE)/mean(Sectoral_PDVTY[G$Year==Start], na.rm=TRUE))^(1/(Final-Start)))

                  Tab1<-merge(Stats_Final_Year,Coumpound_Growth_Rate, by="Country.code")
                  Tab1<-merge(Tab1,Overall_Productivty, by="Country.code" )
                  
                  names(Tab1)[2:7]<-paste(names(Tab1)[2:7]," ",as.character(Final)) 
                  names(Tab1)[8]<-paste(names(Tab1)[8]," ", as.character(Start)," ", as.character(Final))
                  Tab1
                  }
            
          Tab1_PPP.2005<-Table1(DB=PDVTY_PPP,Start=1990,Final=2005);Tab1Q_PPP.2005<-Table1(DB=PDVTYQ_PPP,Start=1990,Final=2005)
          Tab1_LC.2005<-Table1(DB=PDVTY_LC,Start=1990,Final=2005); Tab1Q_LC.2005<-Table1(DB=PDVTYQ_LC,Start=1990,Final=2005)
          
          Tab1_PPP.2010<-Table1(DB=PDVTY_PPP,Start=1990,Final=2010);Tab1Q_PPP.2010<-Table1(DB=PDVTYQ_PPP,Start=1990,Final=2010)
          Tab1_LC.2010<-Table1(DB=PDVTY_LC,Start=1990,Final=2010); Tab1Q_LC.2010<-Table1(DB=PDVTYQ_LC,Start=1990,Final=2010)
          
                  #Alert! They are very different from Rodrik table! ##problem Solved: PPP, see 0
                  
                  
                  #!!#Problem with zambia, its teh most productive country(PDVTY_PPP)! 
          
                  #and is the most producing (given its small population)
                  ##there is a problem either with the convertor PPP or with data ##STILL THERE IS BIGG DIFFERENCES EVERYWHERE
               
                  ##Il faut d'abord recalculer l'overall pdvty en pondérant avec le nb de travaileurs#mais ca ne doit pas chegre PDVTY_PPP
                  #ni les vals max et min 
                  ##Comprtementbizarre du facteur de conv PPP * 10^5 pour BRA de 1990 à 2005
                  
a<-dta %>% filter(Year==2005,DB=="GGDC", PPP=="PPP", Variable=="ptyq") %>% spread(Variable, Value) 
ptyq_tot<-filter(a,  Sector=="SUM") 
varcoef<-filter(a,  Sector!="SUM") %>%group_by(Area) %>% summarize(coeff=100*var(log(ptyq))/mean(log(ptyq)) )
maxmin<-a %>% group_by(Area) %>% summarize(maxptyq=max(ptyq), maxsec=Sector[which.max(ptyq)],
                                           minptyq=max(ptyq), minsec=Sector[which.min(ptyq)]
                                           
                                           
                                           
                                           
                                           
                                           )



    #2- Table 2-Rodrik:####
        
        #Table 2 as a function     
            Table2<-function(DB, year){
        
                              P<-filter(DB, Year==year )
                              
                              Summary<-t(do.call(cbind, lapply(P, summary))) 
                              
                              Summary<-Summary[-c(1:3), c(1,3,4,6)]
                              
                              #Finding Countries with min and max pdvty
                                  D<-matrix(ncol=3, nrow=11) 
                                  
                                  colnames(D)=c("sector","Maximum Productivity Country","Minimum Productivity Country") 
                                  
                                  for (i in 1:11){ 
                                    
                                                  D[i,1]<-colnames(DB)[i+2]
                                                  
                                                  D[i,2]<-P[which.max (P[,i+2]),1] 
                                                  
                                                  D[i,3]<-P[which.min (P[,i+2]),1] 
                                   
                                                  } 
                              #Add it to Summary 2009
                              
                                  Summary<-cbind(Summary, D[,-1])
                              
                              #return value
                                 
                              
                            }
          
        Sum.PDVT.PPP.2005<-Table2(PDVTY_PPP,2005); Sum.PDVTY.LC.2005<-Table2(PDVTY_LC,2005)
        Sum.PDVTQ.PPP.2005<-Table2(PDVTYQ_PPP,2005); Sum.PDVTYQ.LC.2005<-Table2(PDVTYQ_LC,2005)
        
        #NB: Summary stats for the year 2005 coincide because almost #All quantities are expressed in 2005 prices
                                                                                                    
        
        #Alert! They are very different from Rodrik table
        ##Seem to have erroneous data from GGDC, IDN, KOR, etc-tried another download exceel and stata) same results
        ##Try data from other DB PWT or structural change DB,or EASD===>No need Rodrik is using PPP values, we shoud do the same-see above
        
        
    #3- Some Plots#### should create a plot function for panel (using ggplot2)####
        
        #Overall productivity against coefficient of variation.
        
        plot(Tab1Q_PPP.2005$`Overall_Productivty   2005`,Tab1Q_PPP.2005$`Coefficient_of_Variation   2005`)
        
        ggplot(data=PDVTYQ_PPP, mapping=aes(x=Year, y=Manufacturing))+geom_point() +
          
          facet_wrap(vars(Country.code), scales="free")
    
        ggplot(data=PDVTYQ_PPP, mapping=aes(x=Year, y=Agriculture))+geom_point() +
          
          facet_wrap(vars(Country.code), scales="free")
        
    #4- Advanced Plots: Using 'plotly'####
        
        
        library(plotly); 
       
        
      
        #Plotting manufacturing pdvty against agriculture's

              p <- plot_ly(dplyr::filter(PDVTYQ_LC, Manufacturing<200, Country.code=="MAR") ,
                
                type = 'scatter',
                x = ~Agriculture, y = ~Manufacturing,text= ~paste(Country.code,Year),color = ~Country.code,
                hoverinfo = 'text', mode="markers",frame=~Year,
                transforms=list(
                  list( type='filter', target='y',operation='<',value=1000
                       )
                )
                
                  )
              p
              #il faut 
              
              link<-api_create(p, filename = "filter")   # Create a shareable link to your chart
              link
          
          #create a table of overall pdvty and coeff of var
             
              disp<- function(PDVTY_DB){
                
              sdlog<-apply(log(PDVTY_DB[,4:13]),MARGIN= 1, sd, na.rm=TRUE)
              
              meanlog<-apply(log(PDVTY_DB[,4:13]),MARGIN= 1, mean, na.rm=TRUE)    
              
              disp<-cbind(PDVTY_DB[,c(1,2,3,14)], 100*sdlog/meanlog)
              names(disp)[4:5]<-c("Overall_PDVTY", "Coefficient_Of_Variation")
              disp
              }
              
              PDVTYQ_LC.disp<-disp(PDVTYQ_LC);PDVTYQ_PPP.disp=disp(PDVTYQ_PPP)
              
          #plot it
           
               p2 <- plot_ly(data=dplyr::filter(PDVTYQ_LC.disp, Overall_PDVTY<200 & !is.na(Overall_PDVTY) & Year>1960),
                           type = 'scatter3d',
                           x = ~Coefficient_Of_Variation,
                           y = ~Overall_PDVTY,
                           z=~Year,
                           color=~Region.code,
                           text=~Country.code,
                           hoverinfo='text',
                           mode="markers"
                       
                            
                          )
              p2
              
              p3<-plot_ly(data=dplyr::filter(PDVTYQ_LC.disp, Overall_PDVTY<200), x=~Year, y=~Overall_PDVTY, color=~Country.code, text=~Country.code,
                          hoverinfo='text')
                          
              p3
              
              p4<-plot_ly(data=dplyr::filter(PDVTYQ_LC.disp, Overall_PDVTY<200), x=~Year, y=~Coefficient_Of_Variation, color=~Country.code, text=~Country.code,
                          hoverinfo='text')
              
              p4
              
              rm(p,p2,p3,p4)
              
    #5- figure 1_Rodrik: as a function####
              
              fig1<- function(Country, Year){
              
              library(tidyverse)
              
              Employment.shares<-GGDC_large[,c(1:3, 15:25)]
              Employment.shares[,4:14]<-Employment.shares[,4:14]/Employment.shares[,14]
              Employment.shares<-gather(Employment.shares, key=Sector,value=Employment_Share, 4:14 )
              Employment.shares$Sector<-sub( pattern="EMP_", replacement = "", x=Employment.shares$Sector)
              
              PDVTYQ_LC.shares<-PDVTYQ_LC
              PDVTYQ_LC.shares[,4:14]<-PDVTYQ_LC.shares[,4:14]/PDVTYQ_LC.shares[,14]
              PDVTYQ_LC.shares<-gather(PDVTYQ_LC.shares, key="Sector", value='PDVTY_to_ovrall',4:14)
              PDVTYQ_LC.shares$Sector<-sub( pattern="EMP_", replacement = "", x=PDVTYQ_LC.shares$Sector)
              
              fig1<-merge(Employment.shares,PDVTYQ_LC.shares, by=names(Employment.shares)[1:4])
              
              fig1.Country.Year<-filter(fig1, Country.code==as.character(Country)& Year==2005)
              fig1.Country.Year<-fig1.Country.Year[order(fig1.Country.Year$PDVTY_to_ovrall),]
              
              rownames(fig1.Country.Year) <- 1:nrow(fig1.Country.Year)
              
              
              
              #second way to plot
              fig1.Country.Year<-fig1.Country.Year[fig1.Country.Year$Employment_Share!=1, ]
              fig1.Country.Year<-cbind(fig1.Country.Year, cum=cumsum(fig1.Country.Year[,5]))
              fig1.Country.Year<-cbind(fig1.Country.Year, center=fig1.Country.Year[7]-fig1.Country.Year[5]/2)
              
              names(fig1.Country.Year)[8]<-"center"
              
              ggplot(fig1.Country.Year, 
                     mapping=aes(x=center , y=PDVTY_to_ovrall, width=Employment_Share ))+
                geom_bar(aes(fill=Sector), stat="identity")
              
              
              }
             fig1(Country = "EGY", Year=2005)
  
###############################################
lagged<-GGDC_lg %>% group_by(Area,Region.code, Region, PPP, Sector, Variable) %>% arrange(Year) %>%
  mutate(first=dplyr::lag(Value,10),  
         last=Value,
         change=Value-first,
         growth=change/first
        ) %>% gather(operation, Value, c("last","first","change", "growth")) %>% filter(!is.na(Value))


#Calculer les composantes within et between

decomp<-lagged %>%as.data.frame()%>% 
  
  filter(Variable %in% c("EMP_share", "PDVTYQ", "PDVTY"),operation %in% c("last", "change", "first")) %>%
  
  mutate(Variable=paste0(Variable,"_", operation)) %>% select(-operation) %>% spread(Variable, Value) %>% 
  
  mutate(within_PDVTYQ=EMP_share_first*PDVTYQ_change,
         between_PDVTYQ=PDVTYQ_last*EMP_share_change,
         dd_PDVTYQ=PDVTYQ_change*EMP_share_change,
         
         within_PDVTY=EMP_share_first*PDVTY_change,
         between_PDVTY=PDVTY_last*EMP_share_change
         
        ) %>% gather(Variable, Value, 7:20) %>% 
  filter(Sector!="Total" |(Sector=="Total" & 
           ! Variable %in% c("within_PDVTYQ","between_PDVTYQ","within_PDVTY","between_PDVTY" ) ) )

decomp_tot<-decomp %>% filter(Sector!="Total") %>% spread(Variable, Value) %>% 
  group_by(Area,Region.code, Region, PPP, Year) %>% 
  summarize(between_PDVTY=sum(between_PDVTY, na.rm=T),
            within_PDVTY=sum(within_PDVTY, na.rm=T), 
            D_PDVTY=sum(between_PDVTY+within_PDVTY, na.rm=T),#just for check
            
            between_PDVTYQ=sum(between_PDVTYQ, na.rm=T),
            within_PDVTYQ=sum(within_PDVTYQ, na.rm=T), 
            D_PDVTYQ=sum(between_PDVTYQ+within_PDVTYQ, na.rm=T)#just for check 
            
            
            )#Controle3 Reussi:


#Keep Relevant vars and tidy
  
  indic_decomp<-c("EMP_share_change", "PDVTY_change", "PDVTYQ_change", "within_PDVTY", "between_PDVTY","within_PDVTYQ","between_PDVTYQ", "dd_PDVTYQ")
  
  decomp<-decomp %>% filter(Variable%in% indic_decomp); rm(indic_decomp)
  
  decomp_tot<-decomp_tot %>% gather(Variable, Value, 6:ncol(decomp_tot)) %>% as.data.frame()

  #bring together decomp and decomp tot: en considérant tot un secteur=summation.of.sectors GDP
  
  decomp_tot<-decomp_tot %>% mutate(Sector="Total")
  
  decomp<-rbind(decomp, decomp_tot) %>% filter(!is.na(Value)) #We can Choose to rbind it to GGDC_lg or not
  rm(decomp_tot); unique(decomp$Variable)#We have done the most important, now turn to analysis
                            
  sectors<-unique(GGDC_lg["Sector"]) %>% filter(Sector!="Total")
  
  decomp_share<-decomp %>% filter(PPP=="LC", Variable %in% c("within_PDVTYQ","between_PDVTYQ")) %>% 
  spread(Sector, Value) %>% mutate_at(sectors$Sector, ~./`Total`) %>% select(-Total) %>% 
    gather(Sector, Value, sectors$Sector)
  
  p<-decomp %>%filter( Variable %in% c("within_PDVTYQ","between_PDVTYQ", "PDVTYQ_change"),PPP=="PPP", Sector=="Total", 
                     !Area%in%c("ZMB", "BRA", "VEN"), Year%%10==0) %>% 
    spread(Variable, Value) %>% 
  mutate_at(vars("within_PDVTYQ","between_PDVTYQ"), ~./PDVTYQ_change)  %>% group_by(Region) %>% 
  ggplot()+theme_bw() + #geom_point(aes(within_PDVTYQ,between_PDVTYQ, color=Region), size=1/2)+facet_trelliscope(~Region)
  geom_density(aes(between_PDVTYQ, color=Region))
    p %>% ggplotly()
   
  