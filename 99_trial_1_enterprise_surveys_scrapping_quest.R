#___________________________________________________6- enterprise survey:scrapiing questionnaire _______________________________________________________11/mars/2020
rm(list=ls())
#_______________________________________________________________________________________________________________________________________________________________
#________________________________________________________attempt2
rm(list=ls())
path<-"Inputs/9-WB firms survey 2003_2019"
pdf.file <- paste0(path,"/", "Morocco2007_Manufacturing_FR.pdf")

#read the file into character voctor
library(pdftools)
quest.text<-pdf_text(pdf.file)
quest.text<-quest.text[-c(1:3)]


lines <- quest.text %>%  lapply(str_split, "\n", simplify = TRUE)
lines<-lines %>% reduce(c)


#Recovering one line questions
pattern_var<-"(^\\s+|^)[[:upper:]]\\.[[:digit:]]"#detects lines starting with any number of spaces(\\s+)

short<-grep(x=lines, pattern=pattern_var, value=T ) %>% sub(pattern="^\\s+", replacement = "")


#Recovering long (2lines) questions
  
  p2<-grepl(x=lines, pattern=pattern_var )#starting with question
  p3<-grepl(x=lines, pattern="\\?")#containing with question
  #p4<-grepl(x=lines, pattern="\\?$")#ending with question; table(p4: no one)
  
  p5<-p2& p3#starting and cotaining with question: 69 lines/
  table(p5)
  
  p6<-p2[-length(p2)] & p3[-1] #starting and followed by containing question: 41 /
  table(p6)

  #recover the start and the following line
    start<-lines[p6]
    end<-lines[which(p6==T)+1]
  
  #paste the two  
  long<-paste(start, end) %>% sub(pattern="^\\s+", replacement = "")
    

#splitting the questions: code and labels

short<-str_split(short, " ", n=2) %>% reduce(rbind) #goes the first "space and splits the string 
long<-str_split(long, " ", n=2) %>% reduce(rbind) #goes the first "space and splits the string 


var.labels_short<-tibble(code= short[,1], label=short[, 2]) #%>% mutate(code=sub(x=code, pattern=" ", replacement=""))
var.labels_long<-tibble(code= long[,1], label=long[, 2]) #%>% mutate(code=sub(x=code, pattern=" ", replacement=""))

#combine the two
var.labels<-rbind(subset(var.labels_short, !code%in%var.labels_long$code ),
                  var.labels_long)

#Clean up
var.labels<-filter(var.labels,(label!="J.6a\r" ))

var.labels$code<-var.labels$code %>% gsub(pattern="(\r)|(^\\s+)", replacement ="")
var.labels$label<-var.labels$label %>% gsub(pattern="(\r)|(^\\s+)", replacement ="")

#manual correction of the two remianing problems D.16 E.1 J.6a

var.labels$label[var.labels$code=="D.16"]<-"Au moment où votre établissement reçoit sa matière première ou son principal intrant, combien de jours de stock l’entreprise tient-elle en moyenne (intrants utilisés en production)"
var.labels$label[var.labels$code=="E.1"]<-"Durant le courant de l’année 2005, dans quel marché avez-vous principalement opéré (vendu vos produits/ services) ?"


#Save
#var.labels %>% stargazer::stargazer(type="text",summary = F, out = "vars_wb0407.txt")
var.labels %>% write.csv2("MAR_enterprise_survey_07_var.labels.csv")


#Another way...try to recover full questions
#Train in plain
plain<- quest.text %>%reduce(paste) 
a<-gregexpr(pattern="[[:upper:]]\\.[[:digit:]].\\s{5}", text=plain)

line1<-"((\n\\s+)|(\n))[[:upper:]]\\.[[:digit:]]{1,2}[\\.[[:lower:]]]{0,1}[[:alnum:][:blank:][:punct:]]*(\r|\n)\\s{0,5}[[:upper:]]{0}[[:alnum:]]*[[:alnum:][:blank:][:punct:]]*(\r|\n)"
#line2<-"\\s{0,5}[[:upper:]]{0}[[:alnum:][:blank:][:punct:]]*(\r|\n)"
#pattern<-paste0(line1,line2)
questions<-str_extract_all(pattern=line1,plain, simplify = T)

questions<-questions %>%  sub(pattern="^\n*\\s*", replacement="")

questions<-str_split_fixed(questions, "\\s",n =2) 

questions[,2]<-gsub(questions[, 2], pattern="^\\s+", replacement = "")
questions[,2]<-gsub(questions[, 2], pattern="[[:cntrl:]]|\\s+", replacement = " ")

getParseData(questions,includeText = TRUE)

var.labels2<-data.frame(code= questions[,1], label=questions[, 2])
var.labels2 %>% write.csv2("MAR_enterprise_survey_07_var.labels2.csv")
