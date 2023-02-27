#Kruskal-Wallis Test: Change read_excel function to change between excel sheets

library(readxl)
library(FSA)
library(dplyr)
KW_data <- read.csv("20210706_KW_data.csv")
View(KW_data)

#Caparison of same ethnicity in different regions
#For any KW test change Race to "NHB" , "NHA" or "Hispanic". Change region to "South","Midwest", "West"

#NHW
Data<-KW_data%>%filter(Race=="NHW")%>% filter(Region=="Northeast")
Data
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)

Data<-KW_data%>%filter(Race=="NHW")%>% filter(Region=="South")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)

Data<-KW_data%>%filter(Race=="NHW")%>% filter(Region=="Midwest")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)


Data<-KW_data%>%filter(Race=="NHW")%>% filter(Region=="West")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)



Data<-KW_data%>%filter(Race=="NHW")%>% filter(Region=="All Regions")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)

#NHB

Data<-KW_data%>%filter(Race=="NHB")%>% filter(Region=="Northeast")
Data
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)

Data<-KW_data%>%filter(Race=="NHB")%>% filter(Region=="South")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)

Data<-KW_data%>%filter(Race=="NHB")%>% filter(Region=="Midwest")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)


Data<-KW_data%>%filter(Race=="NHB")%>% filter(Region=="West")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)



Data<-KW_data%>%filter(Race=="NHB")%>% filter(Region=="All Regions")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)

#Hispanic
Data<-KW_data%>%filter(Race=="Hispanic")%>% filter(Region=="Northeast")
Data
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)

Data<-KW_data%>%filter(Race=="Hispanic")%>% filter(Region=="South")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)

Data<-KW_data%>%filter(Race=="Hispanic")%>% filter(Region=="Midwest")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)


Data<-KW_data%>%filter(Race=="Hispanic")%>% filter(Region=="West")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)



Data<-KW_data%>%filter(Race=="Hispanic")%>% filter(Region=="All Regions")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)

#NHA
Data<-KW_data%>%filter(Race=="NHA")%>% filter(Region=="Northeast")
Data
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)

Data<-KW_data%>%filter(Race=="NHA")%>% filter(Region=="South")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)

Data<-KW_data%>%filter(Race=="NHA")%>% filter(Region=="Midwest")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)


Data<-KW_data%>%filter(Race=="NHA")%>% filter(Region=="West")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)



Data<-KW_data%>%filter(Race=="NHA")%>% filter(Region=="All Regions")
kruskal.test(Population ~ Group,
             data = Data)
dunnTest(Population ~ Group,
         data = Data)


