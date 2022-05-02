
library(stringr)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(readr)
library(tibble)
library(ggplot2)
library(ggpattern)
library(cowplot)
library(plotly)

footnote <- function(codes) {

  c(codes,paste0("??? ",codes),paste0("???",codes))

}

facet_strip_bigger <- function(gp, size){
  if(missing(gp)){
    print("this function needs a facet_wrap ggplotly object")
  }
  if(missing(size)){
    print("this function needs 'size' argument to be specified as integer. 80 will be introduced as default")
    size <- 80
  }

  n_facets <- c(1:length(gp[["x"]][["layout"]][["shapes"]]))

  for(i in n_facets){
    if(n_facets[i] %% 2 == 0){
      gp[["x"]][["layout"]][["shapes"]][[i]][["y0"]] <- + as.numeric(size)
      gp[["x"]][["layout"]][["shapes"]][[i]][["y1"]] <- 0
    }
  }

  return(gp)
}


#OSPC4
transproc <- c("X15.1","X15.2","X15.4","X15.8","X15.9")
procedurecodes <- footnote(transproc)

mastectomyproc <-c("B27.4","B27.5","B27.6")
phalloplastyproc <- "N28.1"
hysterproc <- c("Q07.3","Q07.4","Q07.5","Q07.8","Q07.9",
                "Q08.3","Q08.8","Q08.9",
                "Q22.1","Q22.2","Q22.3",
                "Q24.1","Q24.2","Q24.3")
orchidectomyproc <- c("N05.1","N05.2","N05.3","N05.8","N06.3")
penectomyproc <-  c("N26.1","N26.2","N26.8","N26.9")
vagplasty <- c("P21.2","P21.3","P21.5")
oocyte <- c("Q48.1","Q48.2","Q48.3","Q48.4","Q48.8","Q48.9")

sperm <- c("N34.1","N34.2","N34.4","N34.5","N34.6","N34.8")


altcodes <- footnote(c(mastectomyproc,phalloplastyproc,hysterproc,orchidectomyproc,penectomyproc,
                       vagplasty,oocyte,sperm))


#ICD10
transdiag <- c("F64.0","F64.1","F64.2","F64.8","F64.9")

CollectedData <- readRDS("../ProcessedData/ProceduresDiag.rds")

relevantcol <- c("year","type","Code","Description","FinEpisodes","Admissions",
                 "GenderMale","GenderFemale","GenderUnknown",
                 "MeanWaiting","MedianWaiting","MeanStay","MedianStay")
yearlabels <- stringr::str_replace_all(unique(CollectedData$year),"to","-")
diaglabels <- c("F64.0\nTranssexualism","F64.1\nDual-role transvestism","F64.2\nGID childhood","F64.8\nOther GIDs","F64.9\nGID, unspecified")



totalproc <- CollectedData %>% filter(type=="Procedures") %>%
  select(relevantcol) %>%
  select(c("year","type","Code","Description","FinEpisodes",
           "GenderMale","GenderFemale","GenderUnknown")) %>%
  mutate(across(c("FinEpisodes","GenderMale","GenderFemale","GenderUnknown"),as.numeric)) %>%
  replace_na(list(GenderFemale=0,GenderUnknown=0)) %>%
  mutate(NotProvided = FinEpisodes - GenderMale - GenderFemale -GenderUnknown) %>%

  pivot_longer(cols=c("GenderMale","GenderFemale","GenderUnknown","NotProvided"),names_to="gender",values_to="value") %>%
  group_by(year,gender) %>%
  mutate(value = as.numeric(value),
         FinEpisodes = as.numeric(FinEpisodes)) %>%
  summarize(totalep = sum(FinEpisodes,na.rm=T),
            sumvalue = sum(value,na.rm=T)) %>%
  mutate(gender= factor(gender,levels=rev(c("GenderMale","GenderUnknown","GenderFemale","NotProvided")),
                        labels=rev(c("Male","Unknown","Female","N/A"))))


proclabels <- c("X15.1\nTransformation from\nmale to female",
                "X15.2\nTransformation from\nfemale to male",
                "X15.4\nConstruction\nof scrotum",
                "X15.8\nOther specified operations\nfor sexual transformation",
                "X15.9\nUnspecified operations\nfor sexual transformation")

totalprocbytype <- CollectedData %>% filter(type=="Procedures") %>%
  select(relevantcol) %>%

  mutate(across(c("FinEpisodes","GenderMale","GenderFemale","GenderUnknown","MedianWaiting"),as.numeric)) %>%
  replace_na(list(GenderFemale=0,GenderUnknown=0)) %>%
  mutate(NotProvided = FinEpisodes - GenderMale - GenderFemale -GenderUnknown) %>%

  pivot_longer(cols=c("GenderMale","GenderFemale","GenderUnknown","NotProvided"),names_to="gender",values_to="value") %>%
  group_by(year,Code,gender) %>%
  mutate(value = as.numeric(value),
         FinEpisodes = as.numeric(FinEpisodes)) %>%
  summarize(totalep = sum(FinEpisodes,na.rm=T),
            sumvalue = sum(value,na.rm=T)) %>%
  mutate(gender= factor(gender,levels=rev(c("GenderMale","GenderUnknown","GenderFemale","NotProvided")),
                        labels=rev(c("Male","Unknown","Female","N/A")))) %>%
  mutate(Code = factor(Code,levels=transproc[1:5],labels=proclabels)) %>%
  rename("number" = sumvalue)



totalproc_bytype<-ggplot(totalprocbytype, aes(x = number, y= year, fill = gender,label=ifelse(number == 0, "",number))) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  facet_grid(.~Code)+
  geom_bar(stat="identity",color="transparent")+
  #geom_text(size = 3, position = position_stack(vjust = 0.5),color="transparent")+

  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Finished Consultant Episodes")+
  #coord_flip() +
  scale_fill_manual(name="Gender Marker",values=c("#FC8D62","#E78AC3","#66C2A5","#8DA0CB")) +
  theme_bw()+
  theme(axis.text= element_text(size=10),
        axis.title= element_text(size=10),
        axis.title.y = element_blank(),
        strip.text = element_text(size=10),
        strip.background = element_rect(color="transparent",fill="white"),
        legend.title  = element_text(size=10))



proctype <- ggplotly(totalproc_bytype, tooltip = c("gender","number")) %>%
  layout(margin = list(t = 110),
         legend=list(itemclick = "toggleothers",
                     bgcolor="white",
                     bordercolor="grey",
                     y = 0.5,
                     title = list(font = list(size=16)),
                     font = list(size=14)),
         hovermode = "closest") %>%
  facet_strip_bigger(60)

htmlwidgets::saveWidget(proctype, file.path(getwd(), "standalonesite", "procedures_bytype.html"), selfcontained = F,libdir="lib")


plotly

associateddata <- CollectedData %>% filter(type=="Procedures") %>%
  select(year,type,Code,
         FinEpisodes,Admissions,
         Emergency,WaitingList,Planned,OtherAdmission,
         MedianWaiting,MedianStay,
         DayCase) %>%

  mutate(across(c("FinEpisodes","Admissions","Emergency","WaitingList","Planned",
                  "OtherAdmission","MedianWaiting","MedianStay","DayCase"),as.numeric)) %>%
  replace_na(list(Planned=0,OtherAdmission=0,Emergency=0))


admissiontype <-  associateddata %>%
  select(c("year","type","Code","FinEpisodes","Admissions","Emergency","WaitingList","Planned",
           "OtherAdmission" )) %>%
  mutate(OtherAdmission = ifelse(Admissions !=  Emergency + WaitingList + Planned + OtherAdmission,
                                 Admissions - (Emergency + WaitingList + Planned + OtherAdmission),
                                 OtherAdmission)) %>%
  pivot_longer(cols=c("Emergency","WaitingList","Planned","OtherAdmission"),names_to="Admission",values_to="value") %>%
  group_by(year,Code) %>%

  mutate(Code = factor(Code,levels=transproc[1:5],labels=proclabels))


admissions_graph <- ggplot(admissiontype, aes(x = value, y= year, fill = Admission)) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  facet_grid(.~Code,scales = "free")+

  geom_bar(data = admissiontype %>% select(year,Code,FinEpisodes) %>% distinct() %>% rename("value"=FinEpisodes),fill="transparent",color="black",linetype="dashed",position=position_dodge(0.9),stat="identity")+

  geom_bar(data=admissiontype, aes(x = value, y= year, fill = Admission,label=ifelse(value == 0, "",value)),
           stat="identity",color="black")+
  geom_text(data=admissiontype, aes(x = value, y= year, fill = Admission,label=ifelse(value == 0, "",value)),size = 3, position = position_stack(vjust = 0.5))+
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Finished Consultant Episodes")+
  #coord_flip() +
  scale_fill_manual(name="Admissions",values=c("#FC8D62","#E78AC3","#66C2A5","#8DA0CB")) +
  theme_bw()+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=14),
        legend.title  = element_text(size=14))

daycases <- associateddata %>%
  select(c("year","type","Code","FinEpisodes","DayCase","MedianStay","MedianWaiting")) %>%
  group_by(year,Code) %>%

  mutate(Code = factor(Code,levels=transproc[1:5],labels=proclabels))



daycase_graph <- ggplot(daycases, aes(x = DayCase, y= year)) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  facet_grid(.~Code,scales = "free")+

  geom_bar(data = daycases %>% select(year,Code,FinEpisodes) %>% distinct() %>% rename("DayCase"=FinEpisodes),fill="transparent",color="black",linetype="dashed",position=position_dodge(0.9),stat="identity")+

  geom_bar(data=daycases, aes(x = DayCase, y= year),fill="#FFD92F",
           stat="identity",color="black")+
  geom_text(data=daycases, aes(x = DayCase, y= year, label=ifelse(DayCase == 0, "",DayCase)),
            size = 3, position = position_stack(vjust = 0.5))+
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Finished Consultant Episodes")+
  theme_bw()+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=14),
        legend.title  = element_text(size=14))


stay_graph <- ggplot(daycases, aes(x = MedianStay, y= year)) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  facet_grid(.~Code)+

  geom_bar(data=daycases, aes(x = MedianStay, y= year),fill="#FFD92F",
           stat="identity",color="black")+
  geom_text(data=daycases, aes(x = MedianStay, y= year, label=ifelse(MedianStay == 0, "",MedianStay)),
            size = 3, position = position_stack(vjust = 0.5))+
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Median Hospital Stay (in days)")+
  theme_bw()+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=14),
        legend.title  = element_text(size=14))


WaitingTimesbytype <- ggplot(daycases %>% select(year,Code,MedianWaiting) %>% distinct() ,
                             aes(x = MedianWaiting/7, y= year,
                                 label=ifelse(MedianWaiting == 0, "",round(MedianWaiting/7,1)))) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  facet_grid(.~Code)+
  geom_bar(stat="identity",color="black",fill="#FFD92F" )+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+

  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Median Waiting Time (in weeks)")+
  #coord_flip() +

  theme_bw()+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        legend.text = element_text(size=14),
        legend.title  = element_text(size=14),
        strip.text = element_text(size=14))

detailgraph <- plot_grid(admissions_graph,daycase_graph,stay_graph,WaitingTimesbytype, nrow=4,labels ="AUTO")


totaldiag <- CollectedData %>% filter(type=="Diagnoses") %>%
  select(relevantcol) %>%
  select(c("year","type","Code","Description","FinEpisodes",
           "GenderMale","GenderFemale","GenderUnknown")) %>%
  mutate(across(c("FinEpisodes","GenderMale","GenderFemale","GenderUnknown"),as.numeric)) %>%
  replace_na(list(GenderFemale=0,GenderUnknown=0)) %>%
  mutate(NotProvided = FinEpisodes - GenderMale - GenderFemale -GenderUnknown) %>%

  pivot_longer(cols=c("GenderMale","GenderFemale","GenderUnknown","NotProvided"),names_to="gender",values_to="value") %>%
  group_by(year,gender) %>%
  mutate(value = as.numeric(value),
         FinEpisodes = as.numeric(FinEpisodes)) %>%
  summarize(totalep = sum(FinEpisodes,na.rm=T),
            sumvalue = sum(value,na.rm=T)) %>%
  mutate(gender= factor(gender,levels=rev(c("GenderMale","GenderUnknown","GenderFemale","NotProvided")),
                        labels=rev(c("Male","Unknown","Female","N/A"))))

diag<-ggplot(totaldiag, aes(x = sumvalue, y= year, fill = gender,label=ifelse(sumvalue == 0, "",sumvalue))) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  geom_bar(stat="identity",color="black")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+

  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Finished Consultant Episodes")+
  #coord_flip() +
  scale_fill_manual(name="Gender Marker",values=c("#FC8D62","#E78AC3","#66C2A5","#8DA0CB")) +
  theme_bw()

# gender diagnosis by type


totaldiag_type <- CollectedData %>% filter(type=="Diagnoses") %>%
  select(relevantcol) %>%
  select(c("year","type","Code","Description","FinEpisodes",
           "GenderMale","GenderFemale","GenderUnknown")) %>%
  mutate(across(c("FinEpisodes","GenderMale","GenderFemale","GenderUnknown"),as.numeric)) %>%
  replace_na(list(GenderFemale=0,GenderUnknown=0)) %>%
  mutate(NotProvided = FinEpisodes - GenderMale - GenderFemale -GenderUnknown) %>%

  pivot_longer(cols=c("GenderMale","GenderFemale","GenderUnknown","NotProvided"),names_to="gender",values_to="value") %>%
  group_by(year,Code,gender) %>%
  mutate(value = as.numeric(value),
         FinEpisodes = as.numeric(FinEpisodes)) %>%
  summarize(totalep = sum(FinEpisodes,na.rm=T),
            sumvalue = sum(value,na.rm=T)) %>%
  mutate(gender= factor(gender,levels=rev(c("GenderMale","GenderUnknown","GenderFemale","NotProvided")),
                        labels=rev(c("Male","Unknown","Female","N/A")))) %>%
  mutate(Code = factor(Code,levels=transdiag,
                       labels=diaglabels))

totaldiag_bytype<-ggplot(totaldiag_type, aes(x = sumvalue, y= year, fill = gender,label=ifelse(sumvalue == 0, "",sumvalue))) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  facet_grid(.~Code,scales = "free")+
  geom_bar(stat="identity",color="black")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+

  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Finished Consultant Episodes")+
  #coord_flip() +
  scale_fill_manual(name="Gender Marker",values=c("#FC8D62","#E78AC3","#66C2A5","#8DA0CB")) +
  theme_bw()+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=14),
        legend.title  = element_text(size=14))




associateddataD <- CollectedData %>% filter(type=="Diagnoses") %>%
  select(year,type,Code,
         FinEpisodes,Admissions,
         Emergency,WaitingList,Planned,OtherAdmission,
         MedianWaiting,MedianStay,
         DayCase) %>%

  mutate(across(c("FinEpisodes","Admissions","Emergency","WaitingList","Planned",
                  "OtherAdmission","MedianWaiting","MedianStay","DayCase"),as.numeric)) %>%
  replace_na(list(Planned=0,OtherAdmission=0,Emergency=0))


admissiontypeD <-  associateddataD %>%
  select(c("year","type","Code","FinEpisodes","Admissions","Emergency","WaitingList","Planned",
           "OtherAdmission" )) %>%
  mutate(OtherAdmission = ifelse(Admissions !=  Emergency + WaitingList + Planned + OtherAdmission,
                                 Admissions - (Emergency + WaitingList + Planned + OtherAdmission),
                                 OtherAdmission)) %>%
  pivot_longer(cols=c("Emergency","WaitingList","Planned","OtherAdmission"),names_to="Admission",values_to="value") %>%
  group_by(year,Code) %>%
  mutate(Code = factor(Code,levels=transdiag,
                       labels=diaglabels))


admissions_graphD <- ggplot(admissiontypeD, aes(x = value, y= year, fill = Admission)) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  facet_grid(.~Code,scales = "free")+

  geom_bar(data = admissiontypeD %>% select(year,Code,FinEpisodes) %>% distinct() %>% rename("value"=FinEpisodes),fill="transparent",color="black",linetype="dashed",position=position_dodge(0.9),stat="identity")+

  geom_bar(data=admissiontypeD, aes(x = value, y= year, fill = Admission,label=ifelse(value == 0, "",value)),
           stat="identity",color="black")+
  geom_text(data=admissiontypeD, aes(x = value, y= year, fill = Admission,label=ifelse(value == 0, "",value)),size = 3, position = position_stack(vjust = 0.5))+
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Finished Consultant Episodes")+
  #coord_flip() +
  scale_fill_manual(name="Admissions",values=c("#FC8D62","#E78AC3","#66C2A5","#8DA0CB")) +
  theme_bw()+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=14),
        legend.title  = element_text(size=14))

daycasesD <- associateddataD %>%
  select(c("year","type","Code","FinEpisodes","DayCase","MedianStay","MedianWaiting")) %>%
  group_by(year,Code)  %>%
  mutate(Code = factor(Code,levels=transdiag,
                       labels=diaglabels))




daycase_graphD <- ggplot(daycasesD, aes(x = DayCase, y= year)) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  facet_grid(.~Code,scales = "free")+

  geom_bar(data = daycasesD %>% select(year,Code,FinEpisodes) %>% distinct() %>% rename("DayCase"=FinEpisodes),fill="transparent",color="black",linetype="dashed",position=position_dodge(0.9),stat="identity")+

  geom_bar(data=daycasesD, aes(x = DayCase, y= year),fill="#FFD92F",
           stat="identity",color="black")+
  geom_text(data=daycasesD, aes(x = DayCase, y= year, label=ifelse(DayCase == 0, "",DayCase)),
            size = 3, position = position_stack(vjust = 0.5))+
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Finished Consultant Episodes")+
  theme_bw()+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=14),
        legend.title  = element_text(size=14))


stay_graphD <- ggplot(daycasesD, aes(x = MedianStay, y= year)) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  facet_grid(.~Code,scales="free")+

  geom_bar(data=daycasesD, aes(x = MedianStay, y= year),fill="#FFD92F",
           stat="identity",color="black")+
  geom_text(data=daycasesD, aes(x = MedianStay, y= year, label=ifelse(MedianStay == 0, "",MedianStay)),
            size = 3, position = position_stack(vjust = 0.5))+
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Median Hospital Stay (in days)")+
  theme_bw()+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=14),
        legend.title  = element_text(size=14))


WaitingTimesbytypeD <- ggplot(daycasesD %>% select(year,Code,MedianWaiting) %>% distinct() ,
                              aes(x = MedianWaiting/7, y= year,
                                  label=ifelse(MedianWaiting == 0, "",round(MedianWaiting/7,1)))) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  facet_grid(.~Code)+
  geom_bar(stat="identity",color="black",fill="#FFD92F" )+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+

  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Median Waiting Time (in weeks)")+
  #coord_flip() +

  theme_bw()+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        legend.text = element_text(size=14),
        legend.title  = element_text(size=14))

detailgraphD <- plot_grid(daycase_graphD,stay_graphD,WaitingTimesbytypeD, nrow=3,labels ="AUTO")



CorrectFCEs <- CollectedData %>% filter(type=="Diagnoses") %>%
  select(year,type,Code,
         FinEpisodes,GenderMale,GenderFemale,GenderUnknown,
         DayCase) %>%
  mutate(across(c("FinEpisodes","GenderMale","GenderFemale","GenderUnknown","DayCase"),as.numeric)) %>%
  replace_na(list(GenderFemale=0,GenderUnknown=0)) %>%
  mutate(NotProvided = FinEpisodes - GenderMale - GenderFemale -GenderUnknown) %>%
  mutate(CGenderMale = GenderMale - round((DayCase * GenderMale/(GenderMale + GenderFemale+GenderUnknown + NotProvided) )),
         CGenderFemale = GenderFemale - round((DayCase * GenderFemale/(GenderMale + GenderFemale+GenderUnknown + NotProvided) )),
         CGenderUnknown = GenderUnknown- round((DayCase * GenderUnknown/(GenderMale + GenderFemale+GenderUnknown + NotProvided) )),
         CNotProvided= NotProvided- round((DayCase * NotProvided/(GenderMale + GenderFemale+GenderUnknown + NotProvided) )) ) %>%

  mutate(CFinEpisodes = CGenderMale + CGenderFemale + CGenderUnknown + CNotProvided) %>%
  mutate(CGenderMale = ifelse((FinEpisodes == CFinEpisodes & DayCase > 0),CGenderMale - DayCase,CGenderMale),
         CFinEpisodes = ifelse((FinEpisodes == CFinEpisodes & DayCase > 0),CFinEpisodes - DayCase,CFinEpisodes)) %>%
  pivot_longer(cols=c("CGenderMale","CGenderFemale","CGenderUnknown","CNotProvided",
                      "GenderMale","GenderFemale","GenderUnknown","NotProvided"),names_to="gender",values_to="value") %>%
  mutate(correction = ifelse(gender %in% c("CGenderMale","CGenderFemale","CGenderUnknown","CNotProvided"),"Corrected","Uncorrected")) %>%
  group_by(year,correction,gender) %>%
  mutate(value = as.numeric(value),
         CFinEpisodes = as.numeric(CFinEpisodes)) %>%
  summarize(ctotalep = sum(CFinEpisodes,na.rm=T),

            totalep = sum(FinEpisodes),
            sumvalue = sum(value,na.rm=T)) %>%
  mutate(gender= factor(gender,levels=c(rev(c("GenderMale","GenderUnknown","GenderFemale","NotProvided")),
                                        rev(c("CGenderMale","CGenderUnknown","CGenderFemale","CNotProvided"))),
                        labels=rep(rev(c("Male","Unknown","Female","N/A")),2)))%>%
  mutate(year = factor(year,levels=unique(totaldiag$year),labels=yearlabels))

totalproc <- totalproc %>% mutate(year = factor(year,levels=unique(totaldiag$year),labels=yearlabels))

procedurevsdiag <- ggplot(CorrectFCEs %>% filter(correction == "Uncorrected"), aes(x = sumvalue, y=gender,fill = gender)) +
  facet_wrap(.~year,nrow=2)+
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  geom_bar(data=CorrectFCEs %>% filter(correction == "Uncorrected"), aes(x = sumvalue, y=gender),stat="identity",color="black",position=position_dodge(0.9),alpha=0.1,fill="transparent",linetype="dashed")+
  geom_bar(data=CorrectFCEs %>% filter(correction == "Corrected") , aes(x = sumvalue, y=gender,fill = gender),stat="identity",color="black",position=position_dodge(0.9),alpha=0.1,size=1)+
  geom_col_pattern(data=totalproc,aes(x=sumvalue,y=gender,fill=gender),alpha=0.3,
                   pattern="stripe",
                   pattern_fill="black",color="black", pattern_key_scale_factor = 1.2
  )+

  #geom_bar(data=totprocedures,aes(x=sumvalue,y=gender,fill=gensource),color="black")+
  scale_y_discrete(name="Gender Marker")+
  scale_x_continuous(name="Finished Consultant Episodes")+
  #coord_fixed(ratio = 2/1) +

  theme_bw() +

  scale_fill_manual(name="Procedure: X15",values=c("#FC8D62","#E78AC3","#66C2A5","#8DA0CB")) +
  theme(legend.key.size = unit(1.5, 'cm'))+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=14),
        legend.title  = element_text(size=14))



altprocbytype <- CollectedData %>% filter(type=="AltProcedures") %>%
  select(relevantcol) %>%
  mutate(across(c("FinEpisodes","GenderMale","GenderFemale","GenderUnknown","MedianWaiting"),as.numeric)) %>%
  replace_na(list(GenderFemale=0,GenderUnknown=0)) %>%
  mutate(NotProvided = FinEpisodes - GenderMale - GenderFemale -GenderUnknown) %>%

  pivot_longer(cols=c("GenderMale","GenderFemale","GenderUnknown","NotProvided"),names_to="gender",values_to="value") %>%
  mutate(GroupCode = case_when(Code %in% mastectomyproc ~ "Mastectomy",
                               Code %in% phalloplastyproc ~ "Phalloplasty",
                               Code %in% hysterproc ~ "Hysterectomy",
                               Code %in% orchidectomyproc ~ "Orchidectomy",
                               Code %in% penectomyproc ~ "Penectomy",
                               Code %in% vagplasty ~ "Vaginoplasty",
                               Code %in% oocyte ~ "Oocyte recovery",
                               Code %in% sperm ~ "Aspiration of sperm",
                               TRUE ~ "NA")) %>%
  group_by(year,GroupCode,gender) %>%
  mutate(value = as.numeric(value),
         FinEpisodes = as.numeric(FinEpisodes)) %>%
  summarize(totalep = sum(FinEpisodes,na.rm=T),
            sumvalue = sum(value,na.rm=T)) %>%
  mutate(gender= factor(gender,levels=rev(c("GenderMale","GenderUnknown","GenderFemale","NotProvided")),
                        labels=rev(c("Male","Unknown","Female","N/A")))) %>%
  mutate(GroupCode = factor(GroupCode,levels= c("Mastectomy","Phalloplasty","Hysterectomy","Oocyte recovery",
                                                "Orchidectomy","Penectomy","Vaginoplasty",
                                                "Aspiration of sperm"),
                            labels=c("Mastectomy","Phalloplasty","Hysterectomy","Oocyte recovery",
                                     "Orchidectomy","Penectomy","Vaginoplasty",
                                     "Aspiration of sperm")))


altproccodes <- altprocbytype %>% filter((GroupCode %in% c("Mastectomy","Hysterectomy","Phalloplasty","Oocyte recovery") & gender %in% c("Male","Unknown")) |
                                           (GroupCode %in% c("Orchidectomy","Penectomy","Vaginoplasty","Aspiration of sperm") & gender %in% c("Female","Unknown","N/A")))

altproc_bytype<-ggplot(altproccodes, aes(x = sumvalue, y= year, fill = gender,label=ifelse(sumvalue == 0, "",sumvalue))) +
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  facet_wrap(.~GroupCode,scales = "free",nrow=2)+
  geom_bar(stat="identity",color="black",alpha=0.8)+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+

  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="transparent",color="black",stat="identity")+

  scale_y_discrete(labels = yearlabels,name="Period")+
  scale_x_continuous(name="Finished Consultant Episodes")+
  #coord_flip() +
  scale_fill_manual(name="Gender Marker",values=c("#FC8D62","#E78AC3","#66C2A5","#8DA0CB")) +
  theme_bw()+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        legend.text = element_text(size=14),
        strip.text = element_text(size=14),
        legend.title  = element_text(size=14))


altcodesprocedures <- altproccodes %>%
  filter(!GroupCode %in% c("Aspiration of sperm","Oocyte recovery")) %>%
  group_by(year,gender) %>% summarize(sums = sum(sumvalue)) %>%
  rename("sumvalue" = sums) %>% mutate(source = "alt") %>%
  mutate(year = factor(year,levels=unique(totaldiag$year),labels=yearlabels))

x15procedures <-  totalproc %>%  mutate(source = "x15")

totprocedures <- bind_rows(altcodesprocedures,x15procedures) %>% mutate(gensource = paste0(gender,"_",source))

diagforproc <- CorrectFCEs %>% filter(correction == "Corrected") %>%
  mutate(gensource = paste0(gender,"_","x15"))
#totalproc <- totalproc %>% mutate(year=factor(year,levels=unique(totaldiag$year),labels=yearlabels))

procedurevsdiagALT <- ggplot(diagforproc, aes(x = sumvalue, y=gender,fill = gender,label=ifelse(sumvalue == 0, "",sumvalue))) +
  facet_wrap(.~year,nrow=2)+
  # geom_bar(data=totaldiag %>% filter(gender=="Male"), aes(x=totalep,y=year),
  #          fill="grey",color="black",stat="identity")+
  geom_bar(stat="identity",color="black",position=position_dodge(0.9),alpha=0.1,size=1)+
  geom_col_pattern(data=totprocedures,aes(x=sumvalue,y=gender,fill=gender, pattern=as.factor(source)),alpha=0.3,pattern_fill="black",color="black",
                   pattern_key_scale_factor = 1.2
  )+

  #geom_bar(data=totprocedures,aes(x=sumvalue,y=gender,fill=gensource),color="black")+
  scale_y_discrete(name="Gender Marker")+
  scale_x_continuous(name="Finished Consultant Episodes")+
  #coord_fixed(ratio = 2/1) +

  theme_bw() +
  scale_pattern_discrete(name = "Procedures",labels=c("Generic","X15"),choices=c("circle","stripe"),guide="legend")+
  scale_fill_manual(name="Gender Marker",values=c("#FC8D62","#E78AC3","#66C2A5","#8DA0CB")) +
  theme(legend.key.size = unit(1.5, 'cm'))+
  theme(axis.text= element_text(size=14),
        axis.title= element_text(size=14),
        strip.text = element_text(size=14),
        legend.text = element_text(size=14),
        legend.title  = element_text(size=14))


data <- data.frame(
  METRIC = paste('var', 1:10),
  VAL = runif(10),
  stringsAsFactors = FALSE
)

tab_number <- 1:nrow(data)
tab_color <- ifelse(data$VAL > 0.75, 'green', ifelse(data$VAL > 0.25, 'yellow', 'red'))
css <- sprintf(".color-tabs>.nav-pills>li:nth-child(%d){background:%s;}", tab_number, tab_color)
css <- paste(css, collapse = "\n")
css <- paste("<style>", css, "</style>")
cat(css)
