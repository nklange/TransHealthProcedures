# Extract relevant information


library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(readr)
library(tibble)
library(ggplot2)


footnote <- function(codes){

  c(codes,paste0("‡ ",codes),paste0("‡",codes))
}

#OSPC4
transproc <- c("X15.1","X15.2","X15.4","X15.8","X15.9")
procedurecodes <- footnote(transproc)

mastectomyproc <-c("B27.5","B27.6")
phalloplastyproc <- "N28.1"
hysterproc <- c("Q07.1","Q07.2","Q07.3","Q07.4","Q07.5","Q07.8","Q07.9",
               "Q08.1","Q08.2","Q08.3","Q08.8","Q08.9")
              #  "Q22.1","Q22.2","Q22.3",
               # "Q24.1","Q24.2","Q24.3")
orchidectomyproc <- c("N05.1","N05.2","N05.3","N05.8","N06.3")
penectomyproc <-  c("N26.1","N26.2","N26.8","N26.9")
vagplasty <- c("P21.2","P21.3","P21.5")
oocyte <- c("Q48.1","Q48.2","Q48.3","Q48.4","Q48.8","Q48.9")

sperm <- c("N34.1","N34.2","N34.4","N34.5","N34.6","N34.8")





altcodes <- footnote(c(mastectomyproc,phalloplastyproc,hysterproc,orchidectomyproc,penectomyproc,
                       vagplasty,oocyte,sperm))



#ICD10
transdiag <- c("F64.0","F64.1","F64.2","F64.8","F64.9")

#2020 - 2021 -------------------------------------------------------------------


hosp <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-proc-2020-21-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

header_prov <- hosp[8,]
header_rfriendly <- c("Code","Description",	"T1",	"T2",	"T3",	"T4","T5",
                      "FinEpisodes","Admissions","GenderMale","GenderFemale","GenderUnknown",
                      "Emergency","WaitingList","Planned","OtherAdmission",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay","MeanAge","Age0",	"Age1to4","Age5to9","Age10to14",
                      "Age15","Age16","Age17","Age18","Age19","Age20to24",
                      "Age25to29","Age30to34",	"Age35to39","Age40to44","Age45to49","Age50to54",
                      "Age55to59","Age60to64","Age65to69","Age70to74","Aget75to79",
                      "Age80to84","Age85to89","Age90plus","DayCase","FCEBedDays",
                      "ZeroBedEmergency","ZeroBedElective","ZeroBedOther")

extracthosp <- hosp %>% filter(codes %in% procedurecodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2020to2021",
                                                         type="Procedures")

extracthospAlt <- hosp %>% filter(codes %in% altcodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2020to2021",
                                                         type="AltProcedures")

diag <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-diag-2020-21-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

extractdiag <- diag %>% filter(codes %in% transdiag) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2020to2021",
                                                         type="Diagnoses")

year2020to2021 <- bind_rows(extractdiag,extracthosp,extracthospAlt)

#2019 - 2020 -------------------------------------------------------------------


hosp <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-proc-2019-20-tab.xlsx",
                  sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

header_prov <- hosp[8,]
header_rfriendly <- c("Code","Description",	"T1",	"T2",	"T3",	"T4","T5",
                      "FinEpisodes","Admissions","GenderMale","GenderFemale","GenderUnknown",
                      "Emergency","WaitingList","Planned","OtherAdmission",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay","MeanAge","Age0",	"Age1to4","Age5to9","Age10to14",
                      "Age15","Age16","Age17","Age18","Age19","Age20to24",
                      "Age25to29","Age30to34",	"Age35to39","Age40to44","Age45to49","Age50to54",
                      "Age55to59","Age60to64","Age65to69","Age70to74","Aget75to79",
                      "Age80to84","Age85to89","Age90plus","DayCase","FCEBedDays",
                      "ZeroBedEmergency","ZeroBedElective","ZeroBedOther")

extracthosp <- hosp %>% filter(codes %in% procedurecodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%

  set_names(header_rfriendly) %>%
 dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2019to2020",
                                                        type="Procedures")

extracthospAlt <- hosp %>% filter(codes %in% altcodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%

  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2019to2020",
                                                         type="AltProcedures")


diag <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-diag-2019-20-tab supp.xlsx",
                                  sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

extractdiag <- diag %>% filter(codes %in% transdiag) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2019to2020",
                                                         type="Diagnoses")

year2019to2020 <- bind_rows(extractdiag,extracthosp,extracthospAlt)


#2018 - 2019 -------------------------------------------------------------------


hosp <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-proc-2018-19-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

header_prov <- hosp[8,]
header_rfriendly <- c("Code","Description",	"T1",	"T2",	"T3",	"T4","T5",
                      "FinEpisodes","Admissions","GenderMale","GenderFemale","GenderUnknown",
                      "Emergency","WaitingList","Planned","OtherAdmission",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay","MeanAge","Age0",	"Age1to4","Age5to9","Age10to14",
                      "Age15","Age16","Age17","Age18","Age19","Age20to24",
                      "Age25to29","Age30to34",	"Age35to39","Age40to44","Age45to49","Age50to54",
                      "Age55to59","Age60to64","Age65to69","Age70to74","Aget75to79",
                      "Age80to84","Age85to89","Age90plus","DayCase","FCEBedDays",
                      "ZeroBedEmergency","ZeroBedElective","ZeroBedOther")

extracthosp <- hosp %>% filter(codes %in% procedurecodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2018to2019",
                                                         type="Procedures")

extracthospAlt <- hosp %>% filter(codes %in% altcodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2018to2019",
                                                         type="AltProcedures")


diag <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-diag-2018-19-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

extractdiag <- diag %>% filter(codes %in% transdiag) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2018to2019",
                                                         type="Diagnoses")

year2018to2019 <- bind_rows(extractdiag,extracthosp,extracthospAlt)

#2017 - 2018 -------------------------------------------------------------------


hosp <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-proc-2017-18-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

header_prov <- hosp[8,]
header_rfriendly <- c("Code","Description",	"T1",	"T2",	"T3",	"T4","T5",
                      "FinEpisodes","Admissions","GenderMale","GenderFemale","GenderUnknown",
                      "Emergency","WaitingList","Planned","OtherAdmission",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay","MeanAge","Age0",	"Age1to4","Age5to9","Age10to14",
                      "Age15","Age16","Age17","Age18","Age19","Age20to24",
                      "Age25to29","Age30to34",	"Age35to39","Age40to44","Age45to49","Age50to54",
                      "Age55to59","Age60to64","Age65to69","Age70to74","Aget75to79",
                      "Age80to84","Age85to89","Age90plus","DayCase","FCEBedDays",
                      "ZeroBedEmergency","ZeroBedElective","ZeroBedOther")

extracthosp <- hosp %>% filter(codes %in% procedurecodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2017to2018",
                                                         type="Procedures")

extracthospAlt <- hosp %>% filter(codes %in% altcodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2017to2018",
                                                         type="AltProcedures")


diag <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-diag-2017-18-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

extractdiag <- diag %>% filter(codes %in% transdiag) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2017to2018",
                                                         type="Diagnoses")

year2017to2018 <- bind_rows(extractdiag,extracthosp,extracthospAlt)

#2016 - 2017 -------------------------------------------------------------------


hosp <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-proc-2016-17-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

header_prov <- hosp[8,]
header_rfriendly <- c("Code","Description",	"T1",	"T2",	"T3",	"T4","T5",
                      "FinEpisodes","Admissions","GenderMale","GenderFemale","GenderUnknown",
                      "Emergency","WaitingList","Planned","OtherAdmission",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay","MeanAge","Age0",	"Age1to4","Age5to9","Age10to14",
                      "Age15","Age16","Age17","Age18","Age19","Age20to24",
                      "Age25to29","Age30to34",	"Age35to39","Age40to44","Age45to49","Age50to54",
                      "Age55to59","Age60to64","Age65to69","Age70to74","Aget75to79",
                      "Age80to84","Age85to89","Age90plus","DayCase","FCEBedDays",
                      "ZeroBedEmergency","ZeroBedElective","ZeroBedOther")

extracthosp <- hosp %>% filter(codes %in% procedurecodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2016to2017",
                                                         type="Procedures")

extracthospAlt <- hosp %>% filter(codes %in% altcodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2016to2017",
                                                         type="AltProcedures")


diag <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-diag-2016-17-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

extractdiag <- diag %>% filter(codes %in% transdiag) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2016to2017",
                                                         type="Diagnoses")

year2016to2017 <- bind_rows(extractdiag,extracthosp,extracthospAlt) %>%
  mutate_all(funs(stringr::str_replace(., "-", "0")))

#2015 - 2016 -------------------------------------------------------------------


hosp <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-proc-2015-16-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

header_prov <- hosp[8,]
header_rfriendly <- c("Code","Description",	"T1",	"T2",	"T3",	"T4","T5",
                      "FinEpisodes","Admissions","GenderMale","GenderFemale","GenderUnknown",
                      "Emergency","WaitingList","Planned","OtherAdmission",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay","MeanAge","Age0",	"Age1to4","Age5to9","Age10to14",
                      "Age15","Age16","Age17","Age18","Age19","Age20to24",
                      "Age25to29","Age30to34",	"Age35to39","Age40to44","Age45to49","Age50to54",
                      "Age55to59","Age60to64","Age65to69","Age70to74","Aget75to79",
                      "Age80to84","Age85to89","Age90plus","DayCase","FCEBedDays",
                      "ZeroBedEmergency","ZeroBedElective","ZeroBedOther")

extracthosp <- hosp %>% filter(codes %in% procedurecodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2015to2016",
                                                         type="Procedures")

extracthospAlt <- hosp %>% filter(codes %in% altcodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2015to2016",
                                                         type="AltProcedures")


diag <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-diag-2015-16-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

extractdiag <- diag %>% filter(codes %in% transdiag) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2015to2016",
                                                         type="Diagnoses")

year2015to2016 <- bind_rows(extractdiag,extracthosp,extracthospAlt)

#2014 - 2015 -------------------------------------------------------------------


hosp <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-proc-2014-15-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

header_prov <- hosp[8,]
header_rfriendly <- c("Code","Description",	"T1",	"T2",	"T3",	"T4","T5",
                      "FinEpisodes","Admissions","GenderMale","GenderFemale","GenderUnknown",
                      "Emergency","WaitingList","Planned","OtherAdmission",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay","MeanAge","Age0",	"Age1to4","Age5to9","Age10to14",
                      "Age15","Age16","Age17","Age18","Age19","Age20to24",
                      "Age25to29","Age30to34",	"Age35to39","Age40to44","Age45to49","Age50to54",
                      "Age55to59","Age60to64","Age65to69","Age70to74","Aget75to79",
                      "Age80to84","Age85to89","Age90plus","DayCase","FCEBedDays",
                      "ZeroBedEmergency","ZeroBedElective","ZeroBedOther")

extracthosp <- hosp %>% filter(codes %in% procedurecodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2014to2015",
                                                         type="Procedures")
extracthospAlt <- hosp %>% filter(codes %in% altcodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2014to2015",
                                                         type="AltProcedures")

diag <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-diag-2014-15-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

extractdiag <- diag %>% filter(codes %in% transdiag) %>%
  set_names(header_rfriendly) %>%
  dplyr::select(-c("T1","T2","T3","T4","T5")) %>% mutate(year="2014to2015",
                                                         type="Diagnoses")

year2014to2015 <- bind_rows(extractdiag,extracthosp,extracthospAlt)

#2013 - 2014 -------------------------------------------------------------------


hosp <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-proc-2013-14-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

header_prov <- hosp[8,]
header_rfriendly <- c("Code","Description",	#"T1",	"T2",	"T3",	"T4","T5",
                      "FinEpisodes","Admissions","GenderMale","GenderFemale","GenderUnknown",
                      "Emergency","WaitingList","Planned","OtherAdmission",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay","MeanAge","Age0",	"Age1to4","Age5to9","Age10to14",
                      "Age15","Age16","Age17","Age18","Age19","Age20to24",
                      "Age25to29","Age30to34",	"Age35to39","Age40to44","Age45to49","Age50to54",
                      "Age55to59","Age60to64","Age65to69","Age70to74","Aget75to79",
                      "Age80to84","Age85to89","Age90plus","DayCase","FCEBedDays",
                      "ZeroBedEmergency","ZeroBedElective","ZeroBedOther")

extracthosp <- hosp %>% filter(codes %in% procedurecodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  #dplyr::select(-c("T1","T2","T3","T4","T5")) %>%
  mutate(year="2013to2014",
         type="Procedures")

extracthospAlt <- hosp %>% filter(codes %in% altcodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  #dplyr::select(-c("T1","T2","T3","T4","T5")) %>%
  mutate(year="2013to2014",
         type="AltProcedures")



diag <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-diag-2013-14-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

extractdiag <- diag %>% filter(codes %in% transdiag) %>%
  set_names(header_rfriendly) %>%
  #dplyr::select(-c("T1","T2","T3","T4","T5")) %>%
  mutate(year="2013to2014",
         type="Diagnoses")

year2013to2014 <- bind_rows(extractdiag,extracthosp,extracthospAlt)

#2012 - 2013 -------------------------------------------------------------------


hosp <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-proc-2012-13-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

header_prov <- hosp[8,]
header_rfriendly <- c("Code","Description",	#"T1",	"T2",	"T3",	"T4","T5",
                      "FinEpisodes","Admissions","GenderMale","GenderFemale","GenderUnknown",
                      "Emergency","WaitingList","Planned","OtherAdmission",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay","MeanAge","Age0",	"Age1to4","Age5to9","Age10to14",
                      "Age15","Age16","Age17","Age18","Age19","Age20to24",
                      "Age25to29","Age30to34",	"Age35to39","Age40to44","Age45to49","Age50to54",
                      "Age55to59","Age60to64","Age65to69","Age70to74","Aget75to79",
                      "Age80to84","Age85to89","Age90plus","DayCase","FCEBedDays")#,
                      #"ZeroBedEmergency","ZeroBedElective","ZeroBedOther")

extracthosp <- hosp %>% filter(codes %in% procedurecodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  #dplyr::select(-c("T1","T2","T3","T4","T5")) %>%
  mutate(year="2012to2013",
         type="Procedures")

extracthospAlt <- hosp %>% filter(codes %in% altcodes) %>%
  mutate(codes = stringr::str_remove_all(codes,"[‡ ]")) %>%
  set_names(header_rfriendly) %>%
  #dplyr::select(-c("T1","T2","T3","T4","T5")) %>%
  mutate(year="2012to2013",
         type="AltProcedures")


diag <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-diag-2012-13-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

extractdiag <- diag %>% filter(codes %in% transdiag) %>%
  set_names(c(header_rfriendly,"out")) %>%
  dplyr::select(-c("out")) %>%
  mutate(year="2012to2013",
         type="Diagnoses")

year2012to2013 <- bind_rows(extractdiag,extracthosp,extracthospAlt)

#2011 - 2012 --------------------------------------------

hosp <- read.csv("NHSDocuments/hes_apc_national_procedure_2011_12.csv") %>%
  filter(DimensionCode %in% procedurecodes) %>%
  pivot_wider(id_cols = c(DimensionCode,DimensionDescription),names_from=c("MeasureCategory","MeasureSubCategory"),values_from="MeasureValue")


header_rfriendly <- c("Code","Description",
                      "FinEpisodes","Admissions","GenderMale",
                      "Emergency","WaitingList",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay",
                      "MeanAge","Age0to14",
                      "Age15to59","Age60to74","Age75plus","DayCase","FCEBedDays")
header_rfriendly2 <- c("CodeDescription",
                      "FinEpisodes","Admissions","GenderMale",
                      "Emergency","WaitingList",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay",
                      "MeanAge","Age0to14",
                      "Age15to59","Age60to74","Age75plus","DayCase","FCEBedDays")
extracthosp <- hosp %>% set_names(header_rfriendly) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(codes = stringr::str_remove_all(Code,"[‡ ]")) %>%
  mutate(year="2011to2012",
         type="Procedures")


hospAlt <- read.csv("NHSDocuments/hes_apc_national_procedure_2011_12.csv") %>%
  filter(DimensionCode %in% altcodes) %>%
  pivot_wider(id_cols = c(DimensionCode,DimensionDescription),names_from=c("MeasureCategory","MeasureSubCategory"),values_from="MeasureValue")

extracthospAlt <- hospAlt %>% set_names(header_rfriendly) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(codes = stringr::str_remove_all(Code,"[‡ ]")) %>%
  mutate(year="2011to2012",
         type="AltProcedures")



diag <- tibble(readxl::read_xls("NHSDocuments/hosp-epis-stat-admi-prim-diag-4cha-11-12-tab.xls",
                                 sheet = 1)) %>% set_colnames(c("codes",c(1:49)))

extractdiag <- diag %>% filter(grepl(paste(transdiag, collapse = "|"),codes)) %>%
  set_names(c(header_rfriendly2)) %>%
  mutate(Code = substr(CodeDescription, 1, 5)   ) %>%
  mutate(Description = substr(CodeDescription, 7, nchar(CodeDescription)) ) %>%

  dplyr::select(-c("CodeDescription")) %>%
  mutate(year="2011to2012",
         type="Diagnoses")

year2011to2012 <- bind_rows(extractdiag,extracthosp,extracthospAlt)



# Collect all -----------------------------------------------------------------

CollectedData <- bind_rows(year2011to2012,year2012to2013,year2013to2014,year2014to2015,year2015to2016,year2016to2017,
          year2017to2018,year2018to2019,year2019to2020,year2020to2021) %>%
  relocate(year,type)

saveRDS(CollectedData,file="ProcessedData/ProceduresDiag.rds")

###############################################################################
# Code for further analysis and graphs is in the R-markdown document
# 'InitialReport_publicNHSDigital.Rmd'


# Extract Age information - is it only redacted for X15?
# Answer no: but not redacted for that many procedures

hosp <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-proc-2020-21-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

header_prov <- hosp[8,]
header_rfriendly <- c("Code","Description",	"T1",	"T2",	"T3",	"T4","T5",
                      "FinEpisodes","Admissions","GenderMale","GenderFemale","GenderUnknown",
                      "Emergency","WaitingList","Planned","OtherAdmission",	"MeanWaiting","MedianWaiting",
                      "MeanStay","MedianStay","MeanAge","Age0",	"Age1to4","Age5to9","Age10to14",
                      "Age15","Age16","Age17","Age18","Age19","Age20to24",
                      "Age25to29","Age30to34",	"Age35to39","Age40to44","Age45to49","Age50to54",
                      "Age55to59","Age60to64","Age65to69","Age70to74","Aget75to79",
                      "Age80to84","Age85to89","Age90plus","DayCase","FCEBedDays",
                      "ZeroBedEmergency","ZeroBedElective","ZeroBedOther")

EmptyAge <- hosp[c(13:c(dim(hosp)[1] -13)),] %>%
  set_names(header_rfriendly) %>%
  select(Code,Description,FinEpisodes,MeanAge) %>%
  filter(is.na(MeanAge))


# Extract Age diagnosis:
# Redaction?


diag <- tibble(readxl::read_xlsx("NHSDocuments/hosp-epis-stat-admi-diag-2020-21-tab.xlsx",
                                 sheet = 4)) %>% set_colnames(c("codes",c(1:49)))

EmptyAge <- diag[c(13:c(dim(diag)[1] -13)),] %>%
  set_names(header_rfriendly)  %>%
  select(Code,Description,FinEpisodes,MeanAge) %>%
  filter(is.na(MeanAge))
