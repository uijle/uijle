### GFR tabel; cyclus 9
### V0.1 16-11-21
### Clear
rm(list=ls())

# 21 dec 2021
# GFR categorieen (r123): aangepast sequence 1-61 ipv 0-60, anders wordt GFR60 niet meegenomen:   mutate(GFR_cat = cut(Uitslag, seq(1,61,10), right=FALSE, labels=c(1:6)))
# toevoeging stukje einddatum dbc invullen (r124-128)
# (r138)  ipv is.na(stopwindow_DBC) moet er staan is.na(SluitingsDatumDBC)
# (r141)  Subtraject moet Subtraject1 zijn 

# 24 dec 2021
  # statusdbc ==0  filteren ipv statusdbc ==1 (regel 137)

# 21 januari 2021
# (r152) Diagnosecode is numeriek, dus "076" en "078" moet zijn "76" en "78".

#detach("package:xlsx", unload=TRUE)
#detach("package:readxl", unload=TRUE)
#install.packages("dplyr")
#install.packages("xlsx")
#install.package("plotly")
#install.packages("readxl")
#install.packages("devtools")
#devtools::install_github("kas43/2
#install.packages("XLConnect")
#install.packages("RSQLite")
#install.packages("sqldf")
#install.packages("foreign")
#install.packages("lubridate")


library(readxl)
library(dplyr)
library(tidyr)
library(XLConnect)
library(sqldf)
library(stringr)
library(foreign)
library(lubridate)

# inladen tabbladen GFR patiëntselectie (patiëntselectie 2)

setwd ("K:/SHAREDIR/Data Santeon projecten/scorekaarten/nierfalen/Cyclus 9")
gfr_info <- lapply(excel_sheets("MZR000334_Nierfalen cyclus 9_GFR_2021-11-12.xlsx"), read_excel, path = "MZR000334_Nierfalen cyclus 9_GFR_2021-11-12.xlsx")

Lab = gfr_info[[5]]
Subtraject = gfr_info[[3]]
Verrichtingen = gfr_info[[4]]
Opnames = gfr_info[[6]]

Lab <- as.data.frame(Lab)%>%
  mutate(PatientNr=as.character(PatientNr),
         Datum = as.Date(AfnameDatumTijd, format="%Y-%m-%d"),
         Uitslag=as.numeric(Uitslag),
         BepalingOms=as.character(BepalingOms))%>%
  select(PatientNr, Datum, Uitslag, BepalingOms)


Verrichtingen <- as.data.frame(Verrichtingen)%>%
  mutate(PatientNr=as.character(PatientNr),
         VerrichtingDatum = as.Date(Verrichtingdatum, format="%Y-%m-%d"),
         ZACode=as.character(ZACode),
         OpnameNr=as.numeric(OpnameNr))%>%
  select(PatientNr, VerrichtingDatum, ZACode)

Opnames <- as.data.frame(Opnames)%>%
  mutate(PatientNr=as.character(PatientNr),
         OpnameDatum = as.Date(OpnameDatumTijd, format="%Y-%m-%d"),
         OntslagDatum = as.Date(OntslagDatumTijd, format="%Y-%m-%d"))%>%
  select(PatientNr, OpnameDatum, OntslagDatum)

Subtraject <-as.data.frame(Subtraject)%>%
  mutate( PatientNr = as.character(PatientNr),
          OpeningsDatumDBC    = as.Date(OpeningsDatumDBC, format = "%Y-%m-%d"),
          SluitingsDatumDBC   = as.numeric(SluitingsDatumDBC),                                 # kijk hoe je in eigen zh de datum goed krijgt
          SluitingsDatumDBC   = as.Date(SluitingsDatumDBC, origin = "1970-01-01"),             # kijk hoe je in eigen zh de datum goed krijgt
          StatusDBC           = ifelse(StatusDBC=="Nee",1,0),                     # leeg=1, gevuld=0
          ZorgType            = ifelse(ZorgType=="R", 11, ZorgType),
          ZorgType            = ifelse(ZorgType=="L", 21, ZorgType),
         DiagnoseCode=as.numeric(DiagnoseCode),
         SpecialismeCode=as.character(SpecialismeCode))%>%  # staat in format "INT", "CHI" etc. 
  select(PatientNr, OpeningsDatumDBC, SluitingsDatumDBC, DiagnoseCode, SpecialismeCode, StatusDBC, ZorgType)

############
############

startwindow <- as.Date("2019-01-01")
stopwindow_GFR <- as.Date("2020-12-31")
stopwindow_DBC<- as.Date("2021-11-01")

# selecteer GFR waardes <=60
GFR <- Lab %>%
  filter(BepalingOms=="GFR (CKD-EPI)" | BepalingOms=="GFR-EPI")%>%  # kijk hoe GFR waardes in eigen Lab staan omschreven, dit ivm lokale codes voor GFR labwaardes
  filter(Uitslag<=60)%>%
  select(-BepalingOms)


# Excludeer GFR waardes afgenomen tijdens SEH of klinische opname
Verrichtingen <- Verrichtingen%>%
  filter(ZACode=="190021" | ZACode=="190015" | ZACode=="190016")

  # koppel opnames aan verrichtingen (in MSZ worden de opnamenummers niet goed geregistreerd bij Verrichtingen)
temp <-  left_join(Verrichtingen, Opnames)%>%
  filter(ZACode=="190021" & VerrichtingDatum>=OpnameDatum-1 & VerrichtingDatum<=OntslagDatum)%>%
  arrange(PatientNr, OpnameDatum) %>% 
  distinct(PatientNr, OpnameDatum,.keep_all=TRUE)
  
Verrichtingen1 <- left_join(Verrichtingen, temp, by=c("PatientNr", "VerrichtingDatum", "ZACode"))

  # koppel aan GFR waardes
GFR1 <- left_join(GFR, Verrichtingen1, by="PatientNr")%>%
  mutate(excludeer=ifelse(Datum==VerrichtingDatum | (Datum>=OpnameDatum & Datum<=OntslagDatum),1,0))%>%
  arrange(PatientNr, Datum, desc(excludeer))%>%  
  group_by(PatientNr, Datum)%>%
  summarise(Uitslag=min(Uitslag), excludeer=first(excludeer)) %>%  # indien op een bepaalde datum inimaal 1x gelijk aan SEh datum of klinische opname, dan staat de 1 bovenaan --> excluderen
  filter(excludeer==0 | is.na(excludeer))%>%
  select(-excludeer)


# selecteer  de laagste gemeten GFR waarde per patiënt binnen de selectie periode 
GFR1 <- GFR1 %>%
  filter(Datum>=startwindow & Datum<=stopwindow_GFR)%>%
  group_by(PatientNr)%>%
  summarise(Uitslag=min(Uitslag))%>%
  mutate(GFR_cat = cut(Uitslag, seq(1,61,10), right=FALSE, labels=c(1:6)))
  

# Zet bij missende einddatum de DBC op 90 (nieuwe DBC) of 120 dagen (vervolg DBC) vanaf de startdatum
Subtraject <- Subtraject %>% 
  mutate(SluitingsDatumDBC = ifelse(is.na(SluitingsDatumDBC) & ZorgType=="11",OpeningsDatumDBC + 89,SluitingsDatumDBC)) %>%
  mutate(SluitingsDatumDBC = ifelse(is.na(SluitingsDatumDBC) & ZorgType=="21",OpeningsDatumDBC + 119,SluitingsDatumDBC)) %>%
  mutate(SluitingsDatumDBC = as.Date(SluitingsDatumDBC,origin="1970-01-01"))



# van alle patiënten die nu in de dataset GFR1 zitten (jan19 t/m dec20) willen we alle DBC's in jan 2019 tot nov 2021 weten
  # merge tabbladen 
Subtraject1 <- Subtraject%>%
  filter(StatusDBC==0)%>%
  filter((is.na(SluitingsDatumDBC)|OpeningsDatumDBC<=stopwindow_DBC) & SluitingsDatumDBC>=startwindow)


GFR_tabel<-left_join(GFR1, Subtraject1, by = "PatientNr")

# indeling specialismen
  # als patient DBCcode nefro heeft --> groep1
  # als patiënt geen DBCcode nefro heeft, maar wel andere interne --> groep 2
  # als patiënt geeen DBCcode nefro of andere interne heeft --> groep 3

# alle nefrologie codes onder 'code' benoemen (eenvoudiger coderen stap erna)
DBC_interne<-c("324","325", "301", "303", "304", "311", "313", "322", "323", "331", "332", "336", "339", "399", "76", "78")


# creeëren groepen, houdt er rekening mee dat bijv. DBC 325 ook bij ander specialisme gebruikt kan worden, 
# dus bij nefrologie specifiek als inclusiecriteria: DBC codes & specialisme interne
# eerst groep 2, want deze programmering schrijft zichzelf over, dus veranderd 2 nu in 1 indien nefro DBC's
# group_by patientnummer & summarise op min; dus als DBC_group 1 en 2 --> behoud 1 (nefro), 
# voor behoud gegevens GFR_cat, moet deze er ook bij gezet worden (eerste waarde), 
GFR_tabel<- as.data.frame(GFR_tabel)%>%
  mutate(DBC_groep=NA)%>%
  mutate(DBC_groep = ifelse(!is.na(match(SpecialismeCode,"INT")),2, NA))%>%
  mutate(DBC_groep = ifelse(DBC_groep==2 & !is.na(match(DiagnoseCode, DBC_interne)),1, DBC_groep))%>%
  mutate (DBC_groep = ifelse(is.na(DBC_groep),3, DBC_groep))%>%
  group_by(PatientNr)%>%
  summarise(DBC_final=min(DBC_groep), GFR_cat=first(GFR_cat))

GFR_tabel<- GFR_tabel%>%
  filter (!is.na(GFR_cat)&!is.na(DBC_final))


#Uitdraai GFR tabel
table(GFR_tabel$DBC_final,GFR_tabel$GFR_cat)
  

# vrijblijvende verdieping: check GFR<30 die niet bij het specialisme interne onder behandeling zijn 
# 
# verdieping_gfr_tabel <- GFR_tabel%>%
#   mutate(GFR_cat=as.numeric(GFR_cat))%>%
#   filter(GFR_cat<=3 & DBC_final==2)
  
  