### GFR tabel; cyclus 11
### V0.1 16-11-21
### Clear
rm(list=ls())

library(readxl)
library(dplyr)
library(tidyr)
library(XLConnect)
library(sqldf)
library(stringr)
library(foreign)
library(lubridate)


startwindow <- as.Date("2021-01-01")
stopwindow_GFR <- as.Date("2021-12-31")
stopwindow_DBC<- as.Date("2022-11-01")


dir_patselectie = "K:/SHAREDIR/Data Santeon projecten/scorekaarten/nierfalen/Cyclus 11/Bron"
GFR = "gfr.csv"
Verrichting = "verrichting_u8.csv"
Opnames = "Opnames.csv"
Subtraject = "MZR000334_Cyclus 11_Subtrajecten.csv"


# inladen Excel data 
setwd(dir_patselectie) ## hier het pad waar je patientenselectie staat
GFR<- read.csv(GFR, header=TRUE, sep=";",stringsAsFactors=FALSE) 
Verrichting<- read.csv(Verrichting, header=TRUE, sep=";",stringsAsFactors=FALSE) 
Opnames<- read.csv(Opnames, header=TRUE, sep=";",stringsAsFactors=FALSE) 
Subtraject<- read.csv(Subtraject, header=FALSE, sep=";",stringsAsFactors=FALSE) 

Subtraject <- Subtraject %>%
  transmute( PatientNr = as.character(V1),
             OpeningsDatumDBC    = as.Date(Openingsdatumdbc, format = "%Y-%m-%d"),
             SluitingsDatumDBC   = as.Date(Sluitingsdatumdbc, format = "%Y-%m-%d"),
             StatusDBC           = ifelse(Statusdbc=="Leeg",1,0),                     
             ZorgType            = ifelse(Zorgtype=="R", 11, V3),
             ZorgType            = ifelse(Zorgtype=="L", 21, ZorgType),
             SpecialismeCode     = as.character(Specialisme),
             DiagnoseCode        = as.character(Diagnosecode))


GFR <- as.data.frame(GFR)%>%
  filter(BepalingOms=="GFR (CKD-EPI)" | BepalingOms=="GFR-EPI")%>%  # kijk hoe GFR waardes in eigen Lab staan omschreven, dit ivm lokale codes voor GFR labwaardes
  transmute(PatientNr=as.character(PatientNr),
            Datum = as.Date(AfnameDatumTijd, format="%Y-%m-%d"),
            Uitslag= ifelse(Uitslag==">90",90,Uitslag),
            Uitslag=as.numeric(Uitslag))%>%
  filter(!is.na(Uitslag))

Verrichting <- as.data.frame(Verrichting)%>%
  transmute(PatientNr=as.character(PatientNr),
            VerrichtingDatum = as.Date(Verrichtingdatum, format="%d-%m-%Y"),
            ZACode=as.character(ZACode),
            Specialisme=as.character(Specialisme), #"INT", "CAR" etc
            Aantal=as.numeric(Aantal))


Opnames <- as.data.frame(Opnames)%>%
  transmute(PatientNr=as.character(PatientNr),
            OpnameDatum = as.Date(OpnameDatumTijd, format="%Y-%m-%d"),
            OntslagDatum = as.Date(OntslagDatumTijd, format="%Y-%m-%d"))





# Excludeer GFR waardes afgenomen tijdens SEH of klinische opname
Verrichting <- Verrichting%>%
  filter(ZACode=="190021" | ZACode=="190015" | ZACode=="190016")

# koppel opnames aan verrichtingen (in MSZ worden de opnamenummers niet goed geregistreerd bij Verrichtingen)
temp <-  left_join(Verrichting, Opnames)%>%
  filter(ZACode=="190021" & VerrichtingDatum>=OpnameDatum-1 & VerrichtingDatum<=OntslagDatum)%>%
  arrange(PatientNr, OpnameDatum) %>% 
  distinct(PatientNr, OpnameDatum,.keep_all=TRUE)

Verrichting1 <- left_join(Verrichting, temp, by=c("PatientNr", "VerrichtingDatum", "ZACode"))




# VERDIEPING 8-3-22 PAK GEMIDDELDE GFR IPV LAAGSTE GFR

GFR_gem <- left_join(GFR, Verrichting1, by="PatientNr")%>%
  mutate(excludeer=ifelse(Datum==VerrichtingDatum | (Datum>=OpnameDatum & Datum<=OntslagDatum),1,0))%>%
  arrange(PatientNr, Datum, desc(excludeer))%>%  
  group_by(PatientNr, Datum)%>%
  summarise(Uitslag=min(Uitslag), excludeer=first(excludeer))%>%   # indien op een bepaalde datum inimaal 1x gelijk aan SEh datum of klinische opname, dan staat de 1 bovenaan --> excluderen
  filter(excludeer==0 | is.na(excludeer))%>%
  select(-excludeer)

# selecteer  de gemiddelde GFR waarde per patiënt binnen de selectie periode 
GFR_gem <- GFR_gem %>%
  filter(Datum>=startwindow & Datum<=stopwindow_GFR)%>%
  group_by(PatientNr)%>%
  mutate(min_dat_GFR=min(Datum),
         max_dat_GFR=max(Datum),
         interval=as.numeric(difftime(max_dat_GFR, min_dat_GFR, units="days")))%>%
  filter(interval >= 30) %>%
  summarise(Uitslag=mean(Uitslag))%>%
  mutate(GFR_cat = cut(Uitslag, seq(1,61,10), right=FALSE, labels=c(1:6)))



# van alle patiënten die nu in de dataset GFR1 zitten (jan20 t/m dec21) willen we alle DBC's in jan 2020 tot nov 2022 weten
# merge tabbladen 
Subtraject1 <- Subtraject%>%
  filter(StatusDBC==0)%>%
  filter((is.na(SluitingsDatumDBC)|OpeningsDatumDBC<=stopwindow_DBC) & SluitingsDatumDBC>=startwindow)


GFR_tabel_gem<-left_join(GFR_gem, Subtraject1, by = "PatientNr")

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
GFR_tabel_gem<- as.data.frame(GFR_tabel_gem)%>%
  mutate(DBC_groep=NA)%>%
  mutate(DBC_groep = ifelse(!is.na(match(SpecialismeCode,"INT")),2, NA))%>%
  mutate(DBC_groep = ifelse(DBC_groep==2 & !is.na(match(DiagnoseCode, DBC_interne)),1, DBC_groep))%>%
  mutate (DBC_groep = ifelse(is.na(DBC_groep),3, DBC_groep))%>%
  group_by(PatientNr)%>%
  summarise(DBC_final=min(DBC_groep), GFR_cat=first(GFR_cat), DiagnoseCode=max(DiagnoseCode))  %>%  # LG diagnosecode toegevoegd om te kijken of patiënten minimaal één dbc hebben
  filter(!is.na(DiagnoseCode) & DiagnoseCode!="0" & DiagnoseCode!="00" & DiagnoseCode!="000" & DiagnoseCode!="0000")  #LG nieuw in C11; excludeer patiënten zonder enige dbc


GFR_tabel_gem<- GFR_tabel_gem%>%
  filter (!is.na(GFR_cat)&!is.na(DBC_final))%>%
  mutate(GFR_cat =as.numeric(GFR_cat))%>%
  filter(GFR_cat<=3)

#Uitdraai GFR tabel (verticale 1,2,3 weergeven de GFR categorie, horizontale ,2,3 weergeeft nefroloog-interne-overige specialismes)
table(GFR_tabel_gem$GFR_cat,GFR_tabel_gem$DBC_final)

  