### GFR tabel; cyclus 11
### V0.1 16-11-21
### Clear
rm(list=ls())

library(readxl)
library(dplyr)
library(tidyr)
#library(XLConnect)
library(sqldf)
library(stringr)
library(foreign)
library(lubridate)

# inladen tabbladen GFR pati?ntselectie (pati?ntselectie 2)

#setwd("/home/afdelingen/kwaliteit.en.veiligheid/Nier/Cyclus11/Data")
setwd("/home/afdelingen/kwaliteit.en.veiligheid/Nier/Cyclus9/")

load("Lab_gfr.RData")
load("Subtraject_gfr.RData")
load("Verrichtingen_gfr.RData")
load("Opnames_gfr.RData")

Lab <- Lab_gfr

Verrichting <- Verrichtingen_gfr

Opnames <- Opnames_gfr

Subtraject <-Subtraject_gfr


############
############

#startwindow <- as.Date("2021-01-01")
#stopwindow_GFR <- as.Date("2021-12-31")
#stopwindow_DBC<- as.Date("2022-11-01")

startwindow <- as.Date("2019-01-01")
stopwindow_GFR <- as.Date("2020-12-31")
stopwindow_DBC<- as.Date("2021-11-01")

#eGFR gegevens voor verdieping GFRtabel
#GFR_raw <- read.csv("EGFR uitslagen ronde 11.csv", header=T, sep=";", dec=",", stringsAsFactors = F, na.strings="", quote="", fileEncoding = "latin1")

#GFR <- GFR_raw %>%
#  mutate(PatientNr=as.character(PAT_CODE),
#         Datum = as.Date(aflever_date, format="%d-%m-%Y"),
#         Uitslag = as.numeric(lab_uitslag),
#         BepalingOms=as.character(OMSCHRIJVING),
#         Som_aantal_per_patient = as.numeric(Som_aantal_per_patient)) %>%
#  select(PatientNr, Datum, Uitslag, BepalingOms, Som_aantal_per_patient) %>%
#  filter(Datum >= "2018-01-01" & Datum <= "2022-10-31" & Som_aantal_per_patient >= 2)  # Uitkomst tabel veranderd niet als je dit weg neemt

#GFR <- as.data.frame(GFR)%>%
#  filter(BepalingOms=="estimated GFR (CKD-EPI)")%>%  # kijk hoe GFR waardes in eigen Lab staan omschreven, dit ivm lokale codes voor GFR labwaardes
#  transmute(PatientNr=as.character(PatientNr),
#            Datum = as.Date(Datum, format="%d-%m-%Y"),
#            Uitslag= ifelse(Uitslag==">90",90,Uitslag),
#            Uitslag=as.numeric(Uitslag))%>%
#  filter(!is.na(Uitslag))

# selecteer GFR waardes <=60
GFR <- Lab %>%
  filter(BepalingOms=="estimated GFR (CKD-EPI)")%>%  # kijk hoe GFR waardes in eigen Lab staan omschreven, dit ivm lokale codes voor GFR labwaardes
  filter(Uitslag<=60)%>%
  select(-BepalingOms)

### MOET VERRICHTING NOG AANGEPAST?? specialisme en aantal toevoegen? of wordt het niet gebruikt? 02-01-2023

# Excludeer GFR waardes afgenomen tijdens SEH of klinische opname
Verrichting <- Verrichting %>%
  filter(ZACode=="190021" | ZACode=="190015" | ZACode=="190016")

# koppel opnames aan verrichtingen (in MSZ worden de opnamenummers niet goed geregistreerd bij Verrichtingen)
temp <-  left_join(Verrichting, Opnames)%>%
  filter(ZACode=="190021" & VerrichtingDatum>=OpnameDatum-1 & VerrichtingDatum<=OntslagDatum)%>%
  arrange(PatientNr, OpnameDatum) %>%
  distinct(PatientNr, OpnameDatum,.keep_all=TRUE)

Verrichting1 <- left_join(Verrichting, temp, by=c("PatientNr", "VerrichtingDatum", "ZACode"))

# VERDIEPING 8-3-22 PAK GEMIDDELDE GFR IPV LAAGSTE GFR

# koppel aan GFR waardes
GFR_gem <- left_join(GFR, Verrichting1, by="PatientNr")%>%
  mutate(excludeer=ifelse(Datum==VerrichtingDatum | (Datum>=OpnameDatum & Datum<=OntslagDatum),1,0))%>%
  arrange(PatientNr, Datum, desc(excludeer))%>%
  group_by(PatientNr, Datum)%>%
  summarise(Uitslag=min(Uitslag), excludeer=first(excludeer)) %>%  # indien op een bepaalde datum inimaal 1x gelijk aan SEh datum of klinische opname, dan staat de 1 bovenaan --> excluderen
  ungroup() %>%
  filter(excludeer==0 | is.na(excludeer))%>%
  select(-excludeer)


# selecteer  de gemiddelde GFR waarde per patiënt binnen de selectie periode 
GFR_gem <- GFR_gem %>%
  filter(Datum>=startwindow & Datum<=stopwindow_GFR) %>%
  group_by(PatientNr)%>%
  mutate(min_dat_GFR=min(Datum),
         max_dat_GFR=max(Datum),
         interval=as.numeric(difftime(max_dat_GFR, min_dat_GFR, units="days"))) %>%
  filter(interval >= 30) %>%
  summarise(Uitslag=mean(Uitslag))%>%
  ungroup() %>%
  mutate(GFR_cat = cut(Uitslag, seq(1,61,10), right=FALSE, labels=c(1:6)))

# van alle pati?nten die nu in de dataset GFR1 zitten (jan20 t/m dec21) willen we alle DBC's in jan 2020 tot nov 2022 weten
# merge tabbladen
Subtraject1 <- Subtraject %>%
  filter(StatusDBC==0) %>%
  filter((is.na(SluitingsDatumDBC)|OpeningsDatumDBC<=stopwindow_DBC) & SluitingsDatumDBC>=startwindow)


GFR_tabel_gem <-left_join(GFR_gem, Subtraject1, by = "PatientNr")

# indeling specialismen
# als patient DBCcode nefro heeft --> groep1
# als pati?nt geen DBCcode nefro heeft, maar wel andere interne --> groep 2
# als pati?nt geeen DBCcode nefro of andere interne heeft --> groep 3

# alle nefrologie codes onder 'code' benoemen (eenvoudiger coderen stap erna)
DBC_interne<-c("324","325", "301", "303", "304", "311", "313", "322", "323", "331", "332", "336", "339", "399", "76", "78")


# cree?ren groepen, houdt er rekening mee dat bijv. DBC 325 ook bij ander specialisme gebruikt kan worden,
# dus bij nefrologie specifiek als inclusiecriteria: DBC codes & specialisme interne
# eerst groep 2, want deze programmering schrijft zichzelf over, dus veranderd 2 nu in 1 indien nefro DBC's
# group_by patientnummer & summarise op min; dus als DBC_group 1 en 2 --> behoud 1 (nefro),
# voor behoud gegevens GFR_cat, moet deze er ook bij gezet worden (eerste waarde),
GFR_tabel_gem <- as.data.frame(GFR_tabel_gem)%>%
  mutate(DBC_groep=NA)%>%
  mutate(DBC_groep = ifelse(!is.na(match(SpecialismeCode,"INT")),2, NA))%>%
  mutate(DBC_groep = ifelse(DBC_groep==2 & !is.na(match(DiagnoseCode, DBC_interne)),1, DBC_groep))%>%
  mutate (DBC_groep = ifelse(is.na(DBC_groep),3, DBC_groep))%>%
  group_by(PatientNr)%>%
  summarise(DBC_final=min(DBC_groep), GFR_cat=first(GFR_cat), DiagnoseCode=max(DiagnoseCode)) %>%  # LG diagnosecode toegevoegd om te kijken of patiënten minimaal één dbc hebben
  filter(!is.na(DiagnoseCode) & DiagnoseCode!="0" & DiagnoseCode!="00" & DiagnoseCode!="000" & DiagnoseCode!="0000") %>%  #LG nieuw in C11; excludeer patiënten zonder enige dbc
  ungroup()

GFR_tabel_gem <- GFR_tabel_gem %>%
  filter (!is.na(GFR_cat)&!is.na(DBC_final)) %>%
  mutate(GFR_cat =as.numeric(GFR_cat))%>%
  filter(GFR_cat<=3)


#Uitdraai GFR tabel
table(GFR_tabel_gem$GFR_cat,GFR_tabel_gem$DBC_final)


