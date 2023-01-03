# V3 28-12-21; tabblad Subtraject opnieuw inladen ivm andere dbc selectie dan in script c9 scorekaart
# regel 101 veranderd, nazorg groep 5 (ipv groep 2) genoemd, anders worden deze ten onrechte opgeteld bij PD groep
# K: regel 90 Leeftijd filter toegevoegd en regel 52 filter op Tx_lijst toegevoegd
# K: regel 61 076,078 aangepast naar 76 en 78 ivm diagnosecode = numeriek
# K: regel 131 <60 aangepast naar >60
# K: regel 59 --> status DBC is gevuld
# K: regel 88 en 94 --> eerdere toevoeging in script nazorg_tx om te voorkomen dat nazorg patiënten met alleen 76/78 DBC en geen GFR waarde er onterecht uitvallen.

library(readxl)
library(dplyr)
library(tidyr)
library(XLConnect)
library(lubridate)

rm(list=ls())

setwd ("K:/SHAREDIR/Data Santeon projecten/scorekaarten/nierfalen/Cyclus 9")

data <- lapply(excel_sheets("MZR000334_Nierfalen cyclus 9_Nierfalen_2021-12-23.xlsx"), read_excel, path = "MZR000334_Nierfalen cyclus 9_Nierfalen_2021-12-23.xlsx")

Subtraject = data[[3]]

load("Verrichtingen.RData")
load("Patient.RData")
load("Tx_lijst.RData")
load("GFR.RData")
################ ANALYSES #######################

startwindow = as.Date("2020-01-01")
stopwindow = as.Date("2020-12-31")


Subtraject <- Subtraject %>%
  filter(SpecialismeCode=="INT")%>%
  mutate( PatientNr = as.character(PatientNr),
          OpeningsDatumDBC    = as.Date(OpeningsDatumDBC, format = "%Y-%m-%d"),
          SluitingsDatumDBC   = as.numeric(SluitingsDatumDBC),                                 # kijk hoe je in eigen zh de datum goed krijgt
          SluitingsDatumDBC   = as.Date(SluitingsDatumDBC, origin = "1899-12-30"),             # kijk hoe je in eigen zh de datum goed krijgt
          StatusDBC           = ifelse(StatusDBC=="Nee",1,0),                     # leeg=1, gevuld=0
          ZorgType            = ifelse(ZorgType=="R", 11, ZorgType),
          ZorgType            = ifelse(ZorgType=="L", 21, ZorgType),
          DiagnoseCode        = as.numeric(DiagnoseCode))



# Selecteer verrichtingen mbt nazorg transplantatie 
nazorg_Tx <-left_join(Verrichtingen, Tx_lijst)%>%
  filter(Specialisme=="INT")%>%
  mutate(nazorg_Tx=ifelse(ZACode=="39350" | ZACode=="39351" | ZACode=="39385",1,0))%>%
  filter(nazorg_Tx==1)%>%
  filter(VerrichtingDatum>=startwindow & VerrichtingDatum<=stopwindow)%>%
  group_by(PatientNr)%>%
  summarise(VerrichtingDatum=min(VerrichtingDatum))

Tx_lijst <- Tx_lijst %>%
  filter((Tx_datum>=startwindow& Tx_datum<=stopwindow))

# groepen creeeren 
nierschade_populatie <- full_join(Subtraject, Tx_lijst)%>%
  filter((OpeningsDatumDBC<=stopwindow & SluitingsDatumDBC>=startwindow ) & (is.na(Tx_datum))| (Tx_datum>=startwindow& Tx_datum<=stopwindow))%>%  # binnen timewindow   # LG: & StatusDBC=="Gevuld"
  filter(StatusDBC == 0)%>%
  mutate(nierschadegroep = ifelse(DiagnoseCode=="336"|DiagnoseCode=="339",3,NA))%>% # groep 3= HD groep
  mutate(nierschadegroep = ifelse(DiagnoseCode=="331" | DiagnoseCode=="332",2,nierschadegroep))%>% # groep2= PD groep
  mutate(nierschadegroep=ifelse(DiagnoseCode=="301"|DiagnoseCode=="303"|DiagnoseCode=="304"|DiagnoseCode=="311"|DiagnoseCode=="313"|
                                  DiagnoseCode=="324"|DiagnoseCode=="325"|DiagnoseCode=="399"|DiagnoseCode=="76"|DiagnoseCode=="78",1,nierschadegroep))#groep 1= nierfalen

 # bij nierfalen (predialys groep) alleen andere dbc's dan 324/325 meenemen als er ook minimaal één GFR meting is (ongeacht de waarde van deze)
temp <- nierschade_populatie%>%
  group_by(PatientNr)%>%
  mutate(predialyse=ifelse(DiagnoseCode=="324" | DiagnoseCode=="325",1,0))%>%
  arrange(PatientNr, desc(predialyse))%>%
  summarise(predialyse=first(predialyse))

temp1 <- left_join(temp, GFR)%>%
  filter(predialyse==0)%>%
  group_by(PatientNr)%>%
  arrange(PatientNr, Uitslag)%>%
  summarise(predialyse = first(Uitslag))%>%
  filter(!is.na(predialyse))
  
temp <- temp %>%
  filter(predialyse==1)

temp2 <- rbind(temp1,temp)%>%
  mutate(predialyse=1)

nierschade_populatie <- left_join(nierschade_populatie, temp2)
nierschade_populatie <- left_join(nierschade_populatie, nazorg_Tx, by="PatientNr")%>%
  filter(nierschadegroep!=1 | is.na(nierschadegroep) | (nierschadegroep==1 & !is.na(predialyse)|!is.na(VerrichtingDatum)))

rm(temp, temp2)

  # nazorg toevoegen aan dataframe
 nierschade_populatie <- nierschade_populatie%>%
     mutate  (GeboorteDatum=as.Date(GeboorteDatum, format="%d-%m-%Y"),
           Leeftijd   = difftime(startwindow,GeboorteDatum, units = "days")/365.25)%>%
    filter  (Leeftijd>=18)
  
  
  ############### taartpunten maken###############################
  
   # hiërarchische volgorde aanbrengen
  # indien transplantatie - patiënt behoort tot transplantatiegroep
  # indien nazorgtransplantatie / dialyse - patiënt behoort tot de groep in welke de patiënt het laatste zat
  # indien een patiënt in geen van bovenstaande groepen valt, nierfalen patiënt
  
  
  # uit deze alinea worden Tx en nierfalen groepen bepaald (obv hierarchische volgorde)
  nierschade_populatie1 <- nierschade_populatie%>%
    mutate(centrumgrootte= ifelse(nierschadegroep==1,1,NA))%>%
    mutate(centrumgrootte = ifelse(is.na(centrumgrootte) & !is.na(VerrichtingDatum), 5, centrumgrootte))%>%
    mutate(centrumgrootte=ifelse(!is.na(Tx_datum),4,centrumgrootte))%>% 
    arrange(PatientNr, desc(centrumgrootte))%>%
    group_by(PatientNr)%>%
    summarise(nierschadegroep=first(centrumgrootte))
  
  
  # uitdeze alinea worden dialyse / nazorg bepaald, staan gelijk in de hiearchische volgorde, waardoor de laatste behandelmodaliteit telt  
nierschade_populatie2 <- nierschade_populatie%>%
  mutate(nierschadegroep = ifelse(nierschadegroep==1 & !is.na(VerrichtingDatum),5,nierschadegroep))%>% # groep 5=nazorg
  filter(nierschadegroep!=1 & is.na(Tx_datum))%>%
  arrange(PatientNr, desc(OpeningsDatumDBC))%>%
  group_by(PatientNr)%>%
  summarise(nierschadegroep=first(nierschadegroep))


# samenvoegen 2 alinea's hierboven
centrumgrootte = rbind(nierschade_populatie1, nierschade_populatie2)%>%
  group_by(PatientNr)%>%
  summarise(nierschadegroep=last(nierschadegroep))


  #  als predialyse / nierfalen groep en GFR>60,excludeer, deze patiënt heeft waarschijnlijk bijv. alleen een hypertensie DBC en geen nierfalen
  temp3 = left_join(centrumgrootte,temp1)%>%
  filter(nierschadegroep==1 & predialyse>60)%>%
  rename(geenpredialyse=nierschadegroep)%>%
  select(-predialyse)

centrumgrootte = left_join(centrumgrootte,temp3)%>%
  filter(is.na(geenpredialyse))



# lees van deze tabel alleen Tx en nierfalen (predialyse) af! de overige groepen worden hieronder verder uitgewerkt
# groep 1= is nierfalen (predialyse)
# groep 2= PD
# groep 3= HD
# groep 4= Tx
# groep 5 = nazorg Tx
table(centrumgrootte$nierschadegroep)



  