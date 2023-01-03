# V3 28-12-21; tabblad Subtraject opnieuw inladen ivm andere dbc selectie dan in script c9 scorekaart

library(readxl)
library(dplyr)
library(tidyr)
library(XLConnect)
library(lubridate)


rm(list=ls())

setwd ("K:/SHAREDIR/Data Santeon projecten/scorekaarten/nierfalen/Cyclus 11/Bron")

data <- lapply(excel_sheets("MZR000334_Cyclus 11_DBC nier centrumgrootte_2022-11-10.xlsx"), read_excel, path = "MZR000334_Cyclus 11_DBC nier centrumgrootte_2022-11-10.xlsx")
data1 <- lapply(excel_sheets("transplantatielijst_okt22.xlsx"), read_excel, path = "transplantatielijst_okt22.xlsx")

Patient = data[[2]]
Subtraject = data[[3]]
Verrichting = data[[4]]
Opnames = data[[5]]
GFR = data[[6]]
Tx_lijst = data1[[1]]


################ ANALYSES #######################

startwindow = as.Date("2021-01-01")
stopwindow = as.Date("2021-12-31")

Patient <- Patient%>%
  transmute(PatientNr=as.character(PatientNr),
            GeboorteDatum=as.Date(GeboorteDatum, format="%Y-%m-%d"))

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

GFR <- GFR%>%
filter(BepalingOms=="GFR (CKD-EPI)" | BepalingOms=="GFR-EPI")%>%  # kijk hoe GFR waardes in eigen Lab staan omschreven, dit ivm lokale codes voor GFR labwaardes
  transmute(PatientNr=as.character(PatientNr),
            Datum = as.Date(AfnameDatumTijd, format="%Y-%m-%d"),
            Uitslag= ifelse(Uitslag==">90",90,Uitslag),
            Uitslag=as.numeric(Uitslag))%>%
  filter(!is.na(Uitslag))

Tx_lijst <- as.data.frame(Tx_lijst) %>%
  rename(Tx_datum = "Tx datum")%>%
  transmute(PatientNr=as.character(hisNumber), 
            Tx_datum = as.Date(Tx_datum, "%Y-%m-%d"))


Verrichting <- Verrichting %>% 
  transmute (PatientNr = as.character(PatientNr),
             Verrichtingdatum= as.Date(Verrichtingdatum ,  format = "%Y-%m-%d"),	
             ZACode = as.character(ZACode),
             Aantal=as.numeric(Aantal),
             Specialisme=as.character(Specialisme)) %>% 
  distinct()


Opnames <- Opnames %>%
  filter(OntslagDatumTijd!="NULL" & !is.na(OntslagDatumTijd))%>%
  transmute (PatientNr = as.character(PatientNr),
             OpnameDatumTijd = substr(OpnameDatumTijd,1,10),
             OntslagDatumTijd = substr(OntslagDatumTijd,1,10),
             OpnameDatumTijd=as.Date(OpnameDatumTijd, format="%Y-%m-%d"),         #kijk o.b.v. je eigen data hoe je datum in juiste format krijgt
             OntslagDatumTijd =  as.Date(OntslagDatumTijd),
             OpnameNr=as.character(OpnameNr))%>%
  distinct()

# Selecteer verrichtingen mbt nazorg transplantatie 

nazorg_Tx <- as.data.frame(Verrichting)%>%
  mutate(PatientNr=as.character(PatientNr),
            ZACode= as.character(ZACode),
            VerrichtingDatum = as.Date(Verrichtingdatum, format="%Y-%m-%d"),
            Specialisme=as.character(Specialisme), #"INT", "CAR" etc
            Aantal=as.numeric(Aantal),
            nazorg_Tx=ifelse(ZACode=="039350" | ZACode=="039351" | ZACode=="039385" | ZACode=="39350" | ZACode=="39351" | ZACode=="39385",1,0))%>%
  filter(nazorg_Tx==1 & Specialisme=="INT")%>%
  filter(VerrichtingDatum>=startwindow & VerrichtingDatum<=stopwindow)%>%
  group_by(PatientNr)%>%
  summarise(VerrichtingDatum=min(VerrichtingDatum))



# groepen creeeren 
nierschade_populatie <- full_join(Subtraject, Tx_lijst)%>%
  filter((OpeningsDatumDBC<=stopwindow & SluitingsDatumDBC>=startwindow ) & (is.na(Tx_datum))| (Tx_datum>=startwindow& Tx_datum<=stopwindow))%>%  # binnen timewindow   # LG: & StatusDBC=="Gevuld"
  filter(StatusDBC == 0)%>%
  mutate(nierschadegroep = ifelse(DiagnoseCode=="336"|DiagnoseCode=="339",3,NA))%>% # groep 3= HD groep
  mutate(nierschadegroep = ifelse(DiagnoseCode=="331" | DiagnoseCode=="332",2,nierschadegroep))%>% # groep2= PD groep
  mutate(nierschadegroep=ifelse(DiagnoseCode=="301"|DiagnoseCode=="303"|DiagnoseCode=="304"|DiagnoseCode=="311"|DiagnoseCode=="313"|
                                  DiagnoseCode=="324"|DiagnoseCode=="325"|DiagnoseCode=="399"|DiagnoseCode=="76"|DiagnoseCode=="78",1,nierschadegroep))#groep 1= nierfalen

 # bij nierfalen (predialyse groep) alleen andere dbc's dan 324/325 meenemen als er ook minimaal één GFR meting is (ongeacht de waarde van deze)
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


# Tx nazorgcodes toevoegen
nierschade_populatie <- left_join(nierschade_populatie, nazorg_Tx, by="PatientNr")%>%
  filter(nierschadegroep!=1 | is.na(nierschadegroep) | (nierschadegroep==1 & !is.na(predialyse)|!is.na(VerrichtingDatum)))

rm(temp, temp2)

# exl< 18jaar
nierschade_populatie <- left_join(nierschade_populatie, Patient)%>%
  mutate  (Leeftijd   = difftime(startwindow,GeboorteDatum, units = "days")/365.25)%>%
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



# Excludeer pals predialyse / nierfalen groep en GFR>60,excludeer, deze patiënt heeft waarschijnlijk bijv. alleen een hypertensie DBC en geen nierfalen

# GFR waardes afgenomen tijdens SEH of klinische opname
Verrichting <- Verrichting%>%
  filter(ZACode=="190021" | ZACode=="190015" | ZACode=="190016")

# koppel opnames aan verrichtingen (in MSZ worden de opnamenummers niet goed geregistreerd bij Verrichtingen)
temp <-  left_join(Verrichting, Opnames)%>%
  filter(ZACode=="190021" & Verrichtingdatum>=OpnameDatumTijd-1 & Verrichtingdatum<=OntslagDatumTijd)%>%
  arrange(PatientNr, OpnameDatumTijd) %>% 
  distinct(PatientNr, OpnameDatumTijd,.keep_all=TRUE)

Verrichting1 <- left_join(Verrichting, temp, by=c("PatientNr", "Verrichtingdatum", "ZACode"))


# koppel aan GFR waardes
GFR1 <- left_join(GFR, Verrichting1, by="PatientNr")%>%
  mutate(excludeer=ifelse(Datum==Verrichtingdatum | (Datum>=OpnameDatumTijd & Datum<=OntslagDatumTijd),1,0))%>%
  arrange(PatientNr, Datum, desc(excludeer))%>%  
  group_by(PatientNr, Datum)%>%
  summarise(Uitslag=min(Uitslag), excludeer=first(excludeer)) %>%  # indien op een bepaalde datum inimaal 1x gelijk aan SEh datum of klinische opname, dan staat de 1 bovenaan --> excluderen
  filter(excludeer==0 | is.na(excludeer))%>%
  select(-excludeer)

rm(GFR)


#  als predialyse / nierfalen groep en GFR>60,excludeer, deze patiënt heeft waarschijnlijk bijv. alleen een hypertensie DBC en geen nierfalen
gfr_gem <- GFR1%>%
  filter(Datum>=startwindow & Datum<=stopwindow)%>%
  group_by(PatientNr)%>%
  summarise(gem_GFR=mean(Uitslag))


#####################
####################


centrumgrootte_final = left_join(centrumgrootte,gfr_gem)%>%
  filter((nierschadegroep==1 & gem_GFR<60) | nierschadegroep>1)




# groep 1= is nierfalen (predialyse)
# groep 2= PD
# groep 3= HD
# groep 4= Tx
# groep 5 = nazorg Tx
table(centrumgrootte_final$nierschadegroep)



