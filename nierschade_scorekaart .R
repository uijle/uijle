
# Import standard libraries 
library(dplyr)        # For using piping operator 
library(lubridate)    # Voor easy date manupilation
library(readxl)       # Nodig zolang Excel ingelezen wordt

rm(list=ls())


#CYCLUS 11 patientselectie scorekaart

# inladen 
setwd("K:/SHAREDIR/Data Santeon projecten/scorekaarten/nierfalen/Cyclus 11/Bron")
dataset <- lapply(excel_sheets("MZR000334_Cyclus 11_DBC populatie_2022-11-11.xlsx"), read_excel, path = "MZR000334_Cyclus 11_DBC populatie_2022-11-11.xlsx")

Patient = dataset[[2]]          
Subtraject = dataset[[3]]
Verrichtingen = dataset[[4]]      
Opnames = dataset[[5]]          
Lab = dataset[[6]]

tx_lijst <- lapply(excel_sheets("transplantatielijst_okt22.xlsx"), read_excel, path = "transplantatielijst_okt22.xlsx")
Tx_lijst= tx_lijst[[1]]


Patient <- Patient%>%
  transmute (PatientNr = as.character(PatientNr), 
          GeboorteDatum=as.Date(GeboorteDatum, format="%Y-%m-%d"),
          Geslacht = as.character(Geslacht))%>% 
  distinct()

 
Subtraject <- Subtraject %>%
  transmute( PatientNr = as.character(PatientNr),
          OpeningsDatumDBC    = as.Date(OpeningsDatumDBC, format = "%Y-%m-%d"),
          SluitingsDatumDBC   = as.numeric(SluitingsDatumDBC),                                      # kijk hoe je in eigen zh de datum goed krijgt
          SluitingsDatumDBC   = as.Date(SluitingsDatumDBC, origin = "1899-12-30"),             # kijk hoe je in eigen zh de datum goed krijgt
          StatusDBC           = ifelse(StatusDBC=="Nee",1,0),                     # leeg=1, gevuld=0
          ZorgType            = ifelse(ZorgType=="R", 11, ZorgType),
          ZorgType            = ifelse(ZorgType=="L", 21, ZorgType),
          DiagnoseCode        = as.numeric(DiagnoseCode),
          SpecialismeCode     =as.character(SpecialismeCode),
           SubtrajectNr =as.character(SubtrajectNr)) 
  


Verrichtingen <- Verrichtingen %>% 
  transmute (PatientNr = as.character(PatientNr),
          VerrichtingDatum= as.Date(Verrichtingdatum ,  format = "%Y-%m-%d"),	
          ZACode = as.character(ZACode),
          Aantal=as.numeric(Aantal),
          SubtrajectNr =as.character(SubtrajectNr),
          Specialisme = as.character(Specialisme),
          AGB_CodeUitvoerder = as.character(AGB_CodeUitvoerder)) %>% 
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


GFR <- Lab%>%
  transmute(PatientNr = as.character(PatientNr),
         AfnameDatumTijd = as.Date(AfnameDatumTijd),
         Code = as.character(Code),
         Uitslag = ifelse(Uitslag==">90", 90, Uitslag),
         Uitslag = as.numeric(Uitslag),
         BepalingOms=as.character(BepalingOms))%>%
  filter(BepalingOms=="GFR (CKD-EPI)" | BepalingOms=="GFR-EPI")%>%  # kijk hoe GFR waardes in eigen Lab staan omschreven, dit ivm lokale codes voor GFR labwaardes
  select(-BepalingOms)


Tx_lijst <- as.data.frame(Tx_lijst) %>%
  transmute(PatientNr=as.character(hisNumber), 
         Tx_datum = as.Date("Tx datum", "%Y-%m-%d"))


###################################################
#### basisselectie & klaar maken voor analyses
###################################################

startwindow =as.Date("2020-01-01", format="%Y-%m-%d")
stopwindow_dbc =as.Date("2022-07-31", format="%Y-%m-%d")
stopwindow_fu =as.Date("2022-10-31", format="%Y-%m-%d")



Subtraject1 <- Subtraject %>%
  filter(SpecialismeCode=="INT" )%>%
  mutate(    groep               = ifelse(DiagnoseCode==324 | DiagnoseCode==325 | DiagnoseCode==076  | DiagnoseCode==078 ,1,NA), 
             groep               = ifelse( DiagnoseCode==336 | DiagnoseCode==339,2,groep),         # HD
             groep               = ifelse( DiagnoseCode==331 | DiagnoseCode==332,3,groep))%>%         # PD
  filter(!is.na(groep))


# We tellen de Tx dbc's op bij de nierschade dbc's als er minimaal één nierschade dbc is geweest 

temp <- Subtraject1 %>% 
  group_by(PatientNr)%>%
  filter(groep==1 & OpeningsDatumDBC<=stopwindow_dbc & (SluitingsDatumDBC >=startwindow | is.na(SluitingsDatumDBC)))%>%    #HIPS; is.na toegevoegd voor als sluitingsdatum nog niet is geweest
  arrange(PatientNr, desc(DiagnoseCode))%>%
  summarise(DiagnoseCode=first(DiagnoseCode))%>%
  filter(DiagnoseCode==324 | DiagnoseCode==325)%>%
  mutate(groep=1)%>%
  select(-DiagnoseCode)

Subtraject1 <- left_join(Subtraject1, temp, by="PatientNr")%>%
  mutate(groep.x= ifelse(groep.x==1 & groep.y!=1,NA,groep.x))%>%     
  select(-groep.y)%>%
  rename(groep = groep.x)%>%
  filter(!is.na(groep))

rm(temp)




# excludeer patiënten <18 jaar
Patient <- Patient%>%
  mutate  (Leeftijd   = difftime( startwindow,GeboorteDatum, units = "days")/365.25)%>%
  filter  (Leeftijd>=18)%>%
  mutate  (Leeftijdsgroep = ifelse(Leeftijd>=18 & Leeftijd<45,1,NA),
           Leeftijdsgroep = ifelse(Leeftijd>=45 & Leeftijd<65,2,Leeftijdsgroep),
           Leeftijdsgroep = ifelse(Leeftijd>=65 & Leeftijd<75,3, Leeftijdsgroep),
           Leeftijdsgroep = ifelse(Leeftijd>=75 & Leeftijd <80,4,Leeftijdsgroep),
           Leeftijdsgroep = ifelse(Leeftijd>=80,5, Leeftijdsgroep))


# Datum bij missende einddatum invullen
Subtraject1 <- left_join(Patient, Subtraject1, by="PatientNr")

Subtraject1 <- as.data.frame(Subtraject1) %>%
  mutate(SluitingsDatumDBC = ifelse(is.na(SluitingsDatumDBC) & ZorgType==11,OpeningsDatumDBC + 89,SluitingsDatumDBC))  %>%
  mutate(SluitingsDatumDBC = ifelse(is.na(SluitingsDatumDBC) & ZorgType==21,OpeningsDatumDBC + 119,SluitingsDatumDBC)) %>%
  mutate(SluitingsDatumDBC = as.Date(SluitingsDatumDBC,origin="1970-01-01"))

Subtraject1 <- Subtraject1 %>%
  filter(SluitingsDatumDBC>=startwindow & OpeningsDatumDBC<=stopwindow_dbc)



# Selecteer op volledige data
Subtraject1 <- Subtraject1 %>%
  filter(!(is.na(PatientNr) | is.na(OpeningsDatumDBC) | is.na(SluitingsDatumDBC) | is.na(DiagnoseCode) | is.na(ZorgType)))


### Samenvoegen opvolgende regels, per PatientNr, behandelgroep en status vol/leeg
Subtraject1 <- Subtraject1 %>%
  ungroup() %>%
  arrange(PatientNr,groep,StatusDBC,OpeningsDatumDBC,SluitingsDatumDBC) %>%
  group_by(PatientNr,groep,StatusDBC) %>%
  mutate(g = as.numeric(row_number()==1) + as.numeric(cummax(lag(as.numeric(SluitingsDatumDBC+1),default=first(as.numeric(SluitingsDatumDBC+1))))<as.numeric(OpeningsDatumDBC))) %>%
  ungroup() %>%
  mutate(g = cumsum(g)) %>%
  group_by(g) %>%
  summarise(
    PatientNr         = first(PatientNr),
    Geslacht          = first(Geslacht), 
    Leeftijdsgroep    = first(Leeftijdsgroep),
    groep             = first(groep),
    StatusDBC         = first(StatusDBC),
    OpeningsDatumDBC  = min(OpeningsDatumDBC),
    SluitingsDatumDBC = max(SluitingsDatumDBC),
    ZorgType          = first(ZorgType)) %>%
  ungroup() %>%
  select(-g)  %>%
  filter(StatusDBC==0 | StatusDBC==1)   # verwijder ongeldige dbc's 


#  als dbc's niet volledig aansloten --> fu stopte, nu mogen er 30 lege dagen tussen zitten (sinds C8)
Subtraject1 <- Subtraject1%>%
  group_by(PatientNr, groep, StatusDBC)%>%
  arrange(PatientNr, OpeningsDatumDBC)%>%
  mutate(dagen_tussen = as.numeric(lead(OpeningsDatumDBC)-SluitingsDatumDBC))%>%
  mutate(SluitingsDatumDBC= ifelse(lead(groep)==groep &dagen_tussen<30 &!is.na(dagen_tussen),lead(SluitingsDatumDBC), SluitingsDatumDBC))%>%
  mutate(SluitingsDatumDBC=as.Date(SluitingsDatumDBC,origin="1970-01-01"))

Subtraject1<- Subtraject1%>%
  group_by(PatientNr, groep, SluitingsDatumDBC, StatusDBC)%>%
  summarise(      OpeningsDatumDBC  = min(OpeningsDatumDBC),
                  ZorgType          = first(ZorgType),
                  Geslacht          = first(Geslacht), 
                  Leeftijdsgroep    = first(Leeftijdsgroep))%>%
  ungroup() 


#######################################################################################
################################ nierschade groep ######################################

### Haal lege DBCs >360 dagen op het einde eruit voor DBC 324 / 325

nierschade <- Subtraject1 %>%
  arrange(PatientNr,groep,OpeningsDatumDBC,SluitingsDatumDBC) %>%
  mutate(leegduur = ifelse(groep==1 & StatusDBC==1,SluitingsDatumDBC - OpeningsDatumDBC + 1,NA))

# bepaal of de lege DBC de laatste regel is binnen patient en groep
nierschade <- nierschade %>%
  arrange(PatientNr,groep,OpeningsDatumDBC,StatusDBC) %>%
  group_by(PatientNr,groep) %>%
  mutate(seq = row_number()) %>%
  mutate(revseq = max(row_number())-row_number()+1)%>%
  mutate(minleeg=min(StatusDBC))%>%
  mutate(maxleeg=max(StatusDBC))


nierschade <- nierschade %>%
  filter(!(leegduur>=360 & groep==1 & StatusDBC==1 & revseq==1))%>%
  filter(!(groep==1 & StatusDBC==1 & revseq==1 & minleeg==1 & maxleeg==1))%>%
  select(-seq,-revseq,-leegduur)



### Samenvoegen opvolgende regels, per behandelgroep, ongeacht status leeg

nierschade <- nierschade%>%
  arrange(PatientNr,groep,OpeningsDatumDBC,SluitingsDatumDBC) %>%
  group_by(PatientNr,groep) %>%
  mutate(g = as.numeric(row_number()==1) + as.numeric(cummax(lag(as.numeric(SluitingsDatumDBC+1),default=first(as.numeric(SluitingsDatumDBC+1))))<as.numeric(OpeningsDatumDBC))) %>%
  ungroup() %>%
  mutate(g = cumsum(g)) %>%
  group_by(g) %>%
  summarise(
    PatientNr  = first(PatientNr),
    groep          = first(groep),
    OpeningsDatumDBC = min(OpeningsDatumDBC),
    SluitingsDatumDBC  = max(SluitingsDatumDBC),
    Geslacht          = first(Geslacht), 
    Leeftijdsgroep    = first(Leeftijdsgroep)) %>%
  ungroup() %>%
  select(-g)  


# Toevoegen daadwerkelijke transplantatiedatums 

nierschade <- left_join(nierschade,Tx_lijst)  


### Bepaal de follow-up duur van de nierschade groep. Follow-up stopt op het moment dat er een andere behandelgroep start.

#Situatie 1: Een andere behandeling start, terwijl nierschade doorloopt. Zet einddatum nierschade op dag voor start behandeling

nierschade1 <- nierschade %>%
  arrange(PatientNr, OpeningsDatumDBC, groep, SluitingsDatumDBC)%>%
  mutate(check = ifelse(groep==1 & lead(PatientNr)==PatientNr & lead(groep)!=groep & lead(OpeningsDatumDBC)<=SluitingsDatumDBC,1,0)) %>%
  mutate(check = ifelse(max(row_number())-row_number()==0,0,check)) %>%
  mutate(SluitingsDatumDBC = if_else(check==1,lead(OpeningsDatumDBC)-1,SluitingsDatumDBC)) %>%
  mutate(SluitingsDatumDBC = as.Date(SluitingsDatumDBC,origin="1970-01-01"))

# LG: toegevoegd C9 o.b.v. transplantatielijsten ipv dbc 076/078

nierschade1 <- nierschade1 %>%    
  mutate(SluitingsDatumDBC = ifelse(Tx_datum<SluitingsDatumDBC & !is.na(Tx_datum), Tx_datum - 1, SluitingsDatumDBC))%>%
  mutate(SluitingsDatumDBC = as.Date(SluitingsDatumDBC,origin="1970-01-01"))%>%
  filter(!(OpeningsDatumDBC>SluitingsDatumDBC)) %>%
  select(-check)


#Situatie 2: Een andere behandeling was al gestart op het moment dat nierschade start. Verwijder nierschade traject

nierschade1 <- nierschade1 %>%
  mutate(tmpcheck = ifelse(groep==1 & lag(PatientNr)==PatientNr & lag(groep)!=groep & lag(OpeningsDatumDBC)<=OpeningsDatumDBC,1,0)) %>%
  mutate(tmpcheck = ifelse(row_number()==1,0,tmpcheck))

temp <- nierschade1[,c("PatientNr","tmpcheck")]

temp <- temp %>%
  group_by(PatientNr) %>%
  summarise(check = sum(tmpcheck)) %>%
  mutate(check = as.numeric(check>0))

temp <- data.frame(temp)

nierschade1 <- nierschade1 %>%
  left_join(temp,by="PatientNr") %>%
  mutate(check= ifelse(check==1 & lag(PatientNr)!=PatientNr,0,check))%>%  #HIPS: indien eerste regel 324/325 is, meenemen
  filter(check==0) %>%
  select(-tmpcheck,-check)

rm(temp)


### Gooi de andere groepen eruit

nierschade1 <- nierschade1 %>%
  filter(groep==1)


### Selecteer het eerste traject

nierschade1 <- nierschade1 %>%
  arrange(PatientNr,OpeningsDatumDBC) %>%
  group_by(PatientNr) %>%
  mutate(seq = row_number()) %>%
  filter(seq == 1) %>%
  select(-seq)

# final selectie
nierschade <- data.frame(nierschade1)%>%
  filter(OpeningsDatumDBC<=stopwindow_dbc & SluitingsDatumDBC>=startwindow)


save(nierschade, file="nierschade.RData")

rm(nierschade1)

###########
# CASEMIX
##########

table(nierschade$Geslacht)

table(nierschade$Leeftijdsgroep)

# mediane leeftijd
# 
# mediane_leeftijd<- left_join(nierschade, Patient)%>%
#   mutate(Leeftijd=as.numeric(Leeftijd))
# summary(mediane_leeftijd$Leeftijd)


#########################
# SCOREKAART INDICATOREN#
########################
# U4 cardiovasculaire incidenten


# koppel bestanden
U4 <- left_join(nierschade %>% select(PatientNr, OpeningsDatumDBC, SluitingsDatumDBC),
         Subtraject %>% select(PatientNr, OpeningsDatumDBC, SluitingsDatumDBC, ZorgType, StatusDBC, DiagnoseCode, SpecialismeCode), by="PatientNr")%>%
         filter(ZorgType=="11" & StatusDBC==0 &  # gaat om incidenten, dus alleen nieuwe, gevulde dbc's 
         SluitingsDatumDBC.y>=OpeningsDatumDBC.x & OpeningsDatumDBC.y <=SluitingsDatumDBC.x)  # cardio dbc's binnen individuele nierschade follow-up

# hercodeer CVD in subgroepen
U4<- as.data.frame(U4)%>%  
  mutate (infarct = ifelse(SpecialismeCode=="CAR" & (DiagnoseCode==203 | DiagnoseCode==204 | DiagnoseCode==205), 1, 0),
          neuro = ifelse(SpecialismeCode=="NEU"  & (DiagnoseCode==402 | DiagnoseCode==1101 | DiagnoseCode==1102 | DiagnoseCode==1103 | DiagnoseCode==1111 | DiagnoseCode==1112 | DiagnoseCode==1121 | DiagnoseCode==1199), 1, 0), 
          perifeer = ifelse(SpecialismeCode=="CHI" & (DiagnoseCode==405 | DiagnoseCode==406 | DiagnoseCode==408 | DiagnoseCode==409 | DiagnoseCode==412 | DiagnoseCode==416 | DiagnoseCode==417 | DiagnoseCode==418 | DiagnoseCode==419 | 
                            DiagnoseCode==420 | DiagnoseCode==421 | DiagnoseCode==422 | DiagnoseCode==432  | DiagnoseCode==439), 1,0),
          cvd_totaal = infarct + neuro + perifeer)%>%
  group_by(PatientNr)%>%
  summarise(cvd_totaal=max(cvd_totaal), infarct=max(infarct), neuro=max(neuro), perifeer=max(perifeer))

U4 <- left_join(nierschade %>% select(PatientNr),
                U4)%>%
  mutate(cvd_totaal = ifelse(is.na(cvd_totaal),0,cvd_totaal),
         infarct    = ifelse(is.na(infarct),0,infarct),
         neuro      = ifelse(is.na(neuro),0,neuro),
         perifeer   = ifelse(is.na(perifeer),0,perifeer))

# aantallen incidenten
table(U4$cvd_totaal)
table(U4$infarct)
table(U4$neuro)
table(U4$perifeer)




# K1 klinische opnames

# koppel opnames aan verrichtingen (in MSZ worden de opnamenummers niet goed geregistreerd bij Verrichtingen)
temp <-  left_join(Verrichtingen, Opnames)%>%
  filter(ZACode=="190021" & VerrichtingDatum>=OpnameDatumTijd-1 & VerrichtingDatum<=OntslagDatumTijd)%>%
  arrange(PatientNr, OpnameDatumTijd) %>% 
  distinct(PatientNr, OpnameDatumTijd,.keep_all=TRUE)


# koppelen aan nierschade groep
K11 <- left_join(nierschade, temp)%>%
  filter(VerrichtingDatum >= OpeningsDatumDBC & 
           VerrichtingDatum <= SluitingsDatumDBC)

K111 <- K11 %>%
  summarise(Aantal=sum(Aantal))

K112 <- K11%>%
  filter(Specialisme=="INT")%>%
  summarise(Aantal=sum(Aantal))

K113 <- K11%>%
  filter(Specialisme=="CHI")%>%
  summarise(Aantal=sum(Aantal))

K116 <- K11%>%
  filter(Specialisme=="CAR")%>%
  summarise(Aantal=sum(Aantal))


# Noemer persoonsjaren K1, K3 (& subindicatoren)
noemer_K1_K3 <- nierschade %>%
  mutate(OpeningsDatumDBC= ifelse(OpeningsDatumDBC<startwindow, startwindow, OpeningsDatumDBC), 
         SluitingsDatumDBC = ifelse(SluitingsDatumDBC>stopwindow_fu, stopwindow_fu, SluitingsDatumDBC))%>%
  mutate(OpeningsDatumDBC=as.Date(OpeningsDatumDBC,origin="1970-01-01"),
         SluitingsDatumDBC=as.Date(SluitingsDatumDBC,origin="1970-01-01"))%>%
  mutate(FU = difftime(SluitingsDatumDBC, OpeningsDatumDBC, units = "days")+1) %>%
  mutate(FU = as.numeric(FU/365))%>%
  group_by(groep)%>%
  summarize(FU_sum=sum(FU))



### K31a fysieke poli
# excl poli's gekoppeld aan seh 
# binnen timewindow

poli <- left_join(nierschade, Verrichtingen, by="PatientNr")
SEH_bezoeken  <- poli %>% filter(   ZACode %in% c("190016", "190015") ) %>%                                               # Selecteer polibezoeken
  group_by( PatientNr, VerrichtingDatum, Specialisme) %>%
  mutate(   AantalSEH  = sum(Aantal)) %>%
  distinct( PatientNr, VerrichtingDatum, Specialisme, AantalSEH )

fysiek  <- merge( x      = poli %>% filter(  ZACode %in% c("190060","190013","190063", "190066", "198200","198201","198202") ) %>%                                               # Poli-bezoeken
                    group_by(PatientNr, VerrichtingDatum, Specialisme) %>%
                    mutate(  AantalPoli  = sum(Aantal)) %>%
                    distinct(PatientNr, VerrichtingDatum, Specialisme, .keep_all = TRUE),
                  y      = SEH_bezoeken,
                  by     = c("PatientNr", "VerrichtingDatum", "Specialisme"),
                  all.x  = TRUE) %>%
  mutate(Aantal = ifelse(AantalPoli - AantalSEH <  0 & !is.na(AantalSEH), 0, AantalPoli),
         Aantal = ifelse(AantalPoli - AantalSEH >= 0 & !is.na(AantalSEH), AantalPoli-AantalSEH, AantalPoli))%>%
  filter(VerrichtingDatum>=OpeningsDatumDBC & VerrichtingDatum<=SluitingsDatumDBC)%>% # binnen individuele timewindow
  group_by(PatientNr, VerrichtingDatum, AGB_CodeUitvoerder,Specialisme)%>%
  summarise(Aantal=sum(Aantal))%>%
  mutate(Aantal=ifelse(Aantal>1,1,Aantal))%>%
  mutate(poli_tele = 1) # 1 = fysiek

K31a <- fysiek %>%
  group_by(PatientNr, VerrichtingDatum, AGB_CodeUitvoerder)%>%
  summarise(Aantal=sum(Aantal))%>%
  mutate(Aantal=ifelse(Aantal>1,1,Aantal))

sum(K31a$Aantal)

# totaal tele
tele <- left_join(nierschade, Verrichtingen)%>%
  filter(ZACode=="190019"| ZACode=="190025"| ZACode=="190162"|ZACode=="190161"| ZACode=="190164"| 
           ZACode=="190165"| ZACode=="198203"| ZACode=="198204"| ZACode=="190163"| ZACode=="190166"| ZACode=="190167")%>%
  filter(VerrichtingDatum>=OpeningsDatumDBC & VerrichtingDatum<=SluitingsDatumDBC)%>% # binnen individuele timewindow
  group_by(PatientNr, VerrichtingDatum, AGB_CodeUitvoerder,Specialisme)%>%
  summarise(Aantal=sum(Aantal))%>%
  mutate(Aantal=ifelse(Aantal>1,1,Aantal)) %>%
mutate(poli_tele = 2) # 2 = teleconsult


K311<- tele %>%
  group_by(PatientNr, VerrichtingDatum, AGB_CodeUitvoerder)%>%
  summarise(Aantal=sum(Aantal))%>%
  mutate(Aantal=ifelse(Aantal>1,1,Aantal))

sum(K311$Aantal)


# INTERNE

# K33a fysiek
K33a <- fysiek%>%
  filter(Specialisme=="INT")
sum(K33a$Aantal)

# K331 tele
K331 <- tele%>%
  filter(Specialisme=="INT")
sum(K331$Aantal)



# CHIRURGIE

# K34 fysiek
K34a <- fysiek%>%
  filter(Specialisme=="CHI")
sum(K34a$Aantal)

# K341 tele
K341 <- tele%>%
  filter(Specialisme=="CHI")
sum(K341$Aantal)



# CARDIOLOGIE

# K37 fysiek
K37a <- fysiek%>%
  filter(Specialisme=="CAR")
sum(K37a$Aantal)

# K371 tele
K371 <- tele%>%
  filter(Specialisme=="CAR")
sum(K341$Aantal)




# p1.2 % PATIËNTEN DAT BINNEN 3MND START MET DIALYSEREN

start_dialyse_DBC <- Subtraject %>%
  filter(DiagnoseCode==331 | DiagnoseCode == 332 | DiagnoseCode == 336 | DiagnoseCode ==339)%>%
  arrange(PatientNr, OpeningsDatumDBC)%>%
  group_by(PatientNr)%>%
  summarise(OpeningsDatumDBC= first(OpeningsDatumDBC),DiagnoseCode= first(DiagnoseCode),ZorgType=  first(ZorgType))


# merge met verrichtingen
verrichtingen_dialyse <- Verrichtingen%>%
  filter(Specialisme=="INT" & ZACode=="192048"| ZACode=="192049"| ZACode=="192051"| ZACode=="192052"| ZACode=="192053"| ZACode=="192054"| 
           ZACode=="192055"| ZACode=="192056"| ZACode=="192058"| ZACode=="192059"| ZACode=="192060"| ZACode=="192061"| ZACode=="192062"| ZACode=="192063"|
           ZACode=="192064"| ZACode=="192065"| ZACode=="192066"| ZACode=="192068"| ZACode=="192069" | ZACode=="190128"| ZACode=="190137"|ZACode=="190144"|
           ZACode=="190156"| ZACode=="39977"| ZACode=="39978") 

verrichtingen_dialyse <- left_join(start_dialyse_DBC, verrichtingen_dialyse, by="PatientNr")


# verwijder opvolgende DBC's behoudt alleen 1e dialyse dbc; voorkom nieuwe ivm switch ene dialyse naar andere dialyse
verrichtingen_dialyse<- as.data.frame(verrichtingen_dialyse)%>%
  group_by(PatientNr)%>%
  arrange(PatientNr, VerrichtingDatum)%>%
  summarise(VerrichtingDatum = first(VerrichtingDatum), ZACode=first(ZACode), DiagnoseCode= first(DiagnoseCode))



# koppelen verrichtingen aan nieuwe DBC codes
# timewindow vanaf april 2020, zodat je minimaal 3 maanden terug in de tijd kunt kijken of er een nierschade dbc is
start_window_dialyse = as.Date("2020-04-01")   

dialyse_nieuw <- left_join(start_dialyse_DBC, verrichtingen_dialyse, by=c("PatientNr", "DiagnoseCode"))%>%
  filter(ZorgType=="11" & VerrichtingDatum >=start_window_dialyse)%>%
  group_by(PatientNr)%>%
  arrange(PatientNr, VerrichtingDatum)%>%
  summarise(VerrichtingDatum=first(VerrichtingDatum)) # dit is de noemer voor indicator P1.2



# Bekijk hoeveel van deze nieuwe dialyse patiënten <=3 maanden daarvoor voor het eerste een 324/325 hadden
# behoudt alleen nieuwe DBC 324/325

eerste_NF_DBC<- Subtraject%>%  
  filter(DiagnoseCode == "324" | DiagnoseCode == "325")%>%
  arrange(PatientNr, OpeningsDatumDBC)%>%
  group_by(PatientNr) %>%
  summarise(OpeningsDatumDBC =first(OpeningsDatumDBC), ZorgType=first(ZorgType))%>% #behoudt oudste datum DBC 324 of 325
  filter(ZorgType=="11")

# koppelen eerste 324/325 DBC aan nieuwe dialyse
dialyse_3mnd <- left_join(dialyse_nieuw, eerste_NF_DBC, by="PatientNr")%>%
  filter(!is.na(ZorgType))


# koppel aan patiëntselectie
dialyse_3mnd <- left_join(nierschade %>% select(PatientNr),
                          dialyse_3mnd,by="PatientNr")%>%
  filter(!is.na(VerrichtingDatum))

# check alle patiënten handmatig en deel deze in juiste subindicator in (P1.2.1-P1.2.4)
dialyse_3mnd <- dialyse_3mnd%>%
  mutate(dagen_verschil = as.numeric(VerrichtingDatum - OpeningsDatumDBC))%>%
  filter(dagen_verschil <=90 | is.na(dagen_verschil))
  



# P7 GFR bij verwijzing NF

# Excludeer GFR waardes afgenomen tijdens SEH of klinische opname
Verrichtingen_GFR <- Verrichtingen%>%
  filter(ZACode=="190021" | ZACode=="190015" | ZACode=="190016")

# koppel opnames aan verrichtingen (in MSZ worden de opnamenummers niet goed geregistreerd bij Verrichtingen)
temp <-  left_join(Verrichtingen_GFR, Opnames)%>%
  filter(ZACode=="190021" & VerrichtingDatum>=OpnameDatumTijd-1 & VerrichtingDatum<=OntslagDatumTijd)%>%
  arrange(PatientNr, OpnameDatumTijd) %>% 
  distinct(PatientNr, OpnameDatumTijd,.keep_all=TRUE)

Verrichtingen1 <- left_join(Verrichtingen_GFR, temp, by=c("PatientNr", "VerrichtingDatum", "ZACode"))


# koppel aan GFR waardes
GFR1 <- left_join(GFR, Verrichtingen1, by="PatientNr")%>%
  mutate(excludeer=ifelse(AfnameDatumTijd==VerrichtingDatum | (AfnameDatumTijd>=OpnameDatumTijd & AfnameDatumTijd<=OntslagDatumTijd),1,0))%>%
  arrange(PatientNr, AfnameDatumTijd, desc(excludeer))%>%  
  group_by(PatientNr, AfnameDatumTijd)%>%
  summarise(Uitslag=min(Uitslag), excludeer=first(excludeer)) %>%  # indien op een bepaalde datum inimaal 1x gelijk aan SEh datum of klinische opname, dan staat de 1 bovenaan --> excluderen
  filter(excludeer==0 | is.na(excludeer))%>%
  select(-excludeer)


# GFR koppelen aan nieuwe DBC codes
eerste_NF_DBC <- eerste_NF_DBC%>%
  group_by(PatientNr)%>%
  summarise(OpeningsDatumDBC=min(OpeningsDatumDBC))%>%
  filter(OpeningsDatumDBC>=startwindow & OpeningsDatumDBC<=stopwindow_dbc) # binnen timewindow

# behoudt eerst gemeten GFR na openen dbc 324/325 (kijken vanaf een maand voor dbc diagnose)
GFR_eerste_DBC <- left_join(eerste_NF_DBC, GFR1, by="PatientNr")%>%
  mutate(verschil_datum_GFR_DBC = as.numeric(AfnameDatumTijd - OpeningsDatumDBC))%>%
  filter(verschil_datum_GFR_DBC >=-30 & verschil_datum_GFR_DBC<=90) %>%
  group_by(PatientNr)%>%
  arrange(PatientNr, AfnameDatumTijd)%>%
  summarise(Uitslag=first(Uitslag))%>%
  ungroup()%>%
  mutate(aantal=1) 

# bereken gemiddelde/mediaan/kwartiel etc
P73 <- GFR_eerste_DBC%>%
  filter(!is.na(Uitslag))%>%
  summarise(teller=sum(Uitslag), noemer=sum(aantal),gem=mean(Uitslag), mediaan=median(Uitslag), min(Uitslag),p5=quantile(Uitslag,c(0.05)),Q1=quantile(Uitslag,c(0.25)),Q3=quantile(Uitslag,c(0.75)), p95=quantile(Uitslag,c(0.95)),max= max(Uitslag))


# P8  Patiënten die met GFR<30 voor het eerst bij de nefroloog komen 
GFR_eerste_DBC <- GFR_eerste_DBC%>%
mutate(GFR30 = ifelse(Uitslag<30,1,0))

P8 <- GFR_eerste_DBC %>%
  filter(!is.na(GFR30))%>%
  summarise(teller=sum(GFR30), noemer=sum(aantal))



