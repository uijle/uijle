
# 23-12: verwijderd berekening indicatoren totale poli (bij K3 en subindicatoren) (FYSIEK + DIGITAAL) wordt nu automatisch berekend in scorekaart
# 28-12  verwijderd 'save(Subtraject)' in het andere script moeten er meerdere dbc's worden meegenomen dan in dit script
#        GFR dataframe opslaan voor syntax centrumgroote
# 3-1-22 K33 toegevoegd
# 4-1-22 regel 520 fout in datum (start_window_dialyse = as.Date("2019-01-04")  moet zijn 1 april 2019  as.Date("2019-04-01") 
# 10-1-22 bij K3 klopte de nummering niet er stond bijv K3.2.2 = fysiek, terwijl K3.3a fysieke poli is. De totalen worden in de scorekaart berekend, niet in de syntax
# 13-1-22 verwijder patiënten met geen enkele gevulde DBC (aanpassing regels 234,235,240)
# 19-1-22 Neem alleen nierfalen patiënten mee bij indicator P1.2 (r566)
# 20-1-22 P1.2 join by patiëntnr (r566)

library(readxl)
library(dplyr)
library(tidyr)
library(XLConnect)
library(lubridate)
library(sqldf)
library(xlsx)


rm(list=ls())

### Lees bronbestand in


setwd ("K:/SHAREDIR/Data Santeon projecten/scorekaarten/nierfalen/Cyclus 9")

# data voor standaard scorekaart & casemix
data <- lapply(excel_sheets("MZR000334_Nierfalen cyclus 9_Nierfalen_2021-12-23.xlsx"), read_excel, path = "MZR000334_Nierfalen cyclus 9_Nierfalen_2021-12-23.xlsx")

Patient =data[[2]]
Subtraject = data[[3]]
Verrichtingen = data[[4]]
Opnames = data[[6]]
Agenda = data[[7]]
Lab = data[[5]]

tx_lijst <- lapply(excel_sheets("transplantatielijst.xlsx"), read_excel, path = "transplantatielijst.xlsx")
Tx_lijst= tx_lijst[[2]]


# Stel startdatum en einddatum tijdswindow in

startwindow = as.Date("2019-01-01")
stopwindow = as.Date("2021-10-31")

##########################
# Standaardiseren data
#########################
Patient <- as.data.frame(Patient)%>%
mutate (PatientNr = as.character(PatientNr), 
        GeboorteDatum=as.Date(GeboorteDatum, format="%Y-%m-%d"),
        Geslacht = as.character(Geslacht))%>% 
  distinct()%>%
  select(PatientNr,Geslacht,GeboorteDatum)


# Nieuw sinds C9 : nierfalen en Tx dbc's worden samengenomen: Tx dbc's zeggen niks over de transplanatie status, maar puur of ze gescreend zijn voor Tx
  # vereiste dat een patiënt minimaal één keer een 324/325 dbc moet hebben om in de nierfalen groep terecht te komen
  # later in het script wordt de daadwerkelijke transplantatiedatum meegenomen om het einde van het nierfalen traject aan te duiden
Subtraject <- Subtraject %>%
  filter(SpecialismeCode=="INT")%>%
  mutate( PatientNr = as.character(PatientNr),
          OpeningsDatumDBC    = as.Date(OpeningsDatumDBC, format = "%Y-%m-%d"),
          SluitingsDatumDBC   = as.numeric(SluitingsDatumDBC),                                 # kijk hoe je in eigen zh de datum goed krijgt
          SluitingsDatumDBC   = as.Date(SluitingsDatumDBC, origin = "1970-01-01"),             # kijk hoe je in eigen zh de datum goed krijgt
          StatusDBC           = ifelse(StatusDBC=="Nee",1,0),                     # leeg=1, gevuld=0
          ZorgType            = ifelse(ZorgType=="R", 11, ZorgType),
          ZorgType            = ifelse(ZorgType=="L", 21, ZorgType),
          DiagnoseCode        = as.numeric(DiagnoseCode),
          groep               = ifelse(DiagnoseCode==324 | DiagnoseCode==325 | DiagnoseCode==076  | DiagnoseCode==078,1,NA), # chronische nierfalen, nieuw sinds C9 ook Tx dbc's erbij
          groep               = ifelse( DiagnoseCode==336 | DiagnoseCode==339,2,groep),         # HD
          groep               = ifelse( DiagnoseCode==331 | DiagnoseCode==332,3,groep))%>%         # PD
  filter(!is.na(groep))%>%
  select(PatientNr,groep, DiagnoseCode,OpeningsDatumDBC,SluitingsDatumDBC, StatusDBC, ZorgType)
  

# NIEUW IN C9: we tellen de Tx dbc's op bij de nierfalen dbc's als er minimaal één nierfalen dbc is geweest

temp <- Subtraject %>% 
  group_by(PatientNr)%>%
  filter(groep==1 & OpeningsDatumDBC<=stopwindow & SluitingsDatumDBC >=startwindow)%>%
  arrange(PatientNr, desc(DiagnoseCode))%>%
  summarise(DiagnoseCode=first(DiagnoseCode))%>%
  filter(DiagnoseCode==324 | DiagnoseCode==325)%>%
  mutate(groep=1)%>%
  select(-DiagnoseCode)

Subtraject1 <- left_join(Subtraject, temp, by="PatientNr")%>%
  mutate(groep.x= ifelse(groep.x==1 & groep.y!=1,NA,groep.x))%>%
  select(-groep.y)%>%
  rename(groep = groep.x)%>%
  filter(!is.na(groep))

rm(temp)




Verrichtingen <- as.data.frame(Verrichtingen)%>%
  mutate(PatientNr=as.character(PatientNr),
         VerrichtingDatum = as.Date(Verrichtingdatum, format="%Y-%m-%d"),
         ZACode=as.character(ZACode),
         OpnameNr=as.numeric(OpnameNr),
         Specialisme=as.character(Specialisme), #"INT", "CAR" etc
         AGB_CodeUitvoerder = as.character(AGB_CodeUitvoerder),
         Aantal=as.numeric(Aantal))%>% 
  select(PatientNr, VerrichtingDatum, ZACode, Specialisme,AGB_CodeUitvoerder, Aantal)


Opnames <- as.data.frame(Opnames)%>%
  mutate(PatientNr=as.character(PatientNr),
         OpnameDatum = as.Date(OpnameDatumTijd, format="%Y-%m-%d"),
         OntslagDatum = as.Date(OntslagDatumTijd, format="%Y-%m-%d"))%>%
  select(PatientNr, OpnameDatum, OntslagDatum)

Agenda <- as.data.frame(Agenda)%>%
  mutate(PatientNr=as.character(Patientnummer),
         AfspraakDatum = as.Date(AfspraakDatum, format="%Y-%m-%d"),
         AfspraakCode=as.character(AfspraakCode),   # kijk in eigen ziekenhuis welke codes fysieke poli's en teleconsulten representeren 
         Consulttype=as.character(Consulttype))%>%     # hier moet straks de onderverdeling tele/fysiek consult duidelijk worden
         filter(AgendaNaam=="INTERNE GENEESKUNDE MSZ")%>% # filter op agenda's nefrologen 
         filter(SubagendaNaam=="ALPHEN, VAN" |SubagendaNaam=="DORPEL, VAN DEN"|SubagendaNaam=="BOOTS"|SubagendaNaam=="KLEINHERENBRINK ,NEFROLOOG I.O"|
         SubagendaNaam=="FORTRIE, NEFROLOOG I.O."|SubagendaNaam=="JONKERS, NEFROLOOG  I.O."|SubagendaNaam=="MEURS, VAN,  NEFROLOOG I.O."|
         SubagendaNaam=="HUANG, NEFROLOOG I.O."|SubagendaNaam=="KHARAGJITSING, NEFROLOOG I.O."|SubagendaNaam=="VAREWIJCK, Nefroloog i.o."|
                  SubagendaNaam=="SWART"|SubagendaNaam=="VERBERK-JONKERS"|   SubagendaNaam=="P. VAN DE VEN" | SubagendaNaam=="DE BIE, Nefroloog  i.o." )
  select(PatientNr, AfspraakDatum)


Lab <- as.data.frame(Lab)%>%
  mutate(PatientNr=as.character(PatientNr),
         Datum = as.Date(AfnameDatumTijd, format="%Y-%m-%d"),
         Uitslag= ifelse(Uitslag==">90",90,Uitslag),
         Uitslag=as.numeric(Uitslag),
         BepalingOms=as.character(BepalingOms))%>%
  select(PatientNr, Datum, Uitslag, BepalingOms)

Tx_lijst <- as.data.frame(Tx_lijst) %>%
  mutate(PatientNr=as.character(hisNumber), 
         Tx_datum = as.Date(MaxVanrangeStart, "%d-%m-%Y"))%>%    # check hoe de datum in eigen data staat, pas indien nodig aan!
  select(PatientNr, Tx_datum)



###################################################
#### basisselectie & klaar maken voor analyses
###################################################

# excludeer patiënten <18 jaar
Patient <- Patient%>%
  mutate  (Leeftijd   = difftime( startwindow,GeboorteDatum, units = "days")/365.25)%>%
  filter  (Leeftijd>=18)%>%
  mutate  (Leeftijdsgroep = ifelse(Leeftijd>=18 & Leeftijd<45,1,NA),
           Leeftijdsgroep = ifelse(Leeftijd>=45 & Leeftijd<65,2,Leeftijdsgroep),
           Leeftijdsgroep = ifelse(Leeftijd>=65 & Leeftijd<75,3, Leeftijdsgroep),
           Leeftijdsgroep = ifelse(Leeftijd>=75 & Leeftijd <80,4,Leeftijdsgroep),
           Leeftijdsgroep = ifelse(Leeftijd>=80,5, Leeftijdsgroep))

save(Patient, file="Patient.RData")

# Datum bij missende einddatum invullen

Subtraject <- left_join(Patient, Subtraject1, by="PatientNr")


Subtraject <- as.data.frame(Subtraject) %>%
  mutate(SluitingsDatumDBC = ifelse(is.na(SluitingsDatumDBC) & ZorgType==11,OpeningsDatumDBC + 89,SluitingsDatumDBC))  %>%
  mutate(SluitingsDatumDBC = ifelse(is.na(SluitingsDatumDBC) & ZorgType==21,OpeningsDatumDBC + 119,SluitingsDatumDBC)) %>%
  mutate(SluitingsDatumDBC = as.Date(SluitingsDatumDBC,origin="1899-12-30"))

Subtraject <- Subtraject %>%
  filter(SluitingsDatumDBC>=startwindow & OpeningsDatumDBC<=stopwindow)

# Selecteer op volledige data
Subtraject <- Subtraject %>%
  filter(!(is.na(PatientNr) | is.na(OpeningsDatumDBC) | is.na(SluitingsDatumDBC) | is.na(DiagnoseCode) | is.na(ZorgType)))


save(Verrichtingen, file="Verrichtingen.RData")
save(Tx_lijst, file="Tx_lijst.RData")

### Samenvoegen opvolgende regels, per PatientNr, behandelgroep en status vol/leeg
Subtraject1 <- Subtraject %>%
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


# LG: toegevoegd (vanaf cyclus 8); als dbc's niet volledig aansloten --> fu stopte, nu mogen er 30 lege dagen tussen zitten
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
################################ nierfalen groep ######################################


### Haal lege DBCs >360 dagen op het einde eruit voor DBC 324 / 325

nierfalen <- Subtraject1 %>%
  arrange(PatientNr,groep,OpeningsDatumDBC,SluitingsDatumDBC) %>%
  mutate(leegduur = ifelse(groep==1 & StatusDBC==1,SluitingsDatumDBC - OpeningsDatumDBC + 1,NA))

# bepaal of de lege DBC de laatste regel is binnen patient en groep
nierfalen <- nierfalen %>%
  arrange(PatientNr,groep,OpeningsDatumDBC,StatusDBC) %>%
  group_by(PatientNr,groep) %>%
  mutate(seq = row_number()) %>%
  mutate(revseq = max(row_number())-row_number()+1)%>%
  mutate(minleeg=min(StatusDBC))%>%
  mutate(maxleeg=max(StatusDBC))



nierfalen <- nierfalen %>%
  filter(!(leegduur>=360 & groep==1 & StatusDBC==1 & revseq==1))%>%
  filter(!(groep==1 & StatusDBC==1 & revseq==1 & minleeg==1 & maxleeg==1))%>%
select(-seq,-revseq,-leegduur)



### Samenvoegen opvolgende regels, per behandelgroep, ongeacht status leeg

nierfalen <- nierfalen%>%
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


# NIEUW IN C9: # Toevoegen daadwerkelijke transplantatiedatums 

nierfalen <- left_join(nierfalen,Tx_lijst)  


### Bepaal de follow-up duur van de nierfalen groep. Follow-up stopt op het moment dat er een andere behandelgroep start.

#Situatie 1: Een andere behandeling start, terwijl nierfalen doorloopt. Zet einddatum nierfalen op dag voor start behandeling

nierfalen1 <- nierfalen %>%
  arrange(PatientNr, OpeningsDatumDBC, groep, SluitingsDatumDBC)%>%
  mutate(check = ifelse(groep==1 & lead(PatientNr)==PatientNr & lead(groep)!=groep & lead(OpeningsDatumDBC)<=SluitingsDatumDBC,1,0)) %>%
  mutate(check = ifelse(max(row_number())-row_number()==0,0,check)) %>%
  mutate(SluitingsDatumDBC = if_else(check==1,lead(OpeningsDatumDBC)-1,SluitingsDatumDBC)) %>%
  mutate(SluitingsDatumDBC = as.Date(SluitingsDatumDBC,origin="1970-01-01"))

  # LG: toegevoegd C9 o.b.v. transplantatielijsten ipv dbc 076/078

nierfalen1 <- nierfalen1 %>%    
  mutate(SluitingsDatumDBC = ifelse(Tx_datum<SluitingsDatumDBC & !is.na(Tx_datum), Tx_datum - 1, SluitingsDatumDBC))%>%
  mutate(SluitingsDatumDBC = as.Date(SluitingsDatumDBC,origin="1970-01-01"))%>%
  filter(!(OpeningsDatumDBC>SluitingsDatumDBC)) %>%
  select(-check)


#Situatie 2: Een andere behandeling was al gestart op het moment dat nierfalen start. Verwijder nierfalen traject

nierfalen1 <- nierfalen1 %>%
  mutate(tmpcheck = ifelse(groep==1 & lag(PatientNr)==PatientNr & lag(groep)!=groep & lag(OpeningsDatumDBC)<=OpeningsDatumDBC,1,0)) %>%
  mutate(tmpcheck = ifelse(row_number()==1,0,tmpcheck))

temp <- nierfalen1[,c("PatientNr","tmpcheck")]

temp <- temp %>%
  group_by(PatientNr) %>%
  summarise(check = sum(tmpcheck)) %>%
  mutate(check = as.numeric(check>0))
temp <- data.frame(temp)

nierfalen1 <- nierfalen1 %>%
  left_join(temp,by="PatientNr") %>%
  filter(check==0) %>%
  select(-tmpcheck,-check)

rm(temp)


### Gooi de andere groepen eruit

nierfalen1 <- nierfalen1 %>%
  filter(groep==1)


### Selecteer het eerste traject

nierfalen1 <- nierfalen1 %>%
  arrange(PatientNr,OpeningsDatumDBC) %>%
  group_by(PatientNr) %>%
  mutate(seq = row_number()) %>%
  filter(seq == 1) %>%
  select(-seq)

# final selectie
nierfalen <- data.frame(nierfalen1)%>%
  filter(OpeningsDatumDBC<=stopwindow & SluitingsDatumDBC>=startwindow)


###########
# CASEMIX
##########

table(nierfalen$Geslacht)

table(nierfalen$Leeftijdsgroep)

# mediane leeftijd

MSZ<- left_join(nierfalen, Patient)%>%
  mutate(Leeftijd=as.numeric(Leeftijd))
summary(MSZ$Leeftijd)

#########################
# SCOREKAART INDICATOREN#
########################

# K1 klinische opnames

# koppel opnames aan verrichtingen (in MSZ worden de opnamenummers niet goed geregistreerd bij Verrichtingen)
temp <-  left_join(Verrichtingen, Opnames)%>%
  filter(ZACode=="190021" & VerrichtingDatum>=OpnameDatum-1 & VerrichtingDatum<=OntslagDatum)%>%
  arrange(PatientNr, OpnameDatum) %>% 
  distinct(PatientNr, OpnameDatum,.keep_all=TRUE)

#Verrichtingen1 <- left_join(Verrichtingen, temp, by=c("PatientNr", "VerrichtingDatum", "ZACode"))


# koppelen aan nierfalen groep
K11 <- left_join(nierfalen, temp)%>%
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
noemer_K1_K3 <- nierfalen %>%
  mutate(OpeningsDatumDBC= ifelse(OpeningsDatumDBC<startwindow, startwindow, OpeningsDatumDBC), 
         SluitingsDatumDBC = ifelse(SluitingsDatumDBC>stopwindow, stopwindow, SluitingsDatumDBC))%>%
  mutate(OpeningsDatumDBC=as.Date(OpeningsDatumDBC,origin="1970-01-01"),
         SluitingsDatumDBC=as.Date(SluitingsDatumDBC,origin="1970-01-01"))%>%
  mutate(FU = difftime(SluitingsDatumDBC, OpeningsDatumDBC, units = "days")+1) %>%
  mutate(FU = as.numeric(FU/365))%>%
  group_by(groep)%>%
  summarize(FU_sum=sum(FU))



### K31a fysieke poli
   # excl poli's gekoppeld aan seh 
   # binnen timewindow
  
  poli <- left_join(nierfalen, Verrichtingen, by="PatientNr")
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
    select(PatientNr, VerrichtingDatum, Aantal, Specialisme, AGB_CodeUitvoerder)%>%
    mutate(poli_tele = 1) # 1 = fysiek
  
  K31a <- fysiek %>%
    group_by(PatientNr, VerrichtingDatum, AGB_CodeUitvoerder)%>%
    summarise(Aantal=sum(Aantal))%>%
    mutate(Aantal=ifelse(Aantal>1,1,Aantal))
  
  sum(K31a$Aantal)
  
  # totaal tele
  tele <- left_join(nierfalen, Verrichtingen)%>%
    filter(ZACode=="190019"| ZACode=="190025"| ZACode=="190162"|ZACode=="190161"| ZACode=="190164"| 
             ZACode=="190165"| ZACode=="198203"| ZACode=="198204"| ZACode=="190163"| ZACode=="190166"| ZACode=="190167")%>%
    filter(VerrichtingDatum>=OpeningsDatumDBC & VerrichtingDatum<=SluitingsDatumDBC)%>% # binnen individuele timewindow
    select(PatientNr, VerrichtingDatum, Aantal, Specialisme, AGB_CodeUitvoerder)%>%
    mutate(poli_tele = 2) # 2 = teleconsult
  
    
    K311<- tele %>%
    group_by(PatientNr, VerrichtingDatum, AGB_CodeUitvoerder)%>%
    summarise(Aantal=sum(Aantal))%>%
    mutate(Aantal=ifelse(Aantal>1,1,Aantal))
  
  sum(K311$Aantal)
  


# NEFROLOOG
# K32 nefroloog 
  # koppel agenda nefroloog aan verrichtingen en behoudt teleconsulten + fysieke polibezoeken
poli_tele <- rbind(fysiek, tele)%>%
  mutate(Aantal=ifelse(Aantal>1,1,Aantal))

K32 <- left_join(Agenda, poli_tele)%>%
  filter(Specialisme=="INT" & VerrichtingDatum==AfspraakDatum)%>%
  distinct(PatientNr, poli_tele, VerrichtingDatum,.keep_all=TRUE)

# K321 fysiek nefroloog
K32a <- K32%>%
  filter(poli_tele==1)
sum(K32a$Aantal)
  
  # K322 teleconsult nefroloog
  K321 <- K32%>%
    filter(poli_tele==2)
  sum(K321$Aantal)


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
  # timewindow vanaf april 2019, zodat je minimaal 3 maanden terug in de tijd kunt kijken of er een nierfalen dbc is
start_window_dialyse = as.Date("2019-04-01")   

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
  summarise(OpeningsDatumDBC =first(OpeningsDatumDBC), ZorgType=first(ZorgType)) #behoudt oudste datum DBC 324 of 325


# koppelen eerste 324/325 DBC aan nieuwe dialyse
dialyse_3mnd <- left_join(dialyse_nieuw, eerste_NF_DBC, by="PatientNr")

# behoudt juiste tijdsperiode
dialyse_3mnd <- dialyse_3mnd %>%
  mutate(dagen_verschil = as.numeric(VerrichtingDatum - OpeningsDatumDBC))

# neem alleen nierfalen patiënten mee
dialyse_3mnd <- left_join(nierfalen,dialyse_3mnd,by="PatientNr")%>%
  filter(!is.na(VerrichtingDatum))

# Koppel met agenda nefroloog om missings en negatieve uitkomsten in 'dagen_verschil' mogelijk aan te vullen
  # check alle patiënten in de teller handmatig en deel deze in juiste subindicator in (P1.2.1-P1.2.4)
  Agenda1 <- Agenda%>%
    group_by(PatientNr)%>%
    summarise(AfspraakDatum = min(AfspraakDatum))


# koppel bestanden

dialyse_3mnd_2 <- left_join(dialyse_3mnd,Agenda1, by="PatientNr")%>%
  mutate(dagen_verschil = ifelse(is.na(dagen_verschil) | dagen_verschil<90, VerrichtingDatum- AfspraakDatum, dagen_verschil))%>%
  filter(dagen_verschil <=90 | is.na(dagen_verschil)) # check resterende handmatig: overname ander ziekenhuis / langer geleden bij nefroloog geweest / neit bij nefroloo geweest




# P7 GFR bij verwijzing NF

GFR <- Lab %>%
  filter(BepalingOms=="GFR (CKD-EPI)" | BepalingOms=="GFR-EPI")%>%  # kijk hoe GFR waardes in eigen Lab staan omschreven, dit ivm lokale codes voor GFR labwaardes
  select(-BepalingOms)

save(GFR, file="GFR.RData")


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


# GFR koppelen aan nieuwe DBC codes
eerste_NF_DBC <- eerste_NF_DBC%>%
  filter(ZorgType=="11")%>%
  group_by(PatientNr)%>%
  summarise(OpeningsDatumDBC=min(OpeningsDatumDBC))%>%
  filter(OpeningsDatumDBC>=startwindow & OpeningsDatumDBC<=stopwindow) # binnen timewindow

# behoudt eerst gemeten GFR na openen dbc 324/325 (kijken vanaf een maand voor dbc diagnose)
GFR_eerste_DBC <- left_join(eerste_NF_DBC, GFR1, by="PatientNr")%>%
  mutate(verschil_datum_GFR_DBC = as.numeric(Datum - OpeningsDatumDBC))

# kijken naar GFR vanaf 30 dagen voor start DBC
GFR_eerste_DBC <- GFR_eerste_DBC%>%
  filter(verschil_datum_GFR_DBC >=-30 & verschil_datum_GFR_DBC<=90) %>%
  group_by(PatientNr)%>%
  arrange(PatientNr, Datum)%>%
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


