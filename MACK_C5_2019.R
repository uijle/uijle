# ------------- MACK CYCLUS 5 ---------------- #
# VBHC traject:  MACK
# Cyclus:        5

# Script versie: 1
# Datum:         04-2023
# Auteur:        Esmée den Uijl
# Encoding:      UTF-8

########## WORKING DIRECTIORY, PACKAGES EN SETTINGS ########## 

rm(list=ls())                                                      # Remove/empty workspace
setwd("/home/afdelingen/kwaliteit.en.veiligheid/MACK/Cyclus 5/Data/")   # Define working directory

library(dplyr)        # For using piping operator 
library(lubridate)    # Voor easy date manipulation
library(tidyr)
library(openxlsx)

startwindow  <- as.Date("2020-01-01", format ="%Y-%m-%d")
stopwindow   <- as.Date("2021-03-31", format ="%Y-%m-%d")
Cyclus_nr    <- 5
Cyclus_jaar  <- 2020


CTG_verrichtingen <- c("190157", "190158")

########## INLEZEN VOEDSELPROVOCATIE DATA EN PATIENTSELECTIE ########## 

#alle patienten in 2020
pat_nrs    <- read.csv("2304 0916 VBHC MACK Agenda afspraken.csv",header=T, sep=";", dec=",", stringsAsFactors = F, na.strings="", quote="", fileEncoding = "latin1") %>%
  mutate(PatientNr = as.character(patientnr),
         datum = as.Date(datum, format = "%d-%m-%Y")) %>%
  filter(datum >= "2020-01-01" & datum <= "2020-12-31" &
           code %in% c("APNW", "PPC", "NP-19", "NPOLIB40")) %>% # 505 unieke patienten 
  distinct(PatientNr, code)
table(pat_nrs$code) #alle patienten met APNW of PPC in 2020
length(unique(pat_nrs$PatientNr))

vp_raw1 <- read.csv("Provocatietest MACK 20230413.csv",header=T, sep=";", dec=",", stringsAsFactors = F, na.strings="", quote="", fileEncoding = "latin1")

vp_raw <- vp_raw1 %>% filter(STELLING %in% c("provocatie open", "Provocatie (open) nw", "provocatie ei", "provocatie melk",
                                             "provocatie overig", "provocatie pinda", "provocatie tarwe", "Provocatie (oud)", "Provocatie",
                                             "Conclusie (totaal)", "Indien advies anders", "Indien conclusie anders", "Toelichting conclusie", "memo overig", "overig voedingsmiddelenlijst")) %>%
  rename(PatientNr = PAT_CODE)

#Bereken de datums apart want anders dubbelingen en dan wordt de verkeerde datum meegenomen (zie DA logboek)
vp_raw_datum <- vp_raw1 %>% filter(STELLING %in% c("Datum", "Datum provocatie", "Datum provocatie dag 1", "Datum provocatie dag 2")) %>%
  rename(PatientNr = PAT_CODE) %>%
  filter(!(STELLING == "Datum" & ANTWOORD <= "2021-07-01")) %>% #Want verandering van Datum provocatie naar Datum!
  mutate(ANTWOORD = as.Date(ANTWOORD, format = "%d-%m-%Y")) %>%
  arrange(PatientNr, Datum_Antwoord, STELLING, desc(ANTWOORD)) %>%
  distinct(PatientNr, geboortedatum, Vraagid, Datum_Lijst, Datum_Antwoord, STELLING, .keep_all = TRUE) %>%
  mutate(ANTWOORD = as.character(ANTWOORD))

vp_raw <- rbind(vp_raw, vp_raw_datum) %>% 
  select(-c(Vraagid, OPSLAGID, Datum_Antwoord, Tijd_Antwoord)) %>%
  mutate(verwijderen = ifelse(STELLING == "Provocatie" & ANTWOORD == "dubbelblind", 1, 0)) %>%
  filter(verwijderen == "0") %>%  #toegevoegd want anders is onbekend op welke allergie wordt getest (er staat dan alleen dubbelblind)
  group_by(PatientNr, STELLING) %>%
  distinct(PatientNr, Datum_Lijst, .keep_all = TRUE) %>%
  tidyr::pivot_wider(names_from = STELLING, values_from = ANTWOORD) %>%
  ungroup()


#vp_raw_geendatum <- vp_raw %>% group_by(PATIENTNR) %>%
#  filter(is.na(`Datum provocatie`) & is.na(`Datum provocatie dag 1`) & is.na(`Datum provocatie dag 2`)) %>%
#  ungroup()
#length(unique(vp_raw_geendatum$PATIENTNR))


vp <- vp_raw %>% mutate(PatientNr = as.character(PatientNr),
                        Geboortedatum = as.Date(geboortedatum, format="%d-%m-%Y"),
                        #Datum = as.Date(Datum,format="%d-%m-%Y"),
                        Datum.provocatie = ifelse(!is.na(`Datum provocatie`), `Datum provocatie`, Datum),
                        Datum.provocatie = as.Date(Datum.provocatie, format="%Y-%m-%d"),
                        Provocatie.dag.1 = as.Date(`Datum provocatie dag 1`, format="%Y-%m-%d"),
                        Provocatie.dag.2 = as.Date(`Datum provocatie dag 2`,format="%Y-%m-%d"),
                        
                        #Provocatie open en blind goed registreren
                        Provocatie_blind = ifelse(!(Provocatie %in% c("open")), Provocatie, NA),
                        Provocatie       = ifelse((Provocatie != "open" & !is.na(Provocatie)), "dubbelblind", Provocatie),
                        Provocatie       = as.character(ifelse(!is.na(`Provocatie (oud)`), `Provocatie (oud)`, Provocatie)),
                        #Provocatie.oud = as.character(`Provocatie (oud)`),
                        
                        provocatie.open = as.character(ifelse(!is.na(`Provocatie (open) nw`), `Provocatie (open) nw`, `provocatie open`)),
                        #Provocatie.open.nw = as.character(`Provocatie (open) nw`),
                        Open_overig      = ifelse(!is.na(`memo overig`), `memo overig`, `overig voedingsmiddelenlijst`),
                        provocatie.ei = as.character(`provocatie ei`),
                        provocatie.melk = as.character(`provocatie melk`),
                        provocatie.pinda = as.character(`provocatie pinda`),
                        provocatie.tarwe = as.character(`provocatie tarwe`),
                        provocatie.overig = as.character(`provocatie overig`),
                        #memo.overig = as.character(`memo overig`),
                        Conclusie = as.character(`Conclusie (totaal)`),
                        Indien_conclusie_anders = as.character(ifelse(!is.na(`Indien conclusie anders`), `Indien conclusie anders`, `Indien advies anders`)),
                        Toelichting.conclusie = as.character(`Toelichting conclusie`)) %>%
  mutate(PatientNr = gsub("^0000", "", PatientNr), #voorloopnullen verwijderen
         PatientNr = gsub("^000", "", PatientNr),
         PatientNr = gsub("^00", "", PatientNr),
         PatientNr = gsub("^0", "", PatientNr)) %>%
  mutate(Datum.provocatie = if_else(Provocatie == "dubbelblind", as.Date(NA, "%d-%m-%Y"), Datum.provocatie)) %>%  # as.Date is nodig
  filter((Datum.provocatie > startwindow & Datum.provocatie < stopwindow) | #Datum.provocatie of Datum, dit is vanaf juli 2021 veranderd lijkt het
           (Provocatie.dag.1 > startwindow & Provocatie.dag.1 < stopwindow))  %>%
  filter(PatientNr %in% pat_nrs$PatientNr) %>%
  select(PatientNr, Geboortedatum, Datum.provocatie, Provocatie.dag.1, Provocatie.dag.2, Provocatie, Provocatie_blind,
         provocatie.open, Open_overig, provocatie.ei, provocatie.melk, provocatie.pinda,
         provocatie.tarwe, provocatie.overig, Conclusie, Indien_conclusie_anders, Toelichting.conclusie)

length(unique(pat_nrs$PatientNr)) #n= 734 pt met APNW, PPC of NP-19 code.
length(unique(vp$PatientNr)) # n=291 met 457 voedselprovocatietesten


# patient selectie
pat_sel <- pat_nrs %>% filter(pat_nrs$PatientNr %in% vp$PatientNr) %>%
  distinct(PatientNr, .keep_all = TRUE)
length(unique(pat_sel$PatientNr)) #n = 291

#write.table(pat_sel$PatientNr, file = "./patientselectie_C5_2020_VP.csv", sep = ";", row.names = F) 

patsel_2019 <- read.csv("patientselectie_C5_2019_VP.csv",header=T, sep=";", dec=",", stringsAsFactors = F, na.strings="", quote="", fileEncoding = "latin1") %>%
  mutate(PatientNr = as.character(X.x.))
Overlap_patienten <- semi_join(pat_sel, patsel_2019, by = "PatientNr")
pat_sel <- pat_sel %>% filter(!(PatientNr %in% Overlap_patienten$PatientNr))

rm(Overlap_patienten)

########## OVERIGE DATA INLEZEN ########## 

# !gebruik bestand dat in regel 84 is aangemaakt om DBC_OPNAME_OK en AGENDA data uit de selfservice te halen
# data DBC_OPNAME_OK
data_epd    <- read.csv("MACK_C5_DBC_OPNAME_OK_SV.csv",header=T, sep=";", dec=",", stringsAsFactors = F, na.strings="") %>%
  mutate(PatientNr = as.character(PATNR)) %>%
  mutate(DATUM_VERRICHTING = as.Date(DATUM_VERRICHTING,format="%d-%m-%Y")) %>%
  filter(DATUM_VERRICHTING >= startwindow & DATUM_VERRICHTING <= stopwindow) %>%
  filter(PatientNr %in% pat_sel$PatientNr)

# data agenda afspraken
data_agenda <- read.csv("MACK_C5_AGENDA_AFSPRAKEN_SV.csv",header=T, sep=";", dec=",", stringsAsFactors = F, na.strings="") %>%
  mutate(PatientNr = as.character(PATNR),
         AFSPRAAK_DATUM = as.Date(AFSPRAAK_DATUM,format="%d-%m-%Y"),
         EERSTE_INVOER_DATUM = as.Date(EERSTE_INVOER_DATUM,format="%d-%m-%Y")) %>%
  filter(AFSPRAAK_DATUM >= startwindow & AFSPRAAK_DATUM <= stopwindow)
length(unique(data_agenda$PatientNr))

# Aanmaken startdatum follow-up = datum 1e APNW, 1e PPC, 1e NPOLIB40 of 1e NP-19
startdat <- data_agenda %>% filter(CODE=="APNW" | CODE=="PPC" | CODE == "NPOLIB40" | CODE == "NP-19") %>% 
  arrange(PatientNr,AFSPRAAK_DATUM) %>% filter(!duplicated(PatientNr)) %>%
  rename(startdat = AFSPRAAK_DATUM)

########## BEWERKEN PROVOCATIEDATA ########## 
#als een pt 2x een open provocatie heeft gehad dan gaat er 1 weg met onderstaande, niet de bedoeling denk ik!
#vp <- vp %>% 
#  distinct(PatientNr, Datum.provocatie, Provocatie.dag.1, Provocatie.dag.2, .keep_all = TRUE)


  
#overbodig? als datum.provocatie niet wordt gebruikt.
#vp_test <- vp %>%
#  mutate(Datum.provocatie=ifelse(is.na(Datum.provocatie),Datum.provocatie.1,
#                                 ifelse(is.na(Datum.provocatie) & is.na(Datum.provocatie.1), Datum.provocatie.2, Datum.provocatie))) %>%
#  mutate(Datum.provocatie=as.Date(Datum.provocatie,origin="1970-01-01")) %>%
#  arrange(PatientNr,Datum.provocatie)

#vp <- vp %>% filter(Datum>="2019-01-01" & Datum <= "2020-03-31")


# definieren provocatie allergeen
#vp <-  vp %>%
#  mutate(open=if_else(Provocatie.oud =="open",1,0),
#         dubbelblind=if_else(Provocatie.oud =="dubbelblind",1,0),
#         open_amandel=if_else(provocatie.open=="amandel", 1, 0, missing=NULL),
#         open_cashewnoot=if_else(provocatie.open=="cashewnoot", 1, 0, missing=NULL),
#         open_ei=if_else(provocatie.open=="ei", 1, 0, missing=NULL),
#         open_hazelnoot=if_else(provocatie.open=="hazelnoot", 1, 0, missing=NULL),
#         open_melk=if_else(provocatie.open=="melk", 1, 0, missing=NULL),
#         open_overig=if_else(provocatie.open=="overig", 1, 0, missing=NULL),
#         open_pinda=if_else(provocatie.open=="pinda", 1, 0, missing=NULL),
#         open_pistache=if_else(provocatie.open=="pistache", 1, 0, missing=NULL),
#         open_schaaldieren=if_else(provocatie.open=="schaaldieren", 1, 0, missing=NULL),
#         open_soja=if_else(provocatie.open=="soja", 1, 0, missing=NULL),
#         open_tarwe=if_else(provocatie.open=="tarwe", 1, 0, missing=NULL),
#         open_vis=if_else(provocatie.open=="vis", 1, 0, missing=NULL),
#         open_walnoot=if_else(provocatie.open=="walnoot", 1, 0, missing=NULL),
#         blind_ei=ifelse(grepl("STATE=Y", provocatie.ei, ignore.case = TRUE), 1,0),
#         blind_melk=ifelse(grepl("STATE=Y", provocatie.melk, ignore.case = TRUE), 1,0),
#         blind_pinda=ifelse(grepl("STATE=Y", provocatie.pinda, ignore.case = TRUE), 1,0))

vp <-  vp %>%
  mutate(open=if_else(Provocatie =="open",1,0),
         dubbelblind=if_else(Provocatie =="dubbelblind",1,0),
         open_amandel=if_else(provocatie.open=="amandel", 1, 0, missing=NULL),
         open_cashewnoot=if_else(provocatie.open=="cashewnoot", 1, 0, missing=NULL),
         open_ei=if_else(provocatie.open=="ei", 1, 0, missing=NULL),
         open_hazelnoot=if_else(provocatie.open=="hazelnoot", 1, 0, missing=NULL),
         open_melk=if_else(provocatie.open=="melk", 1, 0, missing=NULL),
         open_overig=if_else(provocatie.open=="overig", 1, 0, missing=NULL),
         open_pinda=if_else(provocatie.open=="pinda", 1, 0, missing=NULL),
         open_pistache=if_else(provocatie.open=="pistache", 1, 0, missing=NULL),
         open_schaaldieren=if_else(provocatie.open=="schaaldieren", 1, 0, missing=NULL),
         open_soja=if_else(provocatie.open=="soja", 1, 0, missing=NULL),
         open_tarwe=if_else(provocatie.open=="tarwe", 1, 0, missing=NULL),
         open_vis=if_else(provocatie.open=="vis", 1, 0, missing=NULL),
         open_walnoot=if_else(provocatie.open=="walnoot", 1, 0, missing=NULL),
         blind_ei= ifelse((grepl("STATE=Y", provocatie.ei, ignore.case = TRUE) | Provocatie_blind == "ei"), 1,0),
         blind_melk=ifelse((grepl("STATE=Y", provocatie.melk, ignore.case = TRUE) | Provocatie_blind == "melk"), 1,0),
         blind_pinda=ifelse((grepl("STATE=Y", provocatie.pinda, ignore.case = TRUE) | Provocatie_blind == "pinda"), 1,0),
         blind_tarwe=ifelse((grepl("STATE=Y", provocatie.tarwe, ignore.case = TRUE) | Provocatie_blind == "tarwe"), 1,0))

#table(vp$memo.overig)  #check de overige open provocaties (deze vallen allemaal onder "overig" en worden niet apart genoteerd)
table(vp$Open_overig)
table(vp$provocatie.overig) #op oude manier #check welke dubbelblinde provocaties er zijn gedaan naast ei, melk, pinda en tarwe
table(vp$Provocatie_blind)

#1 patient heeft is.na(provocatie.oud) maar wel provocatie.open ingevuld.
#vp <- vp %>% mutate(Provocatie.oud = ifelse((is.na(Provocatie.oud) & !is.na(provocatie.open)), "open", Provocatie.oud))

## definieren provocatie overig
vp <-  vp %>%
  mutate(blind_hazelnoot = ifelse((grepl(tolower("hazelnoot"), provocatie.overig, ignore.case=T) | grepl("hazrlnoot", provocatie.overig, ignore.case=T) |
                                          Provocatie_blind == "hazelnoot"), 1, 0),
         blind_cashew    = ifelse((grepl("cashew", provocatie.overig, ignore.case=T)| grepl("Cashew", provocatie.overig, ignore.case=T) |
                                     Provocatie_blind == "cashewnoot"), 1, 0),
         blind_walnoot   = ifelse((grepl("walnoot", provocatie.overig, ignore.case=T) |
                                          Provocatie_blind == "walnoot"), 1, 0),
         blind_sesam     = ifelse((grepl("sesam", provocatie.overig, ignore.case=T) |
                                    Provocatie_blind == "sesam"), 1, 0),
         blind_amandel   = ifelse((grepl("amandel", provocatie.overig, ignore.case=T) | grepl("Amandel", provocatie.overig, ignore.case=T) |
                                    Provocatie_blind == "amandel"), 1, 0)) %>%
  mutate_at(vars(open_amandel:blind_amandel),  replace_na, 0) %>% #NA aanpassen naar 0 zodat er met de variabelen gerekend kan worden
  mutate(blind_overig    = ifelse(dubbelblind==1 & blind_ei==0 & blind_melk==0 & blind_pinda==0 & blind_sesam==0 & blind_hazelnoot==0 & blind_cashew==0 & blind_walnoot==0 & blind_amandel==0, 1, 0))

#table(vp$blind_cashew)
#table(vp$Provocatie_blind)

table(vp$blind_overig) #check of er nog allergenen zijn die niet gedefinieerd zijn

table(vp$Conclusie) # andere conclusies getrokken

#NOG SAMENVOEGEN MET DE NIET ANDERS CONCLUSIES?
#conc_anders_zonderNA <- vp %>% filter(Conclusie == "anders") %>%
#  mutate(Conclusie = ifelse(grepl("positief", Indien_conclusie_anders) | grepl("Positief", Indien_conclusie_anders) |grepl("positef", Indien_conclusie_anders) |
#                              grepl("postief", Indien_conclusie_anders) | grepl("postitef", Indien_conclusie_anders), "positief", NA),
#         Conclusie = ifelse(grepl("negatief", Indien_conclusie_anders) | grepl("neagtief", Indien_conclusie_anders), "negatief", Conclusie),
#         Conclusie = ifelse(grepl("dubieus", Indien_conclusie_anders), "dubieus", Conclusie),
#         Conclusie = ifelse(grepl("kan niet getrokken worden", Indien_conclusie_anders), "kan niet getrokken worden", Conclusie))
#%>%
#mutate(Conclusie = ifelse(Conclusie == "anders", NA, Conclusie))


#Conclusie provocatie
#conc_provocatie <-  bron %>%
#  select(Aandoening, Cyclus, Groep, Wegingsfactor, PatientNr, Datum.provocatie,conclusie) %>%
#  full_join(conc_anders_zonderNA) %>%
#  mutate(conclusie_incl_anders = ifelse((conclusie=="anders" & !is.na(conclusie_anders)), conclusie_anders, conclusie), #neem alleen de conclusie_anders als er bij conclusie ook anders staat
#         conclusie_incl_anders = ifelse(conclusie == "dubieus positief/negatief", "dubieus", conclusie_incl_anders))  


# HIER GEBLEVEN 19-04-2023
  
#U1 uitgesplitst naar open, dubbelblind en dubbelblind zonder melk
conc_zeker_open <- vp %>% filter(Provocatie == "open") %>%
  mutate(conc_zeker = ifelse(Conclusie %in% c("negatief", "positief"), 1, 0))
table(conc_zeker_open$conc_zeker) 

conc_zeker_blind <- vp %>% filter(Provocatie == "dubbelblind") %>%
  mutate(conc_zeker = ifelse(Conclusie %in% c("negatief", "positief"), 1, 0))
table(conc_zeker_blind$conc_zeker)

conc_zeker_blind_minmelk <- vp %>% filter(Provocatie == "dubbelblind" & blind_melk == 0) %>%
  mutate(conc_zeker = ifelse(Conclusie %in% c("negatief", "positief"), 1, 0)) 
table(conc_zeker_blind_minmelk$conc_zeker) 


########## BRONBESTAND MAKEN ########## 
vp_selectie <- vp %>% 
  select(PatientNr, Datum.provocatie, Provocatie.dag.1, Provocatie.dag.2, Geboortedatum, Provocatie, open, dubbelblind, open_amandel, open_cashewnoot, 
         open_ei, open_hazelnoot, open_melk, open_overig, open_pinda, open_pistache, open_schaaldieren, open_soja, open_tarwe,
         open_vis, open_walnoot, blind_ei, blind_melk, blind_pinda, blind_tarwe, blind_hazelnoot, blind_walnoot, blind_cashew,
         blind_amandel, blind_sesam, blind_overig, conclusie = Conclusie) #conc_zeker_open, conc_zeker_blind, code

Patient     <- data_epd %>% select(PatientNr, GeboorteDatum = GEB_DAT, Geslacht = GESLACHT, ses = SES_SCORE) %>%
  mutate(GeboorteDatum = as.Date(GeboorteDatum, format='%d-%m-%Y'), ses = as.numeric(ses)) %>%
  distinct(PatientNr, .keep_all=T) %>%
  filter(PatientNr %in% pat_sel$PatientNr)

Afspraak    <- startdat %>% select(PatientNr, startdat, EERSTE_INVOER_DATUM) %>%
  filter(PatientNr %in% pat_sel$PatientNr)

Patient = merge(x=Patient,y=Afspraak,by="PatientNr",all.y=TRUE) %>% 
  distinct() 

bron =  merge(x=vp_selectie,y=Patient,by="PatientNr",all.y=TRUE) 

bron <- bron %>% 
  mutate(SES_groep=cut(ses, breaks=c(-4, -0.51, 0.5, 4), labels=c("< -0.5","-0.5 - 0.5","> 0.5"), include.lowest=T),
         leeftijd=round(as.numeric(difftime(startdat,GeboorteDatum,units="days")/365.25),digits=1),
         lft_groep=cut(leeftijd, breaks=c(0, 1, 4, 12, 18, 25), labels=c("0-1","1-4 jaar","4-12 jaar","12-18 jaar", ">18 jaar"), include.lowest=T))


########## CASE/BEHANDELMIX juiste format voor toeschrijven excel ########## 
bron <- bron %>% mutate(Aandoening = "MACK",
                        Cyclus = Cyclus_jaar,
                        Groep = "Voedselprovocatie",
                        Wegingsfactor = 1)
#Casemix
casemix <- bron %>%
  select(Aandoening, Cyclus, Wegingsfactor, Groep, PatientNr, lft_groep, Geslacht, SES_groep) %>% #code
  group_by(PatientNr) %>% 
  mutate(seqn = row_number()) %>%
  filter(seqn==1) %>%
  ungroup(PatientNr)

summary(casemix$lft_groep)
table(casemix$Geslacht)
summary(casemix$SES_groep)

lft                <- casemix %>% mutate( Ind    = "lft",
                                          Waarde = ifelse( lft_groep == "0-1", 1, NA),
                                          Waarde = ifelse( lft_groep == "1-4 jaar", 2, Waarde),
                                          Waarde = ifelse( lft_groep == "4-12 jaar", 3, Waarde),
                                          Waarde = ifelse( lft_groep == "12-18 jaar", 4, Waarde), 
                                          Waarde = ifelse( lft_groep == ">18 jaar", 5, Waarde)) %>%
  select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor) 

leeftijd           <- casemix %>% mutate(Ind    = "leeftijd",
                                         Waarde =  lft_groep)  %>%
  select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor)

gsl                <- casemix %>% mutate( Ind    = "gsl",
                                          Waarde = ifelse( Geslacht == "M", 1, NA),
                                          Waarde = ifelse( Geslacht == "V", 2, Waarde),
                                          Waarde = ifelse(is.na(Geslacht), 99, Waarde)) %>%
  select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor) 

ses                <- casemix %>% mutate( Ind    = "ses",
                                          Waarde = ifelse( SES_groep == "< -0.5", 1, NA),
                                          Waarde = ifelse( SES_groep == "-0.5 - 0.5", 2, Waarde),
                                          Waarde = ifelse( SES_groep == "> 0.5", 3, Waarde),
                                          Waarde = ifelse(is.na(SES_groep), 99, Waarde)) %>%
  select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor) 

case_mix <- rbind(lft, gsl, ses)

#o_pinda              <- bron %>%  mutate(Ind     = "o_pinda",
#                                         Waarde  = ifelse( open_pinda == 1 & !is.na(open_pinda), 1, 0)) %>% 
#  select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor)

#provocaties in juiste format voor toeschrijven excel
provocaties <- function(indicator, column_name){
  aantal_provocaties              <- bron %>%  mutate(Ind     = indicator,
                                                      Waarde  = ifelse( column_name == 1 & !is.na(column_name), 1, 0)) %>% 
    select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor)
  return(aantal_provocaties)
}

o_pinda <- provocaties("o_pinda", bron$open_pinda)
o_hazelnoot <- provocaties("o_hazelnoot", bron$open_hazelnoot)
o_walnoot <- provocaties("o_walnoot", bron$open_walnoot)
o_cashewnoot <- provocaties("o_cashewnoot", bron$open_cashewnoot)
o_ei <- provocaties("o_ei", bron$open_ei)
o_melk <- provocaties("o_melk", bron$open_melk)
o_amandel <- provocaties("o_amandel", bron$open_amandel)
o_pistache <- provocaties("o_pistache", bron$open_pistache)
o_soja <- provocaties("o_soja", bron$open_soja)
o_vis <- provocaties("o_vis", bron$open_vis)
o_tarwe <- provocaties("o_tarwe", bron$open_tarwe)
o_schaaldieren <- provocaties("o_schaaldieren", bron$open_schaaldieren)
o_overig <- provocaties("o_overig", bron$open_overig)

b_pinda <- provocaties("b_pinda", bron$blind_pinda)
b_hazelnoot <- provocaties("b_hazelnoot", bron$blind_hazelnoot)
b_walnoot <- provocaties("b_walnoot", bron$blind_walnoot)
b_cashewnoot <- provocaties("b_cashewnoot", bron$blind_cashew)
b_ei <- provocaties("b_ei", bron$blind_ei)
b_melk <- provocaties("b_melk", bron$blind_melk)
b_amandel <- provocaties("b_amandel", bron$blind_amandel)
b_sesam <- provocaties("b_sesam", bron$blind_sesam)
b_tarwe <- provocaties("b_tarwe", bron$blind_sesam)
b_overig <- provocaties("b_overig", bron$blind_overig)

#Totaal aantal vp (open en blind)
o_aantal              <- bron %>%  mutate(Ind     = "o_aantal",
                                          Waarde  = ifelse( Provocatie == "open", 1, 0)) %>%
  select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor)
table(o_aantal$Waarde) #aantal open provocaties

b_aantal              <- bron %>%  mutate(Ind     = "b_aantal",
                                          Waarde  = ifelse( Provocatie == "dubbelblind", 1, 0)) %>%
  select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor)
table(b_aantal$Waarde) #aantal dubbelblinde provocaties

aantal_vp             <- bron %>%  mutate(Ind     = "aantal_vp",
                                          Waarde  = ifelse( Provocatie %in% c("dubbelblind", "open"), 1, 0)) %>%
  select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor)
table(aantal_vp$Waarde) #totaal aantal provocaties

#Multiple voedselallergie
multiple_allergie <-  bron %>%
  select(PatientNr, Datum.provocatie, conclusie) %>%
  #full_join(conc_anders_zonderNA) %>%
  mutate(conclusie_positief = ifelse((conclusie=="positief"), 1, 0)) %>% #| (conclusie == "anders" & conclusie_anders=="positief")
  filter(conclusie_positief==1) %>%
  group_by(PatientNr) %>% 
  mutate(seqn = row_number())%>%
  filter(seqn > 1) %>%
  ungroup(PatientNr) %>%
  distinct(PatientNr, .keep_all = TRUE)

mv1 <- multiple_allergie %>% mutate(Aandoening = "MACK",
                                    Cyclus = Cyclus_jaar,
                                    Groep = "Voedselprovocatie",
                                    Wegingsfactor = 1,
                                    Ind = "mv",
                                    Waarde = 1) %>%
  select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor)


#Conclusie provocatie
conc_provocatie <-  bron %>%
  select(Aandoening, Cyclus, Groep, Wegingsfactor, PatientNr, Datum.provocatie,conclusie) #%>%
#full_join(conc_anders_zonderNA) %>%
#mutate(conclusie_incl_anders = ifelse((conclusie=="anders" & !is.na(conclusie_anders)), conclusie_anders, conclusie), #neem alleen de conclusie_anders als er bij conclusie ook anders staat
#       conclusie_incl_anders = ifelse(conclusie == "dubieus positief/negatief", "dubieus", conclusie_incl_anders))  

conclusie_provocatie                <- conc_provocatie %>% mutate( Ind    = "conc",
                                                                   Waarde = ifelse( conclusie == "positief", 1, NA), #Conclusie ipv conclusie_incl_anders
                                                                   Waarde = ifelse( conclusie == "negatief", 2, Waarde),
                                                                   Waarde = ifelse( conclusie == "anders", 3, Waarde), 
                                                                   Waarde = ifelse( conclusie == "dubieus positief/negatief", 4, Waarde), 
                                                                   Waarde = ifelse( conclusie == "kan niet getrokken worden", 5, Waarde),
                                                                   Waarde = ifelse( is.na(conclusie), 99, Waarde)) %>%
  select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor) 
table(conclusie_provocatie$Waarde)

#x <- vp %>%
#  mutate(conc_zeker_open = ifelse(open==1 & (conc_negatief==1 | conc_positief==1),1,0),
#         conc_zeker_blind = ifelse(dubbelblind==1 & (conc_negatief==1 | conc_positief==1),1,0),
#         conc_zeker_blind_minmelk = ifelse(dubbelblind==1 & (conc_negatief==1 | conc_positief==1) & blind_melk==0,1,0)) 


behandel_mix <- rbind(o_pinda, o_hazelnoot, o_walnoot, o_cashewnoot, o_ei, o_melk, o_amandel, o_pistache, o_soja, o_vis, o_tarwe, o_schaaldieren, o_overig, o_aantal,
                      b_pinda, b_hazelnoot, b_walnoot, b_cashewnoot, b_ei, b_melk, b_amandel, b_sesam, b_tarwe, b_overig, b_aantal,
                      aantal_vp, mv1, conclusie_provocatie)




########## SCOREKAART ##########

#--------------------- SCOREKAART UITKOMSTEN ----------------- #
# U1 Patiënten met duidelijkheid na voedselprovocatie
U1 <- conclusie_provocatie %>% mutate(Ind = "U1",
                                      Waarde = ifelse((Waarde == 1 | Waarde == 2), 1, 0)) #positief of negatief
table(U1$Waarde)

#Percentage duidelijkheid bepalen voor 2020:
U1_duidelijkheid <- bron %>% mutate(duidelijkheid = ifelse(conclusie  %in% c("positief", "negatief"), "duidelijkheid", "Geen duidelijkheid")) %>%
  group_by(Provocatie, duidelijkheid) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n/sum(n))*100,1)) %>% #Open: 91.3%   dubbelblind: 73.3%
  ungroup()

U1_duidelijkheid_blind_melk <- bron %>% filter(Provocatie == "dubbelblind") %>%
  mutate(duidelijkheid = ifelse(conclusie  %in% c("positief", "negatief"), "duidelijkheid", "Geen duidelijkheid")) %>%
  group_by(blind_melk, duidelijkheid) %>%
  summarise(n = n()) %>%
  mutate(prop = round((n/sum(n))*100,1)) %>% #dubbelblind zonder blind melk: 81,0%
  ungroup()

# U2 PROMs: Ander script

# U3 IC opnames n.a.v. voedselprovocatietest
U3 <- data_epd %>% 
  mutate(IC_opname = ifelse(CTG_VERRICHTING %in% CTG_verrichtingen, 1, 0)) %>%
  group_by(PatientNr) %>%
  summarise(Waarde = sum(IC_opname,na.rm=T)) %>% # check waarvoor IC opname was
  mutate(Aandoening = "MACK",
         Cyclus = Cyclus_jaar,
         Groep = "Voedselprovocatie",
         Wegingsfactor = 1,
         Ind = "U3") %>%
  select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor)

bron <-  bron %>% left_join(U3[,c("PatientNr","Waarde")], by="PatientNr")
sum(bron$U3) #teller U3

#----------------------- SCOREKAART KOSTEN --------------------- #
#x <- data_agenda %>% filter(AGENDA_NAAM == "KINDERGENEESKUNDE" & grepl("arts", SUBAGENDA_OMS))
#table(x$CODE)

# K1 Polikliniekbezoeken en teleconsulten
kinderarts_poli <- c("APNW", "ACP", "PPC")
kinderarts_tele <- c("HPOLIB20", "TELSPR", "NPOLIB40", "NP-19")
dietist_polibezoeken <- c("ADN", "ADC", "PINDA&EI")
dietist_telebezoeken <- c("TELP&EI", "TEL", "TELDALL", "TELNW", "DIEMAIL")

consulten <- data_agenda %>%
  filter(PatientNr %in% pat_sel$PatientNr) %>%
  mutate(arts_poli = ifelse(CODE %in% kinderarts_poli, 1,0),
         arts_tele = ifelse(CODE %in% kinderarts_tele,1,0),
         dietist_poli = ifelse(CODE %in% dietist_polibezoeken,1,0),
         dietist_tele = ifelse(CODE %in% dietist_telebezoeken,1,0)) %>%
  group_by(PatientNr) %>%
  summarise(arts_poli = sum(arts_poli,na.rm=T), 
            arts_tele = sum(arts_tele,na.rm=T),
            dietist_poli = sum(dietist_poli,na.rm=T),
            dietist_tele = sum(dietist_tele,na.rm=T)) %>%
  ungroup()

x1 <- data_agenda %>% distinct(CODE, CODE_OMS)

x <-  bron %>% left_join(consulten, by="PatientNr") #waarom toevoegen?? --> dubbel geteld voor sommige patienten

sum(consulten$arts_poli)
summary(consulten$arts_poli)
sum(consulten$arts_tele)
summary(consulten$arts_tele)
sum(consulten$dietist_poli)
summary(consulten$dietist_poli)
sum(consulten$dietist_tele)
summary(consulten$dietist_tele)

kosten_in_excel_format <- function(indicator, column){
  kosten <- consulten %>% mutate(Aandoening = "MACK",
                                 Cyclus = Cyclus_jaar,
                                 Groep = "Voedselprovocatie",
                                 Wegingsfactor = 1,
                                 Ind = indicator,
                                 Waarde = column) %>%
    select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor)
  return(kosten)
}

K1.1 <- kosten_in_excel_format("K1.1", consulten$arts_poli)
K1.2 <- kosten_in_excel_format("K1.2", consulten$arts_tele)
K1.3 <- kosten_in_excel_format("K1.3", consulten$dietist_poli)
K1.4 <- kosten_in_excel_format("K1.4", consulten$dietist_tele)


# --------------------- SCOREKAART PROCES ---------------- #
# P1 Doorlooptijd in dagen start behandeling - Verwijzing huisarts:
# EERSTE_INVOER_DATUM als substituut voor verwijzing HA, APNW/PPC datum (startdat) als substituut voor start behandeling
bron$Datum <- as.Date(bron$Datum.provocatie, format = "%d-%m-%Y")
bron <-  bron %>%
  mutate(huisarts_startdat=as.numeric(difftime(startdat, EERSTE_INVOER_DATUM,units="days")),
         Datum = if_else(is.na(Datum), Provocatie.dag.1, Datum), #Datum is een combinatie van open en dubbelblinde provocaties
         startdat_VP=as.numeric(difftime(Datum, startdat,units="days"))) 

bron <-  bron %>%
  group_by(PatientNr) %>% 
  mutate(seqn = row_number())%>%
  mutate(huisarts_startdat=ifelse(seqn>1,NA,huisarts_startdat)) %>%
  mutate(startdat_VP=ifelse(seqn>1,NA,startdat_VP)) %>%
  ungroup(PatientNr)

#melk 0 tot 1 jaar melk vs. de rest (kinderen tot 1 jaar dus niet incl. 1 jaar!)
proces_indicatoren <-  bron %>%
  select(PatientNr, EERSTE_INVOER_DATUM, startdat, Datum, leeftijd,huisarts_startdat,startdat_VP,open_melk,blind_melk) %>%
  mutate(melk0tot1=if_else(leeftijd < 1 & (open_melk >= 1 | blind_melk>=1), 1, 0, missing=0)) %>%  
  mutate(p11=ifelse(melk0tot1==1,huisarts_startdat,NA)) %>%
  mutate(p12=ifelse(melk0tot1==0,huisarts_startdat,NA)) %>%
  mutate(p21=ifelse(melk0tot1==1,startdat_VP,NA)) %>%
  mutate(p22=ifelse(melk0tot1==0,startdat_VP,NA)) %>%
  drop_na("huisarts_startdat")
length(unique(proces_indicatoren$PatientNr)) # check of er 240 patienten overblijven

#noemers p1
table(proces_indicatoren$melk0tot1)

p_indicatoren <- function(noemer,  indicator, column){
  proces <- noemer %>% mutate(Aandoening = "MACK",
                              Cyclus = Cyclus_jaar,
                              Groep = "Voedselprovocatie",
                              Wegingsfactor = 1,
                              Ind = indicator, 
                              Waarde = column) %>%
    select( Aandoening, Cyclus, Groep, PatientNr, Ind, Waarde, Wegingsfactor)
  return(proces)
}

#p1
#sum(proces_indicatoren$huisarts_startdat, na.rm=TRUE)
#summary(proces_indicatoren$huisarts_startdat)
P1 <- p_indicatoren(proces_indicatoren, "P1", proces_indicatoren$huisarts_startdat)
summary(P1$Waarde)
boxplot(P1$Waarde)

#p1.1
#sum(proces_indicatoren$p11, na.rm=TRUE)
#summary(proces_indicatoren$p11)
p11_noemer <- proces_indicatoren %>% filter(!is.na(proces_indicatoren$p11))
P1.1 <- p_indicatoren(p11_noemer, "P1.1", p11_noemer$p11)

#p1.2
#sum(proces_indicatoren$p12, na.rm=TRUE)
#summary(proces_indicatoren$p12)
p12_noemer <- proces_indicatoren %>% filter(!is.na(proces_indicatoren$p12))
P1.2 <- p_indicatoren(p12_noemer, "P1.2", p12_noemer$p12)


# P2 Doorlooptijd in dagen 1e polibezoek - 1e provocatie:
# APNW/PPC datum (startdat) als substituut voor 1e polibezoek

#eerst negatieve doorlooptijden verwijderen
proces_indicatoren_zondernegdoorlooptijden <- proces_indicatoren %>%   #
  filter(startdat_VP>=0)

#check wie neg doorlooptijd heeft en check in HiX waarom:
check_negdoorlooptijd <- proces_indicatoren %>%
  select(PatientNr,huisarts_startdat,startdat_VP,melk0tot1,p21,p22) %>%
  filter(startdat_VP<0)

#noemers p2
table(proces_indicatoren_zondernegdoorlooptijden$melk0tot1)

#p2
#sum(proces_indicatoren_zondernegdoorlooptijden$startdat_VP, na.rm=TRUE)
#summary(proces_indicatoren_zondernegdoorlooptijden$startdat_VP)
P2 <- p_indicatoren(proces_indicatoren_zondernegdoorlooptijden, "P2", proces_indicatoren_zondernegdoorlooptijden$startdat_VP) #sum = 15122

#p2.1
#sum(proces_indicatoren_zondernegdoorlooptijden$p21, na.rm=TRUE)
#summary(proces_indicatoren_zondernegdoorlooptijden$p21)
p21_noemer <- proces_indicatoren_zondernegdoorlooptijden %>% filter(!is.na(proces_indicatoren_zondernegdoorlooptijden$p21))
P2.1 <- p_indicatoren(p21_noemer, "P2.1", p21_noemer$p21) #sum is 3350


#p2.2
#sum(proces_indicatoren_zondernegdoorlooptijden$p22, na.rm=TRUE)
#summary(proces_indicatoren_zondernegdoorlooptijden$p22)
p22_noemer <- proces_indicatoren_zondernegdoorlooptijden %>% filter(!is.na(proces_indicatoren_zondernegdoorlooptijden$p22))
P2.2 <- p_indicatoren(p22_noemer, "P2.2", p22_noemer$p22) #sum is 11772


scorekaart <- rbind(U1, U3, K1.1, K1.2, K1.3, K1.4, P1, P1.1, P1.2, P2, P2.1, P2.2) 

OutputDataSet     <- as.data.frame(rbind(case_mix, behandel_mix, scorekaart))
save(OutputDataSet, file = "OutputDataSetMACK_C5.rda")
write.table(OutputDataSet, file = "OutputDataSet_2020.csv", sep = ";", row.names = F)
