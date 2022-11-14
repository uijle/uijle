# VBHC Aandoening: Kwetsbare ouderen
# Cyclus 0
#
# Script versie: 1
#
# Datum: 13-4-2022
#
# R and library versions
# R version:  3.6.2 (2018-04-23)
# dplyr:      1.0.6
# lubridate:  1.7.10
# readxl:     1.3.1
# tidyr:      1.1.3
# openxlsx:   4.2.3

# wdir = "M:/VBHC/11. Heupfractuur/Cyclus 4/3. Werkbestanden/R"
# 
# setwd(wdir)
 rm(list=ls())                   # Clear all variables from environment 
 
 wdir = "M:/VBHC/15. Kwetsbare ouderen/Data-analyse/dataframes"

setwd(wdir)
load("M:/VBHC/15. Kwetsbare ouderen/Data-analyse/dataframes/naPatSelectie.RData")

library(dplyr)        # For using piping operator 
library(lubridate)    # Voor easy date manupilation
library(readxl)       # Nodig zolang Excel ingelezen wordt
library(tidyr)
library(openxlsx)
library(stringi)


#ZA- en DBC codes instellen
ZA_ligduur         <- c("190090" , "190091" , "190218" , "190031" , "190038" ,"190092","190093",
                         "190034" , "190204" , "190205" , "190035" , "190055","190157" , "190158")
ZA_ligduurIC       <- c("190157" , "190158")
ZA_verkeerdbed         <- c("190092","190093","190031","190038")

# 190005   Multidisciplinair overleg (MDO).	
# 190006   Overleg palliatieve zorg.	
# 190009   Klinisch intercollegiaal consult.	
# 039581   Beperkt CGA in het kader van een medebehandeling.
# 190017   Medebehandeling 
# 	

ZA_medeConsult      <- c("190005" , "190006" , "190009" , "039581","190017") 
ZA_consult         <- c("190060" , "190013" , "190063" , "190066" , 
                         "190025" , "190161" , "190162" , "190163" , "190164" , "190165" , "190166" ,"190019")
ZA_poli            <- c("190060" , "190013" , "190063" , "190066") 
ZA_tel             <- c("190025" , "190161" , "190162" , "190163" , "190164" , "190165" , "190166" , "190019")           
ZA_SEH             <- c("190015" , "190016")
OM_Delier          <- c("delier", "delier (al dan niet na toediening van medicatie)")

ZA_psych           <- c("194152", "194164" , "194163" , "194162" , "194166", "194171", "194172", "194166", "194161", "194160"  )

 ZA_Dietist         <- c("192989" , "192990" , "192840", "192841")
 ZA_Fysiotherapeut        <- c("193001" , "193025" , "193029")
 ZA_Ergotherapeut        <- c("193012" , "192948" )
 ZA_Logopedist        <- c("192979" , "193021" , "193085" , "699824","198207","198205")
# 
ZA_Maatschappelijk        <- c("699821" , "198208" , "699825" , "699824","198207","198205","699822","699823","699826","699827",
                               "699828","699820","697401")
Delier_MED          <- c("N05AD01", "N05AH02", "N05AH03","N05AH04", "N05AX08")
DBC_CHI          <- c("218", "219","3019", "3020")     #let op: Interne heeft ook een DBC 218, dit is iets heel anders
DBC_ORT          <- c("3019", "3020") 
DBC_DEM          <- c("242","0401")     #let op: Interne en fys hebben ook een DBC 242, dit is iets heel anders
DBC_DEMNeu          <- c("0401") 
DBC_OpnRed_PNEU  <- c("1401") #### Pneumonie en COPD
DBC_OpnRed_COPD  <- c("1241") #### Pneumonie en COPD
DBC_OpnRed_NEU   <- c("1102", "1103","1111", "1112") #### CVA
DBC_OpnRed_HEUP          <- c("218", "219","3019", "3020")     #let op: Interne heeft ook een DBC 218, dit is iets heel anders
#DBC_OpnRed_ORT          <- c("3019", "3020") 
DBC_OpnRed_CAR  <- c("301","302") ##### Hartfalen
DBC_OpnRed_INT  <- c("431") #### bacteria Sepsis
#ZA_eind_dexa     = as.Date("31-08-2021", format = "%d-%m-%Y")


PatSel <- OpnameCompleetSelectie %>% 
          mutate (Aandoening = "Kwetsbare Ouderen",
                  Cyclus = ifelse(helft=="2021.1","0",
                           ifelse(helft=="2021.2","1",
                           ifelse(helft=="2022.1","2",NA))),       
                  Wegingsfactor = 1)

Complicatie <- Complicatie %>% filter(PatientNr %in% PatSel$PatientNr)
Diagnoses <- Diagnoses %>% filter(PatientNr %in% PatSel$PatientNr)
Medicatie <- Medicatie%>% filter(PatientNr %in% PatSel$PatientNr)
Operatie <-  Operatie %>% filter(PatientNr %in% PatSel$PatientNr)
OpnameMutaties <- OpnameMutaties %>% filter(PatientNr %in% PatSel$PatientNr)
Patient <- Patient %>% filter(PatientNr %in% PatSel$PatientNr)
Subtraject <- Subtraject %>% filter(PatientNr %in% PatSel$PatientNr) %>% rename(SubtrajectNr= SubTrajectNr)
VBI <- VBI %>% filter(PatientNr %in% PatSel$PatientNr)
Verrichting <- Verrichting %>% filter(PatientNr %in% PatSel$PatientNr)
Vragenlijst <- Vragenlijst %>% filter(PatientNr %in% PatSel$PatientNr)

### subtrajectnr toevoegen wordt later gebruikt  

PatSel <- left_join(PatSel,
                     Verrichting %>% filter(OpnameNr!="<< Onbekend >>") %>%  distinct(OpnameNr,SubtrajectNr)) %>% 
          distinct(PatientNr,Cyclus,OpnameNr, .keep_all = TRUE)

save.image("M:/VBHC/15. Kwetsbare ouderen/Data-analyse/dataframes/naPatSelectie.RData")
# --------------- Leeftijd -----------------                                             
lft         <-  PatSel %>%  
  mutate ( Ind    = "lft",
           Groep  = case_mix_name,
          Waarde = ifelse( LeeftijdOpname >= 70 & LeeftijdOpname <  80, 1, 
                    ifelse( LeeftijdOpname >= 80 & LeeftijdOpname <  90, 2, 
                    ifelse( LeeftijdOpname >= 90, 3, LeeftijdOpname))),
          Waarde =  ifelse(is.na(LeeftijdOpname), 99, Waarde)) %>% 
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 

table(lft$Waarde,lft$Cyclus)

# --------------- Geslacht -----------------                                            
gsl         <-  left_join(PatSel,
                          Patient %>% select(PatientNr,Geslacht))%>%  
                mutate (Ind    = "gsl",
                        Groep  = case_mix_name,
                        Waarde = ifelse(Geslacht == "M",1,ifelse(Geslacht=="V",2,99))) %>% 
        select( Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 

table(gsl$Waarde,gsl$Cyclus)
test <- Metingen   %>% filter(   tolower(ObservatieOmschrijving) == "lengte" &
                                           as.numeric(Uitslag) >= 100 & as.numeric(Uitslag) <= 220) %>% mutate (UitslagNum=as.numeric(Uitslag))
# --------------- Koppel lengte [m] meting dichtste bij startwindow aan PatSelectie --------------
Lengte        <- left_join(PatSel,
                           Metingen   %>% filter(   tolower(ObservatieOmschrijving) == "lengte" &
                                           as.numeric(Uitslag) >= 100 & as.numeric(Uitslag) <= 220) %>%
                                     select(PatientNr,ObservatieDatum,ObservatieOmschrijving,Uitslag )) %>% 
                    group_by(PatientNr) %>%
                    filter(  min( abs( difftime(ObservatieDatum, OpnameDatumTijd, units = "days"))) ==
                              abs( difftime(ObservatieDatum, OpnameDatumTijd, units = "days"))) %>%
                    mutate(  Lengte = mean(as.numeric(Uitslag))/100) %>%
                    ungroup() %>%
                    distinct(PatientNr, Lengte)

PatSel2       <- merge( x     = PatSel,
                        y     = Lengte,
                        by    = "PatientNr",
                        all.x = TRUE)

# --------------- Koppel gewicht [kg] meting dichtste bij peildatum aan PatSelectie --------------
#Let op: >1 meting voor gewicht geregistreerd op dezelfde dag? Je wilt 1 meting per patient.
Gewicht      <-  left_join(PatSel,
                           Metingen   %>% filter(  tolower(ObservatieOmschrijving) == "gewicht" &
                                       as.numeric(Uitslag) >= 30 & as.numeric(Uitslag) <=300) %>%
                                     select(PatientNr,ObservatieDatum,ObservatieOmschrijving,Uitslag ) )%>%
                  group_by(PatientNr) %>%
                  filter(  min( abs( difftime(ObservatieDatum, OpnameDatumTijd,units = "days"))) ==
                            abs( difftime(ObservatieDatum, OpnameDatumTijd,units = "days")) ) %>%
                  mutate(  Gewicht = mean(as.numeric(Uitslag))) %>%
                  ungroup() %>%
                  distinct(PatientNr, Gewicht)

PatSel2      <- merge( x     = PatSel2,
                       y     = Gewicht,
                       by    = "PatientNr",
                       all.x = TRUE)

# ---------------------------------- Bepaal BMI van patient ----------------------------------
PatSel2 <- PatSel2 %>% mutate(BMI = Gewicht/(Lengte*Lengte))

bmi     <- PatSel2 %>% mutate(Ind    = "bmi",
                              Groep  = case_mix_name,
                               Waarde = ifelse(BMI <  18.5, 1, NA),
                               Waarde = ifelse(BMI >= 18.5 & BMI <  25.0, 2, Waarde),
                               Waarde = ifelse(BMI >= 25.0 & BMI <  30.0, 3, Waarde),
                               Waarde = ifelse(BMI >= 30.0 & BMI <  35.0, 4, Waarde),
                               Waarde = ifelse(BMI >= 35.0, 5, Waarde),
                               Waarde = ifelse(is.na(BMI),  99, Waarde)) %>%
                    select( Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 

table(bmi$Waarde,bmi$Cyclus)

# ---------------------------------- Roken -------------------------------------------------
# O.b.v. JAARCONTROLE
########Kies welke je wilt gebruiken
# rok     <- Jaarcontrole %>%
#               filter(VraagOms=="Roken") %>%               #LET OP: IN STA GECODEERD ALS 1 = 'ja, 0 = 'nee'
#               mutate(rok = as.numeric(Uitslag)) %>%
#               filter(!is.na(rok)) %>%
#               distinct()
## omzetscript ROKEM
#CWZ 
rok      <-  left_join(PatSel,
                           Roken   %>% 
                                     select(PatientNr,ObservatieDatum,roken ) )%>%
                  group_by(PatientNr) %>%
                  filter(  min( abs( difftime(ObservatieDatum, OpnameDatumTijd,units = "days"))) ==
                            abs( difftime(ObservatieDatum, OpnameDatumTijd,units = "days")) ) %>%
                  mutate(  rok = roken) %>%
                  ungroup() %>%
                  distinct(PatientNr, rok)                   
rok <- Roken %>% select(PatientNr,roken) %>% rename(rok=roken) %>% ungroup()
rok       <- left_join(PatSel, rok, by="PatientNr") %>%
                              mutate( Groep  = case_mix_name,
                                      Ind    = "rok",
                                      Waarde = ifelse(rok == 0, 0, NA),
                                      Waarde = ifelse(rok == 1, 1, Waarde),
                                      Waarde = ifelse(is.na(Waarde), 99, Waarde)) %>%
                select( Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 
                table(rok$Waarde,rok$Cyclus)


#### check welke uitslagen er zijn en zet dat om naar ja of nee  "gestopt" "ja"      "nooit"   ""        "passief"


# ---------------------------------- Comorbiditeiten ----------------------------------
# Comorbiditeit <- Vragenlijst %>% filter(ParentVragenlijst=="Charlson Comorbidity Index")
# unique((Comorbiditeit$Vraagstelling))   
# # [1] "* Myocardinfarct"                                 "* Gastrointestinaal ulcuslijden"                  "* Congestief hartfalen"                          
# #  [4] "* Bindweefselziekte (o.a. reumato\xefde ziekten)" "* Dementie"                                       "* Diabetes mellitus"                             
# #  [7] "* Para-/hemiplegie"                               "* Leverziekte"                                    "* Nierziekte"                                    
# # [10] "* HIV/AIDS"                                       "Geregistreerde comorbiditeiten"                   "* Maligniteit (excl. PCC, BCC)"                  
# # [13] "* Chronische longziekte"                          "* Cerebrovasculaire aandoening (o.a. CVA/TIA)"    "* Perifeer vaatlijden/aneurysma aorta"           
# # [16] "Geen comorbiditeiten"   
# 
# COM_cardiaal   <- c("* Myocardinfarct" , "* Congestief hartfalen")
# COM_dementie   <- c("* Dementie" )
# COM_CVA        <- c("* Cerebrovasculaire aandoening (o.a. CVA/TIA)" )
# COM_diab        <- c("* Diabetes mellitus")
# 
# ComorbCM  <- left_join(PatSel %>% select(PatientNr, Aandoening, Cyclus, Groep, OpnameDatum, OntslagDatum),
#                                Comorbiditeit %>% select(PatientNr, ParentVragenlijst, Beantwoordingsdatum, Vraagstelling, Antwoord) %>% 
#                                  mutate(Beantwoordingsdatum=as.Date(Beantwoordingsdatum, format = "%Y-%m-%d")) %>% 
#                                  filter(  (  Vraagstelling %in% COM_cardiaal)|(    Vraagstelling %in% COM_dementie)|(    Vraagstelling %in% COM_CVA ) |(    Vraagstelling %in% COM_diab ))) %>%
#                                  filter(      Beantwoordingsdatum<=OpnameDatum) %>% 
#                       distinct(PatientNr,Groep,Vraagstelling, .keep_all=TRUE) %>% 
#                       mutate(dementie = ifelse((  Vraagstelling %in% COM_dementie), 1,0),
#                              cardiaal = ifelse(( Vraagstelling %in% COM_cardiaal), 1,0),
#                              cva = ifelse((  Vraagstelling %in% COM_CVA ), 1,0),
#                              diabetes = ifelse(( Vraagstelling %in% COM_diab), 1,0))
# ComortTest <- PatSel %>% 
#           mutate(Charlson=ifelse(PatientNr %in% Comorbiditeit$PatientNr,1,0))
# table(ComortTest$Charlson)
#    0    1 
# 3022   29 
#DementieTest2 <- Subtraject %>% filter(AGB_Code %in% c("NEU","GER")& DiagnoseCode %in% DBC_DEM)
DementieTest <- Vragenlijst %>% filter (Vraagcode=="CS00215458" & Antwoord=="ja") ## Dit is de vraag uit de APOP "Is de diagnose dementie bij de pati�nt vastgesteld?"
dem <- PatSel %>% 
          mutate(  Ind    = "dem",
                        Groep  = case_mix_name,
                   Waarde = ifelse(PatientNr %in% DementieTest$PatientNr,1,0)) %>% 
          select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
table(dem$Waarde,dem$Cyclus)

# dem <- PatSel %>% 
#           mutate(  Ind    = "dem",
#                         Groep  = case_mix_name,
#                    Waarde = ifelse(PatientNr %in% DementieTest2$PatientNr,1,0)) %>% 
#           select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
# table(dem$Waarde)
#    0    1   op basis van dbc diagnose
# 2208  106 
####volgende cyclus onbekend nakijken
RedenOpname <- left_join(PatSel %>% select (Aandoening, Cyclus, PatientNr, OpnameNr,SubtrajectNr,Wegingsfactor),
                     Opname %>% select(OpnameNr,Spoed, HerkomstOmschrijving, BestemmingOmschrijving,
                                             OpnameAfdelingOmschrijving, OpnameAfdeling, OpnameIndicatie, Spoed, AGB_CodeOpnameSpecialisme)) %>% distinct(PatientNr,OpnameNr, .keep_all=TRUE) %>% #moet Opname Worden
          left_join(Subtraject %>% select(SubtrajectNr, AGB_Code,DiagnoseCode,DiagnoseOmschrijving))

# --------------- reden opname -----------------      
### redenopnametest <- as.data.frame(table(RedenOpname$AGB_Code,RedenOpname$DiagnoseCode)) #### Bepalen top 6 opnameredenen

red <- RedenOpname %>% select(Aandoening,Cyclus, PatientNr,Wegingsfactor, AGB_Code,DiagnoseCode,DiagnoseOmschrijving,OpnameIndicatie) %>% 
          mutate(Ind    = "red",
                        Groep  = case_mix_name,
                 Waarde=ifelse( OpnameIndicatie=="COVID", 1,
                        ifelse( AGB_Code!="INT" & DiagnoseCode %in% DBC_OpnRed_HEUP, 2,
                        ifelse( AGB_Code=="CAR" & DiagnoseCode %in% DBC_OpnRed_CAR, 3,
                        ifelse( AGB_Code=="NEU" & DiagnoseCode %in% DBC_OpnRed_NEU, 4,
                        ifelse( AGB_Code=="INT" & DiagnoseCode %in% DBC_OpnRed_INT, 5,
                        ifelse( AGB_Code=="LON" & DiagnoseCode %in% DBC_OpnRed_COPD, 6,7))))))) %>% 
          select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

table(red$Waarde,red$Cyclus)

urg <- RedenOpname %>% select(Aandoening, Cyclus, PatientNr,Wegingsfactor,Spoed) %>% 
          mutate(Ind    = "urg",
                        Groep  = case_mix_name,
                 Waarde=ifelse( Spoed=="J", 1,
                        ifelse( Spoed=="N", 2,99))) %>% 
          select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

table(urg$Waarde,urg$Cyclus)

loctest <- RedenOpname %>% select(Aandoening, Cyclus, PatientNr,Wegingsfactor, OpnameAfdelingOmschrijving, OpnameAfdeling) 
loctest <- as.data.frame(table(loctest$OpnameAfdelingOmschrijving)) #### Bepalen top 6 opnameredenen

#### moet later bepaald worden hoe dit over meerdere ziekenhuizen 

# --------------- Woonsituatie voor opname -----------------      
unique(RedenOpname$HerkomstOmschrijving)
#### "Instelling (anders)"       "Eigen woonomgeving"        "Ander ziekenhuis"          "Verpleeg-/verzorgingshuis" "Uit eigen ziekenhuis"      "Instelling (revalidatie)" 
woonvoor         <- RedenOpname %>% mutate( Ind    = "woonvoor",
                        Groep  = case_mix_name,
                                            Waarde=ifelse( HerkomstOmschrijving=="Eigen woonomgeving", 1,
                                                   ifelse( HerkomstOmschrijving=="Verpleeg-/verzorgingshuis", 2,
                                                   ifelse( HerkomstOmschrijving=="Instelling (revalidatie)", 3,
                                                   ifelse( HerkomstOmschrijving=="Uit eigen ziekenhuis", 4,
                                                   ifelse( HerkomstOmschrijving=="Ander ziekenhuis", 5,
                                                   ifelse( HerkomstOmschrijving=="Instelling (anders)", 6,99))))))) %>% 
  select( Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 
table(woonvoor$Waarde,woonvoor$Cyclus)

# --------------- Woonsituatie voor opname -----------------      
unique(RedenOpname$BestemmingOmschrijving)
#### "Overleden (zonder obductie)"    "Eigen woonomgeving"             "Instelling (revalidatie)"       "Instelling (anders)"            "Verpleeg- / verzorgingshuis"    "Hospice"                       
#### "Ander ziekenhuis"               "Eigen woonomgeving met zorg"    "Terug naar initiele instelling" "Instelling (psychiatrisch)"     "Overleden (met obductie)" 

besttest <- as.data.frame(table(RedenOpname$BestemmingOmschrijving))


# Eigen woonomgeving	729
# Instelling (revalidatie)	274
# Verpleeg- / verzorgingshuis	122
# Overleden (zonder obductie)	109
# Eigen woonomgeving met zorg	103
# Hospice	27
# Instelling (anders)	22
# Ander ziekenhuis	17
# Instelling (psychiatrisch)	5
# Terug naar initiele instelling	5
# Overleden (met obductie)	1

Omschr_Best_overig <- c("Instelling (psychiatrisch)","Terug naar initiele instelling","Instelling (anders)")
Omschr_Best_overleden <- c("Overleden (zonder obductie)","Overleden (met obductie)")
besttest <- as.data.frame(table(RedenOpname$BestemmingOmschrijving))
RedenOpname <- RedenOpname %>% mutate(OntslagBestemming=BestemmingOmschrijving)
woonontsl         <- RedenOpname %>% mutate( Ind    = "woonontsl",
                        Groep  = case_mix_name,
                                            Waarde=ifelse( OntslagBestemming=="Eigen woonomgeving", 1,
                                                   ifelse( OntslagBestemming=="Eigen woonomgeving met zorg", 2,
                                                   ifelse( OntslagBestemming=="Instelling (revalidatie)", 3,
                                                   ifelse( OntslagBestemming=="Verpleeg- / verzorgingshuis", 4,
                                                   ifelse( OntslagBestemming %in% Omschr_Best_overig, 5,
                                                   ifelse( OntslagBestemming=="Ander ziekenhuis", 8,
                                                   ifelse( OntslagBestemming %in% Omschr_Best_overleden, 6,
                                                   ifelse( OntslagBestemming=="Hospice", 7,99))))))))) %>% 
  select( Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 
table(woonontsl$Waarde,woonontsl$Cyclus)

# ----------PolyPharmacy ----------
PolyPharmacy      <- left_join(PatSel %>% select(PatientNr, Aandoening, Cyclus, Wegingsfactor,OpnameDatumTijd,OntslagDatumTijd),
                           Medicatie %>% select(PatientNr, StartDatum, EindDatum, ATC_code, Setting) %>% 
                                         filter(EindDatum >= inc_start & ATC_code !="<< Onbekend >>") %>% 
                                         mutate(ATC3 = substr(ATC_code,1,3),
                                                duur = difftime(EindDatum ,StartDatum,  units = c("days")))) %>% 
                 mutate(BijOpname=ifelse(StartDatum<=OpnameDatumTijd,1,0),
                        BijOntslag=ifelse(StartDatum>=OpnameDatumTijd & EindDatum>OntslagDatumTijd,1,0)) %>% 
                   filter(duur>=90|duur<0)  ###### chronisch gebruik volgens definitie >= 90 dagen, in CWZ is bij Startdatum onbekend de datum 2099 (aangenomen dat dit langer gebruik is)
                 
table(PolyPharmacy$ATC3)

medvoor <- left_join(PatSel %>% select(PatientNr, Aandoening, Cyclus, Wegingsfactor,OpnameDatumTijd,OntslagDatumTijd),
           PolyPharmacy %>% filter(BijOpname==1) %>% 
                            distinct(PatientNr,ATC3, .keep_all=TRUE) %>% 
                            group_by(PatientNr) %>% 
                            mutate(  AantalPharm  = n()) %>% select(PatientNr,AantalPharm) %>% distinct (PatientNr,AantalPharm))%>% 
            mutate(  Ind    = "medvoor",
                        Groep  = case_mix_name,
                     Waarde = ifelse(is.na(AantalPharm),1,
                              ifelse(AantalPharm>=5,3,2))) %>% 
            select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
table(medvoor$Waarde,medvoor$Cyclus)
          
medontsl <- left_join(PatSel %>% select(PatientNr, Aandoening, Cyclus, Wegingsfactor,OpnameDatumTijd,OntslagDatumTijd),
           PolyPharmacy %>% filter(BijOntslag==1) %>% 
                            distinct(PatientNr,ATC3, .keep_all=TRUE) %>% 
                            group_by(PatientNr) %>% 
                            mutate(  AantalPharm  = n()) %>% select(PatientNr,AantalPharm) %>% distinct (PatientNr,AantalPharm))%>% 
            mutate(  Ind    = "medontsl",
                        Groep  = case_mix_name,
                     Waarde = ifelse(is.na(AantalPharm),1,
                              ifelse(AantalPharm>=5,3,2))) %>% 
            select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
table(medontsl$Waarde,medontsl$Cyclus)

begink <- PatSel %>% 
          mutate (Ind    = "begink",
                  Groep  = case_mix_name,
                  Waarde = ifelse(Primairehelft=="2021.1","0",
                           ifelse(Primairehelft=="2021.2","1",
                           ifelse(Primairehelft=="2022.1","2",NA)))) %>% 
            select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)   
 table(begink$Waarde,lft$Cyclus)                
##########------------ Hoofd en medebehandelaar   

OpnameSpecialismetest <- as.data.frame(table(RedenOpname$AGB_CodeOpnameSpecialisme))

Top8OpnSpec <- c("CAR", "CHI", "INT", "LON", "MDL", "NEU", "ORT", "URO" ,"GER")

`%!in%` <- Negate(`%in%`) ### functie maken om waarden niet in bovenstaande lijst te coderen

opnamespec         <- RedenOpname %>% mutate( Ind    = "opnspec",
                        Groep  = case_mix_name,
                                            Waarde=ifelse( AGB_CodeOpnameSpecialisme=="INT", 1,
                                                   ifelse( AGB_CodeOpnameSpecialisme=="CAR", 2,
                                                   ifelse( AGB_CodeOpnameSpecialisme=="LON", 3,
                                                   ifelse( AGB_CodeOpnameSpecialisme=="CHI", 4,
                                                   ifelse( AGB_CodeOpnameSpecialisme=="NEU", 5,
                                                   ifelse( AGB_CodeOpnameSpecialisme=="MDL", 6,
                                                   ifelse( AGB_CodeOpnameSpecialisme=="ORT", 7,
                                                   ifelse( AGB_CodeOpnameSpecialisme=="URO", 8,
                                                   ifelse( AGB_CodeOpnameSpecialisme=="GER", 9,
                                                   ifelse( AGB_CodeOpnameSpecialisme %!in% Top8OpnSpec,10,99))))))))))) %>% 
  select( Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 

table(opnamespec$Waarde,opnamespec$Cyclus)
# ###~~~~~~~~~~~~~~U1 ~~~~~~~~~~~~###
# TOPIC <- left_join(PatSel %>% select(PatientNr, Aandoening, Cyclus, Wegingsfactor,OntslagDatumTijd),
#                    Vragenlijst %>% filter(Vragenlijstnaam=="TOPICS-SF") %>% distinct(PatientNr,Beantwoordingsdatum)) %>% 
#         mutate(TijdOntslagTopic=as.numeric(difftime(Beantwoordingsdatum,OntslagDatumTijd,units="weeks"))/52)
# 
# U11        <- TOPIC %>% 
#                  mutate(Waarde=ifelse(!is.na(TijdOntslagTopic),1,0),
#                         Groep  = scorekaart_name,
#                         Ind="U1.1") %>% 
#                  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

                 
# table(U11$Waarde,U11$Cyclus)
###~~~~~~~~~~~~~~overleving ~~~~~~~~~~~~###

overleving  <- left_join(PatSel,
                         Patient %>% select(PatientNr, OverlijdensDatum)) %>%
  mutate( OntslagDatum=as.Date(OntslagDatumTijd,  format = "%Y-%m-%d"),
            overleden  = difftime(OverlijdensDatum, OntslagDatum, units = "days"),
          overl30    = ifelse(overleden>0 & overleden <=30, 0, 1),
          overl90    = ifelse(overleden>0 & overleden <=90, 0, 1),
          zkhmort    = ifelse(OverlijdensDatum<=OntslagDatum & OverlijdensDatum>=OpnameDatumTijd , 1, 0))

noemerselectieFU <- overleving %>% filter(zkhmort==0)
table(noemerselectieFU$zkhmort,noemerselectieFU$Cyclus)
####~~~~~~~~~~~~~~U2 ~~~~~~~~~~~~###

# ----------- SEH bezoeken  -----------------
SEH_Verrichting <- Verrichting %>% filter( ZACode %in% ZA_SEH)


SEH2 <- merge(  x     = PatSel %>% select(PatientNr, OpnameNr, Aandoening, Cyclus, Wegingsfactor,OntslagDatumTijd),
               y     = SEH_Verrichting,
               by    = "PatientNr",
               all.x = TRUE) %>%
       filter (Verrichtingdatum > inc_start & 
               Verrichtingdatum >= OntslagDatumTijd,
               Verrichtingdatum <= OntslagDatumTijd + days(90)) %>% 
       mutate(Aantal30=ifelse(Verrichtingdatum <= OntslagDatumTijd + days(30),1,0),
              Aantal90=ifelse(Verrichtingdatum <= OntslagDatumTijd + days(90),1,0)) %>% 
       group_by(PatientNr) %>% 
       mutate(AantalSEH30=sum(Aantal30),
              AantalSEH90=sum(Aantal90))


AantalSEH <- left_join(PatSel , 
                       SEH2 %>% select(PatientNr,AantalSEH30,AantalSEH90) %>% distinct(PatientNr,AantalSEH30,AantalSEH90)) %>% 
            mutate(AantalSEH30 = ifelse( is.na(AantalSEH30), 0, AantalSEH30),
                   AantalSEH90 = ifelse( is.na(AantalSEH90), 0, AantalSEH90))

U21  <- AantalSEH %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>% 
          mutate( Ind           = "U2.1",
                        Groep  = scorekaart_name,
                              Waarde        = ifelse( AantalSEH30 >= 1, 1, 0), 
                              Wegingsfactor =  1)  %>% 
           select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 

table(U21$Waarde,U21$Cyclus)

U22 <- AantalSEH %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>% 
          mutate (Ind    = "U2.2",
                        Groep  = scorekaart_name,
                              Waarde = AantalSEH90) %>% 
              select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

table(U22$Waarde, U22$Cyclus)

U22 %>% 
  group_by(Cyclus) %>% 
  summarize(teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))
### volgende cyclus noemer alleen seh ptn?
  #  U22 %>% filter(Waarde>0) %>% 
  # group_by(Cyclus) %>% 
  # summarize(teller=sum(Waarde),
  #           Noemer=sum(Wegingsfactor),
  #           gemid=mean(Waarde),
  #           mini=min(Waarde),
  #           q5 = quantile(Waarde, 0.05),
  #           q1 = quantile(Waarde, 0.25),
  #           q3 = quantile(Waarde, 0.75),
  #           q95 = quantile(Waarde, 0.95),
  #           maxi=max(Waarde))                  

spoed_heropn <- left_join (PatSel %>% select(PatientNr, OpnameNr, Aandoening, Cyclus, Wegingsfactor,OntslagDatumTijd),
                           Opname %>%    mutate (HeropDat = as.Date(OpnameDatumTijd, format = "%Y-%m-%d"),
                                                 HerontDat = as.Date(OntslagDatumTijd, format = "%y-%m-%d")) %>% 
                             select (PatientNr, HeropDat, HerontDat, BehandelSettingCode, OpnameIndicatie, Spoed) %>%
                             filter (Spoed == "J",
                                     BehandelSettingCode == "K|"),
                           by = "PatientNr") %>% 
                 filter (HeropDat >= OntslagDatumTijd,
                         HeropDat <= OntslagDatumTijd + days(90)) %>% 
                 mutate(Aantal30=ifelse(HeropDat <= OntslagDatumTijd + days(30),1,0),
                        Aantal90=ifelse(HeropDat <= OntslagDatumTijd + days(90),1,0)) %>% 
                 group_by(PatientNr) %>% 
                 mutate(Aantal30tot=sum(Aantal30),
                        Aantal90tot=sum(Aantal90))


heropntot <- left_join (PatSel %>% select(PatientNr, OpnameNr, Aandoening, Cyclus, Wegingsfactor),
                        spoed_heropn %>%  select (PatientNr, OpnameNr,Aantal30tot,Aantal90tot)) %>% 
  mutate    (Aantal30tot = ifelse (is.na(Aantal30tot), 0 , Aantal30tot),
             Aantal90tot = ifelse (is.na(Aantal90tot), 0 , Aantal90tot)) %>% 
  distinct  (PatientNr, Cyclus, .keep_all = TRUE)

table(heropntot$Aantal30tot)

U23       <- heropntot %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>% 
  mutate (Ind    = "U2.3",
                        Groep  = scorekaart_name,
          Waarde = ifelse( Aantal30tot >= 1, 1, 0))  %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
table(U23$Waarde,U23$Cyclus)

U24       <- heropntot  %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>% 
  mutate (Ind    = "U2.4",
                        Groep  = scorekaart_name,
          Waarde = Aantal90tot)  %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

table(U24$Waarde,U24$Cyclus)


#U3
overleving  <- left_join(PatSel,
                         Patient %>% select(PatientNr, OverlijdensDatum)) %>%
  mutate( OntslagDatum=as.Date(OntslagDatumTijd,  format = "%Y-%m-%d"),
            overleden  = difftime(OverlijdensDatum, OntslagDatum, units = "days"),
          overl30    = ifelse(overleden>0 & overleden <=30, 1, 0),
          overl90    = ifelse(overleden>0 & overleden <=90, 1, 0),
          overl180    = ifelse(overleden>0 & overleden <=180, 1, 0),
          overl365    = ifelse(overleden>0 & overleden <=365, 1, 0),
          zkhmort    = ifelse(OverlijdensDatum<=OntslagDatum & OverlijdensDatum>=OpnameDatumTijd , 1, 0))


#----- U31 ziekenhuismortaliteit -----
U31        <- overleving %>% 
  mutate (Ind    = "U3.1",
                        Groep  = scorekaart_name,
          Waarde = zkhmort) %>%
     select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 

table(U31$Waarde,U31$Cyclus)

# --- U32 Overleving 30 dgn -----
U32        <- overleving %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>% 
                 mutate (Ind    = "U3.2",
                        Groep  = scorekaart_name,
                         Waarde = overl30) %>%
                 select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 

table(U32$Waarde,U32$Cyclus)

# --- U32 Overleving 90 dgn -----
U33       <- overleving %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>% 
  mutate (Ind    = "U3.3",
                        Groep  = scorekaart_name,
          Waarde = overl90) %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 

table(U33$Waarde,U33$Cyclus)

# --- U32 Overleving 180dgn ----- noemer alleen cyclus 0 en 1?
U34       <- overleving %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>% 
  mutate (Ind    = "U3.4",
                        Groep  = scorekaart_name,
          Waarde = overl180) %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 

table(U34$Waarde,U34$Cyclus)

# --- U32 Overleving 365 dgn ----- Noemer alleen Cyclus 0?
U35       <- overleving %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>% 
  mutate (Ind    = "U3.5",
                        Groep  = scorekaart_name,
          Waarde = overl365) %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) 

table(U35$Waarde,U35$Cyclus)

# --- U4 Complicaties -----


compl        <- left_join (PatSel,
                           Complicatie %>% rename (AGB_codeCompl = AGB_code)  %>% filter(ComplicatieDatum>=inc_start & ComplicatieDatum<=inc_eind)) %>% 
                 filter    (ComplicatieDatum >= OpnameDatumTijd & ComplicatieDatum <= OntslagDatumTijd) %>% #### moet Ontslagdatum ruimer worden genomen bij late registratie?
                 mutate(Omschrijving2=tolower(Omschrijving),
                        Omschrijving3=ifelse(grepl("urineweginf",Omschrijving2),"urineweginfectie",
                                      ifelse(grepl("decubi",Omschrijving2),"decubitus",
                                      ifelse(grepl("delie",Omschrijving2),"delier",
                                      ifelse(grepl("overlij",Omschrijving2),"overlijden",
                                      ifelse((grepl("val",Omschrijving2)& !grepl("uitval",Omschrijving2)),"val",Omschrijving2)))))) %>% 
          distinct()### Val gaf ook 'uitval neurolo....' in testcompl hieronder

##### Gebruik testcompl om te kijken of het omzetten hierboven goed is gegaan
testcompl <- compl %>% select(Omschrijving2, Omschrijving3) %>% filter(Omschrijving3=="delier"|
                                                                         Omschrijving3=="overlijden"|
                                                                         Omschrijving3=="decubitus"|
                                                                         Omschrijving3=="val"|
                                                                         Omschrijving3=="urineweginfectie")  %>% 
                                                                distinct(Omschrijving2, .keep_all=TRUE)


test <-  as.data.frame(table(compl$Omschrijving3))  ### Gebruiken om overzicht van complicaties te zien

# ---------- U4 Complicatie  ----------

U4         <- left_join(PatSel %>% select(PatientNr, Aandoening,Cyclus,Wegingsfactor),
                         compl %>% filter (Omschrijving3!="geen complicatie")) %>% 
               mutate(Waarde=ifelse(is.na(Omschrijving3),0,1)) %>% 
               group_by(PatientNr) %>% 
               mutate(Aantalcompl=sum(Waarde)) %>% 
               distinct(PatientNr, Cyclus, Aantalcompl, .keep_all=TRUE) %>% 
               mutate (Ind    = "U4",
                        Groep  = scorekaart_name,
               Waarde = ifelse( Aantalcompl >= 1, 1, 0)) %>%
               select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()

table(U4$Waarde, U4$Cyclus)

#### Op basis van complicaties
U41c         <- left_join(PatSel %>% select(Aandoening, Cyclus, PatientNr),
                        compl %>% filter (Omschrijving3=="delier")) %>% 
  mutate(Waarde=ifelse(is.na(Omschrijving3),0,1)) %>% 
  group_by(PatientNr,Cyclus) %>% 
  mutate(Aantalcompl=sum(Waarde)) %>% 
  distinct(PatientNr,Aantalcompl, .keep_all=TRUE) %>% 
  mutate (Ind    = "U4.1c",
                        Groep  = scorekaart_name,
          Waarde = ifelse( Aantalcompl >= 1, 1, 0),
          Wegingsfactor= 1) %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()

table(U41c$Waarde, U41c$Cyclus)

#### Op basis van DOS 
DOS         <- left_join(PatSel %>% select(Aandoening,Cyclus, PatientNr,OntslagDatumTijd,OpnameDatumTijd),
                         Vragenlijst %>% filter (Vraagstelling=="DOS schaal eindscore" & Antwoord=="waarschijnlijk delier") %>% select(PatientNr,Beantwoordingsdatum, Antwoord)) %>% 
  mutate(Waarde =ifelse(Beantwoordingsdatum<=OntslagDatumTijd & Beantwoordingsdatum>=OpnameDatumTijd,1,0)) %>% 
  filter(Waarde==1) %>% 
  distinct(PatientNr,Cyclus,.keep_all=TRUE)
 
U41d <- left_join(PatSel,
                    DOS) %>% 
  mutate (Ind    = "U4.1d",
                        Groep  = scorekaart_name,
          Waarde = ifelse( is.na(Waarde), 0, Waarde),
          Wegingsfactor= 1) %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()

table(U41d$Waarde, U41d$Cyclus)


#### Op basis van Haloperidol 
ProxyU41         <- left_join(PatSel ,
                           Medicatie %>% filter (ATC_code %in% Delier_MED)) %>%
  filter(StartDatum<=OntslagDatumTijd & StartDatum>=OpnameDatumTijd) %>%
  distinct(PatientNr, Cyclus, .keep_all=TRUE) %>%
  filter(!is.na(StartDatum)) %>%
  mutate(Waarde=1) %>%
  select(PatientNr, Cyclus, Waarde)



U41f <- left_join(PatSel,
                  ProxyU41) %>%
  mutate (Ind    = "U41f",
                        Groep  = scorekaart_name,
          Waarde = ifelse( is.na(Waarde), 0, 1),
          Wegingsfactor= 1) %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()
table(U41f$Waarde, U41f$Cyclus)



U411c <- left_join(PatSel %>% select(PatientNr,Cyclus,DelierRisico),
                  U41c ) %>% 
  filter(DelierRisico==1) %>% 
  select(-DelierRisico)%>% 
  mutate (Ind    = "U4.1.1c",
                        Groep  = scorekaart_name) %>% 
          select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()
table(U411c$Waarde,U411c$Cyclus)

U411d <- left_join(PatSel %>% select(PatientNr,Cyclus,DelierRisico),
                  U41d ) %>% 
  filter(DelierRisico==1) %>% 
  select(-DelierRisico)%>% 
  mutate (Ind    = "U4.1.1d")
table(U411d$Waarde,U411d$Cyclus)

U411f <- left_join(PatSel %>% select(PatientNr,Cyclus, DelierRisico),
                  U41f ) %>%
  filter(DelierRisico==1) %>%
  select(-DelierRisico)%>%
  mutate (Ind    = "U411f")

table(U411f$Waarde, U411f$Cyclus)

U412c <- left_join(PatSel %>% select(PatientNr,Cyclus,DelierRisico),
                  U41c ) %>% 
  filter(DelierRisico==0) %>% 
  select(-DelierRisico) %>% 
  mutate (Ind    = "U4.1.2c")

table(U412c$Waarde, U412c$Cyclus)

U412d <- left_join(PatSel %>% select(PatientNr,Cyclus, DelierRisico),
                  U41d ) %>% 
  filter(DelierRisico==0) %>% 
  select(-DelierRisico) %>% 
  mutate (Ind    = "U4.1.2d")

table(U412d$Waarde, U412d$Cyclus)


U412f <- left_join(PatSel %>% select(PatientNr,Cyclus,DelierRisico),
                  U41f ) %>% 
  filter(DelierRisico==0) %>% 
  select(-DelierRisico) %>% 
  mutate (Ind    = "U4.1.2f")

table(U412f$Waarde, U412f$Cyclus)


U413c <- left_join(PatSel %>% select(PatientNr,Cyclus, DelierTijd),
                  U41c ) %>% 
          mutate(DelierTijd=ifelse(is.na(DelierTijd),99,DelierTijd)) %>% 
  filter(DelierTijd>2) %>% 
  select(-DelierTijd) %>% 
  mutate (Ind    = "U4.1.3c")

table(U413c$Waarde,U413c$Cyclus)

U413d <- left_join(PatSel %>% select(PatientNr,Cyclus, DelierTijd),
                  U41d ) %>% 
          mutate(DelierTijd=ifelse(is.na(DelierTijd),99,DelierTijd)) %>% 
  filter(DelierTijd>2) %>% 
  select(-DelierTijd) %>% 
  mutate (Ind    = "U4.1.3d")

table(U413d$Waarde,U413d$Cyclus)

U413f <- left_join(PatSel %>% select(PatientNr,Cyclus, DelierTijd),
                  U41f ) %>% 
          mutate(DelierTijd=ifelse(is.na(DelierTijd),99,DelierTijd)) %>% 
  filter(DelierTijd>2) %>% 
  select(-DelierTijd) %>% 
  mutate (Ind    = "U4.1.3f")

table(U413f$Waarde,U413f$Cyclus)

# ############moet op basis van VIM onderstaande code moet worden aangepast
# U42         <- left_join(PatSel %>% select(PatientNr, Aandoening,Cyclus,Wegingsfactor),
#                          compl %>% filter (Omschrijving3=="val")) %>% 
#   mutate(Waarde=ifelse(is.na(Omschrijving3),0,1)) %>% 
#   group_by(PatientNr) %>% 
#   mutate(Aantalcompl=sum(Waarde)) %>% 
#   distinct(PatientNr, Aantalcompl, .keep_all=TRUE) %>% 
#   mutate (Ind    = "U4.2",
#                         Groep  = scorekaart_name,
#           Waarde = ifelse( Aantalcompl >= 1, 1, 0)) %>%
#   select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()
# 
# table(U42$Waarde, U42$Groep)
# 
# U421 <- left_join(PatSel %>% select(PatientNr, Aandoening,Cyclus,Wegingsfactor,ValRisico),
#                   U42 ) %>% 
#   filter(ValRisico==1) %>% 
#   select(-ValRisico)%>% 
#   mutate (Ind    = "U4.2.1")
# 
# table(U421$Waarde, U421$Groep)
# 
# U422 <- left_join(PatSel %>% select(PatientNr, Aandoening,Cyclus,Wegingsfactor,ValRisico),
#                   U42 ) %>% 
#   filter(ValRisico==0) %>% 
#   select(-ValRisico) %>% 
#   mutate (Ind    = "U4.2.2")
# 
# table(U422$Waarde, U422$Groep)
# 
# U423 <- left_join(PatSel %>% select(PatientNr, Aandoening,Cyclus,Wegingsfactor,ValRisico,ValTijd),
#                   U42 ) %>% 
#   filter(is.na(ValTijd)) %>% 
#   select(-ValRisico,-ValTijd) %>% 
#   mutate (Ind    = "U4.2.3")
# 
# table(U423$Waarde, U423$Groep)

U43         <- left_join(PatSel %>% select(PatientNr, Aandoening,Cyclus,Wegingsfactor),
                         compl %>% filter (Omschrijving3=="decubitus")) %>% 
  mutate(Waarde=ifelse(is.na(Omschrijving3),0,1)) %>% 
  group_by(PatientNr,Cyclus) %>% 
  mutate(Aantalcompl=sum(Waarde)) %>% 
  distinct(PatientNr, Cyclus,Aantalcompl, .keep_all=TRUE) %>% 
  mutate (Ind    = "U4.3",
                        Groep  = scorekaart_name,
          Waarde = ifelse( Aantalcompl >= 1, 1, 0)) %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()

table(U43$Waarde, U43$Cyclus)

U44         <- left_join(PatSel %>% select(PatientNr, Aandoening,Cyclus,Wegingsfactor),
                         compl %>% filter (Omschrijving3=="urineweginfectie")) %>% 
  mutate(Waarde=ifelse(is.na(Omschrijving3),0,1)) %>% 
  group_by(PatientNr,Cyclus) %>% 
  mutate(Aantalcompl=sum(Waarde)) %>% 
  distinct(PatientNr,Cyclus, Aantalcompl, .keep_all=TRUE) %>% 
  mutate (Ind    = "U4.4",
                        Groep  = scorekaart_name,
          Waarde = ifelse( Aantalcompl >= 1, 1, 0)) %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()

table(U44$Waarde, U44$Cyclus)

# ---------- U5 VBI  ----------
VBI2         <- left_join(PatSel %>% select(Aandoening,  Cyclus, PatientNr,OntslagDatumTijd,OpnameDatumTijd),
                         VBI) %>% 
  mutate(Waarde =ifelse(BeginDatum<=OntslagDatumTijd & BeginDatum>=OpnameDatumTijd,1,0)) %>% 
  filter(Waarde==1) %>% 
  distinct( PatientNr,Cyclus, .keep_all=TRUE)

unique(VBI2$VBIomschrijving)
#[1] "Polsband"              "Extra laag bed"        "Tentbed"               "Tafelblad"             "Onrustband"            "Bewegingsalarm"        "Stoelfixatie"          "Veiligheidshandschoen" "Cameratoezicht"       
#[10] "Bedhekken"     
unique(VBI2$Verzetomschrijving)
#[1] "Geen"                  "<< Onbekend >>"        "Fysiek"                "Met eigen toestemming"

U51 <- left_join(PatSel,
                    VBI2) %>% 
  mutate (Ind    = "U5.1",
                        Groep  = scorekaart_name,
          Waarde = ifelse( is.na(Waarde), 0, Waarde),
          Wegingsfactor= 1) %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()

table(U51$Waarde, U51$Cyclus)

# ---------- U61 Ontslagbestemming  ----------
OM_Bestemming  <- c( "Instelling (revalidatie)","Instelling (anders)","Verpleeg- / verzorgingshuis","Eigen woonomgeving met zorg",   
"Hospice","Ander ziekenhuis","Terug naar initiele instelling", "Instelling (psychiatrisch)")  #"Overleden (met obductie)","Onbekend","Tegen advies" ,"Overleden (zonder obductie)" )  


U61       <- left_join(PatSel,
                       Opname %>% select(OpnameNr,HerkomstOmschrijving,BestemmingOmschrijving))%>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>% 
             filter(HerkomstOmschrijving=="Eigen woonomgeving") %>% 
  mutate (Ind    = "U6.1",
                        Groep  = scorekaart_name,
          Waarde = ifelse( BestemmingOmschrijving %in% OM_Bestemming, 1, 0))  %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

table(U61$Waarde, U61$Cyclus)

# Indicator K2.1 - Aantal ligdagen per opname

Verrichting_K1  <- left_join(PatSel %>% select(PatientNr, Aandoening, Cyclus,  OpnameDatumTijd, OntslagDatumTijd),
                               Verrichting) %>%
                                 filter(     Verrichtingdatum>=OpnameDatumTijd & (difftime(Verrichtingdatum, OntslagDatumTijd, units = "days"))<90) %>% 
                      filter(  (  ZACode %in% ZA_ligduur)|(    ZACode %in% ZA_ligduurIC)|(    ZACode %in% ZA_verkeerdbed ) )%>%
                      distinct(PatientNr,Verrichtingdatum, .keep_all=TRUE) %>% 
                      mutate(vpd = ifelse(( ZACode %in% ZA_ligduur), 1,0),
                             icd = ifelse(( ZACode %in% ZA_ligduurIC), 1,0),
                             vbd = ifelse(( ZACode %in% ZA_verkeerdbed), 1,0),
                             fu = ifelse(Verrichtingdatum>OntslagDatumTijd,1,0))


# ---------- K1.1 Gemiddeld aantal verpleegdagen tijdens primaire opname  ----------
K11    <- left_join(PatSel, 
                      Verrichting_K1 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(vpd==1 & fu==0) %>% 
                      mutate(  Waarde = sum(vpd)) %>%
                      distinct(PatientNr, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                  Ind    = "K1.1",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
                      summary(K11$Waarde,K11$Cyclus)
                      sum(K11$Waarde)
K11 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))
# ---------- K1.1 Gemiddeld aantal verpleegdagen tijdens FU  ----------
K12    <- left_join(PatSel, 
                    Verrichting_K1 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(vpd==1 & fu==1) %>% 
                      mutate(  Waarde = sum(vpd)) %>%
                      distinct(PatientNr, Waarde)) %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>% 
                        mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                    Ind    = "K1.2",
                                    Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

K12 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))                    
# ---------- K1.1 Gemiddeld aantal IC tijdens PO  ----------
K13    <- left_join(PatSel, 
                       Verrichting_K1 %>%
                       group_by(   PatientNr, Cyclus) %>%
                       filter(icd==1 & fu==0) %>% 
                       mutate(  Waarde = sum(icd)) %>%
                       distinct(PatientNr, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                    Ind    = "K1.3",
                                    Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

K13 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))                    
                      
# ---------- K1.4 Gemiddeld aantal IC dagen tijdens FU  ----------
K14    <- left_join(PatSel, 
                       Verrichting_K1 %>%
                       group_by(   PatientNr, Cyclus) %>%
                       filter(icd==1 & fu==1) %>% 
                       mutate(  Waarde = sum(icd)) %>%
                       distinct(PatientNr, Waarde))%>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>% 
                       mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                    Ind    = "K1.4",
                                    Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

K14 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))                     
# ---------- K2.1 Gemiddeld aantal verkeerde beddagen PO  ----------
K21    <- left_join(PatSel, 
                      Verrichting_K1 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(vbd==1 & fu!=1) %>% 
                      mutate(  Waarde = sum(vbd)) %>%
                      distinct(PatientNr, Cyclus, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                    Ind    = "K2.1",
                                    Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
K21 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))              

# ---------- K2.1 Gemiddeld aantal verkeerde beddagen PO en FU  ----------
K22    <- left_join(PatSel, 
                      Verrichting_K1 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(vbd==1 & fu==1) %>% 
                      mutate(  Waarde = sum(vbd)) %>%
                      distinct(PatientNr, Cyclus, Waarde)) %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>%  
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                    Ind    = "K2.2",
                                    Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
K22 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))       
# ---------- K3 Verrichtingen per zorgprofielklass ----------

unique(VerrichtingZP$ZorgprofielklasseOmschrijving)                      
zpklasse <- VerrichtingZP %>% distinct(ZorgprofielklasseCode,ZorgprofielklasseOmschrijving)
 #                   [1] "KLINIEK"                                                   "POLIKLINIEK-, EERSTE HULPBEZOEK EN CONSULTATIE OP AFSTAND" "KL. CHEMIE EN HAEMATOLOGIE"                               
      #                [4] "(PARA)MEDISCHE EN ONDERSTEUNENDE FUNCTIES"                 "MICROBIOLOGIE EN PARASITOLOGIE"                            "Niet in profiel opgenomen"                                
       #               [7] "OVERIGE LABORATORIUMVERRICHTINGEN"                         "DIAGNOSTISCHE ACTIVITEITEN"                                "OVERIGE THERAPEUTISCHE ACTIVITEITEN"                      
        #              [10] "NIET IN PROFIEL MEEGENOMEN"                                "BEELDVORMENDE DIAGNOSTIEK"                                 "OVERIGE ZORGACTIVITEITEN T.B.V. AFLEIDING"                
         #             [13] "DAGVERPLEGING"                                             "REVALIDATIE"                                               "PATHOLOGIE"                                               
          ###          [22] "IC ZORGACTIVITEITEN NIET ZIJNDE IC-BEHANDELDAG"           




Verrichting_K3  <- left_join(PatSel %>% select(PatientNr, Aandoening, Cyclus,  OpnameDatumTijd, OntslagDatumTijd),
                               VerrichtingZP %>% select(PatientNr, ZACode, Verrichtingdatum, ZorgprofielklasseCode, ZorgprofielklasseOmschrijving)) %>%
                                 filter(     Verrichtingdatum>=OpnameDatumTijd & Verrichtingdatum<= OntslagDatumTijd) %>% 
                     # filter(  (  ZACode %in% ZA_ligduur)|(    ZACode %in% ZA_ligduurIC)|(    ZACode %in% ZA_verkeerdbed ) )%>%
                     # distinct(PatientNr,Groep,Verrichtingdatum, .keep_all=TRUE) %>% 
                      mutate(Diagnostiek = ifelse( ZorgprofielklasseCode =="04", 1,0),
                             Beeldvormend = ifelse(ZorgprofielklasseCode =="07", 1,0),
                             Operatief = ifelse(ZorgprofielklasseCode =="05", 1,0),
                             Paramedisch = ifelse(ZorgprofielklasseCode =="12", 1,0))
                      
  
# ---------- K3.1 Gemiddeld aantal diagnostische activiteiten tijdens primaire opname  ----------
K31    <- left_join(PatSel, 
                      Verrichting_K3 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(Diagnostiek==1) %>% 
                      mutate(  Waarde = sum(Diagnostiek)) %>%
                      distinct(PatientNr, Cyclus,Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                  Ind    = "K3.1",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
K31 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))       

# ---------- K3.1 Gemiddeld aantal beeldvormende activiteiten tijdens primaire opname  ----------
K32    <- left_join(PatSel, 
                      Verrichting_K3 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(Beeldvormend==1) %>% 
                      mutate(  Waarde = sum(Beeldvormend)) %>%
                      distinct(PatientNr,Cyclus, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                  Ind    = "K3.2",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

K32 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))                      
# ---------- K3.1 Gemiddeld aantal operatieve activiteiten tijdens primaire opname  ----------
K33    <- left_join(PatSel, 
                      Verrichting_K3 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(Operatief==1) %>% 
                      mutate(  Waarde = sum(Operatief)) %>%
                      distinct(PatientNr,Cyclus, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,1),
                        Groep  = scorekaart_name,
                                  Ind    = "K3.3",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

table(K33$Waarde,K33$Cyclus)

                      
# ---------- K3.1 Gemiddeld aantal paramedische activiteiten tijdens primaire opname  ----------
K34    <- left_join(PatSel, 
                      Verrichting_K3 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(Paramedisch==1) %>% 
                      mutate(  Waarde = sum(Paramedisch)) %>%
                      distinct(PatientNr,Cyclus, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                  Ind    = "K3.4",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
 
K34 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde)) 

###-------------------------------------------------------------------------------------------------------
###----------------------------------- K3 Polibezoeken -----------------------------------------------------
###-------------------------------------------------------------------------------------------------------

## Polibezoeken, tel consulten en SEH 
consulten <- left_join(PatSel %>% select(PatientNr, Aandoening, Cyclus,  OpnameDatumTijd, OntslagDatumTijd),
                               Verrichting) %>%
                         filter(     Verrichtingdatum>=OntslagDatumTijd & (difftime(Verrichtingdatum, OntslagDatumTijd, units = "days"))<90) %>% 
                      filter(  (ZACode %in% ZA_consult | ZACode %in% ZA_SEH)) %>% 
                        mutate(Aantal=1)
                                      

# Indicator K3.1 - Polibezoeken en tel consulten 
## Polibezoeken gekoppeld aan SEH bezoek eruit halen
SEH_bezoeken  <- consulten %>%
                        filter(     ZACode %in% ZA_SEH) %>%
                        group_by(   PatientNr,Cyclus, Verrichtingdatum, AGB_CodeUitvoerder) %>%
                        mutate(     AantalSEH  = sum(Aantal)) %>%
                        distinct(   PatientNr, Verrichtingdatum, AGB_CodeUitvoerder, AantalSEH )

consulten_2  <-    merge(      x      = consulten %>%
                                    filter( ZACode %in% ZA_consult) %>%
                                    group_by(PatientNr, Cyclus, Verrichtingdatum, AGB_CodeUitvoerder) %>%
                                    mutate(  AantalPoli  = sum(Aantal)) %>%
                                    distinct(PatientNr,Cyclus, Verrichtingdatum, AGB_CodeUitvoerder,  .keep_all = TRUE),
                                    y      = SEH_bezoeken,
                                    by     = c("PatientNr","Cyclus", "Verrichtingdatum", "AGB_CodeUitvoerder"),
                                    all.x  = TRUE) %>%
                        mutate(     Aantal = ifelse(AantalPoli - AantalSEH <  0 & !is.na(AantalSEH), 0, AantalPoli),
                                    Aantal = ifelse(AantalPoli - AantalSEH >= 0 & !is.na(AantalSEH), AantalPoli-AantalSEH, AantalPoli)) %>%
                        filter(     Aantal>0) %>% distinct(PatientNr,  AGB_CodeUitvoerder, .keep_all = TRUE)

# K4         <- left_join(PatSel, consulten_2 %>% select(PatientNr,  AGB_CodeUitvoerder,Aantal)) %>% 
#                         group_by(PatientNr) %>%
#                           mutate(  Waarde = sum(Aantal)) %>% 
#                         mutate(     Ind    = "K4",
#                         Groep  = scorekaart_name,
#                                     Waarde = ifelse(!is.na(Waarde),Waarde,0),
#                                     Wegingsfactor = 1) %>%
#                          distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()
#     summary(K41$Waarde)

# Subindicator K3.1.1 - Polibezoeken 
poli_tot  <- consulten_2 %>%
  filter(ZACode %in% ZA_poli)

K41         <- left_join(PatSel, poli_tot %>% select(PatientNr,Cyclus, AGB_CodeUitvoerder,Aantal)) %>% 
                        group_by(PatientNr,Cyclus) %>%
                          mutate(  Waarde = sum(Aantal))  %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>%  
                        mutate(     Ind    = "K4.1",
                        Groep  = scorekaart_name,
                                    Waarde = ifelse(!is.na(Waarde),Waarde,0),
                                    Wegingsfactor = 1) %>%
                         distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()

K41 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde)) 

# Subindicator K3.1.2 - Tel consulten 
tele_int  <- consulten_2 %>%
      filter(ZACode %in% ZA_tel)

K42         <- left_join(PatSel, tele_int %>% select(PatientNr,Cyclus,  AGB_CodeUitvoerder,Aantal)) %>% 
                        group_by(PatientNr,Cyclus) %>%
                          mutate(  Waarde = sum(Aantal))  %>% 
          filter(OpnameNr%in%noemerselectieFU$OpnameNr) %>%  
                        mutate(     Ind    = "K4.2",
                        Groep  = scorekaart_name,
                                    Waarde = ifelse(!is.na(Waarde),Waarde,0),
                                    Wegingsfactor = 1) %>%
                         distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()
 
K42 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde)) 

# ---------- P.1.1 Percentage patienten dat is binnengekomen via de SEH  ----------    


SEHtest <- left_join(PatSel,
                     SEH %>% select(PatientNr, TijdsduurSehBezoek, OpnameNr) )
          
P11         <- SEHtest %>%
                        mutate(     Ind    = "P1.1",
                        Groep  = scorekaart_name,
                                    Waarde = ifelse(is.na(TijdsduurSehBezoek),0,1),
                                    Wegingsfactor = 1) %>%
                         distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
    table(P11$Waarde,P11$Groep)      
    
P12         <- SEHtest %>% filter(!is.na(TijdsduurSehBezoek)) %>% 
                        mutate(     Ind    = "P1.2",
                        Groep  = scorekaart_name,
                                    Waarde = TijdsduurSehBezoek,
                                    Wegingsfactor = 1) %>%
                         distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
P12 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))

# P2 aantal afdelingen   
P21 <- left_join (PatSel %>% select(PatientNr, Aandoening, Cyclus, Wegingsfactor,OpnameDatumTijd, OntslagDatumTijd),
                        OpnameMutaties %>%   
                             distinct (PatientNr, OpnameDatumTijd, OntslagDatumTijd, AfdelingCode)) %>%
                             filter(OpnameDatumTijd==OpnameDatumTijd)%>% 
                             mutate(Aantal=1) %>% 
                 group_by(PatientNr, Cyclus) %>%
                          mutate(  Waarde = sum(Aantal)) %>% 
                        mutate(     Ind    = "P2.1",
                        Groep  = scorekaart_name,
                                    Waarde = ifelse(!is.na(Waarde),Waarde,0),
                                    Wegingsfactor = 1) %>%
                         distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()
P21 %>% 
  group_by(Cyclus) %>%  summarize(teller=sum(Waarde),
                                  noemer=sum(Wegingsfactor))                     

# P22 aantal afdelingen  Buitenbedden hebben we niet in CWZ 
    
    
# ---------- P.3 screening binnen 24/48 uur ----------    


P31         <- PatSel %>% 
                        mutate(     Ind    = "P3.1",
                        Groep  = scorekaart_name,
                                    Waarde = ifelse(is.na(DelierTijd),0,ifelse(as.numeric(DelierTijd)>-8 &as.numeric(DelierTijd)<2,1,0)),
                                    Wegingsfactor = ifelse(is.na(DelierTijd),0,1)) %>%
                         distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
    table(P31$Waarde,P31$Cyclus)
    
    
# P311         <- PatSel %>% 
#                         mutate(     Ind    = "P3.1.1",
#                         Groep  = scorekaart_name,
#                                     Waarde = ifelse(is.na(DelierTijd),0,ifelse(as.numeric(DelierTijd)>1 &as.numeric(DelierTijd)<3,1,0)),
#                                     Wegingsfactor = ifelse(is.na(DelierTijd),0,1)) %>%
#                          distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
#     table(P311$Waarde,P311$Groep)
    
P32         <- PatSel %>% 
                        mutate(     Ind    = "P3.2",
                        Groep  = scorekaart_name,
                                    Waarde = ifelse(is.na(ValTijd),0,ifelse(as.numeric(ValTijd)>-8 &as.numeric(ValTijd)<2,1,0)),
                                    Wegingsfactor = ifelse(is.na(ValTijd),0,1)) %>%
                         distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
    table(P32$Waarde,P32$Cyclus)
    
    
# P321         <- PatSel %>% 
#                         mutate(     Ind    = "P3.2.1",
#                         Groep  = scorekaart_name,
#                                     Waarde = ifelse(is.na(ValTijd),0,ifelse(as.numeric(ValTijd)>1 &as.numeric(ValTijd)<3,1,0)),
#                                     Wegingsfactor = ifelse(is.na(ValTijd),0,1)) %>%
#                          distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
#     table(P321$Waarde,P321$Groep)

P33          <- PatSel %>% 
                        mutate(     Ind    = "P3.3",
                        Groep  = scorekaart_name,
                                    Waarde = ifelse(is.na(FysiekTijd),0,ifelse(as.numeric(FysiekTijd)>-8 &as.numeric(FysiekTijd)<2,1,0)),
                                    Wegingsfactor = ifelse(is.na(FysiekTijd),0,1)) %>%
                         distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
    table(P33$Waarde,P33$Cyclus)
    
    
# P331         <- PatSel %>% 
#                         mutate(     Ind    = "P3.3.1",
#                         Groep  = scorekaart_name,
#                                     Waarde = ifelse(is.na(FysiekTijd),0,ifelse(as.numeric(FysiekTijd)>1 &as.numeric(FysiekTijd)<3,1,0)),
#                                     Wegingsfactor = ifelse(is.na(FysiekTijd),0,1)) %>%
#                          distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
#     table(P331$Waarde,P331$Groep)
    
P34          <- PatSel %>% 
                        mutate(     Ind    = "P3.4",
                        Groep  = scorekaart_name,
                                    Waarde = ifelse(is.na(OndervoedTijd),0,ifelse(as.numeric(OndervoedTijd)>-8 &as.numeric(OndervoedTijd)<2,1,0)),
                                    Wegingsfactor = ifelse(is.na(OndervoedTijd),0,1)) %>%
                         distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
    table(P34$Waarde,P34$Cyclus)
    
    
# P341         <- PatSel %>% 
#                         mutate(     Ind    = "P3.4.1",
#                         Groep  = scorekaart_name,
#                                     Waarde = ifelse(is.na(OndervoedTijd),0,ifelse(as.numeric(OndervoedTijd)>1 &as.numeric(OndervoedTijd)<3,1,0)),
#                                     Wegingsfactor = ifelse(is.na(OndervoedTijd),0,1)) %>%
#                          distinct (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
#     table(P341$Waarde)
        
###-------------------------------------------------------------------------------------------------------
###----------------------------------- P4 Betrokken specialisten -----------------------------------------------------
###-------------------------------------------------------------------------------------------------------

##### Medebehandeling Collumfractuur
        
HeupDBC <- Subtraject %>% filter(DiagnoseCode %in% DBC_CHI & AGB_Code != "INT") %>% 
              left_join(Verrichting %>% filter (OpnameNr!="<< Onbekend >>" ) %>%  select(OpnameNr,SubtrajectNr)) %>% filter (!is.na(OpnameNr)) %>% 
              distinct(OpnameNr,SubtrajectNr,.keep_all=TRUE)

      
PatselHeup <- PatSel %>% mutate(Heup=ifelse(OpnameNr %in% HeupDBC$OpnameNr,1,0))

table(PatselHeup$Heup)
## Polibezoeken, tel consulten en SEH 
medeconsulten <- left_join(PatselHeup,
          Verrichting %>% select(PatientNr,Verrichtingdatum,ZACode,AGB_CodeUitvoerder,ZAOmschrijving) %>%
                      filter(ZACode %in% ZA_medeConsult &  AGB_CodeUitvoerder=="GER")) %>% ##### in het CWZ geen internist ouderengeneeskunde, zelf toevoegen indien aanwezig!!!!
          mutate(Consult=ifelse(Verrichtingdatum>=OpnameDatumTijd & Verrichtingdatum<=OntslagDatumTijd,1,0 )) %>% 
          group_by(OpnameNr) %>% 
          mutate(Aantal=sum(Consult)) %>% 
          ungroup() %>% distinct(OpnameNr,.keep_all=TRUE)

P41  <- medeconsulten  %>% 
                      mutate(     Waarde = ifelse(is.na(Consult),0,Consult),
                        Groep  = scorekaart_name,
                                  Ind    = "P4.1",
                                  Wegingsfactor = 1) %>%
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
                      summary(P41$Waarde)
                      sum(P41$Waarde) 
table(P41$Waarde,P41$Cyclus)

P411  <- medeconsulten  %>% filter(Heup==1) %>% 
                      mutate(     Waarde = ifelse(is.na(Consult),0,Consult),
                        Groep  = scorekaart_name,
                                  Ind    = "P4.1.1",
                                  Wegingsfactor = 1) %>%
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
table(P411$Waarde,P411$Cyclus)
                      
# P412 <- left_join(medeconsulten %>% select (PatientNr,Aantal,Consult),U41d) %>% 
#           mutate(Wegingsfactor=Waarde) %>% filter(Wegingsfactor==1) %>% 
#            mutate(     Waarde = ifelse(is.na(Consult),0,Consult),
#                         Groep  = scorekaart_name,
#                                   Ind    = "P4.1.2") %>%
#                         ungroup() %>% 
#                         select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
# table(P412$Waarde)

                      
medeconsultensp <- left_join(PatSel ,
                               Verrichting %>% select(PatientNr,Verrichtingdatum,ZACode,AGB_CodeUitvoerder,ZAOmschrijving) %>%
                       filter( AGB_CodeUitvoerder=="GER")) %>% ##### in het CWZ geen internist ouderengeneeskunde, zelf toevoegen indien aanwezig!!!!
           mutate(Consult=ifelse(Verrichtingdatum>=OpnameDatumTijd & Verrichtingdatum<=OntslagDatumTijd,1,0 )) %>% 
                        group_by(OpnameNr) %>% 
          mutate(Aantal=sum(Consult)) %>% 
          ungroup() %>% distinct(OpnameNr,.keep_all=TRUE)
    
Verrichting_P4  <- left_join(PatSel %>% select(PatientNr, Aandoening, Cyclus,  OpnameDatumTijd, OntslagDatumTijd,
                                                   DelierRisico,OndervoedRisico,ValRisico,FysiekRisico),
                               Verrichting %>% select(PatientNr, ZACode, Verrichtingdatum, ZAOmschrijving)) %>%
                                 filter(     Verrichtingdatum>=OpnameDatumTijd & Verrichtingdatum<=OntslagDatumTijd) %>% 
                      filter(  (  ZACode %in% ZA_Fysiotherapeut)|(    ZACode %in% ZA_Ergotherapeut)|(    ZACode %in% ZA_Dietist ) |(    ZACode %in% ZA_Logopedist )|
                                         (    ZACode %in% ZA_psych) |(    ZACode %in% ZA_Maatschappelijk ))%>%
                      distinct(PatientNr,ZACode, .keep_all=TRUE) %>% 
                      mutate(fys = ifelse(( ZACode %in% ZA_Fysiotherapeut), 1,0),
                             ergo = ifelse(( ZACode %in% ZA_Ergotherapeut), 1,0),
                             diet = ifelse(( ZACode %in% ZA_Dietist), 1,0),
                             logo = ifelse(( ZACode %in% ZA_Logopedist), 1,0),
                             psy = ifelse(( ZACode %in% ZA_psych), 1,0),
                             maat = ifelse(( ZACode %in% ZA_Maatschappelijk), 1,0))

# ---------- P4.2 Psychosociale contacten tijdens opname  ----------
P421    <- left_join(PatSel, 
                      Verrichting_P4 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(maat==1) %>% 
                      mutate(  Waarde = sum(maat)) %>%
                      distinct(PatientNr, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                  Ind    = "P4.2.1",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
                      summary(P421$Waarde)
                      sum(P421$Waarde)
P421 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))                  

P422    <- left_join(PatSel, 
                      Verrichting_P4 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(psy==1) %>% 
                      mutate(  Waarde = sum(psy)) %>%
                      distinct(PatientNr, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                  Ind    = "P4.2.2",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

P422 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))                  
#### ---------- P4.3 Paramedisch contacten tijdens opname  ----------
P431    <- left_join(PatSel, 
                      Verrichting_P4 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(fys==1) %>% 
                      mutate(  Waarde = sum(fys)) %>%
                      distinct(PatientNr, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                  Ind    = "P4.3.1",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
P431 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))                  

P432    <- left_join(PatSel, 
                      Verrichting_P4 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(ergo==1) %>% 
                      mutate(  Waarde = sum(ergo)) %>%
                      distinct(PatientNr, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                  Ind    = "P4.3.2",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

P432 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))                                               
                      
P433    <- left_join(PatSel, 
                      Verrichting_P4 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(logo==1) %>% 
                      mutate(  Waarde = sum(logo)) %>%
                      distinct(PatientNr, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                  Ind    = "P4.3.3",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

P433 %>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))         
                      
P434    <- left_join(PatSel, 
                      Verrichting_P4 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(diet==1) %>% 
                      mutate(  Waarde = sum(diet)) %>%
                      distinct(PatientNr, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,Waarde),
                        Groep  = scorekaart_name,
                                  Ind    = "P4.3.4",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
 
P434%>% 
  group_by(Cyclus) %>% 
  summarize(med=median(Waarde),
            teller=sum(Waarde),
            Noemer=sum(Wegingsfactor),
            gemid=mean(Waarde),
            mini=min(Waarde),
            q5 = quantile(Waarde, 0.05),
            q1 = quantile(Waarde, 0.25),
            q3 = quantile(Waarde, 0.75),
            q95 = quantile(Waarde, 0.95),
            maxi=max(Waarde))              

#### P42 lukt nog niet: hier staat DUMMY GERIATRIE PSYCHIATRISCH
###-------------------------------------------------------------------------------------------------------
###----------------------------------- P6 Ingezette behandeling-------------------------------------------
###-------------------------------------------------------------------------------------------------------
DOSP         <- left_join(PatSel %>% select(Aandoening, Cyclus, PatientNr,OntslagDatumTijd,OpnameDatumTijd),
                         Vragenlijst %>% filter (Vraagstelling=="DOS schaal eindscore" ) %>% select(PatientNr,Beantwoordingsdatum, Antwoord)) %>% 
  mutate(Waarde =ifelse(Beantwoordingsdatum<=OntslagDatumTijd & Beantwoordingsdatum>=OpnameDatumTijd,1,0)) %>% 
  filter(Waarde==1) %>% 
  distinct( PatientNr,Cyclus,.keep_all=TRUE)
 
P61 <- left_join(PatSel,
                    DOSP) %>% 
  mutate (Ind    = "P6.1",
                        Groep  = scorekaart_name,
          Waarde = ifelse( is.na(Waarde), 0, Waarde),
          Wegingsfactor= 1) %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()

table(P61$Waarde, P61$Cyclus)

P641 <- left_join(PatSel,
                    DOSP) %>% 
          filter(DelierRisico==1) %>% 
  mutate (Ind    = "P6.4.1",
                        Groep  = scorekaart_name,
          Waarde = ifelse( is.na(Waarde), 0, Waarde),
          Wegingsfactor= 1) %>%
  select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor) %>% ungroup()
table(P641$Waarde, P641$Cyclus)
# ---------- P4.3 Paramedisch contacten tijdens opname  ----------
P642    <- left_join(PatSel %>% filter(FysiekRisico==1), 
                      Verrichting_P4 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter((fys==1)|(ergo==1)) %>% 
                      mutate(  Waarde = sum(fys)+sum(ergo)) %>%
                      distinct(PatientNr, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,1),
                        Groep  = scorekaart_name,
                                  Ind    = "P6.4.2",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

table(P642$Waarde,P642$Cyclus)
                     
P643   <- left_join(PatSel %>% filter(OndervoedRisico==1), 
                      Verrichting_P4 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(diet==1) %>% 
                      mutate(  Waarde = sum(diet)) %>%
                      distinct(PatientNr, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,1),
                        Groep  = scorekaart_name,
                                  Ind    = "P6.4.3",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)
 
table(P643$Waarde,P643$Cyclus)

P644    <- left_join(PatSel %>% filter(ValRisico==1), 
                      Verrichting_P4 %>%
                      group_by(   PatientNr,Cyclus) %>%
                      filter(fys==1) %>% 
                      mutate(  Waarde = sum(fys)) %>%
                      distinct(PatientNr, Waarde)) %>% 
                      mutate(     Waarde = ifelse(is.na(Waarde),0,1),
                        Groep  = scorekaart_name,
                                  Ind    = "P6.4.4",
                                  Wegingsfactor = 1) %>%
                        ungroup() %>% 
                        select (Aandoening, Groep, Cyclus, PatientNr, Ind, Waarde, Wegingsfactor)

table(P644$Waarde,P644$Cyclus)
                     
################TOT HIER GEKOMEN
 indicatoren       <-    rbind(U11,U21,U22,U23,U24,U31,U32,U33,U41c,
                               U411c,U411d,U412c,U41d,U412d,
                               U413c,U413d,U42,U421,U422,U423,U43,U44,U51,U61,K11,K12,K13,K14,
                               K21,K31,K32,K33,K34,K41,K42,P11,
                               P12,P21,P31,P311,P32,P321,P33,P331,P34,P341,P41,
                               P411,P412,P421,P422,P431,P432,P433,P434,P61,P641,P642,P643,P644,
                               lft,gsl,bmi,rok,dem,red,urg,woonvoor,woonontsl,
                               medvoor,medontsl,opnamespec)  
 indicatoren <- indicatoren %>% filter(Groep=="Scorekaart"|Groep=="Case-mix")
 
 
 write.csv(PatSel,"PatSelectie.csv")
 write.csv(indicatorentest,"indicatoren.csv")
 save.image("M:/VBHC/15. Kwetsbare ouderen/Cyclus 1/3. Werkbestanden/R/KwetsbareOuderenCyclus1/9april.RData")   
 
    test2 <- as.data.frame(head(PatSel))
write.csv(test2,"dataframepat.csv")
rm(list=ls(pattern="test")) ###even opruimen

table(P11$Waarde)
