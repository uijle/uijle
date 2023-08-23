# VBHC Aandoening: Diabetes
# Cyclus 7
# Script 2
#
#
# Datum: 13-12-2021
#
#
# R and library versions
# R version:  3.6.2 (2018-04-23)
# dplyr:      0.8.5
# lubridate:  1.7.8
# readxl:     1.3.1

source("./Diabetes_1_voorbereidingsscript.R")

###---------------------------------------------------------------------------------------------------------------
### LOAD EXCEL SHEET (regel 20 tm 33 is niet nodig wanneer gebruik wordt gemaakt van een bron dataframe) ###
###---------------------------------------------------------------------------------------------------------------
Patient_bron            <- read_excel(fname, sheet = "Patient")                # load xlsx data
PatientSelectie_bron    <- read_excel(fname, sheet = "PatientSelectie")
Subtraject_bron         <- read_excel(fname, sheet = "Subtraject")
                                #mutate(ZorgType     = ifelse(ZorgType == "R", 11, ZorgType),
                                       #ZorgType     = ifelse(ZorgType == "L", 21, ZorgType))
Verrichting_bron        <- read_excel(fname, sheet = "Verrichting")
Probleemlijst_bron      <- read_excel(fname, sheet = "Probleemlijst")
Jaarcontrole_bron       <- read_excel(fname, sheet = "Jaarcontrole")
Afspraak_bron           <- read_excel(fname, sheet = "Afspraak")
Opname_bron             <- read_excel(fname, sheet = "Opname")
Medicatie_bron          <- read_excel(fname, sheet = "Medicatie")
Lab_bron                <- read_excel(fname, sheet = "Lab") %>%
                             filter(Uitslag    != "n.v.t"   | Uitslag !=  "Zie opm." |
                                    Uitslag    != "-volgt-" | Uitslag !=  "vervalt")
Metingen_bron           <- read_excel(fname, sheet = "Metingen")

###---------------------------------------------------------------------------------------------------------------
### INLEZEN DATA DICTIONARY (regel 38 tm 48 is niet nodig wanneer bronbestand in excel wordt gebruikt) ###
###---------------------------------------------------------------------------------------------------------------
#load("Patient")
#load("Subtraject")
#load("Verrichting")
#load("Probleemlijst")
#load("Jaarcontrole")
#load("Afspraak")
#load("Opname")
#load("Medicatie")
#load("Lab")
#load("Metingen")
#load("DPARD")

###---------------------------------------------------------------------------------------------------------------
### INLEZEN DATA DICTIONARY ###
###---------------------------------------------------------------------------------------------------------------

Patient <- Patient_bron          %>% mutate (
                                        PatientNr          = as.character(PatientNr),
                                        Geslacht           = as.character(Geslacht),
                                        GeboorteDatum      = as.Date(GeboorteDatum, format = "%Y-%m-%d"),
                                        OverlijdensDatum   = as.Date(OverlijdensDatum, format = "%Y-%m-%d")) %>%
                               distinct(PatientNr,Geslacht,GeboorteDatum,OverlijdensDatum)

PatientSelectie <- PatientSelectie_bron    %>% mutate(
                                        PatientNr = as.character(PatientNr),
                                        Groep = as.numeric(Groep),
                                        Cyclus = as.character(Cyclus),
                                        TypeDM = as.character(TypeDM)) %>%
                              distinct(PatientNr,Groep,Cyclus,TypeDM)

Subtraject <- Subtraject_bron    %>% mutate (
                                        PatientNr                = as.character(PatientNr),
                                        ZorgtrajectNr            = as.numeric(ZorgtrajectNr),
                                        SubtrajectNr             = as.numeric(SubtrajectNr),
                                        SpecialismeCode          = as.character(AGB_Code),
                                        DiagnoseCode             = as.character(DiagnoseCode),
                                        OpeningsDatum            = as.Date(OpeningsDatumDBC, format = "%Y-%m-%d"),
                                        SluitingsDatum           = as.Date(SluitingsDatumDBC, format = "%Y-%m-%d"),
                                        Status                   = as.character(StatusDBC),
                                        LeegSubtraject           = as.character(LeegSubtraject),
                                        ZorgType                 = as.character(ZorgType)) %>%
                               distinct(PatientNr,ZorgtrajectNr,SubtrajectNr,SpecialismeCode,DiagnoseCode,OpeningsDatum,
                                        SluitingsDatum,Status,LeegSubtraject,ZorgType)

Verrichting <- Verrichting_bron  %>% mutate (
                                        PatientNr          = as.character(PatientNr),
                                        ZorgtrajectNr      = as.numeric(ZorgtrajectNr),
                                        SubtrajectNr       = as.numeric(SubtrajectNr),
                                        OpnameNr           = as.numeric(OpnameNr),
                                        VerrichtingNr      = as.numeric(VerrichtingNr),
                                        ZACode             = as.character(ZACode),
                                        ZAOmschrijving     = as.character(ZAOmschrijving),
                                        Verrichtingdatum   = as.Date(Verrichtingdatum, format = "%Y-%m-%d"),
                                        AGB_CodeAanvrager  = as.character(AGB_CodeAanvrager),
                                        AGB_CodeUitvoerder = as.character(AGB_CodeUitvoerder),
                                        ZorgprofielKlasse  = as.character(ZorgprofielKlasse),
                                        Aantal             = as.numeric(Aantal)) %>%
                               distinct(PatientNr,ZorgtrajectNr,SubtrajectNr,OpnameNr,VerrichtingNr,ZACode,
                                        ZAOmschrijving,Verrichtingdatum,AGB_CodeAanvrager,AGB_CodeUitvoerder,ZorgprofielKlasse,Aantal)

Probleemlijst <- Probleemlijst_bron  %>% mutate (
                                        PatientNr          = as.character(PatientNr),
                                        ICDcode            = as.character(ICDcode),
                                        ConstateringsDatum = as.Date(ConstateringsDatum, format = "%Y-%m-%d"))%>%
                               mutate(  ICDcode            = gsub("\\*|\\+", "", ICDcode)) %>%
                               distinct(PatientNr,ICDcode,ConstateringsDatum)

Jaarcontrole <- Jaarcontrole_bron %>% mutate (
                                        PatientNr          = as.character(PatientNr),
                                        JaarcontroleDatum  = as.Date(JaarcontroleDatum, format = "%Y-%m-%d"),
                                        VraagOms           = as.character(VraagOms),
                                        Uitslag            = as.character(Waarde)) %>%
                               distinct(PatientNr,JaarcontroleDatum,VraagOms,Uitslag)

Opname <-  Opname_bron           %>% mutate (
                                        PatientNr                   = as.character(PatientNr),
                                        OpnameNr                    = as.numeric(OpnameNr),
                                        OpnameDatumTijd             = as.Date(OpnameDatumTijd, format = "%Y-%m-%d"),
                                        OntslagDatumTijd            = as.Date(OntslagDatumTijd, format = "%Y-%m-%d"),
                                        OpnameIndicatie             = as.character(OpnameIndicatie),
                                        AGB_CodeOpnameSpecialisme   = as.character(AGB_CodeOpnameSpecialisme),
                                        AGB_CodeOntslagSpecialisme  = as.character(AGB_CodeOntslagSpecialisme),
                                        AGB_CodeAanvrager           = as.character(AGB_CodeAanvrager),
                                        AGB_CodeUitvoerder          = as.character(AGB_CodeUitvoerder),
                                        Spoed                       = as.character(Spoed)) %>%
                               distinct(PatientNr,OpnameNr,OpnameDatumTijd,OntslagDatumTijd,OpnameIndicatie,AGB_CodeOpnameSpecialisme,
                                        AGB_CodeOntslagSpecialisme,AGB_CodeAanvrager,AGB_CodeUitvoerder,Spoed)

Metingen <- Metingen_bron        %>% mutate (
                                        PatientNr          = as.character(PatientNr),
                                        ObservatieOms      = as.character(ObservatieOmschrijving),
                                        ObservatieDatum    = as.Date(ObservatieDatum),
                                        Uitslag            = as.character(Uitslag),
                                        Eenheid            = as.character(Eenheid)) %>%
                               distinct(PatientNr,ObservatieOms,ObservatieDatum,Uitslag,Eenheid)

Medicatie <- Medicatie_bron      %>% mutate (
                                        PatientNr             = as.character(PatientNr),
                                        StartDatum            = as.Date(StartDatum ,  format = "%Y-%m-%d"),
                                        EindDatum             = as.Date(EindDatum ,  format = "%Y-%m-%d"),
                                        ATC_code              = as.character(ATC_code),
                                        ATC_omschrijving      = as.character(ATC_omschrijving)) %>%
                               distinct(PatientNr,StartDatum,EindDatum,ATC_code,ATC_omschrijving)

Lab <- Lab_bron                  %>% mutate (
                                        PatientNr           = as.character(PatientNr),
                                        AfnameDatumTijd     = as.Date(AfnameDatumTijd ,  format = "%Y-%m-%d"),
                                        Uitslag             = as.character(gsub("[<>]", "", Uitslag)),
                                        Eenheid             = as.character(Eenheid),
                                        BepalingOms         = as.character(BepalingOms)) %>%
                                 filter(!is.na(Uitslag)) %>%
                               distinct(PatientNr,AfnameDatumTijd,Uitslag,BepalingOms)

Afspraak <- Afspraak_bron        %>% mutate (
                                        PatientNr          = as.character(PatientNr),
                                        AfspraakDatum      = as.Date(AfspraakDatum, format = "%Y-%m-%d"),
                                        AfspraakStatus     = as.character(AfspraakStatus),
                                        AfspraakCode       = as.character(AfspraakCode),
                                        AfspraakCodeOms    = as.character(AfspraakCodeOms),
                                        AnnuleringsReden   = as.character(AnnuleringsReden),
                                        Afdeling           = as.character(Afdeling),
                                        ConsultType        = as.character(ConsultType),
                                        SpecialistTypeOms  = as.character(SpecialistTypeOms),
                                        Verwijzer          = as.character(Verwijzer)) %>%
                               distinct(PatientNr,AfspraakDatum,AfspraakStatus,AfspraakCode,AfspraakCodeOms,AnnuleringsReden,Afdeling,ConsultType,SpecialistTypeOms,Verwijzer)

###------------------------------------------------------------------------------------------------------------
### Indien gebruik van DPARD: DPARD importeren ###
###------------------------------------------------------------------------------------------------------------
#
#DPARDLocatie     <- "L:/VBHC/Data/Diabetes/Cyclus 2/DPARD/"
#DPARDNaam        <- "DPARDbestand.xlsx"
#
#DPARD_raw <- read_xlsx(paste0(DPARDLocatie, DPARDNaam))
#
#DHFA_raw <-   DPARD_raw %>% mutate (PatientNr                = as.character(PatientNr),
#                                    Aandoening               = "Diabetes",
#                                    datumintake              = as.Date(datumintake, format = "%Y-%m-%d"),
#                                    diagdat                  = as.Date(diagdat, format = "%Y-%m-%d"),
#                                    tabakgebruikstatus       = as.character(tabakgebruikstatus),
#                                    alcoholgebruikstatus     = as.character(alcoholgebruikstatus),
#                                    leefdat                  = as.Date(leefdat, format = "%Y-%m-%d"))
#
###------------------------------------------------------------------------------------------------------------
### Indien gebruik van DPARD: Selecteer op patienten binnen de tijdswindow ###
###------------------------------------------------------------------------------------------------------------
#
#DPARD_raw <- DPARD_raw %>%
#  filter(datumintake<=stopwindow & datumintake>=startwindow)
#
# --- Get unique based on key variables ---
#DPARD_raw      <- DPARD_raw %>% distinct(idcode, datumintake, .keep_all = TRUE)
#
#DPARD          <- left_join(Patient, DPARD_raw, by="PatientNr")
#

###------------------------------------------------------------------------------------------------------------
### OPSLAAN VAN TABBLADEN ###
###------------------------------------------------------------------------------------------------------------
save(Patient, file="Patient")
save(PatientSelectie, file="PatientSelectie")
save(Subtraject, file="Subtraject")
save(Verrichting, file="Verrichting")
save(Probleemlijst, file="Probleemlijst")
save(Jaarcontrole, file="Jaarcontrole")
save(Afspraak, file="Afspraak")
save(Opname, file="Opname")
#save(Operatie, file="Operatie")
save(Medicatie, file="Medicatie")
save(Lab, file="Lab")
save(Metingen, file="Metingen")
#save(Complicatie, file = "Complicatie")
#save(DPARD, file="DPARD")

###------------------------------------------------------------------------------------------------------------
### RENAMEN VAN LABBEPALINGEN ###
###------------------------------------------------------------------------------------------------------------

#eGFR: gebruik CKD-EPI tenzij alleen MDRD gebruikt wordt in je ziekenhuis
#pH in bloed, niet in urine

#                                        Vul hier de lokale naam in:            Naam in script:
Lab <- Lab %>%
  mutate(BepalingOms=ifelse(BepalingOms=="HbA1c",                               "hba1c",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="HbA1c POC",                           "hba1c",BepalingOms),

         #        Alb/kreat ratio alleen uit urine!
         BepalingOms=ifelse(BepalingOms=="Alb/kreat ratio in urine portie",     "acr in urine",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="Alb/kreat ratio in urine verzameld",  "acr in urine",BepalingOms),

         BepalingOms=ifelse(BepalingOms=="Micro Albumine in urine portie",      "micro albumine in urine",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="Micro Albumine in urine verzameld",   "micro albumine in urine",BepalingOms),

         BepalingOms=ifelse(BepalingOms=="Kreatinine in urine portie",          "kreatinine in urine",BepalingOms),

         BepalingOms=ifelse(BepalingOms=="Aniongap",                            "anion gap",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="Aniongap veneus",                     "anion gap",BepalingOms),

         BepalingOms=ifelse(BepalingOms=="anti-GAD",                            "antiGAD",BepalingOms),

         BepalingOms=ifelse(BepalingOms=="Bicarbonaat",                         "bicarbonaat",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="Bicarbonaat veneus",                  "bicarbonaat",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="Bicarbonaat arterieel",               "bicarbonaat",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="Bicarbonaat capillair",               "bicarbonaat",BepalingOms),

         #        eGFR alleen o.b.v. CKD-EPI!
         BepalingOms=ifelse(BepalingOms=="eGFR CKD-EPI",                        "egfr",BepalingOms),

         #        Glucose wordt alleen als naslag gebruikt bij U3.2, zowel nuchter als niet-nuchter
         BepalingOms=ifelse(BepalingOms=="Glucose POC",                         "glucose",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="Glucose",                             "glucose",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="Glucose-nuchter",                     "glucose",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="Glucose metercontrole",               "glucose",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="GlucoseNF",                           "glucose",BepalingOms),

         BepalingOms=ifelse(BepalingOms=="Ketonen in portie urine",             "ketonen in urine",BepalingOms),

         BepalingOms=ifelse(BepalingOms=="LDL-cholesterol",                     "ldl cholesterol",BepalingOms),

         BepalingOms=ifelse(BepalingOms=="pH veneus",                           "ph",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="pH arterieel",                        "ph",BepalingOms),
         BepalingOms=ifelse(BepalingOms=="pH capillair",                        "ph",BepalingOms),

         BepalingOms=ifelse(BepalingOms=="B-OH boterzuur",                      "b-oh boterzuur",BepalingOms)) %>%

  filter(Uitslag!="GEANNULEERD" & Uitslag!="Geen materiaal")

###------------------------------------------------------------------------------------------------------------
### RENAMEN VAN JAARCONTROLEFORMULIER ###
###------------------------------------------------------------------------------------------------------------

#Lange dataset
Jaarcontrole <- Jaarcontrole %>%
  mutate(
         #                               Vul hier de lokale naam in:            Naam in script:
         VraagOms = ifelse(VraagOms==   "SAZ#3988",                            "BloedGlucoseMeter",  VraagOms),
         VraagOms = ifelse(VraagOms==   "EPIC#31000091026",                    "DiagnoseJaar",       VraagOms),
         VraagOms = ifelse(VraagOms==   "SAZ#3987",                            "DiabetesType",       VraagOms),
         VraagOms = ifelse(VraagOms==   "SAZ#4011",                            "Roken",              VraagOms),
         VraagOms = ifelse(VraagOms==   "EPIC#31000060868",                    "RTGCM",              VraagOms),
         VraagOms = ifelse(VraagOms==   "SAZ#3992",                            "RTGCMType",          VraagOms),
         VraagOms = ifelse(VraagOms==   "SAZ#4004",                            "Flash",              VraagOms),
         VraagOms = ifelse(VraagOms==   "SAZ#4816",                            "Insuline",           VraagOms),
         VraagOms = ifelse(VraagOms==   "EPIC#31000045231",                    "DKA",                VraagOms),
         VraagOms = ifelse(VraagOms==   "EPIC#1867",                           "Hypo",               VraagOms),
         VraagOms = ifelse(VraagOms==   "SAZ#3989",                            "Insulinepen",        VraagOms),
         VraagOms = ifelse(VraagOms==   "SAZ#3990",                            "Insulinepomp",       VraagOms),
         VraagOms = ifelse(VraagOms==   "SAZ#4012",                            "Alcohol",            VraagOms),
         VraagOms = ifelse(VraagOms==   "EPIC#1951",                           "AfspraakType",       VraagOms),
         VraagOms = ifelse(VraagOms==   "EPIC#1865",                           "Opname",             VraagOms),
         VraagOms = ifelse(VraagOms==   "EPIC#48428",                          "Neuropathie",        VraagOms)) %>%
  filter(!is.na(VraagOms)) %>%
  group_by(PatientNr, VraagOms) %>%
  filter(JaarcontroleDatum == max(JaarcontroleDatum)) %>%
  distinct() %>%
  ungroup()
