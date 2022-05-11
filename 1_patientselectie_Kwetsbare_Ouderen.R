###################################################################################################
#
# Doel script: Patiëntselectie en voorbereiding data
# Aandoening: Kwetsbare Ouderen
# Output: Patiëntselectie en databronnen
# Auteur: Jacco Westeneng en Cornelieke van Steenbeek
# Datum: 20-04-2021
# Versie: 1
#
###################################################################################################

# 0. Initialisatie --------------------------------------------------------------------------------

# Workspace leegmaken
rm(list=ls())

path          <- "P:/BB/Value Based Health Care/Analisten/"
folder        <- "Kwetsbare Ouderen/Cyc1/Data"

# Set working directory
setwd(paste0(path, folder))

# Libraries
library(dplyr)        
library(lubridate)    
library(readxl)       
library(tidyr)
library(openxlsx)
library(stringi)

# Settings
case_mix_name <- "Case-mix"
scorekaart_name <- "Scorekaart"

inc_start        <- as.Date("2021-01-01")
inc_eind         <- as.Date("2021-12-31")
ZA_start         <- as.Date("2021-04-01")
ZA_eind          <- as.Date("2022-03-31")

Cyclus_nr        = 1

# 1. Laad Databronnen in juiste format ------------------------------------------------------------

load("Patient")

Patient <- Patient %>%
  transmute(PatientNr = as.character(PatientNr),
            Geslacht = as.character(Geslacht),
            GeboorteDatum = as.Date(GeboorteDatum),
            OverlijdensDatum = as.Date(OverlijdensDatum))

load("Subtraject")

Subtraject <- Subtraject %>%
  transmute(PatientNr = as.character(PatientNr),
            SubtrajectNr = as.character(SubtrajectNr),
            AGB_Code = as.character(AGB_Code),
            DiagnoseCode = as.character(DiagnoseCode),
            OpeningsDatumDBC = as.Date(OpeningsDatumDBC),
            SluitingsDatumDBC = as.Date(SluitingsDatumDBC),
            ZorgType = as.integer(ZorgType))

load("Verrichting")

Verrichting <- Verrichting %>%
  transmute(PatientNr = as.character(PatientNr),
            SubtrajectNr = as.character(SubtrajectNr),
            OpnameNr = as.character(OpnameNr),
            OperatieNr = as.character(OperatieNr),
            ZACode = as.character(ZACode),
            ZAOmschrijving = as.character(ZAOmschrijving),
            VerrichtingDatum = as.Date(VerrichtingDatum),
            AGB_CodeAanvrager = as.character(AGB_CodeAanvrager),
            AGB_CodeUitvoerder = as.character(AGB_CodeUitvoerder),
            Aantal = as.integer(Aantal),
            ZorgprofielKlasse = as.character(ZorgprofielKlasse),
            ZorgprofielklasseOmschrijving = as.character(ZorgprofielklasseOmschrijving))

load("Opname")

Opname <- Opname %>%
  transmute(PatientNr = as.character(PatientNr),
            OpnameNr = as.character(OpnameNr),
            SubtrajectNr = as.character(SubtrajectNr),
            OpnameDatumTijd = as.POSIXct(OpnameDatumTijd),
            OntslagDatumTijd = as.POSIXct(OntslagDatumTijd),
            OpnameIndicatie = as.character(OpnameIndicatie),
            AGB_CodeOpnameSpecialisme = as.character(AGB_CodeOpnameSpecialisme),
            AGB_CodeOntslagSpecialisme = as.character(AGB_CodeOntslagSpecialisme),
            HerkomstOmschrijving = as.character(HerkomstOmschrijving),
            OntslagBestemming = as.character(OntslagBestemming),
            BehandelSettingCode = as.character(BehandelSettingCode),
            Spoed = as.character(Spoed),
            OpnameAfdeling = as.character(OpnameAfdeling))

load("Operatie")

Operatie <- Operatie %>%
  transmute(PatientNr = as.character(PatientNr),
            OperatieNr = as.character(OperatieNr),
            OpnameNr = as.character(OpnameNr),
            SubtrajectNr = as.character(SubtrajectNr),
            OperatieCode = as.integer(OperatieCode),
            OperatieOmschrijving = as.character(OperatieOmschrijving),
            OperatieDatum = as.Date(OperatieDatum),
            BrutoStartDatumTijd = as.Date(BrutoStartDatumTijd),
            NettoStartDatumTijd = as.Date(NettoStartDatumTijd),
            BrutoEindDatumTijd = as.Date(BrutoEindDatumTijd),
            NettoEindDatumTijd = as.Date(NettoEindDatumTijd),
            AanvragendSpecialisme = as.character(AanvragendSpecialisme),
            UitvoerendSpecialisme = as.character(UitvoerendSpecialisme),
            ASA_Score = as.integer(ASA_Score),
            AnesthesieTechniek = as.character(AnesthesieTechniek),
            AanvraagDatum = as.Date(AanvraagDatum))

load("Metingen")

Metingen <- Metingen %>% 
  transmute(PatientNr = as.character(PatientNr),
            ObservatieOmschrijving = as.character(ObservatieOmschrijving),
            ObservatieDatum = as.Date(ObservatieDatum),
            Uitslag = as.character(Uitslag),
            Eenheid = as.character(Eenheid))

load("Medicatie")

Medicatie <- Medicatie %>% 
  transmute(PatientNr = as.character(PatientNr),
            StartDatum = as.Date(StartDatum),
            EindDatum = as.Date(EindDatum),
            ATC_code = as.integer(ATC_code),
            ATC_omschrijving = as.character(ATC_omschrijving),
            Toedieningsvorm = as.character(Toedieningsvorm),
            Dosering = as.integer(Dosering),
            DoseringsEenheid = as.character(DoseringsEenheid),
            AGB_codeVoorschrijver = as.character(AGB_codeVoorschrijver),
            Setting = as.character(Setting))

load("Lab")

Lab <- Lab %>% 
  transmute(PatientNr = as.character(PatientNr),
            AfnameDatumTijd = as.Date(AfnameDatumTijd),
            Uitslag = as.character(Uitslag),
            Code = as.character(Code),
            BepalingOms = as.character(BepalingOms),
            Eenheid = as.character(Eenheid),
            AGB_codeAanvrager = as.character(AGB_codeAanvrager),
            AanvraagSetting = as.character(AanvraagSetting))

load("Complicatie")

Complicatie <- Complicatie %>% 
  transmute(PatientNr = as.character(PatientNr),
            AGB_code = as.character(AGB_code),
            ComplicatieCode = as.character(ComplicatieCode),
            ComplicatieDatum = as.Date(ComplicatieDatum),
            Omschrijving = as.character(Omschrijving))

load("OpnameMutaties")

OpnameMutaties <- OpnameMutaties %>% 
  transmute(PatientNr = as.character(PatientNr),
            OpnameNr = as.character(OpnameNr),
            OpnameDatumTijd = as.POSIXct(OpnameDatumTijd),
            OntslagDatumTijd = as.POSIXct(OntslagDatumTijd),
            MutatieDatumTijd = as.POSIXct(MutatieDatumTijd),
            AfdelingCode = as.character(AfdelingCode))

load("Vragenlijst")

Vragenlijst <- Vragenlijst %>% 
  transmute(PatientNr = as.character(PatientNr),
            Vragenlijstnaam = as.character(Vragenlijstnaam),
            ParentVragenlijst = as.character(ParentVragenlijst),
            Vraagcode = as.character(Vraagcode),
            Vraagstelling = as.character(Vraagstelling),
            BeantwoordingsDatum = as.Date(BeantwoordingsDatum),
            Antwoord = as.character(Antwoord),
            AntwoordGetal = as.integer(AntwoordGetal))

load("SEH")

SEH <- SEH %>% 
  transmute(PatientNr = as.character(PatientNr),
            TijdsduurSehBezoek = as.integer(TijdsduurSehBezoek),
            OpnameNr = as.character(OpnameNr))

load("VBI")

VBI <- VBI %>% 
  transmute(PatientNr = as.character(PatientNr),
            BeginDatum = as.Date(BeginDatum),
            VBIOmschrijving = as.character(VBIOmschrijving))


# 2. Patiëntselectie ------------------------------------------------------------------------------

# Selecteer patiëntnummers met opnames >= 24 uur en leeftijd >= 70 tijdens opname en opnames tussen inclusiedata

SelectieOpnameLeeftijd <- Opname %>% 
  left_join(Patient, by = "PatientNr") %>% 
  mutate(OpnameDuur = difftime(OntslagDatumTijd, OpnameDatumTijd, units = "days"),
         LeeftijdOpname = floor(time_length(difftime(OpnameDatumTijd, GeboorteDatum), "years"))) %>% 
  filter(OpnameDuur >= 1,
         LeeftijdOpname >= 70,
         BehandelSettingCode == "K",
         OpnameDatumTijd >= inc_start & OpnameDatumTijd <= inc_eind) %>%
  distinct()

### Klinische opname filteren

# Valrisico screening

ScreeningVal <- Vragenlijst %>% 
  left_join(SelectieOpnameLeeftijd, by = "PatientNr") %>% 
  filter(PatientNr %in% SelectieOpnameLeeftijd$PatientNr,
         Vragenlijstnaam == "Screening Kwetsbare Ouderen (VMS)",
         Vraagstelling == "Valrisico score") %>% 
  mutate(ValTijd = difftime(BeantwoordingsDatum, OpnameDatumTijd, units = "days"),
         ValRisico = ifelse(AntwoordGetal == 1, 1, 0)) %>% # Screeningsuitkomst bij val is 1 of 0
  filter(BeantwoordingsDatum <= OntslagDatumTijd) %>% 
  group_by(OpnameNr) %>% 
            arrange(abs(ValTijd)) %>%             
            mutate(seq=row_number()) %>% 
            filter(seq==1) %>%
  ungroup() %>% 
  distinct(PatientNr, OpnameNr, ValTijd, ValRisico)  

# Delierrisico screening

ScreeningDelier <- Vragenlijst %>% 
  left_join(SelectieOpnameLeeftijd, by = "PatientNr") %>% 
  filter(PatientNr %in% SelectieOpnameLeeftijd$PatientNr,
         Vragenlijstnaam == "Screening Kwetsbare Ouderen (VMS)",
         Vraagstelling == "Delier score") %>% 
  mutate(DelierTijd = difftime(BeantwoordingsDatum, OpnameDatumTijd, units = "days"),
         DelierRisico = ifelse(AntwoordGetal >= 1, 1, 0)) %>%
  filter(BeantwoordingsDatum <= OntslagDatumTijd) %>% 
  group_by(OpnameNr) %>% 
            arrange(abs(DelierTijd)) %>% 
            mutate(seq=row_number()) %>% 
            filter(seq==1) %>% 
  ungroup() %>% 
  distinct(PatientNr, OpnameNr, DelierTijd, DelierRisico)

# Screening Fysieke Beperking

ScreeningFysiek <- Vragenlijst %>% 
  left_join(SelectieOpnameLeeftijd, by = "PatientNr") %>% 
  filter(PatientNr %in% SelectieOpnameLeeftijd$PatientNr,
         Vragenlijstnaam == "Screening Kwetsbare Ouderen (VMS)",
         Vraagstelling == "Katz-ADL score") %>% 
  mutate(FysiekTijd = difftime(BeantwoordingsDatum, OpnameDatumTijd, units = "days"),
         FysiekRisico = ifelse(AntwoordGetal >= 2, 1, 0)) %>%
  filter(BeantwoordingsDatum <= OntslagDatumTijd) %>% 
  group_by(OpnameNr) %>% 
            arrange(abs(FysiekTijd)) %>% 
            mutate(seq=row_number()) %>% 
            filter(seq==1) %>% 
  ungroup() %>% 
  distinct(PatientNr, OpnameNr, FysiekTijd, FysiekRisico)

# Screening Ondervoeding

ScreeningOndervoed <- Vragenlijst %>% 
  left_join(SelectieOpnameLeeftijd, by = "PatientNr") %>% 
  filter(PatientNr %in% SelectieOpnameLeeftijd$PatientNr,
         Vragenlijstnaam == "Screening Kwetsbare Ouderen (VMS)",
         Vraagstelling == "SNAQ score") %>% #Of MUST score >= 1
  mutate(OndervoedTijd = difftime(BeantwoordingsDatum, OpnameDatumTijd, units = "days"),
         OndervoedRisico = ifelse(AntwoordGetal >= 2, 1, 0)) %>%
  filter(BeantwoordingsDatum <= OntslagDatumTijd) %>% 
  group_by(OpnameNr) %>% 
            arrange(abs(OndervoedTijd)) %>% 
            mutate(seq=row_number()) %>% 
            filter(seq==1) %>% 
  ungroup() %>% 
  distinct(PatientNr, OpnameNr, OndervoedTijd, OndervoedRisico)

# Patiëntselectie
PatSel <- SelectieOpnameLeeftijd %>% 
  
  left_join(ScreeningVal, by = c("PatientNr", "OpnameNr")) %>% 
  left_join(ScreeningDelier, by = c("PatientNr", "OpnameNr")) %>%
  left_join(ScreeningFysiek, by = c("PatientNr", "OpnameNr")) %>%
  left_join(ScreeningOndervoed, by = c("PatientNr", "OpnameNr")) %>%
  
  # Voor sommatie NA = 0
  mutate_at(c("ValRisico", "DelierRisico", "FysiekRisico", "OndervoedRisico"), ~replace(., is.na(.), 0)) %>% 
  
  # Totaal aantal positieve screeningen berekenen
  mutate(TotaalPos = rowSums(select(., ValRisico, DelierRisico, FysiekRisico, OndervoedRisico))) %>% 
  
  # Filter patiënten die als kwetsbaar worden gezien (positief op 2 of meer VMS criteria)
  filter(TotaalPos >= 2) %>%

  # Bepalen Primaire Opname Datum
  group_by(PatientNr) %>% 
  mutate(PrimaireOpnameDatum = first(OpnameDatumTijd)) %>% 
  ungroup() %>% 
  
  # Opnames aanmerken als primair door een 1, anders 0
  mutate(PrimaireOpname = ifelse(OpnameDatumTijd == PrimaireOpnameDatum, 1, 0)) %>% 
  
  # Filter op primaire opnames
  filter(PrimaireOpname == 1) %>% 
  
  # Variabelen in Patiëntselectie
  distinct(PatientNr,
           OpnameNr,
           LeeftijdOpname,
           OpnameDuur,
           PrimaireOpname,
           ValRisico,
           ValTijd,
           DelierRisico,
           DelierTijd,
           FysiekRisico,
           FysiekTijd,
           OndervoedRisico,
           OndervoedTijd,
           TotaalPos)

save(PatSel, file = "Patientselectie")