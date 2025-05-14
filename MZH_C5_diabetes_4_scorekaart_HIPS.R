# VBHC Aandoening: Diabetes
# Cyclus 5
# Script 4
#
#
# Datum: 13-12-2021
#
# R and library versions
# R version:  3.6.2 (2018-04-23)
# dplyr:      0.8.5
# lubridate:  1.7.8
# readxl:     1.3.1


library(dplyr)        # For using piping operator
library(lubridate)    # Voor easy date manupilation
library(readxl)       # Nodig zolang Excel ingelezen wordt
library(tidyr)
library(openxlsx)
library(stringr)
setwd("/home/afdelingen/kwaliteit.en.veiligheid/Diabetes/Cyclus 5_HIPS vergelijking/")


# Selecteer DM1 patienten uit cyclus 5, en groep 1
input_hips <- Subtraject_stap9  %>%
  left_join(PatientSelectie_final %>% select(PatientNr, Groep, type), by = c("PatientNr", "Groep")) %>% # zelf toegevoegd 11-03-25
  filter((Groep != 4 ),
         type %in% c("1", "Type 1 diabetes mellitus", "Latent-onset autoimmune diabetes of the adult (LADA)")) # zelf toegevoegd 11-03-25
# Cyclus == "C5" &
#(TypeDM == "Type 1" | TypeDM == "LADA"))
#RL voor HIPS: input dataframe maken
input_hips <- input_hips %>% 
  #left_join(PatientSelectie, by = "PatientNr") %>% 
  mutate(Aandoening = "diabetes",
         # Groep = "volwassenen",
         ZiekenhuisCode = "mzh", 
         DM_stop=as.Date(DM_stop)) %>% #aanvulling Whitney
  rename(Identificatienummer = PatientNr,
         InclusieDatum = DM_start,
         EindDatum = DM_stop) %>% 
  select(Aandoening, Groep, Identificatienummer, InclusieDatum, EindDatum, ZiekenhuisCode)

x <- input_hips %>% filter(InclusieDatum > EindDatum) # NOG NAAR KIJKEN!
input_hips <- input_hips %>% filter(InclusieDatum < EindDatum) # Eruit gehaald want dan werkt onderstaande for-loop niet, toegevoegd 10-03-25

#uitsplitsen van de patienten populatie naar patienten per jaar (uit f_Patient_per_jaar HIPS), aanvulling whitney
# Bepaal voor welk kalenderjaar de patient in de populatie zit 
kalenderjaar_in_inclusie = list()
for(i in 1:nrow(input_hips)) {
  kalenderjaar_in_inclusie[[i]] = (seq(year(input_hips$InclusieDatum)[i], year(input_hips$EindDatum)[i], by= 1))
}

# Herhaal voor iedere jaar in de selectie de gehele selectie periode
df_long <- input_hips[rep(1:nrow(input_hips), times = lengths(kalenderjaar_in_inclusie)),]

# Splits periode op naar kalender jaar
df_Populatie_by_year <- df_long %>% mutate(   Jaar          = unlist(kalenderjaar_in_inclusie),
                                              Groep         = as.character(Groep),
                                              InclusieDatum = if_else(as.Date(paste0(Jaar,"-01-01")) > InclusieDatum,  as.Date(paste0(Jaar,"-01-01")), InclusieDatum  ),
                                              EindDatum     = if_else(as.Date(paste0(Jaar,"-12-31")) < as.Date(EindDatum),      as.Date(paste0(Jaar,"-12-31")), EindDatum ),
                                              InclusieDatum = as.Date( format(InclusieDatum, "%Y-%m-%d")),
                                              EindDatum     = as.Date( format(EindDatum,  "%Y-%m-%d"))) %>% 
  distinct( Identificatienummer, Groep, InclusieDatum, EindDatum, Aandoening, ZiekenhuisCode) 

# selecteer alleen 2021
window_begindatum <- as.Date( "2021-01-01", format = "%Y-%m-%d")
window_einddatum  <- as.Date( "2021-12-31", format = "%Y-%m-%d")
input_hips<-df_Populatie_by_year%>%filter(InclusieDatum >= window_begindatum &
                                            InclusieDatum <= window_einddatum)

write.xlsx(input_hips, "diabetes_WideFormat_huis_patientselectie.xlsx", col_names = TRUE)

######################################################### TOT HIER GERUND 10-03-2025 ####################################################################



# Selecteer DM1 patienten uit cyclus 5, en groep 1
PatientSelectie <- PatientSelectie_final %>%
  filter((Groep == 1 ) &
           Cyclus == "C5" &
           (type == "Type 1 diabetes mellitus" | grepl("LADA", type) | type == "1")) %>%
  mutate(type = ifelse(type == "1", "Type 1 diabetes mellitus", type)) %>%
  distinct(PatientNr, .keep_all = TRUE)

# ---- Selecteer alleen de benodigde gegevens -----
Patient       <- Patient       %>% filter( PatientNr %in% PatientSelectie$PatientNr)

Subtraject    <- Subtraject    %>% filter( PatientNr %in% PatientSelectie$PatientNr)

Verrichting   <- Verrichting   %>% filter( PatientNr %in% PatientSelectie$PatientNr)

Probleemlijst <- Probleemlijst %>% filter( PatientNr %in% PatientSelectie$PatientNr)

Jaarcontrole  <- Jaarcontrole  %>% filter( PatientNr %in% PatientSelectie$PatientNr)

Afspraak      <- Afspraak      %>% filter( PatientNr %in% PatientSelectie$PatientNr)

Opname        <- Opname        %>% filter( PatientNr %in% PatientSelectie$PatientNr)

Medicatie     <- Medicatie     %>% filter( PatientNr %in% PatientSelectie$PatientNr)

Lab           <- Lab           %>% filter( PatientNr %in% PatientSelectie$PatientNr)

Metingen      <- Metingen      %>% filter( PatientNr %in% PatientSelectie$PatientNr)

###---------------------------------------------------------------------------------------------------------
###--------------------------------------------- CASEMIX ---------------------------------------------------
###---------------------------------------------------------------------------------------------------------

# --------------- Leeftijd ---------------------------------------------------------------------------
# Leeftijd berekenen a.d.h.v. geboortedatum uit tabblad patient en peildatum gelijk aan startwindow
leeftijd    <- Patient %>%
  mutate (leeftijd=(as.numeric(difftime(startwindow, GeboorteDatum, units = "days")))/365) %>%
  select(PatientNr, leeftijd)

lft         <- leeftijd %>%  mutate( Groep  = case_mix_name,
                                     Ind    = "lft",
                                     Waarde = ifelse( leeftijd < 30, 1, NA),
                                     Waarde = ifelse( leeftijd >= 30 & leeftijd < 50, 2, Waarde),
                                     Waarde = ifelse( leeftijd >= 50 & leeftijd < 70, 3, Waarde),
                                     Waarde = ifelse( leeftijd >= 70, 4, Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(lft$Waarde)

# --------------- Geslacht ---------------------------------------------------------------------------
gsl         <- Patient %>%  mutate( Groep  = case_mix_name,
                                    Ind    = "gsl",
                                    Waarde = ifelse(Geslacht == "M",1,NA),
                                    Waarde = ifelse(Geslacht == "V",2,Waarde),
                                    Waarde = ifelse(is.na(Geslacht) | Geslacht == "NULL", 99, Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(gsl$Waarde)

# --------------- Koppel lengte [m] meting dichtste bij startwindow aan patientselectie --------------
Lengte        <- Metingen   %>% filter(  PatientNr    %in% Patient$PatientNr &
                                           ObservatieOms == "Lengte" &
                                           as.numeric(Uitslag) >= 100 & as.numeric(Uitslag) <= 220) %>%
  group_by(PatientNr) %>%
  filter(  min( abs( difftime(ObservatieDatum, startwindow, units = "days"))) ==
             abs( difftime(ObservatieDatum, startwindow, units = "days")) ) %>%
  mutate(  Lengte = mean(as.numeric(Uitslag))/100) %>%
  ungroup() %>%
  distinct(PatientNr, Lengte)

Patient       <- merge( x     = Patient,
                        y     = Lengte,
                        by    = "PatientNr",
                        all.x = TRUE)

# --------------- Koppel gewicht [kg] meting dichtste bij peildatum aan patientselectie --------------
#Let op: >1 meting voor gewicht geregistreerd op dezelfde dag? Je wilt 1 meting per patient.
Gewicht      <-  Metingen %>% filter(PatientNr    %in% Patient$PatientNr &
                                       ObservatieOms == "Gewicht" &
                                       as.numeric(Uitslag) >= 30 & as.numeric(Uitslag) <=300) %>%
  group_by(PatientNr) %>%
  filter(  min( abs( difftime(ObservatieDatum, startwindow, units = "days"))) ==
             abs( difftime(ObservatieDatum, startwindow, units = "days")) ) %>%
  mutate(  Gewicht = mean(as.numeric(Uitslag))) %>%
  ungroup() %>%
  distinct(PatientNr, Gewicht)

Patient      <- merge( x     = Patient,
                       y     = Gewicht,
                       by    = "PatientNr",
                       all.x = TRUE)

# ---------------------------------- Bepaal BMI van patient ----------------------------------
Patient <- Patient %>% mutate(BMI = Gewicht/(Lengte*Lengte))

#bmi2_raw <- read.csv("DIABETES_C5_BMI_SV.csv", header=T, sep=";", dec=",", stringsAsFactors = F, na.strings="", quote="", fileEncoding = "latin1") %>%
#  mutate(PatientNr = as.character(PATIENTNR))

#bmi2      <-  bmi2_raw %>% filter(PatientNr    %in% Patient$PatientNr &
#                                       #ObservatieOms == "BMI" &
#                                       as.numeric(BMI) >= 10 & as.numeric(BMI) <=70) %>%
#  mutate(ObservatieDatum = as.Date(DATUMTIJD, format = "%Y-%m-%d %H:%M:%S")) %>%
#  group_by(PatientNr) %>%
#  filter(  min( abs( difftime(ObservatieDatum, startwindow, units = "days"))) ==
#             abs( difftime(ObservatieDatum, startwindow, units = "days")) ) %>%
#  mutate(  bmi2 = mean(as.numeric(BMI))) %>%
#  ungroup() %>%
#  distinct(PatientNr, bmi2)

#Patient      <- merge( x     = Patient,
#                       y     = bmi2,
#                       by    = "PatientNr",
#                       all.x = TRUE)


#BMI halen uit metingen
BMI2      <-  Metingen %>% filter(PatientNr    %in% Patient$PatientNr &
                                       ObservatieOms == "BMI" &
                                       as.numeric(Uitslag) >= 10 & as.numeric(Uitslag) <= 70) %>%
  group_by(PatientNr) %>%
  filter(  min( abs( difftime(ObservatieDatum, startwindow, units = "days"))) ==
             abs( difftime(ObservatieDatum, startwindow, units = "days")) ) %>%
  mutate(  BMI2 = mean(as.numeric(Uitslag))) %>%
  ungroup() %>%
  distinct(PatientNr, BMI2)

Patient      <- merge( x     = Patient,
                       y     = BMI2,
                       by    = "PatientNr",
                       all.x = TRUE)

Patient <- Patient %>% mutate(BMI = ifelse(is.na(BMI2), BMI, BMI2))%>%
  select(PatientNr, Geslacht, GeboorteDatum, OverlijdensDatum, Lengte, Gewicht, BMI)


bmi     <- Patient %>% mutate( Groep  = case_mix_name,
                               Ind    = "bmi",
                               Waarde = ifelse(BMI <  18.5, 1, NA),
                               Waarde = ifelse(BMI >= 18.5 & BMI <  25.0, 2, Waarde),
                               Waarde = ifelse(BMI >= 25.0 & BMI <  30.0, 3, Waarde),
                               Waarde = ifelse(BMI >= 30.0, 4, Waarde),
                               Waarde = ifelse(is.na(BMI),  99, Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(bmi$Waarde)


x <- Patient %>% filter(is.na(BMI))
#write.xlsx(x, file = "BMI onbekend.xlsx")

# ---------------------------------- Etniciteit ----------------------------------     
#etn        <- Metingen %>%
#  filter(ObservatieOms == "Etniciteit") %>%
#  group_by(PatientNr)

#etn        <- left_join(Patient, etn, by="PatientNr") %>%  
#  mutate( Groep  = case_mix_name,
#          Ind    = "etn",
#          Waarde = ifelse(Uitslag == "Kaukasisch", 1, NA),
#          Waarde = ifelse(Uitslag == "Noord-Afrikaans", 2, Waarde),
#          Waarde = ifelse(Uitslag == "Overig Afrikaans", 3, Waarde),
#          Waarde = ifelse(Uitslag == "Turks- en Caicoseilanden", 4, Waarde),
#          Waarde = ifelse(Uitslag == "Hindoestaans", 5, Waarde),
#          Waarde = ifelse(Uitslag == "Overig Aziatisch", 6, Waarde),
#          Waarde = ifelse(Uitslag == "Latijns Amerikaans", 7, Waarde),
#          Waarde = ifelse(Uitslag == "Meervoudige afkomst", 8, Waarde),
#          Waarde = ifelse(Uitslag == "Onbekend", 99, Waarde),
#          Waarde = ifelse(is.na(Waarde), 99, Waarde)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#table(etn$Waarde)                                      

# ---------------------------------- Sociaal economische status ----------------------------------
#ses        <- Metingen %>%
#  filter(ObservatieOms == "Hoogst genoten opleiding") %>%
#  group_by(PatientNr)

#ses        <- left_join(Patient, ses, by="PatientNr") %>%  
#  mutate( Groep  = case_mix_name,
#          Ind    = "ses",
#          Waarde = ifelse(Uitslag == "Basisschool", 1, NA),
#          Waarde = ifelse(Uitslag == "Lager beroepsonderwijs", 2, Waarde),
#          Waarde = ifelse(Uitslag == "vmbo-t", 3, Waarde), 
#          Waarde = ifelse(Uitslag == "Middelbaar beroepsonderwijs", 4, Waarde),
#          Waarde = ifelse(Uitslag == "Hoger algemeen onderwijs", 5, Waarde),
#          Waarde = ifelse(Uitslag == "Hoger beroepsonderwijs", 6, Waarde),
#          Waarde = ifelse(Uitslag == "Wetenschappelijk onderwijs", 7, Waarde),
#          Waarde = ifelse(Uitslag == "Anders", 8, Waarde),
#          Waarde = ifelse(Uitslag == "Onbekend", 99, Waarde),
#          Waarde = ifelse(is.na(Waarde), 99, Waarde)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#table(ses$Waarde)

# ---------------------------------- Diagnose sinds ------------------------------------------
# Datum diagnose o.b.v. ConstateringsDatum (Probleemlijst) of diagdat (DPARD)
# O.b.v. PROBLEEMLIJST
diagnose    <- Probleemlijst %>%
  filter( grepl("E10", ICDcode) ) %>% #| grepl("C51.2", ICDcode)
  mutate (diagnose=(as.numeric(difftime(startwindow, ConstateringsDatum, units = "days")))/365) #%>%
  #select(PatientNr, diagnose)

diagnose1        <- diagnose %>%  mutate( Groep  = case_mix_name,
                                          Ind    = "diagnose",
                                          Waarde = ifelse( diagnose <= 0, 1, NA),
                                          Waarde = ifelse( diagnose > 0 & diagnose <= 5, 2, Waarde),
                                          Waarde = ifelse( diagnose > 5 & diagnose <= 10, 3, Waarde),
                                          Waarde = ifelse( diagnose > 10 & diagnose <=15, 4, Waarde),
                                          Waarde = ifelse( diagnose > 15, 5, Waarde),
                                          Waarde = ifelse( is.na(diagnose ), 99, Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(diagnose1$Waarde)

x <- left_join(PatientSelectie, Probleemlijst, by = "PatientNr")

#x1 <- anti_join(ptsel_type_db, diagnose1, by = "PatientNr")


#voor nu gebruikt.
data_epd_klaas       <- data_epd_klaas       %>% mutate(PatientNr = as.character(patientnr)) %>%
  filter( PatientNr %in% PatientSelectie$PatientNr)

diagnose2        <- data_epd_klaas %>%  
  mutate(diagnose= (2021 - DMJAAR)) %>%
  mutate( Groep  = case_mix_name,
                                          Ind    = "diagnose",
                                          Waarde = ifelse( diagnose <= 0, 1, NA),
                                          Waarde = ifelse( diagnose > 0 & diagnose <= 5, 2, Waarde),
                                          Waarde = ifelse( diagnose > 5 & diagnose <= 10, 3, Waarde),
                                          Waarde = ifelse( diagnose > 10 & diagnose <=15, 4, Waarde),
                                          Waarde = ifelse( diagnose > 15, 5, Waarde),
                                          Waarde = ifelse( is.na(diagnose ), 99, Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(diagnose2$Waarde)

# O.b.v. JAARCONTROLE
#diagnose     <- Jaarcontrole %>%
#  filter(VraagOms=="DiagnoseJaar") %>%
#  mutate(diagnose = as.numeric(Uitslag)) %>%
#  filter(!is.na(diagnose)) %>%
#  distinct()%>%
#  mutate(diagnose = as.numeric( format(startwindow, format = "%Y") )- diagnose)

#diagnose         <- left_join(Patient, diagnose, by="PatientNr") %>%
#  mutate( Groep  = case_mix_name,
#          Ind    = "diagnose",
#          Waarde = ifelse( diagnose <= 0, 1, NA),
#          Waarde = ifelse( diagnose > 0 & diagnose <= 5, 2, Waarde),
#          Waarde = ifelse( diagnose > 5 & diagnose <= 10, 3, Waarde),
#          Waarde = ifelse( diagnose > 10 & diagnose <=15, 4, Waarde),
#          Waarde = ifelse( diagnose > 15, 5, Waarde),
#          Waarde = ifelse( is.na(diagnose ), 99, Waarde)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#table(diagnose$Waarde)

# ---------------------------------- Roken -------------------------------------------------
# O.b.v. JAARCONTROLE
#rok     <- Jaarcontrole %>%
#  filter(VraagOms=="roken") %>%               #LET OP: IN STA GECODEERD ALS 1 = 'ja, 0 = 'nee'
#  mutate(rok = ifelse(Uitslag == "ja", "1", NA),
#         rok = ifelse(Uitslag %in% c("nee", "Nooit", "nooit", "Niet"), "0", rok),
#         rok = ifelse(Uitslag %in% c("gestopt", "1998 gestopt met roken"), "1", rok),
#         rok = as.numeric(rok)) %>%
#  filter(!is.na(rok)) %>%
#  arrange(PatientNr, desc(JaarcontroleDatum)) %>%
#  distinct(PatientNr, .keep_all = TRUE)

rok     <- Jaarcontrole %>%
  filter(VraagOms=="roken") %>%               #LET OP: IN STA GECODEERD ALS 1 = 'ja, 0 = 'nee'
  mutate(rok = ifelse(Uitslag == "ja", "1", NA),
         rok = ifelse(Uitslag %in% c("nee", "gestopt"), "0", rok),
         #rok = ifelse(Uitslag %in% c("gestopt", "1998 gestopt met roken"), "1", rok),
         rok = as.numeric(rok)) %>%
  filter(!is.na(rok)) %>%
  arrange(PatientNr, desc(JaarcontroleDatum)) %>%
  distinct(PatientNr, .keep_all = TRUE)

rok       <- left_join(Patient, rok, by="PatientNr") %>%
  mutate( Groep  = case_mix_name,
          Ind    = "rok",
          Waarde = ifelse(rok == 0, 0, NA),
          Waarde = ifelse(rok == 1, 1, Waarde),
          Waarde = ifelse(is.na(Waarde), 99, Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(rok$Waarde)

# O.b.v. EPD FLOWSHEETMETING#
#roken       <- Metingen %>%
#  filter(ObservatieOms=="roken") %>%
#  group_by(PatientNr) %>%
#  filter(ObservatieDatum==max(ObservatieDatum))

#roken       <- roken   %>%    mutate( Groep  = case_mix_name,
#                                      Ind    = "rok",
#                                      Waarde = ifelse(Uitslag == "Rookt", 1, NA),
#                                      Waarde = ifelse(Uitslag == "Ex-roker", 2, Waarde),
#                                      Waarde = ifelse(Uitslag == "Nooit gerookt" , 2, Waarde),
#                                      Waarde = ifelse(Uitslag == "Onbekend", 99, Waarde)) %>%
#  select(PatientNr, Groep, Ind, Waarde) %>%
#  distinct(PatientNr, Groep, Ind, Waarde) %>%
#  mutate(check=ifelse(duplicated(PatientNr),1,0))

# ----------------------- Alcohol consumptie --------------------------------------------------
# O.b.v. DPARD
#alcohol     <- DPARD   %>%   mutate( Groep  = case_mix_name,
#                                     Ind    = "alc",
#                                     Waarde = ifelse(is.na(alcoholgebruikstatus), 99, alcoholgebruikstatus)) %>%
#              select(PatientNr, Groep, Ind, Waarde)

# ----------------------- HbA1c ----------------------------------------------------------
# HbA1c wordt berekend onder Uitkomstindicatoren.

# ----------------------- Retinopathie ---------------------------------------------------
retinopathie  <-  subset(     Subtraject,(DiagnoseCode %in% DBC_retino)
                              & SpecialismeCode=='OOG' & (OpeningsDatum<=stopwindow & SluitingsDatum>=startwindow)) %>% #OOG ipv 0301
  group_by(   PatientNr) %>%
  summarise(  retinopathie = first(DiagnoseCode)) %>%
  ungroup()

retino           <-  left_join(Patient, retinopathie, by="PatientNr") %>%
  mutate(     Groep  = case_mix_name,
              Ind    = "retino",
              Waarde = ifelse(!is.na(retinopathie),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(retino$Waarde)

# ----------------------- Leefstijl advies --------------------------------------------------
# O.b.v. DPARD
#leefstijl   <- DPARD   %>%   mutate( Groep  = case_mix_name,
#                                     Ind    = "adv",
#                                     Waarde = ifelse(leefdat>=startwindow & leefdat<=stopwindow, 1, NA),
#                                     Waarde = ifelse(leefdat<=startwindow | leefdat>=stopwindow, 2, Waarde),
#                                     Waarde = ifelse(is.na(leefdat), 99, Waarde)) %>%
#              select(PatientNr, Groep, Ind, Waarde)
#              table(leefstijl$Waarde)

# ----------------------- Medicatie ---------------------------------------------------------
#Medicatie        <- Medicatie %>%
#                    mutate(EindDatum = ifelse(is.na(EindDatum), stopwindow, EindDatum),
#                       StartDatum = ifelse(is.na(StartDatum), EindDatum, StartDatum),
#                       EindDatum = as.Date(EindDatum, origin = "1970-01-01")) %>%
#                    filter(StartDatum <= stopwindow & EindDatum >= startwindow) %>%
#                    distinct(PatientNr, StartDatum, EindDatum, ATC_code) %>%
#                    group_by(PatientNr)

#Verdieping voor OLVG EN STA
#Medicatie2 <- Medicatie %>% mutate( Groep  = case_mix_name,
#                                    Ind    = "med",
#                                    Waarde = ifelse(grepl("C10", ATC_code),1,0),
#                                    Waarde = ifelse(grepl("C02", ATC_code),2,Waarde),
#                                    Waarde = ifelse(grepl("C03", ATC_code),2,Waarde),
#                                    Waarde = ifelse(grepl("C04", ATC_code),2,Waarde),
#                                    Waarde = ifelse(grepl("C07", ATC_code),2,Waarde),
#                                    Waarde = ifelse(grepl("C08", ATC_code),2,Waarde),
#                                    Waarde = ifelse(grepl("C09", ATC_code),2,Waarde),
#                                    Waarde = ifelse(ATC_code == "N06AA09",3,Waarde),
#                                    Waarde = ifelse(ATC_code == "N03AX12",3,Waarde),
#                                    Waarde = ifelse(ATC_code == "N03AX16",3,Waarde),
#                                    Waarde = ifelse(ATC_code == "N06AX21",3,Waarde),
#                                    Waarde = ifelse(ATC_code == "G04BE03",3,Waarde),
#                                    Waarde = ifelse(ATC_code == "N01BX04",3,Waarde)) %>%
#                filter(Waarde!= 0) %>%
#                distinct(PatientNr, Groep, Ind, Waarde)
#
#Medicatie2   <- left_join(Patient,Medicatie2, by="PatientNr") %>%
#                mutate ( Groep  = case_mix_name,
#                         Ind    = "med",
#                         Waarde = ifelse(is.na(Waarde),4,Waarde)) %>%
#                select(PatientNr, Groep, Ind, Waarde)
#                table(Medicatie2$Waarde)

# ----------------------- Soort insuline ---------------------------------------
# Cyclus 1: O.b.v. medicatie.
#Insuline   <- Medicatie %>% mutate ( Groep  = case_mix_name,
#                                     Ind    = "insul",
#                                     Waarde = ifelse(grepl("A10AB", ATC_code), 1, 0),
#                                     Waarde = ifelse(grepl("A10AC", ATC_code), 2, Waarde),
#                                     Waarde = ifelse(grepl("A10AE", ATC_code), 3, Waarde),
#                                     Waarde = ifelse(grepl("A10AD", ATC_code), 4, Waarde)) %>%
#                            filter(Waarde!= 0) %>%
#                            distinct(PatientNr, Groep, Ind, Waarde)

#Insuline   <- left_join(Patient,Insuline, by="PatientNr") %>%
#                            mutate ( Groep  = case_mix_name,
#                                     Ind    = "insul",
#                                     Waarde = ifelse(is.na(Waarde),99,Waarde)) %>%
#                            select(PatientNr, Groep, Ind, Waarde)
#                            table(Insuline$Waarde)

# Cyclus 2 O.b.v. JAARCONTROLE
#Insuline     <- Jaarcontrole %>%
#                            filter(VraagOms=="insuline") %>%
#                            mutate(Insuline = as.character(Uitslag)) %>%
#                            filter(!is.na(Insuline)) %>%
#                            distinct()

#Insuline     <- left_join(Patient, Insuline, by="PatientNr") %>%
#                            mutate( Groep  = case_mix_name,
#                                   Ind    = "insul",
#                                    Waarde = ifelse(Insuline == 'Kortwerkende insuline',1,NA),
#                                    Waarde = ifelse(Insuline == 'Middel langwerkende insuline',2,Waarde),
#                                    Waarde = ifelse(Insuline == 'Langwerkende insuline',3,Waarde),
#                                    Waarde = ifelse(Insuline == 'Mix-insulines',4,Waarde),
#                                    Waarde = ifelse(is.na(Waarde),99,Waarde))%>%
#                            distinct(PatientNr, Groep, Ind, Waarde)
#table(Insuline$Waarde)

# ------------------- Insuline therapie ----------------------------------------------
# O.b.v. JAARCONTROLE
# Check patienten met alleen tabletten. Dit zal altijd in combinatie zijn met pomp of pen.
# Categoriseer ze vervolgens in juiste categorie, anders bij 'onbekend'.
#Insulinetherapie     <- Jaarcontrole %>%
#  filter(VraagOms=="Insulinetherapie") %>%
#  mutate(Insulinetherapie = as.character(Uitslag)) %>%
#  filter(!is.na(Insulinetherapie)) %>%
#  distinct()

#Insulinetherapie     <- left_join(Patient, Insulinetherapie, by="PatientNr") %>%
#  mutate( Groep  = case_mix_name,
#          Ind    = "insulther",
#          Waarde = ifelse(Insulinetherapie == 'pomp',1,NA),
#          Waarde = ifelse(Insulinetherapie == 'pen',2,Waarde),
#          Waarde = ifelse(Insulinetherapie == 'tabletten',3,Waarde),
#          Waarde = ifelse(is.na(Waarde),99,Waarde))%>%
#  distinct(PatientNr, Groep, Ind, Waarde)
#table(Insulinetherapie$Waarde)

# BOVENSTAANDE NIET GEBRUIKEN? VRAAG KLAAS NA OF POMP DBC 223 GOED GENOEG IS

# Controle: Pomp o.b.v. DBC's
# Check verschil tussen jaarcontrole en DBC (bijv: patient in begin inclusie periode pomp, vervolgens over op pen)
# Deze indicator wordt ook gebruikt bij combi pomp/RT-CGM
#pomp        <- Subtraject %>%
#  filter(     DiagnoseCode=="223" & (SluitingsDatum>=startwindow & OpeningsDatum<=stopwindow)) %>%
#  group_by(   PatientNr) %>%
#  summarise(  pomp = n())

#pomp         <- left_join(Patient, pomp, by="PatientNr") %>%
#  mutate(     Groep  = case_mix_name,
#              Ind    = "pomp",
#              Waarde = ifelse(!is.na(pomp),1,0)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#table(pomp$Waarde)


#Insulinetherapie
pomp <- Jaarcontrole %>% filter(VraagOms == "Insulinetherapie") %>%
  group_by(   PatientNr) %>%
  summarise(  pomp = n()) %>%
  ungroup()

pomp         <- left_join(Patient, pomp, by="PatientNr") %>%
  mutate(     Groep  = case_mix_name,
              Ind    = "pomp",
              Waarde = ifelse(!is.na(pomp),1,2)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(pomp$Waarde)

# ------------------- Real-time continue glucosemeter --------------------------------
# Real-time continue glucosemeter wordt geregistreerd met ZACode 190351
#rtcgm       <- Verrichting %>%
#  filter(     ZACode=="190351" & (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow)) %>%
#  group_by(   PatientNr) %>%
#  summarise(  rtcgm = n()) %>%
#  ungroup()

#rtcgm         <- left_join(Patient, rtcgm, by="PatientNr") %>%
#  mutate(     Groep  = case_mix_name,
#              Ind    = "rtcgm",
#              Waarde = ifelse(!is.na(rtcgm),1,0)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#table(rtcgm$Waarde)

#Obv CGM lijst patienten (zoals Klaas voorstelde)
rtcgm <- Jaarcontrole %>% filter(VraagOms == "rtcgm" #,
                                  #JaarcontroleDatum >= startwindow & JaarcontroleDatum <= stopwindow
                                  ) %>%
  group_by(   PatientNr) %>%
  summarise(  rtcgm = n()) %>%
  ungroup()

rtcgm         <- left_join(Patient, rtcgm, by="PatientNr") %>%
  mutate(     Groep  = case_mix_name,
              Ind    = "rtcgm",
              Waarde = ifelse(!is.na(rtcgm),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(rtcgm$Waarde)


# O.b.v. JAARCONTROLE
#rtcgm     <- Jaarcontrole %>%
#  filter(     VraagOms=="rtcgm") %>%
#  mutate(     rtcgm = as.numeric(Uitslag)) %>%  #LET OP: IN STA GECODEERD ALS 1 = 'ja, 0 = 'nee'
#  filter(!is.na(rtcgm)) %>%
#  distinct()

#rtcgm       <- left_join(Patient, rtcgm, by="PatientNr") %>%
#  mutate(     Groep  = case_mix_name,
#              Ind    = "rtcgm",
#              Waarde = ifelse(!is.na(rtcgm),1,0)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#table(rtcgm$Waarde)

# ----------------------- Flash glucose meter --------------------------------------------------
# Flash glucose meter wordt vergoed door basisverzekering, in eigen ziekenhuis achterhalen hoe dit wordt geregistreerd
#IN MZH OBV TRIAL EN FAVORIETEN LIJSTEN IN HIX DUS NU NIET OP TE HALEN
# O.b.v. JAARCONTROLE
Flash     <- Jaarcontrole %>%
  filter(     VraagOms=="FSL") %>%
  mutate(     Flash = "1") %>%  #LET OP: IN STA GECODEERD ALS 1 = 'ja, 0 = 'nee'
  filter(!is.na(Flash)) %>%
  distinct()

Flash     <- left_join(Patient, Flash, by="PatientNr") %>%
  mutate(     Groep  = case_mix_name,
              Ind    = "Flash",
              Waarde = ifelse(Flash == 0, 0, NA),
              Waarde = ifelse(Flash == 1, 1, Waarde),
              Waarde = ifelse(is.na(Flash), 0, Waarde)) %>% #Als niet ingevuld dan 0 ipv 99
  select(PatientNr, Groep, Ind, Waarde)
table(Flash$Waarde)

# ----------------------- Combi pomp en RT-CGM ---------------------------------------
combi         <- left_join(pomp,rtcgm, by="PatientNr") %>%  
  mutate(     Groep  = case_mix_name,
              Ind    = "combi",
              Waarde = ifelse(Waarde.x==1 & Waarde.y==1,1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(combi$Waarde)

###---------------------------------------------------------------------------------------------------------
###--------------------------------------------- SCOREKAART ------------------------------------------------
###---------------------------------------------------------------------------------------------------------


###-------------------------------------------------------------------------------------------------------
###----------------------------------- U1 HbA1c ------------------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Dichtsbijzijnde datum een jaar voor de laatste meting zoeken, met window van +-90 dagen.
hba1c       <- Lab %>%
  filter(  BepalingOms=="hba1c" & (AfnameDatumTijd>=startwindow_lab & AfnameDatumTijd<=stopwindow) & !is.na(Uitslag)) %>% #Volgorde verwisseld
  #filter(Uitslag != "Zie opm." & Uitslag != "-volgt-") %>% #Zelf toegevoegd anders error bij uitslag naar numeric
  mutate(  Uitslag = as.numeric(Uitslag)) %>% 
  filter(Uitslag >= 20 ) %>% #Om zo de percentages eruit te filteren en alleen te kijken naar de mmol/mol
  group_by(PatientNr, AfnameDatumTijd) %>%
  mutate(  Uitslag          = mean(as.numeric(Uitslag), na.rm = TRUE) ) %>%
  distinct(PatientNr, AfnameDatumTijd, .keep_all =TRUE) %>%
  ungroup() %>%
  group_by(PatientNr) %>%
  arrange( PatientNr, AfnameDatumTijd) %>%
  mutate(  min  = min( abs( difftime(AfnameDatumTijd, max(AfnameDatumTijd) - years(1), units = "days"))),
           diff = abs( difftime(AfnameDatumTijd, max(AfnameDatumTijd) - years(1), units = "days")) ) %>%
  filter(  max(AfnameDatumTijd) >= startwindow &
             ( AfnameDatumTijd  == max(AfnameDatumTijd) |
                 ((min == diff & min <=90)
                 )
             )
  ) %>%
  mutate( eerste   = as.numeric(first(Uitslag)),
          laatste  = as.numeric(last(Uitslag)),
          verschil = laatste-eerste) %>%
  ungroup()

hba1c_cat   <- hba1c %>%
  filter(     min <=90) %>%
  select(     PatientNr, eerste, laatste, verschil) %>%
  distinct(   PatientNr, eerste, laatste, verschil) %>%
  mutate(     cat_eerste=cut(eerste, breaks=c(-Inf, 53, 64, 74, Inf), labels=c("<=53",">53,<65",">=65,<75",">=75")),
              cat_laatste=cut(laatste, breaks=c(-Inf, 53, 64, 74, Inf), labels=c("<=53",">53,<65",">=65,<75",">=75")))

##### VOOR CASEMIX #####
HbA1c    <- hba1c %>% 
  group_by(PatientNr) %>% #Toegevoegd
  filter(     AfnameDatumTijd == max(AfnameDatumTijd)) %>%
  ungroup()

HbA1c    <- left_join(Patient, HbA1c, by="PatientNr") %>%
  mutate(     Uitslag = as.numeric(Uitslag),
              cat_laatste=cut(Uitslag, breaks=c(-Inf, 53, 64, 74, Inf), labels=c("<=53",">53,<65",">=65,<75",">=75"))) %>%
  mutate    ( Groep  = case_mix_name,
              Ind    = "HbA1c",
              Waarde = ifelse(cat_laatste=="<=53", 1, 0),
              Waarde = ifelse(cat_laatste==">53,<65", 2, Waarde),
              Waarde = ifelse(cat_laatste==">=65,<75", 3, Waarde),
              Waarde = ifelse(cat_laatste==">=75", 4, Waarde),
              Waarde = ifelse(is.na(cat_laatste), 99, Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(HbA1c$Waarde)  #Soms onbekenden door "zie opm. en -volgt- eruit gefilterd

##### VOOR TABBLAD HbA1c tabel #####
table(hba1c_cat$cat_eerste, hba1c_cat$cat_laatste) #horizontale categorieen = laatste meting, verticale categorieen = eerste meting

# Indicator U1.1
#U11         <- hba1c_cat %>%
#  filter(     cat_laatste!="<=53") %>%
#  mutate(     Groep  = scorekaart_name,
#              Ind    = "U1.1",
#              Waarde = ifelse(verschil>=10,1,0)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#table(U11$Waarde)

# Indicator U1.2
U12         <- hba1c_cat %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "U1.2",
              Waarde = ifelse(cat_eerste==">=75" & cat_laatste==">=75",1,0)) %>%
  filter(     !is.na(Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(U12$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- U2 Intermediate outcomes --------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Indicator U2.1.1 - Systolische bloeddruk
sysbp       <- Metingen %>%
  filter(     ObservatieOms=="SysBloeddruk" & (ObservatieDatum >=startwindow & ObservatieDatum <= stopwindow),
              Uitslag >= 80 & Uitslag <= 250) %>%
  group_by(   PatientNr) %>%
  mutate(     Uitslag=as.numeric(Uitslag),
              hoog=ifelse(any(Uitslag<140),0,1)) %>%
  summarise(  Waarde=max(hoog)) %>%
  ungroup()

sysbp       <- Metingen %>%
  filter(     ObservatieOms=="NIBP" & (ObservatieDatum >=startwindow & ObservatieDatum <= stopwindow),
              Uitslag >= 80 & Uitslag <= 250) %>%
  group_by(   PatientNr) %>%
  mutate(     Uitslag=as.numeric(Uitslag),
              hoog=ifelse(any(Uitslag<140),0,1)) %>%
  summarise(  Waarde=max(hoog)) %>%
  ungroup()

U211a        <- sysbp %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "U2.1.1a") %>%
  select(PatientNr, Groep, Ind, Waarde)
table(U211a$Waarde)

# Indicator U2.1.2 - Systolische bloeddruk en nefropathie
nefropathie <- Lab %>%
  filter(     BepalingOms=="acr in urine" & (AfnameDatumTijd>=startwindow & AfnameDatumTijd<=stopwindow)) %>%
  group_by(   PatientNr) %>%
  mutate(     Uitslag=as.numeric(Uitslag),
              nefropathie=ifelse(any(Uitslag<=30),0,1)) %>%
  summarise(  nefropathie=max(nefropathie)) %>%
  ungroup()

sysbp_130   <- Metingen %>%
  filter(     ObservatieOms=="NIBP" & (ObservatieDatum >= startwindow & ObservatieDatum <= stopwindow),
              Uitslag >= 80 & Uitslag <= 250) %>%
  group_by(   PatientNr) %>%
  mutate(     Uitslag=as.numeric(Uitslag),
              hoog=ifelse(any(Uitslag<130),0,1)) %>%
  summarise(  U212a=max(hoog)) %>%
  ungroup()

U212a        <- left_join(nefropathie, sysbp_130, by="PatientNr") %>%
  filter(     nefropathie==1) %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "U2.1.2a",
              Waarde = ifelse(U212a==1,1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(U212a$Waarde)

# Indicator U2.2 - LDL cholesterol en leeftijd >= 40 jaar
U221a         <- left_join(Patient, Lab, by="PatientNr") %>%
  mutate (    leeftijd=(as.numeric(difftime(startwindow, GeboorteDatum, units = "days")))/365) %>%
  filter(     leeftijd>=40 & BepalingOms=="ldl cholesterol" & (AfnameDatumTijd>=startwindow & AfnameDatumTijd<=stopwindow)) %>%
  group_by(   PatientNr) %>%
  mutate(     Uitslag=as.numeric(Uitslag),
              hoog=ifelse(any(Uitslag<=2.5),0,1)) %>%
  summarise(  U221a=max(hoog)) %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "U2.2.1a",
              Waarde = ifelse(U221a==1,1,0)) %>%
  ungroup() %>% #Toegevoegd
  select(PatientNr, Groep, Ind, Waarde)
table(U221a$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- U3 Acute events -----------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Indicator U3.1 - DKA / U3.2 - hypoglykemie
SEH_U3    <- Verrichting %>%
  filter(     ZACode=="190015" & (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow) & AGB_CodeUitvoerder=="INT") %>% #INT ipv 0313
  group_by(   PatientNr, Verrichtingdatum, ZACode) %>%
  mutate(     Aantal = sum(Aantal)) %>%
  filter(     Aantal >= 1) %>%
  ungroup() %>%
  select(     PatientNr,ZACode,ZAOmschrijving,Verrichtingdatum,AGB_CodeUitvoerder)

Opname_U3  <- Opname %>% 
  select(     PatientNr, OpnameDatumTijd, OntslagDatumTijd, OpnameIndicatie, Spoed) %>%
  filter(     OpnameDatumTijd>=startwindow & OpnameDatumTijd<=stopwindow)

Verrichting_U3    <- Verrichting %>%
  filter(     Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow & Aantal>=1 &
                ((ZACode %in% ZA_ligduurIC) |
                   ((ZACode %in% ZA_ligduur) & AGB_CodeUitvoerder=="INT"))) %>% #0313
  select(     PatientNr,ZACode,ZAOmschrijving,Verrichtingdatum,AGB_CodeUitvoerder)

Opname_U3  <- left_join(Opname_U3, Verrichting_U3, by="PatientNr") %>%
  filter(     Verrichtingdatum>=OpnameDatumTijd & Verrichtingdatum<=OntslagDatumTijd)

#Er staan een aantal strings in dus logisch dat er NA's komen bij as.numeric
Lab_U3    <- Lab %>%
  mutate(     #Uitslag=ifelse(grepl("[+]", Uitslag),1,Uitslag),
    glucose=as.numeric(ifelse(BepalingOms=="glucose",Uitslag,"N/A")),
    ph=as.numeric(ifelse(BepalingOms=="ph",Uitslag,"N/A")),
    bicarbonaat=as.numeric(ifelse(BepalingOms=="bicarbonaat",Uitslag,"N/A")),
    ketonen=as.numeric(ifelse(BepalingOms=="ketonen in urine" & grepl("[+]",Uitslag),1,"N/A")),
    boterzuur=as.numeric(ifelse(BepalingOms=="b-oh boterzuur",Uitslag,"N/A")),
    aniongap=as.numeric(ifelse(BepalingOms=="anion gap",Uitslag,"N/A"))) %>%
  select(     PatientNr,AfnameDatumTijd,glucose,ph,bicarbonaat,ketonen,boterzuur,aniongap)

Events_U3    <- left_join(Opname_U3,Lab_U3,by="PatientNr") %>%
  filter(     AfnameDatumTijd>=OpnameDatumTijd & AfnameDatumTijd<=OntslagDatumTijd) %>%
  group_by(   PatientNr, OpnameDatumTijd) %>%
  select(     PatientNr,OpnameDatumTijd, OpnameIndicatie,glucose,ph,bicarbonaat,ketonen,boterzuur,aniongap,Spoed) %>%
  mutate(     Spoed=ifelse(Spoed=="J",1,0)) %>%
  summarise_all(list(min=~min(.,na.rm = TRUE))) %>%          #Neem het minimum in elke rij voor patientnr en opnamedatumtijd (NA wordt niet meegenomen)
  mutate(     glucose_min = ifelse(is.infinite(glucose_min), NA, glucose_min),
              ph_min = ifelse(is.infinite(ph_min), NA, ph_min),
              bicarbonaat_min = ifelse(is.infinite(bicarbonaat_min), NA, bicarbonaat_min),
              ketonen_min = ifelse(is.infinite(ketonen_min), NA, ketonen_min),
              boterzuur_min = ifelse(is.infinite(boterzuur_min), NA, boterzuur_min),
              aniongap_min = ifelse(is.infinite(aniongap_min), NA, aniongap_min),
              Event_check=ifelse((ph_min<7.35 | bicarbonaat_min<18 | ketonen_min==1 | boterzuur_min>3 | aniongap_min>10),1,0)) %>%
  ungroup()

Events_U3 <- full_join(SEH_U3, Events_U3, by=c("PatientNr", "Verrichtingdatum" = "OpnameDatumTijd"))
#Events_U3 <- full_join(SEH_U3, Events_U3, by=c("PatientNr", ("Verrichtingdatum" + days(1) = "OpnameDatumTijd")))

###-------------------------------------------------------------------------------------------------------
#write.xlsx(Events_U3, "./Events_U3.xlsx")

# WANNEER DOSSIERONDERZOEK GEDAAN DAN INDICATOREN U3 BEREKENEN!

DKA_hypoglykemie <- read.xlsx("DKA_hypoglykemie.xlsx")

U31a <- DKA_hypoglykemie %>% filter(DKA == "Ja") %>%
  distinct(PatientNr, .keep_all = TRUE)

U311 <- DKA_hypoglykemie %>% filter(DKA == "Ja")

U32a <- DKA_hypoglykemie %>% filter(Hypoglykemie == "Ja") %>%
  distinct(PatientNr, .keep_all = TRUE)

U321 <- DKA_hypoglykemie %>% filter(Hypoglykemie == "Ja")

### SEH en opname lijst verrijken met extra ziekenhuis specifieke gegevens rondom een opname (OLVG: opname-indicatie of reden SEH bezoek).
### D.m.v. die informatie en dossieronderzoek achterhalen of de patient was opgenomen vanwege DKA of hypoglykemie.
### Subindicatoren toegevoegd voor aantal opnames vanwege DKA of hypoglykemie.
### Hypoglykemie = glucose <4 mmol/l, maar suiker kan alweer normaal worden gemeten omdat hypo al is opgelost (bijvoorbeeld in ambulance).
###-------------------------------------------------------------------------------------------------------

# Indicator U3.3 - Amputatie onderste extremiteiten
# Check aantal amputaties per patient voor subindicator U3.3.1
amputatie <- Verrichting %>%
  filter(     ZACode %in% ZA_amputatie &  (Verrichtingdatum >= startwindow &
                                             Verrichtingdatum <= stopwindow)) %>%
  group_by(   PatientNr, Verrichtingdatum, ZACode) %>%
  mutate(     Aantal = sum(Aantal)) %>%
  filter(     Aantal >= 1) %>%
  distinct(   PatientNr, Verrichtingdatum, ZACode, Aantal)

U33 <- left_join(Patient, amputatie, by="PatientNr") %>%
  mutate( Groep    = scorekaart_name,
          Ind      = "U3.3",
          Waarde   = ifelse( PatientNr %in% amputatie$PatientNr,1,0) ) %>%
  distinct(PatientNr, Groep, Ind, Waarde)
table(U33$Waarde)

#U3.3.1 is de teller het aantal amputaties en noemer is de teller van U3.3

# Indicator U3.4 - Acuut myocardinfarct
#myocardinfarct     <- c("I21.0", "I21.1", "I21.2", "I21.3", "I21.4", "I21.9", "I22.0", "I22.1", "I22.8", "I22.9")

#incmyocardinfarct  <- Probleemlijst %>%
#  filter(   ICDcode %in% myocardinfarct) %>%
#  filter(   ConstateringsDatum <= stopwindow & ConstateringsDatum >= startwindow) %>%
#  group_by( PatientNr) %>%
#  distinct( PatientNr)

#U34  <- left_join(Patient, incmyocardinfarct, by = "PatientNr") %>%
#  mutate(   Groep  = scorekaart_name,
#            Ind    = "U3.4",
#            Waarde = ifelse( PatientNr %in% incmyocardinfarct$PatientNr,1,0) ) %>%
#  distinct(   PatientNr, Groep, Ind, Waarde)
#table(U34$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- U4 Chronische complicatie -------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Eerst koppelen tabladden Subtraject en Verrichtingen
VerrichtingDiagn <- left_join(Verrichting, Subtraject, by=c("PatientNr", "SubtrajectNr")) %>%
  select(PatientNr, OpnameNr, ZACode, Verrichtingdatum, AGB_CodeUitvoerder, Aantal, DiagnoseCode)

# Indicator U4.1a - Intra-oculaire injecties
injecties <- VerrichtingDiagn %>%
  filter((    ZACode %in% ZA_IV) &  (Verrichtingdatum >= startwindow & Verrichtingdatum <= stopwindow)) %>%
  filter(     DiagnoseCode %in% DBC_retino) %>%
  group_by(   PatientNr, Verrichtingdatum, ZACode) %>%
  mutate(     Aantal = sum(Aantal)) %>%
  filter(     Aantal >= 1) %>%
  distinct(   PatientNr, Verrichtingdatum, ZACode, Aantal)

U41a <- left_join(Patient, injecties, by="PatientNr") %>%
  mutate(     Groep    = scorekaart_name,
              Ind      = "U4.1a",
              Waarde   = ifelse( PatientNr %in% injecties$PatientNr,1,0) ) %>%
  distinct(PatientNr, Groep, Ind, Waarde)
table(U41a$Waarde)

# Indicator U4.2 - Diabetische neuropathie
# O.B.V. JAARCONTROLE
#Neuropathie  <-         Jaarcontrole %>%
#                        filter(     VraagOms == "Neuropathie") %>%
#                        mutate(     Uitslag = as.numeric(Uitslag)) %>%
#                        filter(     !is.na(Uitslag)) %>%
#                        distinct()

#U42         <- left_join(Patient, Neuropathie, by="PatientNr") %>%
#                        mutate(     Groep  = scorekaart_name,
#                                    Ind    = "U4.2",
#                                    Waarde = ifelse(Uitslag==1,1,0)) %>%
#                        select(PatientNr, Groep, Ind, Waarde)
#  table(U42$Waarde)


# O.B.V. PROBLEEMLIJST
#neuropathie        <- c("E10.4", "E11.4", "E12.4", "E13.4", "E14.4",
#                        "E10.4+", "E11.4+", "E12.4+", "E13.4+", "E14.4+") #In MZH plusje erbij

#prevneuropathie  <- Probleemlijst %>%
#  filter(   ICDcode %in% neuropathie) %>%
#  filter(   ConstateringsDatum <= stopwindow) %>%
#  group_by( PatientNr) %>%
#  distinct( PatientNr)

#U42  <- left_join(Patient, prevneuropathie, by = "PatientNr") %>%
#  mutate(   Groep  = scorekaart_name,
#            Ind    = "U4.2",
#            Waarde = ifelse( PatientNr %in% prevneuropathie$PatientNr,1,0) ) %>%
#  distinct(   PatientNr, Groep, Ind, Waarde)
#table(U42$Waarde)

# Indicator U4.7 - Ischemische hartziekte
# O.B.V. DBC
cardiovasc  <-          subset(     Subtraject,(DiagnoseCode %in% DBC_cardiovasc)
                                    & ZorgType=="11"
                                    & SpecialismeCode=='CAR' #CAR ipv 0320??
                                    & (SluitingsDatum>=startwindow & OpeningsDatum <= stopwindow)) %>%
  group_by(   PatientNr) %>%
  summarise(  cardiovasc = first(DiagnoseCode)) %>%
  ungroup()

U47         <- left_join(Patient, cardiovasc, by="PatientNr") %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "U4.7",
              Waarde = ifelse(!is.na(cardiovasc),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(U47$Waarde)

# O.B.V. PROBLEEMLIJST  #BETER OM OBV DBC TE BEPALEN
#ischemie           <- c("I23.0", "I23.1", "I23.2", "I23.3", "I23.4", "I23.5", "I23.6", "I23.8", "I25.0", "I25.1", 
#                        "I25.3", "I25.4", "I25.5", "I25.6", "I25.8", "I25.9")

#previschemie  <- Probleemlijst %>%
#  filter(   ICDcode %in% ischemie) %>%
#  filter(   ConstateringsDatum <= stopwindow) %>%
#  group_by( PatientNr) %>%
#  distinct( PatientNr)

#U47  <- left_join(Patient, previschemie, by = "PatientNr") %>%
#  mutate(   Groep  = scorekaart_name,
#            Ind    = "U4.7",
#            Waarde = ifelse( PatientNr %in% previschemie$PatientNr,1,0) ) %>%
#  distinct(   PatientNr, Groep, Ind, Waarde)
#table(U47$Waarde)  

# Indicator U4.9 - Nierfunctie (LAATSTE METING, alleen berekend m.b.v. CKD-EPI)
nierfun         <- Lab %>%
  filter(     BepalingOms=="egfr" & (AfnameDatumTijd>=startwindow & AfnameDatumTijd<=stopwindow)) %>%
  select(     PatientNr,AfnameDatumTijd,Uitslag) %>%
  mutate(     Uitslag = as.numeric(Uitslag)) %>%
  filter(     !is.na(Uitslag)) %>%
  group_by(   PatientNr) %>%
  filter(     AfnameDatumTijd==max(AfnameDatumTijd)) %>%
  ungroup() %>%
  group_by(   PatientNr, AfnameDatumTijd) %>%
  summarise(  Uitslag= mean(Uitslag)) %>%
  ungroup()

U49a  <- nierfun %>%
  mutate( Groep   = scorekaart_name,
          Ind     = "U4.9a",
          Waarde  = ifelse(Uitslag <= 30, 1, 0),
          Waarde  = ifelse(is.na(Uitslag), 0, Waarde)) %>%
  select( PatientNr, Groep, Ind, Waarde)
table(U49a$Waarde)

# Indicator U4.10 - ACR (LAATSTE METING)
ACR         <- Lab %>%
  filter(     BepalingOms=="acr in urine" & (AfnameDatumTijd>=startwindow & AfnameDatumTijd<=stopwindow)) %>%
  select(     PatientNr,AfnameDatumTijd,Uitslag) %>%
  mutate(     Uitslag = as.numeric(Uitslag)) %>%
  filter(     !is.na(Uitslag)) %>%
  group_by(   PatientNr) %>%
  filter(     AfnameDatumTijd==max(AfnameDatumTijd)) %>%
  ungroup() %>%
  group_by(   PatientNr, AfnameDatumTijd) %>%
  summarise(  Uitslag=mean(Uitslag)) %>%
  ungroup()

U4101a       <- ACR %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "U4.10.1a",
              Waarde = ifelse((Uitslag>=3 & Uitslag <=30),1,0),
              Waarde = ifelse(is.na(Uitslag), 0, Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(U4101a$Waarde)

U4102a       <- ACR %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "U4.10.2a",
              Waarde = ifelse(Uitslag>30,1,0),
              Waarde = ifelse(is.na(Uitslag), 0, Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(U4102a$Waarde)

# Indicator U4.12 - Dialyse
dialyse     <-          subset(     Subtraject,(DiagnoseCode %in% DBC_dialyse) & SpecialismeCode=='INT' & (SluitingsDatum<=stopwindow & OpeningsDatum>=startwindow)) %>%
  group_by(   PatientNr) %>%
  summarise(  dialyse = first(DiagnoseCode)) %>%
  ungroup()

U412        <- left_join(Patient, dialyse, by="PatientNr") %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "U4.12",
              Waarde = ifelse(!is.na(dialyse),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(U412$Waarde)

# Indicator U5.1 - Aantal overleden patienten in het afgelopen jaar (vitale status)
# Vooraf invullen in bronbestand: GBA check!
U51  <- Patient %>%
  group_by(PatientNr) %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "U5.1",
              Waarde = ifelse(OverlijdensDatum>=startwindow & OverlijdensDatum<=stopwindow,1,0),
              Waarde = ifelse(is.na(Waarde),0,Waarde)) %>%
  ungroup() %>%
  select(PatientNr, Groep, Ind, Waarde)
table(U51$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- K1 Hulpmiddelen -----------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Verplaatst naar CaseBehandelmix

###-------------------------------------------------------------------------------------------------------
###----------------------------------- K2 Verblijfsduur ----------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Indicator K2.1 - Aantal ligdagen per opname
Opname_K2       <- Opname %>%
  select(     PatientNr, OpnameNr, OpnameDatumTijd, OntslagDatumTijd, OpnameIndicatie, Spoed) %>%
  filter(     OpnameDatumTijd>=startwindow & OpnameDatumTijd<=stopwindow)

Verrichting_K2  <- VerrichtingDiagn %>%
  filter((    ZACode %in% ZA_ligduur) & (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow)) %>%
  filter(     DiagnoseCode %in% DBC_diabetes) %>%
  filter(     AGB_CodeUitvoerder == "INT") %>%
  filter(     Aantal != 0) %>%
  select(     PatientNr,OpnameNr,ZACode,Verrichtingdatum,AGB_CodeUitvoerder,Aantal,DiagnoseCode)

K21b    <- inner_join(Opname_K2, Verrichting_K2, by=c("PatientNr", "OpnameNr")) %>%
  group_by(   OpnameNr) %>%
  mutate(  Waarde = sum(Aantal)) %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K2.1b") %>%
  ungroup() %>%
  distinct(   PatientNr, Groep, Ind, Waarde)
summary(K21b$Waarde)
sum(K21b$Waarde)

# Indicator K2.2 - Aantal ligdagen op IC
# D.m.v. dossieronderzoek achterhalen of IC opname gerelateerd is aan diabetes
K22a         <- VerrichtingDiagn %>%
  filter((    ZACode %in% ZA_ligduurIC) & (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow)) %>%
  filter(     Aantal != 0) %>%
  group_by(   PatientNr) %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K2.2a",
              Waarde = sum(Aantal)) %>%
  ungroup() %>%
  arrange(PatientNr, Verrichtingdatum) %>%
  distinct(PatientNr, Groep, Ind, Waarde)
summary(K22a$Waarde)
sum(K22a$Waarde)

###-------------------------------------------------------------------------------------------------------
#write.xlsx(K22a, "./nazoeken_IC.xlsx")
# D.M.V. Dossieronderzoek nazoeken of de reden van opname op IC diabetes gerelateerd is
IC_dossieronderzoek <- read.xlsx("Nazoeken_IC_opnames.xlsx")

K22a <- IC_dossieronderzoek %>% filter(Op.IC.voor.diabetes == "Ja") %>%
  group_by(PatientNr) %>%
  summarise(verblijfsduur_IC = n()) %>%
  ungroup()
median(K22a$verblijfsduur_IC)

###-------------------------------------------------------------------------------------------------------

# Indicator K2.3 - SEH opname
SEH         <- VerrichtingDiagn %>%
  filter(     ZACode %in% ZA_SEH & (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow)) %>% 
  filter(     DiagnoseCode %in% DBC_diabetes) %>%
  group_by(   PatientNr) %>%
  mutate(  SEH = sum(Aantal)) %>%
  ungroup() %>%
  distinct(PatientNr, SEH)  #Vraag: Wat moet je met een aantal -1?

K23a         <- left_join(Patient, SEH, by="PatientNr") %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K2.3a",
              Waarde = ifelse(!is.na(SEH),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(K23a$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- K3 Polibezoeken -----------------------------------------------------
###-------------------------------------------------------------------------------------------------------

## Polibezoeken, tel consulten en SEH geregistreerd in diabetes DBC
consulten_internist <- VerrichtingDiagn %>%
  filter(     AGB_CodeUitvoerder=="INT" &
                (DiagnoseCode %in% DBC_diabetes) &
                (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow) &
                (ZACode %in% ZA_consult | ZACode %in% ZA_SEH))

# Indicator K3.1 - Polibezoeken en tel consulten bij internist
## Polibezoeken gekoppeld aan SEH bezoek eruit halen
SEH_bezoeken  <- consulten_internist %>%
  filter(     ZACode %in% ZA_SEH) %>%
  group_by(   PatientNr, Verrichtingdatum, AGB_CodeUitvoerder, DiagnoseCode) %>%
  mutate(     AantalSEH  = sum(Aantal)) %>%
  distinct(   PatientNr, Verrichtingdatum, AGB_CodeUitvoerder, Aantal, DiagnoseCode, AantalSEH )

consulten_int  <-    merge(      x      = consulten_internist %>%
                                   filter( ZACode %in% ZA_consult) %>%
                                   group_by(PatientNr, Verrichtingdatum, AGB_CodeUitvoerder, DiagnoseCode) %>%
                                   mutate(  AantalPoli  = sum(Aantal)) %>%
                                   ungroup() %>%
                                   distinct(PatientNr, Verrichtingdatum, AGB_CodeUitvoerder, Aantal, DiagnoseCode, .keep_all = TRUE),
                                 y      = SEH_bezoeken,
                                 by     = c("PatientNr", "Verrichtingdatum", "AGB_CodeUitvoerder", "DiagnoseCode"),
                                 all.x  = TRUE) %>%
  mutate(     Aantal = ifelse(AantalPoli - AantalSEH <  0 & !is.na(AantalSEH), 0, AantalPoli),
              Aantal = ifelse(AantalPoli - AantalSEH >= 0 & !is.na(AantalSEH), AantalPoli-AantalSEH, AantalPoli)) %>%
  filter(     Aantal>0)

K31         <- left_join(Patient, consulten_int, by="PatientNr") %>%
  group_by(   PatientNr) %>%
  summarise(  Waarde = sum(Aantal)) %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K3.1",
              Waarde = ifelse(!is.na(Waarde),Waarde,0)) %>%
  ungroup() %>%
  select(PatientNr, Groep, Ind, Waarde)
summary(K31$Waarde)
sum(K31$Waarde)
K31 %>% summarize(Aantal = quantile(Waarde, 0.05))
K31 %>% summarize(Aantal = quantile(Waarde, 0.95))

# Subindicator K3.1.1 - Polibezoeken bij internist
poli_int  <- consulten_int %>%
  filter(ZACode %in% ZA_poli)

K311         <- left_join(Patient, poli_int, by="PatientNr") %>%
  group_by(   PatientNr) %>%
  summarise(  Waarde = sum(Aantal)) %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K3.1.1",
              Waarde = ifelse(!is.na(Waarde),Waarde,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
summary(K311$Waarde)
sum(K311$Waarde)

K311 %>% summarize(Aantal = quantile(Waarde, 0.05))
K311 %>% summarize(Aantal = quantile(Waarde, 0.95))

# Subindicator K3.1.2 - Tel consulten bij internist
tele_int  <- consulten_int %>%
  filter(ZACode %in% ZA_tel)

K312         <- left_join(Patient, tele_int, by="PatientNr") %>%
  group_by(   PatientNr) %>%
  summarise(  Waarde = sum(Aantal)) %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K3.1.2",
              Waarde = ifelse(!is.na(Waarde),Waarde,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
summary(K312$Waarde)
sum(K312$Waarde)
K312 %>% summarize(Aantal = quantile(Waarde, 0.05))
K312 %>% summarize(Aantal = quantile(Waarde, 0.95))

# Indicator K3.2 - Diabetes Verpleegkundige
DM_vpk_poli_tele      <- Afspraak %>%
  filter(     #AfspraakStatus=="Afgerond" & 
    (Afdeling=="DIABETESZORG") & #DIABETESZORG is verpleegkundige afspraken
      (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow) &
      (AfspraakCodeOms == "controle bezoek diabetesverpleegkundige" | AfspraakCodeOms == "adolescentenpoli diabetesverpleegkundige" |
         AfspraakCodeOms == "de diabetesverpleegkundige" | AfspraakCodeOms == "Dialysepatienten diabetesverpleegkundige" |
         AfspraakCodeOms == "eerste bezoek" | AfspraakCodeOms == "kliniek consult" |
         AfspraakCodeOms == "klinische diabeteszorg" | AfspraakCodeOms == "telefonisch consult DC" |
         AfspraakCodeOms =="telefonische consulten" | AfspraakCodeOms == "telefonische consulten DC 30min" |
         AfspraakCodeOms == "telefonische consulten dialysepatient" | grepl("Videoconsult", AfspraakCodeOms)))

# LET OP: o.b.v. afspraken! Filter afdeling niet nodig indien er filter diabetes verpleegkundige is.
DM_vpk      <- DM_vpk_poli_tele %>% group_by(PatientNr) %>%
  summarise(  aantal = n()) %>%
  ungroup()

K32         <- left_join(Patient, DM_vpk, by="PatientNr") %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K3.2",
              Waarde = ifelse(!is.na(aantal),aantal,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
summary(K32$Waarde)
sum(K32$Waarde)
K32 %>% summarize(Aantal = quantile(Waarde, 0.05))
K32 %>% summarize(Aantal = quantile(Waarde, 0.95))

# Subindicator K3.2.2 - Tel consulten bij diabetes verpleegkundige
DM_vpk_teleconsulten        <- DM_vpk_poli_tele %>% filter(grepl("telefonisch", AfspraakCodeOms) | grepl("Video", AfspraakCodeOms))

DM_vpk_tel        <- DM_vpk_teleconsulten %>% group_by(PatientNr) %>%
  summarise(  aantal = n()) %>%
  ungroup()


# Subindicator K3.2.1 - Polibezoeken bij diabetes verpleegkundige
DM_vpk_poli_raw <- anti_join(DM_vpk_poli_tele, DM_vpk_teleconsulten)

DM_vpk_poli      <- DM_vpk_poli_raw %>% group_by(PatientNr) %>%
  summarise(  aantal = n()) %>%
  ungroup()

K321               <- left_join(Patient, DM_vpk_poli, by="PatientNr") %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K3.2.1",
              Waarde = ifelse(!is.na(aantal),aantal,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
summary(K321$Waarde)
sum(K321$Waarde)
K321 %>% summarize(Aantal = quantile(Waarde, 0.05))
K321 %>% summarize(Aantal = quantile(Waarde, 0.95))

#Teleconsulten diabetes verpleegkundige
K322               <- left_join(Patient, DM_vpk_tel, by="PatientNr") %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K3.2.2",
              Waarde = ifelse(!is.na(aantal),aantal,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
summary(K322$Waarde)
sum(K322$Waarde)
K322 %>% summarize(Aantal = quantile(Waarde, 0.05))
K322 %>% summarize(Aantal = quantile(Waarde, 0.95))

# Indicator K3.3 - Dietist
dietist     <- Verrichting %>%
  filter((    Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow) &
           (ZACode %in% ZA_Dietist)) %>%
  filter(Aantal != 0) %>%
  group_by(   PatientNr) %>%
  mutate(Waarde = sum(Aantal)) %>%
  ungroup() %>%
  distinct(PatientNr, Waarde)

K33         <- left_join(Patient, dietist, by="PatientNr") %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K3.3",
              Waarde = ifelse(!is.na(Waarde),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(K33$Waarde)

## evt. o.b.v. afspraken
dietist2 <- left_join(Patient, Afspraak, by="PatientNr") %>%
  filter(#AfspraakStatus=="Afgerond" &
           (Afdeling=="DIETETIEK") &
           (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow) #&
           #(SpecialistTypeOms=="Dietist")
             ) %>%
  group_by(PatientNr) %>%
  summarise(aantal = n()) %>%
  ungroup()

length(unique(dietist2$PatientNr))
summary(dietist$aantal)
sum(dietist2$aantal)

x <- full_join(dietist, dietist2, by = "PatientNr")

# Indicator K3.4 - Psycholoog/psychiater
psych       <- Verrichting %>%
  filter((    Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow) &
           (ZACode %in% ZA_psych)) %>%
  filter(Aantal != 0) %>%
  group_by(   PatientNr) %>%
  mutate(Waarde = sum(Aantal)) %>%
  distinct(PatientNr, Waarde)

K34         <- left_join(Patient, psych, by="PatientNr") %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K3.4",
              Waarde = ifelse(!is.na(Waarde),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(K34$Waarde)

## evt. o.b.v. afspraken
psych2 <- Afspraak %>%
  filter(#AfspraakStatus=="Afgerond" &
           (Afdeling=="MEDISCHE PSYCHOLOGIE") &
           (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow)) %>%
  group_by(PatientNr) %>%
  summarise(aantal = n())
length(unique(psych2$PatientNr))
summary(psych$aantal)
sum(psych$aantal)

x <- full_join(psych, psych2, by = "PatientNr")

# Indicator K3.5a - Oogarts/optometrist
polibezoeken_oogarts <- VerrichtingDiagn %>%
  filter(     AGB_CodeUitvoerder=="OOG" & #0301
                (DiagnoseCode %in% DBC_oogarts) &
                (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow) &
                (ZACode %in% ZA_consult | ZACode %in% ZA_SEH))

## Polibezoeken gekoppeld aan SEH bezoek eruit halen
SEH_bezoeken  <- polibezoeken_oogarts %>%
  filter(     ZACode %in% ZA_SEH) %>%
  group_by(   PatientNr, Verrichtingdatum, AGB_CodeUitvoerder, DiagnoseCode) %>%
  mutate(     AantalSEH  = sum(Aantal)) %>%
  distinct(   PatientNr, Verrichtingdatum, AGB_CodeUitvoerder, DiagnoseCode, AantalSEH )

polibezoeken_oogarts <- merge(      x      = polibezoeken_oogarts %>%
                                      filter( ZACode %in% ZA_consult) %>%
                                      group_by(PatientNr, Verrichtingdatum, AGB_CodeUitvoerder, DiagnoseCode) %>%
                                      mutate(  AantalPoli  = sum(Aantal)) %>%
                                      distinct(PatientNr, Verrichtingdatum, AGB_CodeUitvoerder, Aantal, DiagnoseCode, .keep_all = TRUE),
                                    y      = SEH_bezoeken,
                                    by     = c("PatientNr", "Verrichtingdatum", "AGB_CodeUitvoerder", "DiagnoseCode"),
                                    all.x  = TRUE) %>%
  mutate(     Aantal = ifelse(AantalPoli - AantalSEH <  0 & !is.na(AantalSEH), 0, AantalPoli),
              Aantal = ifelse(AantalPoli - AantalSEH >= 0 & !is.na(AantalSEH), AantalPoli-AantalSEH, AantalPoli)) %>%
  filter(     Aantal>0)

optometrie <- VerrichtingDiagn %>%
  filter(     AGB_CodeUitvoerder=="OOG" &
                (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow) &
                (ZACode %in% ZA_optometrie))

oogarts_optometrist <- bind_rows(polibezoeken_oogarts, optometrie)%>%
  group_by(   PatientNr) %>%
  mutate( Waarde = sum(Aantal)) %>%
  ungroup() %>%
  arrange(PatientNr, Verrichtingdatum) %>%
  distinct()

K35a         <- left_join(Patient, oogarts_optometrist, by="PatientNr") %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K3.5a",
              Waarde = ifelse(!is.na(Waarde),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(K35a$Waarde) #dubbele patienten erin.

# Indicator K3.5.1 - Fundusfoto
fundus <- VerrichtingDiagn %>%
  filter((    ZACode %in% ZA_fundus) &
           (Verrichtingdatum >= startwindow & Verrichtingdatum <= stopwindow)) %>%
  group_by(   PatientNr, Verrichtingdatum, ZACode) %>%
  mutate(     Aantal = sum(Aantal)) %>%
  filter(     Aantal >= 1) %>%
  distinct(   PatientNr, Verrichtingdatum, ZACode, Aantal)

K351 <- left_join(Patient, fundus, by="PatientNr") %>%
  mutate( Groep    = scorekaart_name,
          Ind      = "K3.5.1",
          Waarde   = ifelse( PatientNr %in% fundus$PatientNr,1,0) ) %>%
  distinct(PatientNr, Groep, Ind, Waarde)
table(K351$Waarde)

# Indicator K3.6 - Podotherapeut
# LET OP: o.b.v. afspraken! Filter afdeling niet nodig indien er filter podotherapeut is.
#podotherapeut <- Afspraak %>%
#  filter(    #AfspraakStatus=="Afgerond" &
#                SpecialistTypeOms=="Podotherapeut" &
#                (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow)) %>%
#  group_by(   PatientNr) %>%
#  summarise(  aantal = n())

#K36         <- left_join(Patient, podotherapeut, by="PatientNr") %>%
#  mutate(     Groep  = scorekaart_name,
#              Ind    = "K3.6",
#              Waarde = ifelse(!is.na(aantal),1,0)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#table(K36$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- K4 Labaanvragen -----------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Selecteer alle verrichtingen met zorgprofielklasse 8 - Kl. chemie en haematologie
lab         <- Verrichting %>%
  select(     PatientNr, ZACode, AGB_CodeAanvrager, Verrichtingdatum, ZorgprofielKlasse) %>%
  filter(     ZorgprofielKlasse=="8" & AGB_CodeAanvrager=="INT" & (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow)) %>%
  group_by(   PatientNr, Verrichtingdatum) %>%
  summarise(  aantal_lab = n())

# Indicator K4.1 - Aantal poliklinische labbepalingen
lab_klin    <- select(Opname, PatientNr, OpnameDatumTijd, OntslagDatumTijd)

K41a         <- left_join(lab,lab_klin, by="PatientNr") %>%
  mutate(     lab_klin2=ifelse(Verrichtingdatum>=OpnameDatumTijd & Verrichtingdatum<=OntslagDatumTijd,1,0)) %>%
  group_by(   PatientNr, Verrichtingdatum) %>%
  summarise(  lab_klin=max(lab_klin2), aantal_lab=max(aantal_lab)) %>%
  ungroup() %>%
  mutate(     lab_klin=replace_na(lab_klin,0)) %>%
  filter(     lab_klin!=1) %>%
  group_by(   PatientNr) %>%
  summarise(  Waarde = sum(aantal_lab)) %>%
  ungroup()

K41a         <- K41a %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "K4.1a",
              Waarde = ifelse(is.na(Waarde), 0, Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
summary(K41a$Waarde)
sum(K41a$Waarde)
K41a %>% summarize(Aantal = quantile(Waarde, 0.05))
K41a %>% summarize(Aantal = quantile(Waarde, 0.95))

###-------------------------------------------------------------------------------------------------------
###----------------------------------- P1 Doorlooptijden ---------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Indicator P1.2 - Geannuleerde afspraken door patient bij de internist (Patient kan meerdere keren voorkomen)
#P12         <- Afspraak %>%
#  filter((    Afdeling=="DIABETESZORG" | Afdeling=="INTERNE GENEESKUNDE") &
#           (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow) &
#           (AfspraakCodeOms=="Spreekuurbezoek" | AfspraakCodeOms=="Tel consult" | AfspraakCodeOms=="Videoconsult") &
# (AfspraakStatus=="Geannuleerd" | AfspraakStatus=="Afgerond" | AfspraakStatus=="No-show") &
#           (SpecialistTypeOms=="Medisch Specialist" | SpecialistTypeOms=="Verpleegkundig Specialist")) %>%
#  mutate(     Groep  = scorekaart_name,
#              Ind    = "P1.2",
#              Waarde = ifelse((AfspraakStatus=="Geannuleerd" & AnnuleringsReden=="Patient"),1,0)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#table(P12$Waarde)

# Indicator P1.2.1 - Aantal dagen van annulering tot nieuwe afspraak
#P121        <- Afspraak %>%
#  filter((    Afdeling=="DIABETOLOGIE" | Afdeling=="INT") &
#           (AfspraakCodeOms=="Spreekuurbezoek" | AfspraakCodeOms=="Tel consult" | AfspraakCodeOms=="Videoconsult") &
#           (((AfspraakStatus=="Geanuleerd" & AnnuleringsReden=="Patient") & (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow)) |
#              ((AfspraakStatus=="Geannuleerd" | AfspraakStatus=="Afgerond" | AfspraakStatus=="No-show") & (AfspraakDatum>=startwindow & AfspraakDatum<=Eind_FU))) &
#           (SpecialistTypeOms=="Medisch Specialist" | SpecialistTypeOms=="Verpleegkundig Specialist")) %>%
#  mutate(     annuleren=ifelse(((AfspraakStatus=="Geannuleerd" & AnnuleringsReden=="Patient") & (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow)),1,0)) %>%
#  group_by(   PatientNr) %>%
#  arrange(    PatientNr, AfspraakDatum) %>%
# mutate(     Groep  = scorekaart_name,
#              Ind    = "P1.2.1",
#              Waarde = as.numeric(c(difftime(tail(AfspraakDatum, -1), head(AfspraakDatum, -1),units = "days"),0))) %>%
#  filter(     annuleren==1 & Waarde>0) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#summary(P121$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- P2 No shows ---------------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Indicator P2.1 - % No shows bij internist
P21a         <- Afspraak %>%
  filter((    Afdeling=="INTERNE GENEESKUNDE") &
           AfspraakDatum >= startwindow & AfspraakDatum <= stopwindow &
           !grepl("verpleegkundig", AfspraakCodeOms) & !grepl("ietist", AfspraakCodeOms) &
           !grepl("Statusafspraak", AfspraakCodeOms) & !grepl("secretaresse", AfspraakCodeOms) &
           !grepl("oncologievpk", AfspraakCodeOms) & !grepl("physician assistant", AfspraakCodeOms) &
           !grepl("Belafspraak", AfspraakCodeOms) & !grepl("belafspraak", AfspraakCodeOms) & !grepl("video", AfspraakCodeOms) &
           !grepl("Multidisciplinair overleg", AfspraakCodeOms) & !grepl("verplk spec", AfspraakCodeOms) &
           !grepl("bloeddrukmeting", AfspraakCodeOms) &
           (Voldaan=="N" | Voldaan =="J")) %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "P2.1a",
              Waarde=ifelse((Voldaan =="N"),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(P21a$Waarde)


# Indicator P2.1 - % No shows bij diabetes verpleegkundige (er wordt alleen gekeken naar de polibezoeken)
P22         <- DM_vpk_poli_raw %>%
  filter((Voldaan =="N" | Voldaan =="J")) %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "P2.2",
              Waarde=ifelse((Voldaan=="N"),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(P22$Waarde)


###-------------------------------------------------------------------------------------------------------
###----------------------------------- P3 Lost to follow up ------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Lost-to-follow-up is o.b.v. DM1 patienten uit voorgaande cyclus die vervolgens twee jaar geen diabetes DBC hebben 
setwd("/home/afdelingen/kwaliteit.en.veiligheid/Diabetes/Cyclus 6/Data/")
PatientSelectie_voorgaandjaar <- read.xlsx("Patientselectie_2020.xlsx")
setwd("/home/afdelingen/kwaliteit.en.veiligheid/Diabetes/Cyclus 5/")


data_epd <- read.csv("DM_LTFU_DBC_OPNAME_OK_SV.csv",header=T, sep=";", dec=",", stringsAsFactors = F, na.strings="", quote="", fileEncoding = "latin1")

data_epd <- data_epd %>%
  mutate(LeegSubtraject=case_when(is.na(UNIEKE_NR) ~ "Leeg", !is.na(UNIEKE_NR) ~ "Gevuld")) %>% #Kolom toevoegen om aan te geven welke regels lege DBC's zijn (= Leeg).
  select(-UNIEKE_NR)

# Subtraject ----
Subtraject_bron  <- data_epd %>% distinct(PatientNr = PATNR,
                                          ZorgtrajectNr = ZORGTRAJECT_NR,
                                          SubtrajectNr = DBC_NR, 
                                          AGB_Code = DBC_SPEC,  
                                          DiagnoseCode = DIAGNOSE_CODE,             
                                          OpeningsDatumDBC = as.Date(DBC_BEGINDATUM, format = "%d-%m-%Y"),
                                          SluitingsDatumDBC = as.Date(DBC_EINDDATUM, format = "%d-%m-%Y"),
                                          ZorgType = ZORGTYPE_CODE,
                                          LeegSubtraject)

#Follow Up instellen
LTFU               <- as.Date("2021-01-01", format ="%Y-%m-%d")

# Inlezen DBC bestand (LET OP: zie logboek voor welke periode bron bestand vullen)
DBC_LTFU <- Subtraject_bron %>% 
  mutate( LeegSubtraject = ifelse(LeegSubtraject == "Leeg", 1, 0)) %>%
  filter( ZorgType       == "11" | ZorgType      == "21" |
            ZorgType       == "R"  | ZorgType      == "L" ) %>%
  filter(LeegSubtraject == 0 & OpeningsDatumDBC >= LTFU) %>%
  filter(DiagnoseCode %in% c("221", "222", "223")) %>%
  distinct(PatientNr)

LTFU <- PatientSelectie_voorgaandjaar %>%
  filter(!(PatientNr %in% DBC_LTFU$PatientNr))

setwd("/home/afdelingen/kwaliteit.en.veiligheid/Diabetes/Cyclus 6/Data/")
# D.m.v. dossieronderzoek achterhalen welke patienten lost-to-follow-up zijn
#write.xlsx(LTFU, "LTFU_2020.xlsx") #in 2021 pas na te kijken en zal ook onder 2021 staan in logboek

###-------------------------------------------------------------------------------------------------------
###----------------------------------- P4 Moment van labafname ---------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Indicator P4.1 - % patienten met jaar controle lab in afgelopen jaar
jaarlab     <- Lab %>%
  filter((    BepalingOms=="ldl cholesterol" | BepalingOms=="hba1c" | BepalingOms=="acr in urine" | BepalingOms=="Albumine") &
           (AfnameDatumTijd>=startwindow & AfnameDatumTijd <= stopwindow)) %>%
  select(     PatientNr, BepalingOms, AfnameDatumTijd) %>%
  mutate(     LDL=as.numeric(ifelse(BepalingOms=="ldl cholesterol",1,0)),
              hba1c=as.numeric(ifelse(BepalingOms=="hba1c",1,0)),
              ACR=as.numeric(ifelse(BepalingOms=="acr in urine",1,0)),
              albumine=as.numeric(ifelse(BepalingOms=="Albumine",1,0))) %>% #micro albumine in urine wil je eigenlijk, is dit hetzelfde als albumine?
  group_by(   PatientNr) %>%
  summarise(  LDL=max(LDL), hba1c=max(hba1c), ACR=max(ACR), albumine=max(albumine)) %>%
  ungroup()

P41         <- left_join(Patient, jaarlab, by="PatientNr") %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "P4.1",
              Waarde = ifelse(LDL==1 & hba1c==1 & (ACR==1 | albumine==1),1,0),
              Waarde = ifelse(is.na(Waarde),0,Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
table(P41$Waarde)

# Indicator P4.2 - % consulten met lab prikken binnen een week na consult
jaarlab     <- Lab %>%
  filter((    BepalingOms=="ldl cholesterol" | BepalingOms=="hba1c" | BepalingOms=="acr in urine" | BepalingOms=="Albumine") & #micro albumine in urine"
           (AfnameDatumTijd>=startwindow & AfnameDatumTijd <= stopwindow)) %>%
  select(     PatientNr, BepalingOms, AfnameDatumTijd) %>%
  distinct()

## Polibezoeken gekoppeld aan SEH bezoek eruit halen
consulten_int  <-    merge(      x      = consulten_internist %>%
                                   filter( ZACode %in% ZA_consult) %>%
                                   group_by(PatientNr, Verrichtingdatum, AGB_CodeUitvoerder, DiagnoseCode) %>%
                                   mutate(  AantalPoli  = sum(Aantal)) %>%
                                   distinct(PatientNr, Verrichtingdatum, AGB_CodeUitvoerder, DiagnoseCode, .keep_all = TRUE),
                                 y      = SEH_bezoeken,
                                 by     = c("PatientNr", "Verrichtingdatum", "AGB_CodeUitvoerder", "DiagnoseCode"),
                                 all.x  = TRUE) %>%
  mutate(     Aantal = ifelse(AantalPoli - AantalSEH <=  0 & !is.na(AantalSEH), 0, AantalPoli),
              Aantal = ifelse(AantalPoli - AantalSEH > 0 & !is.na(AantalSEH), AantalPoli-AantalSEH, AantalPoli)) %>%
  filter(     Aantal>0)

na_consult  <- left_join(consulten_int, jaarlab, by="PatientNr") %>%
  mutate(     week=ifelse(AfnameDatumTijd-Verrichtingdatum>=0 & AfnameDatumTijd-Verrichtingdatum<=7,1,0)) %>%
  filter(     week==1) %>%
  group_by(   PatientNr, Verrichtingdatum) %>%
  summarise(  week=max(week))

noemer_P42  <- sum(consulten_int$Aantal)
teller_P42  <- sum(na_consult$week)

##----------------------------------------------------------------------------------------------------
##------------------------------------ OPSLAAN DATA ----------------------------------------------------
##----------------------------------------------------------------------------------------------------

# Invullen met diabetes casemix
case_mix   <- as.data.frame( rbind(lft, gsl, bmi, rok, retino, pomp, rtcgm, Flash, combi, HbA1c)) %>% # diagnose, alc, ther,
  mutate(Wegingsfactor=1,
         Aandoening    =  "Diabetes", 
         Cyclus        =  Cyclus_nr)

# Invullen met diabetes indicatoren
Indicatoren <- as.data.frame( rbind(U12, U211a, U212a, U221a, U33, U41a, U47, U49a, U4101a, U4102a, U412, U51,
                                    K21b, K23a, K31, K311, K312, K32, K321, K322, K33, K34, K35a, K351, K41a, P21a,
                                    P22, P41)) %>%  #U11, U34, U42a, P12, P121, K22a   K36
  mutate(Wegingsfactor=1,
         Aandoening    =  "Diabetes", 
         Cyclus        =  Cyclus_nr)

OutputDataSet <- rbind(case_mix, Indicatoren)

# Save indicatoren to R-file
save(OutputDataSet, file = paste0("OutputDataSet_", as.character(Cyclus_nr), ".rda") )
write.xlsx(OutputDataSet, paste0("OutputDataSet_", as.character(Cyclus_nr), ".xlsx"), col_names = TRUE)

save.image("DM_C5_2021.RDATA")

#LET OP: handmatige indicatoren     U3.1 Ketoacidose
#                                   U3.2 Hypo
#                                   HbA1c tabel
#                                   K2.2 Verblijfsduur IC gerelateerd aan diabetes
#                                   P3 Lost-to-follow-up
#                                   P4.2 Moment van lab prikken

#write.xlsx(HbA1c, file = "HbA1c_C5.xlsx")
