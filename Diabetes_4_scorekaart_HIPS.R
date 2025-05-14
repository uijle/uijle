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
setwd("//olvg.nl/dfs/Groups02/5125/Concernstaf/1. Kwaliteit & veiligheid/4 Transparantie/VBHC/VBHC_data/Diabetes/Cyclus 5/Data")
#save.image('Image 20240815.RData')
#load('Image 20240815.RData')
#save.image('Image 20241103.RData')
#load('Image 20241103.RData')

#PatientSelectie <- PatientSelectie %>% 
#  select(-Groep)

# Selecteer DM1 patienten uit cyclus 5, en groep 1
input_hips <- Subtraject_stap9  %>%
                      filter((Groep != 4 )) 
                               # Cyclus == "C5" &
                                  #(TypeDM == "Type 1" | TypeDM == "LADA"))
#RL voor HIPS: input dataframe maken
input_hips <- input_hips %>% 
 #left_join(PatientSelectie, by = "PatientNr") %>% 
 mutate(Aandoening = "diabetes",
        # Groep = "volwassenen",
         ZiekenhuisCode = "olvg", 
         DM_stop=as.Date(DM_stop)) %>% #aanvulling Whitney
  rename(Identificatienummer = PatientNr,
         InclusieDatum = DM_start,
         EindDatum = DM_stop) %>% 
  select(Aandoening, Groep, Identificatienummer, InclusieDatum, EindDatum, ZiekenhuisCode)

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
  mutate(leeftijd = ceiling((as.numeric(difftime(startwindow, GeboorteDatum, units = "days"))/365) - 0.5))%>% #WF: geboortedatum afronden naar hele jaren
  select(PatientNr, leeftijd) %>% # RL: voor HIPS
  rename(lft = leeftijd,
         Identificatienummer = PatientNr)  # RL: voor HIPS

# RL: voor HIPS
input_hips = input_hips %>% 
  left_join(leeftijd, by = "Identificatienummer")

# lft         <- leeftijd %>%  mutate( Groep  = case_mix_name,
                                     #Ind    = "lft") %>% 
                                     # Waarde = ifelse( leeftijd < 30, 1, NA),
                                     # Waarde = ifelse( leeftijd >= 30 & leeftijd < 50, 2, Waarde),
                                     # Waarde = ifelse( leeftijd >= 50 & leeftijd < 70, 3, Waarde),
                                     # Waarde = ifelse( leeftijd >= 70, 4, Waarde)) %>%
               # select(PatientNr, Groep, Ind, Waarde)
               # table(lft$Waarde)


# --------------- Geslacht ---------------------------------------------------------------------------
gsl         <- Patient %>%  mutate( #Groep  = case_mix_name,
                                    #Ind    = "gsl")
                                    # Waarde = ifelse(Geslacht == "M",1,NA),
                                    Geslacht = ifelse(Geslacht == "V","F",Geslacht)) %>% #RL: aanpassing V naar F voor volgens HIPS definitie
                                    # Waarde = ifelse(is.na(Geslacht) | Geslacht == "NULL", 99, Waarde)) %>%
                            select(PatientNr, Geslacht) %>% 
                            rename(gsl = Geslacht, 
                                   Identificatienummer = PatientNr) # RL: voor HIPS
               # table(gsl$Waarde)

# RL: voor HIPS
input_hips = input_hips %>% 
  left_join(gsl, by = "Identificatienummer")

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
#bmi <- Patient %>% 
 # mutate(bmi = Gewicht/(Lengte*Lengte)) %>% 
 # select(PatientNr, bmi) %>% # RL: voor HIPS
 # rename(Identificatienummer = PatientNr)# RL: voor HIPS

#bmi     <- Patient %>% mutate( Groep  = case_mix_name,
                               #Ind    = "bmi") %>% 
                               # Waarde = ifelse(BMI <  18.5, 1, NA),
                               # Waarde = ifelse(BMI >= 18.5 & BMI <  25.0, 2, Waarde),
                               # Waarde = ifelse(BMI >= 25.0 & BMI <  30.0, 3, Waarde),
                               # Waarde = ifelse(BMI >= 30.0, 4, Waarde),
                               # Waarde = ifelse(is.na(BMI),  99, Waarde)) %>%
                    #select(PatientNr, BMI) Groep, Ind, Waarde)
                    #table(bmi$Waarde)

# RL: voor HIPS
#input_hips = input_hips %>% 
  #left_join(bmi, by = "Identificatienummer")
                   
# ---------------------------------- Etniciteit ----------------------------------     
#etn        <- Metingen %>%
                    #filter(ObservatieOms == "Etniciteit") %>%
                   # group_by(PatientNr) %>% 
                    #rename(etn = Uitslag,# RL: voor HIPS 
                          # Identificatienummer = PatientNr) %>% # RL: voor HIPS 
                    #select(Identificatienummer, etn)# RL: voor HIPS 
# RL: voor HIPS                    
#etn        <- etn %>% left_join(Patient, etn, by="PatientNr") %>%  
                    #mutate( Groep  = case_mix_name,
                            #Ind    = "etn") %>% 
                            # Waarde = ifelse(Uitslag == "Kaukasisch", 1, NA),
                            # Waarde = ifelse(Uitslag == "Noord-Afrikaans", 2, Waarde),
                            # Waarde = ifelse(Uitslag == "Overig Afrikaans", 3, Waarde),
                            # Waarde = ifelse(Uitslag == "Turks- en Caicoseilanden", 4, Waarde),
                            # Waarde = ifelse(Uitslag == "Hindoestaans", 5, Waarde),
                            # Waarde = ifelse(Uitslag == "Overig Aziatisch", 6, Waarde),
                            # Waarde = ifelse(Uitslag == "Latijns Amerikaans", 7, Waarde),
                            # Waarde = ifelse(Uitslag == "Meervoudige afkomst", 8, Waarde),
                            # Waarde = ifelse(Uitslag == "Onbekend", 99, Waarde),
                            # Waarde = ifelse(is.na(Waarde), 99, Waarde)) %>%
                              #etn = Uitslag) %>% 
                   #select(PatientNr, etn) Groep, Ind, Waarde)
                   #table(etn$Waarde) 

# RL: voor HIPS
#input_hips = input_hips %>% 
  #left_join(etn, by = "Identificatienummer")
                    
# ---------------------------------- Sociaal economische status ----------------------------------
#ses        <- Metingen %>%
#                    filter(ObservatieOms == "Hoogst genoten opleiding") %>%
#                    group_by(PatientNr)
                    
#ses        <- left_join(Patient, ses, by="PatientNr") %>%  
#                    mutate( Groep  = case_mix_name,
#                            Ind    = "ses",
#                            Waarde = ifelse(Uitslag == "Basisschool", 1, NA),
#                            Waarde = ifelse(Uitslag == "Lager beroepsonderwijs", 2, Waarde),
#                            Waarde = ifelse(Uitslag == "vmbo-t", 3, Waarde),                    
#                            Waarde = ifelse(Uitslag == "Middelbaar beroepsonderwijs", 4, Waarde),
#                            Waarde = ifelse(Uitslag == "Hoger algemeen onderwijs", 5, Waarde),
#                            Waarde = ifelse(Uitslag == "Hoger beroepsonderwijs", 6, Waarde),
#                            Waarde = ifelse(Uitslag == "Wetenschappelijk onderwijs", 7, Waarde),
#                            Waarde = ifelse(Uitslag == "Anders", 8, Waarde),
#                            Waarde = ifelse(Uitslag == "Onbekend", 99, Waarde),
#                            Waarde = ifelse(is.na(Waarde), 99, Waarde)) %>%
#                    select(PatientNr, Groep, Ind, Waarde)
#                    table(ses$Waarde)

#RL voor HIPS: ses (=opl) niet meegenomen in cyclus 5, dossiercheck? 
#input_hips = input_hips %>% 
 # mutate(opl = NA)

# ---------------------------------- Diagnose sinds ------------------------------------------
# Datum diagnose o.b.v. ConstateringsDatum (Probleemlijst) of diagdat (DPARD)
# O.b.v. PROBLEEMLIJST

diagduur    <- Probleemlijst %>% # RL: voor HIPS: diagnose in diagduur veranderd
                  filter( grepl("E10", ICDcode)) %>%
                  filter( ICDcode != "E10.4") %>%
                  mutate (diagduur=ceiling(as.numeric(difftime(startwindow, ConstateringsDatum, units = "days")/365)-0.5)) %>% #WF: diagduur afronden naar hele jaren
                  select(PatientNr, diagduur) %>% 
                  rename(Identificatienummer = PatientNr)# RL: voor HIPS

#RL voor HIPS
input_hips = input_hips %>% 
  left_join(diagduur, by = "Identificatienummer")

# RL: voor HIPS
#diagnose         <- diagnose %>%  mutate( Groep  = case_mix_name,
                                      #Ind    = "diagnose") %>% 
                                      # Waarde = ifelse( diagnose <= 0, 1, NA),
                                      # Waarde = ifelse( diagnose > 0 & diagnose <= 5, 2, Waarde),
                                      # Waarde = ifelse( diagnose > 5 & diagnose <= 10, 3, Waarde),
                                      # Waarde = ifelse( diagnose > 10 & diagnose <=15, 4, Waarde),
                                      # Waarde = ifelse( diagnose > 15, 5, Waarde),
                                      # Waarde = ifelse( is.na(diagnose ), 99, Waarde)) %>%
                    #select(PatientNr, Groep, Ind, Waarde)
                    #table(diagnose$Waarde)

# O.b.v. JAARCONTROLE
#diagnose     <- Jaarcontrole %>%
#                  filter(VraagOms=="DiagnoseJaar") %>%
#                  mutate(diagnose = as.numeric(Uitslag)) %>%
#                  filter(!is.na(diagnose)) %>%
#                  distinct()%>%
#                  mutate(diagnose = as.numeric( format(startwindow, format = "%Y") )- diagnose)

#diagnose         <- left_join(Patient, diagnose, by="PatientNr") %>%
#                                mutate( Groep  = case_mix_name,
#                                        Ind    = "diagnose",
#                                        Waarde = ifelse( diagnose <= 0, 1, NA),
#                                        Waarde = ifelse( diagnose > 0 & diagnose <= 5, 2, Waarde),
#                                        Waarde = ifelse( diagnose > 5 & diagnose <= 10, 3, Waarde),
#                                        Waarde = ifelse( diagnose > 10 & diagnose <=15, 4, Waarde),
#                                        Waarde = ifelse( diagnose > 15, 5, Waarde),
#                                        Waarde = ifelse( is.na(diagnose ), 99, Waarde)) %>%
#                   select(PatientNr, Groep, Ind, Waarde)
#                   table(diagnose$Waarde)

# ---------------------------------- Roken -------------------------------------------------
# O.b.v. JAARCONTROLE
#rok     <- Jaarcontrole %>%
#              filter(VraagOms=="Roken") %>%               #LET OP: IN STA GECODEERD ALS 1 = 'ja, 0 = 'nee'
#              mutate(rok = as.numeric(Uitslag)) %>%
#              filter(!is.na(rok)) %>%
#              distinct()

#rok       <- left_join(Patient, rok, by="PatientNr") %>%
#                              mutate( Groep  = case_mix_name,
#                                      Ind    = "rok",
#                                      Waarde = ifelse(rok == 0, 0, NA),
#                                      Waarde = ifelse(rok == 1, 1, Waarde),
#                                      Waarde = ifelse(is.na(Waarde), 99, Waarde)) %>%
#                select(PatientNr, Groep, Ind, Waarde)
#                table(rok$Waarde)


# O.b.v. EPD FLOWSHEETMETING
rok       <- Metingen %>%
               filter(ObservatieOms=="Roken") %>%
               group_by(PatientNr) %>%
               filter(ObservatieDatum==max(ObservatieDatum)) %>% 
  #mutate(Uitslag = case_when(Uitslag == "Rookt" ~ "Rookt dagelijks tabak", # RL voor HIPS: uitkomsten aanpassen naar valueset_TabakGebruikStatusCodelijst.csv 
                             #Uitslag == "Rookt soms" ~ "Rookt soms tabak",
                             #Uitslag == "Rookt passief" ~ "Rookt passief tabak",
                             #Uitslag == "Niet roker, vroeger onbekend" ~ "Niet-roker, maar rookgedrag in verleden onbekend",
                             #Uitslag == "Ex-roker" ~ "Rookte vroeger",
                             #Uitslag == "Nooit gerookt" ~ "Heeft nooit gerookt",
                             #Uitslag == "Onbekend" ~ "Tobacco smoking consumption unknown")) %>% 
               rename(rok = Uitslag, 
                      Identificatienummer = PatientNr) %>% # RL voor HIPS
               select(Identificatienummer, rok)  # RL voor HIPS
#table(rok$Waarde)

#rok       <- left_join(Patient, rok, by="PatientNr") %>%  
                              #mutate( Groep  = case_mix_name,
                                      #Ind    = "rok") %>% 
                                      # Waarde = ifelse(Uitslag == "Rookt", 1, NA),
                                      # Waarde = ifelse(Uitslag == "Ex-roker", 0, Waarde),
                                      # Waarde = ifelse(Uitslag == "Nooit gerookt" , 0, Waarde),
                                      # Waarde = ifelse(Uitslag == "Onbekend", 99, Waarde),
                                      # Waarde = ifelse(is.na(Waarde), 99, Waarde)) %>%
             #select(PatientNr, Groep, Ind, Waarde)
              #table(rok$Waarde)

input_hips = input_hips %>% 
  left_join(rok, by = "Identificatienummer")

# ----------------------- Alcohol consumptie --------------------------------------------------
# O.b.v. DPARD
#alcohol     <- DPARD   %>%   mutate( Groep  = case_mix_name,
#                                     Ind    = "alc",
#                                     Waarde = ifelse(is.na(alcoholgebruikstatus), 99, alcoholgebruikstatus)) %>%
#              select(PatientNr, Groep, Ind, Waarde)

# ----------------------- HbA1c ----------------------------------------------------------
# HbA1c wordt berekend onder Uitkomstindicatoren.

# ----------------------- Retinopathie ---------------------------------------------------
#retinopathie  <-  subset(     Subtraject,(DiagnoseCode %in% DBC_retino)
                                          #& SpecialismeCode=='0301' & (OpeningsDatum<=stopwindow & SluitingsDatum>=startwindow)) %>%
                              #group_by(   PatientNr) %>%
                              #summarise(  retinopathie = first(DiagnoseCode))

#retino           <-  left_join(Patient, retinopathie, by="PatientNr") %>%
                                #mutate(     Groep  = case_mix_name,
                                            #Ind    = "retino",
                                           # Waarde = ifelse(!is.na(retinopathie),1,0)) %>%
                     #select(PatientNr, Groep, Ind, Waarde)
                     #table(retino$Waarde)

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

# Cyclus 2: O.b.v. JAARCONTROLE
#Insuline     <- Jaarcontrole %>%
#                            filter(VraagOms=="Insuline") %>%
#                            mutate(Insuline = as.character(Uitslag)) %>%
#                            filter(!is.na(Insuline)) %>%
#                            distinct()

#Insuline     <- left_join(Patient, Insuline, by="PatientNr") %>%
#                            mutate( Groep  = case_mix_name,
#                                    Ind    = "insul",
#                                    Waarde = ifelse(Variabele == 'kortwerkend',1,NA),
#                                    Waarde = ifelse(Variabele == 'middellangwerkend',2,Waarde),
#                                    Waarde = ifelse(Variabele == 'langwerkend',3,Waarde),
#                                    Waarde = ifelse(Variabele == 'mix',4,Waarde),
#                                    Waarde = ifelse(is.na(Waarde),99,Waarde))%>%
#                            distinct(PatientNr, Groep, Ind, Waarde)
#                            table(Insuline$Waarde)

# ------------------- Insuline therapie ----------------------------------------------
# O.b.v. JAARCONTROLE
# Check patienten met alleen tabletten. Dit zal altijd in combinatie zijn met pomp of pen.
# Categoriseer ze vervolgens in juiste categorie, anders bij 'onbekend'.
ther     <- Jaarcontrole %>%
                            filter(VraagOms=="Insulinetherapie") %>%
                            mutate(ther = as.character(Uitslag)) %>%
                            filter(!is.na(ther)) %>%
                            distinct()
#  RL: voor HIPS | vraag: tabletten worden bij HIPS niet meegenomen? 
ther     <- left_join(Patient, ther, by="PatientNr") %>%
                            mutate( Groep  = case_mix_name,
                                    Ind    = "ther") %>% 
                                    # Waarde = ifelse(ther == 'pomp',1,NA),
                                    # Waarde = ifelse(ther == 'pen',2,Waarde),
                                    # Waarde = ifelse(ther == 'tabletten',3,Waarde),
                                    # Waarde = ifelse(is.na(Waarde),99,Waarde))%>%
                            distinct(PatientNr, Groep, Ind, ther) #Waarde)
                            #table(ther$Waarde)
# Controle: Pomp o.b.v. DBC's
# Check verschil tussen jaarcontrole en DBC (bijv: patient in begin inclusie periode pomp, vervolgens over op pen)
# Deze indicator wordt ook gebruikt bij combi pomp/RT-CGM
pomp        <- Subtraject %>%
                            filter(     DiagnoseCode=="223" & (SluitingsDatum>=startwindow & OpeningsDatum<=stopwindow)) %>%
                            group_by(   PatientNr) %>%
                            summarise(  pomp = n())

pomp         <- left_join(Patient, pomp, by="PatientNr") %>%
                            mutate(     Groep  = case_mix_name,
                                        Ind    = "pomp") %>% 
                                        Waarde = ifelse(!is.na(pomp),1,0) %>%
                            select(PatientNr, Groep, Ind, Waarde)
                            table(pomp$Waarde)

# ------------------- Real-time continue glucosemeter --------------------------------
# Real-time continue glucosemeter wordt geregistreerd met ZACode 190351
# rtcgm       <- Verrichting %>%
#                             filter(     ZACode=="190351" & (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow)) %>%
#                             group_by(   PatientNr) %>%
#                             summarise(  rtcgm = n())
# 
# rtcgm         <- left_join(Patient, rtcgm, by="PatientNr") %>%
#                             mutate(     Groep  = case_mix_name,
#                                         Ind    = "rtcgm") %>% 
#                                         # Waarde = ifelse(!is.na(rtcgm),1,0)) %>%
#                 select(PatientNr, Groep, Ind, Waarde)
#                 table(rtcgm$Waarde)

# RL: voor HIPS > rtcgm wordt voor HIPS als meisch hulpmiddel opgehaald niet (meer) als verrichting, maakt dat uit? Samenvoegen met fgm (en combi?) voor validatiescript
# O.b.v. JAARCONTROLE
rtcgm     <- Jaarcontrole %>%
                            filter(     VraagOms=="rtcgm") %>%
                            mutate(     rtcgm = as.numeric(Uitslag)) %>%  #LET OP: IN STA GECODEERD ALS 1 = 'ja, 0 = 'nee'
                            filter(!is.na(rtcgm)) %>%
                            distinct()

rtcgm       <- left_join(Patient, rtcgm, by="PatientNr") %>%
                          mutate(     Groep  = case_mix_name,
                                      Ind    = "rtcgm",
                                      Waarde = ifelse(!is.na(rtcgm),1,0)) %>%
                select(PatientNr, Groep, Ind, Waarde)
                table(rtcgm$Waarde)

# ----------------------- Flash glucose meter --------------------------------------------------
# Flash glucose meter wordt vergoed door basisverzekering, in eigen ziekenhuis achterhalen hoe dit wordt geregistreerd
# O.b.v. JAARCONTROLE
fgm     <- Jaarcontrole %>%
                            filter(     VraagOms=="Flash") %>%
                            mutate(     fgm = as.numeric(Uitslag)) %>%  #LET OP: IN STA GECODEERD ALS 1 = 'ja, 0 = 'nee'
                            filter(!is.na(fgm)) %>%
                            distinct()

fgm     <- left_join(Patient, fgm, by="PatientNr") %>%
                            mutate(     Groep  = case_mix_name,
                                        Ind    = "fgm",
                                        Waarde = ifelse(fgm == 0, 0, NA),
                                        Waarde = ifelse(fgm == 1, 1, Waarde),
                                        Waarde = ifelse(is.na(fgm), 99, Waarde)) %>%
                            select(PatientNr, Groep, Ind, Waarde)
                            table(fgm$Waarde)

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
                        mutate(  Uitslag = as.numeric(Uitslag)) %>%
                        filter(  BepalingOms=="hba1c" & (AfnameDatumTijd>=startwindow_lab & AfnameDatumTijd<=stopwindow) & !is.na(Uitslag)) %>%
                        group_by(PatientNr, AfnameDatumTijd) %>%
                        mutate(  Uitslag          = mean(as.numeric(Uitslag), na.rm = TRUE) ) %>%
                        distinct(PatientNr, AfnameDatumTijd, .keep_all =TRUE) %>%
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
                                verschil = laatste-eerste)

hba1c_cat   <- hba1c %>%
                        filter(     min <=90) %>%
                        select(     PatientNr, eerste, laatste, verschil) %>%
                        distinct(   PatientNr, eerste, laatste, verschil) %>%
                        mutate(     cat_eerste=cut(eerste, breaks=c(-Inf, 53, 64, 74, Inf), labels=c("<=53",">53,<65",">=65,<75",">=75")),
                                    cat_laatste=cut(laatste, breaks=c(-Inf, 53, 64, 74, Inf), labels=c("<=53",">53,<65",">=65,<75",">=75")))

##### VOOR CASEMIX #####
HbA1c    <- hba1c %>%
                        filter(     AfnameDatumTijd == max(AfnameDatumTijd))

HbA1c    <- left_join(Patient, HbA1c, by="PatientNr") %>%
                        mutate(     Uitslag = as.numeric(Uitslag),
                                    cat_laatste=cut(Uitslag, breaks=c(-Inf, 53, 64, 74, Inf), labels=c("<=53",">53,<65",">=65,<75",">=75"))) %>%
                        #mutate    ( Groep  = case_mix_name, #RL voor HIPS
                                   # Ind    = "HbA1c",
                                   # Waarde = ifelse(cat_laatste=="<=53", 1, 0),
                                   # Waarde = ifelse(cat_laatste==">53,<65", 2, Waarde),
                                   # Waarde = ifelse(cat_laatste==">=65,<75", 3, Waarde),
                                   # Waarde = ifelse(cat_laatste==">=75", 4, Waarde),
                                   # Waarde = ifelse(is.na(cat_laatste), 99, Waarde)) %>%
                        rename(hba1c = Uitslag, #RL voor HIPS
                               Identificatienummer = PatientNr) %>%  #RL voor HIPS
                        select(Identificatienummer, hba1c) #RL voor HIPS
                        #table(HbA1c$Waarde)
  
input_hips = input_hips %>% 
  left_join(HbA1c, by = "Identificatienummer") #RL voor HIPS

##### VOOR TABBLAD HbA1c tabel #####
table(hba1c_cat$cat_eerste, hba1c_cat$cat_laatste) #horizontale categorieen = laatste meting, verticale categorieen = eerste meting

# Indicator U1.1
#U11         <- hba1c_cat %>%
#                        filter(     cat_laatste!="<=53") %>%
#                        mutate(     Groep  = scorekaart_name,
#                                    Ind    = "U1.1",
#                                    Waarde = ifelse(verschil>=10,1,0)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#  table(U11$Waarde)

# Indicator U1.2
U12         <- hba1c_cat %>%
  mutate(     Groep  = scorekaart_name,
              Ind    = "U1.2",
              Waarde = ifelse(cat_eerste==">=75" & cat_laatste==">=75",1,0)) %>%
  rename(U1.2=Waarde, Identificatienummer = PatientNr) %>%  
  select(Identificatienummer, U1.2) 

# df HIPS  
input_hips = input_hips %>% 
  left_join(U12, by = "Identificatienummer")
  
###-------------------------------------------------------------------------------------------------------
###----------------------------------- U2 Intermediate outcomes --------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Indicator U2.1.1 - Systolische bloeddruk
sysbp       <- Metingen %>%
                        filter(     ObservatieOms=="SysBloeddruk" & (ObservatieDatum >=startwindow & ObservatieDatum <= stopwindow)) %>%
                        group_by(   PatientNr) %>%
                        mutate(     Uitslag=as.numeric(Uitslag),
                                    hoog=ifelse(any(Uitslag<140),0,1)) %>%
                        summarise(  Waarde=max(hoog))

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
                        summarise(  nefropathie=max(nefropathie))

sysbp_130   <- Metingen %>%
                        filter(     ObservatieOms=="SysBloeddruk" & (ObservatieDatum >= startwindow & ObservatieDatum <= stopwindow)) %>%
                        group_by(   PatientNr) %>%
                        mutate(     Uitslag=as.numeric(Uitslag),
                                    hoog=ifelse(any(Uitslag<130),0,1)) %>%
                        summarise(  U212a=max(hoog))

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
  select(PatientNr, Groep, Ind, Waarde)
  table(U221a$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- U3 Acute events -----------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Indicator U3.1 - DKA / U3.2 - hypoglykemie
SEH_U3    <- Verrichting %>%
                filter(     ZACode=="190015" & (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow) & AGB_CodeUitvoerder=="0313") %>%
                group_by(   PatientNr, Verrichtingdatum, ZACode) %>%
                mutate(     Aantal = sum(Aantal)) %>%
                filter(     Aantal >= 1) %>%
                select(     PatientNr,ZACode,ZAOmschrijving,Verrichtingdatum,AGB_CodeUitvoerder)

Opname_U3  <- Opname %>%
                select(     PatientNr, OpnameDatumTijd, OntslagDatumTijd, OpnameIndicatie, Spoed) %>%
                filter(     OpnameDatumTijd>=startwindow & OpnameDatumTijd<=stopwindow)

Verrichting_U3    <- Verrichting %>%
                      filter(     Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow & Aantal>=1 &
                            ((ZACode %in% ZA_ligduurIC) |
                            ((ZACode %in% ZA_ligduur) & AGB_CodeUitvoerder=="0313"))) %>%
                      select(     PatientNr,ZACode,ZAOmschrijving,Verrichtingdatum,AGB_CodeUitvoerder)

Opname_U3  <- left_join(Opname_U3, Verrichting_U3, by="PatientNr") %>%
                filter(     Verrichtingdatum>=OpnameDatumTijd & Verrichtingdatum<=OntslagDatumTijd)

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
                  summarise_all(list(min=~min(.,na.rm = TRUE))) %>%
                  mutate(     glucose_min = ifelse(is.infinite(glucose_min), NA, glucose_min),
                                ph_min = ifelse(is.infinite(ph_min), NA, ph_min),
                                bicarbonaat_min = ifelse(is.infinite(bicarbonaat_min), NA, bicarbonaat_min),
                                ketonen_min = ifelse(is.infinite(ketonen_min), NA, ketonen_min),
                                boterzuur_min = ifelse(is.infinite(boterzuur_min), NA, boterzuur_min),
                                aniongap_min = ifelse(is.infinite(aniongap_min), NA, aniongap_min),
                                Event_check=ifelse((ph_min<7.35 | bicarbonaat_min<18 | ketonen_min==1 | boterzuur_min>3 | aniongap_min>10),1,0))

Events_U3 <- full_join(SEH_U3, Events_U3, by=c("PatientNr", "Verrichtingdatum" = "OpnameDatumTijd"))
#Events_U3 <- full_join(SEH_U3, Events_U3, by=c("PatientNr", ("Verrichtingdatum" + days(1) = "OpnameDatumTijd")))

###-------------------------------------------------------------------------------------------------------
#write.xlsx(Events_U3, "./Events_U3.xlsx")
write.xlsx(Events_U3, "//olvg.nl/dfs/Groups02/5125/Concernstaf/1. Kwaliteit & veiligheid/4 Transparantie/VBHC/VBHC_data/Diabetes/Cyclus 5/Data/Events_U3.xlsx")
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
                        distinct(   PatientNr, Verrichtingdatum, ZACode, ZAOmschrijving, Aantal)

write.xlsx(amputatie, "//olvg.nl/dfs/Groups02/5125/Concernstaf/1. Kwaliteit & veiligheid/4 Transparantie/VBHC/VBHC_data/Diabetes/Cyclus 5/Checks/U3.3_amputatie_C5.xlsx")

U33 <- left_join(Patient, amputatie, by="PatientNr") %>%
                        mutate( Groep    = scorekaart_name,
                                Ind      = "U3.3",
                                Waarde   = ifelse( PatientNr %in% amputatie$PatientNr,1,0) ) %>%
                        distinct(PatientNr, Groep, Ind, Waarde)
        table(U33$Waarde)

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

# Indicator U4.7 - Ischemische hartziekte
# O.B.V. DBC
cardiovasc  <-          subset(     Subtraject,(DiagnoseCode %in% DBC_cardiovasc)
                                                & ZorgType=="11"
                                                & SpecialismeCode=='0320'
                                                & (SluitingsDatum>=startwindow & OpeningsDatum <= stopwindow)) %>%
                        group_by(   PatientNr) %>%
                        summarise(  cardiovasc = first(DiagnoseCode))

U47         <- left_join(Patient, cardiovasc, by="PatientNr") %>%
                        mutate(     Groep  = scorekaart_name,
                                    Ind    = "U4.7",
                                    Waarde = ifelse(!is.na(cardiovasc),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
  table(U47$Waarde)

# Indicator U4.9 - Nierfunctie (LAATSTE METING, alleen berekend m.b.v. CKD-EPI)
nierfun         <- Lab %>%
                      filter(     BepalingOms=="egfr" & (AfnameDatumTijd>=startwindow & AfnameDatumTijd<=stopwindow)) %>%
                      select(     PatientNr,AfnameDatumTijd,Uitslag) %>%
                      mutate(     Uitslag = as.numeric(Uitslag)) %>%
                      filter(     !is.na(Uitslag)) %>%
                      group_by(   PatientNr) %>%
                      filter(     AfnameDatumTijd==max(AfnameDatumTijd)) %>%
                      group_by(   PatientNr, AfnameDatumTijd) %>%
                      summarise(  Uitslag= mean(Uitslag))

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
                  group_by(   PatientNr, AfnameDatumTijd) %>%
                  summarise(  Uitslag=mean(Uitslag))

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
dialyse     <-          subset(     Subtraject,(DiagnoseCode %in% DBC_dialyse) & SpecialismeCode=='0313' & (SluitingsDatum<=stopwindow & OpeningsDatum>=startwindow)) %>%
                        group_by(   PatientNr) %>%
                        summarise(  dialyse = first(DiagnoseCode))

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
  select(PatientNr, Groep, Ind, Waarde)
  table(U51$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- U6 Kwaliteit van leven -----------------------------------------------------
###-------------------------------------------------------------------------------------------------------
  
# Indicator U6.1 - % uitgestuurde PROMS vragenlijsten in het afgelopen jaar
  
  
# Indicator U6.2 - % ingevulde PROMS vragenlijsten in het afgelopen jaar  
  
  
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
                      #filter(     AGB_CodeUitvoerder == "0313") %>%
                      filter(     Aantal != 0) %>%
                      select(     PatientNr,OpnameNr,ZACode,Verrichtingdatum,AGB_CodeUitvoerder,Aantal,DiagnoseCode)

# Indicator zelf invullen in scorekaart
K21b    <- inner_join(Opname_K2, Verrichting_K2, by=c("PatientNr", "OpnameNr")) %>%
                      group_by(   OpnameNr) %>%
                      mutate(  Waarde = sum(Aantal)) %>%
                      mutate(     Groep  = scorekaart_name,
                                  Ind    = "K2.1b") %>%
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
                      distinct(PatientNr, Groep, Ind, Waarde)
                      summary(K22a$Waarde)
                      sum(K22a$Waarde)

###-------------------------------------------------------------------------------------------------------
#write.xlsx(K22, "./nazoeken_IC.xlsx")
write.xlsx(K22a, "//olvg.nl/dfs/Groups02/5125/Concernstaf/1. Kwaliteit & veiligheid/4 Transparantie/VBHC/VBHC_data/Diabetes/Cyclus 5/Data/Nazoeken_IC.xlsx")
# D.M.V. Dossieronderzoek nazoeken of de reden van opname op IC diabetes gerelateerd is
###-------------------------------------------------------------------------------------------------------

# Indicator K2.3 - SEH opname
SEH         <- VerrichtingDiagn %>%
                      filter(     ZACode=="190015" & (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow)) %>%
                      filter(     DiagnoseCode %in% DBC_diabetes) %>%
                     #filter(     AGB_CodeUitvoerder == '0313') %>%
                      group_by(   PatientNr) %>%
                      mutate(  SEH = sum(Aantal)) %>%
                      distinct(PatientNr, SEH)

K23a         <- left_join(Patient, SEH, by="PatientNr") %>%
                      mutate(     Groep  = scorekaart_name,
                                  Ind    = "K2.3a",
                                  Waarde = ifelse(!is.na(SEH),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
  table(K23a$Waarde)

write.xlsx(SEH, "//olvg.nl/dfs/Groups02/5125/Concernstaf/1. Kwaliteit & veiligheid/4 Transparantie/VBHC/VBHC_data/Diabetes/Cyclus 5/Data/SEH_0313.xlsx")

###-------------------------------------------------------------------------------------------------------
###----------------------------------- K3 Polibezoeken -----------------------------------------------------
###-------------------------------------------------------------------------------------------------------

## Polibezoeken, tel consulten en SEH geregistreerd in diabetes DBC
consulten_internist <- VerrichtingDiagn %>%
                        filter(     AGB_CodeUitvoerder=="0313" &
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
                        select(PatientNr, Groep, Ind, Waarde)
    summary(K31$Waarde)

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

# Indicator K3.2 - Diabetes Verpleegkundige
# LET OP: o.b.v. afspraken! Filter afdeling niet nodig indien er filter diabetes verpleegkundige is.
DM_vpk      <- Afspraak %>%
                  filter(     AfspraakStatus=="Afgerond" & (Afdeling=="JT POLI INTERNE GENEESKUNDE" | Afdeling=="OP POLI INTERNE GENEESKUNDE" | Afdeling=="JT POLI DIABOSS") &
                              (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow) & 
                              (AfspraakCodeOms=="Spreekuurbezoek" | AfspraakCodeOms=="Telefoon" | AfspraakCodeOms=="Video consult") &
                              (SpecialistTypeOms=="Verpleegkundige" | SpecialistTypeOms=="Verpleegkundige mob")) %>%
                  group_by(   PatientNr) %>%
                  summarise(  aantal = n())
    
K32         <- left_join(Patient, DM_vpk, by="PatientNr") %>%
                  mutate(     Groep  = scorekaart_name,
                              Ind    = "K3.2",
                              Waarde = ifelse(!is.na(aantal),aantal,0)) %>%
                  select(PatientNr, Groep, Ind, Waarde)
                  summary(K32$Waarde)
    
# Subindicator K3.2.1 - Polibezoeken bij diabetes verpleegkundige
DM_vpk_poli      <- Afspraak %>%
                      filter(     AfspraakStatus=="Afgerond" & (Afdeling=="JT POLI INTERNE GENEESKUNDE" | Afdeling=="OP POLI INTERNE GENEESKUNDE" | Afdeling=="JT POLI DIABOSS") &
                                  (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow) & 
                                  (AfspraakCodeOms=="Spreekuurbezoek") &
                                  (SpecialistTypeOms=="Verpleegkundige" | SpecialistTypeOms=="Verpleegkundige mob")) %>%
                      group_by(   PatientNr) %>%
                      summarise(  aantal = n())
    
K321               <- left_join(Patient, DM_vpk_poli, by="PatientNr") %>%
                      mutate(     Groep  = scorekaart_name,
                                  Ind    = "K3.2.1",
                                  Waarde = ifelse(!is.na(aantal),aantal,0)) %>%
                      select(PatientNr, Groep, Ind, Waarde)
                      summary(K321$Waarde)
    
# Subindicator K3.2.2 - Tel consulten bij diabetes verpleegkundige
DM_vpk_tel        <- Afspraak %>%
                      filter(     AfspraakStatus=="Afgerond" & (Afdeling=="JT POLI INTERNE GENEESKUNDE" | Afdeling=="OP POLI INTERNE GENEESKUNDE" | Afdeling=="JT POLI DIABOSS") &
                                  (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow) &
                                  (AfspraakCodeOms=="Telefoon" | AfspraakCodeOms=="Video consult") &
                                  (SpecialistTypeOms=="Verpleegkundige" | SpecialistTypeOms=="Verpleegkundige mob")) %>%
                      group_by(   PatientNr) %>%
                      summarise(  aantal = n())
    
K322               <- left_join(Patient, DM_vpk_tel, by="PatientNr") %>%
                      mutate(     Groep  = scorekaart_name,
                                  Ind    = "K3.2.2",
                                  Waarde = ifelse(!is.na(aantal),aantal,0)) %>%
                      select(PatientNr, Groep, Ind, Waarde)
                      summary(K322$Waarde)
    
# Indicator K3.3 - Dietist
dietist     <- Verrichting %>%
                  filter((    Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow) &
                              (ZACode %in% ZA_Dietist)) %>%
                  filter(Aantal != 0) %>%
                  group_by(   PatientNr) %>%
                  mutate(Waarde = sum(Aantal)) %>%
                  distinct(PatientNr, Waarde)

K33         <- left_join(Patient, dietist, by="PatientNr") %>%
                  mutate(     Groep  = scorekaart_name,
                              Ind    = "K3.3",
                              Waarde = ifelse(!is.na(Waarde),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
  table(K33$Waarde)

                        ## evt. o.b.v. afspraken
                        #dietist <- left_join(Patient, Afspraak, by="PatientNr") %>%
                        #  filter(AfspraakStatus=="Afgerond" &
                        #           (Afdeling=="DIETETIEK") &
                        #           (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow) &
                        #           (SpecialistTypeOms=="Dietist")) %>%
                        #  group_by(PatientNr) %>%
                        #  summarise(aantal = n())
                        #summary(dietist$aantal)
                        #sum(dietist$aantal)

# Indicator K3.4 - Psycholoog/psychiater
#psych       <- Verrichting %>%
#                        filter((    Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow) &
#                                      (ZACode %in% ZA_psych)) %>%
#                        filter(Aantal != 0) %>%
#                        group_by(   PatientNr) %>%
#                        mutate(Waarde = sum(Aantal)) %>%
#                        distinct(PatientNr, Waarde)

#K34         <- left_join(Patient, psych, by="PatientNr") %>%
#                        mutate(     Groep  = scorekaart_name,
#                                    Ind    = "K3.4",
#                                    Waarde = ifelse(!is.na(Waarde),1,0)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#  table(K34$Waarde)

                        ## evt. o.b.v. afspraken
                        psych <- Afspraak %>%
                          filter(AfspraakStatus=="Afgerond" & 
                                   (Afdeling=="JT POLI PSYCHIATRIE & MEDISCHE PSYCHOLOGIE" | Afdeling=="OP POLI PSYCHIATRIE & MEDISCHE PSYCHOLOGIE") &
                                   (Verwijzer=="0313") &
                                   (ConsultType=="Intake psychiatrie" | ConsultType=="Intake psychologie" | ConsultType=="Intake seksuologie") &
                                   (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow)) %>%
                          group_by(PatientNr) %>%
                          summarise(aantal = n())
                        summary(psych$aantal)
                        sum(psych$aantal)
                        
                        K34         <- left_join(Patient, psych, by="PatientNr") %>%
                                                  mutate(     Groep  = scorekaart_name, 
                                                              Ind    = "K3.4",
                                                              Waarde = ifelse(!is.na(aantal),1,0)) %>%
                                                  select(PatientNr, Groep, Ind, Waarde) 
                                                  table(K34$Waarde)

# Indicator K3.5a - Oogarts/optometrist
polibezoeken_oogarts <- VerrichtingDiagn %>%
                        filter(     AGB_CodeUitvoerder=="0301" &
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
                        filter(     AGB_CodeUitvoerder=="0301" &
                                      (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow) &
                                      (ZACode %in% ZA_optometrie))

oogarts_optometrist <- bind_rows(polibezoeken_oogarts, optometrie)%>%
                        group_by(   PatientNr) %>%
                        summarise( Waarde = sum(Aantal))

K35a         <- left_join(Patient, oogarts_optometrist, by="PatientNr") %>%
                        mutate(     Groep  = scorekaart_name,
                                    Ind    = "K3.5a",
                                    Waarde = ifelse(!is.na(Waarde),1,0)) %>%
  select(PatientNr, Groep, Ind, Waarde)
  table(K35a$Waarde)

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
#                        filter(     AfspraakStatus=="Afgerond" &
#                                      SpecialistTypeOms=="Podotherapeut" &
#                                      (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow)) %>%
#                        group_by(   PatientNr) %>%
#                        summarise(  aantal = n())

#K36         <- left_join(Patient, podotherapeut, by="PatientNr") %>%
#                        mutate(     Groep  = scorekaart_name,
#                                    Ind    = "K3.6",
#                                    Waarde = ifelse(!is.na(aantal),1,0)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#  table(K36$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- K4 Labaanvragen -----------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Selecteer alle verrichtingen met zorgprofielklasse 8 - Kl. chemie en haematologie
lab         <- Verrichting %>%
                        select(     PatientNr, ZACode, AGB_CodeAanvrager, Verrichtingdatum, ZorgprofielKlasse) %>%
                        filter(     ZorgprofielKlasse=="0008" & AGB_CodeAanvrager=="0313" & (Verrichtingdatum>=startwindow & Verrichtingdatum<=stopwindow)) %>%
                        group_by(   PatientNr, Verrichtingdatum) %>%
                        summarise(  aantal_lab = n())

# Indicator K4.1 - Aantal poliklinische labbepalingen
lab_klin    <- select(Opname, PatientNr, OpnameDatumTijd, OntslagDatumTijd)
K41a         <- left_join(lab,lab_klin, by="PatientNr") %>%
                        mutate(     lab_klin2=ifelse(Verrichtingdatum>=OpnameDatumTijd & Verrichtingdatum<=OntslagDatumTijd,1,0)) %>%
                        group_by(   PatientNr, Verrichtingdatum) %>%
                        summarise(  lab_klin=max(lab_klin2), aantal_lab=max(aantal_lab)) %>%
                        mutate(     lab_klin=replace_na(lab_klin,0)) %>%
                        filter(     lab_klin!=1) %>%
                        group_by(   PatientNr) %>%
                        summarise(  Waarde = sum(aantal_lab))

K41a         <- K41a %>%
                      mutate(     Groep  = scorekaart_name,
                                  Ind    = "K4.1a",
                                  Waarde = ifelse(is.na(Waarde), 0, Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
  summary(K41a$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- P1 Doorlooptijden ---------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Indicator P1.2 - Geannuleerde afspraken door patient bij de internist (Patient kan meerdere keren voorkomen)
#P12         <- Afspraak %>%
#                        filter((    Afdeling=="DIABETOLOGIE" | Afdeling=="INT") &
#                                      (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow) &
#                                      (AfspraakCodeOms=="Spreekuurbezoek" | AfspraakCodeOms=="Tel consult" | AfspraakCodeOms=="Videoconsult") &
#                                      (AfspraakStatus=="Geannuleerd" | AfspraakStatus=="Afgerond" | AfspraakStatus=="No-show") &
#                                      (SpecialistTypeOms=="Medisch Specialist" | SpecialistTypeOms=="Verpleegkundig Specialist")) %>%
#                        mutate(     Groep  = scorekaart_name,
#                                    Ind    = "P1.2",
#                                    Waarde = ifelse((AfspraakStatus=="Geannuleerd" & AnnuleringsReden=="Patient"),1,0)) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#  table(P12$Waarde)

# Indicator P1.2.1 - Aantal dagen van annulering tot nieuwe afspraak
#P121        <- Afspraak %>%
#                        filter((    Afdeling=="DIABETOLOGIE" | Afdeling=="INT") &
#                                    (AfspraakCodeOms=="Spreekuurbezoek" | AfspraakCodeOms=="Tel consult" | AfspraakCodeOms=="Videoconsult") &
#                                    (((AfspraakStatus=="Geanuleerd" & AnnuleringsReden=="Patient") & (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow)) |
#                                    ((AfspraakStatus=="Geannuleerd" | AfspraakStatus=="Afgerond" | AfspraakStatus=="No-show") & (AfspraakDatum>=startwindow & AfspraakDatum<=Eind_FU))) &
#                                    (SpecialistTypeOms=="Medisch Specialist" | SpecialistTypeOms=="Verpleegkundig Specialist")) %>%
#                        mutate(     annuleren=ifelse(((AfspraakStatus=="Geannuleerd" & AnnuleringsReden=="Patient") & (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow)),1,0)) %>%
#                        group_by(   PatientNr) %>%
#                        arrange(    PatientNr, AfspraakDatum) %>%
#                        mutate(     Groep  = scorekaart_name,
#                                    Ind    = "P1.2.1",
#                                    Waarde = as.numeric(c(difftime(tail(AfspraakDatum, -1), head(AfspraakDatum, -1),units = "days"),0))) %>%
#                        filter(     annuleren==1 & Waarde>0) %>%
#  select(PatientNr, Groep, Ind, Waarde)
#  summary(P121$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- P2 No shows ---------------------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Indicator P2.1 - % No shows bij internist
P21a         <- Afspraak %>%
                  filter((Afdeling=="JT POLI INTERNE GENEESKUNDE" | Afdeling=="OP POLI INTERNE GENEESKUNDE" | Afdeling=="JT POLI DIABOSS") &
                          (AfspraakCodeOms=="Afspraak" | AfspraakCodeOms=="Spreekuurbezoek") &
                          (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow) & 
                          (SpecialistTypeOms=="Arts" | SpecialistTypeOms=="Arts in opleiding" | SpecialistTypeOms=="Verpleegkundig specialist") &
                          (AfspraakStatus=="No-show" | AfspraakStatus=="Afgerond")) %>%
                  mutate(     Groep  = scorekaart_name,
                              Ind    = "P2.1a",
                              Waarde=ifelse((AfspraakStatus=="No-show"),1,0)) %>%
                  select(PatientNr, Groep, Ind, Waarde)
                  table(P21a$Waarde)

# Indicator P2.1 - % No shows bij diabetes verpleegkundige
P22         <- Afspraak %>%
                filter((Afdeling=="JT POLI INTERNE GENEESKUNDE" | Afdeling=="OP POLI INTERNE GENEESKUNDE" | Afdeling=="JT POLI DIABOSS") &
                        (AfspraakCodeOms=="Afspraak" | AfspraakCodeOms=="Spreekuurbezoek") &
                        (AfspraakDatum>=startwindow & AfspraakDatum<=stopwindow) &
                        (SpecialistTypeOms=="Verpleegkundige" | SpecialistTypeOms=="Verpleegkundige mob") &
                        (AfspraakStatus=="No-show" | AfspraakStatus=="Afgerond")) %>%
                mutate(     Groep  = scorekaart_name,
                            Ind    = "P2.2",
                            Waarde=ifelse((AfspraakStatus=="No-show"),1,0)) %>%
                select(PatientNr, Groep, Ind, Waarde)
                table(P22$Waarde)

###-------------------------------------------------------------------------------------------------------
###----------------------------------- P3 Lost to follow up ------------------------------------------------
###-------------------------------------------------------------------------------------------------------
  
PatientSelectie_C3 <- read.xlsx("//olvg.nl/dfs/Groups02/5125/Concernstaf/1. Kwaliteit & veiligheid/4 Transparantie/VBHC/VBHC_data/Diabetes/Cyclus 3/Data/Bron_bestand_diabetes_C3.xlsx", sheet = "PatientSelectie")
  
# Lost-to-follow-up is o.b.v. DM1 patienten uit voorgaande cyclus die vervolgens twee jaar geen diabetes DBC hebben 
PatientSelectie_C3 <- PatientSelectie_C3 %>%
                                  filter(Cyclus == "C3") %>%
                                  filter(Groep == 1 &
                                            (TypeDM == "Type 1" | TypeDM == "LADA"))
  
# Inlezen DBC bestand (LET OP: zie logboek voor welke periode bron bestand vullen)
DBC_LTFU <- Subtraject_bron %>%
                mutate( LeegSubtraject = ifelse(LeegSubtraject == "J", 1, 0)) %>%
                filter( ZorgType       == "11" | ZorgType      == "21" |
                        ZorgType       == "R"  | ZorgType      == "L" ) %>%
                filter(LeegSubtraject == 0 & OpeningsDatumDBC >= LTFU) %>%
                filter(DiagnoseCode %in% c("221", "222", "223")) %>%
                distinct(PatientNr)
  
LTFU <- PatientSelectie_C3 %>%
                filter(!(PatientNr %in% DBC_LTFU$PatientNr))

# D.m.v. dossieronderzoek achterhalen welke patienten lost-to-follow-up zijn
#write.xlsx(LTFU, "LTFU.xlsx")
write.xlsx(LTFU, "//olvg.nl/dfs/Groups02/5125/Concernstaf/1. Kwaliteit & veiligheid/4 Transparantie/VBHC/VBHC_data/Diabetes/Cyclus 5/Data/LTFU_C3.xlsx")

###-------------------------------------------------------------------------------------------------------
###----------------------------------- P4 Moment van labafname ---------------------------------------------
###-------------------------------------------------------------------------------------------------------

# Indicator P4.1 - % patienten met jaar controle lab in afgelopen jaar
jaarlab     <- Lab %>%
                        filter((    BepalingOms=="ldl cholesterol" | BepalingOms=="hba1c" | BepalingOms=="acr in urine" | BepalingOms=="micro albumine in urine") &
                                      (AfnameDatumTijd>=startwindow & AfnameDatumTijd <= stopwindow)) %>%
                        select(     PatientNr, BepalingOms, AfnameDatumTijd) %>%
                        mutate(     LDL=as.numeric(ifelse(BepalingOms=="ldl cholesterol",1,0)),
                                    hba1c=as.numeric(ifelse(BepalingOms=="hba1c",1,0)),
                                    ACR=as.numeric(ifelse(BepalingOms=="acr in urine",1,0)),
                                    albumine=as.numeric(ifelse(BepalingOms=="micro albumine in urine",1,0))) %>%
                        group_by(   PatientNr) %>%
                        summarise(  LDL=max(LDL), hba1c=max(hba1c), ACR=max(ACR), albumine=max(albumine))

P41         <- left_join(Patient, jaarlab, by="PatientNr") %>%
                        mutate(     Groep  = scorekaart_name,
                                    Ind    = "P4.1",
                                    Waarde = ifelse(LDL==1 & hba1c==1 & (ACR==1 | albumine==1),1,0),
                                    Waarde = ifelse(is.na(Waarde),0,Waarde)) %>%
  select(PatientNr, Groep, Ind, Waarde)
  table(P41$Waarde)

# Indicator P4.2 - % consulten met lab prikken binnen een week na consult
jaarlab     <- Lab %>%
                        filter((    BepalingOms=="ldl cholesterol" | BepalingOms=="hba1c" | BepalingOms=="acr in urine" | BepalingOms=="micro albumine in urine") &
                                      (AfnameDatumTijd>=startwindow & AfnameDatumTijd <= stopwindow)) %>%
                        select(     PatientNr, BepalingOms, AfnameDatumTijd)

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
case_mix   <- as.data.frame( rbind(lft, gsl, bmi, etn, diagnose, rok, retino, ther, pomp, rtcgm, fgm, combi, HbA1c)) %>%
  mutate(Wegingsfactor=1,
         Aandoening    =  "Diabetes", 
         Cyclus        =  Cyclus_nr)

# Invullen met diabetes indicatoren
Indicatoren <- as.data.frame( rbind(U12, U211a, U212a, U221a, U33, U41a, U47, U49a, U4101a, U4102a, U412, U51, 
                                    K22a, K23a, K31, K311, K312, K32, K321, K322, K33, K34, K35a, K351, K41a,
                                    P21a, P22, P41)) %>%
  mutate(Wegingsfactor=1,
         Aandoening    =  "Diabetes", 
         Cyclus        =  Cyclus_nr)

OutputDataSet <- rbind(case_mix, Indicatoren)

df_wide <- OutputDataSet %>% pivot_wider(names_from = Ind, values_from = Waarde)

# Save indicatoren to R-file
save(OutputDataSet, file = paste0("OutputDataSet_", as.character(Cyclus_nr), ".rda") )
write.xlsx(OutputDataSet, paste0("OutputDataSet_", as.character(Cyclus_nr), ".xlsx"), col_names = TRUE)

#LET OP: handmatige indicatoren     U3.1 Ketoacidose
#                                   U3.2 Hypo
#                                   HbA1c tabel
#                                   K2.1b Verblijfsduur per opname
#                                   K2.2 Verblijfsduur IC gerelateerd aan diabetes
#                                   P3 Lost-to-follow-up
#                                   P4.2 Moment van lab prikken