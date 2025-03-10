# VBHC Aandoening: Diabetes
# Cyclus 5
# Script 3
#
#
# Datum: 13-12-2021
#
# D. ten Hoorn Boer, St. Antonius Ziekenhuis
# e-mail: d.ten.hoornboer@antoniusziekenhuis.nl
# tel   : 0621447603
#
# R and library versions
# R version:  3.5.0 (2018-04-23)
# dplyr:      0.8.4
# lubridate:  1.7.8
# readxl:     1.3.1

# Haal alle diabetes DBC's op (zowel leeg als gevuld). Excludeer zelf ongeldige DBC's.
# - OpeningsDatum: startWindow t/m stopWindow
# - DiagnoseCode: 7113/7114 (binnen SpecialismeCode 0316) en 221/222/223 (binnen SpecialismeCode 0313)
# Zie DataDictionary voor definities omtrent status DBC

# Inlezen DBC bestand
Subtraject                   <- read_excel(fname, sheet = "Patientselectie Subtraject")

Subtraject <- Subtraject    %>% mutate (PatientNr                = as.character(PatientNr),
                                        ZorgtrajectNr            = as.numeric(ZorgtrajectNr),
                                        SubtrajectNr             = as.numeric(SubtrajectNr),
                                        SpecialismeCode          = as.character(AGB_Code),
                                        DiagnoseCode             = as.character(DiagnoseCode),
                                        OpeningsDatum            = as.Date(OpeningsDatumDBC, format = "%Y-%m-%d"),
                                        SluitingsDatum           = as.Date(SluitingsDatumDBC, format = "%Y-%m-%d"),
                                        StatusDBC                = as.character(StatusDBC),
                                        LeegSubtraject           = as.character(LeegSubtraject),
                                        ZorgType                 = as.character(ZorgType)) %>%
                                #filter(StatusDBC != "N") %>%
                                select( PatientNr,ZorgtrajectNr,SubtrajectNr,SpecialismeCode,DiagnoseCode,OpeningsDatum,
                                        SluitingsDatum,ZorgType, StatusDBC, LeegSubtraject)

# Stap 1: DBCs anders dan aangeleverd omzetten naar X
##DBCs anders dan ZT11 en ZT21 niet meenemen
#als het niet A is wordt het X (regel 59).
#neem van alle ptn die minimaal 1 A hebben (en wellicht bv ook een x) alle subtrajecten mee.
Subtraject_stap1 <- Subtraject %>%   mutate( LeegSubtraject = ifelse(LeegSubtraject == "J", 1, 0)) %>%
                                     filter( ZorgType       == "11" | ZorgType      == "21" |
                                             ZorgType       == "R"  | ZorgType      == "L" ) %>%
                                     group_by(PatientNr) %>%
                                     filter( any(LeegSubtraject == 0)) %>%
                                     ungroup() %>%
                                     filter(!(ZorgType == "11" & LeegSubtraject == 1) )

##Ontbrekende SluitingsDatum invullen
Subtraject_stap1 <- Subtraject_stap1 %>% mutate(SluitingsDatum = ifelse(is.na(SluitingsDatum) & (ZorgType == "11" | ZorgType == "R") ,OpeningsDatum+89,  SluitingsDatum),
                                                SluitingsDatum = ifelse(is.na(SluitingsDatum) & (ZorgType == "21" | ZorgType == "L") ,OpeningsDatum+119, SluitingsDatum),
                                                SluitingsDatum = as.Date(SluitingsDatum, origin="1970-01-01")) %>%
                                         arrange(PatientNr, OpeningsDatum) %>%
                                         distinct()                             # iedere regel moet uniek zijn

##### Identificeren subgroepen (nieuw / overname / kind) #####

      # Identificeren overname patienten kinderarts
      Overname_kind     <- Subtraject_stap1 %>%
                           filter(LeegSubtraject == 0) %>%
                           group_by(PatientNr) %>%
                           mutate(Overname_kind = max(ifelse(  row_number()!=1 &
                                                               ZorgType == "11" &
                                                               DiagnoseCode %in% c("221", "222", "223") &
                                                               SpecialismeCode == "0313" &
                                                              (OpeningsDatum >= startwindow & OpeningsDatum <= stopwindow) &
                                                              (lag(DiagnoseCode == "7113" | DiagnoseCode == "7114") & lag(SpecialismeCode == "0316")), 1,0))) %>%
                           filter(Overname_kind == 1) %>%
                           mutate(Groep = 3) %>%
                           distinct(PatientNr, Groep) %>%
                           ungroup()

      # Identificeren nieuwe (+overname) patienten
      Nieuw             <- Subtraject_stap1 %>%
                           group_by(PatientNr) %>%
                           filter(OpeningsDatum == min(OpeningsDatum)) %>%
                           filter(DiagnoseCode %in% c("221", "222", "223") &
                                    (OpeningsDatum >= startwindow & OpeningsDatum <= stopwindow)) %>%
                           ungroup() %>%
                           mutate(Nieuw = 1) %>%
                           distinct(PatientNr, Nieuw)
      #Check_nieuw <- sample_n(Subtraject_stap3b, 10, replace = FALSE)
      #write.xlsx(Check_nieuw, "Check_nieuw.xlsx")

      # Identificeren lost to follow up
      #LTFU              <- Subtraject_stap1 %>%
      #                     filter(LeegSubtraject == 0 & SluitingsDatum > LTFU) %>%
      #                     group_by(PatientNr) %>%
      #                     filter(DiagnoseCode %in% c("221", "222", "223") &
      #                              SluitingsDatum == max(SluitingsDatum)) %>%
      #                     filter(SluitingsDatum < startwindow) %>%
      #                     mutate(Groep = 4) %>%
      #                     distinct(PatientNr, Groep) %>%
      #                     ungroup()
      #Check_LTFU <- sample_n(Subtraject_stap3c, 10, replace = FALSE)
      #write.xlsx(Check_LTFU, "Check_LTFU.xlsx")

      # Check of patienten voorkomen in meerdere groepen
      #Check <- full_join(LTFU %>% select(PatientNr, LTFU), Nieuw %>% select(PatientNr, Nieuw), by="PatientNr")
      #Check <- full_join(Check, Overname_kind %>% select(PatientNr, Overname_kind), by="PatientNr") %>%
      #  filter((LTFU == 1          & (Nieuw == 1 | Overname_kind ==1))  |
      #           (Nieuw == 1         & (LTFU ==1   | Overname_kind == 1)) |
      #           (Overname_kind == 1 & (LTFU ==1   | Nieuw ==1))          )


# Stap 2: Selecteren juiste DBCs
Subtraject_stap2 <- Subtraject_stap1 %>% filter( (SpecialismeCode == "0313" & DiagnoseCode %in% c("221", "222", "223"))) %>%
                                     arrange(PatientNr, StatusDBC, OpeningsDatum)

# Tussenstap voor diabetes: ZorgType 11 door wijziging DBC beschouwen als vervolg DBC
Subtraject_stap2a <- Subtraject_stap2 %>%
                                     group_by(PatientNr) %>%
                                     mutate(  Dagen = as.numeric(OpeningsDatum - lag(SluitingsDatum))) %>%
                                     mutate(  ZorgType = ifelse( (ZorgType == "11" | ZorgType == "R") & DiagnoseCode != lag(DiagnoseCode) & Dagen <= 120 & row_number()!=1, "21", ZorgType)) %>%
                                     ungroup() %>%
                                     select(  -Dagen) %>%
                                     arrange( PatientNr, OpeningsDatum)

# Stap 3: Identificeer en verwijder reguliere DBCs die niet worden gevolgd door een gevulde vervolg DBC ###
laatste_dbc <- Subtraject_stap2a %>%  filter(  ZorgType == "11" | ZorgType == "R") %>%                                # alle nieuwe DBCs behouden
                                      arrange( PatientNr, desc(OpeningsDatum)) %>%                                     # laatste nieuwe DBC bovenaan
                                      select(  PatientNr, OpeningsDatum) %>%                                           # per patient uniek id aanmaken (benodigd voor vervolgstappen)
                                      rename(  laatste_dbc_start = OpeningsDatum)

Subtraject_stap3  <- full_join(Subtraject_stap2a, laatste_dbc, by="PatientNr") %>%
                     filter  ( OpeningsDatum > laatste_dbc_start &
                             ( ZorgType     == "21" | ZorgType == "L") ) %>%
                     group_by( PatientNr) %>%
                     filter(   any((ZorgType =="21" | ZorgType == "L") & LeegSubtraject == 0)) %>%
                     ungroup()

Subtraject_stap3b <- rbind(Subtraject_stap2a %>% filter(PatientNr %in% Subtraject_stap3$PatientNr),
                           Subtraject_stap2a %>% group_by( PatientNr) %>%
                                                filter(   all(ZorgType=="21" | ZorgType == "L") ) %>%
                                                ungroup() )


# Stap 4: Selecteer op DBCs vanaf 3 jaar voor de selectieperiode ###
Subtraject_stap4 <- Subtraject_stap3b %>% filter(SluitingsDatum >= startwindow - years(2) )


# Stap 5: Bepaal welke DBCs voorkomen en wat de eerste en laatste DBC is die voorkomt ###
# Samenvoegen opvolgende regels, per PatientNr en DBC vol/leeg
Subtraject_stap5 <- Subtraject_stap4 %>% arrange(PatientNr, OpeningsDatum, SluitingsDatum) %>%
                                         group_by(PatientNr) %>%
                                         mutate(samenvoegen = if_else( LeegSubtraject != lag(LeegSubtraject) | row_number() == 1, 1, 0)) %>%
                                         ungroup() %>%
                                         mutate(sv_id = cumsum(samenvoegen)) %>%
                                         group_by(sv_id) %>%
                                         summarise(
                                           PatientNr       = first(PatientNr),
                                           LeegSubtraject  = first(LeegSubtraject),
                                           OpeningsDatum   = min(OpeningsDatum),
                                           SluitingsDatum  = max(SluitingsDatum),
                                           ZorgType        = first(ZorgType),
                                           Diagnose        = last(DiagnoseCode)) %>%
                                         ungroup() %>%
                                         select(-sv_id)


### Geef bij overlap van lege en volle DBCs voorrang aan de volle
# Markeer de lege DBCs waarom het gaat
Subtraject_stap5 <- Subtraject_stap5 %>% arrange( PatientNr, OpeningsDatum, SluitingsDatum) %>%
                                         group_by(PatientNr) %>%
                                         mutate(  first = if_else(row_number() == 1, 1, 0)) %>%
                                         mutate(  last = if_else( max(row_number()) == row_number(), 1, 0))

# 1. Lege DBC wordt boven overlapt door volle DBC: zet beginddatum op einddatum+1 van de volle DBC.
# 2. Volle DBC wordt boven overlapt door lege DBC: Zet einddatum op begindatum-1 van de volle DBC.

Subtraject_stap5 <- Subtraject_stap5 %>% arrange( PatientNr, OpeningsDatum) %>%
                                         group_by(PatientNr) %>%
                                         mutate(  seq=row_number()) %>%
                                         mutate(  OpeningsDatum = if_else( LeegSubtraject      == 1             &
                                                                           lag(SluitingsDatum) >= OpeningsDatum &
                                                                           seq                 !=1, lag(SluitingsDatum) + 1, OpeningsDatum))

Subtraject_stap5 <- Subtraject_stap5 %>% arrange (PatientNr, OpeningsDatum)%>%
                                         group_by(PatientNr) %>%
                                         mutate(  seq = row_number()) %>%
                                         mutate(  SluitingsDatum = if_else( LeegSubtraject      == 1              &
                                                                            lead(OpeningsDatum) <= SluitingsDatum &
                                                                            seq                 != 1, lead(OpeningsDatum) - 1, SluitingsDatum))


# Gooi weg als daarmee begindatum > einddatum.
Subtraject_stap5 <- Subtraject_stap5 %>% filter(OpeningsDatum < SluitingsDatum)


# Stap 5b: verwijder patienten zonder gevulde dbcs in de selectieperiode
Subtraject_stap5b <- Subtraject_stap5 %>% group_by( PatientNr) %>%
                                          filter(   !all(LeegSubtraject == 1 ))  %>%
                                          distinct( PatientNr)


Subtraject_stap5c <- right_join(Subtraject_stap5, Subtraject_stap5b, by="PatientNr")


# Stap 6: Gooi lege periodes vanaf 360 dagen eruit ###
Subtraject_stap6 <- Subtraject_stap5c %>%
                                         filter(  LeegSubtraject == 0 |
                                                  (LeegSubtraject == 1 &
                                                   difftime(SluitingsDatum, OpeningsDatum, units = "days") + 1 < 360) ) %>%
                                         group_by(PatientNr) %>%
                                         filter(any(SluitingsDatum >= startwindow & OpeningsDatum <= stopwindow)) %>%
                                         ungroup()


# Stap 7: Samenvoegen opvolgende regels ongeacht DBC en leeg ###
Subtraject_stap7 <- Subtraject_stap6 %>% arrange(PatientNr, OpeningsDatum, SluitingsDatum) %>%
                                         group_by(PatientNr) %>%
                                         mutate(samenvoegen = if_else(row_number()!= 1 & lag(SluitingsDatum)+1 >= OpeningsDatum, 0, 1)) %>%
                                         ungroup() %>%
                                         mutate(sv_id = cumsum(samenvoegen)) %>%
                                         group_by(sv_id) %>%
                                         summarise(
                                           PatientNr        = first(PatientNr),
                                           OpeningsDatum    = min(OpeningsDatum),
                                           SluitingsDatum   = max(SluitingsDatum),
                                           ZorgType         = first(ZorgType),
                                           Diagnose         = last(Diagnose)) %>%
                                         ungroup() %>%
                                         select(-sv_id)

  # Stap 7a: Samenvoegen trajecten als er minder dat 365 dagen tussen zit
  Subtraject_stap7a <- Subtraject_stap7 %>% arrange(PatientNr, OpeningsDatum, SluitingsDatum) %>%
                                            group_by(PatientNr) %>%
                                            mutate(samenvoegen = if_else(row_number()!= 1 & lag(SluitingsDatum)+365 >= OpeningsDatum, 0, 1)) %>%
                                            ungroup() %>%
                                            mutate(sv_id = cumsum(samenvoegen)) %>%
                                            group_by(sv_id) %>%
                                            summarise(
                                              PatientNr        = first(PatientNr),
                                              OpeningsDatum    = min(OpeningsDatum),
                                              SluitingsDatum   = max(SluitingsDatum),
                                              ZorgType         = first(ZorgType),
                                              Diagnose         = last(Diagnose)) %>%
                                            ungroup() %>%
                                            select(-sv_id)

# Stap 8: Selecteer op trajecten binnen de tijdswindow ###
Subtraject_stap8 <- Subtraject_stap7a %>% filter( OpeningsDatum  <= stopwindow &
                                                 SluitingsDatum >= startwindow)

### Voeg meerdere trajecten samen ###
Subtraject_stap8 <- Subtraject_stap8 %>% arrange(PatientNr,OpeningsDatum,SluitingsDatum) %>%
                                         group_by(PatientNr) %>%
                                         summarise(
                                            DM_start  = min(OpeningsDatum),
                                            DM_stop   = max(SluitingsDatum),
                                            Diagnose   = last(Diagnose)) %>%
                                          mutate(DM_start = as.Date(DM_start,origin="1970-01-01")) %>%
                                          mutate(DM_stop = as.Date(DM_stop,origin="1970-01-01")) %>%
                                          ungroup()

### Zet diabetes_start en diabetes_stop op start- en stopwindow bij overschrijding ###
Subtraject_stap8 <- data.frame(Subtraject_stap8) %>%  mutate(selectie_start = ifelse(DM_start < startwindow, startwindow, DM_start),
                                                             selectie_stop  = ifelse(DM_stop  > stopwindow,  stopwindow,  DM_stop),
                                                             selectie_start = as.Date(selectie_start, origin = "1970-01-01"),
                                                             selectie_stop  = as.Date(selectie_stop,  origin = "1970-01-01"))

# Stap 9: Bereken wegingsfactor
Subtraject_stap9  <- Subtraject_stap8 %>%
                    mutate(Wegingsfactor =  as.numeric( difftime(selectie_stop, selectie_start, units = "days") + 1) /
                                            as.numeric( difftime(stopwindow,    startwindow,    units = "days") + 1 ) ) %>%
                    mutate(Groep = ifelse(selectie_start > startwindow, 2, 1)) %>%
                    filter(!PatientNr %in% Overname_kind$PatientNr)

# Van de volgende groep moet het type diabetes bekend zijn
PatientSelectie_final <- bind_rows(Subtraject_stap9 %>% select(PatientNr, Groep), Overname_kind %>%
                    select(PatientNr, Groep)) %>%
                    mutate(Cyclus = Cyclus_nr)

#PatientSelectie_C3 <- bind_rows(PatientSelectie_C3, LTFU %>% select(PatientNr, Groep)) %>%
#    distinct(PatientNr, Groep)

write.xlsx(PatientSelectie_final, paste0("PatientSelectie_", as.character(Cyclus_nr), ".xlsx"), col_names = TRUE)

# Groep = 1 : Standaard chronische patient
# Groep = 2 : Nieuw / Overname / Herintreder
# Groep = 3 : Overname kinderarts
# Groep = 4 : Lost to follow-up