# VBHC Aandoening: Geboortezorg
# Groep: MoederKind
# Benchmark: Cyclus 4
#
# Stefan. G.H. Heinen, Catharina Ziekenhuis
#
# e-mail: stefan.heinen@catharinaziekenhuis.nl
# tel   : 040 237 6588
#
# R and library versions
# R version:  3.5.0 (2018-04-23)
# dplyr:      0.8.3
# lubridate:  1.7.4
# readxl:     1.3.1


source("./Geboortezorg_voorbereiding.R")


# ------------ Tijdelijke mutaties ---------------------------
mom             <- mom         %>% mutate( PatientNrMoeder       = as.character(PatientNr_Moeder),
                                           GeboorteDatumMoeder   = as.Date(GeboorteDatumMoeder, format = "%Y-%m-%d"),
                                           OverlijdensDatumMoeder= as.Date(OverlijdensDatumMoeder, format = "%Y-%m-%d"),
                                           Zwd                   = as.character(Zwd),
                                           Graviditeit           = as.numeric(Graviditeit),
                                           Pariteit              = as.numeric(Pariteit),
                                           Bloedverlies          = as.numeric(Bloedverlies),
                                           Partusstart           = as.character(Partusstart),
                                           BeginBaring           = as.character(BeginBaring),
                                           Bijstimulatie         = as.character(Bijstimulatie),
                                           GeplandeSectio        = as.character(GeplandeSectio),
                                           Overdracht            = as.numeric(Overdracht),
                                           OverdrachtOms         = as.character(OverdrachtOms),
                                           Perineum              = as.character(Perineum),
                                           BorstvIntentie        = as.character(BorstvIntentie),
                                           NoorseHandgreep       = as.character(NoorseHandgreep),
                                           Amniotomie            = as.character(Amniotomie),
                                           PartusNummer          = as.character(PartusnummerText)) %>%
                                   select( PatientNrMoeder, GeboorteDatumMoeder, OverlijdensDatumMoeder, Zwd, Graviditeit, Pariteit, Bloedverlies,
                                           Partusstart, BeginBaring, Bijstimulatie, GeplandeSectio, Overdracht, OverdrachtOms, Perineum,
                                           NoorseHandgreep, BorstvIntentie) #, BMI, Episiotomie zit niet in de dataset

kids             <- kid        %>% mutate( PatientNrMoeder       = as.character(PatientNr_Moeder),
                                           PatientNrKind         = as.character(PatientNr_Kind),
                                           GeboorteDatumKind     = as.Date(DatumTijdGeboorte, format = "%Y-%m-%d"),
                                           GeboorteDatumTijdKind = as.POSIXct(DatumTijdGeboorte, format = "%Y-%m-%d %H:%M:%S", tz = ""),
                                           VliesDatumTijd        = as.POSIXct(DatumTijdVlies, format = "%Y-%m-%d %H:%M:%S"),
                                           PersenDatumTijd       = as.POSIXct(DatumTijdPersen, format = "%Y-%m-%d %H:%M:%S"),
                                           Duuruitdrijving       = as.character(Duuruitdrijving),
                                           GebgewichtPerc        = as.numeric(GebgewichtPerc),
                                           LigKind               = as.character(Geboorteligging),
                                           OverlijdensDatumKind  = as.Date(OverlijdensDatumKind, format = "%Y-%m-%d"),
                                           BorstvOntslag         = as.character(BorstvOntslag),
                                           Pijnbestrijding       = as.character(Pijnbestrijding),
                                           Partustype            = as.character(Partustype),
                                           Sectio                = as.character(Sectio),
                                           Apgar5min             = as.numeric(Apgar5min),
                                           ArtNavelpH            = as.numeric(ArtNavelpH),
                                           Episiotomie           = as.character(Episiotomie),
                                           IndicatieSectio       = as.character(IndicatieSectio),
                                           Kinderarts            = as.character(Kinderarts),
                                           Overleden             = as.character(Overleden),
                                           Overledenfase         = as.character(Overledenfase),
                                           Geboortegewicht       = as.numeric(Geboortegewicht),
                                           AanpakkerKind         = as.character(AanpakkerKind),
                                           KindNr                = as.numeric(KindNr)) %>%
                                    select(PatientNrMoeder, PatientNrKind, GeboorteDatumKind, GeboorteDatumTijdKind, VliesDatumTijd, PersenDatumTijd, Duuruitdrijving, LigKind,
                                           OverlijdensDatumKind, GebgewichtPerc, BorstvOntslag, Pijnbestrijding, Partustype, Sectio, Apgar5min, ArtNavelpH, Episiotomie, IndicatieSectio,
                                           Kinderarts, Overleden, Overledenfase, Geboortegewicht, AanpakkerKind, KindNr)


Verrichting     <- Verrichting %>% mutate( PatientNrMoeder       = as.character(PatientNr),
                                           PatientNrKind         = as.character(Kind),
                                           ZACode                = as.character(ZACode),
                                           Verrichtingdatum      = as.Date(Verrichtingdatum, format = "%Y-%m-%d"),
                                           ZorgType              = as.character(ZorgType),
                                           DiagnoseCode          = as.character(DiagnoseCode),
                                           AGB_Code              = as.character(AGB_CodeUitvoerder),
                                           Startdatum            = as.Date(StartDatum, format = "%Y-%m-%d"),
                                           Aantal                = as.numeric(Aantal) ) %>%
                                   select( PatientNrMoeder, PatientNrKind, ZACode, Verrichtingdatum, ZorgType, DiagnoseCode, AGB_Code, Startdatum, Aantal)

'Operatie        <- Operatie    %>% mutate( PatientNrMoeder       = as.character(PatientNr),
                                           OperatieDatum         = as.Date(OperatieDatum, format = "%Y-%m-%d"),
                                           OperatieCode          = as.character(OperatieCode),
                                           NettoStartDatumTijd   = as.POSIXct(NettoStartDatumTijd, format = "%Y-%m-%d %H:%M:%S"),
                                           NettoEindDatumTijd    = as.POSIXct(NettoEindDatumTijd,  format = "%Y-%m-%d %H:%M:%S"),
                                           BrutoStartDatumTijd   = as.POSIXct(BrutoStartDatumTijd, format = "%Y-%m-%d %H:%M:%S"),
                                           BrutoEindDatumTijd    = as.POSIXct(BrutoEindDatumTijd,  format = "%Y-%m-%d %H:%M:%S") ) %>%
                                  select(-PatientNr)'


Meting          <- Meting      %>% mutate( PatientNrMoeder       = as.character(PatientNr),
                                           ObservatieDatum       = as.Date(ObservatieDatum, format = "%Y-%m-%d"),
                                           ObservatieOms         = as.character(ObservatieOmschrijving),
                                           Uitslag               = as.character(Uitslag)) %>%
                                    select(PatientNrMoeder, ObservatieDatum, ObservatieOms, Uitslag)


Opname          <- Opname      %>% mutate( PatientNr             = as.character(PatientNr),
                                           PatientNrKind         = as.character(Kind),
                                           OpnameDatum           = as.Date(OpnameDatumTijd,  format = "%Y-%m-%d"),
                                           OpnameDatumTijd       = as.POSIXct(OpnameDatumTijd,  format = "%Y-%m-%d %H:%M:%S"),
                                           OntslagDatum          = as.Date(OntslagDatumTijd, format = "%Y-%m-%d"),
                                           OntslagDatumTijd      = as.POSIXct(OntslagDatumTijd, format = "%Y-%m-%d %H:%M:%S"),
                                           Spoed                 = as.character(Spoed)) %>%
                                   select(PatientNr, PatientNrKind, OpnameDatum, OpnameDatumTijd, OntslagDatumTijd, OntslagDatum, Spoed)


'Lab            <- Lab          %>% mutate( PatientNr             = as.character(PatientNr),
                                           AfnameDatumTijd       = as.POSIXct(AfnameDatumTijd, format = "%Y-%m-%d %H:%M:%S"),
                                           BepalingCode          = as.character(BepalingCode),
                                           Uitslag               = as.character(Uitslag)) %>%
                                  filter( !is.na(Uitslag)) %>%
                                  select( PatientNr, AfnameDatumTijd, BepalingCode, Uitslag)


medicatie     <- medicatie     %>% mutate( PatientNrMoeder       = as.character(PatientNr),
                                           PatientNrKind         = as.character(Kind),
                                           start_datum           = as.Date(StartDatumTijd, format = "%Y-%m-%d"),
                                           stop_datum            = as.Date(EindDatumTijd, format = "%Y-%m-%d"),
                                           atc_code              = as.character(ATCCODE) ) %>%
                                   select( PatientNrMoeder, PatientNrKind, start_datum, stop_datum, atc_code)

Bloedproduct  <- Bloedproduct %>% mutate( PatientNrMoeder        = as.character(PatientNr),
                                          TransfusieDatum        = as.Date(TransfusieDatum, format = "%Y-%m-%d"),
                                          BloedStatusCode        = as.character(BloedStatusCode)) %>%
                                distinct( PatientNrMoeder, TransfusieDatum, BloedStatusCode)


ToedieningRegistratie  <- ToedieningRegistratie %>% mutate( PatientNr           = as.character(PatientNr),
                                                            ToedieningDatumTijd = as.POSIXct(ToedieningDatumTijd, format = "%Y-%m-%d %H:%M:%S"),
                                                            ToedieningDatum    = as.Date(ToedieningDatumTijd, format = "%Y-%m-%d", tz="CET"),  #kan ook met as.Date( format( as.POSIXct(ToedieningDatumTijd), "%Y-%m-%d"))
                                                            #ToedieningDatum     = as.Date(ToedieningDatumTijd, format = "%Y-%m-%d"),
                                                            atc_code            = as.character(ATCCODE) ) %>%
                                                    select( PatientNr, ToedieningDatum, ToedieningDatumTijd, atc_code)'


AanvullingUitkomst <- AanvullingUitkomst %>% mutate( Aandoening         = as.character(Aandoening),
                                                     Cyclus             = as.numeric(Cyclus),
                                                     Groep              = as.character(Groep),
                                                     PatientNr          = as.character(PatientNr),
                                                     Ind                = as.character(Ind),
                                                     Waarde             = as.character(Waarde),
                                                     Wegingsfactor      = as.numeric(Wegingsfactor),
                                                     Datum              = as.character(Datum))


# --- Patient selectie op basis van B41 DBC ----
patientselectie <- Verrichting %>% group_by(PatientNrMoeder, ZACode, Verrichtingdatum, ZorgType, DiagnoseCode, AGB_Code) %>%
                                   mutate(  Aantal             = sum(Aantal)) %>%
                                   filter(  AGB_Code          == "0307"    &
                                            DiagnoseCode      == "B41"     &
                                            Startdatum        >= inc_start &
                                            Startdatum        <= inc_eind  &
                                            Aantal            >= 1         &
                                            ZorgType          == "11" ) %>%
                                   ungroup() %>%
                                   distinct(PatientNrMoeder)


# --------- filter gegevens moeder ------------
moms            <- mom         %>% filter(PatientNrMoeder %in% patientselectie$PatientNrMoeder)


# --------- Koppel gegevens kinderen ------------
moederKind     <- merge( x     = moms,
                         y     = kids %>% filter(   GeboorteDatumKind  >= inc_start &
                                                    GeboorteDatumKind  <= inc_eind ) %>%
                                          arrange(  desc(GeboorteDatumTijdKind))     %>%   # Indien er meer ingevulde verslagen zijn, neem laatst ingevulde PRN verslag van kind
                                          distinct( PatientNrKind, .keep_all = TRUE),
                         by    = "PatientNrMoeder",
                         all.x = TRUE) %>%
                  distinct(PatientNrMoeder, PatientNrKind, .keep_all = TRUE)         # Bug fix voor dubbele die geintroduceerd worden door inclusie 2de vragenlijst HiX


# - Filter kinderen in patientselectie periode -
moederKind     <- moederKind %>% filter(GeboorteDatumKind >= inc_start &
                                        GeboorteDatumKind <= inc_eind)



# ----------- Leeftijd Moeder --------------------
moederKind            <- moederKind %>% mutate( BeginBaring    = ifelse( BeginBaring == 5 &
                                                                         Partusstart == "interventie om baring op gang te brengen", 3, BeginBaring ),
                                                leeftijd       = difftime(GeboorteDatumKind,
                                                                          GeboorteDatumMoeder,
                                                                          units = "days")/365.25,
                                                leeftijd       = as.numeric(leeftijd),
                                                leeftijd       = ifelse(is.na(leeftijd), 9999, leeftijd))


# ------- Convert zwangerschap naar dagen -------
moederKind     <- moederKind %>% filter( Zwd     != "Onbekende à terme datum" ) %>%
                                 mutate( Zwd      = as.numeric( substr(Zwd, 1,2) ) * 7 +
                                                    as.numeric( substr(Zwd, 4,4) ) ) %>%
                                 group_by(PatientNrMoeder) %>%
                                 mutate(  Zwddatum = min(GeboorteDatumKind) - days(Zwd) ) %>%
                                 ungroup()

# - Exludeer vrouwen zwangschapsduur < 32 weken --
moederKind          <- moederKind %>% filter(Zwd >= (32*7) )


# -------- Selecteer gegevens moeder ----------
verrichting_mom <- Verrichting %>% filter(PatientNrMoeder %in% patientselectie$PatientNrMoeder &
                                          is.na(PatientNrKind)
                                          ) %>%
                                   select(-PatientNrKind)

opnames_moeder  <- Opname      %>% filter(PatientNr       %in% patientselectie$PatientNrMoeder &
                                          is.na(PatientNrKind)
                                          ) %>%
                                   mutate(PatientNrMoeder  =   PatientNr) %>%
                                   select(-PatientNr, -PatientNrKind)

# ------- Selecteer gegevens kinderen ------------
opnames_kid     <- Opname      %>% filter(PatientNrKind %in% moederKind$PatientNrKind)

verrichting_kid <- Verrichting %>% filter( PatientNrKind %in%  moederKind$PatientNrKind)



# ---- Excludeer verrichtingen buiten ZA window ----
verrichting_mom <- merge( x     = verrichting_mom %>% filter(PatientNrMoeder %in% unique(moederKind$PatientNrMoeder) ),
                          y     = moederKind      %>% group_by( PatientNrMoeder) %>%
                                                      mutate(  GeboorteDatumKind = max(GeboorteDatumKind) ) %>%
                                                      ungroup() %>%
                                                      distinct( PatientNrMoeder, GeboorteDatumKind),
                          by    = "PatientNrMoeder",
                          all.x = TRUE) %>%
                   filter(Verrichtingdatum >= (GeboorteDatumKind - days(270)) &           # 9 maanden voor de bevalling
                            Verrichtingdatum <= (GeboorteDatumKind + days(60)) ) %>%        # 2 maanden na de bevalling
                   mutate(Aantal = as.numeric(Aantal)) %>%
                   select(-GeboorteDatumKind)


# --- Koppel opname en ontslag datum moeder ------
opname_ontslag_mom <- merge( x     = moederKind     %>% select(PatientNrMoeder, GeboorteDatumKind),
                             y     = opnames_moeder %>% select(PatientNrMoeder, OpnameDatum, OntslagDatum, OntslagDatumTijd),
                             by    = "PatientNrMoeder",
                             all.x = TRUE) %>%
                   filter(  OpnameDatum    <= GeboorteDatumKind &
                            OntslagDatum   >= GeboorteDatumKind) %>%
                   group_by(PatientNrMoeder) %>%
                   mutate(  OntslagDatum_moeder     = max(OntslagDatum),
                            OntslagDatumTijd_moeder = max(OntslagDatumTijd),
                            OpnameDatum_moeder      = min(OpnameDatum)) %>%
                   distinct(PatientNrMoeder, OpnameDatum_moeder, OntslagDatum_moeder, OntslagDatumTijd_moeder)


moederKind     <- merge( x     = moederKind,
                         y     = opname_ontslag_mom,
                         by    = "PatientNrMoeder",
                         all.x = TRUE)


# --- Koppel opname en ontslag datum kind ------
opname_ontslag_kid     <- merge(  x     = moederKind  %>% select(PatientNrKind, GeboorteDatumKind),
                                  y     = opnames_kid %>% select(PatientNrKind, OpnameDatum, OntslagDatum),
                                  by    = "PatientNrKind",
                                  all.x = TRUE) %>%
                         filter(  OpnameDatum      <= GeboorteDatumKind &
                                  OntslagDatum     >= GeboorteDatumKind) %>%
                         group_by(PatientNrKind) %>%
                         mutate(  OpnameDatum_Kind  = min(OpnameDatum),
                                  OntslagDatum_Kind = max(OntslagDatum) ) %>%
                         ungroup() %>%
                         distinct(PatientNrKind, OpnameDatum_Kind, OntslagDatum_Kind)

moederKind     <- merge( x     = moederKind,
                         y     = opname_ontslag_kid,
                         by    = "PatientNrKind",
                         all.x = TRUE)

# ----------------- BMI ---------------
MetingBMI           <- merge(  x     = Meting %>% mutate(Uitslag = as.numeric(Uitslag)),
                               y     = moederKind %>% distinct(PatientNrMoeder, Zwddatum),
                               by    = "PatientNrMoeder",
                               all.x = TRUE) %>%
                       filter((ObservatieOms == "Lengte"  & Uitslag > 120) |
                              (ObservatieOms == "Gewicht" & Uitslag > 30))

MetingBMI           <- MetingBMI %>% filter(  PatientNrMoeder %in% moederKind$PatientNrMoeder &
                                             (ObservatieOms    == "Lengte" |
                                              ObservatieOms    == "Gewicht"))  %>%
                                     group_by(PatientNrMoeder, ObservatieOms) %>%
                                     filter(  min( abs( difftime(ObservatieDatum, Zwddatum, units = "days"))) ==      # Absolute minimale verschil is verschil en dus datum dichtste bij de peildatum
                                                   abs( difftime(ObservatieDatum, Zwddatum, units = "days")) ) %>%
                                     mutate( Uitslag = mean(Uitslag) ) %>%
                                     ungroup(PatientNrMoeder) %>%
                                     mutate(  Uitslag = ifelse( ObservatieOms == "Lengte", as.numeric(Uitslag)/100, as.numeric(Uitslag)) ) %>%
                                     distinct(PatientNrMoeder, ObservatieOms, ObservatieDatum, Uitslag)

BMI                  <- merge( x     = MetingBMI %>% filter(ObservatieOms == "Lengte")  %>% mutate( Lengte  = Uitslag) %>% select(PatientNrMoeder, Lengte),
                               y     = MetingBMI %>% filter(ObservatieOms == "Gewicht") %>% mutate( Gewicht = Uitslag) %>% select(PatientNrMoeder, Gewicht),
                               by    = "PatientNrMoeder",
                               all.x = TRUE ) %>%
                        mutate(BMI   =  round(Gewicht / (Lengte*Lengte), digits = 2),
                               BMI   = ifelse(is.na(BMI), 9999, BMI))


moederKind           <- merge( x      = moederKind,
                               y      = BMI,
                               by     = "PatientNrMoeder",
                               all.x  = TRUE)


# ---------------- Meerling -----------
moederKind             <- moederKind %>% group_by( PatientNrMoeder) %>%
                                         mutate(   Meerling  = n()) %>%
                                         ungroup()


# --------- Ligging kind ----------
moederKind             <- moederKind %>% mutate( LigKind   = ifelse( LigKind == "achterhoofd-voor"   |
                                                                       LigKind == "achterhoofd-achter" |
                                                                       LigKind == "kruin"              |
                                                                       LigKind == "aangezicht" |
                                                                       LigKind == "voorhoofd",                 1, LigKind),
                                                 LigKind   = ifelse( LigKind == "volkomen stuit"     |
                                                                       LigKind == "onvolkomen stuit"   |
                                                                       LigKind == "half onvolkomen stuit",      2, LigKind),
                                                 LigKind   = ifelse(LigKind == "overig",                     3, LigKind),
                                                 LigKind   = ifelse( is.na(LigKind) | LigKind == "onbekend", 9999, LigKind) )

# --------- Koppel eindebaring aan moederKind gegevens
eindebaring          <- moederKind %>%   mutate( Groep       = "BM MoederKind",
                                                 Ind         = "eb",
                                                 PatientNr   = PatientNrMoeder,
                                                 Waarde      = ifelse(Partustype     == "spontaan", 1, Partustype),
                                                 Waarde      = ifelse(Waarde         == "vaginale kunstverlossing", 2, Waarde),
                                                 Waarde      = ifelse(Waarde         == "sectio" &
                                                                      Sectio == "secundaire sectio", 3, Waarde),
                                                 Waarde      = ifelse(Waarde         == "sectio" &
                                                                      Sectio == "primaire sectio"  , 4, Waarde) ) %>%
                                       arrange( desc(Waarde)) %>%                        # Sorteer zodanig dat hoogste code (het ergste type) bovenaan komt te staan
                                       distinct(PatientNrMoeder, .keep_all = TRUE) %>%
                                       mutate(  Waarde      = ifelse(!(Waarde %in% c(1:4)), 9999, Waarde))

moederKind            <- merge( x     = moederKind,
                                y     = eindebaring %>% mutate(eindebaring     = Waarde,
                                                               PatientNrMoeder = PatientNr) %>%
                                  select(PatientNrMoeder, eindebaring),
                                by    = "PatientNrMoeder",
                                all.x = TRUE)


# --- Koppel beginbaring aan moederKind gegevens
beginbaring           <- moederKind %>% arrange(GeboorteDatumTijdKind) %>%                # sorteer dusdanig dat waarde van eerste kind gekozen wordt.
                                        mutate(Groep     = "BM MoederKind",
                                               Ind       = "beginbaring",
                                               PatientNr = PatientNrMoeder,
                                               Waarde    = BeginBaring, #1=spontaan, 2/3=primen/inleiden, 4=geplande sectio
                                               Waarde    = ifelse(BeginBaring == 9, 9999, BeginBaring))

moederKind            <- merge( x     = moederKind  %>% select(-BeginBaring),
                                y     = beginbaring %>% mutate(beginbaring     = Waarde,
                                                               PatientNrMoeder = PatientNr) %>%
                                                        select(PatientNrMoeder, beginbaring),
                                by    = "PatientNrMoeder",
                                all.x = TRUE)


# -------------- Epiduralen --------------
moederKind            <- merge( x         = moederKind,
                                y         = Verrichting %>% filter( ZACode %in% c("039600", "039683", "030545", "030546",
                                                                                  "030547", "030540", "030560", "030564")) %>%
                                                           distinct( PatientNrMoeder) %>%
                                                           mutate(   Epiduraal = 1 ) ,
                                by        = "PatientNrMoeder",
                                all.x     = TRUE) %>%
                        mutate( Epiduraal = ifelse(is.na(Epiduraal), 0, Epiduraal) )

# ------------- Bijstimuleren ------------
moederKind              <- moederKind %>% mutate(Bijstimulatie = ifelse(Bijstimulatie == "ja", 1, 0),
                                                 Bijstimulatie = ifelse(is.na(Bijstimulatie),  9999, Bijstimulatie))


# ----- Duur gebroken vliezen > 1080 min -----
duurgebrvliezen      <- moederKind %>% mutate( Groep      = "BM MoederKind",
                                               PatientNr  = PatientNrKind,
                                               duurVlies  = difftime(GeboorteDatumTijdKind, VliesDatumTijd, units = "min"),
                                               duurVlies  = ifelse(duurVlies >= 365*24*60 & VliesDatumTijd + years(1) <= GeboorteDatumTijdKind,
                                                                   difftime(GeboorteDatumTijdKind, VliesDatumTijd + years(1), units = "min"), duurVlies),
                                               duurVlies  = ifelse(duurVlies < 0 & VliesDatumTijd <= GeboorteDatumTijdKind + days(1),
                                                                   difftime(GeboorteDatumTijdKind + days(1), VliesDatumTijd, units = "min"), duurVlies),
                                               duurVlies  = ifelse(duurVlies >= 0 & duurVlies <= 4500, duurVlies, NA))

duurgebrvliezen       <- duurgebrvliezen %>% mutate( Ind        = "dgv",
                                                     Waarde     = ifelse(duurVlies >= 0 &
                                                                         duurVlies <= 18*60, 0, NA),
                                                     Waarde     = ifelse(duurVlies >  18*60, 1, Waarde),
                                                     Waarde     = ifelse(is.na(Waarde),     9999, Waarde) )

moederKind            <- merge( x     = moederKind,
                                y     = duurgebrvliezen %>% mutate(langgebrvliezen     = Waarde,
                                                                   PatientNrKind     = PatientNr) %>%
                                  select(PatientNrKind, langgebrvliezen),
                                by    = "PatientNrKind",
                                all.x = TRUE)

# ---------- Persduur ---------------
persduur             <- moederKind %>% filter( !(eindebaring == 4 |
                                                (eindebaring == 3 & (IndicatieSectio == "niet vorderende ontsluiting") ) |
                                                (eindebaring == 3 & (IndicatieSectio == "verdenking foetale nood" & is.na(PersenDatumTijd))) |
                                                 eindebaring == 99 ) |
                                               is.na(IndicatieSectio) ) %>%
                                       mutate( Groep         = "BM MoederKind",
                                               Persduur   = difftime(GeboorteDatumTijdKind, PersenDatumTijd, units = "min"),                               # correctie middernacht/registratie fout
                                               Persduur   = ifelse(Persduur >= 365*24*60 & PersenDatumTijd + years(1) <= GeboorteDatumTijdKind,
                                                                   difftime(GeboorteDatumTijdKind, PersenDatumTijd + years(1), units = "min"), Persduur),
                                               Persduur   = ifelse(Persduur < 0,
                                                                   difftime(GeboorteDatumTijdKind + days(1), PersenDatumTijd, units = "min"), Persduur),
                                               Persduur   = ifelse(Persduur >= 0 & Persduur <= 180, Persduur, 9999))


moederKind            <- merge( x     = moederKind,
                                y     = persduur %>%
                                  select(PatientNrKind, Persduur),
                                by    = "PatientNrKind",
                                all.x = TRUE)


# - Moeders overgedragen tijdens bevalling -
# ------ Overdracht van eerste lijn ------
moederKind       <- moederKind %>% mutate(Overdracht    = ifelse(is.na(Overdracht), 9999, Overdracht))

# ------ NTSV bevallingen ------
moederKind            <- moederKind %>% mutate(NTSV = ifelse( Meerling == 1 &        # Eenlingen
                                                              LigKind  == 1 &        # Hoofdligging
                                                              Pariteit == 0 &        # Eerste bevalling
                                                              Zwd      >= 37*7,      # Voldragen zwangerschap
                                                              1, 0))

# ---- U2.2 Bloedverlies moeders ---------
moederKind          <- moederKind %>% mutate(Bloedverlies1000 = ifelse(Bloedverlies > 1000, 1, 0),
                                             Bloedverlies1000 = ifelse(is.na(Bloedverlies), 9999, Bloedverlies1000))


# inleiding
moederKind          <- moederKind %>% mutate(Inleiding = ifelse(beginbaring == 2 | beginbaring == 3, 1, 0),
                                             Inleiding = ifelse(beginbaring==9999, 9999, Inleiding))

# Totaalbestand (pas hier cyclus nummer aan)
moederKind_11          <- moederKind %>% mutate() %>%
                                      filter(NTSV==1) %>%
                                      select(PatientNrMoeder, Zwd,  Bloedverlies, Bloedverlies1000, leeftijd, BMI, Overdracht,
                                             Inleiding, eindebaring, langgebrvliezen, Bijstimulatie, Epiduraal, Geboortegewicht, Persduur)

library(openxlsx)
save(moederKind_11, file = paste0("moederKind_", as.character(Cyclus_nr), ".rda"))

#Draai script hierboven voor cyclus 10 en 11, dan door naar onderstaande stukje


#Koppeling bestanden C10+C11

load("moederKind_10.rda")
load("moederKind_11.rda")

Totaalbestand       <- bind_rows(moederKind_10,
                                 moederKind_11)

#Patiëntnummer versleutelen
library(stringr)
Totaalbestand          <- Totaalbestand %>%
                       mutate(n =1:n()) %>%
                       mutate(sleutelnummer=str_pad(n, 4, pad = "0")) %>%
                       mutate(sleutelnummer = gsub("^(.{0})(.*)$",
                                                   "\\MZH\\2", #pas aan voor eigen ziekenhuis
                                                   sleutelnummer)) %>%
                       select(-n)
Versleuteling_ptnr <- Totaalbestand %>% select(sleutelnummer, PatientNrMoeder)
write.csv(Versleuteling_ptnr, "Versleuteling_ptnr_onderzoek_fluxus.csv", row.names=FALSE)

#Verwijder ptnr Moeder
Totaalbestand <- Totaalbestand %>% select(-PatientNrMoeder)
write.csv(Totaalbestand, "Totaalbestand_fluxus.csv", row.names=FALSE)

