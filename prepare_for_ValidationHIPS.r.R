# Prepare for ValidationHIPS

# Neem de volgende stappen:
# 0. doe een pull zodat je de meest recente versie hebt
# 1. run eerst 30_gold_ibd 
# 2. run daarna dit script (prepare_for_ValidationHIPS.r)
# 3. run daarna ValidationHIPS.r

# Dit script:
# 1. zet huis data om in wide data om te kunnen vergelijken met de hips data
# 2. hernoemt 35_wide naar 35_wide_orig zodat het origineel altijd beschikbaar is
# 2. past de HIPS data (minimaal) aan om te kunnen vergelijken

# verwijder alle variabelen
rm(list=ls())

# Import standard libraries
library(dplyr)
library(lubridate)
library(readxl)
library(stringr)
library(openxlsx)
library(tidyr)

# Goede map zetten (cyclus 7)
setwd("K:/SHAREDIR/Data Santeon projecten/scorekaarten/IBD/Cyclus 7")

# Set disease, type and house
s_Domein <- "sb"
s_Aandoening <- "ibd"
s_Huis <- "msz"

# Inladen huis data
# outputdataset (cyclus 7)
load("OutputDataSet_2021.rda") 

outputdataset = OutputDataSet %>%
   mutate(Groep = ifelse(Groep == "CM Crohn" | Groep == "SK Crohn", "crohn", Groep),
          Groep = ifelse(Groep == "CM CU" | Groep == "SK CU", "cu", Groep)) %>%
   group_by(PatientNr, Groep) %>%
   mutate(Wegingsfactor = min(Wegingsfactor)) %>%
  ungroup()

# Inladen data_hips 
setwd("K:/SHAREDIR/Data Santeon projecten/HIPS/HipsETL_Olof") # HIPS path
hips_wide_orig_path = str_interp("../Data/${s_Huis}-${s_Domein}-${s_Aandoening}/35_wide/${s_Aandoening}_35_wide_orig.xlsx")
hips_wide_path = str_interp("../Data/${s_Huis}-${s_Domein}-${s_Aandoening}/35_wide/${s_Aandoening}_35_wide.xlsx")

# als dit de eerste keer runnen is: kopieërt 35_wide (output 30_gold_ibd script) en hernoemt het 'orig'
# dat blijft altijd de originele output van het 30_gold_ibd script 
if (!file.exists(hips_wide_orig_path)) {
  # If it doesn't exist, copy the content from the existing file
  file.copy(hips_wide_path, hips_wide_orig_path)
  cat("File", hips_wide_orig_path, "created based on", hips_wide_path, "\n")
} else {
  cat("File", hips_wide_orig_path, "already exists.\n")
}

# import 'orig' file
data_hips <- readxl::read_excel(hips_wide_orig_path) 

# aanpassen Inclusiedatum aan HIPS data
window_begindatum <- as.Date( "2021-01-01", format = "%Y-%m-%d")
window_einddatum  <- as.Date( "2021-12-31", format = "%Y-%m-%d")

data_hips_2021 <- data_hips %>% 
  filter(InclusieDatum >= window_begindatum & InclusieDatum <= window_einddatum) %>%
  select(Identificatienummer, InclusieDatum)

# Pas huis data aan

# zet om naar wide data
data_wide <- spread(outputdataset, Ind, Waarde)

# mutate variabelen zodat die van scorekaarten ('data_huis') corresponderen met
# die van HIPS ('data_hips')
data_huis       <- data_wide %>%
  transmute(
    Aandoening                    = s_Aandoening,
    Groep                         = as.character(Groep),
    Identificatienummer           = as.character(PatientNr),
    ZiekenhuisCode                = s_Huis,
    Geslacht                      = ifelse(gsl==2, "F", "M"),
    BMI                           = as.numeric(bmi),
    Leeftijd                      = as.numeric(lft),
    Roken                         = as.numeric(rok),
    Reumatologische_afwijking     = ifelse(reu==2, 0, 1),
    Physician_Global_Assessment   = pga, 
    'Montreal_Score_Crohn,_score_A' = monzvca,
    'Montreal_Score_Crohn,_score_B' = monzvcb,
    'Montreal_Score_Crohn,_score_L' = monzvcl,
    'Montreal_Score_CU,_score_S' = moncus,
    'Montreal_Score_CU,_score_E' = moncue,
    
    'Type_Operatie:_Perianaal_abces'                        = as.numeric(abces),
    'Type_Operatie:_Subtotale_colectomieen'                 = as.numeric(subcol),
    'Type_Operatie:_ileocaecale_resectie_/_hemicolectomie'  = as.numeric(ileoc),
   
    Percentage_patienten_met_ongeplande_opname              = as.numeric(U2.5),
    Percentage_patienten_met_ongeplande_heropname           = as.numeric(U2.6),
    Percentage_patienten_op_SEH                             = as.numeric(U4.1),   
    
    Aantal_keer_op_de_SEH_per_persoonsjaar                  = as.numeric(U4.1.1),
    Percentage_patienten_op_SEH_met_aansluitend_opname      = as.numeric(U4.1.2),       
    
    'Percentage_patienten_medicatie:_Infliximab_gebruik'    = as.numeric(K1.1),
    'Percentage_patienten_medicatie:_Adalimumab_gebruik'    = as.numeric(K1.2),
    'Percentage_patienten_medicatie:_Golimumab_gebruik'     = as.numeric(K1.3),
    'Percentage_patienten_medicatie:_Vedolizumab_gebruik'   = as.numeric(K1.4),
    'Percentage_patienten_medicatie:_Ustekinumab_gebruik'   = as.numeric(K1.5),
    'Percentage_patienten_medicatie:_Tofacitinib_gebruik'   = as.numeric(K1.6),
    'Percentage_patienten_medicatie:_Twee_biologicals'      = as.numeric(K1.11.1),
    'Percentage_patienten_medicatie:_3_of_meer_biologicals' = as.numeric(K1.11.2),
    
    'Diagnostiek:_Aantal_scopieen_per_persoonsjaar'         = as.numeric(K3.1.1),
    'Diagnostiek:_Percentage_patienten_met_scopie'          = as.numeric(K3.1.2),
    'Diagnostiek:_Aantal_MRIs_per_persoonsjaar'             = as.numeric(K3.3.1),
    'Diagnostiek:_Percentage_patienten_met_MRI'             = as.numeric(K3.3.2),
    'Diagnostiek:_Aantal_calpro_metingen'                   = as.numeric(K3.6.1),
    'Diagnostiek:_Percentage_patienten_met_>=_4_calpro_metingen'                      = as.numeric(K3.6.2),
    'Diagnostiek:_Percentage_patienten_met_scopie_en_binnen_90_dagen_calpro_meting'   = as.numeric(K3.6.3),
    'Diagnostiek:_Percentage_patienten_met_scopie_of_calpro_meting'                   = as.numeric(K3.6.5),
    
    Aantal_verpleegdagen_per_patient                      = as.numeric(K4.1),                                         
    Aantal_opnames_per_patient                            = as.numeric(K4.3.1), 

    Percentage_patienten_met_biological_spiegel_meting    = as.numeric(P3),
    'Polikliniekbezoek'                                   = as.numeric(K2.1.1) + as.numeric(K2.1.2),
    'Teleconsult'                                         = as.numeric(K2.2.1) + as.numeric(K2.2.2),
    Inclusiedatum_huis = as.Date(as.numeric(inclusie_datum),format = "%Y-%m-%d",origin = "1970-01-01")) %>%
  left_join(data_hips_2021,by="Identificatienummer")

# path to save data_huis
setwd("K:/SHAREDIR/Data Santeon projecten/HIPS/HipsETL_Olof") # HIPS path
data_huis_path = str_interp("../Data/${s_Huis}-${s_Domein}-${s_Aandoening}/${s_Aandoening}_WideFormat_huis.xlsx") 

# save data_huis
write.xlsx(data_huis,data_huis_path)

# Pas HIPS data aan
age_group_breaks <- c(18,29,49,69,Inf)
bmi_group_breaks <- c(0,18.5,25,30,Inf)

data_hips_recoded <- data_hips %>%
  mutate(
    rok = recode(rok,
      "Rook dagelijks/soms" = 1,
      "Ex-roker" = 2,
      "Nooit gerookt" = 0, 
      "Niet-roker, maar rookgedrag in verleden onbekend" = 0,
      "Rookt passief" = 99,
      .default = NA_real_),
    rok = replace_na(rok, 99),
    lft = cut(lft, breaks = age_group_breaks, include.lowest=T, labels = F),
    lft = replace_na(lft, 99),
    bmi = cut(bmi, breaks = bmi_group_breaks, include.lowest=T, labels = F),
    bmi = replace_na(bmi, 99),
    reu = if_else(reu > 1, 1, reu),
  )

# save data_hips
write.xlsx(data_hips_recoded,hips_wide_path)


