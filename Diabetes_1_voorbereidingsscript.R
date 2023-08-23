# VBHC Aandoening: Diabetes
# Cyclus 7
# Script 1
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

library(dplyr)        # For using piping operator
library(lubridate)    # Voor easy date manupilation
library(readxl)       # Nodig zolang Excel ingelezen wordt
library(tidyr)
library(openxlsx)
library(stringr)

rm(list=ls())

### Settings
case_mix_name      <- "Case-mix"
scorekaart_name    <- "Scorekaart"

### 2019 ###
#startwindow        <- as.Date("2019-01-01", format ="%Y-%m-%d")
#stopwindow         <- as.Date("2019-12-31", format ="%Y-%m-%d")
#startwindow_lab    <- as.Date("2017-10-01", format ="%Y-%m-%d")
#Cyclus_nr          <- "C1"
#Follow Up instellen
#LTFU               <- as.Date("2019-01-01", format ="%Y-%m-%d")

### 2020 ###
#startwindow        <- as.Date("2020-01-01", format ="%Y-%m-%d")
#stopwindow         <- as.Date("2020-12-31", format ="%Y-%m-%d")
#startwindow_lab    <- as.Date("2018-10-01", format ="%Y-%m-%d")
#Cyclus_nr          <- "C3"
#Follow Up instellen
#LTFU               <- as.Date("2020-01-01", format ="%Y-%m-%d")

### 2021 ###
#startwindow        <- as.Date("2021-01-01", format ="%Y-%m-%d")
#stopwindow         <- as.Date("2021-12-31", format ="%Y-%m-%d")
#startwindow_lab    <- as.Date("2019-10-01", format ="%Y-%m-%d")
#Cyclus_nr          <- "C5"
#Follow Up instellen
#LTFU               <- as.Date("2021-01-01", format ="%Y-%m-%d")

### 2022 ###
startwindow        <- as.Date("2022-01-01", format ="%Y-%m-%d")
stopwindow         <- as.Date("2022-12-31", format ="%Y-%m-%d")
startwindow_lab    <- as.Date("2020-10-01", format ="%Y-%m-%d")
Cyclus_nr          <- "C7"
#Follow Up instellen
LTFU               <- as.Date("2022-01-01", format ="%Y-%m-%d")

#ZA- en DBC codes instellen
ZA_ligduur         <- c("190090" , "190091" , "190218" , "190031" , "190038" ,
                        "190034" , "190204" , "190205" , "190035" , "190055")
ZA_ligduurIC       <- c("190157" , "190158")
ZA_consult         <- c("190060" , "190013" , "190063" , "190066" , 
                        "190025" , "190161" , "190162" , "190163" , "190164" , "190165" , "190166" ,"190019")
ZA_poli            <- c("190060" , "190013" , "190063" , "190066") 
ZA_tel             <- c("190025" , "190161" , "190162" , "190163" , "190164" , "190165" , "190166" , "190019")           
ZA_SEH             <- c("190015" , "190016")
ZA_psych           <- c("194164" , "194163" , "194162" , "194166")
ZA_amputatie       <- c("038590" , "038591" , "038790" , "038791" , "038690" ,
                        "038691" , "038791" , "038793" , "038794" , "038795" , "038747")
ZA_fundus          <- c("039917" , "039918")
ZA_IV              <-   "039810"
ZA_optometrie      <- c("192848" , "192849" , "192854")
ZA_Dietist         <- c("192989" , "192990" , "192840")

DBC_retino         <- c("754" , "755" , "757" , "759")
DBC_oogarts        <-   "751"
DBC_cardiovasc     <- c("202" , "203" , "301" , "204" , "205" , "401" , "402" , "403" ,
                        "404" , "409" , "501" , "502" , "509" , "701" , "702" , "709")
DBC_dialyse        <- c("331" , "332" , "336" , "339")
DBC_diabetes       <- c("221" , "222" , "223")

#ICD-10 codes instellen conform DPARD optieset 14
#20230718: H36.0 en G99 toegevoegd
ICD_retinopathie       <- c("E10.3", "E11.3", "E12.3", "E13.3", "E14.3", "H36.0") 
ICD_angina             <- c("I20.0", "I20.1", "I20.8", "I20.9")
ICD_myocardinfarct     <- c("I21.0", "I21.1", "I21.2", "I21.3", "I21.4", "I21.9", "I22.0", "I22.1", "I22.8", "I22.9")
ICD_ischemie           <- c("I23.0", "I23.1", "I23.2", "I23.3", "I23.4", "I23.5", "I23.6", "I23.8", "I25.0", "I25.1", 
                            "I25.3", "I25.4", "I25.5", "I25.6", "I25.8", "I25.9")
ICD_hypertensie_z      <- c("I10")
ICD_hypertensie        <- c("I15.0", "I15.1", "I15.2", "I15.8", "I15.9")
ICD_TIA                <- c("G45.8")
ICD_CVA                <- c("I60.0", "I60.1", "I60.2", "I60.3", "I60.4", "I60.5", "I60.6", "I60.7", "I60.8", "I60.9", 
                            "I61.0", "I61.1", "I61.2", "I61.3", "I61.4", "I61.5", "I61.6", "I61.8", "I61.9", "I62.0", 
                            "I62.1", "I62.9", "I63.0", "I63.1", "I63.2", "I63.3", "I63.4", "I63.5", "I63.6", "I63.8",
                            "I63.9", "I64"  , "I65.0", "I65.1", "I65.2", "I65.3", "I65.8", "I65.9", "I66.0", "I66.1", 
                            "I66.2", "I66.3", "I66.4", "I66.8", "I66.9", "I67.0", "I67.1", "I67.2", "I67.3", "I67.4", 
                            "I67.5", "I67.6", "I67.7", "I67.8", "I67.9", "I68.0", "I68.1", "I68.2", "I68.8", "I69.0", 
                            "I69.1", "I69.2", "I69.3", "I69.4", "I69.8")
ICD_claudicatio        <- c("I73.9")
ICD_aneurysma          <- c("I71.0", "I71.1", "I71.2", "I71.3", "I71.4", "I71.5", "I71.6", "I71.8", "I71.9")
ICD_neuropathie        <- c("E10.4", "E11.4", "E12.4", "E13.4", "E14.4", "G99.0")
ICD_depressie          <- c("F32.0", "F32.1", "F32.2", "F32.3", "F32.8", "F32.9")
ICD_vitiligo           <- c("L80")
ICD_nierfunctie        <- c("N17.0", "N17.1", "N17.2", "N17.8", "N17.9", "N18.1", "N18.2", "N18.3", "N18.4", "N18.5", 
                            "N18.9", "N19")
ICD_albuminurie        <- c("R80")

#Medicatie instellen
MED_RAS                <- c("C09A|C09B|C09C|C09D|C09X")
#MED_Statine            <- c("C10")

# ------ Define working directory and filenames ------
setwd("L:/VBHC/Data/Diabetes/Cyclus 7/Data")
fname              <- "Bronbestand.xlsx"

# ------ Locatie en naam van logboek ------
logboekLocatie     <- "L:/VBHC/Data/Diabetes/Cyclus 7/Scorekaart/"
logboeknaam        <- "Ziekenhuis_Diabetes_Cyc7_YYYY-MM-DD.xlsx"
