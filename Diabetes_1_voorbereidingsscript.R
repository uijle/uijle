# VBHC Aandoening: Diabetes
# Cyclus 5
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
startwindow        <- as.Date("2021-01-01", format ="%Y-%m-%d")
stopwindow         <- as.Date("2021-12-31", format ="%Y-%m-%d")
startwindow_lab    <- as.Date("2019-10-01", format ="%Y-%m-%d")
Cyclus_nr          <- "C5"
#Follow Up instellen
LTFU               <- as.Date("2021-01-01", format ="%Y-%m-%d")

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

# ------ Define working directory and filenames ------
setwd("/home/afdelingen/kwaliteit.en.veiligheid/Diabetes/Cyclus 5/Scorekaart/")
fname              <- "Bronbestand.xlsx"

# ------ Locatie en naam van logboek ------
logboekLocatie     <- "L:/VBHC/Data/Diabetes/Cyclus 5/Scorekaart/"
logboeknaam        <- "Ziekenhuis_Diabetes_Cyc5_2022-07-25.xlsx"
