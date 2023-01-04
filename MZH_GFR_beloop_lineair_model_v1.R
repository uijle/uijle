# install.packages("statmod")
# install.packages("lme4")


library(readxl)
library(dplyr)
library(tidyr)
#library(XLConnect)
library(sqldf)
library(stringr)
library(lubridate)

library(survival)
library(survminer)
library(stadmod)
library(lme4)
library(sqldf)
library(xlsx)

rm(list=ls())


startwindow <- as.Date("2018-01-01",format = "%Y-%m-%d")
stopwindow <- as.Date("2022-10-31",format = "%Y-%m-%d")


#### ALLEEN INLADEN SUBTRAJECT, PATIENT EN TRANSPLANTATIE
# LET OP IVM ZEER GROOT BESTAND, OVERIGE DATA PAS LATER INLADEN!

dir_patselectie = "K:/SHAREDIR/Data Santeon projecten/scorekaarten/nierfalen/Cyclus 11/Bron"
Subtraject = "MZR000334_Cyclus 11_Subtrajecten.csv"
Patient = "patient.csv"

# inladen Excel data 
setwd(dir_patselectie) ## hier het pad waar je patientenselectie staat
Subtraject<- read.csv(Subtraject, header=FALSE, sep=";",stringsAsFactors=FALSE) 
Patient<- read.csv(Patient, header=TRUE, sep=";",stringsAsFactors=FALSE) 



setwd ("K:/SHAREDIR/Data Santeon projecten/scorekaarten/nierfalen/Cyclus 11/Bron")

# data voor 3-jaars afname 
# data <- lapply(excel_sheets("MZR000334_Cyclus 11_GFR populatie_2022-11-10.xlsx"), read_excel, path = "MZR000334_Cyclus 11_GFR populatie_2022-11-10.xlsx")
# Patient =data[[2]]


Tx_lijst <- lapply(excel_sheets("transplantatielijst_okt22.xlsx"), read_excel, path = "transplantatielijst_okt22.xlsx")
Tx_lijst= Tx_lijst[[1]]


##########################
# Standaardiseren data
#########################

Patient <- as.data.frame(Patient)%>%
  transmute (PatientNr = as.character(PatientNr), 
             GeboorteDatum=as.Date(GeboorteDatum, format="%d-%m-%Y"),
             OverlijdensDatum=as.Date(OverlijdensDatum, format="%d-%m-%Y"))%>% 
  distinct()

Tx_lijst <- as.data.frame(Tx_lijst) %>%
  transmute(PatientNr=as.character(hisNumber), 
            Tx_datum = as.Date("Tx datum", "%Y-%m-%d"))


# Nieuw sinds C11 : nierschade, TX dbc's en andere chronische predialyse nierschade gerelateerde dbc's worden samengenomen om de groep 'nierschade' pati?nten vollediger te krijgen 
Subtraject <- Subtraject %>%
  rename(SpecialismeCode=V8)%>%
  filter(SpecialismeCode=="INT" )%>%
  transmute( PatientNr = as.character(V1),
             OpeningsDatumDBC    = as.Date(V6, format = "%Y-%m-%d"),
             SluitingsDatumDBC   = as.Date(V7, format = "%Y-%m-%d"),
             StatusDBC           = ifelse(V11=="Leeg",1,0),                     
             ZorgType            = ifelse(V3=="R", 11, V3),
             ZorgType            = ifelse(V3=="L", 21, ZorgType),
             DiagnoseCode        = as.numeric(V10),
             groep               = ifelse(DiagnoseCode==324 | DiagnoseCode==325 | DiagnoseCode==076  | DiagnoseCode==078 | DiagnoseCode==301 | 
                                            DiagnoseCode==303 | DiagnoseCode==304 | DiagnoseCode==311 | DiagnoseCode==313 | DiagnoseCode==399 ,1,NA), # brede groep chronische nierschade
             groep               = ifelse( DiagnoseCode==336 | DiagnoseCode==339,2,groep),         # HD
             groep               = ifelse( DiagnoseCode==331 | DiagnoseCode==332,3,groep))%>%         # PD
  filter(!is.na(groep))

# Patientselectie 
PatSel <- Subtraject %>%
  filter(ZorgType==11) %>% # voor U8 alleen pati?nten die binnen timewindow nieuwe nierschade pati?nten zijn meenemen
  distinct(PatientNr)

Subtraject <- left_join(PatSel, Subtraject)


# NIEUW: we tellen de Tx dbc's op bij de nierschade dbc's als er minimaal ??n nierschade dbc is geweest

temp <- Subtraject %>% 
  group_by(PatientNr)%>%
  filter(groep==1 & OpeningsDatumDBC<=stopwindow & (SluitingsDatumDBC >=startwindow | is.na(SluitingsDatumDBC)))%>%    #HIPS; is.na toegevoegd voor als sluitingsdatum nog niet is geweest
  arrange(PatientNr, desc(DiagnoseCode))%>%
  summarise(DiagnoseCode=first(DiagnoseCode))%>%
  filter(DiagnoseCode==324 | DiagnoseCode==325| DiagnoseCode==301 | DiagnoseCode==303 | DiagnoseCode==304 | DiagnoseCode==311 | DiagnoseCode==313 | DiagnoseCode==399)%>%
  mutate(groep=1)%>%
  select(-DiagnoseCode)

Subtraject1 <- left_join(Subtraject, temp, by="PatientNr")%>%
  mutate(groep.x= ifelse(groep.x==1 & groep.y!=1,NA,groep.x))%>%     
  select(-groep.y)%>%
  rename(groep = groep.x)%>%
  filter(!is.na(groep))

rm(temp, Subtraject, PatSel)




###################################################
#### basisselectie & klaar maken voor analyses
###################################################

# excludeer pati?nten <18 jaar
Patient <- Patient%>%
  mutate  (Leeftijd   = difftime( startwindow,GeboorteDatum, units = "days")/365.25)%>%
  filter  (Leeftijd>=18)%>%
  distinct(PatientNr, OverlijdensDatum)


# Datum bij missende einddatum invullen

Subtraject <- left_join(Patient, Subtraject1, by="PatientNr")


Subtraject <- as.data.frame(Subtraject) %>%
  mutate(SluitingsDatumDBC = ifelse(is.na(SluitingsDatumDBC) & ZorgType==11,OpeningsDatumDBC + 89,SluitingsDatumDBC))  %>%
  mutate(SluitingsDatumDBC = ifelse(is.na(SluitingsDatumDBC) & ZorgType==21,OpeningsDatumDBC + 119,SluitingsDatumDBC)) %>%
  mutate(SluitingsDatumDBC = as.Date(SluitingsDatumDBC,origin="1970-01-01"))

Subtraject <- Subtraject %>%
  filter(SluitingsDatumDBC>=startwindow & OpeningsDatumDBC<=stopwindow)



### Samenvoegen opvolgende regels, per PatientNr, behandelgroep en status vol/leeg
Subtraject1 <- Subtraject %>%
  ungroup() %>%
  arrange(PatientNr,groep,StatusDBC,OpeningsDatumDBC,SluitingsDatumDBC) %>%
  group_by(PatientNr,groep,StatusDBC) %>%
  mutate(g = as.numeric(row_number()==1) + as.numeric(cummax(lag(as.numeric(SluitingsDatumDBC+1),default=first(as.numeric(SluitingsDatumDBC+1))))<as.numeric(OpeningsDatumDBC))) %>%
  ungroup() %>%
  mutate(g = cumsum(g)) %>%
  group_by(g) %>%
  summarise(
    PatientNr         = first(PatientNr),
    groep             = first(groep),
    StatusDBC         = first(StatusDBC),
    OpeningsDatumDBC  = min(OpeningsDatumDBC),
    SluitingsDatumDBC = max(SluitingsDatumDBC)) %>%
  ungroup() %>%
  select(-g)  %>%
  filter(StatusDBC==0 | StatusDBC==1)   # verwijder ongeldige dbc's 


# LG: toegevoegd (vanaf cyclus 8); als dbc's niet volledig aansloten --> fu stopte, nu mogen er 30 lege dagen tussen zitten
Subtraject1 <- Subtraject1%>%
  group_by(PatientNr, groep, StatusDBC)%>%
  arrange(PatientNr, OpeningsDatumDBC)%>%
  mutate(dagen_tussen = as.numeric(lead(OpeningsDatumDBC)-SluitingsDatumDBC))%>%
  mutate(SluitingsDatumDBC= ifelse(lead(groep)==groep &dagen_tussen<30 &!is.na(dagen_tussen),lead(SluitingsDatumDBC), SluitingsDatumDBC))%>%
  mutate(SluitingsDatumDBC=as.Date(SluitingsDatumDBC,origin="1970-01-01"))

Subtraject1<- Subtraject1%>%
  group_by(PatientNr, groep, SluitingsDatumDBC, StatusDBC)%>%
  summarise(      OpeningsDatumDBC  = min(OpeningsDatumDBC))%>%
  ungroup() 
#######################################################################################
################################ nierschade groep ######################################


### Haal lege DBCs >360 dagen op het einde eruit 

nierschade <- Subtraject1 %>%
  arrange(PatientNr,groep,OpeningsDatumDBC,SluitingsDatumDBC) %>%
  mutate(leegduur = ifelse(groep==1 & StatusDBC==1,SluitingsDatumDBC - OpeningsDatumDBC + 1,NA))

rm(Subtraject, Subtraject1)

# bepaal of de lege DBC de laatste regel is binnen patient en groep
nierschade <- nierschade %>%
  arrange(PatientNr,groep,OpeningsDatumDBC,StatusDBC) %>%
  group_by(PatientNr,groep) %>%
  mutate(seq = row_number()) %>%
  mutate(revseq = max(row_number())-row_number()+1)%>%
  mutate(minleeg=min(StatusDBC))%>%
  mutate(maxleeg=max(StatusDBC))



nierschade <- nierschade %>%
  filter(!(leegduur>=360 & groep==1 & StatusDBC==1 & revseq==1))%>%
  filter(!(groep==1 & StatusDBC==1 & revseq==1 & minleeg==1 & maxleeg==1))%>%
  select(-seq,-revseq,-leegduur)



### Samenvoegen opvolgende regels, per behandelgroep, ongeacht status leeg

nierschade <- nierschade%>%
  arrange(PatientNr,groep,OpeningsDatumDBC,SluitingsDatumDBC) %>%
  group_by(PatientNr,groep) %>%
  mutate(g = as.numeric(row_number()==1) + as.numeric(cummax(lag(as.numeric(SluitingsDatumDBC+1),default=first(as.numeric(SluitingsDatumDBC+1))))<as.numeric(OpeningsDatumDBC))) %>%
  ungroup() %>%
  mutate(g = cumsum(g)) %>%
  group_by(g) %>%
  summarise(
    PatientNr  = first(PatientNr),
    groep          = first(groep),
    OpeningsDatumDBC = min(OpeningsDatumDBC),
    SluitingsDatumDBC  = max(SluitingsDatumDBC)) %>%
  ungroup() %>%
  select(-g)  


# NIEUW IN C9: # Toevoegen daadwerkelijke transplantatiedatums 

nierschade <- left_join(nierschade,Tx_lijst)  


### Bepaal de follow-up duur van de nierschade groep. Follow-up stopt op het moment dat er een andere behandelgroep start.

#Situatie 1: Een andere behandeling start, terwijl nierschade doorloopt. Zet einddatum nierschade op dag voor start behandeling

nierschade1 <- nierschade %>%
  arrange(PatientNr, OpeningsDatumDBC, groep, SluitingsDatumDBC)%>%
  mutate(check = ifelse(groep==1 & lead(PatientNr)==PatientNr & lead(groep)!=groep & lead(OpeningsDatumDBC)<=SluitingsDatumDBC,1,0)) %>%
  mutate(check = ifelse(max(row_number())-row_number()==0,0,check)) %>%
  mutate(SluitingsDatumDBC = if_else(check==1,lead(OpeningsDatumDBC)-1,SluitingsDatumDBC)) %>%
  mutate(SluitingsDatumDBC = as.Date(SluitingsDatumDBC,origin="1970-01-01"))

# LG: toegevoegd C9 o.b.v. transplantatielijsten ipv dbc 076/078

nierschade1 <- nierschade1 %>%    
  mutate(SluitingsDatumDBC = ifelse(Tx_datum<SluitingsDatumDBC & !is.na(Tx_datum), Tx_datum - 1, SluitingsDatumDBC))%>%
  mutate(SluitingsDatumDBC = as.Date(SluitingsDatumDBC,origin="1970-01-01"))%>%
  filter(!(OpeningsDatumDBC>SluitingsDatumDBC)) %>%
  select(-check)


#Situatie 2: Een andere behandeling was al gestart op het moment dat nierschade start. Verwijder nierschade traject

nierschade1 <- nierschade1 %>%
  mutate(tmpcheck = ifelse(groep==1 & lag(PatientNr)==PatientNr & lag(groep)!=groep & lag(OpeningsDatumDBC)<=OpeningsDatumDBC,1,0)) %>%
  mutate(tmpcheck = ifelse(row_number()==1,0,tmpcheck))

temp <- nierschade1[,c("PatientNr","tmpcheck")]

temp <- temp %>%
  group_by(PatientNr) %>%
  summarise(check = sum(tmpcheck)) %>%
  mutate(check = as.numeric(check>0))

temp <- data.frame(temp)

nierschade1 <- nierschade1 %>%
  left_join(temp,by="PatientNr") %>%
  mutate(check= ifelse(check==1 & lag(PatientNr)!=PatientNr,0,check))%>%  #HIPS: indien eerste regel 324/325 is, meenemen
  filter(check==0) %>%
  select(-tmpcheck,-check)

rm(temp)


### Gooi de andere groepen eruit

nierschade1 <- nierschade1 %>%
  filter(groep==1)


### Selecteer het eerste traject

nierschade1 <- nierschade1 %>%
  arrange(PatientNr,OpeningsDatumDBC) %>%
  group_by(PatientNr) %>%
  mutate(seq = row_number()) %>%
  filter(seq == 1) %>%
  select(-seq)

# final selectie
nierschade <- left_join(nierschade1, Patient)%>%
  mutate(SluitingsDatumDBC = ifelse(!is.na(OverlijdensDatum) & SluitingsDatumDBC> OverlijdensDatum, OverlijdensDatum, SluitingsDatumDBC))%>%
  mutate(SluitingsDatumDBC=as.Date(SluitingsDatumDBC, origin="1970-01-01"))%>%
  filter(OpeningsDatumDBC<=stopwindow & SluitingsDatumDBC>=startwindow)

rm(nierschade1, Patient)



#### overige data inladen 

dir_patselectie = "K:/SHAREDIR/Data Santeon projecten/scorekaarten/nierfalen/Cyclus 11/Bron"
Verrichting = "verrichting_u8.csv"
Opnames = "Opnames.csv"
GFR = "gfr.csv"

# inladen Excel data 
setwd(dir_patselectie) ## hier het pad waar je patientenselectie staat
Opnames <- read.csv(Opnames, header=TRUE, sep=";",stringsAsFactors=FALSE) 
Verrichting<- read.csv(Verrichting, header=TRUE, sep=";",stringsAsFactors=FALSE) 
GFR<- read.csv(GFR, header=TRUE, sep=";",stringsAsFactors=FALSE) 



Verrichting <- as.data.frame(Verrichting)%>%
  transmute(PatientNr=as.character(PatientNr),
            VerrichtingDatum = as.Date(Verrichtingdatum, format="%Y-%m-%d"),
            ZACode=as.character(ZACode),
            OpnameNr=as.numeric(OpnameNr),
            Specialisme=as.character(Specialisme), #"INT", "CAR" etc
            AGB_CodeUitvoerder = as.character(AGB_CodeUitvoerder),
            Aantal=as.numeric(Aantal))


Opnames <- as.data.frame(Opnames)%>%
  transmute(PatientNr=as.character(PatientNr),
            OpnameDatum = as.Date(OpnameDatumTijd, format="%Y-%m-%d"),
            OntslagDatum = as.Date(OntslagDatumTijd, format="%Y-%m-%d"))


GFR <- as.data.frame(GFR)%>%
  filter(BepalingOms=="GFR (CKD-EPI)" | BepalingOms=="GFR-EPI")%>%  # kijk hoe GFR waardes in eigen Lab staan omschreven, dit ivm lokale codes voor GFR labwaardes
  transmute(PatientNr=as.character(PatientNr),
            Datum = as.Date(AfnameDatumTijd, format="%Y-%m-%d"),
            Uitslag= ifelse(Uitslag==">90",90,Uitslag),
            Uitslag=as.numeric(Uitslag))


########## Voorbereiding analyse linear mixed model  #############


# Excludeer GFR waardes afgenomen tijdens SEH of klinische opname
Verrichting <- Verrichting%>%
  filter(ZACode=="190021" | ZACode=="190015" | ZACode=="190016")

# koppel opnames aan verrichtingen (in MSZ worden de opnamenummers niet goed geregistreerd bij Verrichtingen)
temp <-  left_join(Verrichting, Opnames)%>%
  filter(ZACode=="190021" & VerrichtingDatum>=OpnameDatum-1 & VerrichtingDatum<=OntslagDatum)%>%
  arrange(PatientNr, OpnameDatum) %>% 
  distinct(PatientNr, OpnameDatum,.keep_all=TRUE)

Verrichting1 <- left_join(Verrichting, temp, by=c("PatientNr", "VerrichtingDatum", "ZACode"))


# koppel aan GFR waardes
GFR1 <- left_join(GFR, Verrichting1, by="PatientNr")%>%
  mutate(excludeer=ifelse(Datum==VerrichtingDatum | (Datum>=OpnameDatum & Datum<=OntslagDatum),1,0))%>%
  arrange(PatientNr, Datum, desc(excludeer))%>%  
  group_by(PatientNr, Datum)%>%
  summarise(Uitslag=min(Uitslag), excludeer=first(excludeer)) %>%  # indien op een bepaalde datum inimaal 1x gelijk aan SEh datum of klinische opname, dan staat de 1 bovenaan --> excluderen
  filter(excludeer==0 | is.na(excludeer))%>%
  select(-excludeer)

rm(GFR)







########## Analyse linear mixed model  #############
datalong <- left_join(nierschade, GFR1)%>%
  select(PatientNr, OpeningsDatumDBC, SluitingsDatumDBC, Datum, Uitslag) %>%
  filter(!is.na(Uitslag) & !is.na(Datum)) %>%
  filter(Datum>=OpeningsDatumDBC & Datum<=SluitingsDatumDBC) %>%
  mutate(dagen = as.numeric(Datum-OpeningsDatumDBC)) %>%
  mutate(jaar = dagen/365.25) 

# minimaal 2 GFR bepalingen, waarbij de eerste en laatst bepaalde GFR meting minimaal 90 dagen uit elkaar moet liggen
temp1 <- datalong%>%
  group_by(PatientNr)%>%
  summarise(eerste_datum = min(Datum), laatste_datum=max(Datum))  %>%
  mutate(dagen_tussen_lab = as.numeric(laatste_datum - eerste_datum))%>%
  filter(dagen_tussen_lab >=90)%>%
  select(PatientNr)

datalong1 <- left_join(temp1, datalong)

datalong <- data.frame(datalong1)

rm(datalong1)

### Model 

fit <- lmer(Uitslag ~ 1 + jaar + (1 + jaar | PatientNr),
            data = datalong)

summary(fit)





### Create data frame with coordinates for the individual predictions (based on the linear mixed model)


predicted <- datalong %>%
  select(PatientNr,dagen) %>%
  group_by(PatientNr) %>%
  mutate(min_dagen = min(dagen), max_dagen = max(dagen)) %>%
  filter(row_number()==1)

# Calculate individual coefficients based on the BLUPS and coefficients of the linear mixed model
blups <- ranef(fit)$PatientNr
coefs <- blups + cbind(rep(summary(fit)$coef["(Intercept)","Estimate"],dim(blups)[1]),rep(summary(fit)$coef["jaar","Estimate"],dim(blups)[1]))

coefs$PatientNr <- rownames(coefs)
colnames(coefs) <- c("(Intercept)","coef_slope","PatientNr")

# Add invidual coefficients to data frame
predicted <- merge(predicted,coefs,by="PatientNr")

rm(coefs,blups)




### Plot 

par(mfrow=c(1,1))

# Start with an empty plot, set limits, main title, and axis titles
plot(0~1,
     type="n",
     xaxt="n",
     yaxt="n",
     xlim= c(0,365.25*5),
     ylim= c(0,100),
     xlab="Jaren sinds start predialyse",
     ylab="eGFR",
     main="GFR beloop predialyse")

# Add axis labels
axis(1,at=seq(0,365.25*5,365.25), labels=0:5)
axis(2,at=seq(0,100,20), labels=seq(0,100,20))


for(i in 1:dim(predicted)[1]) { #Loop over all patients
  
  # Plot measurements (dots)
  points(datalong[which(datalong$PatientNr==predicted$PatientNr[i]),"dagen"],datalong[which(datalong$PatientNr==predicted$PatientNr[i]),"Uitslag"],col="grey40")
  
  # Add predicted curve
  eq <- function(x) { ( predicted[i,"(Intercept)"] + (predicted[i,"coef_slope"]/365.2) * x ) }
  curve(eq,from=predicted[i,"min_dagen"],to=predicted[i,"max_dagen"],col="grey40",lty=2,add=T)
  
}

# Add overall regression line
eq <- function(x) { ( summary(fit)$coef["(Intercept)","Estimate"] + (summary(fit)$coef["jaar","Estimate"]/365.2) * x ) }
curve(eq,from=min(predicted$min_dagen),to=max(predicted$max_dagen),col="red",lwd=4,add=T)


# summarise
U8 <- predicted %>%
  mutate(aantal=1)%>%
  summarise(teller=sum(coef_slope), noemer=sum(aantal), mediaan=median(coef_slope),min=min(coef_slope), p5=quantile(coef_slope,c(0.05),na.rm = TRUE), q1=quantile(coef_slope,c(0.25),na.rm = TRUE), q3=quantile(coef_slope,c(0.75),na.rm = TRUE), p95=quantile(coef_slope,c(0.95),na.rm = TRUE), max=max(coef_slope))


