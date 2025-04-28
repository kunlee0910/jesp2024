####################################Setup datasets############################

library(readxl)
dataset60 <- read_excel("Data/masterdata.xlsx") # dataset for age 60-64 group
# The masterdata file includes all independent variables

library(tidyverse)
library(nlme)      # Estimation of mixed effects models
library(lme4)      # Alternative package for mixed effects models
library(plm)       # Econometrics package for linear panel models
library(arm)       # Gelman & Hill code for mixed effects simulation
library(pcse)      # Calculate PCSEs for LS models (Beck & Katz)
library(tseries)   # For ADF unit root test
library(lmtest)    # bgtest, bptest, coeftest... many tests for linear regressions
library(texreg)
library(ggplot2)
library(panelr)
library(sandwich)
library(boot)

# create group variable
dataset60$group <- paste0(dataset60$country, dataset60$sex, dataset60$edu)
unique(dataset60$group)
dataset60$group <- as.factor(dataset60$group)
dataset60 <- dataset60 %>% relocate(group, .before=year)

#create countrylist
country00 <- paste0(unique(dataset60$country),"00")
country00 # only CY00 selectedd
countrylist <- unique(dataset60$country) # only CY code

# creating education dummies
dataset60$highedu <- as.numeric(dataset60$edu==2)
dataset60$midedu <- as.numeric(dataset60$edu==1)
dataset60$lowedu <- as.numeric(dataset60$edu==0)

# Pension regime variable
dataset60$penregime <- 0
dataset60$penregime[dataset60$country=="CZE" | dataset60$country=="USA" |
                      dataset60$country=="CHE" | dataset60$country=="NOR" |
                      dataset60$country=="BEL"] <- 1
dataset60$penregime[dataset60$country=="DNK" | dataset60$country=="NLD" | 
                      dataset60$country=="GBR" | dataset60$country=="IRL"] <- 2
dataset60$penregime <- factor(dataset60$penregime, levels = c(0,1,2),
                              labels = c("Bismarckian", "Mixed", "Beveridgean"))
dataset60$Bis <- as.numeric(dataset60$penregime=="Bismarckian")
dataset60$Mix <-as.numeric(dataset60$penregime=="Mixed")
dataset60$Bev <-as.numeric(dataset60$penregime=="Beveridgean")

# Alternative regime variable for robustness checks: Move Czechia to Beveridgean
dataset60$penregime1 <- 0
dataset60$penregime1[dataset60$country=="USA" | dataset60$country=="CHE" |
                      dataset60$country=="NOR" | dataset60$country=="BEL"] <- 1
dataset60$penregime1[dataset60$country=="CZE" | dataset60$country=="DNK" | dataset60$country=="NLD" | 
                      dataset60$country=="GBR" | dataset60$country=="IRL"] <- 2
dataset60$penregime1 <- factor(dataset60$penregime1, levels = c(0,1,2),
                              labels = c("Bismarckian", "Mixed", "Beveridgean"))
dataset60$Bis1 <- as.numeric(dataset60$penregime1=="Bismarckian")
dataset60$Mix1 <-as.numeric(dataset60$penregime1=="Mixed")
dataset60$Bev1 <-as.numeric(dataset60$penregime1=="Beveridgean")


# create pension spending per older person, unit: thousand dollars
dataset60$penexppc <- (dataset60$penexp + dataset60$earlyret) * dataset60$gdppc15 / dataset60$eldershare / 1000
dataset60 <- dataset60 %>% relocate(penexppc, .before=penexp)

dataset60$penexp <- dataset60$penexp + dataset60$earlyret

# lagged expenditure variables
dataset60$l.penexppc <- (dataset60$l.penexp + dataset60$l.earlyret) * dataset60$l.gdppc15 / dataset60$l.eldershare / 1000
dataset60 <- dataset60 %>% relocate(l.penexppc, .before=l.penexp)

dataset60$l.penexp <- dataset60$l.penexp + dataset60$l.earlyret

#log gdppc
dataset60$loggdppc <- log(dataset60$gdppc15)
dataset60 <- dataset60 %>% relocate(loggdppc, .after=gdppc15)

# other datasets
dataset65 <- dataset60 # create dataset for age 65-69 group
datasetpov <- dataset60[dataset60$edu==0 & dataset60$country!="USA",]
# dataset for poverty analysis: exclude usa due to lack of at-risk-of poverty data

# paste employment data
aut <- read.csv("Data/emp_aut.csv")
bel <- read.csv("Data/emp_bel.csv")
cze <- read.csv("Data/emp_cze.csv")
dnk <- read.csv("Data/emp_dnk.csv")
fin <- read.csv("Data/emp_fin.csv")
fra <- read.csv("Data/emp_fra.csv")
deu <- read.csv("Data/emp_deu.csv")
grc <- read.csv("Data/emp_grc.csv")
hun <- read.csv("Data/emp_hun.csv")
ire <- read.csv("Data/emp_ire.csv")
ita <- read.csv("Data/emp_ita.csv")
nld <- read.csv("Data/emp_nld.csv")
nor <- read.csv("Data/emp_nor.csv")
pol <- read.csv("Data/emp_pol.csv")
prt <- read.csv("Data/emp_prt.csv")
svk <- read.csv("Data/emp_svk.csv")
esp <- read.csv("Data/emp_esp.csv")
swe <- read.csv("Data/emp_swe.csv")
che <- read.csv("Data/emp_che.csv")
gbr <- read.csv("Data/emp_gbr.csv")
usa <- read.csv("Data/emp_usa.csv")
datalist <- list(aut, bel, cze, dnk, fin, fra, deu, grc, hun, ire, ita, nld, nor, pol, prt,
                 svk, esp, swe, che, gbr, usa)

for(i in 1:21){
  cntry <- countrylist[i]
  dat <- datalist[[i]]
  dataset60$emp[dataset60$country==cntry] <- 100*c(dat$mlowedu60, dat$mmidedu60, dat$mhighedu60,
                                                   dat$flowedu60, dat$fmidedu60, dat$fhighedu60)
}

for(i in 1:21){
  cntry <- countrylist[i]
  dat <- datalist[[i]]
  dataset65$emp[dataset65$country==cntry] <- 100*c(dat$mlowedu65, dat$mmidedu65, dat$mhighedu65,
                                                   dat$flowedu65, dat$fmidedu65, dat$fhighedu65)
}

rm(aut, bel, cze, dnk, fin, fra, deu, grc, hun, ire, ita, nld, nor, pol, prt,
   svk, esp, swe, che, gbr, usa)
rm(dat, datalist)


summary(dataset60$emp) # 45 missing
summary(dataset65$emp) # 84 missing

#linear interpolation of missing values in employment, 65-69
summary(dataset65$emp[dataset65$country=="BEL"])
cbind(dataset65$year[dataset65$country=="BEL"], dataset65$emp[dataset65$country=="BEL"])
dataset65$emp[dataset65$group=="BEL12" & dataset65$year==2000] <- (5.2944550 + 8.5942431)/2
dataset65$emp[dataset65$group=="BEL12" & dataset65$year==2003] <- (1.3016447 + 1.9608302)/2

summary(dataset65$emp[dataset65$country=="SVK"])
cbind(dataset65$year[dataset65$country=="SVK"], dataset65$emp[dataset65$country=="SVK"])
dataset65$emp[dataset65$group=="SVK00" & dataset65$year==2001] <- (0.9714735 + 0.4983417)/2
dataset65$emp[dataset65$group=="SVK00" & dataset65$year==2004] <- (0.4153718 + 0.1305513)/2
dataset65$emp[dataset65$group=="SVK10" & dataset65$year==2001] <- (0.4433034*2 + 0.4525570)/3
dataset65$emp[dataset65$group=="SVK10" & dataset65$year==2002] <- (0.4433034 + 2*0.4525570)/3

summary(dataset65$emp[dataset65$country=="GBR"])
cbind(dataset65$year[dataset65$country=="GBR"], dataset65$emp[dataset65$country=="GBR"])
dataset65$emp[dataset65$group=="GBR00" & dataset65$year==2011] <- (16.416488 + 19.936805)/2
dataset65$emp[dataset65$group=="GBR01" & dataset65$year==2011] <- (21.513551 + 21.294002)/2
dataset65$emp[dataset65$group=="GBR02" & dataset65$year==2011] <- (29.532761 + 29.712560)/2
dataset65$emp[dataset65$group=="GBR10" & dataset65$year==2011] <- (10.773156 + 9.593678)/2
dataset65$emp[dataset65$group=="GBR11" & dataset65$year==2011] <- (17.740276 + 19.150868)/2
dataset65$emp[dataset65$group=="GBR12" & dataset65$year==2011] <- (17.902483 + 18.545359)/2

# create plm dataset
pdataset60 <- pdata.frame(dataset60, index = c("group", "year"))
pvar(pdataset60)
pdataset65 <- pdata.frame(dataset65, index = c("group", "year"))
pvar(pdataset65)
pdatasetpov <- pdata.frame(datasetpov, index = c("group", "year"))
pvar(pdatasetpov)

grouplist <- unique(dataset60$group)

# select male/female only data
mdata60 <- dataset60[dataset60$sex==0,] # Drop UK data for within-country balace
fdata60 <- dataset60[dataset60$sex==1,]
pmdata60 <- pdata.frame(mdata60, index = c("group", "year"))
pfdata60 <- pdata.frame(fdata60, index = c("group", "year"))

mdata65 <- dataset65[dataset65$sex==0,]
fdata65 <- dataset65[dataset65$sex==1,]
pmdata65 <- pdata.frame(mdata65, index = c("group", "year"))
pfdata65 <- pdata.frame(fdata65, index = c("group", "year"))

mdatapov <- datasetpov[datasetpov$sex==0,]
fdatapov <- datasetpov[datasetpov$sex==1,]
pmdatapov <- pdata.frame(mdatapov, index = c("group", "year"))
pfdatapov <- pdata.frame(fdatapov, index = c("group", "year"))

# make group/year dummies
gdum <- make.dummies(dataset60$group)
ydum <- make.dummies(dataset60$year)[,-1]
mgdum <- make.dummies(mdata60$group)
fgdum <- make.dummies(fdata60$group)
mydum <- make.dummies(mdata60$year)[,-1]
fydum <- make.dummies(fdata60$year)[,-1]

gdumpov <- make.dummies(datasetpov$group)
ydumpov <- make.dummies(datasetpov$year)[,-1]
mgdumpov <- make.dummies(mdatapov$group)
fgdumpov <- make.dummies(fdatapov$group)
mydumpov <- make.dummies(mdatapov$year)[,-1]
fydumpov <- make.dummies(fdatapov$year)[,-1]

