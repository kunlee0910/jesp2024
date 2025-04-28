############AUT employment rates by age groups, sex, level of education##############

###Austria: AUT####
# Load yearly data
aut98 <- read.csv("AT_YEAR_1998_onwards/AT1998_y.csv")
aut99 <- read.csv("AT_YEAR_1998_onwards/AT1999_y.csv")
aut00 <- read.csv("AT_YEAR_1998_onwards/AT2000_y.csv")
aut01 <- read.csv("AT_YEAR_1998_onwards/AT2001_y.csv")
aut02 <- read.csv("AT_YEAR_1998_onwards/AT2002_y.csv")
aut03 <- read.csv("AT_YEAR_1998_onwards/AT2003_y.csv")
aut04 <- read.csv("AT_YEAR_1998_onwards/AT2004_y.csv")
aut05 <- read.csv("AT_YEAR_1998_onwards/AT2005_y.csv")
aut06 <- read.csv("AT_YEAR_1998_onwards/AT2006_y.csv")
aut07 <- read.csv("AT_YEAR_1998_onwards/AT2007_y.csv")
aut08 <- read.csv("AT_YEAR_1998_onwards/AT2008_y.csv")
aut09 <- read.csv("AT_YEAR_1998_onwards/AT2009_y.csv")
aut10 <- read.csv("AT_YEAR_1998_onwards/AT2010_y.csv")
aut11 <- read.csv("AT_YEAR_1998_onwards/AT2011_y.csv")
aut12 <- read.csv("AT_YEAR_1998_onwards/AT2012_y.csv")
aut13 <- read.csv("AT_YEAR_1998_onwards/AT2013_y.csv")
aut14 <- read.csv("AT_YEAR_1998_onwards/AT2014_y.csv")
aut15 <- read.csv("AT_YEAR_1998_onwards/AT2015_y.csv")
aut16 <- read.csv("AT_YEAR_1998_onwards/AT2016_y.csv")
aut17 <- read.csv("AT_YEAR_1998_onwards/AT2017_y.csv")
aut18 <- read.csv("AT_YEAR_1998_onwards/AT2018_y.csv")
aut19 <- read.csv("AT_YEAR_1998_onwards/AT2019_y.csv")

# Merge yearly data
autdata <- rbind(aut98, aut99, aut00, aut01, aut02, aut03, aut04, aut05, aut06, aut07, aut08,
                 aut09, aut10, aut11, aut12, aut13, aut14, aut15, aut16, aut17, aut18, aut19)
rm(aut98, aut99, aut00, aut01, aut02, aut03, aut04, aut05, aut06, aut07, aut08,
   aut09, aut10, aut11, aut12, aut13, aut14, aut15, aut16, aut17, aut18, aut19)

# select relevant variables
aut <- autdata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
aut <- aut[aut$AGE>59 & aut$AGE<70, ] # select age
aut$EMP <- 0 # employment variable
aut$EMP[aut$ILOSTAT==1] <- 1
aut$weight <- 1/aut$COEFF

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_aut <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_aut <- as.data.frame(emp_aut)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==1 & aut$HATLEV1D=="L" & aut$AGE==62 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==1 & aut$HATLEV1D=="L" & aut$AGE==62 & aut$YEAR==y])
  emp_aut$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==1 & aut$HATLEV1D=="M" & aut$AGE==62 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==1 & aut$HATLEV1D=="M" & aut$AGE==62 & aut$YEAR==y])
  emp_aut$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==1 & aut$HATLEV1D=="H" & aut$AGE==62 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==1 & aut$HATLEV1D=="H" & aut$AGE==62 & aut$YEAR==y])
  emp_aut$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==2 & aut$HATLEV1D=="L" & aut$AGE==62 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==2 & aut$HATLEV1D=="L" & aut$AGE==62 & aut$YEAR==y])
  emp_aut$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==2 & aut$HATLEV1D=="M" & aut$AGE==62 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==2 & aut$HATLEV1D=="M" & aut$AGE==62 & aut$YEAR==y])
  emp_aut$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==2 & aut$HATLEV1D=="H" & aut$AGE==62 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==2 & aut$HATLEV1D=="H" & aut$AGE==62 & aut$YEAR==y])
  emp_aut$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==1 & aut$HATLEV1D=="L" & aut$AGE==67 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==1 & aut$HATLEV1D=="L" & aut$AGE==67 & aut$YEAR==y])
  emp_aut$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==1 & aut$HATLEV1D=="M" & aut$AGE==67 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==1 & aut$HATLEV1D=="M" & aut$AGE==67 & aut$YEAR==y])
  emp_aut$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==1 & aut$HATLEV1D=="H" & aut$AGE==67 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==1 & aut$HATLEV1D=="H" & aut$AGE==67 & aut$YEAR==y])
  emp_aut$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==2 & aut$HATLEV1D=="L" & aut$AGE==67 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==2 & aut$HATLEV1D=="L" & aut$AGE==67 & aut$YEAR==y])
  emp_aut$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==2 & aut$HATLEV1D=="M" & aut$AGE==67 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==2 & aut$HATLEV1D=="M" & aut$AGE==67 & aut$YEAR==y])
  emp_aut$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(aut$EMP[aut$SEX==2 & aut$HATLEV1D=="H" & aut$AGE==67 & aut$YEAR==y],
                          w = aut$weight[aut$SEX==2 & aut$HATLEV1D=="H" & aut$AGE==67 & aut$YEAR==y])
  emp_aut$fhighedu65 <- emp
}

write.csv(emp_aut, file = "Data/emp_aut.csv")


###Belgium: BE#######
# Load yearly data
bel98 <- read.csv("BE_YEAR_1998_onwards/BE1998_y.csv")
bel99 <- read.csv("BE_YEAR_1998_onwards/BE1999_y.csv")
bel00 <- read.csv("BE_YEAR_1998_onwards/BE2000_y.csv")
bel01 <- read.csv("BE_YEAR_1998_onwards/BE2001_y.csv")
bel02 <- read.csv("BE_YEAR_1998_onwards/BE2002_y.csv")
bel03 <- read.csv("BE_YEAR_1998_onwards/BE2003_y.csv")
bel04 <- read.csv("BE_YEAR_1998_onwards/BE2004_y.csv")
bel05 <- read.csv("BE_YEAR_1998_onwards/BE2005_y.csv")
bel06 <- read.csv("BE_YEAR_1998_onwards/BE2006_y.csv")
bel07 <- read.csv("BE_YEAR_1998_onwards/BE2007_y.csv")
bel08 <- read.csv("BE_YEAR_1998_onwards/BE2008_y.csv")
bel09 <- read.csv("BE_YEAR_1998_onwards/BE2009_y.csv")
bel10 <- read.csv("BE_YEAR_1998_onwards/BE2010_y.csv")
bel11 <- read.csv("BE_YEAR_1998_onwards/BE2011_y.csv")
bel12 <- read.csv("BE_YEAR_1998_onwards/BE2012_y.csv")
bel13 <- read.csv("BE_YEAR_1998_onwards/BE2013_y.csv")
bel14 <- read.csv("BE_YEAR_1998_onwards/BE2014_y.csv")
bel15 <- read.csv("BE_YEAR_1998_onwards/BE2015_y.csv")
bel16 <- read.csv("BE_YEAR_1998_onwards/BE2016_y.csv")
bel17 <- read.csv("BE_YEAR_1998_onwards/BE2017_y.csv")
bel18 <- read.csv("BE_YEAR_1998_onwards/BE2018_y.csv")
bel19 <- read.csv("BE_YEAR_1998_onwards/BE2019_y.csv")

# Merge yearly data
beldata <- rbind(bel98, bel99, bel00, bel01, bel02, bel03, bel04, bel05, bel06, bel07, bel08,
                 bel09, bel10, bel11, bel12, bel13, bel14, bel15, bel16, bel17, bel18, bel19)
rm(bel98, bel99, bel00, bel01, bel02, bel03, bel04, bel05, bel06, bel07, bel08,
   bel09, bel10, bel11, bel12, bel13, bel14, bel15, bel16, bel17, bel18, bel19)

# select relevant variables
bel <- beldata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
bel <- bel[bel$AGE>59 & bel$AGE<70, ] # select age
bel$EMP <- 0 # employment variable
bel$EMP[bel$ILOSTAT==1] <- 1
bel$weight <- 1/bel$COEFF
summary(bel)

bel <- bel[bel$weight<1000, ]

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_bel <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_bel <- as.data.frame(emp_bel)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==1 & bel$HATLEV1D=="L" & bel$AGE==62 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==1 & bel$HATLEV1D=="L" & bel$AGE==62 & bel$YEAR==y])
  emp_bel$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==1 & bel$HATLEV1D=="M" & bel$AGE==62 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==1 & bel$HATLEV1D=="M" & bel$AGE==62 & bel$YEAR==y])
  emp_bel$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==1 & bel$HATLEV1D=="H" & bel$AGE==62 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==1 & bel$HATLEV1D=="H" & bel$AGE==62 & bel$YEAR==y])
  emp_bel$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==2 & bel$HATLEV1D=="L" & bel$AGE==62 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==2 & bel$HATLEV1D=="L" & bel$AGE==62 & bel$YEAR==y])
  emp_bel$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==2 & bel$HATLEV1D=="M" & bel$AGE==62 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==2 & bel$HATLEV1D=="M" & bel$AGE==62 & bel$YEAR==y])
  emp_bel$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==2 & bel$HATLEV1D=="H" & bel$AGE==62 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==2 & bel$HATLEV1D=="H" & bel$AGE==62 & bel$YEAR==y])
  emp_bel$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==1 & bel$HATLEV1D=="L" & bel$AGE==67 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==1 & bel$HATLEV1D=="L" & bel$AGE==67 & bel$YEAR==y])
  emp_bel$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==1 & bel$HATLEV1D=="M" & bel$AGE==67 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==1 & bel$HATLEV1D=="M" & bel$AGE==67 & bel$YEAR==y])
  emp_bel$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==1 & bel$HATLEV1D=="H" & bel$AGE==67 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==1 & bel$HATLEV1D=="H" & bel$AGE==67 & bel$YEAR==y])
  emp_bel$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==2 & bel$HATLEV1D=="L" & bel$AGE==67 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==2 & bel$HATLEV1D=="L" & bel$AGE==67 & bel$YEAR==y])
  emp_bel$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==2 & bel$HATLEV1D=="M" & bel$AGE==67 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==2 & bel$HATLEV1D=="M" & bel$AGE==67 & bel$YEAR==y])
  emp_bel$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(bel$EMP[bel$SEX==2 & bel$HATLEV1D=="H" & bel$AGE==67 & bel$YEAR==y],
                          w = bel$weight[bel$SEX==2 & bel$HATLEV1D=="H" & bel$AGE==67 & bel$YEAR==y])
  emp_bel$fhighedu65 <- emp
}

emp_bel$fhighedu65[emp_bel$fhighedu65==0] <- NA

# no employed person in female, 67, high-edu group in 2000 & 2003 --> treated as missing data
write.csv(emp_bel, file = "Data/emp_bel.csv")
rm(list = ls())

###Czechia: CZE#######
# Load yearly data
cze98 <- read.csv("CZ_YEAR_1998_onwards/CZ1998_y.csv")
cze99 <- read.csv("CZ_YEAR_1998_onwards/CZ1999_y.csv")
cze00 <- read.csv("CZ_YEAR_1998_onwards/CZ2000_y.csv")
cze01 <- read.csv("CZ_YEAR_1998_onwards/CZ2001_y.csv")
cze02 <- read.csv("CZ_YEAR_1998_onwards/CZ2002_y.csv")
cze03 <- read.csv("CZ_YEAR_1998_onwards/CZ2003_y.csv")
cze04 <- read.csv("CZ_YEAR_1998_onwards/CZ2004_y.csv")
cze05 <- read.csv("CZ_YEAR_1998_onwards/CZ2005_y.csv")
cze06 <- read.csv("CZ_YEAR_1998_onwards/CZ2006_y.csv")
cze07 <- read.csv("CZ_YEAR_1998_onwards/CZ2007_y.csv")
cze08 <- read.csv("CZ_YEAR_1998_onwards/CZ2008_y.csv")
cze09 <- read.csv("CZ_YEAR_1998_onwards/CZ2009_y.csv")
cze10 <- read.csv("CZ_YEAR_1998_onwards/CZ2010_y.csv")
cze11 <- read.csv("CZ_YEAR_1998_onwards/CZ2011_y.csv")
cze12 <- read.csv("CZ_YEAR_1998_onwards/CZ2012_y.csv")
cze13 <- read.csv("CZ_YEAR_1998_onwards/CZ2013_y.csv")
cze14 <- read.csv("CZ_YEAR_1998_onwards/CZ2014_y.csv")
cze15 <- read.csv("CZ_YEAR_1998_onwards/CZ2015_y.csv")
cze16 <- read.csv("CZ_YEAR_1998_onwards/CZ2016_y.csv")
cze17 <- read.csv("CZ_YEAR_1998_onwards/CZ2017_y.csv")
cze18 <- read.csv("CZ_YEAR_1998_onwards/CZ2018_y.csv")
cze19 <- read.csv("CZ_YEAR_1998_onwards/CZ2019_y.csv")

# Merge yearly data
czedata <- rbind(cze98, cze99, cze00, cze01, cze02, cze03, cze04, cze05, cze06, cze07, cze08,
                 cze09, cze10, cze11, cze12, cze13, cze14, cze15, cze16, cze17, cze18, cze19)
rm(cze98, cze99, cze00, cze01, cze02, cze03, cze04, cze05, cze06, cze07, cze08,
   cze09, cze10, cze11, cze12, cze13, cze14, cze15, cze16, cze17, cze18, cze19)

# select relevant variables
cze <- czedata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
cze <- cze[cze$AGE>59 & cze$AGE<70, ] # select age
cze$EMP <- 0 # employment variable
cze$EMP[cze$ILOSTAT==1] <- 1
cze$weight <- 1/cze$COEFF

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_cze <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_cze <- as.data.frame(emp_cze)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==1 & cze$HATLEV1D=="L" & cze$AGE==62 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==1 & cze$HATLEV1D=="L" & cze$AGE==62 & cze$YEAR==y])
  emp_cze$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==1 & cze$HATLEV1D=="M" & cze$AGE==62 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==1 & cze$HATLEV1D=="M" & cze$AGE==62 & cze$YEAR==y])
  emp_cze$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==1 & cze$HATLEV1D=="H" & cze$AGE==62 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==1 & cze$HATLEV1D=="H" & cze$AGE==62 & cze$YEAR==y])
  emp_cze$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==2 & cze$HATLEV1D=="L" & cze$AGE==62 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==2 & cze$HATLEV1D=="L" & cze$AGE==62 & cze$YEAR==y])
  emp_cze$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==2 & cze$HATLEV1D=="M" & cze$AGE==62 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==2 & cze$HATLEV1D=="M" & cze$AGE==62 & cze$YEAR==y])
  emp_cze$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==2 & cze$HATLEV1D=="H" & cze$AGE==62 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==2 & cze$HATLEV1D=="H" & cze$AGE==62 & cze$YEAR==y])
  emp_cze$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==1 & cze$HATLEV1D=="L" & cze$AGE==67 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==1 & cze$HATLEV1D=="L" & cze$AGE==67 & cze$YEAR==y])
  emp_cze$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==1 & cze$HATLEV1D=="M" & cze$AGE==67 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==1 & cze$HATLEV1D=="M" & cze$AGE==67 & cze$YEAR==y])
  emp_cze$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==1 & cze$HATLEV1D=="H" & cze$AGE==67 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==1 & cze$HATLEV1D=="H" & cze$AGE==67 & cze$YEAR==y])
  emp_cze$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==2 & cze$HATLEV1D=="L" & cze$AGE==67 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==2 & cze$HATLEV1D=="L" & cze$AGE==67 & cze$YEAR==y])
  emp_cze$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==2 & cze$HATLEV1D=="M" & cze$AGE==67 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==2 & cze$HATLEV1D=="M" & cze$AGE==67 & cze$YEAR==y])
  emp_cze$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cze$EMP[cze$SEX==2 & cze$HATLEV1D=="H" & cze$AGE==67 & cze$YEAR==y],
                          w = cze$weight[cze$SEX==2 & cze$HATLEV1D=="H" & cze$AGE==67 & cze$YEAR==y])
  emp_cze$fhighedu65 <- emp
}

write.csv(emp_cze, file = "Data/emp_cze.csv")
rm(list = ls())


###Denmark: DNK#######
# Load yearly data
dnk98 <- read.csv("DK_YEAR_1998_onwards/DK1998_y.csv")
dnk99 <- read.csv("DK_YEAR_1998_onwards/DK1999_y.csv")
dnk00 <- read.csv("DK_YEAR_1998_onwards/DK2000_y.csv")
dnk01 <- read.csv("DK_YEAR_1998_onwards/DK2001_y.csv")
dnk02 <- read.csv("DK_YEAR_1998_onwards/DK2002_y.csv")
dnk03 <- read.csv("DK_YEAR_1998_onwards/DK2003_y.csv")
dnk04 <- read.csv("DK_YEAR_1998_onwards/DK2004_y.csv")
dnk05 <- read.csv("DK_YEAR_1998_onwards/DK2005_y.csv")
dnk06 <- read.csv("DK_YEAR_1998_onwards/DK2006_y.csv")
dnk07 <- read.csv("DK_YEAR_1998_onwards/DK2007_y.csv")
dnk08 <- read.csv("DK_YEAR_1998_onwards/DK2008_y.csv")
dnk09 <- read.csv("DK_YEAR_1998_onwards/DK2009_y.csv")
dnk10 <- read.csv("DK_YEAR_1998_onwards/DK2010_y.csv")
dnk11 <- read.csv("DK_YEAR_1998_onwards/DK2011_y.csv")
dnk12 <- read.csv("DK_YEAR_1998_onwards/DK2012_y.csv")
dnk13 <- read.csv("DK_YEAR_1998_onwards/DK2013_y.csv")
dnk14 <- read.csv("DK_YEAR_1998_onwards/DK2014_y.csv")
dnk15 <- read.csv("DK_YEAR_1998_onwards/DK2015_y.csv")
dnk16 <- read.csv("DK_YEAR_1998_onwards/DK2016_y.csv")
dnk17 <- read.csv("DK_YEAR_1998_onwards/DK2017_y.csv")
dnk18 <- read.csv("DK_YEAR_1998_onwards/DK2018_y.csv")
dnk19 <- read.csv("DK_YEAR_1998_onwards/DK2019_y.csv")

# Merge yearly data
dnkdata <- rbind(dnk98, dnk99, dnk00, dnk01, dnk02, dnk03, dnk04, dnk05, dnk06, dnk07, dnk08,
                 dnk09, dnk10, dnk11, dnk12, dnk13, dnk14, dnk15, dnk16, dnk17, dnk18, dnk19)
rm(dnk98, dnk99, dnk00, dnk01, dnk02, dnk03, dnk04, dnk05, dnk06, dnk07, dnk08,
   dnk09, dnk10, dnk11, dnk12, dnk13, dnk14, dnk15, dnk16, dnk17, dnk18, dnk19)

# select relevant variables
dnk <- dnkdata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
dnk <- dnk[dnk$AGE>59 & dnk$AGE<70, ] # select age
dnk$EMP <- 0 # employment variable
dnk$EMP[dnk$ILOSTAT==1] <- 1
dnk$weight <- 1/dnk$COEFF

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_dnk <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_dnk <- as.data.frame(emp_dnk)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==1 & dnk$HATLEV1D=="L" & dnk$AGE==62 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==1 & dnk$HATLEV1D=="L" & dnk$AGE==62 & dnk$YEAR==y])
  emp_dnk$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==1 & dnk$HATLEV1D=="M" & dnk$AGE==62 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==1 & dnk$HATLEV1D=="M" & dnk$AGE==62 & dnk$YEAR==y])
  emp_dnk$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==1 & dnk$HATLEV1D=="H" & dnk$AGE==62 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==1 & dnk$HATLEV1D=="H" & dnk$AGE==62 & dnk$YEAR==y])
  emp_dnk$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==2 & dnk$HATLEV1D=="L" & dnk$AGE==62 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==2 & dnk$HATLEV1D=="L" & dnk$AGE==62 & dnk$YEAR==y])
  emp_dnk$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==2 & dnk$HATLEV1D=="M" & dnk$AGE==62 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==2 & dnk$HATLEV1D=="M" & dnk$AGE==62 & dnk$YEAR==y])
  emp_dnk$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==2 & dnk$HATLEV1D=="H" & dnk$AGE==62 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==2 & dnk$HATLEV1D=="H" & dnk$AGE==62 & dnk$YEAR==y])
  emp_dnk$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==1 & dnk$HATLEV1D=="L" & dnk$AGE==67 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==1 & dnk$HATLEV1D=="L" & dnk$AGE==67 & dnk$YEAR==y])
  emp_dnk$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==1 & dnk$HATLEV1D=="M" & dnk$AGE==67 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==1 & dnk$HATLEV1D=="M" & dnk$AGE==67 & dnk$YEAR==y])
  emp_dnk$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==1 & dnk$HATLEV1D=="H" & dnk$AGE==67 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==1 & dnk$HATLEV1D=="H" & dnk$AGE==67 & dnk$YEAR==y])
  emp_dnk$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==2 & dnk$HATLEV1D=="L" & dnk$AGE==67 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==2 & dnk$HATLEV1D=="L" & dnk$AGE==67 & dnk$YEAR==y])
  emp_dnk$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==2 & dnk$HATLEV1D=="M" & dnk$AGE==67 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==2 & dnk$HATLEV1D=="M" & dnk$AGE==67 & dnk$YEAR==y])
  emp_dnk$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(dnk$EMP[dnk$SEX==2 & dnk$HATLEV1D=="H" & dnk$AGE==67 & dnk$YEAR==y],
                          w = dnk$weight[dnk$SEX==2 & dnk$HATLEV1D=="H" & dnk$AGE==67 & dnk$YEAR==y])
  emp_dnk$fhighedu65 <- emp
}

write.csv(emp_dnk, file = "Data/emp_dnk.csv")
rm(list = ls())


###Germany: DEU#######
# Load yearly data
deu98 <- read.csv("DE_YEAR_1998_onwards/DE1998_y.csv")
deu99 <- read.csv("DE_YEAR_1998_onwards/DE1999_y.csv")
deu00 <- read.csv("DE_YEAR_1998_onwards/DE2000_y.csv")
deu01 <- read.csv("DE_YEAR_1998_onwards/DE2001_y.csv")
deu02 <- read.csv("DE_YEAR_1998_onwards/DE2002_y.csv")
deu03 <- read.csv("DE_YEAR_1998_onwards/DE2003_y.csv")
deu04 <- read.csv("DE_YEAR_1998_onwards/DE2004_y.csv")
deu05 <- read.csv("DE_YEAR_1998_onwards/DE2005_y.csv")
deu06 <- read.csv("DE_YEAR_1998_onwards/DE2006_y.csv")
deu07 <- read.csv("DE_YEAR_1998_onwards/DE2007_y.csv")
deu08 <- read.csv("DE_YEAR_1998_onwards/DE2008_y.csv")
deu09 <- read.csv("DE_YEAR_1998_onwards/DE2009_y.csv")
deu10 <- read.csv("DE_YEAR_1998_onwards/DE2010_y.csv")
deu11 <- read.csv("DE_YEAR_1998_onwards/DE2011_y.csv")
deu12 <- read.csv("DE_YEAR_1998_onwards/DE2012_y.csv")
deu13 <- read.csv("DE_YEAR_1998_onwards/DE2013_y.csv")
deu14 <- read.csv("DE_YEAR_1998_onwards/DE2014_y.csv")
deu15 <- read.csv("DE_YEAR_1998_onwards/DE2015_y.csv")
deu16 <- read.csv("DE_YEAR_1998_onwards/DE2016_y.csv")
deu17 <- read.csv("DE_YEAR_1998_onwards/DE2017_y.csv")
deu18 <- read.csv("DE_YEAR_1998_onwards/DE2018_y.csv")
deu19 <- read.csv("DE_YEAR_1998_onwards/DE2019_y.csv")

# Merge yearly data
deudata <- rbind(deu98, deu99, deu00, deu01, deu02, deu03, deu04, deu05, deu06, deu07, deu08,
                 deu09, deu10, deu11, deu12, deu13, deu14, deu15, deu16, deu17, deu18, deu19)
rm(deu98, deu99, deu00, deu01, deu02, deu03, deu04, deu05, deu06, deu07, deu08,
   deu09, deu10, deu11, deu12, deu13, deu14, deu15, deu16, deu17, deu18, deu19)

# select relevant variables
deu <- deudata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
deu <- deu[deu$AGE>59 & deu$AGE<70, ] # select age
deu$EMP <- 0 # employment variable
deu$EMP[deu$ILOSTAT==1] <- 1
deu$weight <- 1/deu$COEFF
# some values in the weighting factor are zero --> listwise deletion
deu <- deu[deu$COEFF!=0, ]

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_deu <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_deu <- as.data.frame(emp_deu)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==1 & deu$HATLEV1D=="L" & deu$AGE==62 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==1 & deu$HATLEV1D=="L" & deu$AGE==62 & deu$YEAR==y])
  emp_deu$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==1 & deu$HATLEV1D=="M" & deu$AGE==62 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==1 & deu$HATLEV1D=="M" & deu$AGE==62 & deu$YEAR==y])
  emp_deu$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==1 & deu$HATLEV1D=="H" & deu$AGE==62 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==1 & deu$HATLEV1D=="H" & deu$AGE==62 & deu$YEAR==y])
  emp_deu$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==2 & deu$HATLEV1D=="L" & deu$AGE==62 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==2 & deu$HATLEV1D=="L" & deu$AGE==62 & deu$YEAR==y])
  emp_deu$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==2 & deu$HATLEV1D=="M" & deu$AGE==62 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==2 & deu$HATLEV1D=="M" & deu$AGE==62 & deu$YEAR==y])
  emp_deu$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==2 & deu$HATLEV1D=="H" & deu$AGE==62 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==2 & deu$HATLEV1D=="H" & deu$AGE==62 & deu$YEAR==y])
  emp_deu$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==1 & deu$HATLEV1D=="L" & deu$AGE==67 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==1 & deu$HATLEV1D=="L" & deu$AGE==67 & deu$YEAR==y])
  emp_deu$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==1 & deu$HATLEV1D=="M" & deu$AGE==67 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==1 & deu$HATLEV1D=="M" & deu$AGE==67 & deu$YEAR==y])
  emp_deu$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==1 & deu$HATLEV1D=="H" & deu$AGE==67 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==1 & deu$HATLEV1D=="H" & deu$AGE==67 & deu$YEAR==y])
  emp_deu$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==2 & deu$HATLEV1D=="L" & deu$AGE==67 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==2 & deu$HATLEV1D=="L" & deu$AGE==67 & deu$YEAR==y])
  emp_deu$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==2 & deu$HATLEV1D=="M" & deu$AGE==67 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==2 & deu$HATLEV1D=="M" & deu$AGE==67 & deu$YEAR==y])
  emp_deu$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(deu$EMP[deu$SEX==2 & deu$HATLEV1D=="H" & deu$AGE==67 & deu$YEAR==y],
                          w = deu$weight[deu$SEX==2 & deu$HATLEV1D=="H" & deu$AGE==67 & deu$YEAR==y])
  emp_deu$fhighedu65 <- emp
}

write.csv(emp_deu, file = "Data/emp_deu.csv")
rm(list = ls())


###Switzerland: CHE#######
# Load yearly data
che98 <- read.csv("CH_YEAR_1998_onwards/CH1998_y.csv")
che99 <- read.csv("CH_YEAR_1998_onwards/CH1999_y.csv")
che00 <- read.csv("CH_YEAR_1998_onwards/CH2000_y.csv")
che01 <- read.csv("CH_YEAR_1998_onwards/CH2001_y.csv")
che02 <- read.csv("CH_YEAR_1998_onwards/CH2002_y.csv")
che03 <- read.csv("CH_YEAR_1998_onwards/CH2003_y.csv")
che04 <- read.csv("CH_YEAR_1998_onwards/CH2004_y.csv")
che05 <- read.csv("CH_YEAR_1998_onwards/CH2005_y.csv")
che06 <- read.csv("CH_YEAR_1998_onwards/CH2006_y.csv")
che07 <- read.csv("CH_YEAR_1998_onwards/CH2007_y.csv")
che08 <- read.csv("CH_YEAR_1998_onwards/CH2008_y.csv")
che09 <- read.csv("CH_YEAR_1998_onwards/CH2009_y.csv")
che10 <- read.csv("CH_YEAR_1998_onwards/CH2010_y.csv")
che11 <- read.csv("CH_YEAR_1998_onwards/CH2011_y.csv")
che12 <- read.csv("CH_YEAR_1998_onwards/CH2012_y.csv")
che13 <- read.csv("CH_YEAR_1998_onwards/CH2013_y.csv")
che14 <- read.csv("CH_YEAR_1998_onwards/CH2014_y.csv")
che15 <- read.csv("CH_YEAR_1998_onwards/CH2015_y.csv")
che16 <- read.csv("CH_YEAR_1998_onwards/CH2016_y.csv")
che17 <- read.csv("CH_YEAR_1998_onwards/CH2017_y.csv")
che18 <- read.csv("CH_YEAR_1998_onwards/CH2018_y.csv")
che19 <- read.csv("CH_YEAR_1998_onwards/CH2019_y.csv")

# Merge yearly data
chedata <- rbind(che98, che99, che00, che01, che02, che03, che04, che05, che06, che07, che08,
                 che09, che10, che11, che12, che13, che14, che15, che16, che17, che18, che19)
rm(che98, che99, che00, che01, che02, che03, che04, che05, che06, che07, che08,
   che09, che10, che11, che12, che13, che14, che15, che16, che17, che18, che19)

# select relevant variables
che <- chedata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
che <- che[che$AGE>59 & che$AGE<70, ] # select age
che$EMP <- 0 # employment variable
che$EMP[che$ILOSTAT==1] <- 1
che$weight <- 1/che$COEFF
che <- che[che$COEFF!=0, ]

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_che <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_che <- as.data.frame(emp_che)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(che$EMP[che$SEX==1 & che$HATLEV1D=="L" & che$AGE==62 & che$YEAR==y],
                          w = che$weight[che$SEX==1 & che$HATLEV1D=="L" & che$AGE==62 & che$YEAR==y])
  emp_che$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(che$EMP[che$SEX==1 & che$HATLEV1D=="M" & che$AGE==62 & che$YEAR==y],
                          w = che$weight[che$SEX==1 & che$HATLEV1D=="M" & che$AGE==62 & che$YEAR==y])
  emp_che$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(che$EMP[che$SEX==1 & che$HATLEV1D=="H" & che$AGE==62 & che$YEAR==y],
                          w = che$weight[che$SEX==1 & che$HATLEV1D=="H" & che$AGE==62 & che$YEAR==y])
  emp_che$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(che$EMP[che$SEX==2 & che$HATLEV1D=="L" & che$AGE==62 & che$YEAR==y],
                          w = che$weight[che$SEX==2 & che$HATLEV1D=="L" & che$AGE==62 & che$YEAR==y])
  emp_che$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(che$EMP[che$SEX==2 & che$HATLEV1D=="M" & che$AGE==62 & che$YEAR==y],
                          w = che$weight[che$SEX==2 & che$HATLEV1D=="M" & che$AGE==62 & che$YEAR==y])
  emp_che$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(che$EMP[che$SEX==2 & che$HATLEV1D=="H" & che$AGE==62 & che$YEAR==y],
                          w = che$weight[che$SEX==2 & che$HATLEV1D=="H" & che$AGE==62 & che$YEAR==y])
  emp_che$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(che$EMP[che$SEX==1 & che$HATLEV1D=="L" & che$AGE==67 & che$YEAR==y],
                          w = che$weight[che$SEX==1 & che$HATLEV1D=="L" & che$AGE==67 & che$YEAR==y])
  emp_che$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(che$EMP[che$SEX==1 & che$HATLEV1D=="M" & che$AGE==67 & che$YEAR==y],
                          w = che$weight[che$SEX==1 & che$HATLEV1D=="M" & che$AGE==67 & che$YEAR==y])
  emp_che$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(che$EMP[che$SEX==1 & che$HATLEV1D=="H" & che$AGE==67 & che$YEAR==y],
                          w = che$weight[che$SEX==1 & che$HATLEV1D=="H" & che$AGE==67 & che$YEAR==y])
  emp_che$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(che$EMP[che$SEX==2 & che$HATLEV1D=="L" & che$AGE==67 & che$YEAR==y],
                          w = che$weight[che$SEX==2 & che$HATLEV1D=="L" & che$AGE==67 & che$YEAR==y])
  emp_che$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <-  weighted.mean(che$EMP[che$SEX==2 & che$HATLEV1D=="M" & che$AGE==67 & che$YEAR==y],
                           w = che$weight[che$SEX==2 & che$HATLEV1D=="M" & che$AGE==67 & che$YEAR==y])
  emp_che$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(che$EMP[che$SEX==2 & che$HATLEV1D=="H" & che$AGE==67 & che$YEAR==y],
                          w = che$weight[che$SEX==2 & che$HATLEV1D=="H" & che$AGE==67 & che$YEAR==y])
  emp_che$fhighedu65 <- emp
}

write.csv(emp_che, file = "Data/emp_che.csv")
rm(list = ls())


###Spain: ESP#######
# Load yearly data
esp98 <- read.csv("ES_YEAR_1998_onwards/ES1998_y.csv")
esp99 <- read.csv("ES_YEAR_1998_onwards/ES1999_y.csv")
esp00 <- read.csv("ES_YEAR_1998_onwards/ES2000_y.csv")
esp01 <- read.csv("ES_YEAR_1998_onwards/ES2001_y.csv")
esp02 <- read.csv("ES_YEAR_1998_onwards/ES2002_y.csv")
esp03 <- read.csv("ES_YEAR_1998_onwards/ES2003_y.csv")
esp04 <- read.csv("ES_YEAR_1998_onwards/ES2004_y.csv")
esp05 <- read.csv("ES_YEAR_1998_onwards/ES2005_y.csv")
esp06 <- read.csv("ES_YEAR_1998_onwards/ES2006_y.csv")
esp07 <- read.csv("ES_YEAR_1998_onwards/ES2007_y.csv")
esp08 <- read.csv("ES_YEAR_1998_onwards/ES2008_y.csv")
esp09 <- read.csv("ES_YEAR_1998_onwards/ES2009_y.csv")
esp10 <- read.csv("ES_YEAR_1998_onwards/ES2010_y.csv")
esp11 <- read.csv("ES_YEAR_1998_onwards/ES2011_y.csv")
esp12 <- read.csv("ES_YEAR_1998_onwards/ES2012_y.csv")
esp13 <- read.csv("ES_YEAR_1998_onwards/ES2013_y.csv")
esp14 <- read.csv("ES_YEAR_1998_onwards/ES2014_y.csv")
esp15 <- read.csv("ES_YEAR_1998_onwards/ES2015_y.csv")
esp16 <- read.csv("ES_YEAR_1998_onwards/ES2016_y.csv")
esp17 <- read.csv("ES_YEAR_1998_onwards/ES2017_y.csv")
esp18 <- read.csv("ES_YEAR_1998_onwards/ES2018_y.csv")
esp19 <- read.csv("ES_YEAR_1998_onwards/ES2019_y.csv")

# Merge yearly data
espdata <- rbind(esp98, esp99, esp00, esp01, esp02, esp03, esp04, esp05, esp06, esp07, esp08,
                 esp09, esp10, esp11, esp12, esp13, esp14, esp15, esp16, esp17, esp18, esp19)
rm(esp98, esp99, esp00, esp01, esp02, esp03, esp04, esp05, esp06, esp07, esp08,
   esp09, esp10, esp11, esp12, esp13, esp14, esp15, esp16, esp17, esp18, esp19)

# select relevant variables
esp <- espdata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
esp <- esp[esp$AGE>59 & esp$AGE<70, ] # select age
esp$EMP <- 0 # employment variable
esp$EMP[esp$ILOSTAT==1] <- 1
esp$weight <- 1/esp$COEFF
summary(esp)
# some values are missing in the weighting factor --> listwise deleted
esp <- na.omit(esp)

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_esp <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_esp <- as.data.frame(emp_esp)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(esp$EMP[esp$SEX==1 & esp$HATLEV1D=="L" & esp$AGE==62 & esp$YEAR==y],
                          w = esp$weight[esp$SEX==1 & esp$HATLEV1D=="L" & esp$AGE==62 & esp$YEAR==y])
  emp_esp$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(esp$EMP[esp$SEX==1 & esp$HATLEV1D=="M" & esp$AGE==62 & esp$YEAR==y],
                          w = esp$weight[esp$SEX==1 & esp$HATLEV1D=="M" & esp$AGE==62 & esp$YEAR==y],
                          na.rm = T)
  emp_esp$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(esp$EMP[esp$SEX==1 & esp$HATLEV1D=="H" & esp$AGE==62 & esp$YEAR==y],
                          w = esp$weight[esp$SEX==1 & esp$HATLEV1D=="H" & esp$AGE==62 & esp$YEAR==y],
                          na.rm = T)
  emp_esp$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(esp$EMP[esp$SEX==2 & esp$HATLEV1D=="L" & esp$AGE==62 & esp$YEAR==y],
                          w = esp$weight[esp$SEX==2 & esp$HATLEV1D=="L" & esp$AGE==62 & esp$YEAR==y],
                          na.rm = T)
  emp_esp$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(esp$EMP[esp$SEX==2 & esp$HATLEV1D=="M" & esp$AGE==62 & esp$YEAR==y],
                          w = esp$weight[esp$SEX==2 & esp$HATLEV1D=="M" & esp$AGE==62 & esp$YEAR==y],
                          na.rm = T)
  emp_esp$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(esp$EMP[esp$SEX==2 & esp$HATLEV1D=="H" & esp$AGE==62 & esp$YEAR==y],
                          w = esp$weight[esp$SEX==2 & esp$HATLEV1D=="H" & esp$AGE==62 & esp$YEAR==y],
                          na.rm = T)
  emp_esp$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(esp$EMP[esp$SEX==1 & esp$HATLEV1D=="L" & esp$AGE==67 & esp$YEAR==y],
                          w = esp$weight[esp$SEX==1 & esp$HATLEV1D=="L" & esp$AGE==67 & esp$YEAR==y],
                          na.rm = T)
  emp_esp$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(esp$EMP[esp$SEX==1 & esp$HATLEV1D=="M" & esp$AGE==67 & esp$YEAR==y],
                          w = esp$weight[esp$SEX==1 & esp$HATLEV1D=="M" & esp$AGE==67 & esp$YEAR==y],
                          na.rm = T)
  emp_esp$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(esp$EMP[esp$SEX==1 & esp$HATLEV1D=="H" & esp$AGE==67 & esp$YEAR==y],
                          w = esp$weight[esp$SEX==1 & esp$HATLEV1D=="H" & esp$AGE==67 & esp$YEAR==y],
                          na.rm = T)
  emp_esp$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(esp$EMP[esp$SEX==2 & esp$HATLEV1D=="L" & esp$AGE==67 & esp$YEAR==y],
                          w = esp$weight[esp$SEX==2 & esp$HATLEV1D=="L" & esp$AGE==67 & esp$YEAR==y],
                          na.rm = T)
  emp_esp$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <-  weighted.mean(esp$EMP[esp$SEX==2 & esp$HATLEV1D=="M" & esp$AGE==67 & esp$YEAR==y],
                           w = esp$weight[esp$SEX==2 & esp$HATLEV1D=="M" & esp$AGE==67 & esp$YEAR==y],
                           na.rm = T)
  emp_esp$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(esp$EMP[esp$SEX==2 & esp$HATLEV1D=="H" & esp$AGE==67 & esp$YEAR==y],
                          w = esp$weight[esp$SEX==2 & esp$HATLEV1D=="H" & esp$AGE==67 & esp$YEAR==y],
                          na.rm = T)
  emp_esp$fhighedu65 <- emp
}

write.csv(emp_esp, file = "Data/emp_esp.csv")
rm(list = ls())


###Finland: FIN#######
# Load yearly data
fin98 <- read.csv("FI_YEAR_1998_onwards/FI1998_y.csv")
fin99 <- read.csv("FI_YEAR_1998_onwards/FI1999_y.csv")
fin00 <- read.csv("FI_YEAR_1998_onwards/FI2000_y.csv")
fin01 <- read.csv("FI_YEAR_1998_onwards/FI2001_y.csv")
fin02 <- read.csv("FI_YEAR_1998_onwards/FI2002_y.csv")
fin03 <- read.csv("FI_YEAR_1998_onwards/FI2003_y.csv")
fin04 <- read.csv("FI_YEAR_1998_onwards/FI2004_y.csv")
fin05 <- read.csv("FI_YEAR_1998_onwards/FI2005_y.csv")
fin06 <- read.csv("FI_YEAR_1998_onwards/FI2006_y.csv")
fin07 <- read.csv("FI_YEAR_1998_onwards/FI2007_y.csv")
fin08 <- read.csv("FI_YEAR_1998_onwards/FI2008_y.csv")
fin09 <- read.csv("FI_YEAR_1998_onwards/FI2009_y.csv")
fin10 <- read.csv("FI_YEAR_1998_onwards/FI2010_y.csv")
fin11 <- read.csv("FI_YEAR_1998_onwards/FI2011_y.csv")
fin12 <- read.csv("FI_YEAR_1998_onwards/FI2012_y.csv")
fin13 <- read.csv("FI_YEAR_1998_onwards/FI2013_y.csv")
fin14 <- read.csv("FI_YEAR_1998_onwards/FI2014_y.csv")
fin15 <- read.csv("FI_YEAR_1998_onwards/FI2015_y.csv")
fin16 <- read.csv("FI_YEAR_1998_onwards/FI2016_y.csv")
fin17 <- read.csv("FI_YEAR_1998_onwards/FI2017_y.csv")
fin18 <- read.csv("FI_YEAR_1998_onwards/FI2018_y.csv")
fin19 <- read.csv("FI_YEAR_1998_onwards/FI2019_y.csv")

# Merge yearly data
findata <- rbind(fin98, fin99, fin00, fin01, fin02, fin03, fin04, fin05, fin06, fin07, fin08,
                 fin09, fin10, fin11, fin12, fin13, fin14, fin15, fin16, fin17, fin18, fin19)
rm(fin98, fin99, fin00, fin01, fin02, fin03, fin04, fin05, fin06, fin07, fin08,
   fin09, fin10, fin11, fin12, fin13, fin14, fin15, fin16, fin17, fin18, fin19)

# select relevant variables
fin <- findata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
fin <- fin[fin$AGE>59 & fin$AGE<70, ] # select age
fin$EMP <- 0 # employment variable
fin$EMP[fin$ILOSTAT==1] <- 1
fin$weight <- 1/fin$COEFF
summary(fin)
# note that so many missing values in the weighting factor
fin <- na.omit(fin)

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_fin <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_fin <- as.data.frame(emp_fin)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fin$EMP[fin$SEX==1 & fin$HATLEV1D=="L" & fin$AGE==62 & fin$YEAR==y],
                          w = fin$weight[fin$SEX==1 & fin$HATLEV1D=="L" & fin$AGE==62 & fin$YEAR==y])
  emp_fin$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fin$EMP[fin$SEX==1 & fin$HATLEV1D=="M" & fin$AGE==62 & fin$YEAR==y],
                          w = fin$weight[fin$SEX==1 & fin$HATLEV1D=="M" & fin$AGE==62 & fin$YEAR==y])
  emp_fin$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fin$EMP[fin$SEX==1 & fin$HATLEV1D=="H" & fin$AGE==62 & fin$YEAR==y],
                          w = fin$weight[fin$SEX==1 & fin$HATLEV1D=="H" & fin$AGE==62 & fin$YEAR==y])
  emp_fin$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fin$EMP[fin$SEX==2 & fin$HATLEV1D=="L" & fin$AGE==62 & fin$YEAR==y],
                          w = fin$weight[fin$SEX==2 & fin$HATLEV1D=="L" & fin$AGE==62 & fin$YEAR==y])
  emp_fin$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fin$EMP[fin$SEX==2 & fin$HATLEV1D=="M" & fin$AGE==62 & fin$YEAR==y],
                         w = fin$weight[fin$SEX==2 & fin$HATLEV1D=="M" & fin$AGE==62 & fin$YEAR==y])
  emp_fin$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fin$EMP[fin$SEX==2 & fin$HATLEV1D=="H" & fin$AGE==62 & fin$YEAR==y],
                          w = fin$weight[fin$SEX==2 & fin$HATLEV1D=="H" & fin$AGE==62 & fin$YEAR==y])
  emp_fin$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fin$EMP[fin$SEX==1 & fin$HATLEV1D=="L" & fin$AGE==67 & fin$YEAR==y],
                          w = fin$weight[fin$SEX==1 & fin$HATLEV1D=="L" & fin$AGE==67 & fin$YEAR==y])
  emp_fin$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fin$EMP[fin$SEX==1 & fin$HATLEV1D=="M" & fin$AGE==67 & fin$YEAR==y],
                          w = fin$weight[fin$SEX==1 & fin$HATLEV1D=="M" & fin$AGE==67 & fin$YEAR==y])
  emp_fin$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fin$EMP[fin$SEX==1 & fin$HATLEV1D=="H" & fin$AGE==67 & fin$YEAR==y],
                          w = fin$weight[fin$SEX==1 & fin$HATLEV1D=="H" & fin$AGE==67 & fin$YEAR==y])
  emp_fin$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fin$EMP[fin$SEX==2 & fin$HATLEV1D=="L" & fin$AGE==67 & fin$YEAR==y],
                          w = fin$weight[fin$SEX==2 & fin$HATLEV1D=="L" & fin$AGE==67 & fin$YEAR==y])
  emp_fin$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <-  weighted.mean(fin$EMP[fin$SEX==2 & fin$HATLEV1D=="M" & fin$AGE==67 & fin$YEAR==y],
                           w = fin$weight[fin$SEX==2 & fin$HATLEV1D=="M" & fin$AGE==67 & fin$YEAR==y])
  emp_fin$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fin$EMP[fin$SEX==2 & fin$HATLEV1D=="H" & fin$AGE==67 & fin$YEAR==y],
                          w = fin$weight[fin$SEX==2 & fin$HATLEV1D=="H" & fin$AGE==67 & fin$YEAR==y])
  emp_fin$fhighedu65 <- emp
}

write.csv(emp_fin, file = "Data/emp_fin.csv")
rm(list = ls())


###France: FRA#######
# Load yearly data
fra98 <- read.csv("FR_YEAR_1998_onwards/FR1998_y.csv")
fra99 <- read.csv("FR_YEAR_1998_onwards/FR1999_y.csv")
fra00 <- read.csv("FR_YEAR_1998_onwards/FR2000_y.csv")
fra01 <- read.csv("FR_YEAR_1998_onwards/FR2001_y.csv")
fra02 <- read.csv("FR_YEAR_1998_onwards/FR2002_y.csv")
fra03 <- read.csv("FR_YEAR_1998_onwards/FR2003_y.csv")
fra04 <- read.csv("FR_YEAR_1998_onwards/FR2004_y.csv")
fra05 <- read.csv("FR_YEAR_1998_onwards/FR2005_y.csv")
fra06 <- read.csv("FR_YEAR_1998_onwards/FR2006_y.csv")
fra07 <- read.csv("FR_YEAR_1998_onwards/FR2007_y.csv")
fra08 <- read.csv("FR_YEAR_1998_onwards/FR2008_y.csv")
fra09 <- read.csv("FR_YEAR_1998_onwards/FR2009_y.csv")
fra10 <- read.csv("FR_YEAR_1998_onwards/FR2010_y.csv")
fra11 <- read.csv("FR_YEAR_1998_onwards/FR2011_y.csv")
fra12 <- read.csv("FR_YEAR_1998_onwards/FR2012_y.csv")
fra13 <- read.csv("FR_YEAR_1998_onwards/FR2013_y.csv")
fra14 <- read.csv("FR_YEAR_1998_onwards/FR2014_y.csv")
fra15 <- read.csv("FR_YEAR_1998_onwards/FR2015_y.csv")
fra16 <- read.csv("FR_YEAR_1998_onwards/FR2016_y.csv")
fra17 <- read.csv("FR_YEAR_1998_onwards/FR2017_y.csv")
fra18 <- read.csv("FR_YEAR_1998_onwards/FR2018_y.csv")
fra19 <- read.csv("FR_YEAR_1998_onwards/FR2019_y.csv")

# Merge yearly data
fradata <- rbind(fra98, fra99, fra00, fra01, fra02, fra03, fra04, fra05, fra06, fra07, fra08,
                 fra09, fra10, fra11, fra12, fra13, fra14, fra15, fra16, fra17, fra18, fra19)
rm(fra98, fra99, fra00, fra01, fra02, fra03, fra04, fra05, fra06, fra07, fra08,
   fra09, fra10, fra11, fra12, fra13, fra14, fra15, fra16, fra17, fra18, fra19)

# select relevant variables
fra <- fradata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
fra <- fra[fra$AGE>59 & fra$AGE<70, ] # select age
fra$EMP <- 0 # employment variable
fra$EMP[fra$ILOSTAT==1] <- 1
fra$weight <- 1/fra$COEFF
summary(fra)

fra <- fra[fra$COEFF!=0,]
fra <- na.omit(fra)

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_fra <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_fra <- as.data.frame(emp_fra)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fra$EMP[fra$SEX==1 & fra$HATLEV1D=="L" & fra$AGE==62 & fra$YEAR==y],
                          w = fra$weight[fra$SEX==1 & fra$HATLEV1D=="L" & fra$AGE==62 & fra$YEAR==y])
  emp_fra$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fra$EMP[fra$SEX==1 & fra$HATLEV1D=="M" & fra$AGE==62 & fra$YEAR==y],
                          w = fra$weight[fra$SEX==1 & fra$HATLEV1D=="M" & fra$AGE==62 & fra$YEAR==y])
  emp_fra$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fra$EMP[fra$SEX==1 & fra$HATLEV1D=="H" & fra$AGE==62 & fra$YEAR==y],
                          w = fra$weight[fra$SEX==1 & fra$HATLEV1D=="H" & fra$AGE==62 & fra$YEAR==y])
  emp_fra$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fra$EMP[fra$SEX==2 & fra$HATLEV1D=="L" & fra$AGE==62 & fra$YEAR==y],
                          w = fra$weight[fra$SEX==2 & fra$HATLEV1D=="L" & fra$AGE==62 & fra$YEAR==y])
  emp_fra$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <-  weighted.mean(fra$EMP[fra$SEX==2 & fra$HATLEV1D=="M" & fra$AGE==62 & fra$YEAR==y],
                           w = fra$weight[fra$SEX==2 & fra$HATLEV1D=="M" & fra$AGE==62 & fra$YEAR==y])
  emp_fra$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fra$EMP[fra$SEX==2 & fra$HATLEV1D=="H" & fra$AGE==62 & fra$YEAR==y],
                          w = fra$weight[fra$SEX==2 & fra$HATLEV1D=="H" & fra$AGE==62 & fra$YEAR==y])
  emp_fra$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fra$EMP[fra$SEX==1 & fra$HATLEV1D=="L" & fra$AGE==67 & fra$YEAR==y],
                          w = fra$weight[fra$SEX==1 & fra$HATLEV1D=="L" & fra$AGE==67 & fra$YEAR==y])
  emp_fra$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fra$EMP[fra$SEX==1 & fra$HATLEV1D=="M" & fra$AGE==67 & fra$YEAR==y],
                          w = fra$weight[fra$SEX==1 & fra$HATLEV1D=="M" & fra$AGE==67 & fra$YEAR==y])
  emp_fra$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fra$EMP[fra$SEX==1 & fra$HATLEV1D=="H" & fra$AGE==67 & fra$YEAR==y],
                          w = fra$weight[fra$SEX==1 & fra$HATLEV1D=="H" & fra$AGE==67 & fra$YEAR==y])
  emp_fra$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fra$EMP[fra$SEX==2 & fra$HATLEV1D=="L" & fra$AGE==67 & fra$YEAR==y],
                          w = fra$weight[fra$SEX==2 & fra$HATLEV1D=="L" & fra$AGE==67 & fra$YEAR==y])
  emp_fra$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <-  weighted.mean(fra$EMP[fra$SEX==2 & fra$HATLEV1D=="M" & fra$AGE==67 & fra$YEAR==y],
                           w = fra$weight[fra$SEX==2 & fra$HATLEV1D=="M" & fra$AGE==67 & fra$YEAR==y])
  emp_fra$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(fra$EMP[fra$SEX==2 & fra$HATLEV1D=="H" & fra$AGE==67 & fra$YEAR==y],
                         w = fra$weight[fra$SEX==2 & fra$HATLEV1D=="H" & fra$AGE==67 & fra$YEAR==y])
  emp_fra$fhighedu65 <- emp
}

write.csv(emp_fra, file = "Data/emp_fra.csv")
rm(list = ls())


###Greece: GRC#######
# Load yearly data
grc98 <- read.csv("GR_YEAR_1998_onwards/GR1998_y.csv")
grc99 <- read.csv("GR_YEAR_1998_onwards/GR1999_y.csv")
grc00 <- read.csv("GR_YEAR_1998_onwards/GR2000_y.csv")
grc01 <- read.csv("GR_YEAR_1998_onwards/GR2001_y.csv")
grc02 <- read.csv("GR_YEAR_1998_onwards/GR2002_y.csv")
grc03 <- read.csv("GR_YEAR_1998_onwards/GR2003_y.csv")
grc04 <- read.csv("GR_YEAR_1998_onwards/GR2004_y.csv")
grc05 <- read.csv("GR_YEAR_1998_onwards/GR2005_y.csv")
grc06 <- read.csv("GR_YEAR_1998_onwards/GR2006_y.csv")
grc07 <- read.csv("GR_YEAR_1998_onwards/GR2007_y.csv")
grc08 <- read.csv("GR_YEAR_1998_onwards/GR2008_y.csv")
grc09 <- read.csv("GR_YEAR_1998_onwards/GR2009_y.csv")
grc10 <- read.csv("GR_YEAR_1998_onwards/GR2010_y.csv")
grc11 <- read.csv("GR_YEAR_1998_onwards/GR2011_y.csv")
grc12 <- read.csv("GR_YEAR_1998_onwards/GR2012_y.csv")
grc13 <- read.csv("GR_YEAR_1998_onwards/GR2013_y.csv")
grc14 <- read.csv("GR_YEAR_1998_onwards/GR2014_y.csv")
grc15 <- read.csv("GR_YEAR_1998_onwards/GR2015_y.csv")
grc16 <- read.csv("GR_YEAR_1998_onwards/GR2016_y.csv")
grc17 <- read.csv("GR_YEAR_1998_onwards/GR2017_y.csv")
grc18 <- read.csv("GR_YEAR_1998_onwards/GR2018_y.csv")
grc19 <- read.csv("GR_YEAR_1998_onwards/GR2019_y.csv")

# Merge yearly data
grcdata <- rbind(grc98, grc99, grc00, grc01, grc02, grc03, grc04, grc05, grc06, grc07, grc08,
                 grc09, grc10, grc11, grc12, grc13, grc14, grc15, grc16, grc17, grc18, grc19)
rm(grc98, grc99, grc00, grc01, grc02, grc03, grc04, grc05, grc06, grc07, grc08,
   grc09, grc10, grc11, grc12, grc13, grc14, grc15, grc16, grc17, grc18, grc19)

# select relevant variables
grc <- grcdata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
grc <- grc[grc$AGE>59 & grc$AGE<70, ] # select age
grc$EMP <- 0 # employment variable
grc$EMP[grc$ILOSTAT==1] <- 1
grc$weight <- 1/grc$COEFF
summary(grc)

grc <- grc[grc$COEFF!=0,]
grc <- na.omit(grc) # seems no missing!

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_grc <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_grc <- as.data.frame(emp_grc)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(grc$EMP[grc$SEX==1 & grc$HATLEV1D=="L" & grc$AGE==62 & grc$YEAR==y],
                          w = grc$weight[grc$SEX==1 & grc$HATLEV1D=="L" & grc$AGE==62 & grc$YEAR==y])
  emp_grc$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(grc$EMP[grc$SEX==1 & grc$HATLEV1D=="M" & grc$AGE==62 & grc$YEAR==y],
                          w = grc$weight[grc$SEX==1 & grc$HATLEV1D=="M" & grc$AGE==62 & grc$YEAR==y])
  emp_grc$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(grc$EMP[grc$SEX==1 & grc$HATLEV1D=="H" & grc$AGE==62 & grc$YEAR==y],
                          w = grc$weight[grc$SEX==1 & grc$HATLEV1D=="H" & grc$AGE==62 & grc$YEAR==y])
  emp_grc$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(grc$EMP[grc$SEX==2 & grc$HATLEV1D=="L" & grc$AGE==62 & grc$YEAR==y],
                          w = grc$weight[grc$SEX==2 & grc$HATLEV1D=="L" & grc$AGE==62 & grc$YEAR==y])
  emp_grc$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <-  weighted.mean(grc$EMP[grc$SEX==2 & grc$HATLEV1D=="M" & grc$AGE==62 & grc$YEAR==y],
                           w = grc$weight[grc$SEX==2 & grc$HATLEV1D=="M" & grc$AGE==62 & grc$YEAR==y])
  emp_grc$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(grc$EMP[grc$SEX==2 & grc$HATLEV1D=="H" & grc$AGE==62 & grc$YEAR==y],
                          w = grc$weight[grc$SEX==2 & grc$HATLEV1D=="H" & grc$AGE==62 & grc$YEAR==y])
  emp_grc$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(grc$EMP[grc$SEX==1 & grc$HATLEV1D=="L" & grc$AGE==67 & grc$YEAR==y],
                          w = grc$weight[grc$SEX==1 & grc$HATLEV1D=="L" & grc$AGE==67 & grc$YEAR==y])
  emp_grc$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(grc$EMP[grc$SEX==1 & grc$HATLEV1D=="M" & grc$AGE==67 & grc$YEAR==y],
                          w = grc$weight[grc$SEX==1 & grc$HATLEV1D=="M" & grc$AGE==67 & grc$YEAR==y])
  emp_grc$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(grc$EMP[grc$SEX==1 & grc$HATLEV1D=="H" & grc$AGE==67 & grc$YEAR==y],
                          w = grc$weight[grc$SEX==1 & grc$HATLEV1D=="H" & grc$AGE==67 & grc$YEAR==y])
  emp_grc$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(grc$EMP[grc$SEX==2 & grc$HATLEV1D=="L" & grc$AGE==67 & grc$YEAR==y],
                          w = grc$weight[grc$SEX==2 & grc$HATLEV1D=="L" & grc$AGE==67 & grc$YEAR==y])
  emp_grc$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <-  weighted.mean(grc$EMP[grc$SEX==2 & grc$HATLEV1D=="M" & grc$AGE==67 & grc$YEAR==y],
                           w = grc$weight[grc$SEX==2 & grc$HATLEV1D=="M" & grc$AGE==67 & grc$YEAR==y])
  emp_grc$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(grc$EMP[grc$SEX==2 & grc$HATLEV1D=="H" & grc$AGE==67 & grc$YEAR==y],
                          w = grc$weight[grc$SEX==2 & grc$HATLEV1D=="H" & grc$AGE==67 & grc$YEAR==y])
  emp_grc$fhighedu65 <- emp
}

write.csv(emp_grc, file = "Data/emp_grc.csv")
rm(list = ls())


###Hungary: HUN#######
# Load yearly data
hun98 <- read.csv("HU_YEAR_1998_onwards/HU1998_y.csv")
hun99 <- read.csv("HU_YEAR_1998_onwards/HU1999_y.csv")
hun00 <- read.csv("HU_YEAR_1998_onwards/HU2000_y.csv")
hun01 <- read.csv("HU_YEAR_1998_onwards/HU2001_y.csv")
hun02 <- read.csv("HU_YEAR_1998_onwards/HU2002_y.csv")
hun03 <- read.csv("HU_YEAR_1998_onwards/HU2003_y.csv")
hun04 <- read.csv("HU_YEAR_1998_onwards/HU2004_y.csv")
hun05 <- read.csv("HU_YEAR_1998_onwards/HU2005_y.csv")
hun06 <- read.csv("HU_YEAR_1998_onwards/HU2006_y.csv")
hun07 <- read.csv("HU_YEAR_1998_onwards/HU2007_y.csv")
hun08 <- read.csv("HU_YEAR_1998_onwards/HU2008_y.csv")
hun09 <- read.csv("HU_YEAR_1998_onwards/HU2009_y.csv")
hun10 <- read.csv("HU_YEAR_1998_onwards/HU2010_y.csv")
hun11 <- read.csv("HU_YEAR_1998_onwards/HU2011_y.csv")
hun12 <- read.csv("HU_YEAR_1998_onwards/HU2012_y.csv")
hun13 <- read.csv("HU_YEAR_1998_onwards/HU2013_y.csv")
hun14 <- read.csv("HU_YEAR_1998_onwards/HU2014_y.csv")
hun15 <- read.csv("HU_YEAR_1998_onwards/HU2015_y.csv")
hun16 <- read.csv("HU_YEAR_1998_onwards/HU2016_y.csv")
hun17 <- read.csv("HU_YEAR_1998_onwards/HU2017_y.csv")
hun18 <- read.csv("HU_YEAR_1998_onwards/HU2018_y.csv")
hun19 <- read.csv("HU_YEAR_1998_onwards/HU2019_y.csv")

# Merge yearly data
hundata <- rbind(hun98, hun99, hun00, hun01, hun02, hun03, hun04, hun05, hun06, hun07, hun08,
                 hun09, hun10, hun11, hun12, hun13, hun14, hun15, hun16, hun17, hun18, hun19)
rm(hun98, hun99, hun00, hun01, hun02, hun03, hun04, hun05, hun06, hun07, hun08,
   hun09, hun10, hun11, hun12, hun13, hun14, hun15, hun16, hun17, hun18, hun19)

# select relevant variables
hun <- hundata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
hun <- hun[hun$AGE>59 & hun$AGE<70, ] # select age
hun$EMP <- 0 # employment variable
hun$EMP[hun$ILOSTAT==1] <- 1
hun$weight <- 1/hun$COEFF
summary(hun)

hun <- hun[hun$COEFF!=0,]
hun <- na.omit(hun) # seems no missing!

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_hun <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_hun <- as.data.frame(emp_hun)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(hun$EMP[hun$SEX==1 & hun$HATLEV1D=="L" & hun$AGE==62 & hun$YEAR==y],
                          w = hun$weight[hun$SEX==1 & hun$HATLEV1D=="L" & hun$AGE==62 & hun$YEAR==y])
  emp_hun$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(hun$EMP[hun$SEX==1 & hun$HATLEV1D=="M" & hun$AGE==62 & hun$YEAR==y],
                          w = hun$weight[hun$SEX==1 & hun$HATLEV1D=="M" & hun$AGE==62 & hun$YEAR==y])
  emp_hun$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(hun$EMP[hun$SEX==1 & hun$HATLEV1D=="H" & hun$AGE==62 & hun$YEAR==y],
                          w = hun$weight[hun$SEX==1 & hun$HATLEV1D=="H" & hun$AGE==62 & hun$YEAR==y])
  emp_hun$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(hun$EMP[hun$SEX==2 & hun$HATLEV1D=="L" & hun$AGE==62 & hun$YEAR==y],
                          w = hun$weight[hun$SEX==2 & hun$HATLEV1D=="L" & hun$AGE==62 & hun$YEAR==y])
  emp_hun$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <-  weighted.mean(hun$EMP[hun$SEX==2 & hun$HATLEV1D=="M" & hun$AGE==62 & hun$YEAR==y],
                           w = hun$weight[hun$SEX==2 & hun$HATLEV1D=="M" & hun$AGE==62 & hun$YEAR==y])
  emp_hun$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(hun$EMP[hun$SEX==2 & hun$HATLEV1D=="H" & hun$AGE==62 & hun$YEAR==y],
                          w = hun$weight[hun$SEX==2 & hun$HATLEV1D=="H" & hun$AGE==62 & hun$YEAR==y])
  emp_hun$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(hun$EMP[hun$SEX==1 & hun$HATLEV1D=="L" & hun$AGE==67 & hun$YEAR==y],
                          w = hun$weight[hun$SEX==1 & hun$HATLEV1D=="L" & hun$AGE==67 & hun$YEAR==y])
  emp_hun$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(hun$EMP[hun$SEX==1 & hun$HATLEV1D=="M" & hun$AGE==67 & hun$YEAR==y],
                          w = hun$weight[hun$SEX==1 & hun$HATLEV1D=="M" & hun$AGE==67 & hun$YEAR==y])
  emp_hun$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(hun$EMP[hun$SEX==1 & hun$HATLEV1D=="H" & hun$AGE==67 & hun$YEAR==y],
                          w = hun$weight[hun$SEX==1 & hun$HATLEV1D=="H" & hun$AGE==67 & hun$YEAR==y])
  emp_hun$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(hun$EMP[hun$SEX==2 & hun$HATLEV1D=="L" & hun$AGE==67 & hun$YEAR==y],
                          w = hun$weight[hun$SEX==2 & hun$HATLEV1D=="L" & hun$AGE==67 & hun$YEAR==y])
  emp_hun$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(hun$EMP[hun$SEX==2 & hun$HATLEV1D=="M" & hun$AGE==67 & hun$YEAR==y],
                          w = hun$weight[hun$SEX==2 & hun$HATLEV1D=="M" & hun$AGE==67 & hun$YEAR==y])
  emp_hun$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(hun$EMP[hun$SEX==2 & hun$HATLEV1D=="H" & hun$AGE==67 & hun$YEAR==y],
                          w = hun$weight[hun$SEX==2 & hun$HATLEV1D=="H" & hun$AGE==67 & hun$YEAR==y])
  emp_hun$fhighedu65 <- emp
}

write.csv(emp_hun, file = "Data/emp_hun.csv")
rm(list = ls())


###Ireland: IRE#######
# Load yearly data
ire98 <- read.csv("IE_YEAR_1998_onwards/IE1998_y.csv")
ire99 <- read.csv("IE_YEAR_1998_onwards/IE1999_y.csv")
ire00 <- read.csv("IE_YEAR_1998_onwards/IE2000_y.csv")
ire01 <- read.csv("IE_YEAR_1998_onwards/IE2001_y.csv")
ire02 <- read.csv("IE_YEAR_1998_onwards/IE2002_y.csv")
ire03 <- read.csv("IE_YEAR_1998_onwards/IE2003_y.csv")
ire04 <- read.csv("IE_YEAR_1998_onwards/IE2004_y.csv")
ire05 <- read.csv("IE_YEAR_1998_onwards/IE2005_y.csv")
ire06 <- read.csv("IE_YEAR_1998_onwards/IE2006_y.csv")
ire07 <- read.csv("IE_YEAR_1998_onwards/IE2007_y.csv")
ire08 <- read.csv("IE_YEAR_1998_onwards/IE2008_y.csv")
ire09 <- read.csv("IE_YEAR_1998_onwards/IE2009_y.csv")
ire10 <- read.csv("IE_YEAR_1998_onwards/IE2010_y.csv")
ire11 <- read.csv("IE_YEAR_1998_onwards/IE2011_y.csv")
ire12 <- read.csv("IE_YEAR_1998_onwards/IE2012_y.csv")
ire13 <- read.csv("IE_YEAR_1998_onwards/IE2013_y.csv")
ire14 <- read.csv("IE_YEAR_1998_onwards/IE2014_y.csv")
ire15 <- read.csv("IE_YEAR_1998_onwards/IE2015_y.csv")
ire16 <- read.csv("IE_YEAR_1998_onwards/IE2016_y.csv")
ire17 <- read.csv("IE_YEAR_1998_onwards/IE2017_y.csv")
ire18 <- read.csv("IE_YEAR_1998_onwards/IE2018_y.csv")
ire19 <- read.csv("IE_YEAR_1998_onwards/IE2019_y.csv")

# Merge yearly data
iredata <- rbind(ire98, ire99, ire00, ire01, ire02, ire03, ire04, ire05, ire06, ire07, ire08,
                 ire09, ire10, ire11, ire12, ire13, ire14, ire15, ire16, ire17, ire18, ire19)
rm(ire98, ire99, ire00, ire01, ire02, ire03, ire04, ire05, ire06, ire07, ire08,
   ire09, ire10, ire11, ire12, ire13, ire14, ire15, ire16, ire17, ire18, ire19)

# select relevant variables
ire <- iredata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
ire <- ire[ire$AGE>59 & ire$AGE<70, ] # select age
ire$EMP <- 0 # employment variable
ire$EMP[ire$ILOSTAT==1] <- 1
ire$weight <- 1/ire$COEFF
summary(ire)

ire <- ire[ire$COEFF!=0,]
ire <- na.omit(ire) # seems no missing!

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_ire <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_ire <- as.data.frame(emp_ire)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==1 & ire$HATLEV1D=="L" & ire$AGE==62 & ire$YEAR==y],
                          w = ire$weight[ire$SEX==1 & ire$HATLEV1D=="L" & ire$AGE==62 & ire$YEAR==y])
  emp_ire$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==1 & ire$HATLEV1D=="M" & ire$AGE==62 & ire$YEAR==y],
                          w = ire$weight[ire$SEX==1 & ire$HATLEV1D=="M" & ire$AGE==62 & ire$YEAR==y])
  emp_ire$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==1 & ire$HATLEV1D=="H" & ire$AGE==62 & ire$YEAR==y],
                          w = ire$weight[ire$SEX==1 & ire$HATLEV1D=="H" & ire$AGE==62 & ire$YEAR==y])
  emp_ire$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==2 & ire$HATLEV1D=="L" & ire$AGE==62 & ire$YEAR==y],
                          w = ire$weight[ire$SEX==2 & ire$HATLEV1D=="L" & ire$AGE==62 & ire$YEAR==y])
  emp_ire$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==2 & ire$HATLEV1D=="M" & ire$AGE==62 & ire$YEAR==y],
                           w = ire$weight[ire$SEX==2 & ire$HATLEV1D=="M" & ire$AGE==62 & ire$YEAR==y])
  emp_ire$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==2 & ire$HATLEV1D=="H" & ire$AGE==62 & ire$YEAR==y],
                          w = ire$weight[ire$SEX==2 & ire$HATLEV1D=="H" & ire$AGE==62 & ire$YEAR==y])
  emp_ire$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==1 & ire$HATLEV1D=="L" & ire$AGE==67 & ire$YEAR==y],
                          w = ire$weight[ire$SEX==1 & ire$HATLEV1D=="L" & ire$AGE==67 & ire$YEAR==y])
  emp_ire$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==1 & ire$HATLEV1D=="M" & ire$AGE==67 & ire$YEAR==y],
                          w = ire$weight[ire$SEX==1 & ire$HATLEV1D=="M" & ire$AGE==67 & ire$YEAR==y])
  emp_ire$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==1 & ire$HATLEV1D=="H" & ire$AGE==67 & ire$YEAR==y],
                          w = ire$weight[ire$SEX==1 & ire$HATLEV1D=="H" & ire$AGE==67 & ire$YEAR==y])
  emp_ire$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==2 & ire$HATLEV1D=="L" & ire$AGE==67 & ire$YEAR==y],
                          w = ire$weight[ire$SEX==2 & ire$HATLEV1D=="L" & ire$AGE==67 & ire$YEAR==y])
  emp_ire$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==2 & ire$HATLEV1D=="M" & ire$AGE==67 & ire$YEAR==y],
                          w = ire$weight[ire$SEX==2 & ire$HATLEV1D=="M" & ire$AGE==67 & ire$YEAR==y])
  emp_ire$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ire$EMP[ire$SEX==2 & ire$HATLEV1D=="H" & ire$AGE==67 & ire$YEAR==y],
                          w = ire$weight[ire$SEX==2 & ire$HATLEV1D=="H" & ire$AGE==67 & ire$YEAR==y])
  emp_ire$fhighedu65 <- emp
}

write.csv(emp_ire, file = "Data/emp_ire.csv")
rm(list = ls())


###Italy: ITA#######
# Load yearly data
ita98 <- read.csv("IT_YEAR_1998_onwards/IT1998_y.csv")
ita99 <- read.csv("IT_YEAR_1998_onwards/IT1999_y.csv")
ita00 <- read.csv("IT_YEAR_1998_onwards/IT2000_y.csv")
ita01 <- read.csv("IT_YEAR_1998_onwards/IT2001_y.csv")
ita02 <- read.csv("IT_YEAR_1998_onwards/IT2002_y.csv")
ita03 <- read.csv("IT_YEAR_1998_onwards/IT2003_y.csv")
ita04 <- read.csv("IT_YEAR_1998_onwards/IT2004_y.csv")
ita05 <- read.csv("IT_YEAR_1998_onwards/IT2005_y.csv")
ita06 <- read.csv("IT_YEAR_1998_onwards/IT2006_y.csv")
ita07 <- read.csv("IT_YEAR_1998_onwards/IT2007_y.csv")
ita08 <- read.csv("IT_YEAR_1998_onwards/IT2008_y.csv")
ita09 <- read.csv("IT_YEAR_1998_onwards/IT2009_y.csv")
ita10 <- read.csv("IT_YEAR_1998_onwards/IT2010_y.csv")
ita11 <- read.csv("IT_YEAR_1998_onwards/IT2011_y.csv")
ita12 <- read.csv("IT_YEAR_1998_onwards/IT2012_y.csv")
ita13 <- read.csv("IT_YEAR_1998_onwards/IT2013_y.csv")
ita14 <- read.csv("IT_YEAR_1998_onwards/IT2014_y.csv")
ita15 <- read.csv("IT_YEAR_1998_onwards/IT2015_y.csv")
ita16 <- read.csv("IT_YEAR_1998_onwards/IT2016_y.csv")
ita17 <- read.csv("IT_YEAR_1998_onwards/IT2017_y.csv")
ita18 <- read.csv("IT_YEAR_1998_onwards/IT2018_y.csv")
ita19 <- read.csv("IT_YEAR_1998_onwards/IT2019_y.csv")

# Merge yearly data
itadata <- rbind(ita98, ita99, ita00, ita01, ita02, ita03, ita04, ita05, ita06, ita07, ita08,
                 ita09, ita10, ita11, ita12, ita13, ita14, ita15, ita16, ita17, ita18, ita19)
rm(ita98, ita99, ita00, ita01, ita02, ita03, ita04, ita05, ita06, ita07, ita08,
   ita09, ita10, ita11, ita12, ita13, ita14, ita15, ita16, ita17, ita18, ita19)

# select relevant variables
ita <- itadata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
ita <- ita[ita$AGE>59 & ita$AGE<70, ] # select age
ita$EMP <- 0 # employment variable
ita$EMP[ita$ILOSTAT==1] <- 1
ita$weight <- 1/ita$COEFF
summary(ita)

# if missing values in weighting factors
#ita <- ita[ita$COEFF!=0,]
#ita <- na.omit(ita)

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_ita <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_ita <- as.data.frame(emp_ita)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==1 & ita$HATLEV1D=="L" & ita$AGE==62 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==1 & ita$HATLEV1D=="L" & ita$AGE==62 & ita$YEAR==y])
  emp_ita$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==1 & ita$HATLEV1D=="M" & ita$AGE==62 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==1 & ita$HATLEV1D=="M" & ita$AGE==62 & ita$YEAR==y])
  emp_ita$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==1 & ita$HATLEV1D=="H" & ita$AGE==62 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==1 & ita$HATLEV1D=="H" & ita$AGE==62 & ita$YEAR==y])
  emp_ita$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==2 & ita$HATLEV1D=="L" & ita$AGE==62 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==2 & ita$HATLEV1D=="L" & ita$AGE==62 & ita$YEAR==y])
  emp_ita$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==2 & ita$HATLEV1D=="M" & ita$AGE==62 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==2 & ita$HATLEV1D=="M" & ita$AGE==62 & ita$YEAR==y])
  emp_ita$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==2 & ita$HATLEV1D=="H" & ita$AGE==62 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==2 & ita$HATLEV1D=="H" & ita$AGE==62 & ita$YEAR==y])
  emp_ita$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==1 & ita$HATLEV1D=="L" & ita$AGE==67 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==1 & ita$HATLEV1D=="L" & ita$AGE==67 & ita$YEAR==y])
  emp_ita$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==1 & ita$HATLEV1D=="M" & ita$AGE==67 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==1 & ita$HATLEV1D=="M" & ita$AGE==67 & ita$YEAR==y])
  emp_ita$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==1 & ita$HATLEV1D=="H" & ita$AGE==67 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==1 & ita$HATLEV1D=="H" & ita$AGE==67 & ita$YEAR==y])
  emp_ita$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==2 & ita$HATLEV1D=="L" & ita$AGE==67 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==2 & ita$HATLEV1D=="L" & ita$AGE==67 & ita$YEAR==y])
  emp_ita$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==2 & ita$HATLEV1D=="M" & ita$AGE==67 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==2 & ita$HATLEV1D=="M" & ita$AGE==67 & ita$YEAR==y])
  emp_ita$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(ita$EMP[ita$SEX==2 & ita$HATLEV1D=="H" & ita$AGE==67 & ita$YEAR==y],
                          w = ita$weight[ita$SEX==2 & ita$HATLEV1D=="H" & ita$AGE==67 & ita$YEAR==y])
  emp_ita$fhighedu65 <- emp
}

write.csv(emp_ita, file = "Data/emp_ita.csv")
rm(list = ls())


###Netherlands: NLD#######
# Load yearly data
nld98 <- read.csv("NL_YEAR_1998_onwards/NL1998_y.csv")
nld99 <- read.csv("NL_YEAR_1998_onwards/NL1999_y.csv")
nld00 <- read.csv("NL_YEAR_1998_onwards/NL2000_y.csv")
nld01 <- read.csv("NL_YEAR_1998_onwards/NL2001_y.csv")
nld02 <- read.csv("NL_YEAR_1998_onwards/NL2002_y.csv")
nld03 <- read.csv("NL_YEAR_1998_onwards/NL2003_y.csv")
nld04 <- read.csv("NL_YEAR_1998_onwards/NL2004_y.csv")
nld05 <- read.csv("NL_YEAR_1998_onwards/NL2005_y.csv")
nld06 <- read.csv("NL_YEAR_1998_onwards/NL2006_y.csv")
nld07 <- read.csv("NL_YEAR_1998_onwards/NL2007_y.csv")
nld08 <- read.csv("NL_YEAR_1998_onwards/NL2008_y.csv")
nld09 <- read.csv("NL_YEAR_1998_onwards/NL2009_y.csv")
nld10 <- read.csv("NL_YEAR_1998_onwards/NL2010_y.csv")
nld11 <- read.csv("NL_YEAR_1998_onwards/NL2011_y.csv")
nld12 <- read.csv("NL_YEAR_1998_onwards/NL2012_y.csv")
nld13 <- read.csv("NL_YEAR_1998_onwards/NL2013_y.csv")
nld14 <- read.csv("NL_YEAR_1998_onwards/NL2014_y.csv")
nld15 <- read.csv("NL_YEAR_1998_onwards/NL2015_y.csv")
nld16 <- read.csv("NL_YEAR_1998_onwards/NL2016_y.csv")
nld17 <- read.csv("NL_YEAR_1998_onwards/NL2017_y.csv")
nld18 <- read.csv("NL_YEAR_1998_onwards/NL2018_y.csv")
nld19 <- read.csv("NL_YEAR_1998_onwards/NL2019_y.csv")

# Merge yearly data
nlddata <- rbind(nld98, nld99, nld00, nld01, nld02, nld03, nld04, nld05, nld06, nld07, nld08,
                 nld09, nld10, nld11, nld12, nld13, nld14, nld15, nld16, nld17, nld18, nld19)
rm(nld98, nld99, nld00, nld01, nld02, nld03, nld04, nld05, nld06, nld07, nld08,
   nld09, nld10, nld11, nld12, nld13, nld14, nld15, nld16, nld17, nld18, nld19)

# select relevant variables
nld <- nlddata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
nld <- nld[nld$AGE>59 & nld$AGE<70, ] # select age
nld$EMP <- 0 # employment variable
nld$EMP[nld$ILOSTAT==1] <- 1
nld$weight <- 1/nld$COEFF
summary(nld)

# if missing values in weighting factors
#nld <- nld[nld$COEFF!=0,]
#nld <- na.omit(nld)
nld <- nld[nld$weight<2000,]

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_nld <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_nld <- as.data.frame(emp_nld)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==1 & nld$HATLEV1D=="L" & nld$AGE==62 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==1 & nld$HATLEV1D=="L" & nld$AGE==62 & nld$YEAR==y])
  emp_nld$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==1 & nld$HATLEV1D=="M" & nld$AGE==62 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==1 & nld$HATLEV1D=="M" & nld$AGE==62 & nld$YEAR==y])
  emp_nld$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==1 & nld$HATLEV1D=="H" & nld$AGE==62 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==1 & nld$HATLEV1D=="H" & nld$AGE==62 & nld$YEAR==y])
  emp_nld$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==2 & nld$HATLEV1D=="L" & nld$AGE==62 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==2 & nld$HATLEV1D=="L" & nld$AGE==62 & nld$YEAR==y])
  emp_nld$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==2 & nld$HATLEV1D=="M" & nld$AGE==62 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==2 & nld$HATLEV1D=="M" & nld$AGE==62 & nld$YEAR==y])
  emp_nld$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==2 & nld$HATLEV1D=="H" & nld$AGE==62 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==2 & nld$HATLEV1D=="H" & nld$AGE==62 & nld$YEAR==y])
  emp_nld$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==1 & nld$HATLEV1D=="L" & nld$AGE==67 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==1 & nld$HATLEV1D=="L" & nld$AGE==67 & nld$YEAR==y])
  emp_nld$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==1 & nld$HATLEV1D=="M" & nld$AGE==67 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==1 & nld$HATLEV1D=="M" & nld$AGE==67 & nld$YEAR==y])
  emp_nld$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==1 & nld$HATLEV1D=="H" & nld$AGE==67 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==1 & nld$HATLEV1D=="H" & nld$AGE==67 & nld$YEAR==y])
  emp_nld$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==2 & nld$HATLEV1D=="L" & nld$AGE==67 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==2 & nld$HATLEV1D=="L" & nld$AGE==67 & nld$YEAR==y])
  emp_nld$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==2 & nld$HATLEV1D=="M" & nld$AGE==67 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==2 & nld$HATLEV1D=="M" & nld$AGE==67 & nld$YEAR==y])
  emp_nld$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nld$EMP[nld$SEX==2 & nld$HATLEV1D=="H" & nld$AGE==67 & nld$YEAR==y],
                          w = nld$weight[nld$SEX==2 & nld$HATLEV1D=="H" & nld$AGE==67 & nld$YEAR==y])
  emp_nld$fhighedu65 <- emp
}

write.csv(emp_nld, file = "Data/emp_nld.csv")
rm(list = ls())



###Norway: NOR#######
# Load yearly data
nor98 <- read.csv("NO_YEAR_1998_onwards/NO1998_y.csv")
nor99 <- read.csv("NO_YEAR_1998_onwards/NO1999_y.csv")
nor00 <- read.csv("NO_YEAR_1998_onwards/NO2000_y.csv")
nor01 <- read.csv("NO_YEAR_1998_onwards/NO2001_y.csv")
nor02 <- read.csv("NO_YEAR_1998_onwards/NO2002_y.csv")
nor03 <- read.csv("NO_YEAR_1998_onwards/NO2003_y.csv")
nor04 <- read.csv("NO_YEAR_1998_onwards/NO2004_y.csv")
nor05 <- read.csv("NO_YEAR_1998_onwards/NO2005_y.csv")
nor06 <- read.csv("NO_YEAR_1998_onwards/NO2006_y.csv")
nor07 <- read.csv("NO_YEAR_1998_onwards/NO2007_y.csv")
nor08 <- read.csv("NO_YEAR_1998_onwards/NO2008_y.csv")
nor09 <- read.csv("NO_YEAR_1998_onwards/NO2009_y.csv")
nor10 <- read.csv("NO_YEAR_1998_onwards/NO2010_y.csv")
nor11 <- read.csv("NO_YEAR_1998_onwards/NO2011_y.csv")
nor12 <- read.csv("NO_YEAR_1998_onwards/NO2012_y.csv")
nor13 <- read.csv("NO_YEAR_1998_onwards/NO2013_y.csv")
nor14 <- read.csv("NO_YEAR_1998_onwards/NO2014_y.csv")
nor15 <- read.csv("NO_YEAR_1998_onwards/NO2015_y.csv")
nor16 <- read.csv("NO_YEAR_1998_onwards/NO2016_y.csv")
nor17 <- read.csv("NO_YEAR_1998_onwards/NO2017_y.csv")
nor18 <- read.csv("NO_YEAR_1998_onwards/NO2018_y.csv")
nor19 <- read.csv("NO_YEAR_1998_onwards/NO2019_y.csv")

# Merge yearly data
nordata <- rbind(nor98, nor99, nor00, nor01, nor02, nor03, nor04, nor05, nor06, nor07, nor08,
                 nor09, nor10, nor11, nor12, nor13, nor14, nor15, nor16, nor17, nor18, nor19)
rm(nor98, nor99, nor00, nor01, nor02, nor03, nor04, nor05, nor06, nor07, nor08,
   nor09, nor10, nor11, nor12, nor13, nor14, nor15, nor16, nor17, nor18, nor19)

# select relevant variables
nor <- nordata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
nor <- nor[nor$AGE>59 & nor$AGE<70,] # select age
nor$EMP <- 0 # employment variable
nor$EMP[nor$ILOSTAT==1] <- 1
nor$weight <- 1/nor$COEFF
summary(nor)

# if missing values in weighting factors
#nor <- nor[nor$COEFF!=0,]
#nor <- na.omit(nor)

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_nor <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_nor <- as.data.frame(emp_nor)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==1 & nor$HATLEV1D=="L" & nor$AGE==62 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==1 & nor$HATLEV1D=="L" & nor$AGE==62 & nor$YEAR==y])
  emp_nor$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==1 & nor$HATLEV1D=="M" & nor$AGE==62 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==1 & nor$HATLEV1D=="M" & nor$AGE==62 & nor$YEAR==y])
  emp_nor$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==1 & nor$HATLEV1D=="H" & nor$AGE==62 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==1 & nor$HATLEV1D=="H" & nor$AGE==62 & nor$YEAR==y])
  emp_nor$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==2 & nor$HATLEV1D=="L" & nor$AGE==62 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==2 & nor$HATLEV1D=="L" & nor$AGE==62 & nor$YEAR==y])
  emp_nor$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==2 & nor$HATLEV1D=="M" & nor$AGE==62 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==2 & nor$HATLEV1D=="M" & nor$AGE==62 & nor$YEAR==y])
  emp_nor$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==2 & nor$HATLEV1D=="H" & nor$AGE==62 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==2 & nor$HATLEV1D=="H" & nor$AGE==62 & nor$YEAR==y])
  emp_nor$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==1 & nor$HATLEV1D=="L" & nor$AGE==67 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==1 & nor$HATLEV1D=="L" & nor$AGE==67 & nor$YEAR==y])
  emp_nor$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==1 & nor$HATLEV1D=="M" & nor$AGE==67 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==1 & nor$HATLEV1D=="M" & nor$AGE==67 & nor$YEAR==y])
  emp_nor$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==1 & nor$HATLEV1D=="H" & nor$AGE==67 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==1 & nor$HATLEV1D=="H" & nor$AGE==67 & nor$YEAR==y])
  emp_nor$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==2 & nor$HATLEV1D=="L" & nor$AGE==67 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==2 & nor$HATLEV1D=="L" & nor$AGE==67 & nor$YEAR==y])
  emp_nor$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==2 & nor$HATLEV1D=="M" & nor$AGE==67 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==2 & nor$HATLEV1D=="M" & nor$AGE==67 & nor$YEAR==y])
  emp_nor$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(nor$EMP[nor$SEX==2 & nor$HATLEV1D=="H" & nor$AGE==67 & nor$YEAR==y],
                          w = nor$weight[nor$SEX==2 & nor$HATLEV1D=="H" & nor$AGE==67 & nor$YEAR==y])
  emp_nor$fhighedu65 <- emp
}

write.csv(emp_nor, file = "Data/emp_nor.csv")
rm(list = ls())



###Poland: POL#######
# Load yearly data
pol98 <- read.csv("PL_YEAR_1998_onwards/PL1998_y.csv")
pol99 <- read.csv("PL_YEAR_1998_onwards/PL1999_y.csv")
pol00 <- read.csv("PL_YEAR_1998_onwards/PL2000_y.csv")
pol01 <- read.csv("PL_YEAR_1998_onwards/PL2001_y.csv")
pol02 <- read.csv("PL_YEAR_1998_onwards/PL2002_y.csv")
pol03 <- read.csv("PL_YEAR_1998_onwards/PL2003_y.csv")
pol04 <- read.csv("PL_YEAR_1998_onwards/PL2004_y.csv")
pol05 <- read.csv("PL_YEAR_1998_onwards/PL2005_y.csv")
pol06 <- read.csv("PL_YEAR_1998_onwards/PL2006_y.csv")
pol07 <- read.csv("PL_YEAR_1998_onwards/PL2007_y.csv")
pol08 <- read.csv("PL_YEAR_1998_onwards/PL2008_y.csv")
pol09 <- read.csv("PL_YEAR_1998_onwards/PL2009_y.csv")
pol10 <- read.csv("PL_YEAR_1998_onwards/PL2010_y.csv")
pol11 <- read.csv("PL_YEAR_1998_onwards/PL2011_y.csv")
pol12 <- read.csv("PL_YEAR_1998_onwards/PL2012_y.csv")
pol13 <- read.csv("PL_YEAR_1998_onwards/PL2013_y.csv")
pol14 <- read.csv("PL_YEAR_1998_onwards/PL2014_y.csv")
pol15 <- read.csv("PL_YEAR_1998_onwards/PL2015_y.csv")
pol16 <- read.csv("PL_YEAR_1998_onwards/PL2016_y.csv")
pol17 <- read.csv("PL_YEAR_1998_onwards/PL2017_y.csv")
pol18 <- read.csv("PL_YEAR_1998_onwards/PL2018_y.csv")
pol19 <- read.csv("PL_YEAR_1998_onwards/PL2019_y.csv")

# Merge yearly data
poldata <- rbind(pol98, pol99, pol00, pol01, pol02, pol03, pol04, pol05, pol06, pol07, pol08,
                 pol09, pol10, pol11, pol12, pol13, pol14, pol15, pol16, pol17, pol18, pol19)
rm(pol98, pol99, pol00, pol01, pol02, pol03, pol04, pol05, pol06, pol07, pol08,
   pol09, pol10, pol11, pol12, pol13, pol14, pol15, pol16, pol17, pol18, pol19)

# select relevant variables
pol <- poldata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
pol <- pol[pol$AGE>59 & pol$AGE<70,] # select age
pol$EMP <- 0 # employment variable
pol$EMP[pol$ILOSTAT==1] <- 1
pol$weight <- 1/pol$COEFF
summary(pol)

# if missing values in weighting factors
pol <- pol[pol$COEFF!=0,]
#pol <- na.omit(pol)

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_pol <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_pol <- as.data.frame(emp_pol)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==1 & pol$HATLEV1D=="L" & pol$AGE==62 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==1 & pol$HATLEV1D=="L" & pol$AGE==62 & pol$YEAR==y])
  emp_pol$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==1 & pol$HATLEV1D=="M" & pol$AGE==62 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==1 & pol$HATLEV1D=="M" & pol$AGE==62 & pol$YEAR==y])
  emp_pol$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==1 & pol$HATLEV1D=="H" & pol$AGE==62 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==1 & pol$HATLEV1D=="H" & pol$AGE==62 & pol$YEAR==y])
  emp_pol$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==2 & pol$HATLEV1D=="L" & pol$AGE==62 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==2 & pol$HATLEV1D=="L" & pol$AGE==62 & pol$YEAR==y])
  emp_pol$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==2 & pol$HATLEV1D=="M" & pol$AGE==62 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==2 & pol$HATLEV1D=="M" & pol$AGE==62 & pol$YEAR==y])
  emp_pol$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==2 & pol$HATLEV1D=="H" & pol$AGE==62 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==2 & pol$HATLEV1D=="H" & pol$AGE==62 & pol$YEAR==y])
  emp_pol$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==1 & pol$HATLEV1D=="L" & pol$AGE==67 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==1 & pol$HATLEV1D=="L" & pol$AGE==67 & pol$YEAR==y])
  emp_pol$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==1 & pol$HATLEV1D=="M" & pol$AGE==67 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==1 & pol$HATLEV1D=="M" & pol$AGE==67 & pol$YEAR==y])
  emp_pol$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==1 & pol$HATLEV1D=="H" & pol$AGE==67 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==1 & pol$HATLEV1D=="H" & pol$AGE==67 & pol$YEAR==y])
  emp_pol$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==2 & pol$HATLEV1D=="L" & pol$AGE==67 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==2 & pol$HATLEV1D=="L" & pol$AGE==67 & pol$YEAR==y])
  emp_pol$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==2 & pol$HATLEV1D=="M" & pol$AGE==67 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==2 & pol$HATLEV1D=="M" & pol$AGE==67 & pol$YEAR==y])
  emp_pol$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(pol$EMP[pol$SEX==2 & pol$HATLEV1D=="H" & pol$AGE==67 & pol$YEAR==y],
                          w = pol$weight[pol$SEX==2 & pol$HATLEV1D=="H" & pol$AGE==67 & pol$YEAR==y])
  emp_pol$fhighedu65 <- emp
}

write.csv(emp_pol, file = "Data/emp_pol.csv")
rm(list = ls())


###UK: GBR#######
# Load yearly data
gbr98 <- read.csv("UK_YEAR_1998_onwards/UK1998_y.csv")
gbr99 <- read.csv("UK_YEAR_1998_onwards/UK1999_y.csv")
gbr00 <- read.csv("UK_YEAR_1998_onwards/UK2000_y.csv")
gbr01 <- read.csv("UK_YEAR_1998_onwards/UK2001_y.csv")
gbr02 <- read.csv("UK_YEAR_1998_onwards/UK2002_y.csv")
gbr03 <- read.csv("UK_YEAR_1998_onwards/UK2003_y.csv")
gbr04 <- read.csv("UK_YEAR_1998_onwards/UK2004_y.csv")
gbr05 <- read.csv("UK_YEAR_1998_onwards/UK2005_y.csv")
gbr06 <- read.csv("UK_YEAR_1998_onwards/UK2006_y.csv")
gbr07 <- read.csv("UK_YEAR_1998_onwards/UK2007_y.csv")
gbr08 <- read.csv("UK_YEAR_1998_onwards/UK2008_y.csv")
gbr09 <- read.csv("UK_YEAR_1998_onwards/UK2009_y.csv")
gbr10 <- read.csv("UK_YEAR_1998_onwards/UK2010_y.csv")
gbr11 <- read.csv("UK_YEAR_1998_onwards/UK2011_y.csv")
gbr12 <- read.csv("UK_YEAR_1998_onwards/UK2012_y.csv")
gbr13 <- read.csv("UK_YEAR_1998_onwards/UK2013_y.csv")
gbr14 <- read.csv("UK_YEAR_1998_onwards/UK2014_y.csv")
gbr15 <- read.csv("UK_YEAR_1998_onwards/UK2015_y.csv")
gbr16 <- read.csv("UK_YEAR_1998_onwards/UK2016_y.csv")
gbr17 <- read.csv("UK_YEAR_1998_onwards/UK2017_y.csv")
gbr18 <- read.csv("UK_YEAR_1998_onwards/UK2018_y.csv")
gbr19 <- read.csv("UK_YEAR_1998_onwards/UK2019_y.csv")

# Merge yearly data
gbrdata <- rbind(gbr98, gbr99, gbr00, gbr01, gbr02, gbr03, gbr04, gbr05, gbr06, gbr07, gbr08,
                 gbr09, gbr10, gbr11, gbr12, gbr13, gbr14, gbr15, gbr16, gbr17, gbr18, gbr19)
rm(gbr98, gbr99, gbr00, gbr01, gbr02, gbr03, gbr04, gbr05, gbr06, gbr07, gbr08,
   gbr09, gbr10, gbr11, gbr12, gbr13, gbr14, gbr15, gbr16, gbr17, gbr18, gbr19)

# select relevant variables
gbr <- gbrdata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
gbr <- gbr[gbr$AGE>59 & gbr$AGE<70,] # select age
gbr$EMP <- 0 # employment variable
gbr$EMP[gbr$ILOSTAT==1] <- 1
gbr$weight <- 1/gbr$COEFF
summary(gbr)

# if missing values in weighting factors
#gbr <- gbr[gbr$COEFF!=0,]
gbr <- na.omit(gbr)

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_gbr <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_gbr <- as.data.frame(emp_gbr)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==1 & gbr$HATLEV1D=="L" & gbr$AGE==62 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==1 & gbr$HATLEV1D=="L" & gbr$AGE==62 & gbr$YEAR==y])
  emp_gbr$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==1 & gbr$HATLEV1D=="M" & gbr$AGE==62 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==1 & gbr$HATLEV1D=="M" & gbr$AGE==62 & gbr$YEAR==y])
  emp_gbr$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==1 & gbr$HATLEV1D=="H" & gbr$AGE==62 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==1 & gbr$HATLEV1D=="H" & gbr$AGE==62 & gbr$YEAR==y])
  emp_gbr$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==2 & gbr$HATLEV1D=="L" & gbr$AGE==62 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==2 & gbr$HATLEV1D=="L" & gbr$AGE==62 & gbr$YEAR==y])
  emp_gbr$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==2 & gbr$HATLEV1D=="M" & gbr$AGE==62 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==2 & gbr$HATLEV1D=="M" & gbr$AGE==62 & gbr$YEAR==y])
  emp_gbr$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==2 & gbr$HATLEV1D=="H" & gbr$AGE==62 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==2 & gbr$HATLEV1D=="H" & gbr$AGE==62 & gbr$YEAR==y])
  emp_gbr$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==1 & gbr$HATLEV1D=="L" & gbr$AGE==67 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==1 & gbr$HATLEV1D=="L" & gbr$AGE==67 & gbr$YEAR==y])
  emp_gbr$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==1 & gbr$HATLEV1D=="M" & gbr$AGE==67 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==1 & gbr$HATLEV1D=="M" & gbr$AGE==67 & gbr$YEAR==y])
  emp_gbr$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==1 & gbr$HATLEV1D=="H" & gbr$AGE==67 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==1 & gbr$HATLEV1D=="H" & gbr$AGE==67 & gbr$YEAR==y])
  emp_gbr$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==2 & gbr$HATLEV1D=="L" & gbr$AGE==67 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==2 & gbr$HATLEV1D=="L" & gbr$AGE==67 & gbr$YEAR==y])
  emp_gbr$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==2 & gbr$HATLEV1D=="M" & gbr$AGE==67 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==2 & gbr$HATLEV1D=="M" & gbr$AGE==67 & gbr$YEAR==y])
  emp_gbr$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(gbr$EMP[gbr$SEX==2 & gbr$HATLEV1D=="H" & gbr$AGE==67 & gbr$YEAR==y],
                          w = gbr$weight[gbr$SEX==2 & gbr$HATLEV1D=="H" & gbr$AGE==67 & gbr$YEAR==y])
  emp_gbr$fhighedu65 <- emp
}

#drop unreliable estimates
emp_gbr$mlowedu65[emp_gbr$YEAR==2011] <- NA
emp_gbr$mmidedu65[emp_gbr$YEAR==2011] <- NA
emp_gbr$mhighedu65[emp_gbr$YEAR==2011] <- NA
emp_gbr$flowedu65[emp_gbr$YEAR==2011] <- NA
emp_gbr$fmidedu65[emp_gbr$YEAR==2011] <- NA
emp_gbr$fhighedu65[emp_gbr$YEAR==2011] <- NA

write.csv(emp_gbr, file = "Data/emp_gbr.csv")
rm(list = ls())


###Portugal: PRT#######
# Load yearly data
prt98 <- read.csv("PT_YEAR_1998_onwards/PT1998_y.csv")
prt99 <- read.csv("PT_YEAR_1998_onwards/PT1999_y.csv")
prt00 <- read.csv("PT_YEAR_1998_onwards/PT2000_y.csv")
prt01 <- read.csv("PT_YEAR_1998_onwards/PT2001_y.csv")
prt02 <- read.csv("PT_YEAR_1998_onwards/PT2002_y.csv")
prt03 <- read.csv("PT_YEAR_1998_onwards/PT2003_y.csv")
prt04 <- read.csv("PT_YEAR_1998_onwards/PT2004_y.csv")
prt05 <- read.csv("PT_YEAR_1998_onwards/PT2005_y.csv")
prt06 <- read.csv("PT_YEAR_1998_onwards/PT2006_y.csv")
prt07 <- read.csv("PT_YEAR_1998_onwards/PT2007_y.csv")
prt08 <- read.csv("PT_YEAR_1998_onwards/PT2008_y.csv")
prt09 <- read.csv("PT_YEAR_1998_onwards/PT2009_y.csv")
prt10 <- read.csv("PT_YEAR_1998_onwards/PT2010_y.csv")
prt11 <- read.csv("PT_YEAR_1998_onwards/PT2011_y.csv")
prt12 <- read.csv("PT_YEAR_1998_onwards/PT2012_y.csv")
prt13 <- read.csv("PT_YEAR_1998_onwards/PT2013_y.csv")
prt14 <- read.csv("PT_YEAR_1998_onwards/PT2014_y.csv")
prt15 <- read.csv("PT_YEAR_1998_onwards/PT2015_y.csv")
prt16 <- read.csv("PT_YEAR_1998_onwards/PT2016_y.csv")
prt17 <- read.csv("PT_YEAR_1998_onwards/PT2017_y.csv")
prt18 <- read.csv("PT_YEAR_1998_onwards/PT2018_y.csv")
prt19 <- read.csv("PT_YEAR_1998_onwards/PT2019_y.csv")

# Merge yearly data
prtdata <- rbind(prt98, prt99, prt00, prt01, prt02, prt03, prt04, prt05, prt06, prt07, prt08,
                 prt09, prt10, prt11, prt12, prt13, prt14, prt15, prt16, prt17, prt18, prt19)
rm(prt98, prt99, prt00, prt01, prt02, prt03, prt04, prt05, prt06, prt07, prt08,
   prt09, prt10, prt11, prt12, prt13, prt14, prt15, prt16, prt17, prt18, prt19)

# select relevant variables
prt <- prtdata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
prt <- prt[prt$AGE>59 & prt$AGE<70,] # select age
prt$EMP <- 0 # employment variable
prt$EMP[prt$ILOSTAT==1] <- 1
prt$weight <- 1/prt$COEFF
summary(prt)

# if missing values in weighting factors
#prt <- prt[prt$COEFF!=0,]
#prt <- na.omit(prt)

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_prt <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_prt <- as.data.frame(emp_prt)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==1 & prt$HATLEV1D=="L" & prt$AGE==62 & prt$YEAR==y],
                          w = prt$weight[prt$SEX==1 & prt$HATLEV1D=="L" & prt$AGE==62 & prt$YEAR==y])
  emp_prt$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==1 & prt$HATLEV1D=="M" & prt$AGE==62 & prt$YEAR==y],
                          w = prt$weight[prt$SEX==1 & prt$HATLEV1D=="M" & prt$AGE==62 & prt$YEAR==y])
  emp_prt$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==1 & prt$HATLEV1D=="H" & prt$AGE==62 & prt$YEAR==y],
                          w = prt$weight[prt$SEX==1 & prt$HATLEV1D=="H" & prt$AGE==62 & prt$YEAR==y])
  emp_prt$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==2 & prt$HATLEV1D=="L" & prt$AGE==62 & prt$YEAR==y],
                          w = prt$weight[prt$SEX==2 & prt$HATLEV1D=="L" & prt$AGE==62 & prt$YEAR==y])
  emp_prt$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==2 & prt$HATLEV1D=="M" & prt$AGE==62 & prt$YEAR==y],
                          w = prt$weight[prt$SEX==2 & prt$HATLEV1D=="M" & prt$AGE==62 & prt$YEAR==y])
  emp_prt$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==2 & prt$HATLEV1D=="H" & prt$AGE==62 & prt$YEAR==y],
                         w = prt$weight[prt$SEX==2 & prt$HATLEV1D=="H" & prt$AGE==62 & prt$YEAR==y])
  emp_prt$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==1 & prt$HATLEV1D=="L" & prt$AGE==67 & prt$YEAR==y],
                          w = prt$weight[prt$SEX==1 & prt$HATLEV1D=="L" & prt$AGE==67 & prt$YEAR==y])
  emp_prt$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==1 & prt$HATLEV1D=="M" & prt$AGE==67 & prt$YEAR==y],
                          w = prt$weight[prt$SEX==1 & prt$HATLEV1D=="M" & prt$AGE==67 & prt$YEAR==y])
  emp_prt$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==1 & prt$HATLEV1D=="H" & prt$AGE==67 & prt$YEAR==y],
                          w = prt$weight[prt$SEX==1 & prt$HATLEV1D=="H" & prt$AGE==67 & prt$YEAR==y])
  emp_prt$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==2 & prt$HATLEV1D=="L" & prt$AGE==67 & prt$YEAR==y],
                          w = prt$weight[prt$SEX==2 & prt$HATLEV1D=="L" & prt$AGE==67 & prt$YEAR==y])
  emp_prt$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==2 & prt$HATLEV1D=="M" & prt$AGE==67 & prt$YEAR==y],
                          w = prt$weight[prt$SEX==2 & prt$HATLEV1D=="M" & prt$AGE==67 & prt$YEAR==y])
  emp_prt$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(prt$EMP[prt$SEX==2 & prt$HATLEV1D=="M" & prt$AGE==67 & prt$YEAR==y],
                          w = prt$weight[prt$SEX==2 & prt$HATLEV1D=="M" & prt$AGE==67 & prt$YEAR==y])
  emp_prt$fhighedu65 <- emp
}

write.csv(emp_prt, file = "Data/emp_prt.csv")
rm(list = ls())




###Swedent: SWE#######
# Load yearly data
swe98 <- read.csv("SE_YEAR_1998_onwards/SE1998_y.csv")
swe99 <- read.csv("SE_YEAR_1998_onwards/SE1999_y.csv")
swe00 <- read.csv("SE_YEAR_1998_onwards/SE2000_y.csv")
swe01 <- read.csv("SE_YEAR_1998_onwards/SE2001_y.csv")
swe02 <- read.csv("SE_YEAR_1998_onwards/SE2002_y.csv")
swe03 <- read.csv("SE_YEAR_1998_onwards/SE2003_y.csv")
swe04 <- read.csv("SE_YEAR_1998_onwards/SE2004_y.csv")
swe05 <- read.csv("SE_YEAR_1998_onwards/SE2005_y.csv")
swe06 <- read.csv("SE_YEAR_1998_onwards/SE2006_y.csv")
swe07 <- read.csv("SE_YEAR_1998_onwards/SE2007_y.csv")
swe08 <- read.csv("SE_YEAR_1998_onwards/SE2008_y.csv")
swe09 <- read.csv("SE_YEAR_1998_onwards/SE2009_y.csv")
swe10 <- read.csv("SE_YEAR_1998_onwards/SE2010_y.csv")
swe11 <- read.csv("SE_YEAR_1998_onwards/SE2011_y.csv")
swe12 <- read.csv("SE_YEAR_1998_onwards/SE2012_y.csv")
swe13 <- read.csv("SE_YEAR_1998_onwards/SE2013_y.csv")
swe14 <- read.csv("SE_YEAR_1998_onwards/SE2014_y.csv")
swe15 <- read.csv("SE_YEAR_1998_onwards/SE2015_y.csv")
swe16 <- read.csv("SE_YEAR_1998_onwards/SE2016_y.csv")
swe17 <- read.csv("SE_YEAR_1998_onwards/SE2017_y.csv")
swe18 <- read.csv("SE_YEAR_1998_onwards/SE2018_y.csv")
swe19 <- read.csv("SE_YEAR_1998_onwards/SE2019_y.csv")

# Merge yearly data
swedata <- rbind(swe98, swe99, swe00, swe01, swe02, swe03, swe04, swe05, swe06, swe07, swe08,
                 swe09, swe10, swe11, swe12, swe13, swe14, swe15, swe16, swe17, swe18, swe19)
rm(swe98, swe99, swe00, swe01, swe02, swe03, swe04, swe05, swe06, swe07, swe08,
   swe09, swe10, swe11, swe12, swe13, swe14, swe15, swe16, swe17, swe18, swe19)

# select relevant variables
swe <- swedata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
swe <- swe[swe$AGE>59 & swe$AGE<70,] # select age
swe$EMP <- 0 # employment variable
swe$EMP[swe$ILOSTAT==1] <- 1
swe$weight <- 1/swe$COEFF
summary(swe)

# if missing values in weighting factors
#swe <- swe[swe$COEFF!=0,]
#swe <- na.omit(swe)

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_swe <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_swe <- as.data.frame(emp_swe)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==1 & swe$HATLEV1D=="L" & swe$AGE==62 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==1 & swe$HATLEV1D=="L" & swe$AGE==62 & swe$YEAR==y])
  emp_swe$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==1 & swe$HATLEV1D=="M" & swe$AGE==62 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==1 & swe$HATLEV1D=="M" & swe$AGE==62 & swe$YEAR==y])
  emp_swe$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==1 & swe$HATLEV1D=="H" & swe$AGE==62 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==1 & swe$HATLEV1D=="H" & swe$AGE==62 & swe$YEAR==y])
  emp_swe$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==2 & swe$HATLEV1D=="L" & swe$AGE==62 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==2 & swe$HATLEV1D=="L" & swe$AGE==62 & swe$YEAR==y])
  emp_swe$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==2 & swe$HATLEV1D=="M" & swe$AGE==62 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==2 & swe$HATLEV1D=="M" & swe$AGE==62 & swe$YEAR==y])
  emp_swe$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==2 & swe$HATLEV1D=="H" & swe$AGE==62 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==2 & swe$HATLEV1D=="H" & swe$AGE==62 & swe$YEAR==y])
  emp_swe$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==1 & swe$HATLEV1D=="L" & swe$AGE==67 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==1 & swe$HATLEV1D=="L" & swe$AGE==67 & swe$YEAR==y])
  emp_swe$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==1 & swe$HATLEV1D=="M" & swe$AGE==67 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==1 & swe$HATLEV1D=="M" & swe$AGE==67 & swe$YEAR==y])
  emp_swe$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==1 & swe$HATLEV1D=="H" & swe$AGE==67 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==1 & swe$HATLEV1D=="H" & swe$AGE==67 & swe$YEAR==y])
  emp_swe$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==2 & swe$HATLEV1D=="L" & swe$AGE==67 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==2 & swe$HATLEV1D=="L" & swe$AGE==67 & swe$YEAR==y])
  emp_swe$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==2 & swe$HATLEV1D=="M" & swe$AGE==67 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==2 & swe$HATLEV1D=="M" & swe$AGE==67 & swe$YEAR==y])
  emp_swe$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(swe$EMP[swe$SEX==2 & swe$HATLEV1D=="H" & swe$AGE==67 & swe$YEAR==y],
                          w = swe$weight[swe$SEX==2 & swe$HATLEV1D=="H" & swe$AGE==67 & swe$YEAR==y])
  emp_swe$fhighedu65 <- emp
}

write.csv(emp_swe, file = "Data/emp_swe.csv")
rm(list = ls())


###Slovakia: SVK#######
# Load yearly data
svk98 <- read.csv("SK_YEAR_1998_onwards/SK1998_y.csv")
svk99 <- read.csv("SK_YEAR_1998_onwards/SK1999_y.csv")
svk00 <- read.csv("SK_YEAR_1998_onwards/SK2000_y.csv")
svk01 <- read.csv("SK_YEAR_1998_onwards/SK2001_y.csv")
svk02 <- read.csv("SK_YEAR_1998_onwards/SK2002_y.csv")
svk03 <- read.csv("SK_YEAR_1998_onwards/SK2003_y.csv")
svk04 <- read.csv("SK_YEAR_1998_onwards/SK2004_y.csv")
svk05 <- read.csv("SK_YEAR_1998_onwards/SK2005_y.csv")
svk06 <- read.csv("SK_YEAR_1998_onwards/SK2006_y.csv")
svk07 <- read.csv("SK_YEAR_1998_onwards/SK2007_y.csv")
svk08 <- read.csv("SK_YEAR_1998_onwards/SK2008_y.csv")
svk09 <- read.csv("SK_YEAR_1998_onwards/SK2009_y.csv")
svk10 <- read.csv("SK_YEAR_1998_onwards/SK2010_y.csv")
svk11 <- read.csv("SK_YEAR_1998_onwards/SK2011_y.csv")
svk12 <- read.csv("SK_YEAR_1998_onwards/SK2012_y.csv")
svk13 <- read.csv("SK_YEAR_1998_onwards/SK2013_y.csv")
svk14 <- read.csv("SK_YEAR_1998_onwards/SK2014_y.csv")
svk15 <- read.csv("SK_YEAR_1998_onwards/SK2015_y.csv")
svk16 <- read.csv("SK_YEAR_1998_onwards/SK2016_y.csv")
svk17 <- read.csv("SK_YEAR_1998_onwards/SK2017_y.csv")
svk18 <- read.csv("SK_YEAR_1998_onwards/SK2018_y.csv")
svk19 <- read.csv("SK_YEAR_1998_onwards/SK2019_y.csv")

# Merge yearly data
svkdata <- rbind(svk98, svk99, svk00, svk01, svk02, svk03, svk04, svk05, svk06, svk07, svk08,
                 svk09, svk10, svk11, svk12, svk13, svk14, svk15, svk16, svk17, svk18, svk19)
rm(svk98, svk99, svk00, svk01, svk02, svk03, svk04, svk05, svk06, svk07, svk08,
   svk09, svk10, svk11, svk12, svk13, svk14, svk15, svk16, svk17, svk18, svk19)

# select relevant variables
svk <- svkdata[, c("YEAR", "SEX", "HATLEV1D", "AGE", "COEFF", "ILOSTAT")]
svk <- svk[svk$AGE>59 & svk$AGE<70,] # select age
svk$EMP <- 0 # employment variable
svk$EMP[svk$ILOSTAT==1] <- 1
svk$weight <- 1/svk$COEFF
summary(svk)

# if missing values in weighting factors
#svk <- svk[svk$COEFF!=0,]
#svk <- na.omit(svk)

# create employment dataset
year <- 1998:2019
mlowedu60 <- rep(NA, 22)
mmidedu60 <- rep(NA, 22)
mhighedu60 <- rep(NA, 22)
flowedu60 <- rep(NA, 22)
fmidedu60 <- rep(NA, 22)
fhighedu60 <- rep(NA, 22)
mlowedu65 <- rep(NA, 22)
mmidedu65 <- rep(NA, 22)
mhighedu65 <- rep(NA, 22)
flowedu65 <- rep(NA, 22)
fmidedu65 <- rep(NA, 22)
fhighedu65 <- rep(NA, 22)

emp_svk <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_svk <- as.data.frame(emp_svk)


# calculate group-specific employment rates
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==1 & svk$HATLEV1D=="L" & svk$AGE==62 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==1 & svk$HATLEV1D=="L" & svk$AGE==62 & svk$YEAR==y])
  emp_svk$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==1 & svk$HATLEV1D=="M" & svk$AGE==62 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==1 & svk$HATLEV1D=="M" & svk$AGE==62 & svk$YEAR==y])
  emp_svk$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==1 & svk$HATLEV1D=="H" & svk$AGE==62 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==1 & svk$HATLEV1D=="H" & svk$AGE==62 & svk$YEAR==y])
  emp_svk$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==2 & svk$HATLEV1D=="L" & svk$AGE==62 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==2 & svk$HATLEV1D=="L" & svk$AGE==62 & svk$YEAR==y])
  emp_svk$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==2 & svk$HATLEV1D=="M" & svk$AGE==62 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==2 & svk$HATLEV1D=="M" & svk$AGE==62 & svk$YEAR==y])
  emp_svk$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==2 & svk$HATLEV1D=="H" & svk$AGE==62 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==2 & svk$HATLEV1D=="H" & svk$AGE==62 & svk$YEAR==y])
  emp_svk$fhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==1 & svk$HATLEV1D=="L" & svk$AGE==67 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==1 & svk$HATLEV1D=="L" & svk$AGE==67 & svk$YEAR==y])
  emp_svk$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==1 & svk$HATLEV1D=="M" & svk$AGE==67 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==1 & svk$HATLEV1D=="M" & svk$AGE==67 & svk$YEAR==y])
  emp_svk$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==1 & svk$HATLEV1D=="H" & svk$AGE==67 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==1 & svk$HATLEV1D=="H" & svk$AGE==67 & svk$YEAR==y])
  emp_svk$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==2 & svk$HATLEV1D=="L" & svk$AGE==67 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==2 & svk$HATLEV1D=="L" & svk$AGE==67 & svk$YEAR==y])
  emp_svk$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==2 & svk$HATLEV1D=="M" & svk$AGE==67 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==2 & svk$HATLEV1D=="M" & svk$AGE==67 & svk$YEAR==y])
  emp_svk$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(svk$EMP[svk$SEX==2 & svk$HATLEV1D=="H" & svk$AGE==67 & svk$YEAR==y],
                          w = svk$weight[svk$SEX==2 & svk$HATLEV1D=="H" & svk$AGE==67 & svk$YEAR==y])
  emp_svk$fhighedu65 <- emp
}


write.csv(emp_svk, file = "Data/emp_svk.csv")
rm(list = ls())