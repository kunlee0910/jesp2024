############US employment rates by age groups, sex, level of education##############

cps <- read.csv(gzfile(file.choose("Data/cps_00002.csv")), as.is = TRUE)
View(cps)

# employment variable
table(cps$EMPSTAT)
cps$EMP <- rep(NA, length(cps$EMPSTAT))
cps$EMP[cps$EMPSTAT==10]=1
cps$EMP[cps$EMPSTAT==12]=1
cps$EMP[cps$EMPSTAT!=10 & cps$EMPSTAT!=12]=0

# select age groups
cpsdata <- cps[cps$AGE>59,] # extract older people only
cpsdata <- cpsdata[cpsdata$AGE<70,] # only below age 65
cpsdata$agegroup <- 0
cpsdata$agegroup[cpsdata$AGE>64] <- 1
table(cpsdata$agegroup)

# make harmonized education variable. ISCED= c(0,1,2)
table(cpsdata$EDUC)
cpsdata$ISCED=NA 
cpsdata$ISCED[cpsdata$EDUC<41]=0
cpsdata$ISCED[cpsdata$EDUC>40 & cpsdata$EDUC<90]=1
cpsdata$ISCED[cpsdata$EDUC>90]=2
table(cpsdata$ISCED)

# create aggregate dataset
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

#create dataset
emp_usa <- cbind(year, mlowedu60, mmidedu60, mhighedu60, flowedu60, fmidedu60, fhighedu60,
                 mlowedu65, mmidedu65, mhighedu65, flowedu65, fmidedu65, fhighedu65)
emp_usa <- as.data.frame(emp_usa)
View(emp_usa)

# making group-specific employment data using loop
emp <- rep(NA, 22)
for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==1 & cpsdata$ISCED==0 & cpsdata$agegroup==0 & cpsdata$YEAR==y],
            w = cpsdata$ASECWT[cpsdata$SEX==1 & cpsdata$ISCED==0 & cpsdata$agegroup==0 & cpsdata$YEAR==y])
  emp_usa$mlowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==1 & cpsdata$ISCED==1 & cpsdata$agegroup==0 & cpsdata$YEAR==y],
                          w = cpsdata$ASECWT[cpsdata$SEX==1 & cpsdata$ISCED==1 & cpsdata$agegroup==0 & cpsdata$YEAR==y])
  emp_usa$mmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==1 & cpsdata$ISCED==2 & cpsdata$agegroup==0 & cpsdata$YEAR==y],
                          w = cpsdata$ASECWT[cpsdata$SEX==1 & cpsdata$ISCED==2 & cpsdata$agegroup==0 & cpsdata$YEAR==y])
  emp_usa$mhighedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==2 & cpsdata$ISCED==0 & cpsdata$agegroup==0 & cpsdata$YEAR==y],
                          w = cpsdata$ASECWT[cpsdata$SEX==2 & cpsdata$ISCED==0 & cpsdata$agegroup==0 & cpsdata$YEAR==y])
  emp_usa$flowedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==2 & cpsdata$ISCED==1 & cpsdata$agegroup==0 & cpsdata$YEAR==y],
                          w = cpsdata$ASECWT[cpsdata$SEX==2 & cpsdata$ISCED==1 & cpsdata$agegroup==0 & cpsdata$YEAR==y])
  emp_usa$fmidedu60 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==2 & cpsdata$ISCED==2 & cpsdata$agegroup==0 & cpsdata$YEAR==y],
                          w = cpsdata$ASECWT[cpsdata$SEX==2 & cpsdata$ISCED==2 & cpsdata$agegroup==0 & cpsdata$YEAR==y])
  emp_usa$fhighedu60 <- emp
}


for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==1 & cpsdata$ISCED==0 & cpsdata$agegroup==1 & cpsdata$YEAR==y],
                          w = cpsdata$ASECWT[cpsdata$SEX==1 & cpsdata$ISCED==0 & cpsdata$agegroup==1 & cpsdata$YEAR==y])
  emp_usa$mlowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==1 & cpsdata$ISCED==1 & cpsdata$agegroup==1 & cpsdata$YEAR==y],
                          w = cpsdata$ASECWT[cpsdata$SEX==1 & cpsdata$ISCED==1 & cpsdata$agegroup==1 & cpsdata$YEAR==y])
  emp_usa$mmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==1 & cpsdata$ISCED==2 & cpsdata$agegroup==1 & cpsdata$YEAR==y],
                          w = cpsdata$ASECWT[cpsdata$SEX==1 & cpsdata$ISCED==2 & cpsdata$agegroup==1 & cpsdata$YEAR==y])
  emp_usa$mhighedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==2 & cpsdata$ISCED==0 & cpsdata$agegroup==1 & cpsdata$YEAR==y],
                          w = cpsdata$ASECWT[cpsdata$SEX==2 & cpsdata$ISCED==0 & cpsdata$agegroup==1 & cpsdata$YEAR==y])
  emp_usa$flowedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==2 & cpsdata$ISCED==1 & cpsdata$agegroup==1 & cpsdata$YEAR==y],
                          w = cpsdata$ASECWT[cpsdata$SEX==2 & cpsdata$ISCED==1 & cpsdata$agegroup==1 & cpsdata$YEAR==y])
  emp_usa$fmidedu65 <- emp
}

for (i in 1:22){
  y <- year[i]
  emp[i] <- weighted.mean(cpsdata$EMP[cpsdata$SEX==2 & cpsdata$ISCED==2 & cpsdata$agegroup==1 & cpsdata$YEAR==y],
                          w = cpsdata$ASECWT[cpsdata$SEX==2 & cpsdata$ISCED==2 & cpsdata$agegroup==1 & cpsdata$YEAR==y])
  emp_usa$fhighedu65 <- emp
}

write.csv(emp_usa, file = "Data/emp_usa.csv")
