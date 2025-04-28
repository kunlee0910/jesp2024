###########################Paper 1 Analysis#######################################

# remove employment data in the UK 1998-2007 to ensure within-country balance
dataset60$emp[dataset60$country=="GBR" & dataset60$year>1998 & dataset60$year<2008] <- NA
pdataset60 <- pdata.frame(dataset60, index = c("group", "year"))
mdata60$emp[mdata60$country=="GBR" & mdata60$year>1998 & mdata60$year<2008] <- NA
pmdata60 <- pdata.frame(mdata60, index = c("group", "year"))

###################### Model 1: main, female, male 60-64############################

ecm1<- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) +
             diff(penexppc):midedu + lag(penexppc):midedu + diff(penexppc):lowedu +
             lag(penexppc):lowedu + incapcash + unempexp + eldershare +
             hle + unemp2554 + outputgap + loggdppc + govdebt + factor(year),
           data = pdataset60, model = "pooling")
ecm1_vcv <- vcovHC(x = ecm1, method = "arellano", type  = 'sss',
                   group = pdataset60$group)
coeftest(ecm1, vcov. = ecm1_vcv) # -1.102435 (full effect)
summary(ecm1) # -0.0939581 / -0.0519616 *** / -0.0905861 ***

ecm1gen<- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):sex +
                lag(penexppc):sex + incapcash + unempexp + eldershare + hle +
                unemp2554 + outputgap + loggdppc + govdebt + factor(year),
              data = pdataset60, model = "pooling")
ecm1gen_vcv <- vcovHC(x = ecm1gen, method = "arellano", type  = 'sss',
                      group = pdataset60$group)
coeftest(ecm1gen, vcov. = ecm1gen_vcv) # -0.0162381 (t = -1.84, p=0.07)
summary(ecm1gen)

######################## Model 2:  main, female, male 65-69 #############################
ecm2<- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) +
             diff(penexppc):midedu + lag(penexppc):midedu + diff(penexppc):lowedu +
             lag(penexppc):lowedu + incapcash + unempexp + eldershare +
             hle + unemp2554 + outputgap + loggdppc + govdebt + factor(year),
            data = pdataset65, model = "pooling")
ecm2_vcv <- vcovHC(x = ecm2, method = "arellano", type  = 'sss',
                   group = pdataset65$group)
coeftest(ecm2, vcov. = ecm2_vcv) # -0.2876256 (full)
summary(ecm2) #  -0.0417003 *** / -0.0516191 *** / -0.0755237 ***

ecm2gen<- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):sex +
                lag(penexppc):sex + incapcash + unempexp + eldershare + hle +
                unemp2554 + outputgap + loggdppc + govdebt + factor(year),
              data = pdataset65, model = "pooling")
ecm2gen_vcv <- vcovHC(x = ecm2gen, method = "arellano", type  = 'sss',
                      group = pdataset65$group)
coeftest(ecm2gen, vcov. = ecm2gen_vcv) # -0.0162381 (t = -1.84, p=0.07)


# Same as Table 2 in the published version
screenreg(list(coeftest(ecm1, vcov. = ecm1_vcv),
               coeftest(ecm1gen, vcov. = ecm1gen_vcv),
               coeftest(ecm2, vcov. = ecm2_vcv),
               coeftest(ecm2gen, vcov. = ecm2gen_vcv)),
          omit.coef = "(year)|(diff)", digits = 3, single.row = F,
          stars = c(0.001, 0.01, 0.05, 0.1))


#################################################################################


# Model 3: interaction effect with pension regime, age 60-64
# All sample
ecm3 <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
              lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
              diff(penexppc):Mix + lag(penexppc):Mix + diff(penexppc):midedu:Mix +
              lag(penexppc):midedu:Mix + diff(penexppc):lowedu:Mix + lag(penexppc):lowedu:Mix+
              diff(penexppc):Bev + lag(penexppc):Bev + diff(penexppc):midedu:Bev +
              lag(penexppc):midedu:Bev + diff(penexppc):lowedu:Bev + lag(penexppc):lowedu:Bev+
              incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
              loggdppc + govdebt + factor(year), data = pdataset60, model = "pooling")
ecm3_vcv <- vcovHC(x = ecm3, method = "arellano", type  = 'sss',
                   group = pdataset60$group)
screenreg(coeftest(ecm3, vcov. = ecm3_vcv), digits = 4,
          omit.coef ="diff", single.row = F)
summary(ecm3)

# female
Fecm3 <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
               lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
               diff(penexppc):Mix + lag(penexppc):Mix + diff(penexppc):midedu:Mix +
               lag(penexppc):midedu:Mix + diff(penexppc):lowedu:Mix + lag(penexppc):lowedu:Mix+
               diff(penexppc):Bev + lag(penexppc):Bev + diff(penexppc):midedu:Bev +
               lag(penexppc):midedu:Bev + diff(penexppc):lowedu:Bev + lag(penexppc):lowedu:Bev+
               incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
               loggdppc + govdebt + factor(year), data = pfdata60, model = "pooling")
Fecm3_vcv <- vcovHC(x = Fecm3, method = "arellano",
                    type  = 'sss', group = pfdata60$group)
screenreg(coeftest(Fecm3, vcov. = Fecm3_vcv), omit.coef = "diff",
          digits = 4, single.row = F)
summary(Fecm3)

# Male
Mecm3 <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
               lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
               diff(penexppc):Mix + lag(penexppc):Mix + diff(penexppc):midedu:Mix +
               lag(penexppc):midedu:Mix + diff(penexppc):lowedu:Mix + lag(penexppc):lowedu:Mix+
               diff(penexppc):Bev + lag(penexppc):Bev + diff(penexppc):midedu:Bev +
               lag(penexppc):midedu:Bev + diff(penexppc):lowedu:Bev + lag(penexppc):lowedu:Bev+
               incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
               loggdppc + govdebt + factor(year), data = pmdata60, model = "pooling")
Mecm3_vcv <- vcovHC(x = Mecm3, type  = 'sss', group = pmdata60$group)
screenreg(coeftest(Mecm3, vcov. = Mecm3_vcv), digits = 4,
          omit.coef = "(year)|(diff)", single.row = F)
summary(Mecm3)

screenreg(list(coeftest(ecm3, vcov. = ecm3_vcv),
               coeftest(Fecm3, vcov. = Fecm3_vcv),
               coeftest(Mecm3, vcov. = Mecm3_vcv)),
          digits = 4, single.row = F, omit.coef = "(year)|(diff)",
          stars = c(0.001, 0.01, 0.05, 0.1))


# Model 4: interaction effect with pension regime, age 65-69
# All sample
ecm4 <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
              lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
              diff(penexppc):Mix + lag(penexppc):Mix + diff(penexppc):midedu:Mix +
              lag(penexppc):midedu:Mix + diff(penexppc):lowedu:Mix + lag(penexppc):lowedu:Mix+
              diff(penexppc):Bev + lag(penexppc):Bev + diff(penexppc):midedu:Bev +
              lag(penexppc):midedu:Bev + diff(penexppc):lowedu:Bev + lag(penexppc):lowedu:Bev+
              incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
              loggdppc + govdebt + factor(year), data = pdataset65, model = "pooling")
ecm4_vcv <- vcovHC(x = ecm4, type  = 'sss', group = pdataset65$group)
screenreg(coeftest(ecm4, vcov. = ecm4_vcv), omit.coef = "(year)|(diff)",
          digits = 4, single.row = F)
summary(ecm4)

# Female
Fecm4 <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
               lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
               diff(penexppc):Mix + lag(penexppc):Mix + diff(penexppc):midedu:Mix +
               lag(penexppc):midedu:Mix + diff(penexppc):lowedu:Mix + lag(penexppc):lowedu:Mix+
               diff(penexppc):Bev + lag(penexppc):Bev + diff(penexppc):midedu:Bev +
               lag(penexppc):midedu:Bev + diff(penexppc):lowedu:Bev + lag(penexppc):lowedu:Bev+
               incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
               loggdppc + govdebt + factor(year), data = pfdata65, model = "pooling")
Fecm4_vcv <- vcovHC(x = Fecm4, type  = 'sss', group = pfdata65$group)
screenreg(coeftest(Fecm4, vcov. = Fecm4_vcv), omit.coef = "(year)|(diff)",
          digits = 4, single.row = F)
summary(Fecm4)

# Male
Mecm4 <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
               lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
               diff(penexppc):Mix + lag(penexppc):Mix + diff(penexppc):midedu:Mix +
               lag(penexppc):midedu:Mix + diff(penexppc):lowedu:Mix + lag(penexppc):lowedu:Mix+
               diff(penexppc):Bev + lag(penexppc):Bev + diff(penexppc):midedu:Bev +
               lag(penexppc):midedu:Bev + diff(penexppc):lowedu:Bev + lag(penexppc):lowedu:Bev+
               incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
               loggdppc + govdebt + factor(year), data = pmdata65, model = "pooling")
Mecm4_vcv <- vcovHC(x = Mecm4, type  = 'sss', group = pmdata65$group)
screenreg(coeftest(Mecm4, vcov. = Mecm4_vcv), omit.coef = "(ydum)|(diff)",
          digits = 4, single.row = F)
summary(Mecm4) 

# Same as Table 3 in the published paper
screenreg(list(coeftest(ecm3, vcov. = ecm3_vcv),
               coeftest(Fecm3, vcov. = Fecm3_vcv),
               coeftest(Mecm3, vcov. = Mecm3_vcv),
               coeftest(ecm4, vcov. = ecm4_vcv),
               coeftest(Fecm4, vcov. = Fecm4_vcv),
               coeftest(Mecm4, vcov. = Mecm4_vcv)),
          digits = 3, single.row = F, omit.coef = "(diff)|(year)",
          stars = c(0.001, 0.01, 0.05, 0.1))


# Model 6: poverty - used as a supplementary analysis in the published version

tw6<- plm(poverty ~ l.penexppc + l.penexppc:Mix + l.penexppc:Bev +
            incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
            loggdppc + govdebt, data = pdatasetpov, model = "within", effect = "twoways")
tw6_vcv <- vcovHC(x = tw6, method = "arellano", type  = 'sss', group = pdatasetpov$country)
# "sss" small sample correction used by Stata
coeftest(tw6, vcov. = tw6_vcv)
summary(tw6)

Ftw6<- plm(poverty ~ l.penexppc + l.penexppc:Mix + l.penexppc:Bev +
             incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
             loggdppc + govdebt, data = pfdatapov, model = "within", effect = "twoways")
Ftw6_vcv <- vcovHC(x = Ftw6, method = "arellano", type  = 'sss', group = pfdatapov$country)
# "sss" small sample correction used by Stata
coeftest(Ftw6, vcov. = Ftw6_vcv)
summary(Ftw6)

Mtw6<- plm(poverty ~ l.penexppc + l.penexppc:Mix + l.penexppc:Bev +
             incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
             loggdppc + govdebt, data = pmdatapov, model = "within", effect = "twoways")
Mtw6_vcv <- vcovHC(x = Mtw6, method = "arellano", type  = 'sss', group = pmdatapov$country)
coeftest(Mtw6, vcov. = Mtw6_vcv)
summary(Mtw6)

# Same as Table A4 in the published version
screenreg(list(coeftest(tw6, vcov. = tw6_vcv),
               coeftest(Ftw6, vcov. = Ftw6_vcv),
               coeftest(Mtw6, vcov. = Mtw6_vcv)),
          digits = 3, single.row = F, stars = c(0.001, 0.01, 0.05, 0.1))



####### Not used in the published version

# Engle-Granger two-step method
# augmented Dickey-Fuller test to check unit root
model1 <- lm(emp ~ penexppc:highedu + penexppc:midedu + penexppc:lowedu + incapcash +
               unempexp+ eldershare + hle + unemp2554 + outputgap + loggdppc + govdebt,
             data = dataset60)
adf.test(model1$residuals) # augmented Dickey-Fuller: reject unit root (residuals are stationary)
model2 <- lm(emp ~ penexppc:highedu + penexppc:midedu + penexppc:lowedu + incapcash +
               unempexp+ eldershare + hle + unemp2554 + outputgap + loggdppc + govdebt,
             data = dataset65)
adf.test(model2$residuals) # augmented Dickey-Fuller: reject unit root (residuals are stationary)

rm(model1, model2)


##############Model 0: average effect - all, female, male 60-64####################

ecm0<- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + 
             incapcash + unempexp + eldershare +
             hle + unemp2554 + outputgap + loggdppc + govdebt +
             factor(year), data = pdataset60, model = "pooling")
ecm0_vcv <- vcovHC(x = ecm0, method = "arellano", type  = 'sss',
                   group = pdataset60$group)
# "sss" small sample correction used by Stata
coeftest(ecm0, vcov. = ecm0_vcv)
summary(ecm0) # average full effect: -1.944958

olsldv0 <- plm(emp ~ lag(emp) + l.penexppc + incapcash + unempexp + eldershare +
                 hle + unemp2554 + outputgap + loggdppc + govdebt +
                 ydum, data = pdataset60, model = "pooling")
olsldv0_vcv <- vcovBK(olsldv0, group = pdataset60$country)
coeftest(olsldv0, vcov. = olsldv0_vcv) # -1.940207
summary(olsldv0)


Fecm0 <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + 
               incapcash + unempexp + eldershare +
               hle + unemp2554 + outputgap + loggdppc + govdebt +
               fydum, data = pfdata60, model = "pooling")
Fecm0_vcv <- vcovHC(x = Fecm0, method = "arellano", type  = 'sss',
                    group = pfdata60$group)
coeftest(Fecm0, vcov. = Fecm0_vcv)
summary(Fecm0) # average full effect: -2.214516

Folsldv0 <- plm(emp ~ lag(emp) + l.penexppc + incapcash + unempexp + eldershare +
                  hle + unemp2554 + outputgap + loggdppc + govdebt +
                  fydum, data = pfdata60, model = "pooling")
Folsldv0_vcv <- vcovBK(Folsldv0, group = pfdata60$country)
coeftest(Folsldv0, vcov. = Folsldv0_vcv) # -2.184283
summary(Folsldv0)


Mecm0<- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + 
              incapcash + unempexp + eldershare +
              hle + unemp2554 + outputgap + loggdppc + govdebt +
              mydum, data = pmdata60, model = "pooling")
Mecm0_vcv <- vcovHC(x = Mecm0, method = "arellano", type  = 'sss',
                    group = pmdata60$group)
coeftest(Mecm0, vcov. = Mecm0_vcv) 
summary(Mecm0) # average full effect: -1.555061

Molsldv0 <- plm(emp ~ lag(emp) + l.penexppc + incapcash + unempexp + eldershare +
                  hle + unemp2554 + outputgap + loggdppc + govdebt +
                  mydum, data = pmdata60, model = "pooling")
Molsldv0_vcv <- vcovBK(Molsldv0, group = pmdata60$country)
coeftest(Molsldv0, vcov. = Molsldv0_vcv) # -1.564169
summary(Molsldv0)



