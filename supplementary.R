###########################Robustness Checks######################################

####################robust: classifying Czechia in Beveridgean Regime###################
# Model 3: interaction effect with pension regime, age 60-64
ecm3r <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
              lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
              diff(penexppc):Mix1 + lag(penexppc):Mix1 + diff(penexppc):midedu:Mix1 +
              lag(penexppc):midedu:Mix1 + diff(penexppc):lowedu:Mix1 + lag(penexppc):lowedu:Mix1+
              diff(penexppc):Bev1 + lag(penexppc):Bev1 + diff(penexppc):midedu:Bev1 +
              lag(penexppc):midedu:Bev1 + diff(penexppc):lowedu:Bev1 + lag(penexppc):lowedu:Bev1+
              incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
              loggdppc + govdebt + factor(year), data = pdataset60, model = "pooling")
ecm3r_vcv <- vcovHC(x = ecm3r, method = "arellano", type  = 'sss',
                   group = pdataset60$group)
screenreg(coeftest(ecm3r, vcov. = ecm3r_vcv), digits = 4,
          omit.coef ="(diff)|(year)", single.row = F)
summary(ecm3r)


Fecm3r <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
               lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
               diff(penexppc):Mix1 + lag(penexppc):Mix1 + diff(penexppc):midedu:Mix1 +
               lag(penexppc):midedu:Mix1 + diff(penexppc):lowedu:Mix1 + lag(penexppc):lowedu:Mix1+
               diff(penexppc):Bev1 + lag(penexppc):Bev1 + diff(penexppc):midedu:Bev1 +
               lag(penexppc):midedu:Bev1 + diff(penexppc):lowedu:Bev1 + lag(penexppc):lowedu:Bev1+
               incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
               loggdppc + govdebt + factor(year), data = pfdata60, model = "pooling")
Fecm3r_vcv <- vcovHC(x = Fecm3r, method = "arellano", type  = 'sss',
                    group = pfdata60$group)
screenreg(coeftest(Fecm3r, vcov. = Fecm3r_vcv), digits = 4,
          omit.coef ="(diff)|(year)", single.row = F)
summary(Fecm3r)


Mecm3r <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
                lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
                diff(penexppc):Mix1 + lag(penexppc):Mix1 + diff(penexppc):midedu:Mix1 +
                lag(penexppc):midedu:Mix1 + diff(penexppc):lowedu:Mix1 + lag(penexppc):lowedu:Mix1+
                diff(penexppc):Bev1 + lag(penexppc):Bev1 + diff(penexppc):midedu:Bev1 +
                lag(penexppc):midedu:Bev1 + diff(penexppc):lowedu:Bev1 + lag(penexppc):lowedu:Bev1+
                incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
                loggdppc + govdebt + factor(year), data = pmdata60, model = "pooling")
Mecm3r_vcv <- vcovHC(x = Mecm3r, method = "arellano", type  = 'sss',
                     group = pmdata60$group)
screenreg(coeftest(Mecm3r, vcov. = Mecm3r_vcv), digits = 4,
          omit.coef ="(diff)|(year)", single.row = F)
summary(Mecm3r)


# Model 4: interaction effect with pension regime, age 65-69
ecm4r <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
               lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
               diff(penexppc):Mix1 + lag(penexppc):Mix1 + diff(penexppc):midedu:Mix1 +
               lag(penexppc):midedu:Mix1 + diff(penexppc):lowedu:Mix1 + lag(penexppc):lowedu:Mix1+
               diff(penexppc):Bev1 + lag(penexppc):Bev1 + diff(penexppc):midedu:Bev1 +
               lag(penexppc):midedu:Bev1 + diff(penexppc):lowedu:Bev1 + lag(penexppc):lowedu:Bev1+
               incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
               loggdppc + govdebt + factor(year), data = pdataset65, model = "pooling")
ecm4r_vcv <- vcovHC(x = ecm4r, method = "arellano", type  = 'sss',
                    group = pdataset65$group)
screenreg(coeftest(ecm4r, vcov. = ecm4r_vcv), digits = 4,
          omit.coef ="(diff)|(year)", single.row = F)
summary(ecm4r)


Fecm4r <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
                lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
                diff(penexppc):Mix1 + lag(penexppc):Mix1 + diff(penexppc):midedu:Mix1 +
                lag(penexppc):midedu:Mix1 + diff(penexppc):lowedu:Mix1 + lag(penexppc):lowedu:Mix1+
                diff(penexppc):Bev1 + lag(penexppc):Bev1 + diff(penexppc):midedu:Bev1 +
                lag(penexppc):midedu:Bev1 + diff(penexppc):lowedu:Bev1 + lag(penexppc):lowedu:Bev1+
                incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
                loggdppc + govdebt + factor(year), data = pfdata65, model = "pooling")
Fecm4r_vcv <- vcovHC(x = Fecm4r, method = "arellano", type  = 'sss',
                     group = pfdata65$group)
screenreg(coeftest(Fecm4r, vcov. = Fecm4r_vcv), digits = 4,
          omit.coef ="(diff)|(year)", single.row = F)
summary(Fecm4r)


Mecm4r <- plm(diff(emp) ~ lag(emp) + diff(penexppc) + lag(penexppc) + diff(penexppc):midedu +
                lag(penexppc):midedu + diff(penexppc):lowedu + lag(penexppc):lowedu +
                diff(penexppc):Mix1 + lag(penexppc):Mix1 + diff(penexppc):midedu:Mix1 +
                lag(penexppc):midedu:Mix1 + diff(penexppc):lowedu:Mix1 + lag(penexppc):lowedu:Mix1+
                diff(penexppc):Bev1 + lag(penexppc):Bev1 + diff(penexppc):midedu:Bev1 +
                lag(penexppc):midedu:Bev1 + diff(penexppc):lowedu:Bev1 + lag(penexppc):lowedu:Bev1+
                incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
                loggdppc + govdebt + factor(year), data = pmdata65, model = "pooling")
Mecm4r_vcv <- vcovHC(x = Mecm4r, method = "arellano", type  = 'sss',
                     group = pmdata65$group)
screenreg(coeftest(Mecm4r, vcov. = Mecm4r_vcv), digits = 4,
          omit.coef ="(diff)|(year)", single.row = F)
summary(Mecm4r)

# Table A3
screenreg(list(coeftest(ecm3r, vcov. = ecm3r_vcv),
               coeftest(Fecm3r, vcov. = Fecm3r_vcv),
               coeftest(Mecm3r, vcov. = Mecm3r_vcv),
               coeftest(ecm4r, vcov. = ecm4r_vcv),
               coeftest(Fecm4r, vcov. = Fecm4r_vcv),
               coeftest(Mecm4r, vcov. = Mecm4r_vcv)),
          digits = 3, single.row = F, omit.coef = "(diff)|(year)",
          stars = c(0.001, 0.01, 0.05, 0.1))


### Not used in the published version
#####################system GMM estimation#################################################

# Model 1 System GMM: all sample only due to small sample bias in gender-specific analysis
gmm1 <- emp ~ lag(emp, 1) + l.penexppc:highedu + l.penexppc:midedu + l.penexppc:lowedu +
  incapcash + unempexp + eldershare + hle + unemp2554 + outputgap + loggdppc +
  govdebt | lag(emp, 2:6)
sgmm1 <- pgmm(gmm1, data = pdataset60, effect = "twoways", transformation = "ld")
sgmm1_vcv <- vcovHC(x = sgmm1, type  = 'sss', group = pdataset60$country)
summary(sgmm1) # 0.7338 *** / -0.1483 ** / -0.3212 *** / -0.4175 ***
screenreg(list(sgmm1, coeftest(sgmm1, vcov. = sgmm1_vcv)),
          digits = 4, single.row = F, omit.coef = "20",
          stars = c(0.001, 0.01, 0.05, 0.1)) # SEs are identical after sss


# Model 2 system GMM
gmm2 <- emp ~ lag(emp, 1) + l.penexppc:highedu + l.penexppc:midedu + l.penexppc:lowedu +
  incapcash + unempexp + eldershare + hle + unemp2554 + outputgap + loggdppc +
  govdebt | lag(emp, 2:6)
sgmm2 <- pgmm(gmm2, data = pdataset65, effect = "twoways", transformation = "ld")
sgmm2_vcv <- vcovHC(x = sgmm2, type  = 'sss', group = pdataset65$country)
summary(sgmm2) # 0.5169 *** / -0.0125 / -0.1900 *** / -0.2569 ***
screenreg(list(sgmm2, coeftest(sgmm2, vcov. = sgmm2_vcv)),
          digits = 4, single.row = F, stars = c(0.001, 0.01, 0.05, 0.1))
# SEs are identical after sss

# Model 3: GMM
gmm3 <- emp ~ lag(emp, 1) + l.penexppc:highedu:Bismarckian + l.penexppc:midedu:Bismarckian +
  l.penexppc:lowedu:Bismarckian + l.penexppc:highedu:Mixed + l.penexppc:midedu:Mixed +
  l.penexppc:lowedu:Mixed + l.penexppc:highedu:Beveridgean + l.penexppc:midedu:Beveridgean +
  l.penexppc:lowedu:Beveridgean + incapcash + unempexp + eldershare + hle + unemp2554 +
  outputgap + loggdppc + govdebt | lag(emp, 2:6)
sgmm3 <- pgmm(gmm3, data = pdataset60, effect = "twoways", transformation = "ld")
summary(sgmm3)

gmm4 <- emp ~ lag(emp, 1) + l.penexppc:highedu:Bismarckian + l.penexppc:midedu:Bismarckian +
  l.penexppc:lowedu:Bismarckian + l.penexppc:highedu:Mixed + l.penexppc:midedu:Mixed +
  l.penexppc:lowedu:Mixed + l.penexppc:highedu:Beveridgean + l.penexppc:midedu:Beveridgean +
  l.penexppc:lowedu:Beveridgean + incapcash + unempexp + eldershare + hle + unemp2554 +
  outputgap + loggdppc + govdebt | lag(emp, 2:6)
sgmm4 <- pgmm(gmm4, data = pdataset65, effect = "twoways", transformation = "ld")
summary(sgmm4) # 0.5116 *** // -0.0133 / -0.1860 ** / -0.2222 *** vs -0.0209 / -0.1876 ** / -0.2052 **






### Using pension spending as % of GDP as an alternative measure

# Model 1
ecm11<- plm(diff(emp) ~ lag(emp) + diff(penexp):highedu + lag(penexp):highedu +
              diff(penexp):midedu + lag(penexp):midedu + diff(penexp):lowedu +
              lag(penexp):lowedu + incapcashpc + unempexppc + eldershare +
              hle + unemp2554 + outputgap + loggdppc + govdebt + ydum,
            data = pdataset60, model = "pooling")
ecm11_vcv <- vcovHC(x = ecm11, type  = 'sss', group = pdataset60$group)
# "sss" small sample correction used by Stata
summary(ecm11)
coeftest(ecm11, vcov. = ecm11_vcv)

Fecm11 <- plm(diff(emp) ~ lag(emp) + diff(penexp):highedu + lag(penexp):highedu +
                diff(penexp):midedu + lag(penexp):midedu + diff(penexp):lowedu +
                lag(penexp):lowedu + incapcashpc + unempexppc + eldershare +
                hle + unemp2554 + outputgap + loggdppc + govdebt + fydum,
              data = pfdata60, model = "pooling")
Fecm11_vcv <- vcovHC(x = Fecm11, type  = 'sss', group = pfdata60$group)
coeftest(Fecm11, vcov. = Fecm11_vcv)

Mecm11 <- plm(diff(emp) ~ lag(emp) + diff(penexp):highedu + lag(penexp):highedu +
                diff(penexp):midedu + lag(penexp):midedu + diff(penexp):lowedu +
                lag(penexp):lowedu + incapcashpc + unempexppc + eldershare +
                hle + unemp2554 + outputgap + loggdppc + govdebt + mydum,
              data = pmdata60, model = "pooling")
Mecm11_vcv <- vcovHC(x = Mecm11, type  = 'sss', group = pmdata60$group)
coeftest(Mecm11, vcov. = Mecm11_vcv)


# Model 2
ecm22<- plm(diff(emp) ~ lag(emp) + diff(penexp):highedu + lag(penexp):highedu +
              diff(penexp):midedu + lag(penexp):midedu + diff(penexp):lowedu +
              lag(penexp):lowedu + incapcashpc + unempexppc + eldershare +
              hle + unemp2554 + outputgap + loggdppc + govdebt + ydum,
            data = pdataset65, model = "pooling")
ecm22_vcv <- vcovHC(x = ecm22, type  = 'sss', group = pdataset65$group)

Fecm22 <- plm(diff(emp) ~ lag(emp) + diff(penexp):highedu + lag(penexp):highedu +
                diff(penexp):midedu + lag(penexp):midedu + diff(penexp):lowedu +
                lag(penexp):lowedu + incapcashpc + unempexppc + eldershare +
                hle + unemp2554 + outputgap + loggdppc + govdebt + fydum,
              data = pfdata65, model = "pooling")
Fecm22_vcv <- vcovHC(x = Fecm22, type  = 'sss', group = pfdata65$group)

Mecm22 <- plm(diff(emp) ~ lag(emp) + diff(penexp):highedu + lag(penexp):highedu +
                diff(penexp):midedu + lag(penexp):midedu + diff(penexp):lowedu +
                lag(penexp):lowedu + incapcashpc + unempexppc + eldershare +
                hle + unemp2554 + outputgap + loggdppc + govdebt + mydum,
              data = pmdata65, model = "pooling")
Mecm22_vcv <- vcovHC(x = Mecm22, type  = 'sss', group = pmdata65$group)


# Model 3
ecm33 <- plm(diff(emp) ~ lag(emp) + diff(penexp):highedu:Bismarckian +
               lag(penexp):highedu:Bismarckian + diff(penexp):midedu:Bismarckian +
               lag(penexp):midedu:Bismarckian +
               diff(penexp):lowedu:Bismarckian + lag(penexp):lowedu:Bismarckian +
               diff(penexp):highedu:Mixed + lag(penexp):highedu:Mixed +
               diff(penexp):midedu:Mixed + lag(penexp):midedu:Mixed +
               diff(penexp):lowedu:Mixed + lag(penexp):lowedu:Mixed +
               diff(penexp):highedu:Beveridgean + lag(penexp):highedu:Beveridgean +
               diff(penexp):midedu:Beveridgean + lag(penexp):midedu:Beveridgean +
               diff(penexp):lowedu:Beveridgean + lag(penexp):lowedu:Beveridgean +
               incapcashpc + unempexppc + eldershare + hle + unemp2554 + outputgap +
               loggdppc + govdebt + ydum, data = pdataset60, model = "pooling")
ecm33_vcv <- vcovHC(x = ecm33, type  = 'sss', group = pdataset60$group)
# "sss" small sample correction used by Stata
screenreg(coeftest(ecm33, vcov. = ecm33_vcv), digits = 4, single.row = F)

Fecm33 <- plm(diff(emp) ~ lag(emp) + diff(penexp):highedu:Bismarckian + lag(penexp):highedu:Bismarckian +
                diff(penexp):midedu:Bismarckian + lag(penexp):midedu:Bismarckian +
                diff(penexp):lowedu:Bismarckian + lag(penexp):lowedu:Bismarckian +
                diff(penexp):highedu:Mixed + lag(penexp):highedu:Mixed +
                diff(penexp):midedu:Mixed + lag(penexp):midedu:Mixed +
                diff(penexp):lowedu:Mixed + lag(penexp):lowedu:Mixed +
                diff(penexp):highedu:Beveridgean + lag(penexp):highedu:Beveridgean +
                diff(penexp):midedu:Beveridgean + lag(penexp):midedu:Beveridgean +
                diff(penexp):lowedu:Beveridgean + lag(penexp):lowedu:Beveridgean +
                incapcashpc + unempexppc + eldershare + hle + unemp2554 + outputgap +
                loggdppc + govdebt + fydum, data = pfdata60, model = "pooling")
Fecm33_vcv <- vcovHC(x = Fecm33, type  = 'sss', group = pfdata60$group)
# "sss" small sample correction used by Stata
screenreg(coeftest(Fecm33, vcov. = Fecm33_vcv), digits = 4, single.row = F)

Mecm33 <- plm(diff(emp) ~ lag(emp) + diff(penexp):highedu:Bismarckian + lag(penexp):highedu:Bismarckian +
                diff(penexp):midedu:Bismarckian + lag(penexp):midedu:Bismarckian +
                diff(penexp):lowedu:Bismarckian + lag(penexp):lowedu:Bismarckian +
                diff(penexp):highedu:Mixed + lag(penexp):highedu:Mixed +
                diff(penexp):midedu:Mixed + lag(penexp):midedu:Mixed +
                diff(penexp):lowedu:Mixed + lag(penexp):lowedu:Mixed +
                diff(penexp):highedu:Beveridgean + lag(penexp):highedu:Beveridgean +
                diff(penexp):midedu:Beveridgean + lag(penexp):midedu:Beveridgean +
                diff(penexp):lowedu:Beveridgean + lag(penexp):lowedu:Beveridgean +
                incapcashpc + unempexppc + eldershare + hle + unemp2554 + outputgap +
                loggdppc + govdebt + mydum, data = pmdata60, model = "pooling")
Mecm33_vcv <- vcovHC(x = Mecm33, type  = 'sss', group = pmdata60$group)
# "sss" small sample correction used by Stata

screenreg(list(coeftest(ecm33, vcov. = ecm33_vcv),
               coeftest(Fecm33, vcov. = Fecm33_vcv),
               coeftest(Mecm33, vcov. = Mecm33_vcv)),
          digits = 4, single.row = F, stars = c(0.001, 0.01, 0.05, 0.1))


# Model 4

Fecm44 <- plm(diff(emp) ~ lag(emp) + diff(penexp):highedu:Bismarckian + lag(penexp):highedu:Bismarckian +
                diff(penexp):midedu:Bismarckian + lag(penexp):midedu:Bismarckian +
                diff(penexp):lowedu:Bismarckian + lag(penexp):lowedu:Bismarckian +
                diff(penexp):highedu:Mixed + lag(penexp):highedu:Mixed +
                diff(penexp):midedu:Mixed + lag(penexp):midedu:Mixed +
                diff(penexp):lowedu:Mixed + lag(penexp):lowedu:Mixed +
                diff(penexp):highedu:Beveridgean + lag(penexp):highedu:Beveridgean +
                diff(penexp):midedu:Beveridgean + lag(penexp):midedu:Beveridgean +
                diff(penexp):lowedu:Beveridgean + lag(penexp):lowedu:Beveridgean +
                incapcashpc + unempexppc + eldershare + hle + unemp2554 + outputgap +
                loggdppc + govdebt  + fydum, data = pfdata65, model = "pooling")
Fecm44_vcv <- vcovHC(x = Fecm44, type  = 'sss', group = pfdata65$group)

Mecm44 <- plm(diff(emp) ~ lag(emp) + diff(penexp):highedu:Bismarckian + lag(penexp):highedu:Bismarckian +
                diff(penexp):midedu:Bismarckian + lag(penexp):midedu:Bismarckian +
                diff(penexp):lowedu:Bismarckian + lag(penexp):lowedu:Bismarckian +
                diff(penexp):highedu:Mixed + lag(penexp):highedu:Mixed +
                diff(penexp):midedu:Mixed + lag(penexp):midedu:Mixed +
                diff(penexp):lowedu:Mixed + lag(penexp):lowedu:Mixed +
                diff(penexp):highedu:Beveridgean + lag(penexp):highedu:Beveridgean +
                diff(penexp):midedu:Beveridgean + lag(penexp):midedu:Beveridgean +
                diff(penexp):lowedu:Beveridgean + lag(penexp):lowedu:Beveridgean +
                incapcashpc + unempexppc + eldershare + hle + unemp2554 + outputgap +
                loggdppc + govdebt + mydum, data = pmdata65, model = "pooling")
Mecm44_vcv <- vcovHC(x = Mecm44, type  = 'sss', group = pmdata65$group)


# Model 6: poverty effects
Ftw66<- plm(poverty ~ l.penexp:Bismarckian + l.penexp:Mixed + l.penexp:Beveridgean +
              incapcashpc + unempexppc + eldershare + hle + unemp2554 + outputgap +
              loggdppc + govdebt, data = pfdatapov, model = "within", effect = "twoways")
Ftw66_vcv <- vcovHC(x = Ftw66, method = "arellano", type  = 'sss', group = pfdatapov$country)
# "sss" small sample correction used by Stata

Mtw66<- plm(poverty ~ l.penexp:Bismarckian + l.penexp:Mixed + l.penexp:Beveridgean +
              incapcashpc + unempexppc + eldershare + hle + unemp2554 + outputgap +
              loggdppc + govdebt, data = pmdatapov, model = "within", effect = "twoways")
Mtw66_vcv <- vcovHC(x = Mtw66, method = "arellano", type  = 'sss', group = pmdatapov$country)

screenreg(list(coeftest(Ftw66, vcov. = Ftw66_vcv),
               coeftest(Mtw66, vcov. = Mtw66_vcv)),
          digits = 4, single.row = F, stars = c(0.001, 0.01, 0.05, 0.1))



