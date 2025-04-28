###################Model standard errors bootstrap#################
library(MASS)

# ecm1
ecm1_b<- plm(diff(emp) ~ lag(emp) + diff(penexppc):highedu + lag(penexppc):highedu +
             diff(penexppc):midedu + lag(penexppc):midedu + diff(penexppc):lowedu +
             lag(penexppc):lowedu + incapcash + unempexp + eldershare +
             hle + unemp2554 + outputgap + loggdppc + govdebt + factor(year),
           data = pdataset60, model = "pooling")
ecm1b_vcv <- vcovHC(x = ecm1_b, method = "arellano", type  = 'sss',
                   group = pdataset60$group)

# Parametric bootstrap
set.seed(1234)
ecm1_sim <- mvrnorm(n = 10000, mu = coef(ecm1_b), Sigma = ecm1b_vcv)

coef(ecm1_b)[32]/coef(ecm1_b)[2] # highedu: 1.102436
ecm1_high <- -ecm1_sim[,32] / ecm1_sim[,2]
round(quantile(ecm1_high, c(0.0005, 0.005, 0.025, 0.975, 0.995, 0.9995)), 3)
sd(ecm1_high) # 0.1859009

coef(ecm1_b)[34]/coef(ecm1_b)[2] # midedu: 1.712115
ecm1_mid <- -ecm1_sim[,34] / ecm1_sim[,2]
round(quantile(ecm1_mid, c(0.0005, 0.005, 0.025, 0.975, 0.995, 0.9995)), 3)
sd(ecm1_mid) # 0.1866433

coef(ecm1_b)[36]/coef(ecm1_b)[2] # lowedu: 2.165307
ecm1_low <- -ecm1_sim[,36] / ecm1_sim[,2]
round(quantile(ecm1_low, c(0.0005, 0.005, 0.025, 0.975, 0.995, 0.9995)), 3)
sd(ecm1_low) # 0.1909162


## ecm1gen
# Parametric bootstrap
set.seed(1234)
ecm1gen_sim <- mvrnorm(n = 10000, mu = coef(ecm1gen), Sigma = ecm1gen_vcv)

coef(ecm1gen)[4]/coef(ecm1gen)[2] # male: 1.741
ecm1gen_male <- -ecm1gen_sim[,4] / ecm1gen_sim[,2]
round(quantile(ecm1gen_male, c(0.0005, 0.005, 0.025, 0.975, 0.995, 0.9995)), 3)
sd(ecm1gen_male) # 0.1859009

(coef(ecm1gen)[4]+coef(ecm1gen)[34])/coef(ecm1gen)[2] # female: 2.042
ecm1gen_female <- -(ecm1gen_sim[,4] + ecm1gen_sim[,34]) / ecm1gen_sim[,2]
round(quantile(ecm1gen_female, c(0.0005, 0.005, 0.025, 0.975, 0.995, 0.9995)), 3)
sd(ecm1gen_female) # 0.1866433

# ecm2
ecm2_b<- plm(diff(emp) ~ lag(emp) + diff(penexppc):highedu + lag(penexppc):highedu +
               diff(penexppc):midedu + lag(penexppc):midedu + diff(penexppc):lowedu +
               lag(penexppc):lowedu + incapcash + unempexp + eldershare +
               hle + unemp2554 + outputgap + loggdppc + govdebt + factor(year),
             data = pdataset65, model = "pooling")
ecm2b_vcv <- vcovHC(x = ecm2_b, method = "arellano", type  = 'sss',
                    group = pdataset65$group)

# Parametric bootstrap
set.seed(1234)
ecm2_sim <- mvrnorm(n = 10000, mu = coef(ecm2_b), Sigma = ecm2b_vcv)

coef(ecm2_b)[32]/coef(ecm2_b)[2] # highedu: 0.2876256
ecm2_high <- -ecm2_sim[,32] / ecm2_sim[,2]
round(quantile(ecm2_high, c(0.0005, 0.005, 0.025, 0.975, 0.995, 0.9995)), 3)
sd(ecm2_high) #0.114972

coef(ecm2_b)[34]/coef(ecm2_b)[2] # midedu: 0.6436656
ecm2_mid <- -ecm2_sim[,34] / ecm2_sim[,2]
round(quantile(ecm2_mid, c(0.0005, 0.005, 0.025, 0.975, 0.995, 0.9995)), 3)
sd(ecm2_mid) # 0.1058612

coef(ecm2_b)[36]/coef(ecm2_b)[2] # lowedu: 0.8085465 
ecm2_low <- -ecm2_sim[,36] / ecm2_sim[,2]
round(quantile(ecm2_low, c(0.0005, 0.005, 0.025, 0.975, 0.995, 0.9995)), 3)
sd(ecm2_low) # 0.1088976


## ecm2gen
# Parametric bootstrap
set.seed(1234)
ecm2gen_sim <- mvrnorm(n = 10000, mu = coef(ecm2gen), Sigma = ecm2gen_vcv)

coef(ecm2gen)[4]/coef(ecm2gen)[2] # male: 0.4504321
ecm2gen_male <- -ecm2gen_sim[,4] / ecm2gen_sim[,2]
round(quantile(ecm2gen_male, c(0.0005, 0.005, 0.025, 0.975, 0.995, 0.9995)), 3)
sd(ecm2gen_male) # 0.1859009

(coef(ecm2gen)[4]+coef(ecm2gen)[34])/coef(ecm2gen)[2] # female: 0.752
ecm2gen_female <- -(ecm2gen_sim[,4] + ecm2gen_sim[,34]) / ecm2gen_sim[,2]
round(quantile(ecm2gen_female, c(0.0005, 0.005, 0.025, 0.975, 0.995, 0.9995)), 3)
sd(ecm2gen_female) # 0.1384997

