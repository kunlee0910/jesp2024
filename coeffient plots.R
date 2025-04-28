#################################coefplots############################

library(ggpubr) # for ggarrange

# Female, 60-64, moderating effect of pension regimes
Fecm3_b <- plm(diff(emp) ~ lag(emp) + diff(penexppc):highedu:Bis + lag(penexppc):highedu:Bis +
                 diff(penexppc):midedu:Bis + lag(penexppc):midedu:Bis +
                 diff(penexppc):lowedu:Bis + lag(penexppc):lowedu:Bis +
               diff(penexppc):highedu:Mix + lag(penexppc):highedu:Mix +
                 diff(penexppc):midedu:Mix + lag(penexppc):midedu:Mix +
                 diff(penexppc):lowedu:Mix + lag(penexppc):lowedu:Mix+
               diff(penexppc):highedu:Bev + lag(penexppc):highedu:Bev +
                 diff(penexppc):midedu:Bev + lag(penexppc):midedu:Bev +
                 diff(penexppc):lowedu:Bev + lag(penexppc):lowedu:Bev+
               incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
               loggdppc + govdebt + factor(year), data = pfdata60, model = "pooling")
Fecm3b_vcv <- vcovHC(x = Fecm3_b, method = "arellano",
                    type  = 'sss', group = pfdata60$group)

set.seed(1212)
simdata <- mvrnorm(n = 10000, mu = coef(Fecm3_b), Sigma = Fecm3b_vcv)
b1 <- -simdata[,32] / simdata[,2]
b2 <- -simdata[,34] / simdata[,2]
b3 <- -simdata[,36] / simdata[,2]
b4 <- -simdata[,38] / simdata[,2]
b5 <- -simdata[,40] / simdata[,2]
b6 <- -simdata[,42] / simdata[,2]
b7 <- -simdata[,44] / simdata[,2]
b8 <- -simdata[,46] / simdata[,2]
b9 <- -simdata[,48] / simdata[,2]

coef <- c(mean(b1), mean(b2), mean(b3), mean(b4), mean(b5),
          mean(b6), mean(b7), mean(b8), mean(b9))
lb <- c(quantile(b1, 0.05), quantile(b2, 0.05), quantile(b3, 0.05),
        quantile(b4, 0.05), quantile(b5, 0.05), quantile(b6, 0.05),
        quantile(b7, 0.05), quantile(b8, 0.05), quantile(b9, 0.05))
ub <- c(quantile(b1, 0.95), quantile(b2, 0.95), quantile(b3, 0.95),
        quantile(b4, 0.95), quantile(b5, 0.95), quantile(b6, 0.95),
        quantile(b7, 0.95), quantile(b8, 0.95), quantile(b9, 0.95))

# create dataset
plot_f6064 <- data.frame(cbind(coef, lb, ub))
plot_f6064$edu <- rep(c("High-Educated", "Mid-Educated", "Low-Educated"), 3)
plot_f6064$edu <- factor(plot_f6064$edu, levels = c("High-Educated", "Mid-Educated", "Low-Educated"))
plot_f6064$regime <- rep(c("Bismarckian", "Mixed", "Beveridgean"), each = 3)
plot_f6064$regime <- factor(plot_f6064$regime, levels = c("Bismarckian", "Mixed", "Beveridgean"))
rm(coef, lb, ub)

# plot coefficients
graph_f6064 <- ggplot(plot_f6064) +
  geom_linerange(aes(x = edu, ymin = lb, ymax = ub, color = edu), lwd = 1)+
  geom_point(aes(x = edu, y = coef, shape = edu), size = 2.0) + 
  geom_hline(yintercept = 0, colour = "black", lty = "dashed") +
  facet_wrap(~regime, ncol = 1)+
  coord_flip()+
  labs(x = element_blank(), y = element_blank(),
       title = "Female, Age 60-64",
       color = "Education",
       shape = "Education")+
  theme(axis.text.x = element_text(size=12), # adjust size of axis numbers
        axis.text.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 15),
        title = element_text(size = 15))
graph_f6064


# Male, 60-64, moderating effect of pension regimes
Mecm3_b <- plm(diff(emp) ~ lag(emp) + diff(penexppc):highedu:Bis + lag(penexppc):highedu:Bis +
                 diff(penexppc):midedu:Bis + lag(penexppc):midedu:Bis +
                 diff(penexppc):lowedu:Bis + lag(penexppc):lowedu:Bis +
                 diff(penexppc):highedu:Mix + lag(penexppc):highedu:Mix +
                 diff(penexppc):midedu:Mix + lag(penexppc):midedu:Mix +
                 diff(penexppc):lowedu:Mix + lag(penexppc):lowedu:Mix+
                 diff(penexppc):highedu:Bev + lag(penexppc):highedu:Bev +
                 diff(penexppc):midedu:Bev + lag(penexppc):midedu:Bev +
                 diff(penexppc):lowedu:Bev + lag(penexppc):lowedu:Bev+
                 incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
                 loggdppc + govdebt + factor(year), data = pmdata60, model = "pooling")
Mecm3b_vcv <- vcovHC(x = Mecm3_b, method = "arellano",
                     type  = 'sss', group = pmdata60$group)

set.seed(1212)
simdata <- mvrnorm(n = 10000, mu = coef(Mecm3_b), Sigma = Mecm3b_vcv)
b1 <- -simdata[,32] / simdata[,2]
b2 <- -simdata[,34] / simdata[,2]
b3 <- -simdata[,36] / simdata[,2]
b4 <- -simdata[,38] / simdata[,2]
b5 <- -simdata[,40] / simdata[,2]
b6 <- -simdata[,42] / simdata[,2]
b7 <- -simdata[,44] / simdata[,2]
b8 <- -simdata[,46] / simdata[,2]
b9 <- -simdata[,48] / simdata[,2]

coef <- c(mean(b1), mean(b2), mean(b3), mean(b4), mean(b5),
          mean(b6), mean(b7), mean(b8), mean(b9))
lb <- c(quantile(b1, 0.05), quantile(b2, 0.05), quantile(b3, 0.05),
        quantile(b4, 0.05), quantile(b5, 0.05), quantile(b6, 0.05),
        quantile(b7, 0.05), quantile(b8, 0.05), quantile(b9, 0.05))
ub <- c(quantile(b1, 0.95), quantile(b2, 0.95), quantile(b3, 0.95),
        quantile(b4, 0.95), quantile(b5, 0.95), quantile(b6, 0.95),
        quantile(b7, 0.95), quantile(b8, 0.95), quantile(b9, 0.95))

# create dataset
plot_m6064 <- data.frame(cbind(coef, lb, ub))
plot_m6064$edu <- rep(c("High-Educated", "Mid-Educated", "Low-Educated"), 3)
plot_m6064$edu <- factor(plot_m6064$edu, levels = c("High-Educated", "Mid-Educated", "Low-Educated"))
plot_m6064$regime <- rep(c("Bismarckian", "Mixed", "Beveridgean"), each = 3)
plot_m6064$regime <- factor(plot_m6064$regime, levels = c("Bismarckian", "Mixed", "Beveridgean"))
rm(coef, lb, ub)

# plot coefficients
graph_m6064 <- ggplot(plot_m6064) +
  geom_linerange(aes(x = edu, ymin = lb, ymax = ub, color = edu), lwd = 1)+
  geom_point(aes(x = edu, y = coef, shape = edu), size = 2.0) + 
  geom_hline(yintercept = 0, colour = "black", lty = "dashed") +
  facet_wrap(~regime, ncol = 1)+
  coord_flip()+
  labs(x = element_blank(), y = element_blank(),
       title = "Male, Age 60-64",
       color = "Education",
       shape = "Education")+
  theme(axis.text.x = element_text(size=12), # adjust size of axis numbers
        axis.text.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 15),
        title = element_text(size = 15))
graph_m6064


# Female, 65-69, moderating effect of pension regimes
Fecm4_b <- plm(diff(emp) ~ lag(emp) + diff(penexppc):highedu:Bis + lag(penexppc):highedu:Bis +
                 diff(penexppc):midedu:Bis + lag(penexppc):midedu:Bis +
                 diff(penexppc):lowedu:Bis + lag(penexppc):lowedu:Bis +
                 diff(penexppc):highedu:Mix + lag(penexppc):highedu:Mix +
                 diff(penexppc):midedu:Mix + lag(penexppc):midedu:Mix +
                 diff(penexppc):lowedu:Mix + lag(penexppc):lowedu:Mix+
                 diff(penexppc):highedu:Bev + lag(penexppc):highedu:Bev +
                 diff(penexppc):midedu:Bev + lag(penexppc):midedu:Bev +
                 diff(penexppc):lowedu:Bev + lag(penexppc):lowedu:Bev+
                 incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
                 loggdppc + govdebt + factor(year), data = pfdata65, model = "pooling")
Fecm4b_vcv <- vcovHC(x = Fecm4_b, method = "arellano",
                     type  = 'sss', group = pfdata65$group)

set.seed(1212)
simdata <- mvrnorm(n = 10000, mu = coef(Fecm4_b), Sigma = Fecm4b_vcv)
b1 <- -simdata[,32] / simdata[,2]
b2 <- -simdata[,34] / simdata[,2]
b3 <- -simdata[,36] / simdata[,2]
b4 <- -simdata[,38] / simdata[,2]
b5 <- -simdata[,40] / simdata[,2]
b6 <- -simdata[,42] / simdata[,2]
b7 <- -simdata[,44] / simdata[,2]
b8 <- -simdata[,46] / simdata[,2]
b9 <- -simdata[,48] / simdata[,2]

coef <- c(mean(b1), mean(b2), mean(b3), mean(b4), mean(b5),
          mean(b6), mean(b7), mean(b8), mean(b9))
lb <- c(quantile(b1, 0.05), quantile(b2, 0.05), quantile(b3, 0.05),
        quantile(b4, 0.05), quantile(b5, 0.05), quantile(b6, 0.05),
        quantile(b7, 0.05), quantile(b8, 0.05), quantile(b9, 0.05))
ub <- c(quantile(b1, 0.95), quantile(b2, 0.95), quantile(b3, 0.95),
        quantile(b4, 0.95), quantile(b5, 0.95), quantile(b6, 0.95),
        quantile(b7, 0.95), quantile(b8, 0.95), quantile(b9, 0.95))

# create dataset
plot_f6569 <- data.frame(cbind(coef, lb, ub))
plot_f6569$edu <- rep(c("High-Educated", "Mid-Educated", "Low-Educated"), 3)
plot_f6569$edu <- factor(plot_f6569$edu, levels = c("High-Educated", "Mid-Educated", "Low-Educated"))
plot_f6569$regime <- rep(c("Bismarckian", "Mixed", "Beveridgean"), each = 3)
plot_f6569$regime <- factor(plot_f6569$regime, levels = c("Bismarckian", "Mixed", "Beveridgean"))
rm(coef, lb, ub)

# plot coefficients
graph_f6569 <- ggplot(plot_f6569) +
  geom_linerange(aes(x = edu, ymin = lb, ymax = ub, color = edu), lwd = 1)+
  geom_point(aes(x = edu, y = coef, shape = edu), size = 2.0) + 
  geom_hline(yintercept = 0, colour = "black", lty = "dashed") +
  facet_wrap(~regime, ncol = 1)+
  coord_flip()+
  labs(x = element_blank(), y = element_blank(),
       title = "Female, Age 65-69",
       color = "Education",
       shape = "Education")+
  theme(axis.text.x = element_text(size=12), # adjust size of axis numbers
        axis.text.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 15),
        title = element_text(size = 15))
graph_f6569



# Male, 65-69, moderating effect of pension regimes
Mecm4_b <- plm(diff(emp) ~ lag(emp) + diff(penexppc):highedu:Bis + lag(penexppc):highedu:Bis +
                 diff(penexppc):midedu:Bis + lag(penexppc):midedu:Bis +
                 diff(penexppc):lowedu:Bis + lag(penexppc):lowedu:Bis +
                 diff(penexppc):highedu:Mix + lag(penexppc):highedu:Mix +
                 diff(penexppc):midedu:Mix + lag(penexppc):midedu:Mix +
                 diff(penexppc):lowedu:Mix + lag(penexppc):lowedu:Mix+
                 diff(penexppc):highedu:Bev + lag(penexppc):highedu:Bev +
                 diff(penexppc):midedu:Bev + lag(penexppc):midedu:Bev +
                 diff(penexppc):lowedu:Bev + lag(penexppc):lowedu:Bev+
                 incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
                 loggdppc + govdebt + factor(year), data = pmdata65, model = "pooling")
Mecm4b_vcv <- vcovHC(x = Mecm4_b, method = "arellano",
                     type  = 'sss', group = pmdata65$group)

set.seed(1212)
simdata <- mvrnorm(n = 10000, mu = coef(Mecm4_b), Sigma = Mecm4b_vcv)
b1 <- -simdata[,32] / simdata[,2]
b2 <- -simdata[,34] / simdata[,2]
b3 <- -simdata[,36] / simdata[,2]
b4 <- -simdata[,38] / simdata[,2]
b5 <- -simdata[,40] / simdata[,2]
b6 <- -simdata[,42] / simdata[,2]
b7 <- -simdata[,44] / simdata[,2]
b8 <- -simdata[,46] / simdata[,2]
b9 <- -simdata[,48] / simdata[,2]

coef <- c(mean(b1), mean(b2), mean(b3), mean(b4), mean(b5),
          mean(b6), mean(b7), mean(b8), mean(b9))
lb <- c(quantile(b1, 0.05), quantile(b2, 0.05), quantile(b3, 0.05),
        quantile(b4, 0.05), quantile(b5, 0.05), quantile(b6, 0.05),
        quantile(b7, 0.05), quantile(b8, 0.05), quantile(b9, 0.05))
ub <- c(quantile(b1, 0.95), quantile(b2, 0.95), quantile(b3, 0.95),
        quantile(b4, 0.95), quantile(b5, 0.95), quantile(b6, 0.95),
        quantile(b7, 0.95), quantile(b8, 0.95), quantile(b9, 0.95))

# create dataset
plot_m6569 <- data.frame(cbind(coef, lb, ub))
plot_m6569$edu <- rep(c("High-Educated", "Mid-Educated", "Low-Educated"), 3)
plot_m6569$edu <- factor(plot_m6569$edu, levels = c("High-Educated", "Mid-Educated", "Low-Educated"))
plot_m6569$regime <- rep(c("Bismarckian", "Mixed", "Beveridgean"), each = 3)
plot_m6569$regime <- factor(plot_m6569$regime, levels = c("Bismarckian", "Mixed", "Beveridgean"))
rm(coef, lb, ub)

# plot coefficients
graph_m6569 <- ggplot(plot_m6569) +
  geom_linerange(aes(x = edu, ymin = lb, ymax = ub, color = edu), lwd = 1)+
  geom_point(aes(x = edu, y = coef, shape = edu), size = 2.0) + 
  geom_hline(yintercept = 0, colour = "black", lty = "dashed") +
  facet_wrap(~regime, ncol = 1)+
  coord_flip()+
  labs(x = element_blank(), y = element_blank(),
       title = "Male, Age 65-69",
       color = "Education",
       shape = "Education")+
  theme(axis.text.x = element_text(size=12), # adjust size of axis numbers
        axis.text.y = element_blank(),
        strip.text = element_text(size = 12),
        legend.text = element_text(size = 15),
        title = element_text(size = 15))
graph_m6569

# Figure 2
ggarrange(graph_f6064, graph_m6064, graph_f6569, graph_m6569, ncol = 4,
          common.legend = T, legend = "bottom")


# Poverty graph: Figure A4
Ftw6g<- plm(poverty ~ l.penexppc:Bis + l.penexppc:Mix + l.penexppc:Bev +
             incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
             loggdppc + govdebt, data = pfdatapov, model = "within", effect = "twoways")
Ftw6g_vcv <- vcovHC(x = Ftw6g, method = "arellano", type  = 'sss', group = pfdatapov$country)

Mtw6g<- plm(poverty ~ l.penexppc:Bis + l.penexppc:Mix + l.penexppc:Bev +
             incapcash + unempexp + eldershare + hle + unemp2554 + outputgap +
             loggdppc + govdebt, data = pmdatapov, model = "within", effect = "twoways")
Mtw6g_vcv <- vcovHC(x = Mtw6g, method = "arellano", type  = 'sss', group = pmdatapov$country)


coef <- c(coef(Ftw6g)[9], coef(Ftw6g)[10], coef(Ftw6g)[11],
          coef(Mtw6g)[9], coef(Mtw6g)[10], coef(Mtw6g)[11])

sds <- sqrt(c(Ftw6g_vcv[9,9], Ftw6g_vcv[10,10], Ftw6g_vcv[11,11],
             Mtw6g_vcv[9,9], Mtw6g_vcv[10,10], Mtw6g_vcv[11,11]))

# create dataset
plot_tw <- data.frame(cbind(coef, sds))
rm(coef, sds)
plot_tw$lb <- plot_tw$coef - plot_tw$sds * qt(0.975, df = Ftw6g$df.residual)
plot_tw$ub <- plot_tw$coef + plot_tw$sds * qt(0.975, df = Ftw6g$df.residual)
plot_tw$regime <- rep(c("Bismarckian", "Mixed", "Beveridgean"), 2)
plot_tw$regime <- factor(plot_tw$regime, levels = c("Bismarckian", "Mixed", "Beveridgean"))
plot_tw$gender <- rep(c("Female", "Male"), each = 3)


# plot coefficients
graph_poverty <- ggplot(plot_tw) +
  geom_linerange(aes(x = regime, ymin = lb, ymax = ub, color = regime), lwd = 1)+
  geom_point(aes(x = regime, y = coef, shape = regime), size = 2.0) + 
  geom_hline(yintercept = 0, colour = "black", lty = "dashed") +
  facet_wrap(~gender, ncol = 2)+
  labs(x = element_blank(), y = element_blank(),
       color = "Education",
       shape = "Education")+
  theme(axis.text.x = element_blank(), # adjust size of axis numbers
        axis.text.y = element_text(size=12),
        strip.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.position = "bottom")
graph_poverty


