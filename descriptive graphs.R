###############################Descriptive Graphs####################################

# Pension Spending
pendata <- dataset60[dataset60$sex==0 & dataset60$edu==0, ]

ggplot(data = pendata, aes(x = year, y = penexppc))+
  geom_line(lwd = 0.8)+
  facet_wrap(~country, ncol = 6)+
  labs(x = "Year", y = "Public Pension Spending per person aged 65+ (PPP USD thousands)")+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015))

# pension spending per gdp
ggplot(data = pendata, aes(x = year, y = penexp))+
  geom_line(lwd = 0.8)+
  facet_wrap(~country, ncol = 6)+
  labs(x = "Year", y = "Public Pension Spending, % of GDP")+
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12))+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015))

# employment 60-64
dataset60$sex <- as.factor(dataset60$sex)
dataset60$edu <- as.factor(dataset60$edu)
ggplot(data = dataset60)+
  geom_line(aes(x = year, y = emp, linetype = sex, color = edu), lwd = 0.8)+
  facet_wrap(~country, ncol = 6)+
  labs(x = "Year", y = "Employment Rate, Age 60-64")+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 12))+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015))+
  scale_color_discrete(name="Education", labels = c("low", "mid", "high"))+
  scale_linetype_discrete(name = "Gender", labels = c("Male", "Female"))

# employment 65-69
dataset65$sex <- as.factor(dataset65$sex)
dataset65$edu <- as.factor(dataset65$edu)
ggplot(data = dataset65)+
  geom_line(aes(x = year, y = emp, linetype = sex, color = edu), lwd = 0.8)+
  facet_wrap(~country, ncol = 6)+
  labs(x = "Year", y = "Employment Rate, Age 65-69")+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 12))+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015))+
  scale_color_discrete(labels = c("low", "mid", "high"))+
  scale_linetype_discrete(labels = c("Male", "Female"))

# At-risk of poverty
datasetpov$sex <- as.factor(datasetpov$sex)
ggplot(data = datasetpov)+
  geom_line(aes(x = year, y = poverty, linetype = sex), lwd = 0.8)+
  facet_wrap(~country, ncol = 5)+
  labs(x = "Year", y = "At-risk-of-poverty Rate, Age 65+")+
  theme(axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        strip.text = element_text(size = 12))+
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015))+
  scale_linetype_discrete(labels = c("Male", "Female"))
