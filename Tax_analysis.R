# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list = ls())
memory.size(max=T)
memory.size(max=F)
gc()
memory.size(max=F)

options(scipen = 999)
library(ggplot2)
library(dplyr)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(geepack)
library(stargazer)
library(psych)
library(remotes)
library(modelsummary)
#install.packages('modelsummary')
library(haven)
tax_data <- read_dta("D:/linearReg/tax_data.dta")
View(tax_data)
sapply(tax_data, class)

tax_data %>%
  select(year, fips) %>%
  table()
tax_data %>%
  is.pbalanced()

table(index(tax_data), useNA = "ifany")

#ggplot(data = tax_data, aes(x = year, y = log_gdp)) +
  #geom_line() +
  #labs(x = "Year",  y = "Log Gross Domestic Product") +
 # theme(legend.position = "none")

tax_data %>%
  group_by(state) %>%
  summarise(mean_gdp = mean(log_gdp)) %>%
  left_join(tax_data) %>%
  ggplot(data = ., 
         aes(x = reorder(as.character(state), state), y = log_gdp)) +
  geom_point() +
  geom_line(aes(x = state, y = mean_gdp), col = "blue") +
  labs(x = "State", y = "GDP")


tax_data %>%
  group_by(year) %>%
  summarise(mean_gdp = mean(log_gdp)) %>%
  left_join(tax_data) %>%
  ggplot(data = ., 
         aes(x = year, y = log_gdp)) +
  geom_point() +
  geom_line(aes(x = year, y = mean_gdp), col = "blue") +
  scale_x_continuous(labels = as.character(tax_data$year), 
                     breaks = tax_data$year) +
  labs(x = "Year", y = "GDP") +
  theme(axis.text.x = element_text(angle = 90))
describe<- describe()
describe
stargazer(as.data.frame(tax_data))
stargazer(tax_data, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")


models <- list(
  "OLS"     = lm(log_gdp ~ cit + cit_flag + sal + bal + pinc + ue + inc, data = tax_data ),
  "Pooled OLS PLM" = plm(log_gdp ~ cit + cit_flag + sal + bal + pinc + ue + inc, data = tax_data, index = c("state", "year"), effect = "individual", model = "pooling"),
  "Fixed Effects"     = plm(log_gdp ~ cit + cit_flag + sal + bal + pinc + ue + inc, data = tax_data, index = c("state", "year"), effect = "individual", model = "within"),
  "Random Effects" = plm(log_gdp ~ cit + cit_flag + sal + bal + pinc + ue + inc, data = tax_data, index = c("state", "year"), effect = "individual", model = "random")
  #"Random Effects Clustered"     = coeftest(reg1, vcov = vcovHC(reg1, type = "sss", cluster = "group"))
)
modelsummary(models, output = "table2.tex", stars = TRUE)

  
pooled_ols_plm <- plm(log_gdp ~ cit + cit_flag + sal + bal + pinc + ue + inc, data = tax_data, index = c("state", "year"), effect = "individual", model = "pooling")  
plmtest(pooled_ols_plm, effect = "individual", type = c("bp"))
#The test shows that there are significant differences across firms. Running a pooled OLS regression is thus not appropriate and the RE model is the better choice.

fe_model_plm <- plm(log_gdp ~ cit + cit_flag + sal + bal + pinc + ue + inc, data = tax_data, index = c("state", "year"), effect = "individual", model = "within")

re_model_plm <- plm(log_gdp ~ cit + cit_flag + sal + bal + pinc + ue + inc, data = tax_data, index = c("state", "year"), effect = "individual", model = "random")

phtest(fe_model_plm, re_model_plm)
#The null hypothesis is rejected here, hence we should use a FE model.

lmtest::bptest(log_gdp ~ cit + cit_flag + sal + bal + pinc + ue + inc + factor(state), 
               studentize = F, data = tax_data)
#There is strong evidence for the presense of heteroskedasticity. Hence, the use of robust standard errors is advised.

pbgtest(fe_model_plm)
#There is strong evidence that the residuals are serially correlated.


reg1<-plm(log_gdp ~ cit + cit_flag + sal + bal + pinc + ue + inc, data = tax_data, index = c("state", "year"), effect = "individual", model = "random")
cluster <- coeftest(reg1, vcov = vcovHC(reg1, type = "sss", cluster = "group"))

cluster
reg2 <-  plm(log_gdp ~ cit + cit_flag + sal + bal + pinc + ue + inc, data = tax_data, index = c("state", "year"), effect = "individual", model = "within")
cluster2 <- coeftest(reg2, vcov = vcovHC(reg1, type = "sss", cluster = "group"))
cluster2
stargazer(cluster, cluster2)

est_cl <- felm(log_gdp ~ cit + cit_flag + sal + bal + pinc + ue + inc| id + firm | 0 | cl1 + cl2, data = d)
summary(est_cl)

models2<-list("Clustered Fixed Effects" = cluster2,
              "Clustered Random Effects" = cluster)

modelsummary(models2, output = "table3.tex", stars = TRUE)
