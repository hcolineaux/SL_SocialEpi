#-------------------------#
## Author: H Colineaux
# Date commenced: sept 2019
# Date finalised: jan 2025
#-------------------------#

rm(list= objects())
graphics.off()

# 0. preprocess ----
#============#

## paths -----
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)
ifelse(dir.exists("../figures"), "", dir.create("../figures"))
ifelse(dir.exists("../figures/DAC"), "", dir.create("../figures/DAC"))
ifelse(dir.exists("../figures/IAC"), "", dir.create("../figures/IAC"))

## packages -----
library(readr)
library(tidyverse)
library(stringr)
library(questionr)
library(GGally)
library(finalfit)
library(data.table)
library(magrittr)
library(kableExtra)
library(expss)
# install.packages("https://www.bnlearn.com/releases/bnlearn_4.9.tar.gz", repos = NULL, type = "source")
library(bnlearn)
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("Rgraphviz")
library(Rgraphviz)
library(dagitty)

## data-----
SIRS <- read_delim("../data/SIRS.csv",";", escape_double = FALSE, trim_ws = TRUE)

## variables and labels -----

# String format
SIRS[,1:36]=lapply(SIRS[,1:36], as.factor)
tot = SIRS[,c(1:7,10,13,15,16,20,21,26)]
rm(SIRS)

# variable DAC
tot$DAC= ifelse(tot$frequency_GP_consultation=="0",
                ifelse(tot$frequency_DAS_consultation=="0","0","1"),"1")
tot$DAC = as.factor(tot$DAC)
tot = tot[,c(1:11,14,15)] 

# rename variables 
names(tot)= c("Age","Gender","Origin","Education level",
              "Employment status","Income","Health insurance status",
              "Health relatives","Social integration","Chronic Disease",
              "Perceived health status","Indirect access to care","Direct access to care")

# recode
tot$"Health insurance status" = ifelse(tot$"Health insurance status"=="1","0","1")
tot$"Perceived health status" = ifelse(tot$"Perceived health status"=="3","2",tot$"Perceived health status")

#label
tot$Age = factor(tot$Age, labels = c("18-29", "30-44", "45-59","60-74","75+"))
tot$Gender = factor(tot$Gender, labels = c("Men", "Women"))
tot$Origin = factor(tot$Origin, labels = c("French, french par.","French, foreign par.","Migrant"))
tot$"Education level" = factor(tot$"Education level", labels = c("Primary or none","Secondary","Tertiary"))
tot$"Employment status" = factor(tot$"Employment status", labels = c("Employed","Unemployed","Inactived"))
tot$Income = factor(tot$Income, labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
tot$"Health insurance status" = factor(tot$"Health insurance status", labels = c("SHI+VHI", "spec/SHI only/none"))
tot$"Health relatives" = factor(tot$"Health relatives", labels = c("No","Yes"))
tot$"Social integration" = factor(tot$"Social integration", labels = c("Q1", "Q2", "Q3", "Q4"))
tot$"Chronic Disease" = factor(tot$"Chronic Disease", labels = c("No","Yes"))
tot$"Perceived health status" = factor(tot$"Perceived health status", labels = c("Good","Bad-Average"))
tot$"Indirect access to care" = factor(tot$"Indirect access to care", labels = c("No","Yes"))
tot$"Direct access to care" = factor(tot$"Direct access to care", labels = c("No","Yes"))

# I. DAC  -----
#============#
datad = tot[,c(1:11,13)] 

## I.A. Non-Automated -----
fit <- glm(datad$`Direct access to care` ~ datad$Age + datad$Gender + datad$Origin + 
             datad$`Education level` + datad$`Employment status` + datad$Income +
             datad$`Health insurance status` + datad$`Health relatives` + 
             datad$`Social integration` + datad$`Chronic Disease` + 
             datad$`Perceived health status`, family = binomial(link = logit))
summary(fit)
summary(step(fit, direction = "backward"))

fit_fin <- glm(datad$`Direct access to care` ~ datad$Age + datad$Gender + 
             datad$`Health insurance status` + datad$`Chronic Disease` + 
             datad$`Perceived health status`, family = binomial(link = logit))
exp(cbind("Odds ratio" = coef(fit_fin), confint.default(fit_fin, level = 0.95)))

### Initial -----
dag_ini = matrix(c(    "Age", "Education level",
                       "Gender", "Education level",
                       "Origin", "Education level",
                       
                       "Age", "Employment status",
                       "Gender", "Employment status",
                       "Origin", "Employment status",
                       "Education level", "Employment status",
                       
                       "Age", "Income",
                       "Gender", "Income",
                       "Origin", "Income",
                       "Education level", "Income",               
                       "Employment status", "Income",
                       
                       "Age", "Health insurance status",
                       "Gender", "Health insurance status",
                       "Origin", "Health insurance status",
                       "Income", "Health insurance status",               
                       "Employment status", "Health insurance status", 
                       
                       "Age", "Health relatives",
                       "Gender", "Health relatives",
                       "Origin", "Health relatives",
                       "Education level", "Health relatives",
                       
                       "Age", "Social integration",
                       "Gender", "Social integration",
                       "Origin", "Social integration",
                       "Education level", "Social integration",                
                       "Employment status", "Social integration", 
                       
                       "Age", "Chronic Disease",
                       "Gender", "Chronic Disease",
                       "Origin", "Chronic Disease",
                       "Education level", "Chronic Disease",                
                       "Income", "Chronic Disease",
                   
                       "Age", "Perceived health status",
                       "Gender", "Perceived health status",
                       "Origin", "Perceived health status",
                       "Employment status", "Perceived health status",
                       "Income", "Perceived health status",
                       "Chronic Disease", "Perceived health status",
                       "Social integration", "Perceived health status",
                       
                       "Age", "Direct access to care",
                       "Gender", "Direct access to care",
                       "Origin", "Direct access to care",
                       "Education level", "Direct access to care",     
                       "Employment status", "Direct access to care",     
                       "Income", "Direct access to care",     
                       "Health insurance status", "Direct access to care",
                       "Health relatives","Direct access to care",
                       "Social integration", "Direct access to care",
                       "Perceived health status", "Direct access to care",               
                       "Chronic Disease", "Direct access to care"),
                   ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))

dagg_ini = empty.graph(names(datad))
arcs(dagg_ini)=dag_ini

jpeg("../figures/DAC/0.DAG_ini.jpg", width = 800, height = "800")
graphviz.plot(dagg_ini, 
              main = "Initial network",
              shape="rectangle", 
              fontsize = 15,
              highlight = list(#arcs = dag2,#
                nodes="Direct access to care"))
dev.off()

### Final -----
dag_final = matrix(c( "Age", "Education level",
                  "Gender", "Education level",
                  "Origin", "Education level",
                  
                  "Age", "Employment status",
                  "Gender", "Employment status",
                  "Origin", "Employment status",
                  "Education level", "Employment status",
                  
                  "Age", "Income",
                  "Gender", "Income",
                  "Origin", "Income",
                  "Education level", "Income",               
                  "Employment status", "Income",
                  
                  "Age", "Health insurance status",
                  "Gender", "Health insurance status",
                  "Origin", "Health insurance status",
                  "Income", "Health insurance status",               
                  "Employment status", "Health insurance status", 
                  
                  "Origin", "Health relatives",
                  "Education level", "Health relatives",
                  
                  "Age", "Social integration",
                  "Gender", "Social integration",
                  "Education level", "Social integration",                
                  "Employment status", "Social integration", 
                  
                  "Age", "Chronic Disease",
                  "Origin", "Chronic Disease",
                  
                  "Age", "Perceived health status",
                  "Origin", "Perceived health status",
                  "Employment status", "Perceived health status",
                  "Income", "Perceived health status",
                  "Chronic Disease", "Perceived health status",
                  "Social integration", "Perceived health status",
                  
                  "Age", "Direct access to care",
                  "Gender", "Direct access to care",
                  "Health insurance status", "Direct access to care",
                  "Perceived health status", "Direct access to care",               
                  "Chronic Disease", "Direct access to care"),
              ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))

dagg_final = empty.graph(names(datad))
arcs(dagg_final)=dag_final

jpeg("../figures/DAC/0.DAG_final.jpg", width = 800, height = "800")
graphviz.plot(dagg_final, 
              main = "Final network",
              shape="rectangle", 
              fontsize = 15,
              highlight = list(#arcs = dag2,#
                nodes="Direct access to care"))
dev.off()

jpeg("../figures/DAC/0.DAG_ini_to_final.jpg", width = 800, height = "600")
set.seed(12345)
graphviz.plot(dagg_ini, 
              main = "",
              shape="rectangle", 
              fontsize = 15,
              highlight = list(arcs = dag_final, col = "black", lwd=2, lty="solid"))
dev.off()

fit = bn.fit(dagg_ini, as.data.frame(datad))
AIC_dag2 = AIC(fit, as.data.frame(datad))

## I.B DATA DRIVEN -----

### Hill-climbing -----

set.seed(12345)
arcs.hc = boot.strength(as.data.frame(datad), 
                        algorithm = "hc",
                        R=1000, 
                        algorithm.args = list(score="bic"))
# plot(arcs.hc)

arcs.hc_2= arcs.hc[(arcs.hc$strength >=0.05), ]
avg.arcs.hc_2 = averaged.network(arcs.hc_2, threshold=0.05)

jpeg("../figures/DAC/Hill_climbing_DataDriven.jpg",width = 800, height = "600")
strength.plot(avg.arcs.hc_2,arcs.hc_2, 
              fontsize = 15,
              shape="rectangle",
              highlight = list(#arcs = dag2,#
                nodes="Direct access to care"))
dev.off()

arcs.hc_3= arcs.hc[(arcs.hc$strength >=0.00), ]
avg.arcs.hc_3 = averaged.network(arcs.hc_3, threshold=0.00)
arrows_table.hc = subset(arcs.hc_3,from=="Direct access to care" | to=="Direct access to care")
arrows_table.hc %>%  
  writexl::write_xlsx(.,"../figures/DAC/Hill_climbing_DataDriven.xlsx")

### Interleaved Incremental Association -----

set.seed(12345)
arcs.iamb = boot.strength(as.data.frame(datad), 
                          algorithm = "inter.iamb",
                          R=1000)
arcs.iamb_2 = arcs.iamb[(arcs.iamb$strength >=0.05), ]
avg.arcs.iamb_2 = averaged.network(arcs.iamb_2, threshold=0.05)

jpeg("../figures/DAC/IAMB_DataDriven.jpg",width = 800, height = "600")
strength.plot(avg.arcs.iamb_2,arcs.iamb_2, 
              fontsize = 15,
              shape="rectangle",
              highlight = list(#arcs = dag2,#
                nodes="Direct access to care"))
dev.off()

# fit.iamb = bn.fit(avg.arcs.iamb_2, as.data.frame(datad))
# fit_table = data.frame(fit.iamb[["Direct access to care"]][["prob"]])

arcs.iamb_3 = arcs.iamb[(arcs.iamb$strength >=0.00), ]
arrows_table.iamb = subset(arcs.iamb_3,from=="Direct access to care" | to=="Direct access to care")
arrows_table.iamb %>%  
  writexl::write_xlsx(.,"../figures/DAC/IAMB_DataDriven.xlsx")

### ARACNE -----

set.seed(12345)
arcs.arac = boot.strength(as.data.frame(datad), 
                          algorithm = "aracne",
                          R=1000)
arcs.arac_2 = arcs.arac[(arcs.arac$strength >=0.05),]
avg.arcs.arac_2 = averaged.network(arcs.arac_2, threshold=0.05)

jpeg("../figures/DAC/ARACNE_DataDriven.jpg",width = 800, height = "600")
strength.plot(avg.arcs.arac_2,arcs.arac_2, 
              fontsize = 15,
              shape="rectangle",
              highlight = list(#arcs = dag2,#
                nodes="Direct access to care"))
dev.off()

arcs.arac_3 = arcs.arac[(arcs.arac$strength >=0.00),]
arrows_table.aracne = subset(arcs.arac_3,from=="Direct access to care" | to=="Direct access to care")
arrows_table.aracne %>%  
  writexl::write_xlsx(.,"../figures/DAC/ARACNE_DataDriven.xlsx")

## I.C CONSTRAINED -----

### blacklist -----
bl = matrix(c( "Gender", "Age",
               "Education level", "Age",
               "Origin", "Age",
               "Employment status", "Age",
               "Income", "Age",
               "Health insurance status", "Age",
               "Health relatives", "Age",
               "Social integration", "Age",
               "Chronic Disease", "Age",
               "Perceived health status", "Age",
               "Direct access to care", "Age",
               "Age", "Gender",
               "Education level", "Gender",
               "Origin", "Gender",
               "Employment status", "Gender",
               "Income", "Gender",
               "Health insurance status", "Gender",
               "Health relatives", "Gender",
               "Social integration", "Gender",
               "Chronic Disease", "Gender",
               "Perceived health status", "Gender",
               "Direct access to care", "Gender",
               "Age", "Origin",
               "Education level", "Origin",
               "Gender", "Origin",
               "Employment status", "Origin",
               "Income", "Origin",
               "Health insurance status", "Origin",
               "Health relatives", "Origin",
               "Social integration", "Origin",
               "Chronic Disease", "Origin",
               "Perceived health status", "Origin",
               "Direct access to care", "Origin",
               "Income","Employment status",
               "Income","Education level",
               "Employment status","Education level",
               "Health insurance status","Income",
               "Health insurance status","Employment status",
               "Health insurance status","Education level",
               "Health insurance status","Health relatives"),
            ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))

### Hill-Climbing -----

set.seed(12345)
arcs.hc2 = boot.strength(as.data.frame(datad), 
                        algorithm = "hc",
                        R=1000, 
                        algorithm.args = list(score="bic", blacklist = bl))
arcs.hc_2.2= arcs.hc2[(arcs.hc2$strength >=0.05), ]
avg.arcs.hc_2.2 = averaged.network(arcs.hc_2.2, threshold=0.05)

jpeg("../figures/DAC/Hill_climbing_Constrained.jpg",width = 800, height = "600")
strength.plot(avg.arcs.hc_2.2,arcs.hc_2.2, 
              fontsize = 15,
              shape="rectangle",
              highlight = list(#arcs = dag2,#
                nodes="Direct access to care"))
dev.off()

arcs.hc_3.2= arcs.hc2[(arcs.hc2$strength >=0.0), ]
arrows_table.hc.2 = subset(arcs.hc_3.2,from=="Direct access to care" | to=="Direct access to care")
arrows_table.hc.2 %>%  
  writexl::write_xlsx(.,"../figures/DAC/Hill_climbing_Constrained.xlsx")

### Interleaved Incremental Association -----

set.seed(12345)
arcs.iamb2 = boot.strength(as.data.frame(datad), 
                          algorithm = "inter.iamb",
                          R=1000,
                          algorithm.args = list(blacklist = bl))
arcs.iamb_2.2 = arcs.iamb2[(arcs.iamb2$strength >=0.05), ]
avg.arcs.iamb2_2. = averaged.network(arcs.iamb_2.2, threshold=0.05)

jpeg("../figures/DAC/IAMB_Constrained.jpg",width = 800, height = "600")
strength.plot(avg.arcs.iamb2_2.,arcs.iamb_2.2, 
              fontsize = 15,
              shape="rectangle",
              highlight = list(#arcs = dag2,#
                nodes="Direct access to care"))
dev.off()

arcs.iamb_3.2 = arcs.iamb2[(arcs.iamb2$strength >=0.0), ]

arrows_table.iamb.2 = subset(arcs.iamb_3.2,from=="Direct access to care" | to=="Direct access to care")
arrows_table.iamb.2 %>%  
  writexl::write_xlsx(.,"../figures/DAC/IAMB_Constrained.xlsx")


# I.D. Synthetic graph


# II. IAC -----
#============#

data = tot[,c(1:12)] 

## II.A CLASSICAL -----

fit <- glm(data$`Indirect access to care` ~ data$Age + data$Gender + data$Origin + 
             data$`Education level` + data$`Employment status` + data$Income +
             data$`Health insurance status` + data$`Health relatives` + 
             data$`Social integration` + data$`Chronic Disease` + 
             data$`Perceived health status`, family = binomial(link = logit))
summary(fit)
summary(step(fit, direction = "backward"))

fit_fin <- glm(data$`Indirect access to care` ~ data$Age + data$Gender + 
                 data$`Education level` + data$Income + data$`Health relatives` +
                 data$`Health insurance status` + data$`Chronic Disease` + 
                 data$`Perceived health status`, family = binomial(link = logit))
exp(cbind("Odds ratio" = coef(fit_fin), confint.default(fit_fin, level = 0.95)))


## II.B DATA DRIVEN -----
### Hill-climbing -----

set.seed(12345)
arcs.hc = boot.strength(as.data.frame(data), 
                        algorithm = "hc",
                        R=100, 
                        algorithm.args = list(score="bic"))
arcs.hc= arcs.hc[(arcs.hc$strength >=0.05), ]
avg.arcs.hc = averaged.network(arcs.hc, threshold=0.05)


strength.plot(avg.arcs.hc,arcs.hc, 
              shape="rectangle",
              highlight = list(#arcs = dag2,#
                nodes="Indirect access to care"))

arrows_table = subset(arcs.hc,from=="Indirect access to care" | to=="Indirect access to care")
arrows_table= kable(arrows_table,
                    row.names=FALSE,
                    booktabs = T,
                    linesep = "",
                    caption ="Strength and direction of the arcs connected to 'DAC'")
kable_styling(arrows_table, font_size = 8, latex_options = "hold_position")


### Interleaved Incremental Association -----

set.seed(12345)
arcs.iamb = boot.strength(as.data.frame(data), 
                          algorithm = "inter.iamb",
                          R=100)
arcs.iamb = arcs.iamb[(arcs.iamb$strength >=0.05), ]
avg.arcs.iamb = averaged.network(arcs.iamb, threshold=0.05)

strength.plot(avg.arcs.iamb,arcs.iamb, 
              shape="rectangle",
              highlight = list(#arcs = dag2,#
                nodes="Indirect access to care"))

arrows_table = subset(arcs.iamb,from=="Indirect access to care" | to=="Indirect access to care")
arrows_table= kable(arrows_table, 
                    row.names=FALSE, 
                    booktabs = T,
                    linesep = "", 
                    caption ="Strength and direction of the arcs connected to 'DAC'")
kable_styling(arrows_table, font_size = 8, latex_options = "hold_position")


### ARACNE-----

set.seed(12345)
arcs.arac = boot.strength(as.data.frame(data), 
                          algorithm = "aracne",
                          R=100)
arcs.arac = arcs.arac[(arcs.arac$strength >=0.05),]
avg.arcs.arac = averaged.network(arcs.arac, threshold=0.05)

strength.plot(avg.arcs.arac,arcs.arac,
              shape="rectangle",
              highlight = list(#arcs = dag2,#
                nodes="Indirect access to care"))

arrows_table = subset(arcs.arac,from=="Indirect access to care" | to=="Indirect access to care")
arrows_table= kable(arrows_table,
                    row.names=FALSE,
                    booktabs = T,
                    linesep = "",
                    caption ="Strength of the arcs connected to 'DAC'")
kable_styling(arrows_table, font_size = 8, latex_options = "hold_position")


## II.C CONSTRAINED -----

### blacklist -----
bl = matrix(c( "Gender", "Age",
               "Education level", "Age",
               "Origin", "Age",
               "Employment status", "Age",
               "Income", "Age",
               "Health insurance status", "Age",
               "Health relatives", "Age",
               "Social integration", "Age",
               "Chronic Disease", "Age",
               "Perceived health status", "Age",
               "Indirect access to care", "Age",
               "Age", "Gender",
               "Education level", "Gender",
               "Origin", "Gender",
               "Employment status", "Gender",
               "Income", "Gender",
               "Health insurance status", "Gender",
               "Health relatives", "Gender",
               "Social integration", "Gender",
               "Chronic Disease", "Gender",
               "Perceived health status", "Gender",
               "Indirect access to care", "Gender",
               "Age", "Origin",
               "Education level", "Origin",
               "Gender", "Origin",
               "Employment status", "Origin",
               "Income", "Origin",
               "Health insurance status", "Origin",
               "Health relatives", "Origin",
               "Social integration", "Origin",
               "Chronic Disease", "Origin",
               "Perceived health status", "Origin",
               "Indirect access to care", "Origin",
               "Income","Employment status",
               "Income","Education level",
               "Employment status","Education level",
               "Health insurance status","Income",
               "Health insurance status","Employment status",
               "Health insurance status","Education level",
               "Health insurance status","Health relatives"),
            ncol = 2, byrow = TRUE, dimnames = list(NULL, c("from", "to")))

### Hill-Climbing -----

set.seed(12345)
arcs.hc2 = boot.strength(as.data.frame(data), 
                         algorithm = "hc",
                         R=100, 
                         algorithm.args = list(score="bic", blacklist = bl))
arcs.hc2= arcs.hc2[(arcs.hc2$strength >=0.05), ]
avg.arcs.hc2 = averaged.network(arcs.hc2, threshold=0.05)

strength.plot(avg.arcs.hc2,arcs.hc2, 
              shape="rectangle",
              highlight = list(#arcs = dag2,#
                nodes="Indirect access to care"))


arrows_table = subset(arcs.hc2,from=="Indirect access to care" | to=="Indirect access to care")
arrows_table= kable(arrows_table, 
                    row.names=FALSE, 
                    booktabs = T,
                    linesep = "", 
                    caption ="Strength and direction of the arcs connected to 'DAC'")
kable_styling(arrows_table, 
              font_size = 8,
              latex_options = "hold_position")

### Interleaved Incremental Association -----

set.seed(12345)
arcs.iamb2 = boot.strength(as.data.frame(data), 
                           algorithm = "inter.iamb",
                           R=100,
                           algorithm.args = list(blacklist = bl))
arcs.iamb2 = arcs.iamb2[(arcs.iamb2$strength >=0.05), ]
avg.arcs.iamb2 = averaged.network(arcs.iamb2, threshold=0.05)


strength.plot(avg.arcs.iamb2,arcs.iamb2, 
              shape="rectangle",
              highlight = list(#arcs = dag2,#
                nodes="Indirect access to care"))

arrows_table = subset(arcs.iamb2,from=="Indirect access to care" | to=="Indirect access to care")
arrows_table= kable(arrows_table, 
                    row.names=FALSE, 
                    booktabs = T,
                    linesep = "", 
                    caption ="Strength and direction of the arcs connected to 'DAC'")
kable_styling(arrows_table, font_size = 8, latex_options = "hold_position")



