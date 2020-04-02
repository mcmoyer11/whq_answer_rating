#Boxes follow-ups
#last edit: 7/6/17
library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(bootstrap)
library(ggpubr)
library(psych)
library(likert)
library(reshape2)
library(lme4)
library(languageR)
library(ordinal)
library(lmerTest)
library(lattice)
library(boot)
library(rcompanion)
#library(lme4)

#Set the working directory to whereever you have your raw data and the "helpers.R" file
setwd("/Users/morganmoyer/Dropbox/Moyer_research/Embedded_Questions/Dissertation/Experiments/Answer_rating/data")
source("helpers.R")

d <- read.csv("ar_raw.csv")

d$likert.f <- as.factor(d$response)
d$response <-as.numeric(d$response)

high = subset(d, d$trial=="high")
low = subset(d, d$trial=="low")
test = rbind(high, low)
# View(test)


filler = subset(d, d$trial=="filler")
length(d$factors)
length(filler$factors)
str(low)

length(unique(d$subject)) #263
View(high)


msmoma = test %>%
  filter(answer %in% c("MO ", "MS ", "MA "))


# OVERALL EFFECTS
kruskal.test(response ~ modal, data = d)
# Kruskal-Wallis chi-squared = 1.483, df = 1, p-value = 0.2233
kruskal.test(response ~ modal, data = msmoma)
# Kruskal-Wallis chi-squared = 0.68286, df = 1, p-value = 0.4086


kruskal.test(response ~ answer, data = test)
# Kruskal-Wallis chi-squared = 2932.9, df = 3, p-value < 2.2e-16
# pairwise.wilcox.test(test$answer, test$response, p.adjust.method = "BH")
kruskal.test(response ~ answer, data = msmoma)
# Kruskal-Wallis chi-squared = 1890.9, df = 2, p-value < 2.2e-16


kruskal.test(response ~ trial, data = test)
# Kruskal-Wallis chi-squared = 17.947, df = 1, p-value = 2.271e-05
kruskal.test(response ~ trial, data = msmoma)
# Kruskal-Wallis chi-squared = 26.232, df = 1, p-value = 3.027e-07


kruskal.test(response ~ subject, data = test)
# Kruskal-Wallis chi-squared = 351.19, df = 262, p-value = 0.0001897
kruskal.test(response ~ subject, data = msmoma)
# Kruskal-Wallis chi-squared = 465.84, df = 262, p-value = 1.319e-13

####### Interaction stakes X answer
interTT<- interaction(test$trial, test$answer)
kruskal.test(response ~ interTT, data = test)
# ruskal-Wallis chi-squared = 2969.6, df = 7, p-value < 2.2e-16

interTT<- interaction(msmoma$trial, msmoma$answer)
kruskal.test(response ~ interTT, data = msmoma)
# Kruskal-Wallis chi-squared = 1931.3, df = 5, p-value < 2.2e-16


########3 Interaction stakes x answer x modal
interMSA <- interaction(test$trial, test$answer, test$modal)
kruskal.test(response ~interMSA, data = test)
# Kruskal-Wallis chi-squared = 2973.1, df = 15, p-value < 2.2e-16

interMSA <- interaction(msmoma$trial, msmoma$answer, msmoma$modal)
kruskal.test(response ~interMSA, data = msmoma)
# Kruskal-Wallis chi-squared = 1934.2, df = 11, p-value < 2.2e-16


####### Interaction answer X modal
interMA <- interaction(test$answer, test$modal)
kruskal.test(response ~ interMA, data = test)
# Kruskal-Wallis chi-squared = 2935.9, df = 7, p-value < 2.2e-16
interMA <- interaction(msmoma$answer, msmoma$modal)
kruskal.test(response ~ interMA, data = msmoma)
# Kruskal-Wallis chi-squared = 1893.2, df = 5, p-value < 2.2e-16


# within staks
# High
kruskal.test(response ~ story, data = high)
# Kruskal-Wallis chi-squared = 8.6792, df = 7, p-value = 0.2765

kruskal.test(response ~ subject, data = high)
# Kruskal-Wallis chi-squared = 311.79, df = 262, p-value = 0.01878

# Low
kruskal.test(response ~ story, data = low)
# Kruskal-Wallis chi-squared = 16.743, df = 7, p-value = 0.01913

pairwise.wilcox.test(low$response, low$story, p.adjust.method = "BH")

kruskal.test(response ~ subject, data = low)
# Kruskal-Wallis chi-squared = 271.67, df = 262, p-value = 0.3277

ma = subset(test, test$answer=="MA ")
ms = subset(test, test$answer=="MS ")
mo = subset(test, test$answer=="MO ")
msmo = rbind(ms,mo)

kruskal.test(response~trial, data = ma)
# Kruskal-Wallis chi-squared = 0.59823, df = 1, p-value = 0.4393
kruskal.test(response~trial, data = ms)
# Kruskal-Wallis chi-squared = 46.927, df = 1, p-value = 7.37e-12
kruskal.test(response~trial, data = mo)
# Kruskal-Wallis chi-squared = 37.013, df = 1, p-value = 1.174e-09


kruskal.test(response ~ trial, data = msmo)
# Kruskal-Wallis chi-squared = 86.789, df = 1, p-value < 2.2e-16 ***

kruskal.test(response ~ answer, data = msmo)
# Kruskal-Wallis chi-squared = 49.775, df = 1, p-value = 1.724e-12 ***

kruskal.test(response ~ modal, data = msmo)
# Kruskal-Wallis chi-squared = 2.3662, df = 1, p-value = 0.124

##### Interactions
interTT<- interaction(msmo$trial, msmo$answer)
kruskal.test(response ~ interTT, data = msmo)
# Kruskal-Wallis chi-squared = 130.41, df = 3, p-value < 2.2e-16

interTM<- interaction(msmo$trial, msmo$modal)
kruskal.test(response ~ interTM, data = msmo)
# Kruskal-Wallis chi-squared = 89.699, df = 3, p-value < 2.2e-16


interMA <- interaction(msmo$answer, msmo$modal)
kruskal.test(response ~ interMA, data = msmo)
# Kruskal-Wallis chi-squared = 52.146, df = 3, p-value = 2.788e-11

interMSA <- interaction(msmo$trial, msmo$answer, msmo$modal)
kruskal.test(response ~interMSA, data = msmo)
# Kruskal-Wallis chi-squared = 133.84, df = 7, p-value < 2.2e-16


# low and high
mo_low = subset(mo, mo$trial=="low")
kruskal.test(response ~ modal, data = mo_low)
# Kruskal-Wallis chi-squared = 2.976, df = 1, p-value = 0.08451

ms_low = subset(ms, ms$trial=="low")
kruskal.test(response ~ modal, data = ms_low)
# Kruskal-Wallis chi-squared = 0.060803, df = 1, p-value = 0.8052

ma_low = subset(ma, ma$trial=="low")
kruskal.test(response ~ modal, data = ma_low)
# Kruskal-Wallis chi-squared = 1.4824, df = 1, p-value = 0.2234

msmo_low = subset(msmo, msmo$trial=="low")
kruskal.test(response~answer, data = msmo_low)
# Kruskal-Wallis chi-squared = 23.511, df = 1, p-value = 1.242e-06


mo_modal = subset(mo, mo$modal=="modal")
kruskal.test(response ~ trial, data = mo_modal)
# Kruskal-Wallis chi-squared = 28.285, df = 1, p-value = 1.047e-07
mo_nomodal = subset(mo, mo$modal=="nomodal")
kruskal.test(response ~ trial, data = mo_nomodal)
# Kruskal-Wallis chi-squared = 11.255, df = 1, p-value = 0.0007941

ms_modal = subset(ms, ms$modal=="modal")
kruskal.test(response ~ trial, data = ms_modal)
# Kruskal-Wallis chi-squared = 24.81, df = 1, p-value = 6.326e-07
ms_nomodal = subset(ms, ms$modal=="nomodal")
kruskal.test(response ~ trial, data = ms_nomodal)
# Kruskal-Wallis chi-squared = 21.603, df = 1, p-value = 3.353e-06

mo_high = subset(mo, mo$trial=="high")
kruskal.test(response ~ modal, data = mo_high)
# Kruskal-Wallis chi-squared = 0.3145, df = 1, p-value = 0.5749
ms_high = subset(ms, ms$trial=="high")
kruskal.test(response ~ modal, data = ms_high)
# Kruskal-Wallis chi-squared = 0.37243, df = 1, p-value = 0.5417
ma_high = subset(ma, ma$trial=="high")
kruskal.test(response ~ modal, data = ma_high)
# Kruskal-Wallis chi-squared = 0.85917, df = 1, p-value = 0.354
msmo_high = subset(msmo, msmo$trial=="high")
kruskal.test(response~answer, data = msmo_high)
# Kruskal-Wallis chi-squared = 21.514, df = 1, p-value = 3.513e-06



# Is including the model a good way to fit the data?
# including modal as a predictor does not improve the model fit
at = clm(likert.f ~ answer + trial, data = test, link = "probit", Hess=TRUE)
summary(at)
atm = clm(likert.f ~ answer + trial + modal, data = test, link = "probit", Hess = TRUE)
summary(atm)

anova(at,atm)



#gather the individuals to look at that
agr = test %>%
  dplyr::group_by(subject,trial,answer) %>%
  dplyr::summarize(MeanLikert=mean(response)) %>%
  dplyr::group_by(subject) %>%
  dplyr::summarize(CILow=ci.low(MeanLikert),CIHigh=ci.high(MeanLikert)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(YMin=MeanLikert-CILow,YMax=MeanLikert+CIHigh)

#plot all the individuals mean chosen
ggplot(agr, aes(x=reorder(subject,MeanLikert),y=MeanLikert)) +
  geom_bar(stat="identity",color="black",fill="gray80") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25)

