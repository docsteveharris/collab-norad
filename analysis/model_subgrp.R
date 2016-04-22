# author: Steve Harris
# date: 2016-03-17
# subject: model subgrp membership 

# Readme
# ======


# Todo
# ====


# Log
# ===
# 2016-03-17
# - file created
library(dsbc)
library(Hmisc)
library(arm)
library(ggplot2)
library(reshape2)

rm(list=ls(all=TRUE))
load(file='../data/cleaned.Rdata')

source(file="../prep/strobe.R")
source(file="../prep/prep_vars.R")
source(file="../share/functions4paper.R")

wdt[,.(mort=mean(mort.itu),.N),by=.(hosp.id.sort, hosp)]
with(wdt, table(hosp.id.sort))



nrow(wdt)
wdt[, grp := 1]
wdt[, grp := ifelse(grp.ne24 == 1, 2, grp)]
wdt[, grp := ifelse(grp.morelli == 1, 3, grp)]
wdt[, grp := factor(grp, levels=c(3,2,1), labels=c("shock", "resistant-shock", "tachycardic-shock"), ordered=TRUE)]
with(wdt, table(hosp.id.sort, grp))

gg_p <- with(wdt, ggMMplot(hosp.id.sort, grp, palette="Greys"))
gg_p <- gg_p + xlab("ICU") + ylab("Shock subgroup")
gg_p
wdt[,.(mort=mean(mort.itu),.N),by=.(hosp.id.sort, hosp)]
ggsave(gg_p, file="../write/figures/model_subgrp_mosaic.jpg", width=6, height=6)
ggsave(gg_p, file="../write/figures/model_subgrp_mosaic.eps", width=6, height=6)

# Single level
m <- glm(grp.ne24 ~
  	age +
    male +
    rescale(age) +
    sepsis.site +
    rescale(sofa.1) +
    (1)
    , data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)

# Multi-level
# Model resistant septic shock
describe(wdt$grp.ne24)
str(wdt)
m <- glmer(grp.ne24 ~
    male +
    rescale(age) +
    sepsis.site +
    rescale(sofa.1) +
    (1 | hosp.id)
    , data=wdt, family=binomial(link="logit"))
(m.var.ranef <- summary(m)$varcor$hosp.id[1])
(m.icc <- m.var.ranef / (m.var.ranef + (pi^2)/3))
str(se.ranef(m))
display(m)
coef.plot(m)
# linearised ICC after logistic model


# Extract ICC or equivalent from model