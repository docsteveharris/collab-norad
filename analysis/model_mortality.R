# author: Steve Harris
# date: 2016-03-15
# subject: model mortality (ICU)

# Readme
# ======


# Todo
# ====


# Log
# ===
# 2016-03-15
# - file cloned from model_ne24
library(dsbc)
library(arm)
library(ggplot2)
library(reshape2)

rm(list=ls(all=TRUE))
source(file="../prep/strobe.R")
source(file="../prep/prep_vars.R")
source(file="../share/functions4paper.R")


describe(wdt$sepsis.site)
require(gmodels)
with(wdt, CrossTable(mort.itu, hosp.id.sort))

# Null model
m <- glm(mort.itu ~ 1 , data=wdt)
m <- glm(mort.itu ~ rescale(age) , data=wdt)
coef.plot(m)


# MAP
# Univariate
gg.q    +
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(40,120)) +
    theme_minimal()
m <- glm(mort.itu ~ map.24, data=wdt)
display(m)
m <- glm(mort.itu ~ map.high, data=wdt)
display(m)
gg.q <- ggplot(data=wdt, aes(x=map.24, y=mort.itu))
# Univariate by site
gg.q    +
    # geom_smooth(method="lm") + 
    geom_smooth(method="loess") + 
    facet_grid(. ~ hosp.id.sort) +
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(40,120)) +
    theme_minimal()
m <- glmer(mort.itu ~ map.24 + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)

# Heart rate
# Univariate
gg.q <- ggplot(data=wdt, aes(x=hr.24, y=mort.itu))
gg.q    +
    geom_smooth(method="loess") + 
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(40,120)) +
    theme_minimal()
m <- glm(mort.itu ~ hr.24, data=wdt)
display(m)
m <- glm(mort.itu ~ hr.high, data=wdt)
display(m)
gg.q <- ggplot(data=wdt, aes(x=hr.24, y=mort.itu))
# Univariate by site
gg.q    +
    geom_smooth(method="lm") + 
    # geom_smooth(method="loess") + 
    facet_grid(. ~ hosp.id.sort) +
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(40,120)) +
    theme_minimal()
m <- glmer(mort.itu ~ hr.24 + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)
m <- glmer(mort.itu ~ hr.high + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)

# Noradrenaline
# Univariate
gg.q <- ggplot(data=wdt, aes(x=ne.24, y=mort.itu))
gg.q    +
    geom_smooth(method="loess") + 
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(0,2)) +
    theme_minimal()
m <- glm(mort.itu ~ ne.24, data=wdt)
display(m)
gg.q <- ggplot(data=wdt, aes(x=ne.24, y=mort.itu))
# Univariate by site
gg.q    +
    # geom_smooth(method="lm") + 
    geom_smooth(method="loess") + 
    facet_grid(. ~ hosp.id.sort) +
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(0,2)) +
    theme_minimal()
m <- glmer(mort.itu ~ ne.24 + (1 | hosp.id), data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)

    
gg.q <- ggplot(data=wdt, aes(x=ne.24, y=mort.itu))
gg.q    +
    geom_smooth(method="lm") + 
    # geom_smooth(method="loess") + 
    facet_grid(hr.high ~ hosp.id.sort) +
    geom_rug(data=wdt[mort.itu==1], sides='t', position='jitter', alpha=1/10) +
    geom_rug(data=wdt[mort.itu==0], sides='b', position='jitter', alpha=1/10) +
    coord_cartesian(x=c(0,2)) +
    theme_minimal()

# Full pooling, no centre effects
m <- glm(mort.itu ~
    male +
    rescale(age) +
    sepsis.site +
    rescale(sofa.1) +
    rescale(fb.24) +
    map.high + 
    hr.high + 
    rescale(ne.24) +
    hr.high:rescale(ne.24) +
    hr.high:map.high
    , data=wdt)
display(m)
coef.plot(m)

# - [ ] TODO(2016-03-15): run twice with sofa.1 and sofa.1.nocvs + ne.1
# check that splitting doesn't make a difference to any conclusions

# Multi-level
with(wdt, summary(ne.24-ne.1))
m <- glmer(mort.itu ~
    male +
    rescale(age) +
    sepsis.site +
    rescale(sofa.1.nocvs) +
    rescale(ne.1) +
    rescale(vadi.24) +
    rescale(sedation.24) +
    rescale(fb.24) +
    map.high + 
    hr.high + 
    rescale(ne.24-ne.1) +
    hr.high:rescale(ne.24-ne.1) +
    map.high:rescale(ne.24-ne.1) +
    (1 | hosp.id)
    , data=wdt, family=binomial(link="logit"))
display(m)
coef.plot(m)
