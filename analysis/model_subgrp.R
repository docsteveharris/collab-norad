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

# Mosaic plot with ggplot function
ggMMplot <- function(var1, var2, palette="YlOrRd"){
  require(ggplot2)

  # Developing
  # var1 <- wdt$hosp.id.sort
  # var2 <- wdt$grp

  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))

  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$marginVar2 <- prop.table(table(var2))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2

  # Define label positions on LEFT (y-axis)
  ylabData <- plotData[plotData$var1==levels(plotData$var1)[1],]
  dd <- (y=c(0, cumsum(ylabData$var2Height)))
  ylabData$ylabCenter <- sapply(1:(length(dd)-1), function(x) dd[x] + (dd[x+1] - dd[x])/2 )
  print(ylabData)

  # Define label positions on the BOTTOM (x-axis)
  xlabData <- plotData[plotData$var2==levels(plotData$var2)[1],]
  dd <- (x=c(0, cumsum(xlabData$marginVar1)))
  xlabData$xlabCenter <- sapply(1:(length(dd)-1), function(x) dd[x] + (dd[x+1] - dd[x])/2 )
  print(xlabData)

  ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "White") +
    scale_fill_brewer(type="seq", palette=palette, guide=FALSE) +
    # xlabels
    geom_text(data=xlabData,
              aes(label = as.character(var1), x = xlabCenter, y = -0.05),
              vjust="inward") +
    # ylabels
    geom_text(data=ylabData, 
              aes(label = as.character(var2), y = ylabCenter, x = -0.05),
                  vjust="top", angle=90) +
    xlab("") + scale_x_discrete(labels=NULL) +
    ylab("") + scale_y_discrete(labels=NULL) +
    theme_minimal() +
    theme(plot.margin=margin(rep(20,4)))
}


nrow(wdt)
wdt[, grp := 1]
wdt[, grp := ifelse(grp.ne24 == 1, 2, grp)]
wdt[, grp := ifelse(grp.morelli == 1, 3, grp)]
wdt[, grp := factor(grp, levels=c(1,2,3), labels=c("tachycardic-shock", "resistant-shock", "shock"), ordered=TRUE)]
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