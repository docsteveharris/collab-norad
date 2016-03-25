# author: Steve Harris
# date: 2016-03-09
# subject: Plots to demonstrate variablility in process measures

# Readme
# ======


# Todo
# ====
# - [ ] heart rate
# - [ ] blood pressure
# - [ ] norad dose
# - [ ] first 24 h fluids
# - [ ] sedation
# - [ ] steroids
# - [ ] vadi


# Log
# ===
# 2016-03-09
# - file created

require(Hmisc)
require(data.table)
require(ggplot2)

rm(list=ls(all=TRUE))
load(file='../data/cleaned.RData')

# Generate working data
source("../prep/prep_vars.R")
source("../prep/strobe.R")
# this saves the data to tdt so rename
wdt <- tdt
str(wdt)

str(wdt$hosp.id.sort)

# - [ ] NOTE(2016-03-15): dropping this b/c not a treatment variable
# Heart rate
# p <- ggplot(data=wdt, aes(y=hr.24, x=hosp.id.sort))
# p <- p + geom_boxplot(notch=TRUE, varwidth=TRUE) +
# 	coord_cartesian(ylim=c(40,160)) +
# 	ylab("Heart rate (per min)") +
# 	xlab("Intensive Care Unit") +
# 	theme_minimal()
# p
# ggsave(p, file="../write/figures/process_hr.pdf", width=3, height=3, scale=2)

# MAP
p <- ggplot(data=wdt, aes(y=map.24, x=hosp.id.sort))
p <- p + geom_boxplot(notch=TRUE, varwidth=TRUE) +
	coord_cartesian(ylim=c(40,100)) +
	ylab("Mean arterial pressure (mmHg)") +
	xlab("Intensive Care Unit") +
	theme_minimal()
p
ggsave(p, file="../write/figures/process_map.eps", width=3, height=3, scale=2)

# Noradrenaline
p <- ggplot(data=wdt, aes(y=ne.24, x=hosp.id.sort))
p <- p + geom_boxplot(notch=TRUE, varwidth=TRUE) +
	coord_cartesian(ylim=c(0,2)) +
	ylab("Noradrenaline dose (mcg/kg/min)") +
	xlab("Intensive Care Unit") +
	theme_minimal()
p
ggsave(p, file="../write/figures/process_ne24.eps", width=3, height=3, scale=2)

# Fluids
p <- ggplot(data=wdt, aes(y=fb.24, x=hosp.id.sort))
p <- p + geom_boxplot(notch=TRUE, varwidth=TRUE) +
	coord_cartesian(ylim=c(-2500,12500)) +
	ylab("Fluid balance (mls)") +
	xlab("Intensive Care Unit") +
	theme_minimal()
p
ggsave(p, file="../write/figures/process_fb24.eps", width=3, height=3, scale=2)

# # Sedation
# p <- ggplot(data=wdt, aes(y=sedation.24, x=hosp.id.sort))
# p + geom_boxplot(notch=TRUE) +
# 	coord_cartesian(ylim=c(0,10)) +
# 	theme_minimal()
	

# Mosaic plot with ggplot function
ggMMplot <- function(var1, var2, ylab=""){
  require(ggplot2)
  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))

  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2

  ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "White") +
    scale_fill_grey(start=0,end=0.9) +
    ylab(ylab) +
    xlab("Intensive Care Unit") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels=NULL) +
    geom_text(aes(label = as.character(var1), x = var1Center, y = -0.05, size=0.5)) +
    guides(fill="none", size="none") +
    coord_fixed(ratio=1) +
    theme_pander()
}

# Sedation
summary(wdt$sedation.24)
tdt <- na.omit(
           wdt[, .(
               hosp.id.sort,
            sedation.24.logical = ifelse(sedation.24 > 0, 1, 0)
            )])
p <- ggMMplot(tdt$hosp.id.sort, !tdt$sedation.24.logical, ylab="Sedation")
p


# VADI
# - [ ] TODO(2016-03-24): remove norad from VADI score or calc by hand
summary(wdt$vadi.24)
summary(wdt$ne.24)
# Hand calculate vadi without norad to estimate use of other vasopressors
tdt <- na.omit(
           wdt[, .(
                hosp.id.sort,
                vadi.24,
                ne.24,
                vadi.other.24 = 
                    ifelse(ne.24 > 0.3, vadi.24 - 3,
                    ifelse(ne.24 > 0.1, vadi.24 - 2,
                    ifelse(ne.24 > 0.0, vadi.24 - 1, vadi.24))))])
tdt[, vadi.other.24.logical := ifelse(vadi.other.24 > 0, 1, 0)]
p <- ggMMplot(tdt$hosp.id.sort, tdt$vadi.other.24.logical, ylab="Other vasopressors/inotropes")
p

# Steroids
summary(wdt$rx.roids)
tdt <- na.omit(
           wdt[, .(
                hosp.id.sort,
                rx.roids.logical = ifelse(rx.roids==TRUE, 1, 0)
            )])
p <- ggMMplot(tdt$hosp.id.sort, tdt$rx.roids.logical, ylab="Corticosteroids")
p

# Mechanical ventilation
summary(wdt$mv.24)
tdt <- na.omit(
           wdt[, .(
                hosp.id.sort,
                mv.24
            )])
p <- ggMMplot(tdt$hosp.id.sort, !tdt$mv.24, ylab="Mechanical ventilation")
p

# Renal replacement therapy
summary(wdt$rrt.24)
tdt <- na.omit(
           wdt[, .(
                hosp.id.sort,
                rrt.24
            )])
p <- ggMMplot(tdt$hosp.id.sort, !tdt$rrt.24, ylab="Renal replacement therapy")
p