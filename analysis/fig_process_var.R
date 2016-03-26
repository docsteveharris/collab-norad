# author: Steve Harris
# date: 2016-03-09
# subject: Plots to demonstrate variablility in process measures

# Readme
# ======
# All at 24h


# Todo
# ====
# - [ ] norad dose

# - continuous by box-whisker
#     - MAP
#     - fb
# - categorical by mosaic
# suggest dichotomise first
#     - sedation 
#     - vadi
#     - steroids
#     - mv
#     - rrt

# Log
# ===
# 2016-03-09
# - file created

rm(list=ls(all=TRUE))

require(Hmisc)
require(data.table)
require(ggplot2)
require(ggthemes)

setwd("/Users/steve/aor/academic/collab-norad/paper-euroepi/src/analysis")
# Generate working data
source("../prep/strobe.R")
source("../prep/prep_vars.R")
source("../share/functions4paper.R")
# this saves the data to tdt so rename

str(wdt$hosp.id.sort)
wdt[,.(mort=mean(mort.itu),.N),by=.(hosp.id.sort, hosp)]

# MAP
p <- ggplot(data=wdt, aes(y=map.24, x=hosp.id.sort))
p <- p + geom_tufteboxplot(median.type="line", whisker.type="line", hoffset=0, width=5) +
    coord_cartesian(ylim=c(40,100)) +
    ylab("Mean arterial pressure (mmHg)") +
    xlab("Intensive Care Unit") +
    theme_pander()
p
ggsave(p, file="../write/figures/process_map.pdf", width=3, height=3, scale=2)

# Noradrenaline
p <- ggplot(data=wdt, aes(y=ne.24, x=hosp.id.sort))
p <- p + geom_tufteboxplot(median.type="line", whisker.type="line", hoffset=0, width=5) +
	coord_cartesian(ylim=c(0,1.5)) +
	ylab("Noradrenaline dose (mcg/kg/min)") +
	xlab("Intensive Care Unit") +
    theme_pander()
p
ggsave(p, file="../write/figures/process_ne24.pdf", width=3, height=3, scale=2)

# Fluids
p <- ggplot(data=wdt, aes(y=fb.24/1000, x=hosp.id.sort))
p <- p + geom_tufteboxplot(median.type="line", whisker.type="line", hoffset=0, width=5) +
	coord_cartesian(ylim=c(-2.5,12.5)) +
	ylab("Fluid balance (litres)") +
	xlab("Intensive Care Unit") +
    theme_pander()
p
ggsave(p, file="../write/figures/process_fb24.pdf", width=3, height=3, scale=2)


# Sedation
summary(wdt$sedation.24)
tdt <- na.omit(
           wdt[, .(
               hosp.id.sort,
            sedation.24.logical = ifelse(sedation.24 > 0, 1, 0)
            )])
t1.trend("sedation.24.logical", "hosp.id.sort", data=tdt, var.cont=FALSE)
p <- ggMMplot(tdt$hosp.id.sort, !tdt$sedation.24.logical, xlab="Intensive Care Unit", ylab="Sedation")
p
ggsave(p, file="../write/figures/process_sedation.pdf", width=3, height=3, scale=2)


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
t1.trend("vadi.other.24.logical", "hosp.id.sort", data=tdt, var.cont=FALSE)
p <- ggMMplot(tdt$hosp.id.sort, tdt$vadi.other.24.logical, xlab="Intensive Care Unit", ylab="Other vasopressors/inotropes")
p
ggsave(p, file="../write/figures/process_vadi.pdf", width=3, height=3, scale=2)

# Steroids
summary(wdt$rx.roids)
tdt <- na.omit(
           wdt[, .(
                hosp.id.sort,
                rx.roids.logical = ifelse(rx.roids==TRUE, 1, 0)
            )])
t1.trend("rx.roids.logical", "hosp.id.sort", data=tdt, var.cont=FALSE)
p <- ggMMplot(tdt$hosp.id.sort, tdt$rx.roids.logical, xlab="Intensive Care Unit", ylab="Corticosteroids")
p
ggsave(p, file="../write/figures/process_roids.pdf", width=3, height=3, scale=2)

# Mechanical ventilation
summary(wdt$mv.24)
tdt <- na.omit(
           wdt[, .(
                hosp.id.sort,
                mv.24
            )])
t1.trend("mv.24", "hosp.id.sort", data=tdt, var.cont=FALSE)
p <- ggMMplot(tdt$hosp.id.sort, !tdt$mv.24, xlab="Intensive Care Unit", ylab="Mechanical ventilation")
p
ggsave(p, file="../write/figures/process_mv.pdf", width=3, height=3, scale=2)

# Renal replacement therapy
summary(wdt$rrt.24)
tdt <- na.omit(
           wdt[, .(
                hosp.id.sort,
                rrt.24
            )])
p <- ggMMplot(tdt$hosp.id.sort, !tdt$rrt.24, xlab="Intensive Care Unit", ylab="Renal replacement therapy")
p
ggsave(p, file="../write/figures/process_rrt.pdf", width=3, height=3, scale=2)