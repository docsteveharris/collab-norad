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
library(arm)
library(ggplot2)
library(reshape2)

rm(list=ls(all=TRUE))
load(file='../data/cleaned.Rdata')
source(file="../prep/strobe.R")
source(file="../prep/prep_vars.R")
source(file="../share/functions4paper.R")

# Model resitant septic shock