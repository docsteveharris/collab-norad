# author: Steve Harris
# date: 2014-11-29
# subject: Load data

# Readme
# ======
# Load / Clean / Functions / Do
# Trying to follow data management as per this response
# http://stackoverflow.com/questions/5365974/writing-functions-vs-line-by-line-interpretation-in-an-r-workflow/5378329#5378329

# Loads rdf (raw data only); wdt will come from cleaned.Rdata and can be loaded separately


# Todo
# ====


# Log
# ===
# 2014-11-29
# - file created
# 2015-04-08
# - switch to using updated file with 8 ICU's

rm(list=ls(all=TRUE))


#  =============
#  = Libraries =
#  =============
require(Hmisc)
require(assertthat)
require(gmodels)
require(descr)
require(data.table)
require(XLConnect)


#  ========
#  = Data =
#  ========

# CHANGED: 2015-04-08 - [ ] switching files
# Original file     ..data/_data_in/6 centres-15-09-14.xls
# Current file      ..data/_data_in/8 ICUs 20150315.xls
warning("Check:\nAssuming ..data/_data_in/8 ICUs 20150315 - corrected.xls is the most current file")
raw_csv_file = '../data/_data_in/8 ICUs 20150315 - corrected.csv'
rdf <- read.table(raw_csv_file,
    sep = ",", quote="\"", header=TRUE,
    strip.white=TRUE, stringsAsFactors=FALSE )
str(rdf)

#  =====================
#  = Generic functions =
#  =====================
# NOTE: 2014-11-29 - [ ] specific analysis and reporting functions do not belong here

# Continuous data checks
check.cont <- function (var) {
    require(Hmisc)
    stem(var)
    describe(var)
}

check.cat <- function(var) {
    require(Hmisc)
    require(gmodels)
    print(CrossTable(var))
    describe(var)
}