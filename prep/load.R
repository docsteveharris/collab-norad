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
# 2015-11-24
# - switching to 8 ICUs 20150826.xls
# - using tab delimiter rather than csv b/c of problems with text

rm(list=ls(all=TRUE))


#  =============
#  = Libraries =
#  =============
require(Hmisc)
require(assertthat)
require(gmodels)
# require(descr)
require(data.table)
require(XLConnect)


#  ========
#  = Data =
#  ========

# CHANGED: 2015-04-08 - [ ] switching files
# Original file     ..data/_data_in/6 centres-15-09-14.xls
# Current file      ..data/_data_in/8 ICUs 20150315.xls
warning("Check:\nAssuming ..data/_data_in/8 ICUs 20150326 - corrected.xls is the most current file")
# raw_csv_file = '../data/_data_in/8 ICUs 20150315 - corrected.txt'
raw_tab_file = '../data/_data_in/8 ICUs 20150826 - corrected.txt'
rdf <- read.table(raw_tab_file,
    sep = "\t", quote="\"",
	header=TRUE,
    strip.white=TRUE, stringsAsFactors=FALSE )
nrow(rdf)


# Additional step to merge in n_days_fb
# Import both original and old to check
# Original file     ..data/_data_in/6 centres-15-09-14.xls
raw_tab_file <- "../data/_data_in/6 centres-15-09-14.txt"
rdt.old <- data.table(read.table(raw_tab_file,
    sep = "\t", quote="\"",
    header=TRUE,
    strip.white=TRUE, stringsAsFactors=FALSE ))
str(rdt.old)
describe(rdt.old$n_days_fb)
rdf <- merge(rdf,rdt.old[,.(id,n_days_fb)],all.x=TRUE)
# New updated 8 centre file
raw_tab_file <- "../data/_data_in/8 ICUs 20160309 cumul_FB.txt"
rdt.old <- data.table(read.table(raw_tab_file,
    sep = "\t", quote="\"",
    header=TRUE,
    strip.white=TRUE, stringsAsFactors=FALSE ))
str(rdt.old)
describe(rdt.old$fb_days)
rdf <- merge(rdf,rdt.old[,.(id,fb_days)],all.x=TRUE)
nrow(rdf)
str(rdf, list.len=200)
describe(rdf$fb_days)
rdt <- data.table(rdf)
# we have 24 patients where fluid balance is reported over zero days - should drop
describe(rdt$n_days_fb)
describe(rdt$fb_days)
head(rdt[n_days_fb!=fb_days,.(id, n_days_fb, fb_days)])
head(rdt[fb_days==0,.(id, n_days_fb, fb_days)])



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