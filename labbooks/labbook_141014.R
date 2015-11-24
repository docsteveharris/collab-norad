# Test the R YAML package

library(data.table) # NB setnames and setorder only exist in >v1.9
library(yaml)

rm(list=ls(all=TRUE))
load(file='../data/working.RData')
wdt.original <- wdt
str(wdt)

dict.fields <- yaml.load_file('dictionary_fields.yml')
str(dict.fields)
dict.fields[[1]]

