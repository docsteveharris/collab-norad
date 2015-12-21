# Makefile for paper-spotearly

paper: 

RData: data/cleaned.Rdata
	R CMD BATCH prep/clean.R ../logs/clean.Rout
	echo "Success: data cleaned"