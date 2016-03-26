# doit file for Norad project
from __future__ import print_function

# def who(task):
#     print('my name is', task.name)
#     print(task.targets)

# def task_x():
#     return {
#         'actions': [who],
#         'targets': ['asdf'],
#         'verbosity': 2,
#         }

def task_clean_data():
	"""Loads and cleans data"""

	return {
        # "basename": "clean",
        "file_dep": ["prep/clean.R"],
        "targets": ["logs/clean.Rout", "data/cleaned.RData"],
        # run from src hence change directory first and move up on level for tee
        "actions": ["cd prep && Rscript clean.R | tee ../logs/clean.Rout"]
    }

def task_prep_long():
    """Prepares long form of time indexed data"""

    return {
        "file_dep": ["prep/melt.R", "data/cleaned.RData"],
        "targets": ["logs/melt.Rout", "data/clean_long.RData"],
        "actions": ["cd prep && Rscript melt.R | tee ../logs/melt.Rout"]
        }

def task_strobe():
    """Runs data through STROBE style inclusion and exclusion criteria"""

    return {
        "file_dep": ["prep/strobe.R", "data/cleaned.RData"],
        "targets": ["logs/strobe.Rout", "data/strobe.RData"],
        "actions": ["cd prep && Rscript strobe.R | tee ../logs/strobe.Rout"]
        }

def task_table1():
	"""Makes table 1 (for all post STROBE)"""

	return {
        # "uptodate": [False],
        "basename": "table1",
		"file_dep": ["data/strobe.RData", "analysis/table1.R"],
		"targets": [
                    "logs/table1_all.Rout",
                    "logs/table1_ne24.Rout",
                    "logs/table1_morelli.Rout",
                    "write/tables/table1_all.xlsx",
                    "write/tables/table1_ne24.xlsx",
                    "write/tables/table1_morelli.xlsx"
                    ],
        "actions": [
            "cd analysis && Rscript table1.R --subgrp=all | tee ../logs/table1_all.Rout",
            "cd analysis && Rscript table1.R --subgrp=ne24 | tee ../logs/table1_ne24.Rout",
            "cd analysis && Rscript table1.R --subgrp=morelli | tee ../logs/table1_morelli.Rout"
            ]
	}


def task_model_rx():
    """Builds treatment variable models"""

    return {
        "uptodate": [False],
        # "basename": "model_rx",
        "file_dep": ["data/strobe.RData", "analysis/model_rx.R"],
        "targets": [
                    "logs/model_rx_ne.24.Rout",
                    "write/figures/model_rx_ne.24.eps",
                    "write/tables/model_rx_ne.24.xlsx"
                    ],
        "actions": [
            "cd analysis && Rscript model_rx.R --rx=ne.24 | tee ../logs/model_rx_ne.24.Rout"
            ]
    }

def task_model_mortality():
    """Build final mortality model"""

    return {
        "uptodate": [False],
        # "basename": "model_mortality",
        "file_dep": ["data/strobe.RData", "analysis/model_mortality.R"],
        "targets": [
                    "logs/model_mortality.Rout",
                    "write/figures/model_mortality.eps",
                    "write/tables/model_mortality.xlsx"
                    ],
        "actions": [
            "cd analysis && Rscript model_mortality.R | tee ../logs/model_mortality.Rout"
            ]
    }

 

