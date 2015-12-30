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

	print("Cleaning data")

	return {
        # "basename": "clean",
        "file_dep": ["prep/clean.R"],
        "targets": ["logs/clean.Rout", "data/cleaned.RData"],
        "actions": ["R CMD BATCH prep/clean.R ../logs/clean.Rout"]
	}

def task_prep_long():
    """Prepares long form of time indexed data"""

    print("Preparing long form of time indexed data")

    return {
        "file_dep": ["prep/melt.R", "data/cleaned.RData"],
        "targets": ["logs/melt.Rout", "data/clean_long.RData"],
        "actions": ["R CMD BATCH prep/melt.R ../logs/melt.Rout"]
        }

def task_table1_all():
	"""Makes table 1"""

	return {
        "basename": "table1",
		"file_dep": ["data/cleaned.RData", "analysis/table1_all.R"],
		"targets": ["logs/table1_all.Rout", "outputs/tables/table1_all.xlsx"],
        "actions": ["R CMD BATCH analysis/table1_all.R ../logs/table1_all.Rout"]
	}

