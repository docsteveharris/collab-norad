# doit file for Norad project

def task_clean_data():
	"""Loads and cleans data"""

	print("Cleaning data")

	return {
        "file_dep": ["prep/clean.R"],
        "targets": ["logs/clean.Rout", "data/cleaned.RData"],
        "actions": ["R CMD BATCH prep/clean.R ../logs/clean.Rout"]
	}