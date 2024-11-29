# IDOC methods paper

Stores the following resources:

* Metadata of flies used in any experiment: metadata collects biologically relevant information specific to each fly. Can be genotype, gender, treatments, etc
Files: `metadata.csv` and `metadata_etho.csv`

* R notebook: `1_tidy_datav2.R.ipynb` reads the metadata and loads the IDOC data from the database. Saves a `tidy_data_wide.csv` file
* R scripts: used to process the behavioral data associated to the flies provided in the metadata. Take `tidy_data_wide.csv` as input
* .svg files: graphs

To generate graphs:

1. Run the R notebook

2. Run the R scripts
```
bash generate_figures.sh
```

3. Combine with svg templates
```
bash combine_figures.sh
```

### Analyze IDOC experiment:

To analyze all experiments and compute a PI for each fly, run

```
bash scripts/main.sh
```
