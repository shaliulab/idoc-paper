# IDOC methods paper

Stores the follwing resources:

* Metadata of flies used in any experiment: metadata collects biologically relevant information specific to each fly. Can be genotype, gender, treatments, etc
* R notebooks: used to load the behavioral data associated to the flies provided in the metadata
* .csv files: contain the PRE and POST readings of the flies provided in the metadata, and are produced by the R notebooks, or amount of sleep
* Prism files: contain the instructions to generate the plots / graphs that illustrate the effect of treatments on the PRE and POST readings
* .svg files: graphs

Note: .csv and .svg files are stored in a separate drive (Google) and can be downloaded using dvc (data version control). Assuming you have:

1. dvc installed
2. access permissions to the Google Drive folder (TODO)

you can get these files by running

```
dvc pull
```
