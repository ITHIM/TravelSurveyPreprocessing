# TravelSurveyPreprocessing

This repo is to centralize all efforts made to preprocess travel surveys from different cities around the world. Although the main use of the processed datasets is the ITHIM-R package, they can be used for other purposes.

Ideally raw datasets should be saved in the V-drive in the path `/CEDAR/Studies/MOVED/HealthImpact/Data/Country/` and processed datasets should be saved here.

If importing raw datasets from V-drive in R is too slow, then you can download them and add to the script a commented line with the path on the V-drive. For example, raw datasets from Bogota are in `CEDAR/Studies/MOVED/HealthImpact/Data/Country/Colombia/Bogota/Travel/trips_2019`. To import them locally the script should have this

```
# V-drive path
# path <- "CEDAR/Studies/MOVED/HealthImpact/Data/Country/Colombia/Bogota/Travel/trips_2019/"

path <- "C:/Users/admin/downloads/trips_2019/"

people <- read_excel(paste0(path, "PersonasEODH2019.xlsx"))
```

In this way, anyone that has access to the V-drive can run the scrip buy uncommenting the first line and uncommenting the second one.
