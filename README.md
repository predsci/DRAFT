# DRAFT
 Disease Rapid Analysis and Forecasting Tool - A shiny app

A shiny app to explore 'what-if' scenarios for COVID-19.  Based on JHU data, the app allows a user to use best-fit parameters, manual entry of two-value R_0, or specify intervention start/end dates to generate projections of the future outbreak.

## To use
After cloning the repository, run in an R session:
```R
library("shiny")
shinyAppFile("/my_local_dir/DRAFT/app.R")
```

