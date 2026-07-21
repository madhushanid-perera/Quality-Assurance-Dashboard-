# QA Dashboard — Phase I & II Control Charts

A Shiny dashboard for constructing Phase I (trial control limits) and 
Phase II (process monitoring) X-bar and R control charts, built with 
`shiny`, `shinydashboard`, and `qcc`.

🔗 **Live app:** https://madhushani-perera.shinyapps.io/QAdashboard/

## Features
- Upload any dataset (CSV, Excel, or TXT)
- Select measurement columns and subgroup size for Phase I limits
- Generate X-bar and/or R charts
- Add new individual observations for real-time Phase II monitoring

## Demo data
`sample_data.csv` is a demo dataset used only to illustrate the dashboard, 
sourced from Douglas C. Montgomery's *Introduction to Statistical Quality 
Control*. Any dataset with numeric measurement columns can be uploaded 
in its place.

## Run locally
```r
install.packages(c("shiny", "shinydashboard", "qcc", "DT", "readxl", "openxlsx"))
shiny::runApp("app.R")
```

## Author
Madhushani Perera — Department of Statistics, University of Sri Jayewardenepura
