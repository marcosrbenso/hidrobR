# hidrobR: Download of Historical Data from Brazilian Hydrological Stations in R
## Overview
`hidrobR` provides access to historical data from the Brazilian hydrological stations maintained by the Brazilian National Agency of Water and Sanitation (Agência Nacional de Águas e Saneamento ANA) network available at the HidroWeb data base (https://www.snirh.gov.br/hidroweb/apresentacao) for the conventional stations and the Telemetric network (https://www.snirh.gov.br/hidrotelemetria/Mapa.aspx). This tool automates the process of selection and download of conventional (estações convencionais) and telemetric (telemetria e estaçoes telemétricas) from HidroWeb and Hidro-Telemetria.

## Installation
`hidrobR` can be installed from GitHub using the `devtools` package.

```r
library(devtools)
install_github('marcosrbenso/hidrobR')
```

```r
library(hidrobR)
```

## Convetional Station 

### Station metadata
The information for the convetional flow and precipitation stations is loaded with the package.

```r
head(flow_gauges)
head(prec_gauges)
```
### Downloading station data
```r
code_hidroweb <- 75890000
var = "cotas"
conventional(code_hidroweb,var)
```


## Telemetric Station

### Station information

The function list_of_stations retrieves the updated list of telemetric stations from the ANA Web Service.

```r
telemetric_stations <- list_of_stations()
head(telemetric_stations)
```

### Downloading station data

```r
data <- get_data(65945400,DataInicio="01/01/2020",DataFim = "01/01/2021")
head(data)
```
