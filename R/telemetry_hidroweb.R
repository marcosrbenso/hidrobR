#' Donwload data from telemetry
#'
#' @param CodEstacao integer. Station code retrieved from HidroWeb
#' @param DataInicio character. Date of the beginning of the observation in the format 'day/month/Year'
#' @param DataFim character. Date of the ending of the observation in the format 'day/month/Year'
#'
#' @export telemetry
#' @import dplyr
#' @import lubridate
#' @import xml2
#'
#'
#' @examples
#' data <- telemetry(65945400,DataInicio="01/01/2020",DataFim = "01/01/2021")
#' head(data)
#'
telemetry <- function(CodEstacao,DataInicio,DataFim){

  url2 <- paste("http://telemetriaws1.ana.gov.br/ServiceANA.asmx/DadosHidrometeorologicos?CodEstacao=",
                CodEstacao,
                "&DataInicio=",
                DataInicio,
                "&DataFim=",
                DataFim,sep='')
  xml_file <- read_xml(url2)

  CodEstacao <- xml_find_all(xml_file,".//CodEstacao") %>% xml_text()
  DataHora <- xml_find_all(xml_file,".//DataHora") %>% xml_text() %>%  as.POSIXct(format = '%Y-%m-%d %H:%M:%S')
  Chuva <- xml_find_all(xml_file,".//Chuva") %>% xml_text() %>% as.numeric()
  Vazao <- xml_find_all(xml_file,".//Vazao") %>% xml_text() %>% as.numeric()
  Nivel <- xml_find_all(xml_file,".//Nivel") %>% xml_text() %>% as.numeric()

  return(data.frame(CodEstacao,DataHora,Chuva,Vazao,Nivel))

}


#' List of Telemetric stations
#'
#' @param Estacoes integer. 0 - active stations and 1 - in maintanance
#' @param origem integer.  0-Todas, 1-ANA/INPE, 2-ANA/SIVAM, 3-RES_CONJ_03, 4-CotaOnline, 5-Projetos Especiais
#'
#' @import dplyr
#' @import lubridate
#' @import xml2
#' @export list_of_telemetric_stations


list_of_telemetric_stations <- function(Estacoes = 0,origem = 0){

  url <- "http://telemetriaws1.ana.gov.br//ServiceANA.asmx/ListaEstacoesTelemetricas?status"
  url2 <- paste0(
      url,
      "Estacoes=",
      Estacoes,
      "&origem=",
      origem
    )

  xml_file <- read_xml(url2)

  CodEstacao <- xml_find_all(xml_file,".//CodEstacao") %>% xml_text()
  Operadora <- xml_find_all(xml_file,".//Operadora") %>% xml_text()
  Bacia <- xml_find_all(xml_file,".//Bacia") %>% xml_text()
  CodEstacao <- xml_find_all(xml_file,".//CodEstacao") %>% xml_text()
  Latitude <- xml_find_all(xml_file,".//Latitude") %>% xml_text()
  Longitude <- xml_find_all(xml_file,".//Longitude") %>% xml_text()
  Altitude <- xml_find_all(xml_file,".//Altitude") %>% xml_text()
  NomeRio <- xml_find_all(xml_file,".//NomeRio") %>% xml_text()
  Origem <- xml_find_all(xml_file,".//Origem") %>% xml_text()
  StatusEstacao <- xml_find_all(xml_file,".//StatusEstacao") %>% xml_text()

  list_of_station <-
    data.frame(
      CodEstacao,Operadora,Bacia,Latitude,Longitude,Altitude,NomeRio,Origem,StatusEstacao
    )

  return(list_of_station)
}






