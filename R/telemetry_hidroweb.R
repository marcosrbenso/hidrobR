#' Donwload data from telemetry
#'
#' @param CodEstacao integer. Station code retrieved from HidroWeb
#' @param DataInicio character. Date of the beginning of the observation in the format 'day/month/Year'
#' @param DataFim character. Date of the ending of the observation in the format 'day/month/Year'
#'
#' @return
#' @export
#' @import dplyr
#' @import lubridate
#' @import xml2
#'
#'
#' @examples
#' data <- get_data(65945400,DataInicio="01/01/2020",DataFim = "01/01/2021")
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





