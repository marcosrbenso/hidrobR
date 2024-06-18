#' Donwload data from conventional stations
#'
#' @param codEstacao integer. Code of rain or flow station
#' @param dataInicio character. Date of the beginning of the observation in the format 'day/month/Year'
#' @param dataFim character. Date of the ending of the observation in the format 'day/month/Year'
#' @param tipoDados integer. 1-Cotas, 2-Chuvas ou 3-Vazões for stage, rain or flow, respectively
#' @param nivelConsistencia integer. 1-Bruto ou 2-Consistido
#'
#' @import jsonlite
#' @import XML
#' @import xlm2
#' @import dplyr
#' @import tidyr
#' @import RJSONIO
#' @return
#' @export
#'
#' @examples
conventional <- function(CodEstacao,DataInicio,DataFim,tipoDados,nivelConsistencia){
  url2 <- paste0("http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroSerieHistorica?codEstacao=",
                CodEstacao,
                "&DataInicio=",
                DataInicio,
                "&DataFim=",
                DataFim,
                "&tipoDados=",
                tipoDados,
                "&nivelConsistencia=",
                nivelConsistencia)
  xml_file <- read_xml(url2)

  ifelse(tipoDados == 1, "cota",
         ifelse(tipoDados == 2, "Chuvas","Vazões")) -> var

  DataHora <- xml_find_all(xml_file,".//DataHora") %>% xml_text() %>%  as.POSIXct(format = '%Y-%m-%d %H:%M:%S')
  EstacaoCodigo <- xml_find_all(xml_file,".//EstacaoCodigo") %>% xml_text() %>% as.numeric()
  dataJSON <- toJSON(xmlToList(xmlTreeParse(xml_file)))
  g <- jsonlite::fromJSON(dataJSON)[["diffgram"]][["DocumentElement"]]
  data_df <- data_df[,c(1,2,3,10:(11+25),70,71,72,73)]
  data_df %>%
    pivot_longer(
      !c(EstacaoCodigo,NivelConsistencia,DataHora),
      names_to="day",
      values_to=var,
    ) %>%
    mutate(
      day = substr(day,5,6),
      DataHora = substr(DataHora,1,8),
      date = as.Date(paste0(DataHora,day)),
      cota = as.numeric(cota)
    ) %>%
    arrange(date) -> data_df

  return(data_df)
}


list_of_conventional_stations <- function(){

  url <- "http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroInventario?"
  read_xml(url)

}


