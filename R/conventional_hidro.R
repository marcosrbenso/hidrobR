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
#' @import xml2
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @import tidyr
#' @export conventional

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

  ifelse(tipoDados == 1, "Cota",
         ifelse(tipoDados == 2, "Chuva","Vazao")) -> var

  DataHora <- xml_find_all(xml_file,".//DataHora") %>% xml_text() %>%  as.POSIXct(format = '%Y-%m-%d %H:%M:%S')

  if(length(DataHora) < 1){
    stop(paste("There is no",var,"data in this station in the period of",DataInicio,"and",DataFim))
  }
  else if(var == "Cota"){
    n <- 1:31
    var_names <- paste0(var,ifelse(n < 10,paste0("0",n),n))
    dataJSON <- toJSON(xmlToList(xmlTreeParse(xml_file)))
    g <- jsonlite::fromJSON(dataJSON)[["diffgram"]][["DocumentElement"]]
    if(is.null(g)){

      jsonlite::fromJSON(dataJSON)[["diffgram"]][[1]][,c('EstacaoCodigo','DataHora','NivelConsistencia',var_names)]
      lapply(jsonlite::fromJSON(dataJSON)[["diffgram"]], function(x){
        tryCatch(
          {
            res <- x[,c('EstacaoCodigo','DataHora','NivelConsistencia',var_names)]
            return(res)
            },
          error = function(e) {}
        )
      }) -> g

      data_df <- do.call("rbind",g[-which(sapply(g, is.null))])

      char_length <- nchar(var)

      data_df %>%
        mutate_all(unlist) %>%
        pivot_longer(
          !c(EstacaoCodigo,NivelConsistencia,DataHora),
          names_to="day",
          values_to=var,
        ) %>%
        mutate(
          day = (substr(day,char_length+1,char_length+2)) %>% gsub("[^0-9.-]", "", .),
          DataHora = substr(DataHora,1,8),
          date = as.Date(paste0(DataHora,day),format="%Y-%m-%d"),
          (!!var) := as.numeric(get(var))
        ) %>%
        arrange(date) -> data_df

      data_df <- data_df[,c('EstacaoCodigo','date',var,'NivelConsistencia')]

      return(data_df)

    }
    else{
      data_df <- g[,c('EstacaoCodigo','DataHora','NivelConsistencia',var_names)]
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
          (!!var) := as.numeric(get(var))
        ) %>%
        arrange(date) -> data_df

      data_df <- data_df[,c('EstacaoCodigo','date',var,'NivelConsistencia')]

      return(data_df)
    }

  }
  else{
    DataHora <- xml_find_all(xml_file,".//DataHora") %>% xml_text() %>%  as.POSIXct(format = '%Y-%m-%d %H:%M:%S')
    EstacaoCodigo <- xml_find_all(xml_file,".//EstacaoCodigo") %>% xml_text() %>% as.numeric()
    n <- 1:31
    var_names <- paste0(var,ifelse(n < 10,paste0("0",n),n))

    lapply(var_names,function(x){
      dat <- xml_find_all(xml_file,paste0(".//",x)) %>% xml_text()
    }) -> test

    I <- 31
    J <- length(test[[1]])
    mat <- matrix(rep(NA,I*J),ncol=31)

    for(i in 1:I){
      for(j in 1:J){
        mat[j,i] <- as.numeric(test[[i]][j])
      }
    }

    data_df <- as.data.frame(mat)
    colnames(data_df) <- var_names

    data_df$EstacaoCodigo <- EstacaoCodigo
    data_df$nivelConsistencia <- nivelConsistencia
    data_df$DataHora <- DataHora

    char_length <- nchar(var)

    data_df %>%
      pivot_longer(
        !c(EstacaoCodigo,nivelConsistencia,DataHora),
        names_to="day",
        values_to=var,
      ) %>%
      mutate(
        day = (substr(day,char_length+1,char_length+2)) %>% gsub("[^0-9.-]", "", .),
        DataHora = substr(DataHora,1,8),
        date = as.Date(paste0(DataHora,day),format="%Y-%m-%d"),
        (!!var) := as.numeric(get(var))
      ) %>%
      arrange(date) -> data_df

    data_df <- data_df[,c('EstacaoCodigo','date',var,'nivelConsistencia')]

    return(data_df)
  }

}




