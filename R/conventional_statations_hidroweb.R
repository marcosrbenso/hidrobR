#' Download data from convetional stations
#'
#' @param var character. Valid variable names are "vazoes", "cotas", "chuvas"
#' @param code_hidroweb integer. HidroWeb code for conventional stations
#'
#' @return
#' @export
#' @import dplyr
#' @import lubridate
#' @import httr
#' @examples
#' code_hidroweb <- 75890000
#' var = "cotas"
#' conventional(code_hidroweb,cotas)


conventional <- function(code_hidroweb,var){

  set.seed(123)
  downloadDir <- tempdir()

  ListaEstacaoes = code_hidroweb
  baseurl = "http://www.snirh.gov.br/hidroweb/rest/api/documento/convencionais?tipo=&documentos="
  destino = downloadDir
  # tipo=1 arquivo access *.mdb
  # tipo=2 arquivo texto  *.txt
  # tipo=3 arquivo excel  *.csv
  tipo = 2

  #substituindo o tipo
  baseurl = gsub("tipo=",paste0("tipo=",tipo),baseurl)

  for(i in 1:length(ListaEstacaoes)){
    baseurl_est = paste0(baseurl,ListaEstacaoes[i])

    #Conexao
    r = POST(url = baseurl_est, body = list(cboTipoReg = "10"), encode = "form")
    cont = content(r, as = "text", encoding="ISO-8859-1")
    download.file(baseurl_est, paste0(destino,"/",ListaEstacaoes[i], ".zip"), mode = "wb")

    print(paste("done ",ListaEstacaoes[i]))
  }


  list_estations <- list.files(path=downloadDir,
                               pattern = "zip")

  #list_estations <- list.files(path=downloadDir,
  #                             pattern="0.zip")

  lapply(paste0(downloadDir,"/",list_estations),function(x){unzip(x,exdir = downloadDir)})

  unzip_var(destino,var)

  data <- read_hidroweb(paste0(destino,"/",list.files(destino, pattern = ".txt")))
  data <- data %>% select(-Day)

  colnames(data) <- c("Quality","Date","code_hidroweb",var)

  unlink(paste0(normalizePath(destino), "/", dir(destino)), recursive = TRUE)

  return(data)

}


unzip_var <- function(downloadDir,var){
  # var = vazoes,
  list_flow <- list.files(path=downloadDir,
                          pattern=var)
  lapply(paste0(downloadDir,"/",list_flow),function(x){unzip(x,exdir = downloadDir)})

}

read_hidroweb <- function(hidrowebFile){
  test <- read.csv(hidrowebFile, row.names = NULL, nrows = -1,skip=13,sep=';', dec=",")
  test <- test[,c(2,3,17:(16+31))]
  colnames(test)  <- c("Quality","Date",as.character(1:31))
  test$gauge_ANA <- substr(hidrowebFile,nchar(hidrowebFile)-11,nchar(hidrowebFile)-4)
  test <- test %>% pivot_longer(!c(Quality,gauge_ANA,Date),names_to="Day",values_to="var") %>%
    mutate(Date = as.Date(paste(Day,substr(Date,4,10),sep="/"),
                          format="%d/%m/%Y"))
  test <- test[is.na(test$Date)==F,]
  return(test)
}

