# introduzco primera linea de texto para generar cambio
# introduzco segunda linea de texto para generar cambio

# Librerias ---------------------------------------------------------------
library(devtools)
# Cuando instalamos la librería, marcamos la linea de codigo con "#" para no
# volverlo a ejecutar

# install_version("rvest", version = "0.3.4")
# install_version("quantmod", version = "0.4-14")  # 0.4.14 version alternativa

library(quantmod)
# packageVersion("quantmod")

library(RCurl) # Libreria para... 1.95/412
library(zoo) # Libreria para... 1.8-5

# install_version("doParallel", version = "1.0.14")
library(doParallel) # Libreria para... 1.0.14
library(operators) # Libreria para... 0.1-8
library(rvest) # Libreria para... 0.3.4
library(stringr) # Libreria para... 1.4.0
library(mailR) # Libreria para...  0.4.1
library(htmlTable) # Libreria para... 1.13.1
library(jsonlite) # 1.6
library(lubridate) # 1.7.10



# Funciones: --------------------------------------------------------------

descarga_lista_valores <- function(){
  data <- fromJSON("https://api.nasdaq.com/api/screener/stocks?tableonly=true&limit=25&offset=0&exchange=NASDAQ&download=true")
  
  # Del fichero data, viene en formato lista, y queremos la tabla de los datos:
  NASDAQ_CSV <- data$data$rows
  
  # Datos missing:
  na.strings <- c("", "n/a", "0")
  
  is.na(NASDAQ_CSV) <- Reduce("|", lapply(na.strings, "==", NASDAQ_CSV))
  
  # Explicación de las columnas:
  
  # Nombre de las columnas:
  colnames(NASDAQ_CSV) <- c('Symbol','Name','LastSale','NetChange','PctChange','Volume','MarketCap','Country','IPOyear','Industry','Sector','URL')

  # Cambiamos el formato de determinadas columnas:
  NASDAQ_CSV$LastSale <- chartr("$", " ", NASDAQ_CSV$LastSale)
  NASDAQ_CSV$LastSale <- as.numeric(as.character(NASDAQ_CSV$LastSale))
  NASDAQ_CSV$MarketCap <- as.numeric(as.character(NASDAQ_CSV$MarketCap))

  # Na para IPOyear: valor fijo 2000
  NASDAQ_CSV[is.na(NASDAQ_CSV$IPOyear),]$IPOyear <- '2000'
  
  NASDAQ_CSV[is.na(NASDAQ_CSV$Country),]$Country <- 'United States' # Nuevo
  NASDAQ_CSV$MarketCap <- ifelse(NASDAQ_CSV$MarketCap==0, NA, NASDAQ_CSV$MarketCap) # Nuevo
  NASDAQ_CSV <- na.omit(NASDAQ_CSV) # Nuevo
  
  # Creamos la columna 'mercado':
  NASDAQ_CSV$Mercado <- 'Nasdaq'

  # Creamos la columna "acciones":
  NASDAQ_CSV$acciones <- round(NASDAQ_CSV$MarketCap / NASDAQ_CSV$LastSale, digits = 0)

  # Replicamos para NYSE:
  data <- fromJSON("https://api.nasdaq.com/api/screener/stocks?tableonly=true&limit=25&offset=0&exchange=NYSE&download=true")
  
  # Del fichero data, viene en formato lista, y queremos la tabla de los datos:
  NYSE_CSV <- data$data$rows
  
  # Datos missing:
  na.strings <- c("", "n/a", "0")
  
  is.na(NYSE_CSV) <- Reduce("|", lapply(na.strings, "==", NYSE_CSV))
  
  # Explicación de las columnas:
  
  # Nombre de las columnas:
  colnames(NYSE_CSV) <- c('Symbol','Name','LastSale','NetChange','PctChange','Volume','MarketCap','Country','IPOyear','Industry','Sector','URL')
  
  # Cambiamos el formato de determinadas columnas:
  NYSE_CSV$LastSale <- chartr("$", " ", NYSE_CSV$LastSale)
  NYSE_CSV$LastSale <- as.numeric(as.character(NYSE_CSV$LastSale))
  NYSE_CSV$MarketCap <- as.numeric(as.character(NYSE_CSV$MarketCap))
  
  # Na para IPOyear: valor fijo 2000
  NYSE_CSV[is.na(NYSE_CSV$IPOyear),]$IPOyear <- '2000'
  
  NYSE_CSV[is.na(NYSE_CSV$Country),]$Country <- 'United States' # Nuevo
  NYSE_CSV$MarketCap <- ifelse(NYSE_CSV$MarketCap==0, NA, NYSE_CSV$MarketCap) # Nuevo
  NYSE_CSV <- na.omit(NYSE_CSV) # Nuevo
  
  # Creamos la columna 'mercado':
  NYSE_CSV$Mercado <- 'NYSE'
  
  # Creamos la columna "acciones":
  NYSE_CSV$acciones <- round(NYSE_CSV$MarketCap / NYSE_CSV$LastSale, digits = 0)
  
  # Unimos mercados y eliminamos duplicados:
  USA_STOCK <- rbind(NASDAQ_CSV, NYSE_CSV[!(NYSE_CSV) %in% (NASDAQ_CSV)])
  
  # Ordenamos los valores:
  USA_STOCK <- USA_STOCK[order(row.names(USA_STOCK), decreasing = F),]
  rownames(USA_STOCK) <- USA_STOCK$Symbol

  return(USA_STOCK)  
}

recomendador_finviz <- function(size){
  # Campos y filtros:
  if(size == "Large"){
  url  <- "https://finviz.com/screener.ashx?v=152&f=cap_large,sh_price_o30,sh_avgvol_o100&ft=4&o=ticker&c=0,1,2,3,5,6,16,57,62,65,66,67,68,70"  
  url2 <- "https://finviz.com/screener.ashx?v=152&f=cap_large,sh_price_o30,sh_avgvol_o100&ft=4&o=ticker&c=0,1,2,3,5,6,16,57,62,65,66,67,68,70&r="

  
  }else if(size == "Mega"){
  url  <- "https://finviz.com/screener.ashx?v=152&f=cap_mega,sh_price_o30,sh_avgvol_o100&ft=4&o=ticker&c=0,1,2,3,5,6,16,57,62,65,66,67,68,70"  
  url2 <- "https://finviz.com/screener.ashx?v=152&f=cap_mega,sh_price_o30,sh_avgvol_o100&ft=4&o=ticker&c=0,1,2,3,5,6,16,57,62,65,66,67,68,70&r="
  
  
  }else if(size == "Mid"){
  url  <- "https://finviz.com/screener.ashx?v=152&f=cap_mid,sh_price_o30,sh_avgvol_o100&ft=4&o=ticker&c=0,1,2,3,5,6,16,57,62,65,66,67,68,70"  
  url2 <- "https://finviz.com/screener.ashx?v=152&f=cap_mid,sh_price_o30,sh_avgvol_o100&ft=4&o=ticker&c=0,1,2,3,5,6,16,57,62,65,66,67,68,70&r="
  
  }else{
  url  <- "https://finviz.com/screener.ashx?v=152&f=cap_small,sh_price_o30,sh_avgvol_o100&ft=4&o=ticker&c=0,1,2,3,5,6,16,57,62,65,66,67,68,70"  
  url2 <- "https://finviz.com/screener.ashx?v=152&f=cap_small,sh_price_o30,sh_avgvol_o100&ft=4&o=ticker&c=0,1,2,3,5,6,16,57,62,65,66,67,68,70&r="
  }
   
  # Descargamos los datos, y los guardamos en una tabla:
  datos_web <- read_html(url) %>%
    html_nodes("table") %>%
    html_table(fill = TRUE)
 
  # Entramos en el fichero, nos quedamos con el elemento 10 de la lista. 
  recomendador <- datos_web[[10]]
  colnames(recomendador) <- recomendador[1,]
  recomendador$No. <- NULL
  
  recomendador <- recomendador[-1,]

  # Nos quedamos con el nº de empresas: "Total: n empresas"
  num_empresas <- as.numeric(strsplit(as.character(datos_web[[9]][1, 1]), " ")[[1]][2])
  contador <- nrow(recomendador) +1

  # url2:
  while(contador < num_empresas){
    datos_web <- read_html(paste0(url2, contador)) %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)
  
  tabla_aux <- datos_web[[10]]
  colnames(tabla_aux) <- tabla_aux[1,]
  tabla_aux$No. <- NULL
  tabla_aux <- tabla_aux[-1,]
  
  # rowbind: añadimos las nuevas empresas al fichero inicial.
  recomendador <- rbind(recomendador, tabla_aux)
  
  # Actualizamos el valor "contador"
  contador <- nrow(recomendador) +1
  
  Sys.sleep((sample(5:9, 1, replace = T)/10)+0.5)
    
  }
  
  # Si $Earnings no tiene valor, sustitumos por 'Dec 31/a'
  # Si $Recom no tiene valor, pon 0
  # Si $EPS no tiene valor, pon 0.1
  ## Formato de las columnas¿?¿?
  
  if(nrow(recomendador[recomendador$Earnings == "-",]) > 0){
    recomendador[recomendador$Earnings == "-", ]$Earnings <- 'Dec 31/a'
  }
  if(nrow(recomendador[recomendador$Recom == "-",])>0){
    recomendador[recomendador$Recom == "-",]$Recom <- '0' # Si no tenemos dato, el 0 indica "sin dato". Otra cosa sería falsificar información
  }
  if(nrow(recomendador[recomendador$EPS == "-",])>0){
    recomendador[recomendador$EPS == "-",]$EPS <- '0.1'
  }

  # Modificar el formato de la fecha de "Earnings"    
  recomendador$Earnings <- ymd(paste0(year(Sys.Date()), substr(recomendador$Earnings, 1, 3), substr(recomendador$Earnings, 5, 6)))
  
  # Cambios de formato de variables:
  recomendador$Recom <- as.numeric(recomendador$Recom)
  recomendador$Price <- as.numeric(recomendador$Price)
  recomendador$EPS <- as.numeric(recomendador$EPS)
  
  # Modificamos la B (de billon), de market cap
  recomendador$`Market Cap` <- as.numeric(substr(recomendador$`Market Cap`, 1, nchar(recomendador$`Market Cap`)-1))*1000000000
  ## Ej: y si trabajamos con empresas "Small" o inferiores, y queremos quitar la "M"? Podríamos dejar todo en un ifelse?
  # ifelse(substr(recomendador$`Market Cap`, -1=="M"...completar)) {
  #   x10^6
  # }else{
  #   10^9
  # }
  
  # Creamos el volumen de acciones:
  recomendador$Acciones <- round(recomendador$`Market Cap`/recomendador$Price, digits = 0)
  
  # Volumen:
  recomendador$Volume <- as.numeric(gsub(",", "", recomendador$Volume))
  recomendador$size <- size
  
  # Eliminamos el % de 52 weeks
  recomendador$`52W High`<- as.numeric(substr(recomendador$`52W High`, 1, nchar(recomendador$`52W High`)-1))
  
  # Filtramos aquellas empresas que cumplen la condicion:
  recomendador <- recomendador[recomendador$Recom < 3.5,] 
  recomendador <- recomendador[recomendador$`52W High` > -35, ]
  recomendador <- na.omit(recomendador)
  rownames(recomendador) <- recomendador$Ticker
  
  return(recomendador)
}

descarga_datos <- function(start, end, periodo, recomendador){
  # Inicializamos la variable para indicar los periodos de bº y volatilidad
  periodo_profit_vol <- 15
  descargar_activo <- function(i, periodo, start, end, ventana, ATRSP, volatilidad, recomendador, ProfitSP500){
    library(quantmod) # Libreria TTR embebida dentro de quantmod
    library(zoo)
    
    # i: fila del fichero (empresa)
    acciones <- recomendador[i,]$Acciones
    
    # La funcion getsymbols nos pide las siguientes variables: 
    # i: Ticker (cómo encontrar cada ticker que queramos?). Buscamos en yahoo finance...
    # src: Fuente de descarga de datos = "yahoo"
    # periodicity: 'daily' / 'weekly'
    # start: as.Date("2007-01-01")
    # ventana: 200 / 52 (va de la mano con "periodicity")
    # end: Sys.Date() +1
    # El +1 incluye hoy como dato
    # auto.assign: Genera la variable 'stock' (ojo con esto!)
    # from: de esta manera nos aseguramos que tenemos datos para la media de 200, desde comienzos de 'start'
    
    stock <- getSymbols(i, src = "yahoo", periodicity = periodo, from = (start -(ventana+1)), to = end, auto.assign = FALSE)
    
    # output (xts: Formato de series temporales)
    
    # Sustituimos los NA de la serie, por los datos más recientes y previos al NA
    stock <- na.locf(stock)
    
    # Omitimos NA
    stock <- na.omit(stock)
    
    # Creamos nuevas columnas:
    if(nrow(stock) > (ventana*2) & acciones > 0){
      stock$Profit <- dailyReturn(stock, type = "arithmetic")*100   # (close/open-1)*100
      stock$Profit.1 <- lag(stock$Profit, 1)                        # Ganancia del dia anterior
      stock$Profit.2 <- lag(stock$Profit, 2)  
      stock$RSI <- RSI(Cl(stock), 14)
      stock$CMO <- CMO(Cl(stock), 21)
      stock$EMA10 <- EMA(Cl(stock), 10)
      stock$EMA10.1 <- lag(stock$EMA10, 1)
      stock$EMA10.2 <- lag(stock$EMA10, 2)
      stock$EMA10.3 <- lag(stock$EMA10, 3)
      stock$EMA20 <- EMA(Cl(stock), 20)
      stock$EMA40 <- EMA(Cl(stock), 40)
      stock$ATR <- EMA(ATR(HLC(stock), 15)[, "atr"], 15)
      stock$ATRSP <- merge.xts(ATRSP)
      stock <- na.locf(stock)
      stock <- na.omit(stock)
      stock <- merge(stock, MACD(Cl(stock), nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = FALSE)) # Calculamos MACD
      stock$ROC <- ROC(Cl(stock), 15)
      stock$ROC.1 <- lag(stock$ROC, 1)
      
      stock$Profit_valor = rollapplyr(stock$Profit, periodo_profit_vol, sum, fill = 0)
      stock$ProfitSP = merge.xts(ProfitSP500)
      
      stock$VolValor <- rollapplyr(stock$Profit, periodo_profit_vol, var, fill = 0)
      stock$Sharpe <- (stock$Profit_valor - stock$ProfitSP) / stock$VolValor
      stock$Sharpe.1 <- lag(stock$Sharpe, 1)
      stock$VolValor.1 <- lag(stock$VolValor, 1)
      stock$volatilidad <- merge.xts(Volatilidad)
      stock$capitalizacion <- acciones * stock[, 6]
      
      stock$Close.1 <- lag(Cl(stock), 1)
      stock$Close.2 <- lag(stock$Close.1, 1)
      stock$Close.3 <- lag(stock$Close.1, 2)
      stock <- round(stock, digits = 3)
      
      # Eliminamos columnas
      stock$Profit_valor = NULL
      stock$ProfitSP = NULL
      
      dates <- index(stock)
      stock <- stock[index(stock)>start]     # Quita los (-ventana +1), del from. Los valores previos
      
      colnames(stock) <- c('Open','High','Low','Close','Volume','Adjusted','Profit','Profit.1','Profit.2','RSI','CMO','EMA10','EMA10.1','EMA10.2','EMA10.3',
                           'EMA20','EMA40','ATRM','ATRSP','macd','signal','ROC','ROC.1','VolValor','Sharpe','Sharpe.1','VolValor.1','Volatilidad','Cap','Close.1',
                           'Close.2','Close.3')
      stock <- na.omit(stock)
     }else{
      stock$Profit <- 0
      stock$Profit.1 <- 0
      stock$Profit.2 <- 0
      stock$RSI <- 0
      stock$CMO <- 0
      stock$EMA10 <- 0
      stock$EMA10.1 <- 0
      stock$EMA10.2 <- 0
      stock$EMA10.3 <- 0
      stock$EMA20 <- 0
      stock$EMA40 <- 0
      stock$ATRM <- 0
      stock$ATRSP <- 0
      stock$macd <- 0
      stock$signal <- 0
      stock$ROC <- 0
      stock$ROC.1 <- 0
      stock$VolValor <- 0
      stock$Sharpe <- 0
      stock$Sharpe.1 <- 0
      stock$VolValor.1 <- 0
      stock$Volatilidad <- 0
      stock$Cap <- 0
      stock$Close.1 <- 0
      stock$Close.2 <- 0
      stock$Close.3 <- 0
      
      colnames(stock) <- c('Open','High','Low','Close','Volume','Adjusted','Profit','Profit.1','Profit.2','RSI','CMO','EMA10','EMA10.1','EMA10.2','EMA10.3',
                           'EMA20','EMA40','ATRM','ATRSP','macd','signal','ROC','ROC.1','VolValor','Sharpe','Sharpe.1','VolValor.1','Volatilidad','Cap','Close.1',
                           'Close.2','Close.3')
    }
    return(stock)
  }
  # Creamos los parametros
  empresas <- recomendador$Ticker
  USA_STOCK_LIST <- list()
  
  if(periodo == "weekly"){
    ventana = 52
  }else{ # Si periodo no es igual a "weekly" (semanal), va a ser igual a "daily" (diario)
    ventana = 200
  }

  # Descargamos la info del SP500
  SP500 <- getSymbols('^GSPC', src = "yahoo", periodicity = periodo, from = (start-(ventana+1)), to = end, auto.assign = FALSE)
  SP500 <- na.locf(SP500)
  SP500 <- na.omit(SP500)
  
  ProfitSP <- dailyReturn(SP500, type = "arithmetic")*100
  SP500$ATR <- ATR(HLC(SP500), 15)[, "atr"]
  ATRSP <- SP500$ATR

  SP500$ATR_Mean <- rollapplyr(SP500$ATR, periodo_profit_vol, mean, fill = NA)
  Volatilidad <- SP500$ATR - SP500$ATR_Mean     
  
  ProfitSP500 <- rollapplyr(ProfitSP, periodo_profit_vol, sum, fill = 0)
  
  # Paralelización:
  cl <- makeCluster(detectCores(), outfile = "")
  registerDoParallel(cl)
  
  USA_STOCK_LIST <- foreach(i = empresas) %dopar%{
    descargar_activo(i, periodo, start, end, ventana, ATRSP, volatilidad, recomendador, ProfitSP500)
  }
  stopCluster(cl)
  
  names(USA_STOCK_LIST) <- empresas
  USA_STOCK_LIST <- USA_STOCK_LIST[order(names(USA_STOCK_LIST))] # ordenamos el fichero

  return(USA_STOCK_LIST)
}

algoritmo_ikaris <- function(stock, Cap){
  # stock, es el nombre de la variable de la función, es local. No confundir con el DF
  dates <- rownames(stock)
  compra <- 0
  stock$compra <- 0
  stock$venta <- 0
  stock$id_compra <- 0
  stock$STOP_M <- 0
  STOP_M <- 0
  stock$STOP_A <- 0
  STOP_A <- 0
  PO <- 0
  ATRM <- 0
  stock$precio <- 0
  year <- 0
  contador <- 0
  id_compra <- 1
  
  max_year <- as.numeric(format(as.Date(Sys.Date(), format = "%Y-%m-%d"), "%Y"))
  
  for(d in dates){
    dia <- stock[d,] # Creamos un vector, para cada uno de los dias, de cada activo
    # Condiciones de entrada:
    if(dia$EMA20 > dia$EMA40 & 
       dia$CMO <= 40 & 
       dia$Close > dia$EMA10 & 
       dia$Close.1 > dia$EMA10.1 & 
       dia$Close > dia$Close.1 & 
       dia$Close.2 < dia$EMA10.2 & 
       (dia$Profit + dia$Profit.1) < 7.0 &
       compra == 0 & 
       dia$Cap > Cap){
      
      # Calculamos valores:
      stock[d,]$STOP_M <- dia$Close - dia$ATRM
      STOP_M <- dia$Close - dia$ATRM
      stock[d,]$STOP_A <- dia$Close - dia$ATRM*2
      STOP_A <- dia$Close - dia$ATRM*2
      stock[d,]$PO <- dia$Close + dia$ATRM
      PO <- dia$Close + dia$ATRM
      ATRM <- stock[d,]$ATRM
      stock[d,]$id_compra <- id_compra
      stock[d,]$compra <- 100
      compra <- 100
      posiciones <- 1
      stock[d,]$posiciones <- posiciones
      stock[d,]$precio <- stock[d,]$Close
      
      # Si estoy dentro del mdo y salta el SL_A:
    }else if(compra > 0 & dia$Low < STOP_A){ # Hay compra y el minimo de hoy es menor al SL auto
      stock[d,]$venta <- 100
      stock[d,]$STOP_A <- 'APLICADO'
      stock[d,]$id_compra <- id_compra
      id_compra <- id_compra + 1
      stock[d,]$posiciones <- posiciones
      compra <- 0
      posiciones <- 0
      
      if(dia$Open < STOP_A){
        stock[d,]$precio <- dia$Open
      }else{
        stock[d,]$precio <- STOP_A
      }
      
      # Si estoy dentro del mercado y toca el SL_M
    }else if(compra > 0 & dia$Close < STOP_M){
      stock[d,]$venta <- 100
      stock[d,]$STOP_M <- 'APLICADO'
      stock[d,]$id_compra <- id_compra
      stock[d,]$posiciones <- posiciones
      id_compra <- id_compra + 1
      compra <- 0
      posiciones <- 0
      stock[d,]$precio <- stock[d,]$Close
      
      # Si hay compra, y el CMO > 50
    }else if(compra > 0 & dia$CMO > 50){
      stock[d,]$venta <- 100
      stock[d,]$id_compra <- id_compra
      stock[d,]$posiciones <- posiciones
      id_compra <- id_compra + 1
      compra <- 0
      posiciones <- 0
      stock[d,]$precio <- stock[d,]$Close
      
    }else if(compra > 0 & dia$Close > PO){
      while(stock[d,]$Close > PO){
      stock[d,]$STOP_M <- STOP_M + ATRM
      STOP_M <- STOP_M + ATRM
      stock[d,]$STOP_A <- STOP_A + ATRM
      STOP_A <- STOP_A + ATRM
      stock[d,]$PO <- PO + ATRM
      PO <-  PO + ATRM
      posiciones <- posiciones +1  
    }
    stock[d,]$posiciones <- posiciones
   }
  }
  return(stock)
}

generar_ranking <- function(operaciones){
  operaciones <- merge(listado_operaciones[listado_operaciones$accion == "COMPRAR", c('ticker', 'precio', 'id', 'fecha')],
                       listado_operaciones[listado_operaciones$accion == "VENDER", c('ticker', 'precio', 'id', 'fecha')], by = 'id')
  
  # Reducimos columnas y renombramos:
  operaciones <- operaciones[, c('ticker.x', 'precio.x', 'fecha.x', 'precio.y', 'fecha.y')]
  colnames(operaciones) <- c('Ticker', 'Precio Compra', 'Fecha Compra', 'Precio Venta', 'Fecha Venta')
  
  operaciones$Ganancia <- round(operaciones$`Precio Venta`- operaciones$`Precio Compra`, digits = 2)
  operaciones$Porcentaje <- round(operaciones$Ganancia / operaciones$`Precio Compra` *100 , digits = 2) 
  
  # Creamos un ranking vacio
  ranking <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(ranking) <- c('Ticker', 'Prob.Exito', 'Media.Ganancia', 'Media.Perdida', 'Ratio.Exito', 'Num.Operaciones')
  
  for(ticker in empresas){
    calculo_ranking <- data.frame(matrix(ncol = 6, nrow = 1))
    colnames(calculo_ranking) <- c('Ticker', 'Prob.Exito', 'Media.Ganancia', 'Media.Perdida', 'Ratio.Exito', 'Num.Operaciones')
    calculo_ranking$Ticker <- ticker
    calculo_ranking$Prob.Exito <- round(sum(operaciones$Ticker == ticker & operaciones$Ganancia >= 0) / sum(operaciones$Ticker == ticker), digits = 2)
    calculo_ranking$Media.Ganancia <- round(sum(operaciones$Ticker== ticker & operaciones$Ganancia >= 0) / sum(operaciones$Ticker == ticker) * 
                                              mean(operaciones[operaciones$Ticker == ticker & operaciones$Ganancia >= 0,]$Porcentaje), digits = 2)
    # Deberes -> alternativa para calcular: calculo_ranking$Media.Ganancia
    
    calculo_ranking$Media.Perdida <- round(sum(operaciones$Ticker== ticker & operaciones$Ganancia < 0) / sum(operaciones$Ticker == ticker) * 
                                             mean(operaciones[operaciones$Ticker == ticker & operaciones$Ganancia < 0,]$Porcentaje), digits = 2)
    calculo_ranking$Ratio.Exito <- round(abs(calculo_ranking$Media.Ganancia / calculo_ranking$Media.Perdida), digits = 2)
    calculo_ranking$Num.Operaciones <- sum(operaciones$Ticker == ticker)
    ranking <- rbind(ranking, calculo_ranking)
  }
  ranking <- na.omit(ranking)
  
  # Para identificar las empresas más rentables:
  # Aquellas que Ratio.Exito > 1.3 (para cubrir comisiones)
  # Ratio ideal: 1.3 - ...
  # Sería interesante establecer un filtro de X operaciones para no obtener valores disparatados (> 30 operaciones desde 2007)
  # ranking[ranking$Ratio.Exito > 1.3, ]
  
  return(ranking)
}


# Funciones extra Backtesting estrategia: ---------------------------------

nvl <- function(value, other){
  if(is.null(value) | is.infinite(value) | is.na(value)){
    return(other)
  }else{
  return(value)
  }
}

backtesting <- function(titulo, total_operaciones, fondos){
  # Creamos DF "calculo" vacio.
  calculo <- data.frame(matrix(0, ncol = 8, nrow = 1))
  colnames(calculo) <- c("Beneficio", "MaxDrawdown", "MaxRunup", "Acierto", "GananciaMedia", "PerdidaMedia", "DuracionMedia", "OpRealizadas")
  rownames(calculo) <- titulo
  
  if(nrow(total_operaciones)>0) {
    
    calculo[1, 'Beneficio'] <- nvl(tail(total_operaciones, 1)$BenPorcentual, 0)
    calculo[1, 'MaxDrawdown'] <- nvl(min(total_operaciones$BenPorcentual), 0)
    calculo[1, 'MaxRunup'] <- nvl(max(total_operaciones$BenPorcentual), 0) # Definición Runup (42'): 
    calculo[1, 'Acierto'] <- nvl(nrow(total_operaciones[total_operaciones$beneficio>0.0 & total_operaciones$accion=='VENDER',]) / 
                                   nrow(total_operaciones[total_operaciones$accion=='VENDER',])*100, 0)
    calculo[1, 'GananciaMedia'] <- (nvl(mean(total_operaciones[total_operaciones$beneficio>0.0 & total_operaciones$accion=='VENDER',]$beneficio), 0))/fondos * 100
    calculo[1, 'PerdidaMedia'] <- (nvl(mean(total_operaciones[total_operaciones$beneficio<0.0 & total_operaciones$accion=='VENDER',]$beneficio), 0))/fondos * 100
    calculo[1, 'DuracionMedia'] <- nvl(mean(total_operaciones[total_operaciones$dias_abierto>0,]$dias_abierto), 0)
    calculo[1, 'OpRealizadas'] <- nvl(nrow(total_operaciones), 0)
  }
  return(calculo)
}

calcularBeneficios <- function(listado_operaciones, fondos, diversificacion, start_year, end_year){
  ## Para debugear:
  # fondos <- 40000
  # diversificacion <- 4
  # start_year <- '2000'
  # end_year   <- '2019'
  # year <- 2018
  
  calcularOperaciones <- function(listado_operaciones, fondos, diversificacion, year){
    orden_operaciones <- listado_operaciones[listado_operaciones$fecha>year & listado_operaciones$fecha<year+1,] 
    rownames(orden_operaciones) <- NULL 
    orden_operaciones <- na.omit(orden_operaciones)
    
    if(nrow(orden_operaciones)>0){
      orden_operaciones$operaciones <- 0
      orden_operaciones$capital_disponible <- 0
      orden_operaciones$acciones <- 0
      orden_operaciones$inversion <- 0
      orden_operaciones$beneficio <- 0
      orden_operaciones$BenAcumulado <- 0
      orden_operaciones$BenPorcentual <- 0
      orden_operaciones$dias_abierto <- 0
      orden_operaciones$disposicion <- 0
      
      # Df "Cartera", con las posiciones que vamos teniendo abiertas:
      cartera <- data.frame(matrix(ncol = 7, nrow=0))
      colnames(cartera) <- c('fecha', 'ticker', 'acciones', 'precio', 'porcentaje', 'inversion', 'disposicion')
      
      # Renombramos ciertas variables:
      operaciones <- diversificacion
      capital_inicial <- fondos
      capital <- capital_inicial
      
      for(i in 1:nrow(orden_operaciones)){  
      # Modificacion de columna "operaciones", en función de las ventas:
      # Nos actualiza la cantidad de "huecos" que quedan libres
      if(orden_operaciones[i,]$accion == 'COMPRAR' & operaciones > 0){
        orden_operaciones[i,]$operaciones <- operaciones -1
        porcentaje_aplicado <- orden_operaciones[i,]$porcentaje
        
        # Cuanto capital tenemos y entre cuantas empresas queremos repartir la cartera, por el ratio (siempre será 1)/precio activo 
      acciones_a_comprar <- floor((capital/operaciones)*(porcentaje_aplicado/100)/orden_operaciones[i,]$precio)
      if (acciones_a_comprar >0){
        orden_operaciones[i,]$acciones <- acciones_a_comprar
        # Inversion - : Compra & Inversion + : Venta
        orden_operaciones[i,]$inversion <- (floor(capital/operaciones*(porcentaje_aplicado/100) / orden_operaciones[i,]$precio) * orden_operaciones[i,]$precio)*-1
        orden_operaciones[i,]$disposicion <- (capital/operaciones)
        capital <- capital - (capital/operaciones)
        orden_operaciones[i,]$capitaldisponible <- capital
        operaciones <- orden_operaciones[i,]$operaciones
        orden_operaciones[i,]$BenPorcentual <- orden_operaciones[i,]$BenAcumulado / capital_inicial *100
        aux <- orden_operaciones[i,]$porcentaje
        orden_operaciones[i,]$porcentaje <- porcentaje_aplicado
        cartera <- rbind(cartera,orden_operaciones[i,c('fecha','ticker','acciones','precio','porcentaje','inversion',
                                                       'disposicion')])
        orden_operaciones[i,]$porcentaje <- aux
       }
      }
      # En caso de que la operación sea venta, verificamos si está en cartera y ejecutamos dicha venta:
        else if (orden_operaciones[i,]$accion == 'VENDER' & nrow(cartera[cartera$ticker==orden_operaciones[i,]$ticker,])>0){
          orden_operaciones[i,]$operaciones <- operaciones +1
          orden_operaciones[i,]$acciones <- as.integer(cartera[cartera$ticker==orden_operaciones[i,]$ticker,]$acciones)
          orden_operaciones[i,]$inversion <- orden_operaciones[i,]$acciones * orden_operaciones[i,]$precio
          orden_operaciones[i,]$beneficio <- as.integer(cartera[cartera$ticker==orden_operaciones[i,]$ticker,]$inversion) + orden_operaciones[i,]$inversion
          orden_operaciones$BenAcumulado <- cumsum(orden_operaciones$beneficio)
          orden_operaciones[i,]$BenPorcentual <- orden_operaciones[i,]$BenAcumulado / capital_inicial *100
          orden_operaciones[i,]$dias_abierto <- as.Date(as.character(orden_operaciones[i,]$fecha), format = "%Y-%m-%d")-
            as.Date(as.character(cartera[cartera$ticker==orden_operaciones[i,]$ticker,]$fecha), format="%Y-%m-%d")  
          capital <- capital + as.numeric(orden_operaciones[i,]$inversion) + (cartera[cartera$ticker==orden_operaciones[i,]$ticker,]$inversion + 
                                                                                cartera[cartera$ticker==orden_operaciones[i,]$ticker,]$disposicion )
          operaciones <- orden_operaciones[i,]$operaciones
          orden_operaciones[i,]$capitaldisponible <- capital
          cartera <- cartera[!cartera$ticker==orden_operaciones[i,]$ticker,]
        } 
      }
      total_operaciones <- orden_operaciones[orden_operaciones$acciones>0,]
    }else{
      total_operaciones <- orden_operaciones[orden_operaciones$acciones>0,]  
    }
    return(total_operaciones)
  }
# Inicializamos vacios: Historico, operaciones_historicas y lista (contiene ambos DF)

  historico <- data.frame()
  operaciones_historicas <- data.frame()
  lista <- list()
  
  # Procesamiento en paralelo: Año a año!
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  total_operaciones <- foreach(year = start_year:end_year) %dopar%{
    calcularOperaciones(listado_operaciones, fondos, diversificacion, year)
  }
  stopCluster(cl)
  
  names(total_operaciones) <- c(start_year:end_year)
  
  for(year in names(total_operaciones)){
    operaciones_historicas <- rbind(operaciones_historicas, total_operaciones[[year]])
    historico <- rbind(historico,backtesting(year, total_operaciones[[year]], fondos))
  }
  
  rownames(historico) <- c(start_year:end_year)
  
  rownames(operaciones_historicas) <- NULL
  
  # Añadimos cada elemento a la lista:
  lista[[1]] <- historico
  lista[[2]] <- operaciones_historicas
  
  return(lista)
  
}  

# Descarga de datos: ------------------------------------------------------
## Ejecutamos a ultima hora de mercado

## Creamos un "cronómetro".
start.time <- Sys.time()

USA_STOCK <- descarga_lista_valores()

# Finviz: Lista de empresas recomendadas, sin deuda, con earning date (entrega 
# resultados) lejano.


# tryCatch, error, finally:

tryCatch(
  {
    # cantidad total de cores:
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    lista_recomendador <- foreach(i = c('Mega', 'Large', 'Mid', 'Small')) %dopar%{
      recomendador_finviz(i)
    }   
    stopCluster(cl)
    # Unimos todos los elementos de la lista, en un solo DF, uniendo por filas 
    recomendador <- do.call("rbind", lista_recomendador)
    recomendador <- recomendador[order(recomendador$Ticker),]
    rownames(recomendador) <- recomendador$Ticker 
    write.csv2(recomendador, file = "recomendador.csv", sep = ";")
    print("Recomendador de Finviz descargado")
    recomendador <- read.csv(file = "recomendador.csv", header = T, row.names = 1,
                             check.names = F, sep = ";", stringsAsFactors = F, dec = ",")
  },
  error = function(e)
  {
    print("No fue posible descargar los datos de Finviz multihilo, realizamos descarga secuencial")
    # Inicializamos una lista de 4 posiciones vacia
    lista_recomendador <- vector(mode = "list", length = 4)
    # Ejecutamos 1 a 1 y guardamos en su posicion
    lista_recomendador[[1]] <- recomendador_finviz('Mega')
    lista_recomendador[[2]] <- recomendador_finviz('Large')
    lista_recomendador[[3]] <- recomendador_finviz('Mid')
    lista_recomendador[[4]] <- recomendador_finviz('Small')
    # Repetimos los pasos
    recomendador <- do.call("rbind", lista_recomendador)
    recomendador <- recomendador[order(recomendador$Ticker),]
    rownames(recomendador) <- recomendador$Ticker
    write.csv2(recomendador, file = "recomendador.csv", sep = ";")
    print("Recomendadiones Finviz descargadas")
    
  },
  finally = 
    {
    recomendador <- read.csv(file = "recomendador.csv", header = T, row.names = 1,
                               check.names = F, sep = ";", stringsAsFactors = F, dec = ",")
    }
)


# Marcamos fecha de inicio y fin de descarga:
start <- as.Date("2007-01-01")
end <- Sys.Date()+1 # el +1 nos permite coger el dia de hoy (que considera "mañana)

USA_STOCK_LIST <- descarga_datos(start, end, "daily", recomendador)

# Volcamos de XTS a un listado de DF (de modo que no vamos a ensuciar los datos originales de Yahoo)
USA_STOCK_LIST_ALGO <- list()

for (i in names(USA_STOCK_LIST)){
  stock <- data.frame(USA_STOCK_LIST[[i]])
  stock$Mercado <- USA_STOCK[gsub('-', '.', i),]$Mercado
  stock$Sector <- USA_STOCK[gsub('-', '.', i),]$Sector  
  USA_STOCK_LIST_ALGO[[i]] <- stock
}

# Lista de empresas
empresas <- names(USA_STOCK_LIST)
stopCluster(cl)


# Aplicamos el algoritmo --------------------------------------------------

## Identificamos la cantidad de nucleos que vamos a utilizar
cl <- makeCluster(5, outfile = "")


## Comienza la paralelización
registerDoParallel(cl)

# Listado de DF. Buscamos añadir las columnas de compra/venta pertinentes para cada valor:
USA_STOCK_LIST_ALGO = foreach(i = 1:length(USA_STOCK_LIST_ALGO)) %dopar%{
  algoritmo_ikaris(USA_STOCK_LIST_ALGO[[i]], 10^9) # 1000000000. 
  # El algoritmo tiene dos variables(stock_ticcker y capitalización bursatil minima del activo)
}

# Columnas a crear:
# Compra: 100/0 (compramos o no)
# Venta: 100/0 (vendemos o no)
# id_compra: Conteo de operaciones por accion, sobre el algoritmo
# Stop_M: Precio de venta a cierre (a mano)
# Stop_A: Precio programado de emergencia (por debajo del Manual)
# PO: Precio sobre el que se activa "trailing stop". A este precio variamos 
## Stop_M, Stop_a y PC
# Posiciones: Nº veces que hemos variado "PO"
# Precio: Precio al que se ejecuta la orden (Compra, Stop_M, Stop_A)

## EJEMPLO: COMENTAR PARA LA EJECUCIÓN DEL ALGORITMO
# prueba <- USA_STOCK_LIST_ALGO[["LULU"]]


# Cerramos los nucleos y añadimos el nombre de las empresas
stopCluster(cl)
names(USA_STOCK_LIST_ALGO) <- empresas


# Preparamos el Backtesting -----------------------------------------------
listado_operaciones <- data.frame()

# Creamos bucle:
for(i in names(USA_STOCK_LIST_ALGO)){
  # ejemplifico para google: Descomentar la linea posterior para el ejemplo:
  # i <- 'GOOGL'
  stock <- USA_STOCK_LIST_ALGO[[i]]
  compras <- subset(stock, compra > 0,
                    select <- c(precio, compra, id_compra, macd, ROC.1, ATRSP, Volatilidad, VolValor.1, Sharpe.1, Sector, Mercado, CMO))
  ventas <- subset(stock, venta > 0,
                    select <- c(precio, venta, id_compra, macd, ROC.1, ATRSP, Volatilidad, VolValor.1, Sharpe.1, Sector, Mercado, CMO))
  
  if(nrow(compras)>0 & nrow(ventas)>0){
    colnames(compras) <- c('precio', 'porcentaje', 'id', 'macd', 'ROC', 'ATRSP', 'volatilidad', 'VolValor', 'Sharpe', 'Sector', 'Mercado', 'CMO')
    compras$id <- paste0(compras$id, i) # Id + ticker
    compras$Ticker <- i
    compras$accion <- 'COMPRAR'
    compras$fecha <- rownames(compras)
    rownames(compras) <- NULL
    
    colnames(ventas) <- c('precio', 'porcentaje', 'id', 'macd', 'ROC', 'ATRSP', 'volatilidad', 'VolValor', 'Sharpe', 'Sector', 'Mercado', 'CMO')
    ventas$id <- paste0(ventas$id, i) # Id + ticker
    ventas$Ticker <- i
    ventas$accion <- 'VENDER'
    ventas$fecha <- rownames(ventas)
    rownames(ventas) <- NULL  
    
    operativa <- rbind(compras, ventas)
    operativa <- operativa[order(operativa$fecha, operativa$accion),]
    rownames(operativa) <- NULL
    
    listado_operaciones <- rbind(listado_operaciones, operativa)
  }
}

listado_operaciones <- merge(listado_operaciones, recomendador[,c("Ticker", "Recom")], by = "Ticker", all.x = TRUE)
colnames(listado_operaciones) <- c('ticker', 'precio', 'porcentaje', 'id', 'macd', 'ROC', 'ATRSP', 'volatilidad', 'VolValor', 'Sharpe', 'Sector', 'Mercado',
                                   'CMO', 'accion', 'fecha', 'Recom')

# Ordenamos el DF:
listado_operaciones <- listado_operaciones[order(listado_operaciones$fecha, listado_operaciones$accion, -rank(as.numeric(listado_operaciones$Sharpe))),]
rownames(listado_operaciones) <- NULL

# Aplicamos la función:

ranking <- generar_ranking(listado_operaciones)


# Qué comprar: ------------------------------------------------------------

# Si lo ejecutamos en Lunes antes de que el mdo abra: fecha <- Sys.Date() -3
# Si lo ejecutaos cualquier dia (que no sea lunes) de forma previa a la apertura del mercado (de 0:00-14:30/15:30): Sys.Date()-1
# Si lo ejecutamos en Sabado: fecha <- Sys.Date() -1
# Si lo ejecutamos en Domingo: fecha <- Sys.Date() -2

## OJO que esto puede dar error en función del dia:
fecha <- Sys.Date()

# Me quedo con los valores de compra que tengan fecha de hoy:
# Creamos un Df con la info de todos los Df previos, y creamos: que_comprar

que_comprar <- recomendador[recomendador$Ticker %in% listado_operaciones[listado_operaciones$accion=='COMPRAR'
                                                                         & listado_operaciones$fecha == fecha,]$ticker,]


total_valor <- data.frame()
for(i in rownames(que_comprar)){
  valor <- tail(USA_STOCK_LIST_ALGO[[i]], 1)[, c('Close', 'ATRM', 'CMO', 'Sharpe', 'Mercado', 'STOP_M', 'STOP_A', 'precio')]
  rownames(valor) <- i
  total_valor <- rbind(total_valor, valor)
}

que_comprar <- merge(que_comprar, total_valor, by = 0, all.x = T)
row.names(que_comprar) <- que_comprar$Ticker

# Columnas que usaremos en el mail, ordenadas por Ratio de Sharpe:
que_comprar <- que_comprar[,c("Ticker", "Company", "Sector", "Country", "Close", "CMO", "Sharpe", "Mercado", "EPS", "52W High", "Recom", 
                              "Change", "Earnings", "IPO Date", "ATRM")]
que_comprar <- que_comprar[order(-que_comprar$Sharpe),]


# Redondeamos valores:
que_comprar$CMO <- round(que_comprar$CMO, 2)
que_comprar$ATRM <- round(que_comprar$ATRM, 2)
que_comprar$Sharpe <- round(que_comprar$Sharpe, 2)

# Activos a seleccionar. Distintos criterios:
elegidas <- ranking[ranking$Ticker %in% rownames(que_comprar) & (ranking$Ratio.Exito > 1.3 & ranking$Num.Operaciones>=30), c("Ticker", "Prob.Exito",
                                                                                                                             "Ratio.Exito", "Num.Operaciones")]

# alternativa:
# elegidas2 <- ranking[ranking$Ticker %in% rownames(que_comprar) & (ranking$Ratio.Exito > 1), c("Ticker", "Prob.Exito", "Ratio.Exito", "Num.Operaciones")]

rownames(elegidas) <- elegidas$Ticker
que_comprar <- merge(que_comprar, elegidas, by = 0, all.x = T)
row.names(que_comprar)<- que_comprar$Ticker.x
que_comprar <- que_comprar[, c("Row.names", "Company", "Sector", "Country", "Close", "CMO", "Sharpe", 
                                "Mercado", "EPS", "52W High", "Recom", "Change", "Earnings", 
                                "IPO Date", "Prob.Exito", "Ratio.Exito", 
                                "Num.Operaciones", "ATRM")]

colnames(que_comprar)[1] <- "Ticker" 
que_comprar <- que_comprar[order(-que_comprar$Ratio.Exito), ]



# Mail: -------------------------------------------------------------------
presupuesto <- 15000 # Por activo
riesgo <- 0.01 # Por activo

operaciones <- data.frame(matrix(ncol = 6, nrow = nrow(que_comprar)))
colnames(operaciones) <- c("Precio Compra", "Stop Loss Manual", "Stop Loss Automatico", "Precio Objetivo", "Posicion", "Dinero necesario")
rownames(operaciones) <- rownames(que_comprar)

for(i in rownames(que_comprar)){
  operaciones[i,]$`Precio Compra` <- que_comprar[i,]$Close
  operaciones[i,]$`Stop Loss Manual` <- que_comprar[i,]$Close - que_comprar[i,]$ATRM
  operaciones[i,]$`Stop Loss Automatico` <- que_comprar[i,]$Close - que_comprar[i,]$ATRM*2
  operaciones[i,]$`Precio Objetivo` <- que_comprar[i,]$Close + que_comprar[i,]$ATRM
  operaciones[i,]$Posicion <- round((presupuesto*riesgo)/que_comprar[i,]$ATRM, 0)
  operaciones[i,]$`Dinero necesario` <- paste(round(operaciones[i,]$Posicion * que_comprar[i,]$Close, 2), '$')
}


# Convertimos el DF "que_comprar" a html_table:
y <- htmlTable(que_comprar, rnames = FALSE, align = "c", col.rgroup = c("none", "#58D3F7"))

# Cuerpo del email: Inicializamos las variables a 0
fotos <- ""
foto <- ""

# Para cada activo: 
for(i in rownames(que_comprar)){
  foto <- paste0(htmlTable(que_comprar[i, c("Ticker", "Company"	, "Sector", "Country", 	"Mercado", 	"Earnings", "IPO Date")],
                           rnames = F, align = "c", col.rgroup = c("none", "#58D3F7")),
                 htmlTable(que_comprar[i, c("Close", "CMO", "EPS", "52W High", "Change")],
                           rnames = F, align = "c", col.rgroup = c("none", "#58D3F7")),
                 htmlTable(que_comprar[i, c("Sharpe",	"Recom",	"Prob.Exito",	"Ratio.Exito", "Num.Operaciones")],
                           rnames = F, align = "c", col.rgroup = c("none", "#58D3F7")),
                 htmlTable(operaciones[i,], rnames = F, align = "c", col.rgroup = c("none", "#58D3F7")),
                 # Grafico chartista:
                 "<p> <img src=\"https://finviz.com/chart.ashx?t=", i, "&ty=c&ta=1&p=d&s=l\" </p> />")
  fotos <- paste0(fotos, foto)
}

html_body <- paste0(y, fotos)

sender <- "cursofarobursatil@gmail.com"
recipients <- c("alberto.aguilera@farobursatil.com", "leon.belena@farobursatil.com")
recipients_2 <- c()

send.mail(from = sender,
          to = recipients,
          bcc = recipients_2,
          subject = paste("Recomendaciones para el día bueno", fecha),
          #body = "test",
          body = html_body,
          html = TRUE,
          smtp = list(host.name = "smtp.gmail.com", port = 465, 
                      user.name = "cursorfarobursatil@gmail.com",            
                      passwd = "FaroBursatil2021.", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE,
          debug = T)

# Puerto 25: Si da error el procedimiento de seguridad:

send.mail(from = "@gmail.com",
          to = "@gmail.com",
          subject = "Subject of the email",
          body = "Body of the email",
          smtp = list(host.name = "aspmx.l.google.com", port = 25),
          authenticate = FALSE,
          send = TRUE)

# Si no queremos enviar un mail, ejecutamos este comando:
htmlTable(paste0(y, fotos))

# Tiempo que ha tardado toda la ejecución
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


# Backtesting de la estrategia --------------------------------------------
## Esta sección ya no forma parte del algoritmo.
# Obtenemos los valores en función del ratio de Sharpe.
resultados <- calcularBeneficios(listado_operaciones, 40000, 5, 2017, 2021)

# Resultado anual de las operaciones:
historico <- resultados[[1]]

# Resultado de todas las operaciones que se han realizado por año:
historico_operaciones <- resultados[[2]]


# Trampa de Overfitting: jugar con este fichero, y re-ejecutar "resultados"
listado_operaciones <- listado_operaciones[listado_operaciones$ticker %in% ranking[ranking$Ratio.Exito>3.3,]$Ticker,]


