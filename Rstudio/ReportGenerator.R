################################################################

# Financial Report Generation - 2023/2024.
# Jorge.Lozano - "BullishReport.com"

################################################################

library(httr)
library(jsonlite)
library(tidyverse)
library(knitr)
library(kableExtra)

API_KEY <- "d2855b21641f2dff7fbee3b4ba44f47c"
params <- list(apikey = "d2855b21641f2dff7fbee3b4ba44f47c")

options(scipen = 999)

################################################################

Ticker <- 'PEP' 

################################################################

# I. Script:

  # 1. Overview:  
  {
  # I. Extracción Data: Outlook Metrics.
  
  # 1.1 Métricas de portada:

    URL_Overview <-  paste0("https://financialmodelingprep.com/api/v4/company-outlook?symbol=", Ticker, "&apikey=", API_KEY) #Extraemos algunas métricas. Resto más abajo 1.5
  
      response <- httr::GET(URL_Overview)
      Overview <- jsonlite::fromJSON(content(response, "text")) # class(Overview) = "list"
  
    metric.name <- Overview$profile[["companyName"]]    # Nombre Compañía
    metric.index <- Overview$profile$exchangeShortName  # Nombre Índice
    metric.image <- Overview$profile$image              # Extraemaos la imagen de empresa
  
  # 1.2 Stock Price y Volume
    
    URL_StockPrice <- paste0("https://financialmodelingprep.com/api/v3/quote-short/", Ticker, "?apikey=", API_KEY)
  
      response <- httr::GET(URL_StockPrice)
      StockPrice <- jsonlite::fromJSON(content(response, "text"))
  
    Stock.Price <- StockPrice$price
    Stock.Volume <- StockPrice$volume
  
  # 1.3 Price Change
  
    URL_PriceChange <- paste0("https://financialmodelingprep.com/api/v3/stock-price-change/", Ticker, "?apikey=", API_KEY)
  
      response <- httr::GET(URL_PriceChange)
      Price.Change <- jsonlite::fromJSON(content(response, "text"))
  
    Price.Change.5d <- round(Price.Change[,3], 2)
    Price.Change.ytd <- round(Price.Change[,4], 2)
  
  # 1.4 AltmanZ-Score & Piotroski-Score
  
    URL_Score <- paste0("https://financialmodelingprep.com/api/v4/score?symbol=", Ticker, "&apikey=", API_KEY)
  
      response <- httr::GET(URL_Score)
      Score <- jsonlite::fromJSON(content(response, "text"))
  
    Z.Score <- round(Score$altmanZScore, 2) # Calificación Solvencia 
    Priotoski.Score <- round(Score$piotroskiScore, 2) # Calificación 'Value Investing' 
  
  # 1.5 Price Target Consensus
  
    URL_PriceTarget <- paste0("https://financialmodelingprep.com/api/v4/price-target-consensus?symbol=", Ticker, "&apikey=", API_KEY)
  
      response <- httr::GET(URL_PriceTarget)
      PriceTarget <- jsonlite::fromJSON(content(response, "text"))
  
    TargetConsensus <- PriceTarget$targetConsensus 
  
    # Margen Seguridad: 
      MargenSeguridad <- round(((TargetConsensus - Stock.Price) / Stock.Price) * 100, 0)
  
  # 1.6 Metrics Outlook. (Endpoint extraído en 1.1)
  
    # Extraer las métricas 'Overview'
      metric.price <- Overview$profile$price
      metric.beta <- Overview$profile$beta
      metric.volAvg <- round((Overview$profile$volAvg)/1e6, 3) # En millones
      metric.mktCap <- Overview$profile["mktCap"]
      metric.lastDiv <- Overview$profile["lastDiv"]
      metric.range <- Overview$profile["range"]
      metric.change <- Overview$profile["changes"]
      metric.sector <- Overview$profile$sector # Segunga forma extraer datos de una lista
      metric.industry <- Overview$profile$industry
      metric.website <- Overview$profile$website
      metric.description <- Overview$profile$description
      metric.country <- Overview$profile$country
      metric.ceo <- Overview$profile$ceo
      metric.employees <- Overview$profile$fullTimeEmployees
      metric.ipodate <- Overview$profile$ipoDate
      
      metric.Per <- Overview$ratios$peRatioTTM
      metric.roce <- Overview $ratios$returnOnCapitalEmployedTTM
      
  # 1.7 Earnings Estimates
      
    URL_EarningsCalendar <-  paste0("https://financialmodelingprep.com/api/v3/earning_calendar?apikey=", API_KEY)
      
      response <- httr::GET(URL_EarningsCalendar)
      EarningsCalendar <- jsonlite::fromJSON(content(response, "text"))
      
    Earnings.Estimates <- EarningsCalendar %>% filter(symbol == Ticker)
    Earnings.Estimates <- Earnings.Estimates[1, ] # Aquí se encuentran las variables de: earnings.date, estimates(eps y revenue) y los que se confirmarán en dicha fecha.
    Earnings.Estimates <- Earnings.Estimates[1,4]
  
    }      
  # 2.Financial Statement:
  {
  # I. Segments:
    {
    # 2.1 Segments: 
    
    # 2.1.1. Revenue:
    
    URL_Segment.Revenue <-  paste0("https://financialmodelingprep.com/api/v4/revenue-product-segmentation?symbol=", Ticker, "&structure=flat&apikey=", API_KEY)
    
      response <- httr::GET(URL_Segment.Revenue)
      Segment.Revenue <- jsonlite::fromJSON(content(response, "text")) 
    
    Segment.Revenue <- as.list(Segment.Revenue)
    
    # Transformación de datos:     
    if (length(Segment.Revenue) > 0){    
      
      # Extraemos los años que vamos a :
      nombres_columnas <- names(Segment.Revenue[1:5])
      años <- lapply(nombres_columnas, function(x) substr(x, 1, 4))
      años <- años[!is.na(años)]
      
      ##length(años) 
      
      # Ajustamos los valores a Billones o Millones
      BillorMillion <- if (Segment.Revenue[[1]][1,1] / 1e9 > 1){BillorMillion <- 1e9
      } else {BillorMillion <- 1e6}
      
      # Crear una lista para almacenar las tablas
      tabla_lista <- list()
      
      # Crear y almacenar las tablas en la lista
      for (i in 1:length(años)) {
        tabla <- Segment.Revenue[[i]] %>%
          slice(i) %>%
          t() %>%
          {./ BillorMillion} %>%
          round(2) %>%
          as.data.frame() %>%
          setNames(años[[i]])
        
        tabla <- data.frame(Name = rownames(tabla), tabla)
        tabla_lista[[i]] <- tabla
      }
      
      # Fusionar las tablas en una sola tabla
      Segment.Revenue.Table <- tabla_lista[[1]]
      for (i in 2:length(tabla_lista)) {
        Segment.Revenue.Table <- merge(Segment.Revenue.Table, tabla_lista[[i]], by = "Name", all = TRUE)
      }
      # Comprobación y cambiamos los nombres
        Segment.Revenue.Table[is.na(Segment.Revenue.Table)] <- 0
        colnames(Segment.Revenue.Table)[-1] <- años
        # print(Segment.Revenue.Table)
      
      if (BillorMillion == 1e9) {print("Data in Bn$")
      } else {print("Data in Mill$")}
      
    } else {print("No Data")}
    
    
    # 2.1.2. Geography:
    
      URL_Segment.Geo <-  paste0("https://financialmodelingprep.com/api/v4/revenue-geographic-segmentation?symbol=", Ticker, "&structure=flat&apikey=", API_KEY)
    
        response <- httr::GET(URL_Segment.Geo)
        Segment.Geo <- jsonlite::fromJSON(content(response, "text")) 
    
      Segment.Geo <- as.list(Segment.Geo)
    
    # Transformación de datos:     
      if (length(Segment.Geo) > 0){    
      
      # Extraemos los años que vamos a :
        nombres_columnas <- names(Segment.Geo[1:5])
        años <- lapply(nombres_columnas, function(x) substr(x, 1, 4))
        años <- años[!is.na(años)]
      
      # Ajustamos los valores a Billones o Millones
        BillorMillion <- if (Segment.Geo[[1]][1,1] / 1e9 > 1){BillorMillion <- 1e9
        } else {BillorMillion <- 1e6}
      
      # Crear una lista para almacenar las tablas
        tabla_lista <- list()
      
      # Crear y almacenar las tablas en la lista
        for (i in 1:length(años)) {
          tabla <- Segment.Geo[[i]] %>%
            slice(i) %>%
            t() %>%
            {./ BillorMillion} %>%
            round(2) %>%
            as.data.frame() %>%
            setNames(años[[i]])
        
        tabla <- data.frame(Name = rownames(tabla), tabla)
        tabla_lista[[i]] <- tabla
      }
      
      # Fusionar las tablas en una sola tabla
        Segment.Geo.Table <- tabla_lista[[1]]
        for (i in 2:length(tabla_lista)) {
          Segment.Geo.Table <- merge(Segment.Geo.Table, tabla_lista[[i]], by = "Name", all = TRUE)
        }

      # Comprobación y cambiamos los nombres
        Segment.Geo.Table[is.na(Segment.Geo.Table)] <- 0
        colnames(Segment.Geo.Table)[-1] <- años
        # print(Segment.Geo.Table)
      
      if (BillorMillion == 1e9) {print("Data in Bn$")
      } else {print("Data in Mill$")}
      
    } else {print("No Data")}
    
}
  # II. Financial Core:
    {
    # 1. Income Statement: 
      {
      # 1.1 Income Statenent: 
      {
      Income.Anual <- Overview$financialsAnnual$income
      
      min_cols <- min(5, nrow(Income.Anual)) # Logica para encontrar el mínimo de columnas, de modo que serán 5 salvo cuando existan menos de 5.(PD: en realidad es num filas pro la posicion horizontal de como se muestran las columnas de Income.Anual, luego se han de trasnponer)
      Income.Anual.Table <- as.data.frame(t(Income.Anual)) %>% select(1:min_cols) # Trasponemos y seleccionamos max 5 columnas
      
      Date.Income <- Income.Anual.Table[1,] %>% lapply(function(x) substr(x, 1, 4)) # Extraemos el año de cada columma 
      colnames(Income.Anual.Table) <- Date.Income # Cambiamos los nombres de las columnas por los años extraidos en Date.Income
      
      Income.finalLink <- Income.Anual[1,"finalLink"]
      
      BillorMillion <- if (as.numeric(Income.Anual.Table[9,1]) / 1e9 > 10){BillorMillion <- 1e9 # Es una buena práctica para entender si los datos son $Bn o $Mll
      } else {BillorMillion <- 1e6}
      
      Income.Anual.Table <- Income.Anual.Table %>%  tibble::rownames_to_column(var = "Name") %>% # Convertimos Nombres de rowIndex en Columna
                                filter(Name %in% c("revenue", "grossProfit", "incomeBeforeTax", "netIncome", "eps", "ebitda" )) %>% #Seleccionamos las variables que queremos 
                                mutate(across(-1, as.numeric)) # Salvo la primera columna, convertir todo a numeric
      
      
      max_eps <- max(abs(Income.Anual.Table[Income.Anual.Table$Name == "eps", -1])) # Esta lógica se hace para dividir debajo toda la tabla por BillorMillion salvo EPS.
      Income.Anual.Table <- Income.Anual.Table %>%
                                mutate(across(-Name, ~ ifelse(abs(.) > max_eps, . /BillorMillion, .)))#Descartando la columna Name, todo lo que sea superior al EPS se divide por BillorMillion, el resto se queda igual.
      Income.Anual.Table <- Income.Anual.Table %>% mutate(across(-Name, ~ round(., 2))) # Salvo la columna Name, redondear todo a 2.
      
      if (BillorMillion == 1e9) {print("Data in Bn$") # Es una buena práctica para entender si los datos son $Bn o $Mll
      } else {print("Data in Mill$")}
      
      Income.Anual.Table <- Income.Anual.Table %>% select(Name, rev(names(.))) # Invertimos orden de tabla para mayor legibilidad 
      
      # View(Income.Anual.Table)
      }
      # 1.2 Cambiamos nombre de Income Statement: 
      {
        # Seleccion Name de Metrics
        Income_name <- c(
          "revenue" = "Revenue",                
          "grossProfit" = "Gross Profit",          
          "ebitda" = "Ebitda",
          "incomeBeforeTax" = "Ebit",         
          "netIncome" = "Net Income",            
          "eps" = "EPS" 
        )
        
        # Actualizar los nombres de la columna 'Name' según nombre
        Income.Anual.Table$Name <- Income_name[Income.Anual.Table$Name]
      
      # print(Income.Anual.Table$Name)
      
      if (BillorMillion == 1e6) {
        BillorMillion.Income <- "Data in Millions $ (expect eps)"
      } else if (BillorMillion == 1e9) {
        BillorMillion.Income <- "Data in Billions $ (expect eps)"
      }
        
        
        
      }
    }
    # 2. Balance Sheet:
      {
        # 2.1 Balance Sheet:
        {
      Balance.Anual <- Overview$financialsAnnual$balance
      
      min_cols <- min(5, nrow(Balance.Anual)) # Logica para encontrar el mínimo de columnas, de modo que serán 5 salvo cuando existan menos de 5.(PD: en realidad es num filas pro la posicion horizontal de como se muestran las columnas de Income.Anual, luego se han de trasnponer)
      Balance.Anual.Table <- as.data.frame(t(Balance.Anual)) %>% select(1:min_cols) # Trasponemos y seleccionamos max 5 columnas
      
      Date.Balance <- Balance.Anual.Table[1,] %>% lapply(function(x) substr(x, 1, 4)) # Extraemos el año de cada columma 
      colnames(Balance.Anual.Table) <- Date.Balance # Cambiamos los nombres de las columnas por los años extraidos en Date.Income
      
      BillorMillion <- if (as.numeric(Balance.Anual.Table[9,1]) / 1e9 > 10){BillorMillion <- 1e9 # Es una buena práctica para entender si los datos son $Bn o $Mll
      } else {BillorMillion <- 1e6}
      
      Balance.Anual.Table <- Balance.Anual.Table %>%  tibble::rownames_to_column(var = "Name") %>% # Convertimos Nombres de rowIndex en Columna
                                filter(Name %in% c("totalCurrentAssets", "totalNonCurrentAssets", "totalAssets", "totalCurrentLiabilities", "totalNonCurrentLiabilities",
                                                    "totalLiabilities", "totalEquity", "totalDebt", "netDebt" )) %>% #Seleccionamos las variables que queremos 
                                mutate(across(-1, as.numeric)) # Salvo la primera columna, convertir todo a numeric
      
      
      Balance.Anual.Table <- Balance.Anual.Table %>%
                                 mutate(across(-Name, ~ ./BillorMillion)) # Descartando la columna Name, todos los valores se dividen por BillorMillion
      # Redondeamos a 2:
      Balance.Anual.Table <- Balance.Anual.Table %>% mutate(across(-Name, ~ round(., 2))) # Salvo la columna Name, redondear todo a 2.
      
      if (BillorMillion == 1e9) {print("Data in Bn$") # Es una buena práctica para entender si los datos son $Bn o $Mll
      } else {print("Data in Mill$")}
      
      Balance.Anual.Table <- Balance.Anual.Table %>% select(Name, rev(names(.))) # Invertimos orden de tabla para mayor legibilidad 
      
      # View(Balance.Anual.Table)
      }
        # 2.2 Cambiamos nombre a Balance Statement: 
        {
          # Seleccion Name de Metrics
          Balance_name <- c(
            "totalCurrentAssets" = "Current Assets",                
            "totalNonCurrentAssets" = "Long-Term Assets",          
            "totalAssets" = "Total Assets",
            "totalCurrentLiabilities" = "Current Liabilities",         
            "totalNonCurrentLiabilities" = "Long-Term Liabilities",            
            "totalLiabilities" = "Total Liabilities",
            "totalEquity" = "Total Equity",
            "totalDebt" = "Total Debt",
            "netDebt" = "Net Debt"
          )
          
          # Actualizar los nombres de la columna 'Name' según nombre
            Balance.Anual.Table$Name <- Balance_name[Balance.Anual.Table$Name]
          
            # print(Balance.Anual.Table$Name)
            
            if (BillorMillion == 1e6) {
              BillorMillion.Balance <- "Data in Millions $"
            } else if (BillorMillion == 1e9) {
              BillorMillion.Balance <- "Data in Billions $"
            }
          
          # print(BillorMillion.Balance)
        }
      }
    # 3. Cash Flow:
      {
      # 3.1 Cash Flow:
      {
      Cash.Anual <- Overview$financialsAnnual$cash
      
      min_cols <- min(5, nrow(Cash.Anual)) # Logica para encontrar el mínimo de columnas, de modo que serán 5 salvo cuando existan menos de 5.(PD: en realidad es num filas pro la posicion horizontal de como se muestran las columnas de Income.Anual, luego se han de trasnponer)
      Cash.Anual.Table <- as.data.frame(t(Cash.Anual)) %>% select(1:min_cols) # Trasponemos y seleccionamos max 5 columnas
      
      Date.Cash <- Cash.Anual.Table[1,] %>% lapply(function(x) substr(x, 1, 4)) # Extraemos el año de cada columma 
      colnames(Cash.Anual.Table) <- Date.Cash # Cambiamos los nombres de las columnas por los años extraidos en Date.Income
      
      
      BillorMillion <- if (abs(as.numeric(Cash.Anual.Table[9,1])) / 1e9 > 10){BillorMillion <- 1e9 # Es una buena práctica para entender si los datos son $Bn o $Mll
      } else {BillorMillion <- 1e6}
      
      Cash.Anual.Table <- Cash.Anual.Table %>%  tibble::rownames_to_column(var = "Name") %>% # Convertimos Nombres de rowIndex en Columna
        filter(Name %in% c("netCashProvidedByOperatingActivities", "netCashUsedForInvestingActivites", "netCashUsedProvidedByFinancingActivities","netChangeInCash", "freeCashFlow")) %>% #Seleccionamos las variables que queremos 
        mutate(across(-1, as.numeric)) # Salvo la primera columna, convertir todo a numeric
      
      
      Cash.Anual.Table <- Cash.Anual.Table %>%
                              mutate(across(-Name, ~ ./BillorMillion)) # Descartando la columna Name, todos los valores se dividen por BillorMillion
      # Redondeamos a 2:
      Cash.Anual.Table <- Cash.Anual.Table %>% mutate(across(-Name, ~ round(., 2))) # Salvo la columna Name, redondear todo a 2.
      
      if (BillorMillion == 1e9) {print("Data in Bn$") # Es una buena práctica para entender si los datos son $Bn o $Mll
      } else {print("Data in Mill$")}
      
      Cash.Anual.Table <- Cash.Anual.Table %>% select(Name, rev(names(.))) # Invertimos orden de tabla para mayor legibilidad 
      
      # View(Cash.Anual.Table)
      }
      # 3.2 Cambiamos nombre a CashFlow Statement: 
      {
        # Seleccion Name de Metrics
        Cash_name <- c(
          "netCashProvidedByOperatingActivities" = "Operating Cash Flow",                
          "netCashUsedForInvestingActivites" = "Investing Cash Flow",          
          "netCashUsedProvidedByFinancingActivities" = "Financing Cash Flow",
          "netChangeInCash" = "Net Cash Flow",
          "freeCashFlow" = "Free Cash Flow"
        )
        
        # Actualizar los nombres de la columna 'Name' según nombre
        Cash.Anual.Table$Name <- Cash_name[Cash.Anual.Table$Name]
        
        # print(Cash.Anual.Table$Name)
        
        if (BillorMillion == 1e6) {
          BillorMillion.Cash <- "Data in Millions $"
        } else if (BillorMillion == 1e9) {
          BillorMillion.Cash <- "Data in Billions $"
        }
        
        # print(BillorMillion.Balance)
      }
    }
    }
  }  
  # 3. Ratios:
  {
    # 3.1. Extracción Ratios Annual:
    {
  # I. Extracción y Carga de Ratios Anual :
  {
    # Ratios Company:
    Ratios.Annual <-  paste0("https://financialmodelingprep.com/api/v3/ratios/", Ticker, "?period=annual&apikey=", API_KEY) #Extraemos algunas métricas. Resto más abajo 1.5
    
    response <- httr::GET(Ratios.Annual)
    Ratios.Annual <- jsonlite::fromJSON(content(response, "text")) 
    
    # Ponemos date como encabezado  
    Ratios.Annual <- as.data.frame(t(Ratios.Annual)) %>% select(1:min(5, ncol(.)))
    nombres <- as.character(unlist(Ratios.Annual[3, ]))
    names(Ratios.Annual) <- nombres
    
    # Ponemos el indice como variable Name:
    Ratios.Annual <- Ratios.Annual[-c(1:4),]
    Ratios.Annual <- Ratios.Annual %>% as.data.frame() %>% mutate(across(everything(), as.numeric)) %>% round(2)
    Ratios.Annual <- rownames_to_column(Ratios.Annual, var = "Name")
    # View(Ratios.Annual)
    
    # Agrupamos los Ratios por grupos:
    # Recogida Ratios.Valuation:
    Ratios.Annual.Valuation <- Ratios.Annual %>% filter(., Name %in% c("priceEarningsRatio", "priceEarningsToGrowthRatio", "priceToBookRatio",
                                                                       "priceToSalesRatio", "priceToOperatingCashFlowsRatio"))
    
    # Recogida Ratios.Solvencia:
    Ratios.Annual.Solvencia <- Ratios.Annual %>% filter(., Name %in% c("debtRatio", "debtEquityRatio", "totalDebtToCapitalization",
                                                                       "interestCoverage", "cashFlowToDebtRatio"))
    # Recogida Ratios.Rentabilidad:
    Ratios.Annual.Rentabilidad <- Ratios.Annual %>% filter(., Name %in% c("returnOnAssets", "returnOnEquity", "returnOnCapitalEmployed",
                                                                          "grossProfitMargin", "operatingProfitMargin", "netProfitMargin")) 
    # Recogida Ratios.Liquidez:
    Ratios.Annual.Liquidez <- Ratios.Annual %>% filter(., Name %in% c("currentRatio", "quickRatio", "cashRatio"))
    
    # Recogida Ratios.Eficiencia:
    Ratios.Annual.Eficiencia <- Ratios.Annual %>% filter(., Name %in% c("inventoryTurnover", "assetTurnover", "receivablesTurnover" ))
  }
  
  # II. Cambiamos el nombre de los Ratios para una mejor Apariencia:
  {
    # Ratios.Valuation:
    {
      # Seleccion Name de Ratios
      ratios_name <- c(
        "priceEarningsRatio" = "P/E",
        "priceEarningsToGrowthRatio" = "PEG",
        "priceToBookRatio" = "P/B",
        "priceToSalesRatio" = "P/S",
        "priceToOperatingCashFlowsRatio" = "P/CF"
      )
      
      # Cambiar el nombre de la columna 'Name' a 'Ratio'
      Ratios.Annual.Valuation <- Ratios.Annual.Valuation %>% rename(Ratio = Name)
      
      # Actualizar los nombres de la columna 'Ratio' según nombre
      Ratios.Annual.Valuation$Ratio <- ratios_name[Ratios.Annual.Valuation$Ratio]
      
      # Cambiamos orden de anual:
        Ratios.Annual.Valuation <- Ratios.Annual.Valuation %>%
            select(Ratio, everything()) %>%
            select(1, rev(seq_along(.)[-1]))
    }
    # print(Ratios.Annual.Valuation)
    
    # Ratios.Solvencia:
    {
      # Seleccion Name de Ratios
      ratios_name <- c(
        "debtRatio" = "Debt Ratio",             
        "debtEquityRatio" =  "Debt To Equity",         
        "totalDebtToCapitalization" = "Debt To Capital",
        "interestCoverage" = "Interest Coverage", 
        "cashFlowToDebtRatio" = "CashFlow to Debt"
      )
      
      # Cambiar el nombre de la columna 'Name' a 'Ratio'
      Ratios.Annual.Solvencia <- Ratios.Annual.Solvencia %>% rename(Ratio = Name)
      # Actualizar los nombres de la columna 'Ratio' según nombre
      Ratios.Annual.Solvencia$Ratio <- ratios_name[Ratios.Annual.Solvencia$Ratio]
      
      # Cambiamos orden:
      Ratios.Annual.Solvencia <- Ratios.Annual.Solvencia %>%
        select(Ratio, everything()) %>%
        select(1, rev(seq_along(.)[-1]))
    }
    # print(Ratios.Annual.Solvencia)  
    
    # Ratios.Rentabilidad:
    {
      # Seleccion Name de Ratios
      ratios_name <- c(
        "grossProfitMargin" = "Gross Margin",   
        "operatingProfitMargin" = "Operating Margin",
        "netProfitMargin" = "Net Margin",        
        "returnOnAssets" = "ROA",         
        "returnOnEquity" = "ROE",         
        "returnOnCapitalEmployed" = "ROCE"
      )
      
      # Cambiar el nombre de la columna 'Name' a 'Ratio'
      Ratios.Annual.Rentabilidad <- Ratios.Annual.Rentabilidad %>% rename(Ratio = Name)
      # Actualizar los nombres de la columna 'Ratio' según nombre
      Ratios.Annual.Rentabilidad$Ratio <- ratios_name
      
      Ratios.Annual.Rentabilidad <- Ratios.Annual.Rentabilidad %>%
        select(Ratio, everything()) %>%
        select(1, rev(seq_along(.)[-1])) %>% 
        mutate_at(vars(rev(seq_along(.)[-1])), function(x) format(round(as.numeric(x) * 100, 2), nsmall = 2))
      
    }
    # print(Ratios.Annual.Rentabilidad)   
    
    # Ratios.Liquidez:
    {
      # Seleccion Name de Ratios
      ratios_name <- c(
        "currentRatio" = "Current Ratio",         
        "quickRatio" = "Quick Turnover",
        "cashRatio" = "Cash Ratio"  
      )
      
      # Cambiar el nombre de la columna 'Name' a 'Ratio'
      Ratios.Annual.Liquidez <- Ratios.Annual.Liquidez %>% rename(Ratio = Name)
      # Actualizar los nombres de la columna 'Ratio' según nombre
      Ratios.Annual.Liquidez$Ratio <- ratios_name[Ratios.Annual.Liquidez$Ratio]
      
      Ratios.Annual.Liquidez <- Ratios.Annual.Liquidez %>%
        select(Ratio, everything()) %>%
        select(1, rev(seq_along(.)[-1]))
    }
    # print(Ratios.Annual.Liquidez) 
    
    # Ratios.Eficiencia:
    {
      # Seleccion Name de Ratios
      ratios_name <- c(
        "inventoryTurnover" = "Inventory Turnover",
        "assetTurnover" = "Asset Turnover",
        "receivablesTurnover" = "Receivables Turnover"
      )
      
      # Cambiar el nombre de la columna 'Name' a 'Ratio'
      Ratios.Annual.Eficiencia <- Ratios.Annual.Eficiencia %>% rename(Ratio = Name)
      # Actualizar los nombres de la columna 'Ratio' según nombre
      Ratios.Annual.Eficiencia$Ratio <- ratios_name[Ratios.Annual.Eficiencia$Ratio]
    
    # print(Ratios.Annual.Eficiencia) 
    Ratios.Annual.Eficiencia <- Ratios.Annual.Eficiencia %>%
      select(Ratio, everything()) %>%
      select(1, rev(seq_along(.)[-1]))
    }
  }
   }
    # 3.2. Extracción RatiosTTM C.I.S:
    {
      # 3.2.1. Unimos tabla de SP500.List con RatiosTTM
      {
        # Lista de perfiles de empresas del SP500:
        NYSE.list <- read.csv("https://financialmodelingprep.com/api/v4/profile/all?apikey=d2855b21641f2dff7fbee3b4ba44f47c")
        
        NYSE.list <- NYSE.list %>% filter(!sector == "" | !industry == "") %>% 
          filter(exchange %in% c("NASDAQ Capital Market", 
                                 "NASDAQ Global Market", 
                                 "New York Stock Exchange", 
                                 "New York Stock Exchange Arca", 
                                 "NASDAQ Global Select", 
                                 "American Stock Exchange")) %>%
          select(symbol = Symbol, sector, industry, exchange)
        
        # Listado de empresas SP500:
        # Modificaremos NYSE.list y añadiremos una nueva variable de "SP500 = T". Para hacer la media del sector de las compañías del SP500...  
        # Extracción Listado SP500:
        SP500.List <-  paste0("https://financialmodelingprep.com/api/v3/sp500_constituent?apikey=", API_KEY) #Extraemos algunas métricas. Resto más abajo 1.5
        
        response <- httr::GET(SP500.List)
        
        SP500.List <- jsonlite::fromJSON(content(response, "text")) # class(Overview) = "list"
        
        # Agregación Columna SP500
        NYSE.list <- NYSE.list %>% mutate(SP500 = FALSE)
        NYSE.list$SP500[NYSE.list$symbol %in% SP500.List$symbol] <- TRUE
        
        # Extraemos el sector e industria de la compañía para la comparativa de ratios:
        metric.sector2 <- NYSE.list %>% filter(symbol == Ticker) %>% select(sector) %>% as.character(.)
        metric.industry2 <- NYSE.list %>% filter(symbol == Ticker) %>% select(industry) %>% as.character(.)
      }
      # 3.2.2. Clasificamos RatiosTTM por C.I.S:
      {
        # 1. RatiosTTM.Company:
        {
          RatiosTTM <- as.list(Overview$ratios)
          RatiosTTM <- as.data.frame(RatiosTTM) %>% t()
          
          RatiosTTM <- data.frame(Name = rownames(RatiosTTM), Value = round(RatiosTTM,2))
          row.names(RatiosTTM) <- NULL # Quitamos las filas del índice 
          # RatiosTTM por Grupos. Company:
          # Recogida Ratios.Valuation:
          RatiosTTM.Company.Valuation <- RatiosTTM %>% filter(., Name %in% c("priceEarningsRatioTTM", "priceEarningsToGrowthRatioTTM", "priceToBookRatioTTM",
                                                                             "priceToSalesRatioTTM", "priceToOperatingCashFlowsRatioTTM"))
          # Recogida Ratios.Solvencia:
          RatiosTTM.Company.Solvencia <- RatiosTTM %>% filter(., Name %in% c("debtRatioTTM", "debtEquityRatioTTM", "totalDebtToCapitalizationTTM",
                                                                             "interestCoverageTTM", "cashFlowToDebtRatioTTM"))
          # Recogida Ratios.Rentabilidad:
          RatiosTTM.Company.Rentabilidad <- RatiosTTM %>% filter(., Name %in% c("returnOnAssetsTTM", "returnOnEquityTTM", "returnOnCapitalEmployedTTM",
                                                                                "grossProfitMarginTTM", "operatingProfitMarginTTM", "netProfitMarginTTM"))
          RatiosTTM.Company.Rentabilidad <- RatiosTTM.Company.Rentabilidad %>% mutate(Value = Value * 100)
          
          # Recogida Ratios.Liquidez:
          RatiosTTM.Company.Liquidez <- RatiosTTM %>% filter(., Name %in% c("currentRatioTTM", "quickRatioTTM", "cashRatioTTM"))
          
          # Recogida Ratios.Eficiencia:
          RatiosTTM.Company.Eficiencia <- RatiosTTM %>% filter(., Name %in% c("inventoryTurnoverTTM", "assetTurnoverTTM", "receivablesTurnoverTTM"))
          
          # Ratios Comparativa:  
          
          # Extraemos los RatiosTTM de todas las empresas: 
          SP.RatiosTTM <- read.csv("https://financialmodelingprep.com/api/v4/ratios-ttm-bulk?apikey=d2855b21641f2dff7fbee3b4ba44f47c")
          
          # Unimos ambas tablas 
          SP.RatiosTTM <- merge(NYSE.list, SP.RatiosTTM, by = "symbol", all.x = FALSE) # Sólo se unen los coincidentes symbol a la tabla NYSE.List
          
          SP.RatiosTTM <- SP.RatiosTTM %>% mutate(across(6:(ncol(.)), ~ round(., 2)))
          
          SP.RatiosTTM.Sector <- SP.RatiosTTM %>% filter(., sector == metric.sector2 & SP500 == TRUE) 
          SP.RatiosTTM.Industry <- SP.RatiosTTM %>% filter(., industry == metric.industry2 & SP500 == TRUE)
        }
        # 2. RatiosTTM.Sector:
        {
          # Recogida Ratios.Sector.Valuation:
            RatiosTTM.Sector.Valuation <- SP.RatiosTTM.Sector %>%
              summarise(across(c("priceEarningsRatioTTM", "priceEarningsToGrowthRatioTTM", "priceToBookRatioTTM", "priceToSalesRatioTTM", "priceToOperatingCashFlowsRatioTTM"), 
                               ~ {
                                 q <- quantile(., c(0.25, 0.75), na.rm = TRUE)
                                 iqr <- q[2] - q[1]
                                 lower_bound <- q[1] - 1.5 * iqr
                                 upper_bound <- q[2] + 1.5 * iqr
                                 filtered_values <- .[between(., lower_bound, upper_bound) | is.na(.)]
                                 mean(filtered_values, na.rm = TRUE)
                               })) %>% t()

          
          RatiosTTM.Sector.Valuation <- data.frame(Name = rownames(RatiosTTM.Sector.Valuation), Value = round(RatiosTTM.Sector.Valuation, 2))
          row.names(RatiosTTM.Sector.Valuation) <- NULL 
          # View(RatiosTTM.Sector.Valuation)
          
          # Recogida Ratios.Sector.Solvencia:
          RatiosTTM.Sector.Solvencia <- SP.RatiosTTM.Sector %>%
            summarise(across(c("debtRatioTTM", "debtEquityRatioTTM", "totalDebtToCapitalizationTTM", "interestCoverageTTM", "cashFlowToDebtRatioTTM"), 
                             ~ {
                               q <- quantile(., c(0.25, 0.75), na.rm = TRUE)
                               iqr <- q[2] - q[1]
                               lower_bound <- q[1] - 1.5 * iqr
                               upper_bound <- q[2] + 1.5 * iqr
                               filtered_values <- .[between(., lower_bound, upper_bound) | is.na(.)]
                               mean(filtered_values, na.rm = TRUE)
                             })) %>% t()
          
          RatiosTTM.Sector.Solvencia <- data.frame(Name = rownames(RatiosTTM.Sector.Solvencia), Value = round(RatiosTTM.Sector.Solvencia, 2))
          row.names(RatiosTTM.Sector.Solvencia) <- NULL 
          # View(RatiosTTM.Sector.Solvencia)
          
          # Recogida Ratios.Sector.Rentabilidad:
          RatiosTTM.Sector.Rentabilidad <- SP.RatiosTTM.Sector %>%
            summarise(across(c("returnOnAssetsTTM", "returnOnEquityTTM", "returnOnCapitalEmployedTTM","grossProfitMarginTTM", "operatingProfitMarginTTM", "netProfitMarginTTM"), 
                             ~ {
                               q <- quantile(., c(0.25, 0.75), na.rm = TRUE)
                               iqr <- q[2] - q[1]
                               lower_bound <- q[1] - 1.5 * iqr
                               upper_bound <- q[2] + 1.5 * iqr
                               filtered_values <- .[between(., lower_bound, upper_bound) | is.na(.)]
                               mean(filtered_values, na.rm = TRUE)
                             })) %>% t()
          
          RatiosTTM.Sector.Rentabilidad <- data.frame(Name = rownames(RatiosTTM.Sector.Rentabilidad), Value = round(RatiosTTM.Sector.Rentabilidad * 100, 2))
          row.names(RatiosTTM.Sector.Rentabilidad) <- NULL 
          # View(RatiosTTM.Sector.Rentabilidad)   
          
          # Recogida Ratios.Sector.Liquidez:
          RatiosTTM.Sector.Liquidez <- SP.RatiosTTM.Sector %>%
            summarise(across(c("currentRatioTTM", "quickRatioTTM", "cashRatioTTM"), 
                             ~ {
                               q <- quantile(., c(0.25, 0.75), na.rm = TRUE)
                               iqr <- q[2] - q[1]
                               lower_bound <- q[1] - 1.5 * iqr
                               upper_bound <- q[2] + 1.5 * iqr
                               filtered_values <- .[between(., lower_bound, upper_bound) | is.na(.)]
                               mean(filtered_values, na.rm = TRUE)
                             })) %>% t()
          
          RatiosTTM.Sector.Liquidez <- data.frame(Name = rownames(RatiosTTM.Sector.Liquidez), Value = round(RatiosTTM.Sector.Liquidez, 2))
          row.names(RatiosTTM.Sector.Liquidez) <- NULL 
          # View(RatiosTTM.Sector.Liquidez)   
          
          # Recogida Ratios.Sector.Eficiencia:
          RatiosTTM.Sector.Eficiencia <- SP.RatiosTTM.Sector %>%
            summarise(across(c("inventoryTurnoverTTM", "assetTurnoverTTM", "receivablesTurnoverTTM"), 
                             ~ {
                               q <- quantile(., c(0.25, 0.75), na.rm = TRUE)
                               iqr <- q[2] - q[1]
                               lower_bound <- q[1] - 1.5 * iqr
                               upper_bound <- q[2] + 1.5 * iqr
                               filtered_values <- .[between(., lower_bound, upper_bound) | is.na(.)]
                               mean(filtered_values, na.rm = TRUE)
                             })) %>% t()
          
          RatiosTTM.Sector.Eficiencia <- data.frame(Name = rownames(RatiosTTM.Sector.Eficiencia), Value = round(RatiosTTM.Sector.Eficiencia, 2))
          row.names(RatiosTTM.Sector.Eficiencia) <- NULL 
          # View(RatiosTTM.Sector.Eficiencia)   
        } 
        # 3. RatiosTTM.Industry:
        {
          # Recogida Ratios.Industry.Valuation:
          RatiosTTM.Industry.Valuation <- SP.RatiosTTM.Industry %>%
            summarise(across(c("priceEarningsRatioTTM", "priceEarningsToGrowthRatioTTM", "priceToBookRatioTTM", "priceToSalesRatioTTM", "priceToOperatingCashFlowsRatioTTM"), 
                             ~ {
                               q <- quantile(., c(0.25, 0.75), na.rm = TRUE)
                               iqr <- q[2] - q[1]
                               lower_bound <- q[1] - 1.5 * iqr
                               upper_bound <- q[2] + 1.5 * iqr
                               filtered_values <- .[between(., lower_bound, upper_bound) | is.na(.)]
                               mean(filtered_values, na.rm = TRUE)
                             })) %>% t()
          
          RatiosTTM.Industry.Valuation <- data.frame(Name = rownames(RatiosTTM.Industry.Valuation), Value = round(RatiosTTM.Industry.Valuation, 2))
          row.names(RatiosTTM.Industry.Valuation) <- NULL 
          # View(RatiosTTM.Industry.Valuation)
          
          # Recogida Ratios.Industry.Solvencia:
          RatiosTTM.Industry.Solvencia <- SP.RatiosTTM.Industry %>%
            summarise(across(c("debtRatioTTM", "debtEquityRatioTTM", "totalDebtToCapitalizationTTM", "interestCoverageTTM", "cashFlowToDebtRatioTTM"), 
                             ~ {
                               q <- quantile(., c(0.25, 0.75), na.rm = TRUE)
                               iqr <- q[2] - q[1]
                               lower_bound <- q[1] - 1.5 * iqr
                               upper_bound <- q[2] + 1.5 * iqr
                               filtered_values <- .[between(., lower_bound, upper_bound) | is.na(.)]
                               mean(filtered_values, na.rm = TRUE)
                             })) %>% t()
          
          RatiosTTM.Industry.Solvencia <- data.frame(Name = rownames(RatiosTTM.Industry.Solvencia), Value = round(RatiosTTM.Industry.Solvencia, 2))
          row.names(RatiosTTM.Industry.Solvencia) <- NULL 
          # View(RatiosTTM.Industry.Solvencia)
          
          # Recogida Ratios.Industry.Rentabilidad:
          RatiosTTM.Industry.Rentabilidad <- SP.RatiosTTM.Industry %>%
            summarise(across(c("returnOnAssetsTTM", "returnOnEquityTTM", "returnOnCapitalEmployedTTM",
                               "grossProfitMarginTTM", "operatingProfitMarginTTM", "netProfitMarginTTM"), 
                             ~ {
                               q <- quantile(., c(0.25, 0.75), na.rm = TRUE)
                               iqr <- q[2] - q[1]
                               lower_bound <- q[1] - 1.5 * iqr
                               upper_bound <- q[2] + 1.5 * iqr
                               filtered_values <- .[between(., lower_bound, upper_bound) | is.na(.)]
                               mean(filtered_values, na.rm = TRUE)
                             })) %>% t()
          
          RatiosTTM.Industry.Rentabilidad <- data.frame(Name = rownames(RatiosTTM.Industry.Rentabilidad), Value = round(RatiosTTM.Industry.Rentabilidad * 100, 2))
          row.names(RatiosTTM.Industry.Rentabilidad) <- NULL 
          # View(RatiosTTM.Industry.Rentabilidad)   
          
          # Recogida Ratios.Industry.Liquidez:
          RatiosTTM.Industry.Liquidez <- SP.RatiosTTM.Industry %>%
            summarise(across(c("currentRatioTTM", "quickRatioTTM", "cashRatioTTM"), 
                             ~ {
                               q <- quantile(., c(0.25, 0.75), na.rm = TRUE)
                               iqr <- q[2] - q[1]
                               lower_bound <- q[1] - 1.5 * iqr
                               upper_bound <- q[2] + 1.5 * iqr
                               filtered_values <- .[between(., lower_bound, upper_bound) | is.na(.)]
                               mean(filtered_values, na.rm = TRUE)
                             })) %>% t()
          
          RatiosTTM.Industry.Liquidez <- data.frame(Name = rownames(RatiosTTM.Industry.Liquidez), Value = round(RatiosTTM.Industry.Liquidez, 2))
          row.names(RatiosTTM.Industry.Liquidez) <- NULL 
          # View(RatiosTTM.Industry.Liquidez)   
          
          # Recogida Ratios.Setor.Eficiencia:
          RatiosTTM.Industry.Eficiencia <- SP.RatiosTTM.Industry %>%
            summarise(across(c("inventoryTurnoverTTM", "assetTurnoverTTM", "receivablesTurnoverTTM"), 
                             ~ {
                               q <- quantile(., c(0.25, 0.75), na.rm = TRUE)
                               iqr <- q[2] - q[1]
                               lower_bound <- q[1] - 1.5 * iqr
                               upper_bound <- q[2] + 1.5 * iqr
                               filtered_values <- .[between(., lower_bound, upper_bound) | is.na(.)]
                               mean(filtered_values, na.rm = TRUE)
                             })) %>% t()
          
          RatiosTTM.Industry.Eficiencia <- data.frame(Name = rownames(RatiosTTM.Industry.Eficiencia), Value = round(RatiosTTM.Industry.Eficiencia, 2))
          row.names(RatiosTTM.Industry.Eficiencia) <- NULL 
          # View(RatiosTTM.Industry.Eficiencia)
        }
      }
    }
    # 3.3. Union RatiosTTM C.I.S
    {
      # 3.3.1. Unir RatiosTTM Comoany, Industry y Sector:
      
      # RatiosTTM Valuation:
      RatiosTTM.Valuation <- RatiosTTM.Company.Valuation %>% merge(RatiosTTM.Industry.Valuation, by = "Name") %>%
        merge(RatiosTTM.Sector.Valuation, by = "Name") %>% 
        rename(Company = Value.x, Industry = Value.y, Sector = Value)
      
      # RatiosTTM Solvencia:
      RatiosTTM.Solvencia <- RatiosTTM.Company.Solvencia %>% merge(RatiosTTM.Industry.Solvencia, by = "Name") %>%
        merge(RatiosTTM.Sector.Solvencia, by = "Name") %>% 
        rename(Company = Value.x, Industry = Value.y, Sector = Value)
      # RatiosTTM Rentabilidad:
      RatiosTTM.Rentabilidad <- RatiosTTM.Company.Rentabilidad %>% merge(RatiosTTM.Industry.Rentabilidad, by = "Name") %>%
        merge(RatiosTTM.Sector.Rentabilidad, by = "Name") %>% 
        rename(Company = Value.x, Industry = Value.y, Sector = Value)
      
      # RatiosTTM Liquidez:
      RatiosTTM.Liquidez <- RatiosTTM.Company.Liquidez %>% merge(RatiosTTM.Industry.Liquidez, by = "Name") %>%
        merge(RatiosTTM.Sector.Liquidez, by = "Name") %>% 
        rename(Company = Value.x, Industry = Value.y, Sector = Value)
      
      # RatiosTTM Eficiencia:
      RatiosTTM.Eficiencia <- RatiosTTM.Company.Eficiencia %>% merge(RatiosTTM.Industry.Eficiencia, by = "Name") %>%
        merge(RatiosTTM.Sector.Eficiencia, by = "Name") %>% 
        rename(Company = Value.x, Industry = Value.y, Sector = Value)
      
      # 3.3.2. Cambiar nombre de ratios indénticos a Ratios.Annual:
      # RatiosTTM.Valuation:
      {
        # Seleccion Name de Ratios
        ratios_name <- c(
          "priceEarningsRatioTTM" = "P/E",
          "priceEarningsToGrowthRatioTTM" = "PEG",
          "priceToBookRatioTTM" = "P/B",
          "priceToSalesRatioTTM" = "P/S",
          "priceToOperatingCashFlowsRatioTTM" = "P/CF"
        )
        
        # Cambiar el nombre de la columna 'Name' a 'Ratio'
        RatiosTTM.Valuation <- RatiosTTM.Valuation %>% rename(Ratio = Name)
        
        # Actualizar los nombres de la columna 'Ratio' según nombre
        RatiosTTM.Valuation$Ratio <- ratios_name[RatiosTTM.Valuation$Ratio]
        
        # print(RatiosTTM.Valuation) 
        }
      # RatiosTTM.Solvencia:
      {
        # Seleccion Name de Ratios
        ratios_name <- c(
          "debtRatioTTM" = "Debt Ratio",             
          "debtEquityRatioTTM" =  "Debt To Equity",         
          "totalDebtToCapitalizationTTM" = "Debt To Capital",
          "interestCoverageTTM" = "Interest Coverage", 
          "cashFlowToDebtRatioTTM" = "CashFlow to Debt"
        )
        
        # Cambiar el nombre de la columna 'Name' a 'Ratio'
        RatiosTTM.Solvencia <- RatiosTTM.Solvencia %>% rename(Ratio = Name)
        
        # Actualizar los nombres de la columna 'Ratio' según nombre
        RatiosTTM.Solvencia$Ratio <- ratios_name[RatiosTTM.Solvencia$Ratio]
        
        # print(RatiosTTM.Solvencia)
      }
      # RatiosTTM.Rentabilidad:
      {
        # Seleccion Name de Ratios
        ratios_name <- c(
          "grossProfitMarginTTM" = "Gross Margin",   
          "operatingProfitMarginTTM" = "Operating Margin",
          "netProfitMarginTTM" = "Net Margin",        
          "returnOnAssetsTTM" = "ROA",         
          "returnOnEquityTTM" = "ROE",         
          "returnOnCapitalEmployedTTM" = "ROCE"
        )
        
        # Cambiar el nombre de la columna 'Name' a 'Ratio'
        RatiosTTM.Rentabilidad <- RatiosTTM.Rentabilidad %>% rename(Ratio = Name)
        
        # Actualizar los nombres de la columna 'Ratio' según nombre
        RatiosTTM.Rentabilidad$Ratio <- ratios_name[RatiosTTM.Rentabilidad$Ratio]
        
        # print(RatiosTTM.Rentabilidad)
      }
      # RatiosTTM.Liquidez:
      {
        # Seleccion Name de Ratios
        ratios_name <- c(
          "currentRatioTTM" = "Current Ratio",         
          "quickRatioTTM" = "Quick Turnover",
          "cashRatioTTM" = "Cash Ratio"  
        )
        
        # Cambiar el nombre de la columna 'Name' a 'Ratio'
        RatiosTTM.Liquidez <- RatiosTTM.Liquidez %>% rename(Ratio = Name)
        
        # Actualizar los nombres de la columna 'Ratio' según nombre
        RatiosTTM.Liquidez$Ratio <- ratios_name[RatiosTTM.Liquidez$Ratio]
        
        # print(RatiosTTM.Liquidez)
      }
      # RatiosTTM.Eficiencia:
      {
        # Seleccion Name de Ratios
        ratios_name <- c(
          "inventoryTurnoverTTM" = "Inventory Turnover",
          "assetTurnoverTTM" = "Asset Turnover",
          "receivablesTurnoverTTM" = "Receivables Turnover"
        )
        
        # Cambiar el nombre de la columna 'Name' a 'Ratio'
        RatiosTTM.Eficiencia <- RatiosTTM.Eficiencia %>% rename(Ratio = Name)
        
        # Actualizar los nombres de la columna 'Ratio' según nombre
        RatiosTTM.Eficiencia$Ratio <- ratios_name[RatiosTTM.Eficiencia$Ratio]
        
        # print(RatiosTTM.Eficiencia)
      }
    }
    # 3.4. Union Anual & RatiosTTM C.I.S + Final.Ratios
    {
      # I. Unimos Ratios.Annual con RatiosTTM para tener una tabla comparativa completa: 
      
        # Final Ratios Valuation:
          Final.Ratios.Valuation <- merge(Ratios.Annual.Valuation, RatiosTTM.Valuation, by = "Ratio") 
          # print(Final.Ratios.Valuation)
      
        # Final Ratios Solvencia:
          Final.Ratios.Solvencia <- merge(Ratios.Annual.Solvencia, RatiosTTM.Solvencia, by = "Ratio") 
          # print(Final.Ratios.Solvencia)
          
        # Final Ratios Rentabilidad:
          Final.Ratios.Rentabilidad <- merge(Ratios.Annual.Rentabilidad, RatiosTTM.Rentabilidad, by = "Ratio") 
          # print(Final.Ratios.Rentabilidad)
          
        # Final Ratios Liquidez:
          Final.Ratios.Liquidez <- merge(Ratios.Annual.Liquidez, RatiosTTM.Liquidez, by = "Ratio") 
          # print(Final.Ratios.Liquidez)
          
        # Final Ratios Eficiencia:
          Final.Ratios.Eficiencia <- merge(Ratios.Annual.Eficiencia, RatiosTTM.Eficiencia, by = "Ratio") 
          # print(Final.Ratios.Eficiencia)
      
      # II. Unimos todos los df para posterior visualización por grupos de ratios:
      
        # Añadir Group: 
          Final.Ratios.Valuation$Grupo <- "Valuation"
          Final.Ratios.Solvencia$Grupo <- "Solvencia"
          Final.Ratios.Rentabilidad$Grupo <- "Rentabilidad"
          Final.Ratios.Liquidez$Grupo <- "Liquidez"
          Final.Ratios.Eficiencia$Grupo <- "Eficiencia"
      
        # Unimos y creamos una tabla full ratios:
          Final.Ratios.Full <- rbind(Final.Ratios.Valuation, Final.Ratios.Solvencia, Final.Ratios.Rentabilidad,
                                     Final.Ratios.Liquidez, Final.Ratios.Eficiencia)
      
          Final.Ratios.Full <- Final.Ratios.Full %>% arrange(Grupo, Ratio)
          # print(Final.Ratios.Full)

    }
  }
  # 4. Metrics: 
  {
    # I. Metrics Annual: 
    {  
      # Metrics Company:
      Metrics.Annual <-  paste0("https://financialmodelingprep.com/api/v3/key-metrics/", Ticker, "?period=annual&apikey=", API_KEY) #Extraemos algunas métricas. Resto más abajo 1.5
      
        response <- httr::GET(Metrics.Annual)
        Metrics.Annual <- jsonlite::fromJSON(content(response, "text")) 
      
      # Ponemos date como encabezado  
      Metrics.Annual <- as.data.frame(t(Metrics.Annual)) %>% select(1:min(5, ncol(.)))
      nombres <- as.character(unlist(Metrics.Annual[3, ]))
      names(Metrics.Annual) <- nombres
      
      # Ponemos el indice como variable Name:
      Metrics.Annual <- Metrics.Annual[-c(1:4),]
      Metrics.Annual <- Metrics.Annual %>% as.data.frame() %>% mutate(across(everything(), as.numeric)) %>% round(2)
      Metrics.Annual <- rownames_to_column(Metrics.Annual, var = "Name")
      # View(Metrics.Annual)
      
      # Agrupamos los Metrics por grupos:
      # Recogida Metrics.Valuation:
      Metrics.Annual.Valuation <- Metrics.Annual %>% filter(., Name %in% c("marketCap", "enterpriseValue", "enterpriseValueOverEBITDA",
                                                                           "evToFreeCashFlow", "earningsYield", "roic"))
      
      BillorMillion <- if (abs(as.numeric(Metrics.Annual.Valuation[1,2])) / 1e9 > 1){BillorMillion <- 1e9 # Es una buena práctica para entender si los datos son $Bn o $Mll
      } else {BillorMillion <- 1e6}
      
      Metrics.Annual.Valuation <- Metrics.Annual.Valuation %>%
                                    mutate(across(where(is.numeric), 
                                          ~ ifelse(Name %in% c("marketCap", "enterpriseValue"), round(./BillorMillion, 2), .))) %>%
                                    select(Name, rev(names(.)[-1]))
      
      if (BillorMillion == 1e6) {
        BillorMillion.Metrics <- "MarketCap & enterpriseValue in Millions $"
      } else if (BillorMillion == 1e9) {
        BillorMillion.Metrics <- "MarketCap & enterpriseValue in Billions $"
        }
      
    } 
    # II. Cambiamos el nombre de los Metrics para una mejor Apariencia:
    {
      # Metrics.Valuation:
      {
        # Seleccion Name de Metrics
        Metrics_name <- c(
          "marketCap" = "MarketCap",                
          "enterpriseValue" = "Enterprise Value",          
          "enterpriseValueOverEBITDA" = "EV/EBITDA",
          "evToFreeCashFlow" = "EV/FCF",         
          "earningsYield" = "Earnings Yield",            
          "roic" = "ROIC" 
        )
        
        # Cambiar el nombre de la columna 'Name' a 'Ratio'
        Metrics.Annual.Valuation <- Metrics.Annual.Valuation %>% rename(Metrics = Name)
        
        # Actualizar los nombres de la columna 'Ratio' según nombre
        Metrics.Annual.Valuation$Metrics <- Metrics_name[Metrics.Annual.Valuation$Metrics]
      
      # print(Metrics.Annual.Valuation)
    }
}
  }
  # 5. Dividends:
  {  
  # I. Extracción Historical Dividend Data:
    Historical.Dividend <- paste0("https://financialmodelingprep.com/api/v3/historical-price-full/stock_dividend/", Ticker, "?limit=40&apikey=", API_KEY)    
  
      response <- httr::GET(Historical.Dividend)
      Historical.Dividend <- jsonlite::fromJSON(content(response, "text"))
  
    Historical.Dividend <- Historical.Dividend$historical
  
  # II. Dividend Metrics: "Yield, Payout, Growh, LastDividend, LastDividendDate" 
  
  # 1. Dividend Yield: 
    DividendYield <- Overview$ratios$dividendYielTTM
    DividendYield <- as.numeric(DividendYield)
  
    if (!is.numeric(DividendYield)) {
      DividendYield <- as.numeric(as.character(DividendYield))
    }
    
    DividendYield <- round(DividendYield * 100, 2)
  
    # 2.Payout:
      DividendPayout <- Overview$ratios$payoutRatioTTM
      DividendPayout <- as.numeric(DividendPayout)
  
    if (!is.numeric(DividendPayout)) {
      DividendPayout <- as.numeric(as.character(payoutRatioTTM))
    }
  
    DividendPayout <- round(DividendPayout * 100, 2)
  
    # 3. Growth Dividend:
      # Dividend.Growth <- Growth.Company %>% filter(Growth == "dividendsperShareGrowth") %>% select(G.Value)
    
    # 4. Last Dividend:
    if (exists("Historical.Dividend") && !is.null(Historical.Dividend) && length(Historical.Dividend$historical.date) > 0) {
      
      Last.Dividend <- Historical.Dividend[1,3]
      Last.Dividend.Date <- Historical.Dividend[1,1]
  
    # 5. Tabla Final Dividend: 
      Historical.Dividend <- Historical.Dividend %>%
        mutate(Year = format(as.Date(date), "%Y")) %>%
        group_by(Year) %>%
        summarise(TotalDividend = sum(dividend, na.rm = TRUE))%>%
        arrange(desc(Year))
    }
}

# II. Tablas y Gráficos: 

  # 1. Tablas:
  {
    # 1. Segments:
    {
    # 1.1. Segment.Revenue.Table: 

      Segment.Revenue.Table.Kble <- Segment.Revenue.Table %>%
                                    kbl(booktabs = TRUE, format = "latex") %>%
                                    kable_styling(latex_options = c("striped", "hold_position"), full_width = T) %>%
                                    column_spec(1, width = "8cm")    

    # 1.2. Segment.Geo.Table:

      Segment.Geo.Table.Kble <- Segment.Geo.Table %>%
                                  kbl(booktabs = TRUE, format = "latex") %>%
                                  kable_styling(latex_options = c("striped", "hold_position"), full_width = T) %>%
                                  column_spec(1, width = "8cm")
  }
    # 2. Financial Statement: 
    {
    # 2.1. Income.Anual.Table:

      Income.Anual.Table.Kble <- Income.Anual.Table %>%
                                  kbl(booktabs = TRUE, format = "latex") %>%
                                  kable_styling(latex_options = c("striped", "hold_position"), full_width = T) %>% 
                                  column_spec(1, width = "5cm")

  
    # 2.2. Balance.Anual.Table:

      Balance.Anual.Table.Kble <- Balance.Anual.Table %>%
                                    kbl(booktabs = TRUE, format = "latex") %>%
                                    kable_styling(latex_options = c("striped", "hold_position"), full_width = T) %>% 
                                    column_spec(1, width = "5cm")

    # 2.3. Cash.Anual.Table:

      Cash.Anual.Table.Kble <- Cash.Anual.Table %>%
                                kbl(booktabs = TRUE, format = "latex") %>%
                                kable_styling(latex_options = c("striped", "hold_position"), full_width = TRUE) %>%
                                column_spec(1, width = "8cm")
  }
    # 3. Ratios y Metricas:
    {
    # 3.1. Ratios: 
    
      Final.Ratios.Full.Kbl <- Final.Ratios.Full %>%
                                select(-Grupo) %>%  
                                kable("latex", booktabs = TRUE) %>%
                                kable_styling(latex_options = c("striped", "scale_down")) %>%
                                group_rows("Valuation", 16, 20) %>%
                                group_rows("Solvencia", 12, 15) %>% 
                                group_rows("Rentabilidad", 6, 11) %>%
                                group_rows("Liquidez", 1, 4) %>% 
                                group_rows("Eficiencia", 5, 5) %>%
                                row_spec(seq(2, nrow(Final.Ratios.Full)), extra_css = "height: 30px;")
    
    # 3.2. Metricas:
      
      Metrics.Annual.Valuation <- Metrics.Annual.Valuation %>%
                                        kbl(booktabs = TRUE, format = "latex") %>%
                                        kable_styling(latex_options = c("striped", "hold_position"), full_width = TRUE) %>%
                                        column_spec(1, width = "8cm")
      
    }
    # 4. Dividendos: 
    {
      if (length(Historical.Dividend) > 0) {
        Historical.Dividend.Kble <- Historical.Dividend %>%
              kbl(booktabs = TRUE, format = "latex") %>%
              kable_styling(latex_options = c("striped", "hold_position"), full_width = TRUE) %>%
              column_spec(1, width = "8cm")
      }
    }
  }
  # 2.Gráficos:
  {
    # 1. Segments:
    {
    # 1.1. Segment.Revenue.Table: 
  
      # print(Segment.Revenue.Table)
      Segment.Revenue.Chart <- gather(Segment.Revenue.Table, Year, Revenue, -Name)
  
      Segment.Revenue.Chart <- ggplot(data = Segment.Revenue.Chart, aes(x = Year, y = Revenue, fill = Name)) +
                                  geom_col(color = "black") +  
                                  theme_minimal() + theme(legend.position = "bottom")
  

    # 1.2. Segment.Geo.Table:
    
      # print(Segment.Geo.Table)
      Segment.Geo.Chart <- gather(Segment.Geo.Table, Year, Revenue, -Name)
    
      Segment.Geo.Chart <- ggplot(data = Segment.Geo.Chart, aes(x = Year, y = Revenue, fill = Name)) +
                              geom_col(color = "black") + 
                              theme_minimal() + theme(legend.position = "bottom")
  }
    # 2. Financial Statement: 
    {
    # 2.1. Income.Anual.Table:
  
    # print(Income.Anual.Table)
    Income.Anual.Table.Chart <- gather(Income.Anual.Table, Year, Value, -Name) %>% 
      filter(Name %in% c("Revenue", "Ebitda", "EPS")) 
  
    Income.Anual.Table.Chart <- ggplot(Income.Anual.Table.Chart, aes(x = factor(Year), y = Value, fill = Name)) +
                                  geom_col(position = "dodge") +
                                  labs(title = "Ingresos anuales por categoría", x = "Año", y = "Valor") +
                                  theme_minimal()
   
    # 2.2. Balance.Anual.Table:
    
      # print(Balance.Anual.Table)
  
      # Assets Short Term:
        Balance.Anual.Table.Chart.Short <- gather(Balance.Anual.Table, Year, Value, -Name) %>% 
          filter(Name %in% c("Current Assets", "Current Liabilities")) %>% 
          filter(Year == max(Year))
  
        Balance.Anual.Table.Chart.Short <- ggplot(Balance.Anual.Table.Chart.Short, aes(x = factor(Year), y = Value, fill = Name)) +
                                              geom_col(position = "dodge") +
                                              labs(title = "Short Term Assets vs Liabilities:", x = "Year", y = "Value") +
                                              theme_minimal() + theme(legend.position = "bottom")
  
      # Total Assets:
        Balance.Anual.Table.Chart.Total <- gather(Balance.Anual.Table, Year, Value, -Name) %>% 
                                            filter(Name %in% c("Total Assets", "Total Liabilities", "Total Equity")) %>% 
                                            filter(Year == max(Year))
  
        Balance.Anual.Table.Chart.Total <- ggplot(Balance.Anual.Table.Chart.Total, aes(x = "", y = Value, fill = Name)) +
                                            geom_bar(stat = "identity", width = 1, color = "white") +
                                            coord_polar("y") +
                                            labs(title = "Balance Summary:", fill = "") +
                                            theme_void() +
                                            theme(legend.position = "bottom") +
                                            scale_fill_brewer(palette = "Set3")
    
  # 2.3. Cash.Anual.Table:

    # print(Cash.Anual.Table)
  
    Cash.Anual.Table.Chart <- gather(Cash.Anual.Table, Year, Value, -Name) %>% 
      filter(Name %in% c("Operating Cash Flow", "Investing Cash Flow",
                         "Financing Cash Flow","Net Cash Flow", "Free Cash Flow"))
    
    my_colors <- c("#A9A9A9", "#75C7FF", "#3498db", "#1F618D", "#2E86C1")
    
    Cash.Anual.Table.Chart <- ggplot(Cash.Anual.Table.Chart, aes(x = factor(Year), y = Value, fill = Name)) +
                                geom_col(position = "stack") +
                                labs(title = "Evol. Cashflow", x = "Año", y = "Valor") +
                                scale_fill_manual(values = my_colors) +
                                theme_minimal() +
                                theme(legend.position = "bottom")
  } 
    # 3. Ratios
    {
    # 1. Transformación:
    {
      Final.Ratios.Full.Chart <- Final.Ratios.Full %>%
        mutate(
          Company = ifelse(Ratio %in% c("Current Ratio", "Cash Ratio"), -1 * Company, Company),
          Industry = ifelse(Ratio %in% c("Current Ratio", "Cash Ratio"), -1 * Industry, Industry),
          Sector = ifelse(Ratio %in% c("Current Ratio", "Cash Ratio"), -1 * Sector, Sector)
        )
      
      # Crear el data frame con las columnas seleccionadas
      Final.Ratios.Full.Chart <- Final.Ratios.Full %>%
        select(Ratio, Company, Industry, Sector, Grupo)
      # Seleccionar solo las columnas numéricas para normalizar
      data_numeric <- Final.Ratios.Full.Chart[, c("Company", "Industry", "Sector")]
      
      # Normalizar los datos de cada fila entre 0 y 5
      for (i in 1:nrow(Final.Ratios.Full.Chart)) {
        row_min <- min(data_numeric[i, ])
        row_max <- max(data_numeric[i, ])
        Final.Ratios.Full[i, c("Company", "Industry", "Sector")] <- (data_numeric[i, ] - row_min) / (row_max - row_min) * 5
      }
      
      # Mostrar el dataframe con los datos normalizados
      # View(Final.Ratios.Full)
      
      resumen <- Final.Ratios.Full.Chart %>%
        group_by(Grupo) %>%
        summarize(
          Company = mean(Company, na.rm = TRUE),
          Industry = mean(Industry, na.rm = TRUE),
          Sector = mean(Sector, na.rm = TRUE)
        )
      
      # Mostrar el resumen
        resumen <- resumen %>% mutate_if(is.numeric, ~ round(., 2))
        resumen <- as.data.frame(resumen)
      
      
      # Usar el dataframe 'resumen' para inicializar 'datos_transformados' y reemplazar valores
        datos_transformados <- data.frame(
          Grupo = resumen$Grupo,
          Company = resumen$Company,
          Industry = resumen$Industry,
          Sector = resumen$Sector
          )
      
      # Cambiar los nombres de las filas
        rownames(datos_transformados) <- c("Max", "Min", "Company", "Industry", "Sector")
      
      # Mostrar los datos transformados
        # print(datos_transformados)
      # Crear un dataframe vacío para almacenar los datos transformados
        datos_transformados <- matrix(0, nrow = 5, ncol = nrow(resumen))
      
      # Configurar la primera fila con el valor máximo (5)
        datos_transformados[1, ] <- 5
      
      # Configurar la segunda fila con el valor mínimo (0)
        datos_transformados[2, ] <- 0
      
      # Configurar la tercera fila con los datos de Company
        datos_transformados[3, 1:5] <- resumen$Company
      
      # Configurar la cuarta fila con los datos de Industry
        datos_transformados[4, 1:5] <- resumen$Industry
      
      # Configurar la quinta fila con los datos de Sector
        datos_transformados[5, 1:5] <- resumen$Sector
      
      # Cambiar los nombres de las columnas y filas
        colnames(datos_transformados) <- resumen$Grupo
        rownames(datos_transformados) <- c("Max", "Min", "Company", "Industry", "Sector")
      
      # Mostrar los datos transformados
        # print(datos_transformados)
        datos_transformados <- as.data.frame(datos_transformados) %>% round(2)
    }
    # 2. Visualización: # Radial C.I.S
    {
      # No se puede almacenar en una variable por lo que se tendrá que pegar donde se desee insertar el código entero:
      {
      # radarchart(datos_transformados,
      #           cglty = 2,        # Tipo de línea del grid
      #           cglcol = "gray",  # Color del grid
      #           pcol = c(4,8,1),  # Color para cada línea
      #           plwd = 2,         # Ancho de linea
      #           plty = 1)         # Tipos de línea 
    
      # legend("bottomleft", legend = c("Company", "Industry", "Sector"), col = c(4, 8, 1), pch = 15, bty = "n")
      }
    }
}
    # 4. Dividends:
    {
      if (exists("Historical.Dividend") && !is.null(Historical.Dividend) && length(Historical.Dividend$historical.date) > 0) {
        # 1. Transformación: 
        { 
    # print(Historical.Dividend)
    # Convierte la columna 'historical.date' a tipo fecha si no lo está
    Historical.Dividend$historical.date <- as.Date(Historical.Dividend$historical.date)
    
    # Crea una columna nueva 'Mes' que contenga el mes extraído de la fecha
    Historical.Dividend$Mes <- format(Historical.Dividend$historical.date, "%m")
    
    # Filtra los datos para mostrar solo los dividendos en meses que sean múltiplos de 4
    dividendos_meses_4 <- subset(Historical.Dividend, as.numeric(Mes) %% 4 == 0)
  }
        # 2. Visualización:
        {
    Historical.Dividend <- ggplot(Historical.Dividend, aes(x = historical.date, y = historical.adjDividend)) +
                              geom_line(color = "#5E4FA2", size = 1.5) +
                              geom_point(color = "darkblue", size = 2.5, shape = 21, fill = "white") +
                              geom_text(data = dividendos_meses_4,
                                        aes(label = as.character(historical.adjDividend), x = historical.date, y = historical.adjDividend),
                                        hjust = -0.1, vjust = 1.5, size = 3.5) +
                              labs(
                                x = "Fecha",
                                y = "Dividendos",
                                title = "Evolución Histórica de Dividendos"
                              ) +
                              theme_minimal() +
                              theme(
                                plot.title = element_text(size = 20, face = "bold", margin = margin(b = 15)),
                                axis.text = element_text(size = 12, color = "#333333"),
                                axis.title = element_text(size = 14, face = "bold", color = "#333333"),
                                axis.line = element_line(color = "#333333"),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                panel.background = element_blank(),
                                plot.background = element_rect(fill = "#F8F8F8", color = NA),
                                legend.position = "none",
                                plot.margin = margin(b = 2)  # Añade margen en la parte inferior para las etiquetas
                              )
  }
      } else {print("No Data")}
    }
  }


