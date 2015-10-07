library(gdata)

################################################################################################################
readBMFzip <- function(zipfile) {
### Read the BM&FBovespa Historical data in the "information recovery" page: http://www2.bmf.com.br/Mais/Index.html?Idioma=pt-br
### Note the site must be in portuguese and the file is "Resumo estatítico do pregão"
### with all the fields available selected
### returns a data.frame  
  
  
  # Create a name for the dir where we'll unzip
  zipdir <- tempfile()
  # Create the dir using that name
  dir.create(zipdir)
  # Unzip the file into the dir
  unzip(zipfile, exdir=zipdir)
  # Get the files into the dir
  files <- list.files(zipdir)
  # Throw an error if there's more than one
  if(length(files)>1) stop("More than one data file inside zip")
  # Get the full name of the file
  file <- paste(zipdir, files[1], sep="/")
  # Read the file
  dados <- read.csv2(file, header = TRUE, sep = ";", quote = "\"",
                     dec = ",", fill = TRUE, skip = 0, nrows = -1, fileEncoding="UTF-16LE",
                     stringsAsFactors = FALSE)
  
  dados[,1]  <- as.Date(dados[,1], format = "%d/%m/%Y")
  dados[,15] <- as.Date(dados[,15], format = "%d/%m/%Y")
  dados[,16] <- as.Date(dados[,16], format = "%d/%m/%Y")
  
  dados <- dados[,1:(ncol(dados) - 1)]
  
  tipos       <- sapply(dados, class)
  flag.tipos  <- which(tipos == "character")
  
  for(k in flag.tipos){
    dados[, k] <- gdata::trim(dados[, k])
    dados[dados[,k] == "", k] <- NA  
  }
return(dados)
}

################################################################################################################
BMeFDadaCatch <- function(InputFile){
# Read the zip BM&F derivatives markets file from "information recovery" page: http://www2.bmf.com.br/Mais/Index.html?Idioma=pt-br
# the file is "resumo estatístico do pregão" with all fields selected.
# The function selects some fields and returns a data.frame.
# It uses the readBMFzip function to read the data.  
  
print(paste("Put to read", InputFile))
  
dados <- readBMFzip(zipfile = InputFile)

m <- as.numeric(format(dados[,16], "%m"))
y <- as.numeric(format(dados[,16], "%Y"))

dados <- transform(dados, "Month" = m, "Year" = y)

# Selected Commodities
mercadorias <- c("WIN", "DDI", "DOL", "EUR", "DI1", "IND",
                 "FRC", "WDO", "WEU", "DR1", "IR1")

# Referência do Resumo ("Reference")
ref.resumo <- "MERC"  

# Type of Market: Future Market
mercado    <- 2 

# Data Fields
campos <- c("Commodity"       = 30,
            "Date"            =  1,
            "Maturity Month"  = 59,
            "Maturity Year"   = 60,
            "Sequence"        = 54,
            "Serie"           = 32,
            "Maturity Date"   = 16,
            "Days"            = 18,
            "Business Days"   = 19,
            "Price-Open"      = 12,
            "Price-High"      = 25,
            "Price-Low"       = 28,
            "Price-Last"      = 13,
            "Price-Adjust"    = 11,
            "Price-Average"   = 14,
            "Open Interest"   =  6,
            "Open Interest (end)"   =  5,
            "Traded Contracts" = 7,
            "Trades"          = 31,
            "Contract Size"   = 48,
            "Volume BRL"      = 57,
            "Volume USD"      = 58)

filtro <- subset(dados, dados[,2]  == ref.resumo & 
                   dados[,29] == mercado & 
                   dados[,30] %in% mercadorias, select = campos)

names(filtro) <- names(campos)

return(filtro)
}

