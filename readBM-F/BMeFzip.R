library(gdata)

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
