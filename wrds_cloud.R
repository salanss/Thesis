library(rJava)
library(RJDBC)

wrdsClient <- function(username = NULL, password = NULL) {
  sasPath <- getOption("wrds.sasPath")
  
  if (is.null(sasPath)) {
    stop("No SAS path set. Please set one with wrdsSASPath() and give it the full path to the SAS JAR installation.")
  }
  
  sasCore <- paste0(sasPath, "/sas.core.jar")
  sasDriver <- paste0(sasPath, "/sas.intrnet.javatools.jar")
  if (!file.exists(sasDriver)) {
    stop(paste("SAS driver", sasDriver, "not found!"))
  }
  
  .jaddClassPath(c(sasCore, sasDriver))
  
  driver <- RJDBC::JDBC("com.sas.net.sharenet.ShareNetDriver", sasDriver, identifier.quote="`")
  
  username <- ifelse(is.null(username), getOption("wrds.username"), username)
  password <- ifelse(is.null(password), getOption("wrds.password"), password)
  
  client <- RJDBC::dbConnect(driver, "jdbc:sharenet://wrds-cloud.wharton.upenn.edu:8551/", username, password)
  
  client
}

wrdsCredentials <- function(username, password) {
  options(wrds.username = username)
  options(wrds.password = password)
}

wrdsSASPath <- function(path) {
  if (!dir.exists(path)) {
    stop(paste("Could not find directory", path))
  }
  
  options(wrds.sasPath = path)
}

wrdsSASPath("C:/Users/anssi/Documents/SAS-JDBC-Drivers")

wrdsCredentials("anssi94", "VasicekMod3l9437+")

a <- wrdsClient()
