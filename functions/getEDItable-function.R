get_edi_table <- function(identifier, entity_seq) {
  scope <- "knb-lter-hbr"
  
  # find the most recent revision number
  revision <- list_data_package_revisions(scope, identifier, filter = "newest")
  
  # create the packageId from scope, identifier and revision
  packageId <- paste(scope, identifier, revision, sep = ".")
  
  # read the data package info (urls to all associated entities)
  pc <- read_data_package(packageId)
  
  # grab the url for the table you are interested in (passed from the function call)
  tableurl <- as.character(pc[entity_seq]) 
  
  # read the data table and return the dataframe to the main program!
  dt <- read.csv(tableurl)
  return(dt)
}