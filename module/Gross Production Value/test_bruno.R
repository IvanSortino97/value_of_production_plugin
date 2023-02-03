library(data.table)
faostat_aggregates_link <- 'http://fenixservices.fao.org/faostat/static/bulkdownloads/domainvargroupvar.csv'

# ca <- fread("grep California us-counties.csv", 
#             col.names = names(mydt10))
# 

faostat_aggregates_temp_file <- paste0(tempfile(), 'domainvargroupvar.csv')

# paste0(faostat_aggregates_temp_file, '/', 'domainvargroupvar.csv')


download.file(faostat_aggregates_link, 
              faostat_aggregates_temp_file, 
              mode = 'wb', 
              method="wget")


bubu = read.csv(faostat_aggregates_temp_file, fill=TRUE, comment.char="", sep = ",") #122802 rows
bubu = read.csv(faostat_aggregates_temp_file) #122802 rows
dim(bubu)
tail(bubu)




test <- read.table(faostat_aggregates_temp_file, comment.char = "", fill=TRUE, 
                   header = TRUE, sep = ",", check.names = FALSE) #58005 rows

dim(test)
head(test)

colClasses = rep('character', 16)

fread(faostat_aggregates_temp_file, 
      colClasses = c("character", "character", "character", "character", "character", 
                     "character", "character", "character", "character", "character", 
                     "character", "character", "character", "character", "character", 
                     "character"))

test <- fread(faostat_aggregates_temp_file,  
              #encoding = "Latin-1", 
              fill=TRUE,
              # header = TRUE,
              sep = ",",
              skip=4118
              #drop = 17
              )


faostat_aggregates <- fread('grep Value of Agricultural Production faostat_aggregates_temp_file')

fread("/tempdir/RtmpUzvyyr/file319363ea5b63")

head(faostat_aggregates)


tf_land <- tempfile()
download.file(land_link, tf_land, mode = 'wb', method="wget")
td_land <- tempdir()
file.name <- unzip(tf_land, exdir = td_land)
file.name <- grep("Normalized", file.name, value = T)
land <- fread(file.name, 
              colClasses = c(`Area Code`="integer", Area="character", 
                             `Item Code`="integer", Item="character",
                             `Element Code`="integer", Element="character", `Year Code`="integer", 
                             Year="character",
                             Unit="character", Value="numeric", Flag="character"))
