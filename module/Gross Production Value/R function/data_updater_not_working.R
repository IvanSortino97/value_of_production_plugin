?AddInsertions

url <- "http://fenixservices.fao.org/faostat/static/bulkdownloads/domainvargroupvar.csv"

dest <- "~/Agriculture Production/module/Gross Production Value/R function/composition_table.csv"

download.file(url = url, destfile = dest)

fread(dest)

#df <- read.csv(dest, fileEncoding="UTF-16LE") work but too slow

