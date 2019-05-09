library(SmarterPoland)
class(countries)
colnames(countries) <- c("country", "birthRate", "deathRate", "population", "continent" )
write.table(file = "countries.tsv", countries, sep = "\t", quote = FALSE, row.names = FALSE)