countries <- c('Estonia', 'Holland', 'Estonia', 'Romania',
               'Holland', 'Poland', 'Slovakia',
               'Spain', 'Slovakia', 'Poland', 'Hungary',
               'Switzerland', 'Hungary', 'Romania', 'Switzerland', 
               'Bulgaria', 'Spain', 'Bulgaria', 'Kosovo',
               'Albania', 'Macedonia', 'Macedonia')

cities <- c('Tallin', 'Amsterdam', 'Tartu', 'Pitesti', 
            'Sint Maartensbrug', 'Warsaw', 'Trencin', 
            'Aviles', 'Bratislava', 'Cracow', 'Sajoszentpeter',
            'Bern', 'Budapest', 'Bucarest', 'Lugano', 
            'Vidin', 'Madrid', 'Sofia', 'Dramjak', 
            'Korce', 'Tetovo', 'Skopje')

days <- c(4, 4, 3, 32, 22, 86, 35, 127, 23, 164, 76, 2, 46,
          41, 9, 166, 20, 71, 67, 61, 293, 162)

annual_concentration <- c(15.7, 22.5, 16.8, 33.9, 27.6, 41.6,
                          29.4, 45.8, 29.1, 56.7, 35.9, 19.3,
                          33.5, 35.5, 19.9, 61.1, 24.0, 40.0,
                          33.7, 40.2, 97.3, 84.1)

smog_data <- data.frame(countries, cities, days, annual_concentration)

write.table(smog_data, "smog_data.tsv", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
