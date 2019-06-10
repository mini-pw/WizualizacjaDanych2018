populations <- c(12,31.35)
cnt_pol <- c("Polacy","Ukraincy","Bialorusini","Litwini","Prusacy","Zydzi","Liwowie")
pop_pol <- c(4.5,3.5,1.5,0.75,0.75,0.5,0.5)
pop_pol_data <-  data.frame(
  country = cnt_pol,
  population= pop_pol
)
cnt_tur <- c("Turcy","Slowianie", "Arabowie","Ormianie","Grecy","Albanczycy","Kurdowie","Inni")
pop_tur <- c(12.8,6.2,4.7,2.4,2,1.5,1,0.75)
pop_tur_data = data.frame(
  country = cnt_tur,
  population= pop_tur
)
Army <- c(60,155.2)

cat_army <- c("Piechota","Kawaleria")
army_pol <- c(38,22)
army_tur <- c(74.4,80.8)

army_data <- data.frame(
  category = cat_army,
  poland_size = army_pol,
  ottoman_size = army_tur
)

combined_army_data <-  data.frame(
  country = c("Rzeczpospolita", "Imperium Osmanskie"),
  population = populations,
  infantry = c(38,74.4),
  cavalry = c(22,80.8)
)

# POPULATION
get_population_plot <- function(data) {
  data_sorted <- data %>% arrange(desc(population)) 
  data_sorted$country <- factor(data_sorted$country, levels=data_sorted$country)
  ggplot(data_sorted, aes(x=country, y=population, fill=country, label=population)) +
    geom_bar(width = 1, stat = "identity") +
    geom_text(aes(label=population),
              colour ="White",
              fontface="bold", 
              size = 6, hjust =0.5,
              vjust = 1.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=14),
          axis.title.y = element_text(size=14),
          legend.position = "None") +
    xlab("narodowosc") +
    ylab("populacja [mln]")
}
population_poland_plot <- get_population_plot(pop_pol_data)
population_poland_plot

population_turkey_plot <- get_population_plot(pop_tur_data)
population_turkey_plot

poland_force_categories_general <- ggplot(army_data, aes(x=category, y=poland_size, fill=category))+
  geom_bar(stat="identity") +
  geom_text(aes(label=poland_size),
            colour ="White",
            fontface="bold", 
            size = 6, hjust =0.5,
            vjust = 1.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.position = "None") +
  xlab("kategoria") +
  ylab("rozmiar [tys]")
poland_force_categories_general

turkey_force_categories_general <- ggplot(army_data, aes(x=category, y=ottoman_size, fill=category))+
  geom_bar(stat="identity") +
  geom_text(aes(label=poland_size),
            colour ="White",
            fontface="bold", 
            size = 6, hjust =0.5,
            vjust = 1.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        legend.position = "None") +
  xlab("kategoria") +
  ylab("rozmiar [tys]")
turkey_force_categories_general

combined_population <- ggplot(combined_army_data, aes(x="", y=population, fill=country)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_minimal() +
  ylab("liczebnosc [mln]") +
  xlab("") + 
  guides(fill=guide_legend(title="Panstwo")) +
  theme(legend.position = "top",
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))
combined_population

combined_infantry <- ggplot(combined_army_data, aes(x="", y=infantry, fill=country)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_minimal() +
  ylab("Piechota") +
  xlab("") +
  guides(fill=guide_legend(title="Panstwo")) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))

combined_infantry

combined_cavalry <- ggplot(combined_army_data, aes(x="", y=cavalry, fill=country)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_minimal() +
  ylab("Kawaleria") +
  xlab("") + 
  guides(fill=guide_legend(title="Panstwo")) +
  theme_minimal() +
  theme(legend.position = "top",
        axis.title.y = element_text(size=16), 
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14))

combined_cavalry
