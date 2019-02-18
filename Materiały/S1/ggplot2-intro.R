library(ggplot2)
library(SmarterPoland) #install.packages("SmarterPoland")
library(dplyr)

# 1. Mapowanie --------------------------------------------
# przypisanie zrodla danych
ggplot(data = countries)

# mapowanie 
# tu przypisujemy dane do osi, zwroc uwage na zmiane wartosci na osi x
ggplot(data = countries, aes(x = birth.rate, y = death.rate))
ggplot(data = countries, aes(x = continent, y = death.rate))

# dodawanie geometrii
ggplot(data = countries, aes(x = birth.rate, y = death.rate)) + 
  geom_point()

ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_point()

set.seed(1410)
ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_point(position = "jitter")

ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_point(alpha = 0.2)

ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_boxplot()

ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_boxplot(outlier.color = "red")

ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_dotplot(binaxis = "y", stackdir = "center")

ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_violin()

# dane mozna przypisywac nie tylko do osi, ale rowniez do innych atrybutow graficznych 
ggplot(data = countries, aes(x = birth.rate, y = death.rate, 
                             color = continent)) +
  geom_point()

# niektore geometrie wymagaja konkretnych atrybutow graficznych
ggplot(data = countries, aes(x = continent)) +
  geom_bar()

ggplot(data = countries, aes(x = death.rate)) +
  geom_density()

ggplot(data = countries, aes(x = death.rate, fill = continent)) +
  geom_density()

p1 <- ggplot(data = countries, aes(x = death.rate, fill = continent)) +
  geom_density(alpha = 0.2)

p2 <- ggplot(countries, aes(x = continent, y = death.rate)) +
  geom_violin()

library(gridExtra)
grid.arrange(p1, p2, nrow = 1)


# 2. Wielowarstwowe wykresy ------------------------
# geometrie mozna laczyc
ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_boxplot() +
  geom_point()

set.seed(15390)
ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_point(position = "jitter") +
  geom_boxplot(outlier.color = NA) # w celu unikniecia dublowania outlierow z geom_point

# wielowymiarowe i wielowarstwowe wykresy

continents <- group_by(countries, continent) %>% 
  na.omit %>% 
  summarise(death.rate = mean(death.rate),
            birth.rate = mean(birth.rate, na.rm = TRUE),
            population = mean(population),
            n.countries = length(country))

ggplot(continents, aes(x = birth.rate, y = death.rate, size = population)) +
  geom_point()

ggplot(continents, aes(x = birth.rate, y = death.rate, size = population, color = continent)) +
  geom_point() +
  scale_color_manual(values = c("red", "navyblue", "orange", "pink", "green"))

ggplot(continents, aes(x = birth.rate, y = death.rate, size = population, color = n.countries)) +
  geom_point()

ggplot(continents, aes(x = birth.rate, y = death.rate, size = population, color = n.countries)) +
  geom_point() +
  scale_size_continuous(range = c(4, 12)) +
  scale_x_continuous("Birth rate", expand = c(0.5, 0.5))

# geometrie automatycznie wykorzystują okreslone atrybuty graficzne.
# w tym przypadku atrybut size automatycznie jest przypisany zarowno do geom_point i geom_text
ggplot(continents, aes(x = birth.rate, y = death.rate, 
                       size = population, label = continent)) +
  geom_point() +
  geom_text(vjust = -1)

ggplot(continents, aes(x = birth.rate, y = death.rate, size = population, label = continent)) +
  geom_point() +
  geom_text(vjust = -1, size = 5)

# wiele uzytecznych geometrii mozna znalezc w pakietach rzoszerzajacych ggplot2
library(ggrepel)
ggplot(continents, aes(x = birth.rate, y = death.rate, size = population, label = continent)) +
  geom_point() +
  geom_text_repel(size = 5, force = 1)

ggplot(continents, aes(x = birth.rate, y = death.rate, size = population, label = continent)) +
  geom_point() +
  geom_label_repel(size = 5, force = 1)

ggplot(continents, aes(x = birth.rate, y = death.rate, size = population, label = continent, 
                       color = n.countries)) +
  geom_point() +
  geom_text_repel(size = 5, force = 1, color = "black") 

ggplot(continents, aes(x = birth.rate, y = death.rate, size = population, 
                       label = paste0(continent, "; ", n.countries, " countries"))) +
  geom_point() +
  geom_text_repel(size = 4, force = 1, color = "black") 

# na jednym rysunku mozna przedstawic dane z wiecej niz jednego zrodla

library(ggbeeswarm)
ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_quasirandom(method = "smiley")

ggplot(data = countries, aes(x = 1, y = death.rate, color = continent)) +
  geom_quasirandom()

ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_quasirandom() +
  geom_point(data = group_by(countries, continent) %>% summarise(mean.death.rate = mean(death.rate)),
             aes(y = mean.death.rate), color = "red", size = 4)

ggplot(data = countries, aes(x = continent, y = death.rate)) +
  geom_quasirandom() +
  geom_point(data = group_by(countries, continent) %>% summarise(mean.death.rate = mean(death.rate)),
             aes(x = continent, y = mean.death.rate), color = "red", size = 4, inherit.aes = FALSE)

# 3. Czytelnosc wykresow ------------------------------ 
# wykresy gestosci bywaja czytelniejsze niz punktowe w przypadku duzej liczby punktow
ggplot(countries, aes(x = birth.rate, y = death.rate, color = continent)) +
  geom_point()

ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  geom_density_2d(color = "black", contour = TRUE) +
  geom_point()

ggplot(countries, aes(x = birth.rate, y = death.rate, fill = continent)) +
  geom_density_2d(color = "black", contour = TRUE)

# wykorzystujemy stat, a nie geom, poniewaz interesuje nas inna geometria (polygon)
ggplot(countries, aes(x = birth.rate, y = death.rate, fill = continent)) +
  stat_density_2d(color = "black", contour = TRUE, geom = "polygon")

ggplot(countries, aes(x = birth.rate, y = death.rate, fill = continent)) +
  stat_density_2d(aes(alpha = ..level..), color = "black", contour = TRUE, geom = "polygon")

# czasami to nie wystarcza i trzeba uciec się do paneli
ggplot(countries, aes(x = birth.rate, y = death.rate, fill = continent)) +
  stat_density2d(aes(alpha = ..level..), color = "black", contour = TRUE, geom = "polygon") +
  facet_wrap(~ continent)

ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  stat_density2d(aes(alpha = ..level..), color = "black", contour = TRUE, geom = "polygon") +
  facet_wrap(~ continent)

ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  stat_density2d(aes(fill = ..level..), color = "black", contour = TRUE, geom = "polygon") +
  facet_wrap(~ continent) +
  scale_fill_gradient(low = "pink", high = "red")

# wykresy zyskuja poprzez pokazanie linii trendu

ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  geom_point() 

ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  geom_point() + 
  geom_smooth() 

ggplot(countries, aes(x = birth.rate, y = death.rate, color = continent)) +
  geom_point() + 
  geom_smooth() 

ggplot(countries, aes(x = birth.rate, y = death.rate)) +
  geom_point() + 
  geom_smooth(se = FALSE) 

# skrajne przypadki mozna podpisac
na.omit(countries) %>% 
  mutate(label.for.plot = ifelse(birth.rate %in% range(birth.rate) | death.rate %in% range(death.rate),
                                 country, "")) %>% 
  ggplot(aes(x = birth.rate, y = death.rate, label = label.for.plot)) +
  geom_point() +
  geom_text_repel()

# 4. Zadania ---------------------------------------------------
mieszkania <- read.csv(file = "https://raw.githubusercontent.com/STWUR/STWUR-2017-06-07/master/data/mieszkania_dane.csv", 
                       encoding = "UTF-8") %>% 
  na.omit

# I. Na jednym wykresie przedstaw rozklad cen i wieku mieszkan dla a) calego Wroclawia b) 
# poszczegolnych dzielnic.
# II. Przedstaw zaleznosc ceny za metr^2 od dzielnicy, pietra i roku zbudowania mieszkania.

# ad. II
mieszkania_pietro <- mutate(mieszkania, 
                            pietro_disc = cut(pietro, breaks = c(0, 1, 2, 3, 5, 16), include.lowest = TRUE))

ggplot(mieszkania_pietro, aes(x = rok, y = cena_m2)) +
  stat_density2d(aes(alpha = ..level..), color = "black", contour = TRUE, geom = "polygon")+
  facet_grid(dzielnica ~ pietro_disc)

ggplot(mieszkania_pietro, aes(x = rok, y = cena_m2)) +
  stat_density2d(aes(alpha = ..level..), color = "black", contour = TRUE, geom = "polygon") +
  facet_wrap(~ dzielnica + pietro_disc)
