library(latticeExtra)
library(ggplot2)
library(ggthemes)

# 3. niepotrzebny trzeci wymiar w słupkowym

d <- read.table(text='Wielkosc_rodziny   Dochody     Zamieszkanie
   mała   8   Duże_miasto
   mała   4   Wieś
   mała   6.6   Małe_miasto
   średnia   7   Duże_miasto
   średnia   3.5   Wieś
   średnia   5   Małe_miasto
   duża  7   Duże_miasto
   duża  5   Wieś
   duża  4.8   Małe_miasto
   ', header=TRUE)

# Zły pryzkład

seven_bad <- cloud(Dochody~Wielkosc_rodziny+Zamieszkanie, d, panel.3d.cloud=panel.3dbars, col.facet='grey',
      xbase=0.2, ybase=0.4, scales=list(arrows=FALSE, col=1),
      par.settings = list(axis.line = list(col = "transparent")))

# Dobry przykład
seven_ok <- ggplot(d, aes(Wielkosc_rodziny, Dochody, group=Zamieszkanie, colour=Zamieszkanie)) +
  geom_line() +
  geom_point() +
  ylim(0.0, 9.0)+
  geom_line(size=2)+
  theme(legend.position = c(0.9, 0.9))+
  theme (
    legend.title = element_text(size=15),
    legend.text = element_text(size=15),
    title = element_text(size=15)
  )+
  labs(title="Dochody w gospodarstwach domowych",
       subtitle="Na podstawie zamieszkania i wielkości rodziny") +
   theme_hc() + scale_colour_hc()

# 4. ucięcie skali

dat <- read.table(text='Partia     Poparcie
   Partia1 21485
   Partia2 20453
   Partia3 19858
   Partia4 19240
   Partia5 18874
   Partia6 18024
   ', header=TRUE)



# Dobry przykład
eight_ok <- ggplot(dat, aes(x=Partia, y=Poparcie, fill=Partia)) +
  geom_bar(stat = "identity")+
  theme(legend.position = c(0.9, 0.9))+
  theme (
    legend.title = element_text(size=15),
    legend.text = element_text(size=15),
    title = element_text(size=15)
  )+
  labs(title="Poparcie partii politycznych") +
   theme_hc() + scale_fill_hc()
#Zły przykład
eight_bad <- ggplot(dat, aes(x=Partia, y=Poparcie, fill=Partia)) +
  geom_bar(stat = "identity")+
  theme(legend.position = c(0.9, 0.9))+
  theme (
    legend.title = element_text(size=15),
    legend.text = element_text(size=15),
    title = element_text(size=15)
  )+
  labs(title="Poparcie partii politycznych")+
  coord_cartesian(ylim=c(18000,22000)) +
   theme_hc() + scale_fill_hc()

# 5. zlewające się kolory

df <- read.table(text='Czas Firma Wartosc
   2001 A  9.5
   2002 A  8.9
   2003 A  9.02
   2004 A  8.42
   2005 A  8.66
   2006 A  8.3
   2007 A  7.94
   2008 A  7.82
   2009 A  8.06
   2010 A  7.7
   2001 B  8.9
   2002 B  8.66
   2003 B  8.3
   2004 B  7.7
   2005 B  7.34
   2006 B  5.9
   2007 B  3.5
   2008 B  3.26
   2009 B  3.14
   2010 B  3.38
   2001 C  10.94
   2002 C  10.7
   2003 C  10.34
   2004 C  8.9
   2005 C  8.78
   2006 C  8.66
   2007 C  7.58
   2008 C  7.1
   2009 C  6.86
   2010 C  6.14
   ', header=TRUE)

# Dobry przykład
six_ok <- ggplot(df, aes(x=Czas, y=Wartosc, group=Firma, colour=Firma)) +
    geom_line(size=2)+
    theme(legend.position = c(0.9, 0.9))+
    theme (
      legend.title = element_text(size=15),
      legend.text = element_text(size=15),
      title = element_text(size=15)
    )+
    labs(x="Czas [Lata]",
         y="Cena Rynkowa[Mln $]",
         title="Wartość rynkowa firmy",
         subtitle="W ostatnich latach ich funkcjonowania")+
    scale_x_continuous(breaks=c(2001:2010)) + scale_colour_hc()
# Zły przykład
six_bad <- p +  scale_color_hue(l=2, c=12)
