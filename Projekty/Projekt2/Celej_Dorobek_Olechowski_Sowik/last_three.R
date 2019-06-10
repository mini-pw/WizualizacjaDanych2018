library(latticeExtra)
library(ggplot2)
library(ggthemes)

# 3. niepotrzebny trzeci wymiar w słupkowym

d <- read.table(text='Family_size   Income     Placement
   small   8   Big_City
   small   4   Village
   small   6   Small_City
   middle   7   Small_City
   middle   3.5   Village
   middle   5   Big_City
   big  7   Big_City
   big  5   Village
   big  4   Small_City
   ', header=TRUE)

# Zły pryzkład

seven_bad <- cloud(Income~Family_size+Placement, d, panel.3d.cloud=panel.3dbars, col.facet='grey',
      xbase=0.2, ybase=0.4, scales=list(arrows=FALSE, col=1),
      par.settings = list(axis.line = list(col = "transparent")))

# Dobry przykład
seven_ok <- ggplot(d, aes(Family_size, Income, group=Placement, colour=Placement)) +
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
  labs(title="Income in households",
       subtitle="by placement and family size") +
   theme_hc() + scale_colour_hc()

# 4. ucięcie skali

dat <- read.table(text='Party     Support
   Party1 21485
   Party2 20453
   Party3 19858
   Party4 19240
   Party5 18874
   Party6 18024
   ', header=TRUE)



# Dobry przykład
eight_ok <- ggplot(dat, aes(x=Party, y=Support, fill=Party)) +
  geom_bar(stat = "identity")+
  theme (title = element_text(size=15))+
  labs(title="Political Parties Support") +
   theme_hc() + scale_fill_hc() +theme(legend.position = "none")

#Zły przykład
eight_bad <- ggplot(dat, aes(x=Party, y=Support, fill=Party)) +
  geom_bar(stat = "identity")+
  theme (title = element_text(size=15))+
  labs(title="Political Parties Support")+
  coord_cartesian(ylim=c(18000,22000)) +
   theme_hc() + scale_fill_hc() +theme(legend.position = "none")

# 5. zlewające się kolory

df <- read.table(text='Time Company Value
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
six_ok <- ggplot(df, aes(x=Time, y=Value, group=Company, colour=Company)) +
    geom_line(size=2)+
    theme(legend.position = c(0.9, 0.9))+
    theme (
      legend.title = element_text(size=15),
      legend.text = element_text(size=15),
      title = element_text(size=15)
    )+
    labs(x="Time [Years]",
         y="Market Value[Mln $]",
         title="Companies Value")+
    scale_x_continuous(breaks=c(2001:2010)) + scale_colour_hc() +theme_hc()
# Zły przykład
six_bad <- ggplot(df, aes(x=Time, y=Value, group=Company, colour=Company)) +
   geom_line(size=2)+
   theme(legend.position = c(0.9, 0.9))+
   theme (
      legend.title = element_text(size=15),
      legend.text = element_text(size=15),
      title = element_text(size=15)
   )+
   labs(x="Time [Years]",
        y="Market Value[Mln $]",
        title="Companies Value")+
   scale_x_continuous(breaks=c(2001:2010)) +  scale_color_hue(l=2, c=12) + theme_hc()

