# Homework of TymonCzarnota
# Original source: https://github.com/mini-pw/WizualizacjaDanych2018/pull/90
# All added/changed lines are preceeded with an appropriate comment with a justification

library("ggplot2")
#The next two lines are obligatory to install the patchwork packages which is used to display plots (using "+" operator) in the orignal code
#install.packages("devtools")
#devtools::install_github("thomasp85/patchwork")
library(patchwork)
# The stringr library is needed to wrap strings (the labels on plots)
#install.packages("stringr")
library(stringr)

Health<-data.frame(
  "reason"=c("No initial health problems",  "No change in health", "No initial problems or change"), 
  "value"=c(-3.2, -1.6, -4.8) 
)

Employment<-data.frame(
  "reason"=c("No voluntary switches","No job loss with new job","No job loss without new job"),
  "value"=c(0.6, 0.3, -2.3) 
)

Family_related<-data.frame(
  "reason"=c("No spouse retirement","No parents moving in"),
  "value"=c(-0.8,-0.2) 
)

Financial<-data.frame(
  "reason"=c("No financial gains","No financial losses"),
  "value"=c(-0.7,-0.9) 
)


HealthPlot<-ggplot(data = Health, aes(x=reason,y=value)) +
  # THEMES: changing color to a neutral taint (red is usually associated with something drastic and negative)
  geom_col(fill = "royalblue2") +
  labs(y="Value(%)",x="Health") +
  # THEMES: wrapping the labels on x-axis, so they are readable
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  # AESTHETICS: Fixed the scale, so that all graphs have the same Y limits and the scale is not misleading while comparing the graphs
  ylim(-5,1)
EmploymentPlot<-ggplot(data = Employment, aes(x=reason,y=value)) +
  # THEMES: changing color to a neutral taint (red is usually associated with something drastic and negative)
  geom_col(fill = "royalblue2") +
  labs(y="Value(%)",x="Employment") +
  # THEMES: wrapping the labels on x-axis, so they are readable
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  # AESTHETICS: Fixed the scale, so that all graphs have the same Y limits and the scale is not misleading while comparing the graphs
  ylim(-5,1)
FamilyPlot<-ggplot(data = Family_related, aes(x=reason,y=value)) +
  # THEMES: changing color to a neutral taint (red is usually associated with something drastic and negative)
  geom_col(fill = "royalblue2") +
  labs(y="Value(%)",x="Family") +
  # THEMES: wrapping the labels on x-axis, so they are readable
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  # AESTHETICS: Fixed the scale, so that all graphs have the same Y limits and the scale is not misleading while comparing the graphs
  ylim(-5,1)
FinancialPlot<-ggplot(data = Financial, aes(x=reason,y=value)) +
  # THEMES: changing color to a neutral taint (red is usually associated with something drastic and negative)
  geom_col(fill = "royalblue2",alpha=0.2) +
  labs(y="Value(%)",x="Financial") +
  # THEMES: wrapping the labels on x-axis, so they are readable
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  # AESTHETICS: Fixed the scale, so that all graphs have the same Y limits and the scale is not misleading while comparing the graphs
  ylim(-5,1)

# DATA (or THEMES?): Added the title of the graph within the joined graphs object so that no matter where they are reused the topic is displayed
# source: (https://www.marketwatch.com/story/why-do-37-of-older-workers-retire-earlier-than-planned-2019-02-27?mod=newsviewer_click)
TitleText="Percentage-Point Change in Share of Sample Retiring Early if No Shocks"

# DATA (or THEMES?): In the source article of the graph there is an important note about the meaning of solid (and nonsolid) bars, 
#                    because it is crucial to understand why some of the bars are partially transparent
# source: (https://www.marketwatch.com/story/why-do-37-of-older-workers-retire-earlier-than-planned-2019-02-27?mod=newsviewer_click)
NoteText <- "NOTE: Solid bars indicate the original coefficient in the regression was statistically significant at least at the 10-percent level."

plots = (HealthPlot+EmploymentPlot+FamilyPlot+FinancialPlot) 
# Added the caption and the title of all graphs
plots + labs(caption = NoteText) +plot_annotation(title = TitleText)