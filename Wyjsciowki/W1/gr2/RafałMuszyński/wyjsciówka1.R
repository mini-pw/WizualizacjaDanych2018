a <- countries %>% 
  filter(continent == 'Europe' | continent == 'Africa') %>% 
  ggplot(aes(x = population, fill = continent)) +
  geom_density(alpha = 0.2, color = 'black') + 
  scale_fill_manual( values = c('red', 'blue'))

b <- countries %>% 
  filter(continent == 'Europe' | continent == 'Africa') %>% 
  ggplot(aes(x = population, fill = continent)) +
  geom_density(alpha = 0.2, color = 'black') + 
  scale_fill_manual( values = c('yellow', 'blue'))

c <- countries %>% 
  filter(continent == 'Europe' | continent == 'Africa') %>% 
  ggplot(aes(x = population, fill = continent)) +
  geom_density(alpha = 0.2, color = 'black') + 
  scale_fill_manual( values = c('red', 'orange'))

d <- countries %>% 
  filter(continent == 'Europe' | continent == 'Africa') %>% 
  ggplot(aes(x = population, fill = continent)) +
  geom_density(alpha = 0.2, color = 'black') + 
  scale_fill_manual( values = c('purple', 'blue'))


grid.arrange(a,b,c,d)