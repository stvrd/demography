library(tidyverse)
library(pxweb)
library(plotly)

# Schweiz -----------------

# Sx

# Get Data
px1 <- "https://www.pxweb.bfs.admin.ch/api/v1/de/px-x-0102020300_101/px-x-0102020300_101.px"
bfs1 <- pxweb_get(url = px1, query = "G:/Vorsorge/Depart/PProf/05_Mitarbeiter/01_RiedoSteve/BFS/bfs_sterberate.json") %>% as.data.frame() 

# Transform Data
bfs2 <- bfs1 %>% 
  as_tibble %>% 
  separate(Alter, into = c('age', 'txt')) %>% 
  separate(Beobachtungseinheit, into = c('tmp', 'var')) %>% 
  transmute(Geburtsjahrgang = as.numeric(as.character(Geburtsjahrgang)),
            sex = Geschlecht,
            age = as.numeric(age),
            var = var,
            val = `Kohortensterbetafeln für die Schweiz (1876-2030) nach Geburtsjahrgang, Geschlecht und Alter`) %>% 
  pivot_wider(names_from = var, values_from = val) %>% 
  group_by(sex, Geburtsjahrgang) %>% 
  mutate(Dx = abs(c(diff(Sx),0)),
         Qx = (Sx-lead(Sx))/Sx)

bfs2 %>% 
  plot_ly(x = ~age,
          frame = ~Geburtsjahrgang,
          color = ~sex,
          colors = 'Paired') %>% 
  add_lines(y = ~Sx) %>%
  layout(title = 'Sx')


# Pyramid
px2 <- 'https://www.pxweb.bfs.admin.ch/api/v1/de/px-x-0102020000_103/px-x-0102020000_103.px'
bfs2 <- pxweb_get(url = px2, query = "G:/Vorsorge/Depart/PProf/05_Mitarbeiter/01_RiedoSteve/BFS/bfs_bev.json") %>% as.data.frame() %>% 
  as_tibble
names(bfs2) <- c('var', 'origin', 'sex', 'age', 'yr', 'val')
bfs3 <- bfs2 %>% 
  separate(age, into = 'age') %>% 
  mutate(age = as.numeric(age))

# Auslaender:
bfs3 %>% 
  group_by(origin, sex,yr) %>% 
  summarise(val = sum(val)) %>%  
  plot_ly(x = ~yr, y = ~val, color = ~origin) %>% 
  add_lines()

mum <- tibble(yr = 1971:2017,
       x0 = -60000,
       x1 = 60000,
       age = yr-1960) %>% 
  pivot_longer(cols = x0:x1,names_to = 'var', values_to = 'val')

me <- tibble(yr = 1971:2017,
       x0 = -60000,
       x1 = 60000,
       age = yr-1988) %>% 
  pivot_longer(cols = x0:x1,names_to = 'var', values_to = 'val')



bfs3 %>% 
  group_by(sex,age,yr) %>% summarise(val = sum(val)) %>% 
  mutate(yr = as.numeric(as.character(yr)),
         val = ifelse(sex == 'Mann', -val, val)) %>% 
  plot_ly(x = ~val,
          y = ~age,
          frame = ~yr) %>% 
  add_segments(xend = 0, 
               yend = ~age, 
               color = ~sex,
               colors = 'Paired') %>% 
  add_trace(data = mum, 
            type = 'scatter', 
            mode = 'lines',
            line = list(color='black')) %>% 
  add_trace(data = me, 
            type = 'scatter', 
            mode = 'lines',
            line = list(color='gold')) %>% 
  layout(title = 'Population Pyramid Switzerland') %>% 
  animation_opts(transition=0,redraw = T) %>% 
  animation_button(visible=FALSE) %>% 
  config(displayModeBar = F)



# Population pyramid UAE------------

library(idbr)

idb_api_key('c5228466ffa5bf9cb7a381a213733db463fd8e41_2020')

male <- idb1("United Arab Emirates", 2016, sex = "male") %>%
  mutate(POP = POP * -1,
         SEX = "Male")

female <- idb1("United Arab Emirates", 2016, sex = "female") %>%
  mutate(SEX = "Female")

uae <- rbind(male, female) %>%
  mutate(abs_pop = abs(POP))

plot_ly(uae, x = ~POP, y = ~AGE, color = ~SEX, frame = ~time) %>%
  add_bars(orientation = "h",
           hoverinfo = "y+text+name", text = ~abs_pop, 
           colors = c("darkgreen", "red")) %>%
  layout(bargap = 0.1, barmode = "overlay", 
         title = "United Arab Emirates, 2016", 
         xaxis = list(tickmode = "array", 
                      tickvals = c(-150000, -100000, -50000, 0, 50000),
                      ticktext = c("150k", "100k", "50k", "0", "50k"), 
                      title = "Population"), 
         yaxis = list(title = "Age"))

# Population pyramid US -----------------

us <- bind_rows(
  idb1(
    country = "US", 
    year = c(1980,1990,2000,2010,2020,2030,2040,2050), 
    variables = c("AGE", "NAME", "POP"), 
    sex = "male"
  ),
  idb1(
    country = "US", 
    year = c(1980,1990,2000,2010,2020,2030,2040,2050), 
    variables = c("AGE", "NAME", "POP"), 
    sex = "female"
  )
)

us <- us %>%
  mutate(
    POP = if_else(SEX == 1, POP, -POP),
    SEX = if_else(SEX == 1, "Male", "Female")
  )

plot_ly(us, size = I(5), alpha  = 0.5) %>%
  add_segments(
    x = ~POP, xend = 0, 
    y = ~AGE, yend = ~AGE, 
    color = ~factor(SEX),
    colors = 'Paired',
    frame = ~time
  ) %>% 
  layout(title = 'US Population Pyramid')

plot_ly(us, alpha  = 0.5) %>%
  add_lines(
    x = ~AGE, y = ~abs(POP),
    frame = ~time, 
    color = ~factor(SEX),
    line = list(simplify = FALSE)
  ) %>%
  layout(yaxis = list(title = "US population"))



