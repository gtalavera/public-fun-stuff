# Campeones futbol peruano
# Fuente: ESPN https://espndeportes.espn.com/futbol/peru/nota/_/id/6428784/historial-campeones-campeonato-torneo-peruano-peru
library(dplyr)
library(ggplot2)
campeones <-
  tribble(
    ~anio, ~equipo,
    1912,	"Lima Cricket",
    1913,	"Jorge Chávez 1",
    1914,	"Lima Cricket",
    1915,	"Sport José Gálvez",
    1916,	"Sport José Gálvez",
    1917,	"Sport Juan Bielovucic",
    1918,	"Alianza Lima",
    1919,	"Alianza Lima",
    1920,	"Sport Inca",
    1921,	"Sport Progreso",
    1926,	"Sport Progreso",
    1927,	"Alianza Lima",
    1928,	"Alianza Lima",
    1929,	"Federación Universitaria",
    1930,	"Atlético Chalaco",
    1931,	"Alianza Lima",
    1932,	"Alianza Lima",
    1933,	"Alianza Lima",
    1934,	"Alianza Lima",
    1935,	"Sport Boys",
    1937,	"Sport Boys",
    1938,	"Deportivo Municipal",
    1939,	"Alianza Lima",
    1940,	"Deportivo Municipal",
    1941,	"Universitario",
    1942,	"Sport Boys",
    1943,	"Deportivo Municipal",
    1944,	"Mariscal Sucre",
    1945,	"Universitario",
    1946,	"Universitario",
    1947,	"Atlético Chalaco",
    1948,	"Alianza Lima",
    1949,	"Universitario",
    1950,	"Deportivo Municipal",
    1951,	"Sport Boys",
    1952,	"Alianza Lima",
    1953,	"Sucre FBC",
    1954,	"Alianza Lima",
    1955,	"Alianza Lima",
    1956,	"Sporting Cristal",
    1957,	"Centro Iqueño",
    1958,	"Sport Boys",
    1959,	"Universitario",
    1960,	"Universitario",
    1961,	"Sporting Cristal",
    1962,	"Alianza Lima",
    1963,	"Alianza Lima",
    1964,	"Universitario",
    1965,	"Alianza Lima",
    1966,	"Universitario",
    1967,	"Universitario",
    1968,	"Sporting Cristal",
    1969,	"Universitario",
    1970,	"Sporting Cristal",
    1971,	"Universitario",
    1972,	"Sporting Cristal",
    1973,	"Defensor Lima",
    1974,	"Universitario",
    1975,	"Alianza Lima",
    1976,	"Unión Huaral",
    1977,	"Alianza Lima",
    1978,	"Alianza Lima",
    1979,	"Sporting Cristal",
    1980,	"Sporting Cristal",
    1981,	"Melgar",
    1982,	"Universitario",
    1983,	"Sporting Cristal",
    1984,	"Sport Boys",
    1985,	"Universitario",
    1986,	"San Agustín",
    1987,	"Universitario",
    1988,	"Sporting Cristal",
    1989,	"Unión Huaral",
    1990,	"Universitario",
    1991,	"Sporting Cristal",
    1992,	"Universitario",
    1993,	"Universitario",
    1994,	"Sporting Cristal",
    1995,	"Sporting Cristal",
    1996,	"Sporting Cristal",
    1997,	"Alianza Lima",
    1998,	"Universitario",
    1999,	"Universitario",
    2000,	"Universitario",
    2001,	"Alianza Lima",
    2002,	"Sporting Cristal",
    2003,	"Alianza Lima",
    2004, "Alianza Lima",
    2005, "Sporting Cristal",
    2006, "Alianza Lima",
    2007, "San Martín",
    2008, "San Martín",
    2009, "Universitario",
    2010, "San Martín",
    2011, "Juan Aurich",
    2012, "Sporting Cristal",
    2013, "Universitario",
    2014, "Sporting Cristal",
    2015, "Melgar",
    2016, "Sporting Cristal",
    2017, "Alianza Lima",
    2018, "Sporting Cristal",
    2019, "Binacional",
    2020,	"Sporting Cristal",
    2021,	"Alianza Lima",
    2022,	"Alianza Lima"
  ) %>% 
  mutate(
    equipo = ifelse(equipo == "Federación Universitaria", "Universitario", equipo)
  ) %>% 
  group_by(equipo) %>%
  mutate(titulos = row_number())

campeones2 <- campeones %>% 
  group_by(equipo) %>% 
  complete(anio = seq(1912, 2022, 1L)) %>% 
  mutate(titulo_rev = ifelse(anio==1912, 0, titulos)) %>% 
  tidyr::fill(titulo_rev, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(label = if_else(anio == max(anio), as.character(equipo), NA_character_))


ggplot(campeones2, aes(anio, titulo_rev, color = equipo))+
  # geom_point(size = 0.3)+
  # geom_smooth(method = "loess", se = FALSE, 
  #             span = 0.1, linetype=1, size = 0.3)+
  geom_step(
    size = 3,
    linejoin = "bevel",
    lineend = "round"
  )+
  ggrepel::geom_label_repel(
    aes(label = label),
    family = "Sanchez",
    direction = "y",
    hjust = "left",
    nudge_x = 5,
    max.overlaps = 5,
    min.segment.length = 500)+
  theme(
    legend.position = "none")
