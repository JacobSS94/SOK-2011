# | warnings : false
# Bibiliotek som trengs for analysen
install.packages("WDI")
install.packages("tidyverse")
install.packages("knitr")
install.packages("patchwork")
library(patchwork)
library(WDI)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(knitr)

# 1. Beregning av variabler knyttet til BNP per innbygger
# Hent data fra WDI
df_wdi_gdp <- WDI(
  country = "all",
  indicator = c('gdppc' = "NY.GDP.PCAP.PP.KD"),
  start = 1999,
  end = 2019,
  extra = TRUE
)

# NIVÅ på og VEKST i BNP per innbygger i perioden 2015 - 2019
# Velg relevant tidsperiode
start_year_gdp <- 2015
end_year_gdp <- 2019

# Filtrer data
df_gdp_subset <- df_wdi_gdp[df_wdi_gdp$year %in% start_year_gdp:end_year_gdp, ]

# Lag et datasett med informasjon om gjennomsnittlig nivå på og vekst i BNP per innbygger
df_gdp <- df_gdp_subset %>%
  filter(iso3c != "", income != "Aggregates") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(country, region, income, year, gdppc) %>% 
  drop_na(gdppc) %>% 
  group_by(country) %>% 
  arrange(country, year) %>% 
  mutate(
    avg_gdppc2019 = mean(gdppc), 
    gdpgrowth = (log(gdppc) - lag(log(gdppc))) * 100, 
    gy = mean(gdpgrowth, na.rm = TRUE)) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  select(-gdppc,
         -gdpgrowth) %>% 
  ungroup()

# Initialt NIVÅ på BNP per innbygger 
# Velg relevant tidsperiode
start_year_gdp0 <- 1999
end_year_gdp0 <- 2005

# Filtrer data
df_gdp0_subset <- df_wdi_gdp[df_wdi_gdp$year %in% start_year_gdp0:end_year_gdp0, ]

# Lag et datasett med initialt nivå på BNP per innbygger
df_gdp0<- df_gdp0_subset %>%
  filter(iso3c != "", income != "Aggregates") %>%
  mutate(year = as.numeric(year)) %>%
  select(country, year, gdppc) %>%
  drop_na(gdppc) %>%
  group_by(country) %>%
  arrange(country, year) %>%
  filter(year == min(year)) %>%
  select(country, gdppc0 = gdppc, year0 = year) %>% 
  mutate(ln_gdppc0 = log(gdppc0)) %>%
  ungroup()


# Lag et datasett med alle variabler knyttet til BNP per innbygger
df_gdp_all <- df_gdp %>%
  left_join(df_gdp0,by ="country")

# 2. Beregning av variabler knyttet til sparing og  befolkningsvekst
# Hent data fra WDI
df_wdi_sn <- WDI(
  country = "all",
  indicator = c('savings' = "NY.ADJ.NNAT.GN.ZS", 'popgrowth' = "SP.POP.GROW"),
  start = 2015,
  end = 2019,
  extra = TRUE
)  

# Lag et datasett med informasjon om gjennomsnittlig nivå på sparing
df_s <- df_wdi_sn %>%
  filter(iso3c != "", income != "Aggregates") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(country, year, savings) %>% 
  drop_na(savings) %>% 
  group_by(country) %>%
  arrange(country, year) %>% 
  mutate(
    s = mean(savings)) %>% 
  slice(1) %>% 
  select(country, s) %>% 
  ungroup()   

# Lag et datasett med informasjon om gjennomsnittlig nivå på befolkningsvekst
df_n <- df_wdi_sn %>%
  filter(iso3c != "", income != "Aggregates") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(country, year, popgrowth) %>% 
  drop_na(popgrowth) %>% 
  group_by(country) %>%
  arrange(country, year) %>% 
  mutate(
    n = mean(popgrowth)) %>% 
  slice(1) %>% 
  select(country, n) %>% 
  ungroup()    

# Sett sammen data om sparing og befolkningsvekst med data på BNP per innbygger
df_gdp_sn <- df_gdp_all %>% 
  left_join(df_s, by ="country") %>% 
  left_join(df_n, by= "country")

# 3. Beregning av variabler knyttet til kvalitet på arbeid (q_L)
WDIsearch(string = "human capital", field = "name", short = TRUE, cache = NULL) 
# Hent data fra wdi
df_wdi_ql <- WDI(
  country = "all",
  indicator = c('hci_index' = "HD.HCI.OVRL"),
  start = 2015,
  end = 2019,
  extra = TRUE
)  
# Lag et datasett med informasjon om gjennomsnittlig utdanningsnivå
df_ql <- df_wdi_ql %>%
  filter(iso3c != "") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(country, year, hci_index) %>% 
  drop_na(hci_index) %>% 
  group_by(country) %>%
  arrange(country, year) %>% 
  mutate(
    hci = mean(hci_index)) %>% 
  slice(1) %>% 
  select(country, hci) %>% 
  ungroup()         

# 4. Beregning av variabler knyttet til forbruk av naturressurer (depletion of natural resources): u
# Hent data fra WDI
df_wdi_u <- WDI(
  country = "all",
  indicator = c('nry' = "NY.ADJ.DRES.GN.ZS"),
  start = 2010,
  end = 2015,
  extra = TRUE
)   
# Beregne gjennomsnittlig forbruk (depletion rate)  
df_u <- df_wdi_u %>%
  filter(iso3c != "", income != "Aggregates") %>% 
  mutate(year = as.numeric(year)) %>% 
  select(country, year, nry) %>% 
  drop_na(nry) %>% 
  group_by(country) %>%
  arrange(country, year) %>% 
  mutate(
    u = mean(nry)) %>% 
  slice(1) %>% 
  select(country, u) %>% 
  ungroup()    

# Sett sammen data om kvalitet på arbeid og forbruk av naturressurser med data på andre variabler
data <- df_gdp_sn %>% 
  left_join(df_ql, by = "country") %>% 
  left_join(df_u, by = "country")

# Beregn deskriptiv statistikk for de utvalgte variablene
statistikk <- data %>%
  select(avg_gdppc2019, gy, s, n, hci, u) %>% 
  summarise_all(list(gjennomsnitt = mean, standardavvik = sd, minimum = min, maksimum = max), na.rm = TRUE)

print(statistikk)


# Scatterplot oppgave 2.2 
# Sparerate vs. BNP per innbygger
ggplot(data, aes(x = s, y = avg_gdppc2019)) +
  geom_point() + 
  theme_minimal() +
  labs(title = "Sparerate vs. BNP per innbygger",
       x = "Sparerate",
       y = "Gjennomsnittlig BNP per innbygger (2015-2019)",
       caption = "Datakilde: World Development Indicators") +
  geom_smooth(method = "lm", color = "blue", se = FALSE)

# Befolkningsvekstrate vs. BNP per innbygger
ggplot(data, aes(x = n, y = avg_gdppc2019)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Befolkningsvekstrate vs. BNP per innbygger",
       x = "Befolkningsvekstrate",
       y = "Gjennomsnittlig BNP per innbygger (2015-2019)",
       caption = "Datakilde: World Development Indicators") +
  geom_smooth(method = "lm", color = "red", se = FALSE)

# Regresjonsanalyse oppgave 2.2b
reg_modell1 <- lm(avg_gdppc2019 ~ s + n, data = data)
summary(reg_modell1)


# Scatterplot 2.3
# Humankapital vs. Vekstrate i BNP per innbygger
p1 <- ggplot(data, aes(x = hci, y = gy)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Humankapital vs. Vekstrate i BNP per innbygger",
       x = "Humankapital",
       y = "Vekstrate i BNP per innbygger")

# Forbruk av naturressurser vs. Vekstrate i BNP per innbygger
p2 <- ggplot(data, aes(x = u, y = gy)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Forbruk av naturressurser vs. Vekstrate i BNP per innbygger",
       x = "Forbruk av naturressurser",
       y = "Vekstrate i BNP per innbygger")

# Initialt nivå på BNP per innbygger vs. Vekstrate i BNP per innbygger
p3 <- ggplot(data, aes(x = ln_gdppc0, y = gy)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Initialt nivå på BNP per innbygger (log) vs. Vekstrate i BNP per innbygger",
       x = "Log av initialt BNP per innbygger",
       y = "Vekstrate i BNP per innbygger")

print(p1)
print(p2)
print(p3)

# Regresjonsanalyse oppgave 2.3b
reg_modell2 <- lm(gy ~ s + n + hci + u + gdppc0, data = data)
summary(reg_modell2)
