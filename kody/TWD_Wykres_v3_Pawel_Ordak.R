library(dplyr)
library(ggplot2)
library(sf)

# Dane: https://bdl.stat.gov.pl/bdl/dane/podgrup/wymiary/22/429/3552 (rodziny, którym przyznano pomoc z powodu narkomanii)
# Ludność: https://bdl.stat.gov.pl/bdl/dane/podgrup/wymiary/3/7/2137
# Granice: https://www.geoportal.gov.pl/pl/dane/panstwowy-rejestr-granic-prg/ (PRG jednostki administracyjne 2025)

rodziny <- read.csv("OCHR_3552_CREL_20261111160441.csv", sep = ";", dec = ",",
                    encoding = "UTF-8", colClasses = c(Kod = "character"))
ludnosc <- read.csv("LUDN_2137_CREL_20261111160851.csv", sep = ";", dec = ",",
                    encoding = "UTF-8", colClasses = c(Kod = "character"))

# Liczba rodzin na 100 tys. mieszkańców w każdym roku, a potem średnia z lat 2014–2025.
dane <- rodziny %>%
  filter(startsWith(Nazwa, "Powiat")) %>%
  select(Kod, Rok, rodziny = Wartosc) %>%
  left_join(ludnosc %>% select(Kod, Rok, mieszkancy = Wartosc), by = c("Kod", "Rok")) %>%
  mutate(na_100_tys = rodziny / mieszkancy * 100000) %>%
  group_by(Kod) %>%
  summarise(na_100_tys = mean(na_100_tys)) %>%
  # W pliku z granicami kod powiatu to pierwsze 4 cyfry kodu z BDL.
  # Łączymy po kodzie, a nie po nazwie, bo nazwy powiatów się powtarzają (np. powiat brzeski).
  mutate(Kod = substr(Kod, 1, 4),
         przedzial = cut(na_100_tys, breaks = c(0, 5, 10, 20, 40, Inf),
                         labels = c("do 5", "5–10", "10–20", "20–40", "powyżej 40")))

powiaty <- st_read("PRG_jednostki_administracyjne_2025/A02_Granice_powiatow.shp", quiet = TRUE)
wojewodztwa <- st_read("PRG_jednostki_administracyjne_2025/A01_Granice_wojewodztw.shp", quiet = TRUE)

# PRG wlicza do powiatów morskie wody wewnętrzne (Zatoka Pucka, zalewy), przez co Hel zlewa się z lądem.
# Przycinamy granice do lądu z GADM: https://gadm.org/download_country.html (GADM 4.1, Polska, poziom 0)
# Przycinamy granice do lądu z GADM: https://gadm.org/download_country.html (GADM 4.1, Polska, poziom 0)
lad <- st_read("gadm41_POL_shp/gadm41_POL_0.shp", quiet = TRUE) %>%
  st_transform(st_crs(powiaty)) %>%
  st_make_valid() %>%
  st_geometry()
powiaty <- powiaty %>% st_make_valid() %>% st_intersection(lad)
wojewodztwa <- wojewodztwa %>% st_make_valid() %>% st_intersection(lad)

wykres <- powiaty %>%
  left_join(dane, by = join_by(JPT_KOD_JE == Kod)) %>%
  ggplot() +
  geom_sf(aes(fill = przedzial), color = "white", linewidth = 0.1) +
  geom_sf(data = wojewodztwa, fill = NA, color = "black", linewidth = 0.3) +
  scale_fill_manual(values = colorRampPalette(c("lightblue", "navy"))(5)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "Pomoc społeczna z powodu narkomanii",
    subtitle = "Srednia roczna liczba rodzin objęta pomocą w powiatach (w latach 2014–2025)",
    fill = "Rodziny na 100 tys.\nmieszkańców",
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(size = 19, face = "bold"),
    plot.caption = element_text(hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(12, 14, 10, 12)
  )

print(wykres)
ggsave("wykres_3.png", wykres, width = 8, height = 8.5, dpi = 300)