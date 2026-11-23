library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)

dane <- data.frame(
  Wojewodztwo = c("DOLNOŚLĄSKIE", "KUJAWSKO-POMORSKIE", "LUBELSKIE", "LUBUSKIE",
                  "ŁÓDZKIE", "MAŁOPOLSKIE", "MAZOWIECKIE", "OPOLSKIE",
                  "PODKARPACKIE", "PODLASKIE", "POMORSKIE", "ŚLĄSKIE",
                  "ŚWIĘTOKRZYSKIE", "WARMIŃSKO-MAZURSKIE", "WIELKOPOLSKIE", "ZACHODNIOPOMORSKIE"),
  iso_3166_2 = c("PL-DS", "PL-KP", "PL-LU", "PL-LB",
                 "PL-LD", "PL-MA", "PL-MZ", "PL-OP",
                 "PL-PK", "PL-PD", "PL-PM", "PL-SL",
                 "PL-SK", "PL-WN", "PL-WP", "PL-ZP"),
  Wydatki_PLN = c(95643905.34, 64674637.42, 51266190.35, 34578481.06,
                  64749111.67, 113868644.41, 180963367.38, 31148128.46,
                  52018823.73, 36878631.62, 89896896.28, 142745044.56,
                  34276618.46, 43777106.92, 125665559.39, 68803454.88),
  Ludnosc = c(2888000, 2006000, 2024000, 979000,
              2378000, 3430000, 5510000, 942000,
              2085000, 1143000, 2358000, 4346000,
              1178000, 1366000, 3496000, 1638000)
)

dane$Per_Capita <- dane$Wydatki_PLN / dane$Ludnosc

dane$Kategoria <- cut(
  dane$Per_Capita,
  breaks = c(-Inf, 27, 31, 34, 38, Inf),
  labels = c("do 27", "27–31", "31–34", "34–38", "powyżej 38"),
  right = TRUE
)

dane$Kategoria <- factor(
  dane$Kategoria,
  levels = c("powyżej 38", "34–38", "31–34", "27–31", "do 27")
)

polska_mapa <- ne_states(country = "poland", returnclass = "sf")
mapa_dane <- polska_mapa %>% left_join(dane, by = "iso_3166_2")

wykres_mapa <- ggplot(data = mapa_dane) +
  geom_sf(aes(fill = Kategoria), color = "#2c3e50", linewidth = 0.35) +
  scale_fill_manual(
    values = c(
      "powyżej 38" = "#080c80",
      "34–38"      = "#3545a3",
      "31–34"      = "#5e76bb",
      "27–31"      = "#88a8d4",
      "do 27"      = "#b2d8e8"
    ),
    name = "Wydatki na 1\nmieszkańca (zł)",
    drop = FALSE
  ) +
  guides(fill = guide_legend(override.aes = list(color = "white", linewidth = 0.5))) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.key.size = unit(0.6, "cm"),
    legend.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 11, color = "black", margin = margin(b = 6)),
    plot.title = element_text(hjust = 0, face = "bold", size = 16, color = "black", margin = margin(b = 14)),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(
    title = "Wydatki gmin na walkę z uzależnieniami w przeliczeniu\nna 1 mieszkańca w województwach (2023 r.)"
  )

print(wykres_mapa)
ggsave("mapa_per_capita.png", plot = wykres_mapa, width = 8.5, height = 6.8, dpi = 300, bg = "transparent")

dane_trend <- data.frame(
  Rok = c(2022, 2023, 2024, 2025),
  Wydatki_PLN = c(1121328321.95, 1230954601.93, 1296396604.12, 1339671045.91)
)

dane_trend$Wydatki_mld <- dane_trend$Wydatki_PLN / 1e9
dane_trend$Etykieta <- paste0(gsub("\\.", ",", sprintf("%.2f", dane_trend$Wydatki_mld)), " mld zł")

wykres_trend <- ggplot(dane_trend, aes(x = Rok, y = Wydatki_mld)) +
  geom_area(fill = "#5e76bb", alpha = 0.30) +
  geom_line(color = "#080c80", linewidth = 1.6) +
  geom_point(color = "#080c80", size = 4.2) +
  geom_text(aes(label = Etykieta), vjust = -1.35, size = 4, fontface = "bold", color = "black") +
  scale_x_continuous(breaks = 2022:2025, expand = expansion(mult = c(0.12, 0.12))) +
  scale_y_continuous(limits = c(0, 1.62), breaks = seq(0, 1.5, by = 0.5), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold", size = 16, color = "black", margin = margin(b = 18)),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold", color = "black", margin = margin(t = 6)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#2c3e5080", linewidth = 0.6, linetype = "dashed"),
    plot.margin = margin(15, 25, 15, 25)
  ) +
  labs(
    title = "Wydatki gmin w Polsce na przeciwdziałanie uzależnieniom\nw latach 2022–2025 (wspólne programy gminne)"
  )

print(wykres_trend)
ggsave("wykres_dynamika_wydatkow.png", plot = wykres_trend, width = 8, height = 5, dpi = 300, bg = "transparent")