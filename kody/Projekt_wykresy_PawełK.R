# install.packages(c("tidyverse", "scales"))
library(tidyverse)
library(scales)

# Dane oparte na wskaźnikach z raportu ESPAD 2024 (odsetek młodzieży, która kiedykolwiek w życiu zetknęła się z daną substancją)
espad_2024 <- tibble(
  substancja = c(
    "Alkohol", 
    "E-papierosy / Vaping", 
    "Tradycyjne papierosy", 
    "Marihuana / Haszysz", 
    "Leki uspokajające (bez recepty)", 
    "Nowe Substancje (NSP / Mefedron)", 
    "Amfetamina", 
    "Kokaina", 
    "LSD / Psychodeliki"
  ),
  odsetek_zastosowania = c(82.5, 64.2, 41.0, 28.5, 14.2, 7.8, 4.5, 3.2, 3.0)
) %>%
  mutate(substancja = fct_reorder(substancja, odsetek_zastosowania))

# Generowanie wykresu
wykres_espad <- ggplot(espad_2024, aes(x = odsetek_zastosowania, y = substancja, fill = odsetek_zastosowania)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(odsetek_zastosowania, "%")),
    hjust = -0.15, size = 3.8, fontface = "bold", color = "grey20"
  ) +
  scale_x_continuous(
    limits = c(0, 95),
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_viridis_c(option = "inferno", begin = 0.2, end = 0.8, direction = -1) +
  labs(
    title = "Eksperymentowanie z substancjami psychoaktywnymi wśród młodzieży",
    subtitle = "Na podstawie ogólnopolskiego badania ESPAD 2024 (odsetek osób 'kontakt w życiu')",
    x = "Odsetek badanych uczniów (%)",
    y = NULL,
    caption = "Źródło: Raport ESPAD 2024 Polska (KCPU)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey30", margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 11, color = "grey15")
  )

print(wykres_espad)

library(tidyverse)
library(scales)

# Przygotowanie danych ESPAD 2024 z podziałem na płeć
espad_plec <- tibble(
  substancja = rep(c("Alkohol", "E-papierosy", "Marihuana", "Leki uspokajające", "NSP / Mefedron", "Amfetamina"), each = 2),
  plec = rep(c("Chłopcy", "Dziewczęta"), times = 6),
  odsetek = c(
    81.2, 83.8,  # Alkohol
    58.5, 69.8,  # E-papierosy
    31.2, 25.8,  # Marihuana
    10.5, 17.8,  # Leki uspokajające
    8.9,  6.7,   # NSP / Mefedron
    5.2,  3.8    # Amfetamina
  )
) %>%
  mutate(
    # Uporządkowanie substancji według łącznego odsetka
    substancja = fct_reorder(substancja, odsetek, .fun = max)
  )

# Generowanie grupowanego wykresu słupkowego
wykres_plec <- ggplot(espad_plec, aes(x = odsetek, y = substancja, fill = plec)) +
  geom_col(position = "dodge", width = 0.7, color = "white") +
  geom_text(
    aes(label = paste0(odsetek, "%")),
    position = position_dodge(width = 0.7),
    hjust = -0.15, size = 3.5, fontface = "bold", color = "grey20"
  ) +
  scale_x_continuous(
    limits = c(0, 95),
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(values = c("Chłopcy" = "#2b5c8f", "Dziewczęta" = "#c73e6d"), name = "Płeć ucznia") +
  labs(
    title = "Używanie substancji psychoaktywnych wg płci (ESPAD 2024)",
    subtitle = "Porównanie odsetka nastolatków z podziałem na płci (tak, są 2)",
    x = "Odsetek badanych (%)",
    y = NULL,
    caption = "Źródło: Opracowanie na podstawie raportu ESPAD 2024 Polska (KCPU)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey30", margin = margin(b = 12)),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face = "bold", size = 11, color = "grey15"),
    legend.position = "top"
  )

print(wykres_plec)

# Zapis do pliku PNG w wysokiej rozdzielczości:
 ggsave("espad_2024_zastosowanie.png", plot = wykres_espad, width = 8, height = 6, dpi = 300, bg = "white")

 
 #Poprawa po kamieniach milowych 
library(tidyverse)
library(scales)
library(showtext)
 # =========================================================
 # 1. POŁĄCZONY ZBIÓR DANYCH (Ogółem + podział na płeć)
 # =========================================================
 # 2. Dane
 espad_razem <- tibble(
   substancja = c(
     "Alkohol",
     "E-papierosy / Vaping",
     "Tradycyjne papierosy",
     "Marihuana / Haszysz",
     "Leki uspokajające",
     "Dopalacze (NSP / Mefedron)",
     "Amfetamina",
     "Kokaina",
     "LSD / Psychodeliki"
   ),
   ogolem     = c(82.5, 64.2, 41.0, 28.5, 14.2, 7.8, 4.5, 3.2, 3.0),
   chlopcy    = c(81.2, 58.5, 40.4, 31.2, 10.5, 8.9, 5.2, 3.9, 3.6),
   dziewczeta = c(83.8, 69.8, 41.6, 25.8, 17.8, 6.7, 3.8, 2.5, 2.4)
 ) %>%
   mutate(
     substancja = fct_reorder(substancja, ogolem),
     max_x = pmax(ogolem, chlopcy, dziewczeta),
     etykieta = sprintf("%.1f%% (Ch %.1f | Dz %.1f)", ogolem, chlopcy, dziewczeta) %>%
       str_replace_all("\\.", ",")
   )
 
 espad_punkty <- espad_razem %>%
   select(substancja, Chłopcy = chlopcy, Dziewczęta = dziewczeta) %>%
   pivot_longer(
     cols = c(Chłopcy, Dziewczęta),
     names_to = "plec",
     values_to = "odsetek"
   )
 
 # 3. Wykres z pełną przezroczystością i kontrastem pod miętowe tło
 wykres_espad <- ggplot(espad_razem, aes(y = substancja)) +
   geom_col(aes(x = ogolem, fill = "Ogółem"), width = 0.68) +
   geom_segment(
     aes(x = chlopcy, xend = dziewczeta, yend = substancja),
     color = "#1e293b", linewidth = 1.0
   ) +
   geom_point(
     data = espad_punkty,
     aes(x = odsetek, color = plec),
     size = 3.2
   ) +
   geom_text(
     aes(x = max_x + 2, label = etykieta),
     family = "lato",
     hjust = 0, size = 3.4, color = "#0f172a"
   ) +
   scale_x_continuous(
     limits = c(0, 130),
     breaks = seq(0, 100, by = 20),
     labels = label_percent(scale = 1),
     expand = expansion(mult = c(0, 0.01))
   ) +
   # Półprzezroczysty grafit naturalnie zgrywa się z miętowym tłem plakatu
   scale_fill_manual(values = c("Ogółem" = alpha("#0f172a", 0.18)), name = NULL) +
   scale_color_manual(
     values = c("Chłopcy" = "#1d4ed8", "Dziewczęta" = "#be185d"),
     name = NULL
   ) +
   labs(
     title = "Używanie substancji psychoaktywnych wśród młodzieży (ESPAD 2024)",
     x = "Odsetek badanych uczniów (%)",
     y = NULL
   ) +
   theme_minimal(base_size = 12, base_family = "lato") +
   theme(
     # PEŁNA PRZEZROCZYSTOŚĆ WSZYSTKICH ELEMENTÓW TŁA
     plot.background       = element_rect(fill = "transparent", color = NA),
     panel.background      = element_rect(fill = "transparent", color = NA),
     legend.background     = element_rect(fill = "transparent", color = NA),
     legend.box.background = element_rect(fill = "transparent", color = NA),
     legend.key            = element_rect(fill = "transparent", color = NA),
     
     plot.title.position = "plot",
     plot.caption.position = "plot",
     plot.margin = margin(t = 4, r = 6, b = 4, l = 4),
     
     plot.title = element_text(
       face = "bold", size = 14, color = "#0f172a", margin = margin(b = 3)
     ),
     plot.subtitle = element_text(
       size = 10.5, color = "#1e293b", margin = margin(b = 6)
     ),
     plot.caption = element_text(
       size = 8.5, color = "#334155", hjust = 0, margin = margin(t = 6)
     ),
     
     axis.title.x = element_text(size = 10, color = "#0f172a", margin = margin(t = 5)),
     axis.text.x = element_text(size = 9.5, color = "#1e293b"),
     axis.text.y = element_text(face = "bold", size = 10.5, color = "#0f172a", margin = margin(r = 4)),
     
     legend.text = element_text(size = 10, color = "#0f172a"),
     legend.position = "top",
     legend.justification = "left",
     legend.box = "horizontal",
     legend.margin = margin(t = 0, b = 2),
     legend.key.size = unit(0.45, "cm"),
     
     panel.grid.major.y = element_blank(),
     panel.grid.minor = element_blank(),
     panel.grid.major.x = element_line(color = alpha("#0f172a", 0.14), linewidth = 0.5)
   )
 
 # 4. Zapis jako przezroczysty PNG
 ggsave(
   "espad_2024_zastosowanie.png",
   plot = wykres_espad,
   width = 8.2,
   height = 4.3,
   dpi = 300,
   bg = "transparent"
 )