# Libraries ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringi)


# Global variables ----
text_colour <- c("black")


# Loading and processing data ----
## Populacja ---- 
populacja_bdl <- read.csv("Populacja.csv", sep = ";")

# Exctracting useful columns
populacja_bdl <- populacja_bdl %>%
  select(Nazwa, ends_with("..osoba."))

# Making the columns names more accessible
colnames(populacja_bdl) <- stri_replace_last_regex(colnames(populacja_bdl),
                                                   pattern = "\\.\\.osoba\\.$",
                                                   replacement = "") |>
  stri_replace_all_regex(pattern = "^ogółem\\.ogółem\\.",
                         replacement = "")
# Some preprocessing
populacja_bdl <- populacja_bdl %>%
  pivot_longer(cols = -Nazwa, names_to = "Rok", values_to = "Populacja") %>%
  mutate(Nazwa = as.factor(Nazwa),
         Rok = as.integer(Rok)) %>%
  arrange(desc(Populacja)) %>%
  mutate(Nazwa = fct_inorder(Nazwa))


## Sposoby samobojstwa bdl ----
sposob_samobojstwa_bdl <- read.csv("Sposoby_samobojstwa_bdl.csv", sep = ";")

# Extracting useful columns
sposob_samobojstwa_bdl <- sposob_samobojstwa_bdl %>%
  select(Nazwa, ends_with("..osoba."))

# Making the columns names more accessible
colnames(sposob_samobojstwa_bdl) <- colnames(sposob_samobojstwa_bdl) |>
  stri_replace_last_regex(pattern = "\\.\\.osoba\\.$",
                          replacement = "") |>
  stri_replace_all_regex(pattern = "\\.",
                         replacement = "_") |>
  stri_replace_all_regex(pattern = "_+",
                         replacement = "_") |>
  stri_replace_first_regex(pattern = "^zakończone_zgonem_",
                           replacement = "zakończoneZgonem_") |>
  stri_replace_last_regex(pattern = "_",
                          replacement = "-") |>
  stri_replace_first_regex(pattern = "_",
                           replacement = "-")

# Changning the format into one more usable for graphs
sposob_samobojstwa_bdl <- sposob_samobojstwa_bdl %>%
  pivot_longer(cols = -Nazwa, names_to = "type", values_to = "Cases") %>%
  separate(type, sep = "-", into = c("Kategoria", "Typ", "Rok")) %>%
  mutate(Nazwa = fct_relevel(as.factor(Nazwa), levels(populacja_bdl$Nazwa)),
         Kategoria = as.factor(Kategoria),
         Rok = as.integer(Rok))
remove(order_of_types)


# Final graph - Percentage of suicide attempts committed by ingesting drugs ----


temp1 <- sposob_samobojstwa_bdl %>%
  filter(Kategoria == "ogółem",
         Typ %in% c("zażycie_środków_nasennych_leków_psychotropowych",
                    "zażycie_innych_leków")) %>%
  group_by(Nazwa, Rok) %>%
  summarise(Leki = sum(Cases), .groups = "drop")

temp2 <- sposob_samobojstwa_bdl %>%
  filter(Kategoria == "ogółem",
         Typ == "ogółem") %>%
  group_by(Nazwa, Rok) %>%
  summarise(Ogolem = sum(Cases), .groups = "drop")

work_df <- inner_join(temp1, temp2,
                      by = c("Nazwa", "Rok")) %>%
  mutate(Procent = 100 * Leki / Ogolem, .keep = "unused") %>%
  filter(Nazwa != "POLSKA")

interesujace_nazwy <- c(
  "ŁÓDZKIE", # high
  "POMORSKIE", # big change
  "WIELKOPOLSKIE" # low
)

# Interesting names
work_df_int <- work_df %>%
  filter(Nazwa %in% interesujace_nazwy) %>%
  mutate(Nazwa = factor(Nazwa, levels = interesujace_nazwy))

# Other 
work_df_other <- work_df %>%
  filter(!(Nazwa %in% interesujace_nazwy))

remove(temp1, temp2)

# The graph
graph_final <- ggplot() +
  geom_line(data = work_df_other,
            aes(x = Rok, y = Procent, fill = Nazwa),
            linewidth = 0.6,
            colour = "gray50",
            alpha = 0.5) +
  geom_point(data = work_df_other,
             aes(x = Rok, y = Procent),
             size = 2,
             colour = "gray50",
             alpha = 0.5) +
  geom_line(data = work_df_int,
            aes(x = Rok, y = Procent, colour = Nazwa),
            linewidth = 0.6) +
  geom_point(data = work_df_int,
             aes(x = Rok, y = Procent, colour = Nazwa),
             size = 2, shape = 15) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.05)),
                     breaks = 2018:2025) +
  scale_y_continuous(limits = c(0, 50),
                     expand = expansion(mult = c(0, 0)),
                     labels = c(" 0%", paste(c(seq(10, 50, 10)), "%", sep = "")),
                     breaks = seq(0, 50, 10)) +
  scale_colour_viridis_d(name = "Wybrane województwa",
                        labels = stri_trans_totitle(interesujace_nazwy),
                        option = "D") +
  labs(title = "Procent prób samobójczych dokonanych poprzez zażycie leków",
       subtitle = "Z podziałem na województwa w latach 2018-25",
       y = "Procent prób z użyciem leków",
       x = "Rok") +
  theme_void() +
  theme(panel.grid.major.y = element_line(colour = "gray50",
                                        linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(fill = "transparent",
                                    colour = NA),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA),
        plot.background = element_rect(fill = "transparent",
                                       colour = NA),
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(margin = margin(t = 5, b = 5, l = 0, r = 0)),
        text = element_text(colour = text_colour),
        axis.text.y = element_text(colour = text_colour,
                                   hjust = -0.3),
        axis.text.x = element_text(colour = text_colour,
                                   vjust = -1),
        axis.line = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent",
                                             colour = NA),
        legend.key = element_rect(fill = "transparent"),
        legend.position = "bottom",
        aspect.ratio = 0.3
        )

# Saving  
ggsave(plot = graph_final,
       filename = "Final_graph.png",
       bg = "transparent",
       width = 8,
       height = 3,
       dpi = 500)
