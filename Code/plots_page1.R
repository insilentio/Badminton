# plots first page
# data prep

present <- stats |>
  left_join(stamm, by = c("ID", "year")) |>
  mutate(cat = case_when(status == "a" ~ "Aktive",
                         status == "p" ~ "Passive",
                         status == "g" ~ "Gäste",
                         ID == "Gäste div." ~ "Gäste",
                         ID == "Passive div." ~ "Passive")) |>
  group_by(year, cat) |> 
  summarise(presence = sum(presence), .groups = "drop")

totals <- stats |>
  filter(type == "Training") |>
  group_by(year, week) |>
  summarise(presence = sum(presence), .groups = "drop_last") |>
  summarise(tot = sum(presence), mean = mean(presence), .groups = "drop")

present <- present |>
  left_join(totals, by = "year")

#plots
#try to replicate excel table. Not so easy, therefore for the moment switched to a solution
#where excel table must be imported as a PNG
# stats |>
#   filter(year == maxyear) |>
#   mutate(presence = ifelse(presence == 0, NA, presence),
#          presence = ifelse(type == "Ferien", "F", presence)) |>
#   left_join(stamm, by = c("ID", "year")) |>
#   select(ID, Vorname, Nachname, week, presence) |>
#   pivot_wider(id_cols = c(1:4), names_from = week, values_from = presence)

# pdf_render_page("Data/BCT_Excel.pdf", dpi = 300) |>
#   writePNG("Data/BCT_Excel.png")

c1 <- ggdraw() +
  draw_image("Data/BCT_Excel.png")

c2 <- ggplot(kpi) +
    aes(y = idx) +
    geom_text(aes(x = .2, label = values), size = 4, hjust = 1) +
    geom_text(aes(x = .25, label = desc), size = 4, hjust = 0) +
    scale_y_continuous(limits = c(0, 11)) +
    scale_x_continuous(limits = c(0, 1)) +
  mytheme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        axis.text.x = element_blank()) +
  labs(title = "Jahres-KPIs")

# stacked bar chart with particpation over year plus rolling average
rolling <- stats |> 
  filter(year == maxyear, type == "Training") |>
  mutate(train = ifelse(type == "Training", 1, 0)) |>
  group_by(week, train) |>
  summarise(presence = sum(presence), .groups  = "drop") |>
  mutate(cum = cumsum(presence), roll = cum/cumsum(train)) |>
  mutate(presence = ifelse(presence == 0, NA, presence)) |>
  select(week, presence, roll) |> 
  pivot_longer(cols = c("presence", "roll"), names_to = "status", values_to = "value") |>
  na.omit()

lim_upper <- max(rolling$value)
c3 <- stats |> 
  left_join(stamm |> select(ID, year, status), by = c("ID", "year")) |> 
  filter(year == maxyear, type == "Training") |>
  mutate(status = ifelse(ID == "Gäste div.", "g", status)) |> 
  mutate(status = ifelse(ID == "Passive div.", "p", status)) |> 
  mutate(status = factor(status, c("g", "p", "a"), c("Gäste", "Passive", "Aktive"))) |> 
  group_by(week, status) |>
  summarise(value = sum(presence), .groups  = "drop") |>
  mutate(value = ifelse(value == 0, NA, value)) |>
  na.omit() |> 
  ggplot() +
    geom_col(aes(x = week, y = value, fill = status)) +
    geom_line(data = rolling |> filter(status == "roll"),
            mapping = aes(x = week, y = value, colour = status)) +
    xlab(label = "Woche") +
    scale_y_continuous(limits = c(0, lim_upper),
                       breaks = c(seq(0, lim_upper, 2), lim_upper),
                       minor_breaks = seq(1, lim_upper),
                       expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 52),
                       breaks = c(1, 10, 20, 30, 40, 50),
                       minor_breaks = seq(2, 52, 2),
                       expand = c(0, 0)) +
    scale_fill_manual(values = c("#4EAD5B", "#2F6EBA")) +
    scale_colour_manual(values = c("#B02418")) +
    mytheme +
    theme(legend.position = "none",
          #legend.title = element_blank(),
          # legend.position = "inside",
          # legend.position.inside = c(.5,1),
          # legend.direction = "horizontal",
          panel.grid.major.x = element_line(),
          panel.grid.minor.x = element_line(),
          axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5),
          axis.title.x = element_text(vjust = 6, hjust = .5),
          plot.margin = unit(c(.3, .3, 0, .3), "cm"))+
  geom_hline(yintercept = 16)

# without unpersonal values
c4 <- stats |>
  filter(!ID %in% c("Gäste div.", "Passive div.")) |>
  filter(year %in% c(maxyear, maxyear-1, maxyear-2, maxyear-3)) |>
  group_by(year, ID) |>
  summarise(presence = sum(presence), .groups = "drop") |>
  ggplot() +
    aes(x = as.factor(year), y = presence) +
    geom_boxplot(width = .5)  +
    stat_summary(fun = mean, geom = "point", size = 1, shape = 3, colour = "steelblue", show.legend = TRUE) +
    mytheme +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    scale_y_continuous(limits = c(0,40),
                       breaks = seq(0,40,10),
                       minor_breaks = seq(0,40,2)) +
    labs(title = "Teilnahmehäufigkeit")

scaler = 30
c5 <- ggplot(present) +
  aes(x = year, y = presence, fill = cat) +
  geom_col() +
  geom_text(aes(label = presence),
            position = position_stack(vjust = .5),
            size = 2) +
  geom_text(aes(x = year, y = tot, label = tot),
            nudge_y = 10,
            inherit.aes = FALSE,
            size = 2) +
  geom_line(aes(y = mean*scaler), color = "pink") +
  labs(fill = "Kategorie", y = "Anz. Teilnehmer") +
  scale_y_continuous(limits = c(0, 700),
                     breaks = seq(0, 700, 100),
                     minor_breaks = seq(0, 700, 25),
                     sec.axis = sec_axis(transform = ~ .x/scaler)) +
  scale_x_continuous(breaks = c(minyear:maxyear)) +
  scale_fill_manual(values = c("steelblue", "limegreen", "darkred")) +
  labs(title = "Teilnehmerentwicklung nach Kategorien (absolut)") +
  mytheme +
  theme(axis.title.y = element_text(),
        legend.position = "none")


c6 <- ggplot(present) +
  aes(x = year, y = presence, fill = cat) +
  geom_col(position = position_fill())  +
  geom_text(aes(label = round(presence/tot, 2)),
            position = position_fill(vjust = .5),
            size = 2) +
  labs(fill = "Kategorie", y = "Ant. Teilnehmer") +
  scale_x_continuous(breaks = c(minyear:maxyear)) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("steelblue", "limegreen", "darkred")) +
  labs(title = "Teilnehmerentwicklung nach Kategorien (relativ)") +
  mytheme

leg <- get_legend(c6 + theme(legend.title = element_blank(),
                             legend.key.size = unit(c(.3), units = "cm"),
                             legend.text = element_text(size = 8),
                             legend.direction = "vertical",
                             legend.background = element_rect(colour = "darkgrey")))

c6 <- c6 + theme(axis.title.y = element_text(),
                 legend.position = "none")
