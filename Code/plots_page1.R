# plots first page
# data prep

present <- stats %>%
  left_join(stamm, by = c("ID", "year")) %>%
  mutate(cat = case_when(status == "a" ~ "Aktive",
                         status == "p" ~ "Passive",
                         status == "g" ~ "Gäste",
                         ID == "Gäste div." ~ "Gäste",
                         ID == "Passive div." ~ "Passive")) %>%
  group_by(year, cat) %>% 
  summarise(presence = sum(presence))

totals <- stats %>%
  filter(type == "Training") %>%
  group_by(year, week) %>%
  summarise(presence = sum(presence), .groups = "drop_last") %>%
  summarise(tot = sum(presence), mean = mean(presence))

present <- present %>%
  left_join(totals, by = "year")


#plots
stats %>%
  filter(year == maxyear) %>%
  mutate(presence = ifelse(presence == 0, NA, presence),
         presence = ifelse(type == "Ferien", "F", presence)) %>%
  left_join(stamm, by = c("ID", "year")) %>%
  select(ID, Vorname, Nachname, week, presence) %>%
  pivot_wider(id_cols = c(1:4), names_from = week, values_from = presence)

c1 <- ggdraw() +
  draw_image("Data/BCT21.png")

c2 <- tableGrob(kpi %>% select(3, 2),
                theme=ttheme_minimal(base_size = 6, padding = unit(c(5,2), "mm"),
                                     core = list(fg_params = list(hjust = c(rep(0, 5), rep(1, 5)), 
                                                                  x = c(rep(0.1, 5), rep(.8, 5))))),
                rows=NULL, cols = NULL)
title <- textGrob("Jahres-KPIs", gp = gpar(col = "darkgrey"))
c2 <- gtable_add_rows(c2,
                        heights = grobHeight(title) + unit(2,"lines"),
                        pos = 0)
c2 <- gtable_add_grob(c2, list(title), 1, 1, 1, ncol(c2))
  
c3 <- stats %>%
  filter(year == maxyear) %>%
  mutate(train = ifelse(type == "Training", 1, 0)) %>%
  group_by(week, train) %>%
  summarise(presence = sum(presence), .groups  = "drop") %>%
  mutate(cum = cumsum(presence), roll = cum/cumsum(train)) %>%
  mutate(presence = ifelse(presence == 0, NA, presence)) %>%
  pivot_longer(cols = c("presence", "roll"), names_to = "type", values_to = "value") %>%
  na.omit() %>%
  ggplot() +
  aes(x = week, y = value, colour = type) +
  geom_line() +
  mytheme +
  theme(legend.title = element_blank(),
        legend.position = c(.2,.92),
        legend.direction = "horizontal",
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5)) +
  scale_y_continuous(limits = c(0,13),
                     breaks = seq(0,14,2)) +
  scale_x_continuous(limits = c(1,52),
                     breaks = seq(0,50,10),
                     minor_breaks = seq(2,52,2)) +
  scale_colour_manual(labels = c("Anz. Besucher", "kum. Mittelwert"),
                      values = c("purple", "pink"))


c4 <- stats %>%
  filter(year %in% c(maxyear, maxyear-1)) %>%
  group_by(year, ID) %>%
  summarise(presence = sum(presence)) %>%
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
  mytheme +
  theme(axis.title.y = element_text(),
        legend.position = "none") +
  labs(fill = "Kategorie", y = "Anz. Teilnehmer") +
  scale_y_continuous(limits = c(0,600),
                     breaks = seq(0,600,100),
                     minor_breaks = seq(0, 600, 25),
                     sec.axis = sec_axis(trans = ~ .x/scaler, name = "Mittelwert")) +
  scale_x_continuous(breaks = c(minyear:maxyear)) +
  scale_fill_manual(values = c("steelblue", "limegreen", "darkred")) +
  labs(title = "Teilnehmerentwicklung nach Kategorien (absolut)")


c6 <- ggplot(present) +
  aes(x = year, y = presence, fill = cat) +
  geom_col(position = position_fill())  +
  geom_text(aes(label = round(presence/tot, 2)),
            position = position_fill(vjust = .5),
            size = 2) +
  mytheme +
  theme(axis.title.y = element_text(),
        legend.position = "none") +
  labs(fill = "Kategorie", y = "Ant. Teilnehmer") +
  scale_x_continuous(breaks = c(minyear:maxyear)) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("steelblue", "limegreen", "darkred")) +
  labs(title = "Teilnehmerentwicklung nach Kategorien (relativ)")

leg <- ggplot(present) +
  aes(x = year, y = presence, fill = cat) +
  geom_col(position = position_fill()) +
  theme(legend.title = element_blank(),
        legend.key.size = unit(c(.3), units = "cm"),
        legend.text = element_text(size = 8)) +
  scale_fill_manual(values = c("steelblue", "limegreen", "darkred"))
leg <- get_legend(leg)

#arrange everything on one page
gridplot1 <- arrangeGrob(c1,
                          arrangeGrob(c2, c3, c4, widths = c(2,6,2)),
                          arrangeGrob(c5, leg, c6, widths = c(4.5, 1, 4.5)),
                          heights = c(5,2,3),
                          layout_matrix = rbind(c(1), c(2), c(3)))
# gridplot1 <- grid.arrange(c1,
#                           arrangeGrob(c2, c3, c4, widths = c(2,6,2)),
#                           arrangeGrob(c5, leg, c6, widths = c(4.5, 1, 4.5)),
#                           heights = c(5,2,3),
#                           layout_matrix = rbind(c(1), c(2), c(3)))
