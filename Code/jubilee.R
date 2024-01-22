# some additional charts for the 2024 GV (20 years of statistics)


# prep work ---------------------------------------------------------------

# all active members since 2004
actives2004plus <- stamm %>%
  filter(year >= 2004 & status == "a") %>%
  distinct(ID, sex, status, n_years, Vorname, since) %>%
  arrange(desc(n_years)) %>%
  mutate(ID = factor(ID, levels = ID), Vorname = paste0(Vorname, " ", substr(ID,1,1), "."))

# all active members ever (currently not used)
actives4ever <- stamm %>%
  filter(status == "a") %>%
  distinct(ID, sex, status, n_years, Vorname, since) %>%
  arrange(desc(n_years)) %>%
  mutate(ID = factor(ID, levels = ID), Vorname = paste0(Vorname, " ", substr(ID,1,1), "."), since = as.numeric(since)) %>%
  add_row(tibble(ID = NA, sex = NA, status = NA, n_years = NA, Vorname = NA, since = (c(1980:maxyear+1))))


# charts ------------------------------------------------------------------

# waterfall chart with cumulated trainings for all active members since 2004
jub1 <- actives2004plus %>%
  left_join(cumvisits, by = "ID", keep = TRUE) %>%
  select(Vorname, n) %>%
  arrange(desc(n)) %>%
  waterfall(draw_lines = FALSE,
            rect_width = .9,
            rect_border = NA,
            fill_colours = rep("steelblue", nrow(actives2004plus)),
            fill_by_sign = FALSE) +
  mytheme +
  ggtitle("Kumulierte Teilnahmen der Aktivmitglieder seit 2004")

# column chart with years of membership for all active members sind 2004
jub2 <- actives2004plus %>%
  arrange(since) %>%
  mutate(Vorname = factor(Vorname, levels = Vorname)) %>%
  ggplot() +
    aes(x = Vorname, y = n_years) +
    geom_col(fill = "steelblue") +
    geom_text(aes(x = Vorname, y = n_years, label = since),
              hjust = 1, angle = 90, colour= "darkgrey") +
    mytheme +
    ggtitle("Mitgliedschaftsdauer in Jahren (inkl. Beitrittsjahr)") +
    scale_y_continuous(limits = c(0,maxact),
                       breaks = seq(0, maxact,10),
                       minor_breaks = seq(0, maxact, 2))


# weekly visits over the years
jub3 <- stats %>%
  mutate(train = ifelse(type == "Training", 1, 0)) %>%
  filter(train == 1) %>%
  group_by(year, week, train) %>%
  summarise(presence = sum(presence), .groups  = "drop") %>%
  group_by(week, train) %>%
  summarise(presence = sum(presence), trainings = sum(train), .groups  = "drop") %>%
  mutate(meanrel = presence/trainings, meanabs = presence/(maxyear-2004+1)) %>%
  pivot_longer(cols = c("meanrel", "meanabs"), names_to = "type", values_to = "value") %>%
  na.omit() %>%
  ggplot() +
  aes(x = week, y = value, color = type) +
  geom_line() +
  geom_smooth(data = . %>% filter(type == "meanrel"), method = "loess", se = FALSE) +
  xlab(label = "Woche") +
  scale_y_continuous(limits = c(0,18),
                     breaks = seq(0,18,2),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(1,52),
                     breaks = c(1,10, 20, 30, 40, 50),
                     minor_breaks = seq(2,52,2),
                     expand = c(0,0)) +
  scale_colour_manual(labels = c("ø Besucher pro Woche", "ø Besucher pro Trainingswoche"),
                      values = c("purple", "red")) +
  mytheme +
  theme(legend.title = element_blank(),
        legend.position = c(.2,.95),
        legend.direction = "horizontal",
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5),
        axis.title.x = element_text(vjust = 6, hjust = .5),
        plot.margin = unit(c(.3, .3, 0, .3), "cm"))

# some informative values -------------------------------------------------
# mean visits by sex
jub4 <- stats %>%
  group_by(ID, year) %>%
  summarise(visits = sum(presence), .groups = "drop_last") %>%
  inner_join(actives, by = "ID", keep = TRUE, suffix = c(".x", "")) %>%
  group_by(sex,) %>%
  summarize(meanvisits = mean(visits)) %>%
  print(n=Inf)

# top dates regarding nr. of visits
jub5 <- stats %>%
  group_by(year, week) %>%
  summarize(nr = sum(presence)) %>%
  arrange(desc(nr)) %>%
  print(n = 40)

