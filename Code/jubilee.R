# some additional charts introduced for the 2024 GV (20 years of statistics)

# prep work ---------------------------------------------------------------

# all active members since 2004
actives2004plus <- stamm |>
  filter(year >= 2004 & status == "a") |>
  distinct(ID, sex, status, n_years, Vorname, since) |>
  arrange(desc(n_years)) 

# charts ------------------------------------------------------------------

# waterfall chart with cumulated trainings for all active members since 2004
jub1 <- actives2004plus |>
  left_join(cumvisits, by = "ID", keep = TRUE) |>
  select(Vorname, n) |>
  arrange(desc(n)) |>
  waterfall(draw_lines = FALSE,
            rect_width = .9,
            rect_border = NA,
            fill_colours = rep(col_a, nrow(actives2004plus)),
            fill_by_sign = FALSE) +
    mytheme +
    ggtitle("Kumulierte Teilnahmen der Aktivmitglieder",
            subtitle = "alle Aktivmitglieder seit 2004")

# column chart with years of membership for all active members sind 2004
jub2 <- actives2004plus |>
  arrange(since) |>
  mutate(Vorname = factor(Vorname, levels = Vorname)) |>
  ggplot() +
    aes(x = Vorname, y = n_years) +
    geom_col(fill = col_a) +
    geom_text(aes(x = Vorname, y = n_years, label = since),
              hjust = 1, angle = 90, colour= "darkgrey") +
    mytheme +
    ggtitle("Aktivmitgliedschaftsdauer in Jahren (inkl. Beitrittsjahr)",
            subtitle = "alle Aktivmitglieder seit 2004") +
    scale_y_continuous(limits = c(0,maxact),
                       breaks = seq(0, maxact,10),
                       minor_breaks = seq(0, maxact, 2))


# weekly visits over the years
# it is a bit far-fetched to interpret anything here
jub3 <- stats |>
  mutate(train = ifelse(type == "Training", 1, 0)) |>
  filter(train == 1) |>
  group_by(year, week, train) |>
  summarise(presence = sum(presence), .groups  = "drop") |>
  na.omit() |>
  ggplot() +
    geom_line(mapping = aes(x = week, y = presence, color = as.factor(year))) +
    xlab(label = "Woche") +
    scale_y_continuous(limits = c(0,lim_upper),
                       breaks = seq(0,lim_upper,2),
                       expand = c(0,0)) +
    scale_x_continuous(limits = c(1,52),
                       breaks = c(1,10, 20, 30, 40, 50),
                       minor_breaks = seq(2,52,2),
                       expand = c(0,0)) +
    mytheme +
    theme(legend.title = element_blank(),
          legend.position = "inside",
          legend.position.inside = c(.2,.95),
          legend.direction = "horizontal",
          panel.grid.major.x = element_line(),
          panel.grid.minor.x = element_line(),
          axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5),
          axis.title.x = element_text(vjust = 6, hjust = .5))

grouped_data <- stats |>
  mutate(train = ifelse(type == "Training", 1, 0)) |>
  filter(train == 1) |>
  group_by(year, week, train) |>
  summarise(presence = sum(presence), .groups  = "drop") |>
  group_by(week, train) |>
  summarise(presence = sum(presence), trainings = sum(train), .groups  = "drop") |>
  mutate(presence = presence/trainings) |>
  filter(trainings > 5) |>
  na.omit()

jub3b <- jub3 +
  theme(legend.position = "none") +
  scale_colour_manual(values = rep("lightgrey", maxyear - 2004 + 1))
jub3c <- jub3b +
  geom_line(data = grouped_data, mapping = aes(x = week, y = presence)) +
  geom_smooth(data = grouped_data, mapping = aes(x = week, y = presence),
              method = "loess", se = TRUE, level = .9) +
  ggtitle("Mittlere und geglättete Besucherzahl pro Kalenderwoche", subtitle = "seit 2004")
jub3d <- jub3c +
  scale_y_continuous(limits = c(8,13),
                     breaks = seq(1,13,1),
                     expand = c(0,0))

# longest streak (with interrupted y scale :-( )
# only active members from stamm relevant
jub4stats <- stats |> 
  filter(ID %in% stamm$ID, 
         type != "Ferien") |>
  arrange(ID, year, week) |>
  mutate(lags = if_else(lag(presence, default = 0) == 1, 0, 1)) |>
  mutate(lags_prevyear = if_else(year != maxyear, lags, 1)) |> 
  mutate(group = cumsum(lags)) |>
  group_by(ID, group) |>
  mutate(streak = sum(presence)) |>
  ungroup() |> 
  mutate(group_prevyear = cumsum(lags_prevyear)) |>
  group_by(ID, group_prevyear) |> 
  mutate(streak_prevyear = sum(presence)) |>
  group_by(ID) |> 
  summarise(maxstreak = max(streak), maxstreak_prevyear = max(streak_prevyear)) |> 
  mutate(plus = maxstreak - maxstreak_prevyear) |>
  mutate(plus = ifelse(plus == 0, "", paste0("+", plus))) |> 
  left_join(stamm |> 
              select(c(ID, Nachname, Vorname, n_years)) |> 
              distinct(ID, .keep_all = TRUE), by = "ID") |>
  filter(n_years > 0,
         maxstreak > 0) |> 
  arrange(desc(maxstreak)) |> 
  # for correct re-order in chart, factor has to be recreated
  mutate(Vorname = factor(Vorname, levels = Vorname))

# some helper variables to determine senseful limits for the chart
upper <- ((jub4stats |> select(maxstreak) |> max()) %/% 10 + 1) * 10
lower <- ((jub4stats |> select(maxstreak) |> max()) %/% 10) * 10
upper2 <- ((jub4stats |> select(maxstreak) |> arrange(desc(maxstreak)) |> slice(2) |> pull()) %/% 10 + 1) * 10

jub4 <- ggplot(jub4stats) +
  aes(x = Vorname, y = maxstreak) +
  geom_col(fill = col_a) +
  mytheme +
  ggtitle("Longest streak sowie allfällige Erhöhung seit letztem Jahr",
          subtitle = "alle Aktivmitglieder seit 2004") +
  scale_y_continuous(limits = c(0,upper),
                     breaks = seq(0, upper,10),
                     minor_breaks = seq(0, upper, 2)) +
  scale_y_break(breaks = c(upper2, lower), expand = expansion(mult = c(.02, .02))) +
  geom_text(aes(x = Vorname, y = maxstreak, label = plus),
            hjust = 0.5, vjust = 1.2, angle = 0, colour= "green", size = 3)
