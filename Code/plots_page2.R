#plots second page
#

#generate the plots for the Teilnehmerstatistik
p1 <- actives |>
  left_join(cumvisits, by = "ID", keep = TRUE) |>
  select(Vorname, n) |>
  waterfall(draw_lines = FALSE,
            rect_width = .9,
            rect_border = NA,
            fill_colours = rep("steelblue", nrow(actives)),
            fill_by_sign = FALSE) +
  mytheme +
  ggtitle("Kumulierte Teilnahmen der Aktivmitglieder seit 2004")

# p2 shows the number of years as active member since the beginning, the year of (first) entering
# active membership and the average visits per year during the years as active member (since 2004 only)
p2 <- actives |>
  left_join(cumvisits, by = "ID") |>
  mutate(avgperyear = n_active/activeYearsSince2004) |> 
ggplot() +
  aes(x = Vorname, y = n_years) +
  geom_col(fill = "steelblue") +
  geom_point(aes(x = Vorname, y = avgperyear), color = "darkblue") +
  geom_text(aes(x = Vorname, y = n_years, label = since),
            hjust = 1, angle = 90, colour= "darkgrey") +
  mytheme +
  ggtitle("Aktivmitgliedschaftsdauer in Jahren, durchschnittliche Besuchsquote und Beitrittsjahr") +
  scale_y_continuous(limits = c(0,maxact),
                     breaks = seq(0, maxact,10),
                     minor_breaks = seq(0, maxact, 2))

p3 <- stats |>
  group_by(ID, year) |>
  summarise(visits = sum(presence), .groups = "drop_last") |>
  inner_join(actives, by = "ID", keep = TRUE, suffix = c(".x", "")) |>
  left_join(nr_trainings, by = "year") |>
  select(ID, visits, Vorname, n) |>
  mutate(visits = visits/n*100) |>
  ggplot() +
    aes(x = Vorname, y = visits) +
    geom_boxplot(outlier.size = 1, outlier.alpha = .5, coef = 100, width = .5) +
    stat_summary(fun = mean, geom = "point", size = 1, shape = 3, colour = "steelblue", show.legend = TRUE) +
    mytheme +
    labs(title = "Persönliche Besuchsbandbreite seit 2004",
         subtitle = "in % der Jahresanzahl Trainings") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,20), minor_breaks = seq(0,100,5))

# ranking per year, only of active members
p4 <- stats |>
  inner_join(stamm, by = c("ID", "year"), keep = TRUE, suffix = c("", ".x")) |>
  group_by(ID, year, status, Vorname) |>
  summarise(visits = sum(presence), .groups = "drop_last") |>
  filter(status == "a") |>
  group_by(year) |>
  mutate(rank = min_rank(desc(visits)), ID = ID, year = year, Vorname = Vorname) |>
  right_join(actives, by = "ID", keep = TRUE, suffix = c("", ".y")) |>
  ggplot() +
    aes(x = Vorname.y, y = rank) +
    geom_boxplot(coef = 100, width = .5) +
    stat_summary(fun = mean, geom = "point", size = 1, shape = 3, colour = "steelblue", show.legend = TRUE) +
    mytheme +
    labs(title = "Persönliche Rankingbandbreite seit 2004",
         subtitle = "") +
    scale_y_continuous(limits =c(1,23), breaks = c(1:23), minor_breaks = c(1:23))


p5a <- ggplot(figs) +
  aes(x = cat, y = 1, label = head) +
  geom_text(size = 3, colour = "darkgrey") +
  theme_void()
p5b <- ggplot(figs) +
  aes(x = cat, y = 1, label = value) +
  geom_text(size = 8) +
  theme_void()
p5c <- ggplot(figs) +
  aes(x = cat, y = 1, label = cat) +
  geom_text(size = 3, colour = "darkgrey") +
  theme_void()

p5 <- arrangeGrob(p5a, p5b, p5c, nrow = 3)
