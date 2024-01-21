#plots second page
#
#overall mean attendance plot
stats %>%
  group_by(year,week) %>%
  summarise(sum=sum(presence)) %>%
  group_by(week) %>%
  summarise(mean=mean(sum)) %>%
  ggplot() +
  aes(x=week, y=mean) +
  geom_line() +
  xlim(c(1,52)) +
  scale_y_continuous(breaks = c(0:12), limits = c(0,12))

#generate the plots for the Teilnehmerstatistik

p1 <- actives %>%
  left_join(cumvisits, by = "ID", keep = TRUE) %>%
  select(Vorname, n) %>%
  waterfall(draw_lines = FALSE,
            rect_width = .9,
            rect_border = NA,
            fill_colours = rep("steelblue", nrow(actives)),
            fill_by_sign = FALSE) +
  mytheme +
  ggtitle("Kumulierte Teilnahmen der Aktivmitglieder seit 2004")

p2 <- ggplot(actives) +
  aes(x = Vorname, y = n_years) +
  geom_col(fill = "steelblue") +
  geom_text(aes(x = Vorname, y = max(n_years), label = since),
            hjust = 1, angle = 90, colour= "darkgrey") +
  mytheme +
  ggtitle("Mitgliedschaftsdauer in Jahren (inkl. Beitrittsjahr)") +
  scale_y_continuous(limits = c(0,maxact),
                     breaks = seq(0, maxact,10),
                     minor_breaks = seq(0, maxact, 2))

p3 <- stats %>%
  group_by(ID, year) %>%
  summarise(visits = sum(presence), .groups = "drop_last") %>%
  inner_join(actives, by = "ID", keep = TRUE, suffix = c(".x", "")) %>%
  left_join(nr_trainings, by = "year") %>%
  select(ID, visits, Vorname, n) %>%
  mutate(visits = visits/n*100) %>%
  ggplot() +
    aes(x = Vorname, y = visits) +
    geom_boxplot(outlier.size = 1, outlier.alpha = .5, coef = 100, width = .5) +
    stat_summary(fun = mean, geom = "point", size = 1, shape = 3, colour = "steelblue", show.legend = TRUE) +
    mytheme +
    labs(title = "Persönliche Besuchsbandbreite seit 2004",
         subtitle = "in % der Jahresanzahl Trainings") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,20), minor_breaks = seq(0,100,5))

# ranking per year, only of active members
p4 <- stats %>%
  inner_join(stamm, by = c("ID", "year"), keep = TRUE, suffix = c("", ".x")) %>%
  group_by(ID, year, status, Vorname) %>%
  summarise(visits = sum(presence), .groups = "drop_last") %>%
  filter(status == "a") %>%
  group_by(year) %>%
  mutate(rank = min_rank(desc(visits)), ID = ID, year = year, Vorname = Vorname) %>%
  right_join(actives, by = "ID", keep = TRUE, suffix = c("", ".y")) %>%
  ggplot() +
    aes(x = Vorname.y, y = rank) +
    geom_boxplot(coef = 100, width = .5) +
    stat_summary(fun = mean, geom = "point", size = 1, shape = 3, colour = "steelblue", show.legend = TRUE) +
    mytheme +
    labs(title = "Persönliche Rankingbandbreite seit 2004",
         subtitle = "") +
    scale_y_continuous(limits =c(1,20), breaks = c(seq(1,9,2), seq(10,20,2)), minor_breaks = c(1:20))


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

p5 <- arrangeGrob(p5a,p5b,p5c, nrow = 3)

#arrange everything on one page
gridplot2 <- arrangeGrob(p1, p2, p3, p4, p5,
                          heights = c(4,4,1),
                          layout_matrix = rbind(c(1,2), c(3,4), c(5)))
#grid.arrange(p1, p2, p3, p4, p5,
#                           heights = c(4,4,1),
#                           layout_matrix = rbind(c(1,2), c(3,4), c(5)))


temp <- stats %>%
  inner_join(stamm, by = c("ID", "year"), keep = TRUE, suffix = c(".x", "")) %>%
  group_by(ID, year, status, Vorname) %>%
  summarise(visits = sum(presence), .groups = "drop_last") %>%
  filter(status == "a") %>%
  # left_join(nr_trainings, by = "year") %>%
  group_by(year) %>%
  mutate(rank = min_rank(desc(visits)), ID = ID, year = year, Vorname = Vorname)
