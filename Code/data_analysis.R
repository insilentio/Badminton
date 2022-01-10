library(waterfalls)
library(gridExtra)
library(scales)

source("Code/data_import.R")


# prep some values ----------------------------------------------------------------------------

#all visits
visits <- stats %>% tally(presence)

#cumulated visits per person
cumvisits <- stats %>%
  count(ID, wt=presence) %>%
  arrange(-n) %>%
  filter(!grepl("Gäste", ID) & !grepl("Passive", ID))

# max. and min. visits
maxmin <- stats %>% 
  filter(type=="Training") %>% 
  group_by(year, week) %>% 
  summarise(presence=sum(presence), .groups = "drop_last") %>%
  summarise(max=max(presence), min=min(presence))

#nr of trainings
all_trainings <- stats %>% 
  filter(type=="Training") %>% 
  distinct(year, week) %>% 
  count()

#years of active memberships since beginning
mitgliedschaftsjahre <- stamm %>%
  count(status) %>%
  filter(status == "a")

#ratio of female memberships since beginning
females <- stamm %>%
  distinct(ID, .keep_all = TRUE) %>%
  count(sex) %>%
  mutate(ratio = n/sum(n)) %>%
  filter(sex == "f")

#number of distinct members since beginning
nmembers <- stamm %>%
  distinct(ID) %>%
  tally()

# number of trainings
nr_trainings <- stats %>%
  group_by(year) %>%
  filter(type == "Training") %>%
  distinct(week) %>%
  tally()

#upper level for memebership years in plots
maxact <- (max(actives$n_years) %/% 10 +1) * 10

#determine first and last year in data
minyear <- min(stats$year)
maxyear <- max(stats$year)

kpi_desc <- c("Mittelwert", "Standardabweichung", "Trainingsbesuche", "Anz. Trainings", "Trainings/Aktivmitglied")
#KPIs last year
kpi <- stats %>%
  filter(year == maxyear & type == "Training") %>%
  group_by(week) %>%
  summarise(visits = sum(presence)) %>%
  summarise(avg = round(mean(visits), 2),
            sd = round(sd(visits), 2),
            visits = sum(visits)) %>%
    bind_cols(nr_tr = (nr_trainings %>% filter(year == maxyear))$n) %>%
    bind_cols(tr_per_active = round(.$visits/nrow(actives), 2)) %>%
  pivot_longer(everything(), names_to = "cat", values_to = "values") %>%
  mutate (desc = kpi_desc, idx = 5:1)


#med, mean, max, min visits per person
stats %>%
  inner_join(actives, by = "ID", keep = TRUE) %>%
  group_by(ID.y, year) %>%
  summarise(visits = sum(presence), .groups = "drop_last") %>%
  summarise(med = median(visits),
            avg = mean(visits),
            max = max(visits),
            min = min(visits),
            .groups = "keep") %>%
  arrange(ID.y)

#mean attendance per year, per person
stats %>%
  filter(!grepl("Gäste", ID) & !grepl("Passive", ID)) %>% 
  filter(type=="Training") %>%
  group_by(ID,year) %>% 
  summarise(sum=sum(presence)) %>% 
  group_by(ID) %>% 
  summarise(mean=mean(sum), tot=mean*n()) %>%
  arrange(-mean) %>% 
  print(n=60)


# big figures for tiles in Teilnehmerstatistik ------------------------------------------------

# collect the figures for the Teilnehmerstatistik
figs <- tibble(cat = "Trainings", value = all_trainings$n,
               head = "seit 2004") %>%
  add_row(cat = "Trainingsbesuche", value = visits$n,
          head = "seit 2004") %>%
  add_row(cat = "max. Teilnehmerzahl", value = max(maxmin$max),
          head = "seit 2004") %>%
  add_row(cat = "min. Teilnehmerzahl", value = min(maxmin$min),
          head = "seit 2004") %>%
  add_row(cat = "Jahre Aktivmitgliedschaften", value = mitgliedschaftsjahre$n,
          head = "seit Gründung")  %>%
  add_row(cat = "bisherige Mitglieder", value = nmembers$n,
          head = "seit Gründung") %>%
  mutate(value = as.character(value)) %>%
  add_row(cat = "Frauenanteil", value = label_percent(accuracy = .1)(females$ratio),
          head = "seit Gründung", .after = 5)%>%
  mutate(cat = factor(cat, levels = cat))


# plot section --------------------------------------------------------------------------------

#second page
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
mytheme <- theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(colour = "darkgrey"),
        plot.title = element_text(colour = "darkgrey", size = 12, hjust = 0.5),
        plot.subtitle = element_text(colour = "darkgrey", size = 10, hjust = 0.5),
        plot.margin = unit(rep(.5,4), "cm"),
        plot.background = element_rect(colour = "darkgrey", fill=NA, size=.5) )

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
  select(c(2,3,4,9,10)) %>%
  mutate(visits = visits/n*100) %>%
ggplot() +
  aes(x = Vorname, y = visits) +
  geom_boxplot(outlier.size = 1, outlier.alpha = .5, coef = 100, width = .5) +
  stat_summary(fun = mean, geom = "point", size = 1, shape = 3, colour = "steelblue", show.legend = TRUE) +
  mytheme +
  labs(title = "Persönliche Besuchsbandbreite seit 2004",
       subtitle = "in % der jährlichen Trainingszahl") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0,100,20), minor_breaks = seq(0,100,5))

# ggplotly(p3)

p4 <- stats %>%
  filter(!(ID %in% c("Gäste div.", "Passive div."))) %>%
  group_by(ID, year) %>%
  summarise(visits = sum(presence), .groups = "drop_last") %>%
  left_join(nr_trainings, by = "year") %>%
  group_by(year) %>%
  mutate(rank = min_rank(desc(visits))) %>%
  inner_join(actives, by = "ID", keep = TRUE, suffix = c(".x", "")) %>%
  select(c(2:6,11)) %>%
ggplot() +
  aes(x = Vorname, y = rank) +
  geom_boxplot(coef = 100, width = .5) +
  stat_summary(fun = mean, geom = "point", size = 1, shape = 3, colour = "steelblue", show.legend = TRUE) +
  mytheme +
  labs(title = "Persönliche Rankingbandbreite seit 2004",
       subtitle = "") +
  scale_y_continuous(limits =c(1,21), breaks = seq(1,21,2))

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

p5 <- grid.arrange(p5a,p5b,p5c, nrow = 3)

#arrange everything on one page
gridplot2 <- grid.arrange(p1, p2, p3, p4, p5,
             heights = c(4,4,1),
             layout_matrix = rbind(c(1,2), c(3,4), c(5)))



# first page
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
c1 <- grob()

c2 <- ggplot(kpi) +
  aes(x = 1, y = idx) +
  geom_text(aes(label = paste0(desc, ": ", values)), size = 3) +
  scale_y_continuous(limits = c(-1, 6)) +
  mytheme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        axis.text.x = element_blank()) +
  labs(title = "Jahres-KPIs")

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
                     minor_breaks = seq(2,50,2)) +
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



c2ga <- grid.arrange(c2, c3, c4,
                     widths = c(2,8,2))

gridplot1 <- grid.arrange(c1, c2ga, c5, c6,
                          heights = c(5,2,3),
                          layout_matrix = rbind(c(1), c(2), c(5,6)))



# save both pages into one pdf
pdf("Output/Teilnehmerstatistik.pdf", width = 29.7/2.54, height = 21/2.54)
lapply(list(gridplot1, gridplot2), grid.arrange)
dev.off()

# ggsave("Teilnehmerstatistik.pdf", gridplot1, "pdf", "Output",
#        width = 29.7, height = 21, units = "cm", dpi = "print")
