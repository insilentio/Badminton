library(waterfalls)

source("data_import.R")


# prep some values ----------------------------------------------------------------------------

#all visits
visits <- stats %>% tally(presence)
visits$n
#cumulated visits per person
cumvisits <- stats %>%
  count(ID, wt=presence) %>%
  arrange(-n) %>%
  filter(!grepl("Gäste", ID) & !grepl("Passive", ID))
cumvisits
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
# max. and min. visits
maxmin <- stats %>% 
  filter(type=="Training") %>% 
  group_by(year, week) %>% 
  summarise(presence=sum(presence)) %>%
  summarise(max=max(presence), min=min(presence))
maxmin
#nr of trainings
nr_trainings <- stats %>% 
  filter(type=="Training") %>% 
  distinct(year, week) %>% 
  count()
nr_trainings
#years of active memberships since beginning
mitgliedschaftsjahre <- stamm %>%
  count(status) %>%
  filter(status == "a")
mitgliedschaftsjahre
#ratio of female memberships since beginning
females <- stamm %>%
  distinct(ID, .keep_all = TRUE) %>%
  count(sex) %>%
  mutate(ratio = n/sum(n)) %>%
  filter(sex == "f")
females
#number of distinct members since beginning
nmembers <- stamm %>%
  distinct(ID) %>%
  tally()
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


# big figures for tiles in Teilnehmerstatistik ------------------------------------------------

# collect the figures for the Teilnehmerstatistik
figs <- tibble(cat = "nr of trainings", value = nr_trainings$n) %>%
  add_row(cat = "all visits", value = visits$n) %>%
  add_row(cat = "max people in training", value = max(maxmin$max)) %>%
  add_row(cat = "min people in training", value = min(maxmin$min)) %>%
  add_row(cat = "years of active memberships", value = mitgliedschaftsjahre$n) %>%
  add_row(cat = "ratio of female memberships", value = females$ratio) %>%
  add_row(cat = "number of members", value = nmembers$n)

# number of trainings
nr_trainings <- stats %>%
  group_by(year) %>%
  filter(type == "Training") %>%
  distinct(week) %>%
  tally()

# plot section --------------------------------------------------------------------------------

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
wf <- actives %>%
  left_join(cumvisits, by = "ID", keep = TRUE) %>%
  select(Vorname, n) %>%
  waterfall(draw_lines = FALSE, rect_border = NA) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggtitle("Kumulierte Teilnahmen der Aktivmitglieder seit 2004")
wf

mj <- ggplot(actives) +
  aes(x = Vorname, y = n_years) +
  geom_col(fill = "lightblue") +
  geom_text(aes(x = Vorname, y = max(n_years), label = since),
            hjust = 1, angle = 90, colour= "darkgrey") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_blank()) +
  ggtitle("Mitgliedschaftsdauer in Jahren (inkl. Beitrittsjahr)")
mj

pv_data <- stats %>%
  group_by(ID, year) %>%
  summarise(visits = sum(presence), .groups = "drop_last") %>%
  inner_join(actives, by = "ID", keep = TRUE, suffix = c(".x", "")) %>%
  left_join(nr_trainings, by = "year") %>%
  select(c(2,3,4,9,10)) %>%
  mutate(visits = visits/n*100) %>%
ggplot() +
  aes(x = Vorname, y = visits) +
  geom_boxplot(outlier.size = 1, outlier.alpha = .5, coef = 100, width = .5) +
  stat_summary(fun = mean, geom = "point", size = 1, shape = 3, colour = "blue", show.legend = TRUE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_blank()) +
  labs(title = "Persönliche Besuchsbandbreite seit 2004",
       subtitle = "in % der jährlichen Trainingszahl") 
pv
ggplotly(pv)

pr <- stats %>%
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
  geom_boxplot(outlier.size = 1, outlier.alpha = .5, coef = 100, width = .5) +
  stat_summary(fun = mean, geom = "point", size = 1, shape = 3, colour = "blue", show.legend = TRUE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_blank()) +
  labs(title = "Persönliche Rankingbandbreite seit 2004",
       subtitle = "") +
  scale_y_continuous(limits =c(1,21), breaks = seq(1,21,2))
pr
