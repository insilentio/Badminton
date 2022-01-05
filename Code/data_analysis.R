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



# big figures for tiles in Teilnehmerstatistik ------------------------------------------------

# collect the figures for the Teilnehmerstatistik
figs <- tibble(cat = "nr of trainings", value = nr_trainings$n) %>%
  add_row(cat = "all visits", value = visits$n) %>%
  add_row(cat = "max people in training", value = max(maxmin$max)) %>%
  add_row(cat = "min people in training", value = min(maxmin$min)) %>%
  add_row(cat = "years of active memberships", value = mitgliedschaftsjahre$n) %>%
  add_row(cat = "ratio of female memberships", value = females$ratio) %>%
  add_row(cat = "number of members", value = nmembers$n)


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
  rename(ID = ID.x) %>%
  select(ID, n) %>%
  waterfall(draw_lines = FALSE, rect_border = NA) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle("Kumulierte Teilnahmen der Aktivmitglieder seit 2004")
wf

mj <- ggplot(actives) +
  aes(x = ID, y = n_years) +
  geom_col(fill = "lightblue") +
  geom_text(aes(x = ID, y = max(n_years), label = since),
            hjust = 1, angle = 90, colour= "darkgrey") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title = element_blank()) +
  ggtitle("Mitgliedschaftsdauer in Jahren (inkl. Beitrittsjahr)")
mj
