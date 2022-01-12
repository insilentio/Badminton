
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
  mutate(desc = kpi_desc, idx = 5:1) %>%
  mutate(values = as.character(values))


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

#first page
source("Code/plots_page1.R")

# second page
source("Code/plots_page2.R")

# save both pages into one pdf
pdf("Output/Teilnehmerstatistik.pdf", width = 29.7/2.54, height = 21/2.54)
lapply(list(gridplot1, gridplot2), grid.arrange)
dev.off()
