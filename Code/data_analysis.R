
# prep some values ----------------------------------------------------------------------------

# if desired, manually verify yearly total with excel file
stats |>
  group_by(year) |>
  summarise(tot = sum(presence))

#all visits
visits <- stats |> tally(presence)

#cumulated visits per person total and only during active member years
cumvisits <- stats |> 
  count(ID, wt = presence) |>
  filter(!grepl("Gäste", ID) & !grepl("Passive", ID)) |> 
  left_join(stats |> 
      left_join(stamm, by = c("ID", "year")) |>
      filter(status == "a") |> 
              count(ID, wt = presence, name = "n_active"), by = "ID") |> 
  arrange(-n)

# max. and min. visits
maxmin <- stats |> 
  filter(type=="Training") |> 
  group_by(year, week) |> 
  summarise(presence=sum(presence), .groups = "drop_last") |>
  summarise(max=max(presence), min=min(presence))

#nr of trainings
all_trainings <- stats |> 
  filter(type=="Training") |> 
  distinct(year, week) |> 
  count()

#years of active memberships since beginning
mitgliedschaftsjahre <- stamm |>
  count(status) |>
  filter(status == "a")

#ratio of female memberships since beginning
females <- stamm |>
  distinct(ID, .keep_all = TRUE) |>
  count(sex) |>
  mutate(ratio = n/sum(n)) |>
  filter(sex == "f")

#number of distinct active members since beginning
nmembers <- stamm |>
  filter(n_years > 0) |>
  distinct(ID) |>
  tally()

# number of trainings
nr_trainings <- stats |>
  group_by(year) |>
  filter(type == "Training") |>
  distinct(week) |>
  tally()

#determine first and last year in data
minyear <- min(stats$year)
maxyear <- max(stats$year)

#only currently active members
actives <- stamm |>
  filter(year == maxyear & status == "a") |>
  mutate(Vorname = paste0(Vorname, " ", substr(Nachname, 1, 1), ".")) |>
  select(ID, sex, status, n_years, Vorname, since) |>
  arrange(desc(n_years)) |>
  mutate(ID = factor(ID, levels = ID), Vorname = factor(Vorname, levels = Vorname)) |> 
  left_join(stamm |> 
    filter(year >= 2004 & status == "a") |> 
    group_by(ID) |> 
    summarise(activeYearsSince2004 = n()), by = "ID")


#upper level for memebership years in plots
maxact <- (max(actives$n_years) %/% 10 +1) * 10

kpi_desc <- c("Besuche/Training", "Aktive/Training", "Standardabweichung", "Besuche (total)", "Besuche\n(Veränderung zum Vorjahr)", "Trainings")
#KPIs last year
kpi <- stats |>
  filter(year == maxyear & type == "Training") |>
  group_by(week) |>
  summarise(visits = sum(presence)) |>
  summarise(avg = round(mean(visits), 1),
            sd = round(sd(visits), 1),
            visits = sum(visits)) |>
  bind_cols(upratio = stats |>
              filter(year %in% c((maxyear-1):maxyear) & type == "Training") |>
              group_by(year) |>
              summarise(visits = sum(presence)) |>
              mutate(up = 100/accumulate(visits, `/`)-100) |>
              pluck(3,2) |>
              round(0)) |>
  bind_cols(nr_tr = (nr_trainings |> filter(year == maxyear))$n) |>
  bind_cols(stats |>
              left_join(stamm |> select(ID, year, status), by = c("ID", "year")) |> 
              filter(year == maxyear & type == "Training" & status == "a") |>
              group_by(week) |>
              summarise(visits = sum(presence)) |> 
              summarise(avg_a = round(mean(visits), 1))) |> 
  select(avg, avg_a, sd, visits, upratio, nr_tr) |> 
  pivot_longer(everything(), names_to = "cat", values_to = "values") |>
  mutate(desc = kpi_desc, idx = seq(11, 1, -2)) |>
  mutate(values = as.character(values)) |>
  mutate(values = if_else(cat == "upratio",
                          if_else(values >= 0, paste0("+", values, "%"), paste0("-", values, "%")),
                                  values))



#med, mean, max, min visits per person
summaries <- stats |>
  inner_join(actives, by = "ID", keep = TRUE) |>
  group_by(ID.y, year) |>
  summarise(visits = sum(presence), .groups = "drop_last") |>
  summarise(med = median(visits),
            avg = mean(visits),
            max = max(visits),
            min = min(visits),
            .groups = "keep") |>
  arrange(ID.y)

#mean attendance per year, per person
mean_pp <- stats |>
  filter(!grepl("Gäste", ID) & !grepl("Passive", ID)) |> 
  filter(type=="Training") |>
  group_by(ID,year) |> 
  summarise(sum=sum(presence), .groups = "drop_last") |> 
  summarise(mean=mean(sum), tot=mean*n()) |>
  arrange(-mean)

# big figures for tiles in Teilnehmerstatistik ------------------------------------------------

# collect the figures for the Teilnehmerstatistik
figs <- tibble(cat = "Trainings", value = all_trainings$n,
               head = "seit 2004") |>
  add_row(cat = "Trainingsbesuche", value = visits$n,
          head = "seit 2004") |>
  add_row(cat = "max. Teilnehmerzahl", value = max(maxmin$max),
          head = "seit 2004") |>
  add_row(cat = "min. Teilnehmerzahl", value = min(maxmin$min),
          head = "seit 2004") |>
  add_row(cat = "Jahre Aktivmitgliedschaften", value = mitgliedschaftsjahre$n,
          head = "seit Gründung")  |>
  add_row(cat = "bisherige Aktivmitglieder", value = nmembers$n,
          head = "seit Gründung") |>
  mutate(value = as.character(value)) |>
  add_row(cat = "Frauenanteil", value = label_percent(accuracy = .1)(females$ratio),
          head = "seit Gründung", .after = 5) |> 
  mutate(cat = factor(cat, levels = cat))

