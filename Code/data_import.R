
Sys.setlocale("LC_ALL", 'de_CH.UTF-8')
path <- "Data/BCT_Excel.xlsx"
#(symbolic link currently gets deleted by NAS sync every day - must be manually re-created first)

#vector with column names. 99 stands for Turnier
cn <- c("Nachname", "Vorname", c(1:52), "ID", "Total", "99", "Kat", "Rang")
events <- c(as.character(c(1:52, 99))) 


# attendance data -----------------------------------------------------------------------------

#read the data
sheets <- excel_sheets(path)
if (exists("stats")) rm(stats)
for (i in 1:(length(sheets)-1)) {
  single <- read_excel(path, sheet = sheets[i], range="A5:BG60", col_names = cn)

  #tidy it up
  #replace all x's with 1, name the sheets with resp. year, name cols, filter lot of rows and
  #finally gather all together
  single <- single %>% 
          replace(. == "x", "1") %>%
          replace(. == "Total Trainings", "Trainings") %>%
          mutate(year=sheets[i]) %>%
          select("ID", "year", all_of(events)) %>%
          mutate(across(1, as.character)) %>%
          mutate(across(c(2:55), as.numeric)) %>%
          filter(!is.na(ID)) %>%
          pivot_longer(all_of(events), names_to="week", values_to = "presence") %>%
          mutate(across("presence", ~if_else(is.na(presence),0,presence)))
  
  #let's evaluate if respective week was vacation or not
  #(if at least somebody was around, then obviously not)
  trainings <- single %>%
    pivot_wider(names_from ="ID", values_from = "presence") %>%
    pivot_longer(-c("year", "week", "Trainings"), names_to = "ID", values_to = "presence") %>%
    group_by(week, Trainings) %>%
    summarise(sum = sum(presence), .groups = "drop") %>%
    mutate(type = if_else(sum > 0, "Training", "Ferien"))  %>%
    mutate(type = if_else(week == "99", "Turnier", type))
  
  # move to next iteration if no trainings available (e.g. current year)
  if ((trainings %>% summarise(sum = sum(sum)))$sum == 0) next
  
  #combine the 2 datasets. Col. "Trainings" can be dropped later, but is used for imputation in next step
  #some entries have to be filtered out as they contain aggregated values already (e.g. "Aktive")
  single <- left_join(single, trainings, by="week") %>%
    select("ID", "year", "week", "presence", "type", "Trainings") %>%
    mutate_at(vars("week"), "as.numeric") %>%
    filter(ID!="Aktive" & ID!="Total" & ID!="Mittelwert" & ID!="Trainings" & ID!="Passive" & ID!="Gäste")
  
  #split Turnier visits into separate variable; otherwise tends to be forgotten later
  turnier_single <- single %>% filter(type=="Turnier")
  single <- single %>% filter(type!="Turnier")
  
  #now the part with the imputation:
  #years <2013 are incomplete regarding presence stats due to absences of Irene. Only trainings
  #when she was present are available. Means: for her, stats are complete,
  #for everybody else we have to assume values are missing.
  #Such values are imputed by simply interpolating:
  #(nr. of trainings w/stats) divided by (nr. of training weeks total) times (trainings attended)
  #values are then assigned randomly to missing weeks
  n_train <- single %>%
    group_by(type, week) %>%
    summarise(n=n_distinct(week), .groups = "drop_last") %>%
    summarise(n=sum(n), .groups = "drop") %>%
    filter(type=="Training")
  data_train <- single %>%
    group_by(Trainings, week) %>% 
    summarise(n=n_distinct(week), .groups = "drop_last") %>%
    summarise(n=sum(n), .groups = "drop") %>%
    filter(Trainings==1)
  
  quote <- n_train$n / data_train$n
  if (quote < 1) {
    #then we have a mismatch between effective trainings and trainings with data
    #first we define which weeks are missing
    mw <- single %>% 
      filter(Trainings == 1 & type == "Ferien") %>% 
      distinct(week)
    #then how many attendances per person have to be added (except Irene)
    ma <- single %>%
      filter(ID != "SchwarzenbachIrene") %>%
      group_by(ID) %>% 
      summarise(sum = sum(presence), .groups = "drop") %>% 
      mutate(interpoliert = round(sum/quote) - sum)
    #now select randomly for each ID as many of the missing weeks as additional attendances were determined
    for (j in 1:nrow(ma)) {
      #first step, we don't impute for Gäste and Passive (needs different approach)
      if (ma[j,]$ID != "Gäste div." & ma[j,]$ID != "Passive div.") {
        #sample does not work properly if x (vector from which to choose from) has length 1,
        # hence special treatment needed
        if (length(mw$week) > 1) {
          fill_ins <- sample(mw$week, ma[j,]$interpoliert, replace = F)
        } else {
          fill_ins <- mw$week * ma[j,]$interpoliert
        }
        single <- single %>% 
          mutate(presence = if_else(ID == ma[j,]$ID & week %in% fill_ins,1,presence))
      }
      #now separately impute for Gäste and Passive
      if (ma[j,]$ID == "Gäste div." | ma[j,]$ID == "Passive div.") {
        #first we define on how many trainings we want to split the interpolated values.
        #this is determined by counting all trainings with at least 1 attendance of
        #Gäste/Passive (similar to attendance of a person)
        miss_gp <- single %>%
          filter(ID == ma[j,]$ID, type=="Training", presence>0) %>%
          group_by(ID) %>% tally() %>% 
          mutate(interpoliert = round(n/quote) - n)
        
        if (length(mw$week) > 1) {
          fill_ins <- sample(mw$week, miss_gp$interpoliert, replace = F)
        } else {
          fill_ins <- mw$week * (ma[j,]$interpoliert > 0)
        }
        #how many attendances should be distributed per training?
        #in average:
        nr_att <- floor(ma[j,]$interpoliert/length(mw$week))
        trs <- rep(nr_att, length(fill_ins))
        #now check for leftovers (from flooring for average) and distribute randomly
        while (ma[j,]$interpoliert - sum(trs) != 0) {
          k <- sample(1:length(trs),1)
          trs[k] <- trs[k] + 1
        }

        single <- single %>%
          filter(ID == ma[j,]$ID & week %in% fill_ins) %>%
          mutate(presence = trs) %>%
          union(single %>% filter(!(ID == ma[j,]$ID & week %in% fill_ins)))
      }
    }
  }
 
  #remove column Trainings
  single <- single |> 
    mutate(type = if_else(Trainings == 1, "Training", type)) |> 
    select(-Trainings)

  #add to the overall tibble
  if (!exists("stats")) {
    stats <- single
    turnier <-  turnier_single
  } else {
    stats <- union(stats, single)
    turnier <- union(turnier, turnier_single)
  }
}

# master data ---------------------------------------------------------------------------------

# read master data
stamm <- read_excel(path, sheet = "Stammdaten", col_names = TRUE)
cn2 <- names(stamm)
cn2 <- ifelse(!grepl("[^[:digit:]]", cn2), paste0("y", cn2), cn2)
cn2 <- gsub(" ", "", cn2)
names(stamm) <- cn2


#all members
stamm <- stamm %>%
  slice(-1) %>%
  pivot_longer(starts_with("y"), names_to = "year",  values_to = "status") %>%
  select(ID, sex, year, status, JahreaktivimVerein, Mitgliedseit, Nachname, Vorname) %>%
  mutate(year = substr(year, 2, 5)) %>%
  rename(n_years = JahreaktivimVerein, since = Mitgliedseit) %>%
  mutate(across(c("year", "n_years"), as.numeric)) %>%
  filter(!is.na(ID))


# remove non-members from stats
stats <- stats |> 
  inner_join(stamm |> select(ID) |> unique(), by = "ID")
