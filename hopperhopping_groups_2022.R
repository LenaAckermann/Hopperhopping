#######################################
### Hopperhopping Gruppeneinteilung ###
#######################################

if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")
  }else{
    print("dplyr vorhanden")
  }
if("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr")
}else{
  print("tidyr vorhanden")
}
library(dplyr)
library(tidyr)

setwd("~/Downloads")
df <- read.csv("Hopperhopping.csv", header = T) %>% arrange(Gender, Kochen)
# Data frame importieren oder hier generieren

df <- df %>% arrange(Kochen)
# df <- df %>% arrange(Gender, Kochen)
df.w <- df %>% filter(Gender == "f")
df.m <- df %>% filter(Gender == "m")

GenderBalance <- nrow(df.w) - nrow(df.m) # Positiv = mehr Frauen, negativ = mehr Männer
Extra <- nrow(df) %% 6 # Gesamtzahl sollte durch 3 teilbar sein, sonst brauchen wir Dreiergruppen

if (Extra != 0 & GenderBalance > 0){
  join <- df.w[sample(nrow(df.w), Extra), ] 
  df <- df %>% filter(!Name %in% join$Name)
} else{
  if (Extra != 0 & GenderBalance <= 0){
    join <- df.m[sample(nrow(df.m), Extra), ] 
    df <- df %>% filter(!Name %in% join$Name)
  } else{
    print("Alles toppi! Nächsten Schritt ausführen")
  }
}

# Zwei Gruppen erstellen
df.yes <- df %>% slice_head(prop=0.5)
df.no <- df  %>% slice_tail(prop=0.5) 

# Partner*innen zuweisen
df.yes$Partner <- sample(df.no$Name)
df.all <- df.yes

# Vier Gruppen erstellen - mit Berücksichtigung Gender
#Group_breaks <- c(nrow(df)/4, (nrow(df)/4)*2, (nrow(df)/4)*3, (nrow(df)/4)*4)
#df.w.yes <- df %>% filter(between(row_number(), 1, Group_breaks[1]))
#df.w.no <- df %>% filter(between(row_number(), Group_breaks[1]+1, Group_breaks[2]))
#df.m.yes <- df %>% filter(between(row_number(), Group_breaks[2]+1, Group_breaks[3]))
#df.m.no <- df %>% filter(between(row_number(), Group_breaks[3]+1, Group_breaks[4]))

# Partner*innen zuweisen
# df.w.yes$Partner <- sample(df.m.no$Name)
# df.m.yes$Partner <- sample(df.w.no$Name)
# df.all <- bind_rows(df.m.yes, df.w.yes, .id = NULL)

# Extras Gruppen zuweisen und nur relevante Spalten behalten
DrittePerson <- df.all[sample(nrow(df.all), Extra), ]
DrittePerson$DrittePerson <- join$Name
df.clean <- df.all  %>%  full_join(DrittePerson) %>% select(Name, Partner, DrittePerson)

# Randomisiert Gang zuweisen
df.clean$Gang <- sample(rep(c("Vorspeise", "Hauptgang", "Nachtisch"), length.out = length(df.clean$Name)))

# Subset nach Gang, randomisiert Zahl zuweisen
NumberGroups <- nrow(df.clean)/3
df.vorspeise <- df.clean %>% filter(Gang=="Vorspeise") %>% mutate(Nummer = sample(c(1:NumberGroups)))
df.hauptgang <- df.clean %>% filter(Gang=="Hauptgang") %>% mutate(Nummer = sample(c(1:NumberGroups)))
df.nachtisch <- df.clean %>% filter(Gang=="Nachtisch") %>% mutate(Nummer = sample(c(1:NumberGroups)))

# Alles in einen df
df.clean <- bind_rows(df.vorspeise, df.hauptgang, df.nachtisch, .id = NULL)  %>% arrange(Gang, Nummer)
