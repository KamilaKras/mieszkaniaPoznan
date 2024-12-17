#0 - Dataframe do modyfikacji
all_data_edit <- all_data

#1 - Znajdowanie braków oznaczonych inaczej niż "NA"
print(all_data_edit %>%
        miss_scan_count(search = list("N/A", "N/a", "missing", " ")), n=40)
#Wynik: wartości puste będące pustym polem występują w kolumnie "quarter"

#2 - Zastąpienie " " w quarter przez NA
all_data_edit <- all_data_edit %>%
  mutate(quarter = ifelse(quarter == "", NA, quarter))

vis_miss(all_data_edit, sort_miss = TRUE, cluster=TRUE) #liczba niepełnych kolumn zwiększyła się o zmienną quarter

#3 - Sprawdzenie logiczności dat
#zamiana typu danych na date
all_data_edit$date_activ <- as.Date(all_data_edit$date_activ)
all_data_edit$date_modif <- as.Date(all_data_edit$date_modif)
all_data_edit$date_expire <- as.Date(all_data_edit$date_expire)
class(all_data_edit$date_activ)
class(all_data_edit$date_modif)
class(all_data_edit$date_expire)

#sprawdzenie czy daty modyfikacji i wyagsnięcia nie są wcześniejsze niż data publikacji
logic_errors_dates <- all_data_edit %>%
  filter(date_activ > date_expire | date_activ > date_modif)
nrow(logic_errors_dates)
#Wynik: nie ma tego rodzaju błedów w datach

#4 - sprawdzenie wartości odstających, nietypowych dla charakteru zmiennych
summary(all_data_edit)

#flat_area - wartości na minusie, 1000 i 2200
#flat_rooms - wartości na minusie i 11 do weryfikacji z tytułem ogłoszenia
#flat_rent  - 17000 i 50000
#flat_deposit - 28000 i 600067
#building_floor_number - pare możliwych wysokich wartości - do weryfikacji z tytułem ogłoszenia
#quarter - nieistniejące dzielnice

#5 - Zamiana wartości odstających na NA

#flat_area mniejsza od 0 i  wieksza od 500
all_data_edit <- all_data_edit %>%
  mutate(flat_area = ifelse(flat_area <= 0 | flat_area > 500, NA, flat_area))

#flat_rooms mniejsze od 0
all_data_edit <- all_data_edit %>%
  mutate(flat_rooms = ifelse(flat_rooms <= 0, NA, flat_rooms))

#flat_rent mniejsze rowne 10 i wieksze od 10000
all_data_edit <- all_data_edit %>%
  mutate(flat_rent = ifelse(flat_rent <= 10 & flat_rent > 0 | flat_rent > 10000, NA, flat_rent))

#flat_deposit - wartości rzędu 1,2 ..15, oraz dziesiatek  tysiecy
all_data_edit <- all_data_edit %>%
  mutate(flat_deposit = ifelse(flat_deposit > 15000, NA, flat_deposit))

gg_miss_var(all_data_edit)

#6 - Wnioskowanie z tytułów ogłoszeń - uzupelnienie brakujacych danych i poprawienie zlych - imputacja dedukcyjna
#6.1 - zmienne binarne
all_data_regex <- all_data_edit
#korzystanie z all_data_regex

#individual
all_data_regex <- all_data_regex %>%
  mutate(individual_cl = case_when(
    str_detect(str_to_lower(ad_title), "oferta prywatna|osoba prywatna|os\\. prywatn|prywatne|prywatnie|bez pośrednik|bezpo|bez po") ~ TRUE,
    TRUE ~ NA_real_  # Ensuring that the result is NA (real) when the condition is not met
  ))

all_data_regex <- all_data_regex %>%
  mutate(individual = coalesce(individual, individual_cl)) %>%
  select(-individual_cl) 

all_data_regex$individual <- as.logical(all_data_regex$individual)

#flat_balcony
all_data_regex <- all_data_regex %>%
  mutate(flat_balcony_cl = case_when(
    str_detect(str_to_lower(ad_title), "balk") ~ TRUE,
    TRUE ~ NA_real_ 
  ))

all_data_regex <- all_data_regex %>%
  mutate(flat_balcony = if_else(!is.na(flat_balcony_cl), flat_balcony_cl, flat_balcony)) %>%
  select(-flat_balcony_cl)  

all_data_regex$flat_balcony <- as.logical(all_data_regex$flat_balcony)

#flat_furnished
extract_furniture <- function(ad_title) {
  ad_title <- tolower(ad_title) # Convert to lowercase
  case_when(
    str_detect(ad_title, "\\bumeblow") ~ TRUE,
    str_detect(ad_title, "\\bmebl") ~ TRUE,
    str_detect(ad_title, "umebl[^\\w]") ~ TRUE,
    str_detect(ad_title, "\\b umeb") ~ TRUE,
    str_detect(ad_title, "\\bumebl\\.") ~ TRUE,
    str_detect(ad_title, "\\bwyposa") ~ TRUE,
    str_detect(ad_title, "\\bnieumebl") ~ FALSE,
    str_detect(ad_title, "\\bbez mebl") ~ FALSE,
    
    # Default Case
    TRUE ~ NA
  )
}
all_data_regex$flat_furnished_cl <- extract_furniture(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_furnished = if_else(!is.na(flat_furnished_cl), flat_furnished_cl, flat_furnished)) %>%
  select(-flat_furnished_cl) 

#flat_for_students
extract_for_students <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title) 
    contains_student <- str_detect(ad_title, "student")
    if (!contains_student) {
      return(NA)  
    }
    return(TRUE)  
  })
}
all_data_regex$flat_for_students_cl <- extract_for_students(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_for_students = if_else(!is.na(flat_for_students_cl), flat_for_students_cl, flat_for_students)) %>%
  select(-flat_for_students_cl) 

#flat_garage
extract_garage <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title) 
    contains_gara <- str_detect(ad_title, "gara")
    if (!contains_gara) {
      return(NA)  
    }
    return(TRUE)
  })
}

all_data_regex$flat_garage_cl <- extract_garage(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_garage = if_else(!is.na(flat_garage_cl), flat_garage_cl, flat_garage)) %>%
  select(-flat_garage_cl) 

#flat_garden
extract_garden <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title) 
    if (str_detect(ad_title, "na ogrod")) {
      return(NA)
    }
    if (str_detect(ad_title, "(ogrod|ogród|z ogrodem|ogródk)")) {
      return(TRUE)
    }
    return(NA)
  })
}

all_data_regex$flat_garden_cl <- extract_garden(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_garden = if_else(!is.na(flat_garden_cl), flat_garden_cl, flat_garden)) %>%
  select(-flat_garden_cl) 

#flat_dishwasher
extract_dishwasher <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title)  
    contains_dishwasher <- str_detect(ad_title, "zmywar")
    if (!contains_dishwasher) {
      return(NA) 
    }
    return(TRUE)  
  })
}
all_data_regex$flat_dishwasher_cl <- extract_dishwasher(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_dishwasher = if_else(!is.na(flat_dishwasher_cl), flat_dishwasher_cl, flat_dishwasher)) %>%
  select(-flat_dishwasher_cl)

#flat_internet
extract_internet <- function(ad_title) {
  ad_title <- tolower(ad_title)
  case_when(
    str_detect(ad_title, "\\bmedia") ~ TRUE,
    str_detect(ad_title, "\\bwifi") ~ TRUE,
    str_detect(ad_title, "\\binternet") ~ TRUE,
    str_detect(ad_title, "\\bintern") ~ TRUE,
    str_detect(ad_title, "\\bwi-fi") ~ TRUE,
    TRUE ~ NA
  )   
}

all_data_regex$flat_internet_cl <- extract_internet(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_internet = if_else(!is.na(flat_internet_cl), flat_internet_cl, flat_internet)) %>%
  select(-flat_internet_cl)

#flat_closed_area
extract_closed_area <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title)  
    contains_closed_area <- str_detect(ad_title, "zamkn")
    if (!contains_closed_area) {
      return(NA) 
    }
    return(TRUE)  
  })
}

all_data_regex$flat_closed_area_cl <- extract_closed_area(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_closed_area = if_else(!is.na(flat_closed_area_cl), flat_closed_area_cl, flat_closed_area)) %>%
  select(-flat_closed_area_cl)

#flat_utility_room
extract_utility_room <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title)  
    contains_utility_room <- str_detect(ad_title, "gosp")
    if (!contains_utility_room) {
      return(NA) 
    }
    return(TRUE)  
  })
}

all_data_regex$flat_utility_room_cl <- extract_utility_room(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_utility_room = if_else(!is.na(flat_utility_room_cl), flat_utility_room_cl, flat_utility_room)) %>%
  select(-flat_utility_room_cl)

#flat_basement
extract_basement <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title)  
    contains_basement <- str_detect(ad_title, "piwn")
    if (!contains_basement) {
      return(NA) 
    }
    return(TRUE)  
  })
}

all_data_regex$flat_basement_cl <- extract_basement(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_basement = if_else(!is.na(flat_basement_cl), flat_basement_cl, flat_basement)) %>%
  select(-flat_basement_cl)

#flat_tarrace
extract_tarrace <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title)  
    contains_tarrace <- str_detect(ad_title, "taras")
    if (!contains_tarrace) {
      return(NA) 
    }
    return(TRUE)  
  })
}

all_data_regex$flat_tarrace_cl <- extract_tarrace(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_tarrace = if_else(!is.na(flat_tarrace_cl), flat_tarrace_cl, flat_tarrace)) %>%
  select(-flat_tarrace_cl)

#flat_lift
extract_lift <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title)  
    contains_lift <- str_detect(ad_title, "wind")
    if (!contains_lift) {
      return(NA) 
    }
    return(TRUE)  
  })
}

all_data_regex$flat_lift_cl <- extract_lift(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_lift = if_else(!is.na(flat_lift_cl), flat_lift_cl, flat_lift)) %>%
  select(-flat_lift_cl)

#flat_two_level
extract_two_level <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title)  
    contains_two_level <- str_detect(ad_title, "pozio")
    if (!contains_two_level) {
      return(NA) 
    }
    return(TRUE)  
  })
}

all_data_regex$flat_two_level_cl <- extract_two_level(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_two_level = if_else(!is.na(flat_two_level_cl), flat_two_level_cl, flat_two_level)) %>%
  select(-flat_two_level_cl)

#flat_air_cond
extract_air_cond <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title)  
    contains_air_cond <- str_detect(ad_title, "klimatyza")
    if (!contains_air_cond) {
      return(NA) 
    }
    return(TRUE)  
  })
}

all_data_regex$flat_air_cond_cl <- extract_air_cond(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_air_cond = if_else(!is.na(flat_air_cond_cl), flat_air_cond_cl, flat_air_cond)) %>%
  select(-flat_air_cond_cl)

#flat_television
extract_flat_tv <- function(ad_title) {
  ad_title <- tolower(ad_title)
  case_when(
    str_detect(ad_title, "\\btv") ~ TRUE,
    str_detect(ad_title, "\\btelew") ~ TRUE,
    TRUE ~ NA
  )   
}

all_data_regex$flat_television_cl <- extract_flat_tv(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_television = if_else(!is.na(flat_television_cl), flat_television_cl, flat_television)) %>%
  select(-flat_television_cl)

#flat_monitoring
extract_flat_monitoring <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title)  
    contains_flat_monitoring <- str_detect(ad_title, "monitor")
    if (!contains_flat_monitoring) {
      return(NA) 
    }
    return(TRUE)  
  })
}

all_data_regex$flat_monitoring_cl <- extract_flat_monitoring(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_monitoring = if_else(!is.na(flat_monitoring_cl), flat_monitoring_cl, flat_monitoring)) %>%
  select(-flat_monitoring_cl)

#flat_kitchen_sep
extract_kitchen_sep <- function(ad_titles) {
  sapply(ad_titles, function(ad_title) {
    ad_title <- tolower(ad_title)  
    contains_kitchen_sep <- str_detect(ad_title, "kuchni")
    if (!contains_kitchen_sep) {
      return(NA) 
    }
    return(TRUE)  
  })
}

all_data_regex$flat_kitchen_sep_cl <- extract_kitchen_sep(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_kitchen_sep = if_else(!is.na(flat_kitchen_sep_cl), flat_kitchen_sep_cl, flat_kitchen_sep)) %>%
  select(-flat_kitchen_sep_cl)

#6.2 dane liczbowe

#flat_rent
extract_rent <- function(ad_title) {
  ad_title <- tolower(ad_title) 
  case_when(
    str_detect(ad_title, "\\bbezczynsz") ~ 0,
    str_detect(ad_title, "\\bbez czynsz") ~ 0,
    str_detect(ad_title, "\\bbrak czyn") ~ 0,
    str_detect(ad_title, "\\bbez opłaty czynsz") ~ 0,
    str_detect(ad_title, "\\bbez dodatkowych opłat") ~ 0
  )}

all_data_regex$flat_rent_cl <- extract_rent(all_data_regex$ad_title)

all_data_regex <- all_data_regex %>%
  mutate(flat_rent = if_else(!is.na(flat_rent_cl), flat_rent_cl, flat_rent)) %>%
  select(-flat_rent_cl)

#flat_deposit - tylko jeden blad
all_data_regex$flat_deposit[all_data_regex$id == 15415] <- 0

#flat_rooms
extract_rooms <- function(ad_title) {
  ad_title <- tolower(ad_title) 
  patterns_to_numbers <- c(
    "1\\s*-?pok\\.?|1pokoje|1 pokoje|1-pokoje|1 - pokojowe|1-pokojowe|1 pokojowe|1pokojowe|1 - pokojowy|1-pokojowy|1 pokojowy|1pokojowy|1 - pok|1-pok|1pokoj|1 duże pok.|1 przestronne pokoje|1 niezależne pokoje|1 oddzielone pokoje|1 oddzielne pokoje|1 osobne pokoje|1 osobne pok.|1 niezal. pokoje|1 niezależnymi pokojami|\\bjednopok|\\bkawale" = 1,
    "2\\s*-?pok\\.?|2pokoje|2 pokoje|2-pokoje|2 - pokojowe|2-pokojowe|2- pokojowe|2 pokojowe|2pokojowe|2 - pokojowy|2-pokojowy|2 pokojowy|2pokojowy|2 - pok|2-pok|2pokoj|2 duże pok.|2 przestronne pokoje|2 niezależne pokoje|2 oddzielone pokoje|2 oddzielne pokoje|2 osobne pokoje|2 osobne pok.|2 niezal. pokoje|2 niezależnymi pokojami|\\bdwupok|\\bdwa pok|\\bdwa|\\bdwu pok|2-u pok" = 2,
    "3\\s*-?pok\\.?|3pokoje|3 pokoje|3-pokoje|3 - pokojowe|3-pokojowe|3 pokojowe|3 - pokojowy|3-pokojowy|3 pokojowy|3pokojowe|3pokojowy|3 - pok|3pokoj|3 duże pok.|3 przestronne pokoje|3 niezależne pokoje|3 oddzielone pokoje|3 oddzielne pokoje|3 osobne pokoje|3 osobne pok.|3 niezal. pokoje|3 niezależnymi pokojami|\\btrzypok|\\btrzy pok|\\btrzy" = 3,
    "4\\s*-?pok\\.?|4pokoje|4 pokoje|4-pokoje|4 - pokojowe|4-pokojowe|4 pokojowe|4 - pokojowy|4-pokojowy|4 pokojowy|4pokojowe|4pokojowy|4 - pok|4pokoj|4 duże pok.|4 przestronne pokoje|4 niezależne pokoje|4 oddzielone pokoje|4 oddzielne pokoje|4 osobne pokoje|4 osobne pok.|4 niezal. pokoje|4 niezależnymi pokojami|\\bczteropok|\\bcztery pok|\\bcztery" = 4,
    "5\\s*-?pok\\.?|5pokoje|5 pokoje|5-pokoje|5 - pokojowe|5-pokojowe|5 pokojowe|5 - pokojowy|5-pokojowy|5 pokojowy|5pokojowe|5pokojowy|5 - pok|5pokoj|5 duże pok.|5 przestronne pokoje|5 niezależne pokoje|5 oddzielone pokoje|5 oddzielne pokoje|5 osobne pokoje|5 osobne pok.|5 niezal. pokoje|5 niezależnymi pokojami|\\bpięciopok" = 5
  )
  
  match_room_number <- function(title) {
    for (pattern in names(patterns_to_numbers)) {
      if (str_detect(title, pattern)) {
        return(patterns_to_numbers[pattern])
      }
    }
    return(NA_integer_)
  }
  sapply(ad_title, match_room_number)  
}

all_data_regex$flat_rooms_cl <- extract_rooms(all_data_regex$ad_title)
all_data_regex <- all_data_regex %>%
  mutate(flat_rooms = if_else(!is.na(flat_rooms_cl), flat_rooms_cl, flat_rooms)) %>%
  select(-flat_rooms_cl)

#flat_area
all_data_regex <- all_data_regex %>%
  mutate(flat_area_cl = str_extract(
    ad_title,
    "(?<!r|mc)\\b(?<!\\d[,.])\\d+[,.]?\\d*\\s?(?=m2|m²|mkw|mkw\\.|m kw\\.|m\\.|m|m,2|sqm)(?!\\d)"
  )) %>%
  mutate(flat_area_cl = str_replace_all(flat_area_cl, ",", ".")) %>%
  mutate(flat_area_cl = as.numeric(flat_area_cl))
all_data_regex <- all_data_regex %>%
  mutate(flat_area_cl = ifelse(flat_area_cl >= 200 | flat_area_cl < 7, NA, flat_area_cl))

all_data_regex <- all_data_regex %>%
  mutate(flat_area = if_else(!is.na(flat_area_cl), flat_area_cl, flat_area)) %>%
  select(-flat_area_cl)

#price
all_data_regex <- all_data_regex %>%
  mutate(price_cl = str_extract(ad_title, "\\b\\d*[0]{2}\\b")) %>%
  mutate(price_cl = as.numeric(price_cl))  # Konwersja wyodrębnionego tekstu na numeryczne
#usuniecie wartosci ponizej 700 z price_cl
all_data_regex <- all_data_regex %>%
  mutate(price_cl = ifelse(price_cl < 700, NA, price_cl))

all_data_regex <- all_data_regex %>%
  mutate(price = coalesce(price, price_cl)) %>%
  select(-price_cl)  

#6.3 zmienne kategoryczne (dzielnice)
#quarter
correct_quarters <- read.csv("C:/Users/kamil/ProjektADN/daneNiekompletneR/correct_quarters_Poznan.csv")
quarters <- unique(c(all_data_regex$quarter, correct_quarters$Correct_quarter))
quarters <- na.omit(quarters)  # Remove NA values

# Wczytywanie nazw dzielnic z pliku do wektora
all_quarters <- readLines("C:/Users/kamil/ProjektADN/daneNiekompletneR/dzielnice_chat.txt")

# Tworzenie nowej kolumny 'quarter_cl' w ramce danych
all_data_regex$quarter_cl <- all_data_regex$quarter

# Funkcja do wyszukiwania nazwy dzielnicy w tytule ogłoszenia
extract_quarter <- function(title, quarters) {
  for (quarter in quarters) {
    if (grepl(quarter, title, fixed = TRUE)) {
      return(quarter)
    }
  }
  return(NA)
}

# Zastosowanie funkcji do każdego wiersza w ramce danych
all_data_regex$quarter_cl <- ifelse(is.na(all_data_regex$quarter_cl),
                                    sapply(all_data_regex$ad_title, extract_quarter, quarters = quarters),
                                    all_data_regex$quarter_cl)

all_data_regex <- all_data_regex %>%
  mutate(quarter = coalesce(quarter, quarter_cl)) %>%
  select(-quarter_cl)  

#poprawa nazwy dzielnic na bardziej poprawne
install.packages("readxl")
library(readxl)
district_mapping <- read_excel("C:/Users/kamil/ProjektADN/daneNiekompletneR/Dzielnice_Poznan.xlsx")

# Przekształć ramkę danych district_mapping w listę dla łatwego mapowania
district_list <- setNames(district_mapping$obszar, district_mapping$dzielnica_potocznie)

# Dodaj nową kolumnę quarter_cl_2 z poprawnymi nazwami dzielnic
all_data_regex <- all_data_regex %>%
  mutate(quarter_cl_2 = recode(quarter, !!!district_list))

all_data_regex <- all_data_regex %>%
  mutate(quarter_cl_2 = coalesce(quarter, quarter_cl_2)) %>%
  select(-quarter_cl_2)

gg_miss_var(all_data_edit)
gg_miss_var(all_data_regex)
vis_miss(all_data_regex, sort_miss = TRUE, cluster=TRUE) #liczba niepełnych kolumn zwiększyła się do 14

