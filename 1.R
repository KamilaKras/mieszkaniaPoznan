#BIBLIOTEKI
library(VIM)
library(naniar)
library(panelView)
library(ggplot2)
library(data.table)
library(dplyr)
library(xtable)
library(stringr)
pdf.options(encoding = "CP1250")

#1 - Wczytanie danych
data_train <- read.csv("C:/Users/kamil/ProjektADN/daneNiekompletneR/pzn-rent-train.csv")
head(data_train)

data_test <- read.csv("C:/Users/kamil/ProjektADN/daneNiekompletneR/pzn-rent-test.csv")
head(data_test)
names(data_train)
names(data_test)
#w data_train jest jedna kolumna wiecej niż w data_test -> price

#2 - polaczenie danych treningowych i testowych w jeden zbiór

all_data <- bind_rows(data_train, data_test)

# Sprawdzenie czy wszystkie id są unikalne
is_unique <- length(all_data$id) == length(unique(all_data$id))
if (is_unique) {
  print("Wszystkie id są unikalne.")
} else {
  print("Są zduplikowane wartości w kolumnie id.")
}
#Wynik: Wszystkie id są unikalne

#3 - Podstawowe informacje o brakach w pełnym zbiorze danych
#tablice i liczby
head(all_data)
summary(all_data) #trzeba popracować nad typami danych, brakami danych, outlierami
n_miss(all_data) #liczba brakujacych danych przed czyszczeniem: 29768
n_complete(all_data) 
prop_miss(all_data) #proporcja brakujących danych przed czyszczeniem: ok. 5,1%
miss_case_table(all_data) #braki danych dla obserwacji
#wizualizacja
vis_miss(all_data, sort_miss = TRUE) 
gg_miss_var(all_data) # Zidentyfikowano 11 kolumn z brakami danych (nie licząc ceny z danych testowych)
gg_miss_case(all_data)


