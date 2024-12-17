data_for_imputation <- all_data_regex
summary(data_for_imputation)

vis_miss(data_for_imputation, sort_miss = TRUE, cluster=TRUE)
gg_miss_case(data_for_imputation)
gg_miss_var(data_for_imputation)

#usuniecie kolumny z tytulami ad_title

all_data_for_imputation <- data_for_imputation %>% select(-ad_title)

#k najbliższych sasiadow dla dzielnicy
vars_by_NAs <- all_data_for_imputation %>%
  is.na() %>%
  colSums() %>%
  sort(decreasing = FALSE) %>%
  names()

all_data_for_imputation <- all_data_for_imputation %>%
  select(all_of(vars_by_NAs)) %>%
  kNN(k=1, variable = c("quarter"))

gg_miss_var(all_data_for_imputation)

###################################tutaj
#regresja logistyczna dla niektóych zmiennych binarnych: flat_furnished, flat_internet, flat_closed_area
#flat_furnished
all_data_for_imputation_II <- all_data_for_imputation
missing_flat_furnished <- is.na(all_data_for_imputation$flat_furnished)
logreg_model <- glm(flat_furnished ~ flat_washmachine + flat_fridge + flat_cooker,
                    data = all_data_for_imputation_II, family = binomial)
preds <- predict(logreg_model, type="response")
preds  <- rbinom(length(preds), size = 1, prob = preds)
all_data_for_imputation_II[missing_flat_furnished, "flat_furnished"] <- preds[missing_flat_furnished]

gg_miss_var(all_data_for_imputation_II)

#flat_internet
missing_flat_internet <- is.na(all_data_for_imputation$flat_internet)
logreg_model <- glm(flat_internet ~ flat_television,
                    data = all_data_for_imputation_II, family = binomial)
preds <- predict(logreg_model, type="response")
preds  <- rbinom(length(preds), size = 1, prob = preds)
all_data_for_imputation_II[missing_flat_internet, "flat_internet"] <- preds[missing_flat_internet]

gg_miss_var(all_data_for_imputation_II)

#flat_closed_area
missing_closed_area <- is.na(all_data_for_imputation$flat_closed_area)
logreg_model <- glm(flat_closed_area ~ flat_monitoring,
                    data = all_data_for_imputation_II, family = binomial)
preds <- predict(logreg_model, type="response")
preds  <- rbinom(length(preds), size = 1, prob = preds)
all_data_for_imputation_II[missing_closed_area, "flat_closed_area"] <- preds[missing_closed_area]

gg_miss_var(all_data_for_imputation_II)

#knn dla zmiennych binarnych w których nadal są braki
all_data_for_imputation_III <- kNN(all_data_for_imputation_II, k=5, variable = c(
  "flat_for_students", "flat_dishwasher", "flat_garden", "flat_garage", "flat_balcony",
  "flat_internet", "flat_closed_area", "flat_furnished", "individual"
))

gg_miss_var(all_data_for_imputation_III)
##################
all_data_for_imputation_III <- all_data_for_imputation_III %>% select(
  -flat_furnished_imp, -flat_closed_area_imp, -flat_internet_imp, -flat_balcony_imp,
  -individual_imp, -flat_garage_imp, -flat_garden_imp, -flat_dishwasher_imp, -flat_for_students_imp,
  -quarter_imp)

names(all_data_for_imputation_III)

summary(all_data_for_imputation_III)

all_data_for_imputation_IV <- all_data_for_imputation_III

cols_to_change <- c("flat_deposit", "flat_utility_room", "flat_basement",
                    "flat_tarrace", "flat_lift", "flat_two_level", "flat_kitchen_sep",
                    "flat_air_cond", "flat_nonsmokers", "flat_washmachine", "flat_fridge",
                    "flat_cooker", "flat_oven", "flat_television", "flat_anti_blinds",
                    "flat_monitoring", "individual", "flat_garden", "flat_dishwasher",
                    "flat_for_students", "flat_closed_area", "flat_internet", "flat_balcony", "flat_garage")  
all_data_for_imputation_IV[cols_to_change] <- lapply(all_data_for_imputation_IV[cols_to_change], as.numeric)
  

#RESZTA LASEM LOSOWYM 
library(missForest)

#trzeba wybrac zmienne factor i numeryczne
names(all_data_for_imputation_IV)

data_for_rf <- all_data_for_imputation_IV %>% select(-date_activ, -date_modif, -date_expire, -quarter)

imp_res <- missForest(data_for_rf)
data_for_rf_imp <- imp_res$ximp
data_for_rf_imp %>% is.na() %>% colSums()
imp_res$OOBerror

gg_miss_var(data_for_rf_imp)

data_for_rf_imp
any_miss(data_for_rf_imp)


# Wyodrębniamy obserwacje z data_test na podstawie id
result_6 <- data_for_rf_imp[data_for_rf_imp$id %in% data_test$id, c("id", "price")]

# Sortujemy dane zgodnie z kolejnością z data_test
result_6 <- result_6[order(match(result_6$id, data_test$id)), ]

# Dodajemy kolumnę z numeracją porządkową
result_6$order <- 1:nrow(result_6)

# Wybieramy tylko dwie kolumny: order i price
final_result <- result_6[, c("order", "price")]

# Zapisujemy wynik do pliku CSV
write.csv(final_result, "wynik6.csv", row.names = FALSE)
