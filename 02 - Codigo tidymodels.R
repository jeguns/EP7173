library(readxl)
library(tidyverse)
library(tidymodels)

datos = read_excel('02 - datos_tidy.xlsx')
datos

### --> Limpieza con dplyr o lubridate <--- ###

datos |> initial_split(prop = 0.75, strata = RECOM) -> division

division |> training() -> training_set

division |> testing() -> testing_set

training_set |> 
  recipe(RECOM ~ .)

training_set |> 
  recipe(RECOM ~ .) |> 
  step_date(F_NAC,F_INGR,features = c("dow", "month", "year"))

training_set |> 
  recipe(RECOM ~ .) |> 
  step_date(F_NAC,F_INGR,features = c("semester","quarter")) |> 
  prep() -> receta

receta

receta |> juice()

training_set |> 
  recipe(RECOM ~ .) |> 
  step_num2factor(TIPO_CLIENTE,
                  transform = function(x) x,
                  levels = c("REGULAR","FRECUENTE","VIP")) |> 
  prep() |> 
  juice()

training_set |> 
  recipe(RECOM ~ .) |> 
  step_indicate_na(all_predictors(),all_outcomes()) |> 
  prep() |> 
  juice() 

training_set |> 
  recipe(RECOM ~ .) |> 
  step_num2factor(TIPO_CLIENTE,
                  transform = function(x) x,
                  levels = c("REGULAR","FRECUENTE","VIP")) |> 
  step_unknown(TIPO_CLIENTE, new_level = "NO CLASIFICADO") |>
  prep() |> 
  juice() 

training_set |> 
  recipe(RECOM ~ .) |> 
  step_num2factor(TIPO_CLIENTE,
                  transform = function(x) x,
                  levels = c("REGULAR","FRECUENTE","VIP")) |> 
  step_unknown(TIPO_CLIENTE, new_level = "NO CLASIFICADO") |>
  step_relevel(TIPO_CLIENTE, ref_level = "VIP") |> 
  prep() |> 
  juice() |> 
  str()

training_set |> 
  recipe(RECOM ~ .) |> 
  step_num2factor(TIPO_CLIENTE,
                  transform = function(x) x,
                  levels = c("REGULAR","FRECUENTE","VIP")) |> 
  step_range(all_numeric(),-all_outcomes()) |> 
  prep() |> 
  juice() 

training_set |> 
  recipe(RECOM ~ INGRESO) |> 
  step_normalize(all_numeric(),-all_outcomes()) |> 
  prep() |> 
  juice() 

training_set |> 
  recipe(RECOM ~ INGRESO) |> 
  step_center(all_numeric(),-all_outcomes()) |> 
  step_scale(all_numeric(),-all_outcomes()) |> 
  prep() |> 
  juice() 

training_set |> 
  recipe(RECOM ~ INGRESO) |> 
  step_filter(INGRESO>2000 & INGRESO<3000) |> 
  prep() |> 
  juice() 

training_set |> 
  recipe(RECOM ~ .) |> 
  step_sample(size = 3) |> 
  prep() |> 
  juice() 

training_set |> 
  recipe(RECOM ~ .) |> 
  step_sample(size = 0.5) |> 
  prep() |> 
  juice() 

training_set |> 
  recipe(RECOM ~ .) |> 
  step_zv(all_predictors()) |> 
  prep() |> 
  juice() 

training_set |> 
  recipe(RECOM ~ .) |> 
  step_naomit(all_outcomes()) |> 
  prep() |> 
  juice() 

training_set |> 
  recipe( ~ .) |> 
  step_select(all_numeric(),-starts_with("NUM")) |> 
  prep() |> 
  juice() 

training_set |> 
  recipe( ~ .) |> 
  step_select(all_numeric(),-starts_with("NUM")) |> 
  check_missing(all_predictors()) |> 
  prep() |> 
  juice() 

training_set |> 
  recipe( ~ .) |> 
  step_select(all_numeric(),-starts_with("NUM")) |> 
  step_naomit(all_numeric()) |> 
  check_missing(all_predictors()) |> 
  prep() |> 
  juice() 
