
# Paquetes ----------------------------------------------------------------

library(readr)
library(caret)
library(dplyr)
library(tidymodels)
library(purrr)

# Lectura de datos --------------------------------------------------------

# El archivo 01 - admitidos.csv contiene datos acerca de los alumnos admitidos
# y los no admitidos a universidades estadounidenses, tiene 4 variables
# admit = 0 si no se le admite, 1 si es admitido
# gre = Graduate Record Examinations
# gpa = Grade Point Average
# rank = puede tomar el valor 1, 2, 3 o 4, siendo 1 el mayor prestigio y va 
# disminuyendo hacia 4

read_csv("01 - admitidos.csv") -> datos 

datos |> head()

datos$admit |> levels()

datos$admit = as.factor(datos$admit)

datos$admit |> levels()

datos |> 
  mutate(admit  = as.factor(admit),
         admit1 = recode_factor(admit,
                               "0"="no","1"="sí")) |> 
  relocate(admit1, .after = admit) -> datos

datos |> head()

# Particionamiento usando caret -------------------------------------------

(datos |> nrow() -> ntotal)

set.seed(564)
datos$admit1 |> createDataPartition(p = 0.8, list = FALSE, times = 1) -> caret_indices
(caret_indices |> length() -> ntrain)
0.8*ntotal

datos[caret_indices,] -> caret_train
datos[-caret_indices,] -> caret_test

caret_train |> dim()
caret_test |> dim()

caret_train |> count(admit) |> mutate(Porc = n/sum(n))
caret_test |> count(admit) |> mutate(Porc = n/sum(n))

## k folds con 1 repetición

createFolds(caret_train$admit, k = 10, returnTrain = FALSE) -> folds_val
folds_val$Fold01 |> length()
folds_val$Fold01
folds_val$Fold02
folds_val$Fold03
folds_val$Fold04
folds_val$Fold05
folds_val$Fold06
folds_val$Fold07
folds_val$Fold08
folds_val$Fold09
folds_val$Fold10

createFolds(caret_train$admit, k = 10, returnTrain = TRUE) -> folds_tra
folds_tra$Fold01 |> length()
folds_tra$Fold01
folds_tra$Fold02
folds_tra$Fold03
folds_tra$Fold04
folds_tra$Fold05
folds_tra$Fold06
folds_tra$Fold07
folds_tra$Fold08
folds_tra$Fold09
folds_tra$Fold10

# Usamos esta configuración al establecer los parámetros de control para el entrenamiento

trainControl(method = "cv", # validación cruzada
             number = 10,   # número de folds
             savePredictions = TRUE,
             classProbs = TRUE) -> caret_control_A

#train(..., trControl = caret_control_A, ...)

trainControl(index = folds_tra,
             savePredictions = TRUE,
             classProbs = TRUE) -> caret_control_B

#train(..., trControl = caret_control_B, ...)

## k folds con más de una repetición

createMultiFolds(caret_train$admit, k = 10, times = 1) -> folds_repe1
folds_repe1$Fold01
str(folds_repe1)
identical(folds_repe1$Fold01,folds_repe1$Fold01.Rep1)
 
createMultiFolds(caret_train$admit, k = 10, times = 2) -> folds_repe2
folds_repe2$Fold01
str(folds_repe2)
folds_repe2$Fold01.Rep1

set.seed(333)
createMultiFolds(caret_train$admit, k = 2, times = 5) -> folds_repe3
str(folds_repe3)
folds_repe3$Fold1.Rep1
folds_repe3$Fold2.Rep1
folds_repe3$Fold1.Rep2
folds_repe3$Fold2.Rep2
folds_repe3$Fold1.Rep3
folds_repe3$Fold2.Rep3
folds_repe3$Fold1.Rep4
folds_repe3$Fold2.Rep4
folds_repe3$Fold1.Rep5
folds_repe3$Fold2.Rep5

trainControl(method  = "repeatedcv", # validación cruzada repetida
             number  = 2, # número de folds
             repeats = 5,  # número de repeticiones
             savePredictions = "all",
             classProbs = TRUE) -> caret_control_C

#train(..., trControl = caret_control_C, ...)

trainControl(method  = "repeatedcv", # validación cruzada repetida
             index   = folds_repe3,
             savePredictions = "all",
             classProbs = TRUE) -> caret_control_D

#train(..., trControl = caret_control_D, ...)

# Particionamiento usando tidymodels --------------------------------------

(datos |> initial_split(strata = SEXO, prop = 0.8) -> tidy_split)
tidy_split |> training() -> tidy_train
tidy_split |> testing() -> tidy_test

tidy_train |> dim()
tidy_test |> dim()

tidy_train |> count(SEXO) |> mutate(Porc = n/sum(n))
tidy_test |> count(SEXO) |> mutate(Porc = n/sum(n))

tidy_train |> vfold_cv(v = 10, repeats = 1, strata = SEXO) -> folds
folds
folds$id
folds$splits

map_dbl(folds$splits,
        function(x) {
          dat <- as.data.frame(x)$SEXO
          mean(dat == "MASCULINO")
        })

datos |> vfold_cv(v = 10, repeats = 2, strata = SEXO) -> folds_rep2
folds_rep2
folds_rep2$id
folds_rep2$splits

map_dbl(folds_rep2$splits,
        function(x) {
          dat <- as.data.frame(x)$SEXO
          mean(dat == "MASCULINO")
        })

datos$SEXO |> table() |> prop.table()
