
# kNN ---------------------------------------------------------------------

library(dplyr)
data.frame(y  = c(0,0,1,1,NA,NA),
           x1 = c(1,2,5,3,6,2),
           x2 = c(3,4,17,10,12,5)) |> 
  mutate(y = as.factor(y)) -> datos

class::knn(train = datos[1:4,2:3], 
           test  = datos[5:6,2:3], 
           k     = 3, 
           cl    = datos[1:4,]$y)

# Muestreo ----------------------------------------------------------------

datos7 <- read.csv('07 - datos1.csv') # https://archive.ics.uci.edu/ml/datasets/ionosphere
datos7 |> glimpse()

set.seed(555);datos7 |> sample_n(150) -> datos7_sample1
set.seed(555);datos7 |> slice_sample(n=150) -> datos7_sample2
identical(datos7_sample1,datos7_sample2)
set.seed(444);datos7 |> sample_frac(0.3) -> datos7_sample3
set.seed(444);datos7 |> slice_sample(prop=0.3) -> datos7_sample4
identical(datos7_sample3,datos7_sample4)
set.seed(333);datos7 |> group_by(Class) |> slice_sample(n=4) -> datos7_sample5

# Selección de prototipos -------------------------------------------------

# Técnicas
# 1 = Condensed Nearest Neighbor (CNN)
# 2 = Tomek Condensed Nearest Neighbor (Tomerk CNN)
# 3 = Reduced Nearest Neighbor (RNN)
# 4 = Edited Nearest Neighbor (ENN)
# 5 = Multiedit

# Paquetes
# 1 = class
# 2 = UBL

# Condensed Nearest Neighbor ----------------------------------------------

datos7 |> dim()

library(class)
set.seed(555)
prototipos_11 = condense(datos7 |> dplyr::select(-Class), datos7$Class) 
datos7_11     = datos7 |> slice(prototipos_11)
absorbentes_11 = setdiff(datos7 |> rownames() |> as.numeric(), prototipos_11) 
datos7_11 |> dim()

library(UBL)
mod_12        = CNNClassif(Class~., datos7, Cl = "all") 
datos7_12     = mod_12[[1]] 
prototipos_12 = datos7_12 |> rownames() |> as.numeric() |> sort() 
absorbentes_12 = setdiff(datos7 |> rownames() |> as.numeric(), prototipos_12)
datos7_12 |> dim()

# Tomek Condensed NearestNeighbor -----------------------------------------

library(UBL)
mod_22        = TomekClassif(Class~., datos7, dist = "Canberra") 
datos7_22     = mod_22[[1]] 
prototipos_22 = datos7_22 |> rownames() |> as.numeric() |> sort() 
absorbentes_22 = setdiff(datos7 |> rownames() |> as.numeric(), prototipos_22) 
datos7_22 |> dim()

# RNN ---------------------------------------------------------------------

library(class)
set.seed(555)
mod_cnn       = condense(datos7 |> dplyr::select(-Class), datos7$Class) 
prototipos_31 = reduce.nn(datos7 |> dplyr::select(-Class), mod_cnn, datos7$Class) 
datos7_31     = datos7 |> slice(prototipos_31)
absorbente_31  = setdiff(datos7 |> rownames() |> as.numeric(), prototipos_31) 

# ENN ---------------------------------------------------------------------

library(UBL)
mod_42        = ENNClassif(Class~., datos7, k = 3, dist = "Euclidean", Cl = "all") 
datos7_42     = mod_42[[1]]
prototipos_42 = datos7_42 |> rownames() |> as.numeric() |> sort()
absorbentes_42 = setdiff(datos7 |> rownames() |> as.numeric(),prototipos_42) 
datos7_42 |> dim()

# Multiedit ---------------------------------------------------------------

library(class)
mod_52        = multiedit(datos7 |> dplyr::select(-Class), datos7$Class, k = 1)
datos7_52     = datos7[mod_52,] 
permanecen_52 = datos7_52 |> rownames() |> as.numeric() |> sort() 
absorbentes_52 = setdiff(datos7 |> rownames() |> as.numeric(),
                       datos7_52 |> rownames() |> as.numeric()) 

