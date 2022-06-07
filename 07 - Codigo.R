
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

library(unbalanced)
data(ubIonosphere)  # https://archive.ics.uci.edu/ml/datasets/ionosphere
ubIonosphere -> datos7
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
# 6 = DROP

# Paquetes
# 1 = class
# 2 = UBL
# 3 = unbalanced
# 4 = NoiseFiltersR

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

library(unbalanced)
Y = datos7 |> pull(Class)
X = datos7 |> dplyr::select(-Class) 
set.seed(345)
mod_13        = ubCNN(X=X, Y=Y, k=1)
datos7_13     = cbind(mod_13$X, mod_13$Y)
prototipos_13 = datos7_13 |> rownames() |> as.numeric() |> sort() 
absorbentes_13 = setdiff(datos7 |> rownames() |> as.numeric(),prototipos_13)
datos7_13 |> dim()

# library(NoiseFiltersR)
# mod_14        = CNN(Class ~ ., data = datos7)
# datos7_14     = datos7[-mod_14$remIdx,];datos7_14 |> dim()
# prototipos_14 = mod_14$cleanData |> rownames() |> as.numeric()
# absorbentes_14 = mod_14$remIdx
# datos7_14 |> dim()

# Tomek Condensed NearestNeighbor -----------------------------------------

library(UBL)
mod_22        = TomekClassif(Class~., datos7, dist = "Canberra") 
datos7_22     = mod_22[[1]] 
prototipos_22 = datos7_22 |> rownames() |> as.numeric() |> sort() 
absorbentes_22 = setdiff(datos7 |> rownames() |> as.numeric(), prototipos_22) 
datos7_22 |> dim()

library(unbalanced)
mod_23        = ubTomek(X=X, Y=Y) 
datos7_23     = cbind(mod_23$X, mod_23$Y) 
absorbentes_23 = mod_23$id.rm
prototipos_23 = setdiff(datos7 |> rownames() |> as.numeric(), absorbentes_23)
datos7_23 |> dim()

# RNN ---------------------------------------------------------------------

library(class)
set.seed(555)
mod_cnn       = condense(datos7 |> dplyr::select(-Class), datos7$Class) 
prototipos_31 = reduce.nn(datos7 |> dplyr::select(-Class), mod_cnn, datos7$Class) 
datos7_31     = datos7 |> slice(prototipos_31)
absorbente_31  = setdiff(datos7 |> rownames() |> as.numeric(), prototipos_31) 

# library(NoiseFiltersR)
# mod_34        = RNN(Class ~., data = datos7) 
# datos7_34     = datos7[-mod_31$remIdx,]
# prototipos_34 = mod_34$cleanData |> rownames() |> as.numeric()  
# absorbentes_34 = mod_34$remIdx 

# ENN ---------------------------------------------------------------------

library(UBL)
mod_42        = ENNClassif(Class~., datos7, k = 3, dist = "Euclidean", Cl = "all") 
datos7_42     = mod_42[[1]]
prototipos_42 = datos7_42 |> rownames() |> as.numeric() |> sort()
absorbentes_42 = setdiff(datos7 |> rownames() |> as.numeric(),prototipos_42) 
datos7_42 |> dim()

# library(NoiseFiltersR)
# mod_44        = ENN(Class~., data = datos7, k = 3) 
# datos7_44     = mod_44$cleanData
# prototipos_44 = datos7_44 |> rownames() |> as.numeric() |> sort()
# absorbentes_44 = mod_44$remIdx
# datos7_44 |> dim()

# Multiedit ---------------------------------------------------------------

library(class)
mod_52        = multiedit(datos7 |> dplyr::select(-Class), datos7$Class, k = 1)
datos7_52     = datos7[mod_52,] 
permanecen_52 = datos7_52 |> rownames() |> as.numeric() |> sort() 
absorbentes_52 = setdiff(datos7 |> rownames() |> as.numeric(),
                       datos7_52 |> rownames() |> as.numeric()) 

# DROP1 -------------------------------------------------------------------

# library(NoiseFiltersR)
# a = Sys.time()
# mod_6A        = DROP1(Class~., data = datos7)
# b = Sys.time()
# datos7_6A     = mod_6A$cleanData
# prototipos_6A = datos7_6A |> rownames() |> as.numeric() |> sort()
# absorbentes_6A = mod_6A$remIdx
# 
# a = Sys.time()
# mod_6B        = DROP2(Class~., data = datos7)
# b = Sys.time()
# datos7_6B     = mod_6B$cleanData
# absorbentes_6B = mod_6B$remIdx
# permanecen_6B = datos7_6B |> rownames() |> as.numeric() |> sort()
# 
# a = Sys.time()
# mod_6C        = DROP3(Class~., data = datos7)
# b = Sys.time()
# datos7_6C     = mod_6C$cleanData
# prototipos_6C = datos7_6C |> rownames() |> as.numeric() |> sort()
# absorbentes_6C = mod_6C$remIdx
