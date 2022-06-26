
library(pacman)
p_load(readr, readxl, dplyr, ggplot2, ROSE, UBL, imbalance, caret, yardstick, DescTools)
# ROSE: https://cran.r-project.org/web/packages/ROSE/ROSE.pdf (2021)
# UBL : https://cran.r-project.org/web/packages/UBL/UBL.pdf (2021)
# imbalance: https://cran.r-project.org/web/packages/imbalance/imbalance.pdf (2020)
library(unbalanced)
# unbalanced: https://cran.r-project.org/web/packages/unbalanced/index.html

# Introducción ------------------------------------------------------------

# ============== #
# 09 - datos.csv #
# ============== #

# Este conjunto de datos se refiere a donantes de sangre, en el cual se
# registran las siguientes variables:
# V1 = Antiguedad: meses desde la última donación. 
# V2 = Frecuencia: número total de donaciones. 
# V3 = Monetario: sangre total donada en CC. 
# V4 = Tiempo: meses desde la primera donación.
# V5 = Donó sangre (0=No, 1=Sí)

read.csv('09 - datos.csv',stringsAsFactors=TRUE) |> 
  mutate(V5 = as.factor(V5)) -> datos1
(datos1 |> count(V5) |> mutate(prop=n/sum(n)) -> tabla1)
(tabla1$prop[1] / (1-tabla1$prop[1]) -> IR1)

# ================ #
# 09 - datos2.xlsx #
# ================ #

# Este es un conjunto de datos sintético con 3 variables
# X1: Atributo numérico
# X2: Atributo numérico
# Y: Target (factor)

read_excel('09 - datos2.xlsx') |> 
  mutate(Y  = as.factor(Y)) -> datos2
(datos2 |> count(Y) |> mutate(prop=n/sum(n)) -> tabla2)
(tabla2$prop[1] / (1-tabla2$prop[1]) -> IR2)

datos2 |> 
  mutate(id = 1:nrow(datos2)) |> 
  ggplot(aes(x=X1,y=X2,color=Y,label=id))+
  geom_point(size=0.25)+
  geom_text(size=5)+
  theme_minimal()

# Random Undersampling ----------------------------------------------------

ovun.sample(V5~., data = datos1, method = "under")$data -> datos1_under1 # ROSE
datos1_under1 |> count(V5) |> mutate(prop=n/sum(n))
X11();plotComparison(datos1, datos9_1_under1, names(datos1), classAttr = "V5") # imbalance

ovun.sample(V5~., data = datos1, method = "under", N = 177*2)$data -> datos1_under2 
datos1_under2 |> count(V5) |> mutate(prop=n/sum(n))

ovun.sample(V5~., data = datos1, method = "under", p = 0.5)$data -> datos1_under3
datos1_under3 |> count(V5) |> mutate(prop=n/sum(n))

RandUnderClassif(V5~., dat = datos1, C.perc='balance') -> datos1_under4 # UBL
datos1_under4 |> count(V5) |> mutate(prop=n/sum(n))

RandUnderClassif(V5~., dat = datos1, C.perc='extreme') -> datos1_under5 
datos1_under5 |> count(V5) |> mutate(prop=n/sum(n))

RandUnderClassif(V5~., dat = datos1, C.perc=list('0'=0.3,'1'=1)) -> datos1_under6
datos1_under6 |> count(V5) |> mutate(prop=n/sum(n))

# ubUnder(X        = datos1 |> select(-V5), # unbalanced
#         Y        = datos1 |> pull(V5)) -> ub_under1 
# data.frame(ub_under1$X,ub_under1$Y) -> datos1_under7
# datos1_under7 |> count(ub_under1.Y) |> mutate(prop=n/sum(n))

# CNN ---------------------------------------------------------------------

set.seed(8)
CNNClassif(V5~., datos1, Cl = "1") -> cnn # UBL
cnn[[1]] -> datos1_cnn
datos1_cnn |> count(V5) |> mutate(prop=n/sum(n))
x11();plotComparison(datos1, datos1_cnn, names(datos1), classAttr = "V5") # imbalanced

# set.seed(8)
# ubCNN(X = datos1 |> select_if(is.numeric), # unbalanced
#       Y = datos1 |> pull(V5)) -> cnn_2
# data.frame(cnn_2$X,cnn_2$Y) -> datos1_cnn2
# datos1_cnn2 |> count(cnn_2.Y) |> mutate(prop=n/sum(n))

set.seed(15)
CNNClassif(Y~., data.frame(datos2), Cl = "1") -> cnn2 # UBL
cnn2[[1]] -> datos2_cnn
datos2_cnn |> count(Y) |> mutate(prop=n/sum(n))
plotComparison(datos2, datos2_cnn, names(datos2), classAttr = "Y") # imbalanced

# set.seed(15)
# ubCNN(X = datos2 |> select_if(is.numeric), # unbalanced
#       Y = datos2 |> pull(Y)) -> cnn2_2
# data.frame(cnn2_2$X,cnn2_2$Y) -> datos2_cnn2
# datos2_cnn2 |> count(cnn2_2.Y) |> mutate(prop=n/sum(n))
# plotComparison(datos2, datos2_cnn2 |> rename(Y=3), names(datos2), classAttr = "Y")

# Tomek Link --------------------------------------------------------------

set.seed(654)
TomekClassif(V5~., datos1, Cl = "0") -> tomek # UBL
tomek[[1]] -> datos1_tomek
datos1_tomek |> count(V5) |> mutate(prop=n/sum(n))

# set.seed(654)
# ubTomek(X = datos1 |> select_if(is.numeric), # unbalanced
#         Y = datos1 |> pull(V5)) -> tomek2
# data.frame(tomek2$X,tomek2$Y) -> datos1_tomek2
# datos1_tomek2 |> count(tomek2.Y) |> mutate(prop=n/sum(n))

set.seed(23435)
TomekClassif(Y~., data.frame(datos2), Cl = "0") -> tomek # UBL
tomek[[1]] -> datos2_tomek
datos2_tomek |> count(Y) |> mutate(prop=n/sum(n))

# OSS ---------------------------------------------------------------------

set.seed(78287)
OSSClassif(V5~., datos1, Cl = "1", start = "CNN") -> oss1 # UBL
oss1 -> datos1_oss1
datos1_oss1 |> count(V5) |> mutate(prop=n/sum(n))

set.seed(78287)
OSSClassif(V5~., datos1, Cl = "1", start = "Tomek") -> oss2 # UBL
oss2 -> datos1_oss2
datos1_oss2 |> count(V5) |> mutate(prop=n/sum(n))

# set.seed(78287)
# ubOSS(X = datos1 |> select_if(is.numeric), # unbalanced
#       Y = datos1 |> pull(V5)) -> oss3
# data.frame(oss3$X,oss3$Y) -> datos1_oss3
# datos1_oss3 |> count(oss3.Y) |> mutate(prop=n/sum(n))

# ENN ---------------------------------------------------------------------

set.seed(1000)
ENNClassif(V5~., datos1, Cl = "0") -> enn1 # UBL
enn1[[1]] -> datos1_enn1
datos1_enn1|> count(V5) |> mutate(prop=n/sum(n))

# set.seed(1000)
# ubENN(X = datos1 |> select_if(is.numeric), # unbalanced
#       Y = datos1$V5) -> enn2
# data.frame(enn2$X,enn2$Y) -> datos1_enn2
# datos1_enn2 |> count(enn2.Y) |> mutate(prop=n/sum(n))

set.seed(100)
ENNClassif(Y~., data.frame(datos2), Cl = "0") -> enn # UBL
enn[[1]] -> datos2_enn
datos2_enn|> count(Y) |> mutate(prop=n/sum(n))

# NCL ---------------------------------------------------------------------

set.seed(222)
NCLClassif(V5~., datos1, Cl = "1") -> datos1_ncl ## NCL
datos1_ncl |> count(V5) |> mutate(prop=n/sum(n))

# set.seed(222)
# ubNCL(X = datos1 |> select_if(is.numeric), # unbalanced
#       Y = datos1$V5) -> ncl2
# data.frame(ncl2$X,ncl2$Y) -> datos1_ncl2
# datos1_ncl2 |> count(ncl2.Y) |> mutate(prop=n/sum(n))

set.seed(984)
NCLClassif(Y~., data.frame(datos2), Cl = "1") -> datos2_ncl ## NCL
datos2_ncl |> count(Y) |> mutate(prop=n/sum(n))

# Oversampling ------------------------------------------------------------

ovun.sample(V5~., data = datos1, method = "over")$data -> datos1_over1 # ROSE
datos1_over1 |> count(V5) |> mutate(prop=n/sum(n))

ovun.sample(V5~., data = datos1, method = "over", N = 570*2)$data -> datos1_over2
datos1_over2 |> count(V5) |> mutate(prop=n/sum(n))

ovun.sample(V5~., data = datos1, method = "over", p = 0.5)$data -> datos1_over3
datos1_over3 |> count(V5) |> mutate(prop=n/sum(n))

RandOverClassif(V5~., dat = datos1, C.perc='balance') -> datos9_1_over4 # UBL
datos9_1_over4 |> count(V5) |> mutate(prop=n/sum(n))

RandOverClassif(V5~., dat = datos1, C.perc='extreme') -> datos9_1_over5
datos9_1_over5 |> count(V5) |> mutate(prop=n/sum(n))

RandOverClassif(V5~., dat = datos1, C.perc=list('0'=1,'1'=3)) -> datos9_1_over6
datos9_1_over6 |> count(V5) |> mutate(prop=n/sum(n))

# ubOver(X = datos1 |> select(-V5), # unbalanced
#        Y = datos1 |> pull(V5)) -> ub_over7
# data.frame(ub_over7$X,ub_over7$Y) -> datos1_over7
# datos1_over7 |> count(ub_over7.Y) |> mutate(prop=n/sum(n))

# SMOTE -------------------------------------------------------------------

SmoteClassif(V5~., datos1, C.perc='balance') -> smote1 # UBL
smote1 -> datos1_smote1
datos1_smote1 |> count(V5) |> mutate(prop=n/sum(n))

SmoteClassif(V5~., datos1, C.perc="extreme") -> smote2
smote2 -> datos1_smote2
datos1_smote2 |> count(V5) |>  mutate(prop=n/sum(n))

SmoteClassif(V5~., datos1, C.perc=list('0'=1,'1'=3)) -> smote3
smote3 -> datos1_smote3
datos1_smote3 |> count(V5) |> mutate(prop=n/sum(n))

# ubSMOTE(X          = datos1 |> select_if(is.numeric), # unbalanced
#         Y          = datos1 |> pull(V5),
#         perc.under = 100,
#         perc.over  = 100) -> smote4
# data.frame(smote4$X,smote4$Y) -> datos1_smote4
# datos1_smote4 |> count(smote4.Y) |> mutate(prop=n/sum(n))

# ROSE --------------------------------------------------------------------

ROSE(V5~., data = datos1)$data -> datos1_rose1 # ROSE
datos1_rose1 |> count(V5) |> mutate(prop=n/sum(n))

ovun.sample(V5~., data = datos1, N = 1000)$data -> datos1_rose2
datos1_rose2 |> count(V5) |> mutate(prop=n/sum(n))

ovun.sample(V5~., data = datos1, p = 0.5)$data -> datos1_rose3
datos1_rose3 |> count(V5) |> mutate(prop=n/sum(n))

# MWMOTE ------------------------------------------------------------------

mwmote(datos1, numInstances = 570-177, classAttr = "V5") -> datos1_mwmote # imbalance
datos1_mwmote |> count(V5) |> mutate(prop=n/sum(n))
datos1_mwmote |> rbind(datos1) -> datos1_mwmote
datos1_mwmote |> count(V5) |> mutate(prop=n/sum(n))

# Random Over & Under -----------------------------------------------------

ovun.sample(V5~., data = datos1, method = "both")$data -> datos1_both1 # ROSE
datos1_both1 |> count(V5) |> mutate(prop=n/sum(n))

ovun.sample(V5~., data = datos1, method = "both", N = 1000)$data -> datos1_both2
datos1_both2 |> count(V5) |> mutate(prop=n/sum(n))

ovun.sample(V5~., data = datos1, method = "both", p = 0.5)$data -> datos1_both3
datos1_both3 |> count(V5) |> mutate(prop=n/sum(n))

# Modelo ------------------------------------------------------------------

# Modelo con datos sin balancear

set.seed(555)
datos1$V5 |> createDataPartition(p = 0.8, list = FALSE) -> ind_train
datos1[ind_train,] -> train_unbal
datos1[-ind_train,] -> test_unbal
train_unbal |> count(V5) |> mutate(prop=n/sum(n))
test_unbal  |> count(V5) |> mutate(prop=n/sum(n))

modelo_unbal = glm(V5~.,train_unbal, family = binomial(link="logit"))
modelo_unbal |> 
  predict(newdata = test_unbal,type="response") |> 
  round() |> as.factor() -> predicciones_unbal
caret::confusionMatrix(data      = predicciones_unbal, # predicho (del modelo training),
                       reference = test_unbal$V5, # real (de testing)
                       positive  = "1") -> mat_conf_unbal
mat_conf_unbal
mat_conf_unbal$table
mat_conf_unbal$overall
mat_conf_unbal$byClass
data.frame(obs = test_unbal$V5, pred = predicciones_unbal) |> 
  conf_mat(obs,pred) |> 
  autoplot(type = "heatmap") +
  scale_fill_gradient(low = "gold", high = "gold4")

# Modelo con datos balanceados 

set.seed(555)
datos1$V5|> createDataPartition(p = 0.8, list = FALSE) -> ind_train_bal
datos1[ind_train_bal,] -> train_bal 
datos1[-ind_train_bal,] -> test_bal 
train_bal |> count(V5) |> mutate(prop=n/sum(n))
test_bal  |> count(V5) |> mutate(prop=n/sum(n))

SmoteClassif(V5~., train_bal, C.perc='balance') -> smote_bal # balancear los datos de entrenamiento
smote_bal |> count(V5) |> mutate(prop=n/sum(n))

modelo_bal = glm(V5~.,smote_bal, family = binomial(link="logit")) # construimos el modelo de entrenamiento
modelo_bal |> 
  predict(newdata = test_bal,type="response") |> 
  round() |> as.factor() -> predicciones_bal # predecimos a partir del modelo de entrenamiento
confusionMatrix(data      = predicciones_bal, # datos predichos
                reference = test_bal$V5, # datos testing
                positive  = "1") -> mat_conf_bal
mat_conf_bal
Lmat_conf_bal$table
mat_conf_bal$overall
mat_conf_bal$byClass
data.frame(obs = test_bal$V5, pred = predicciones_bal) |> 
  conf_mat(obs,pred) |> 
  autoplot(type = "heatmap") +
  scale_fill_gradient(low = "gold", high = "gold4")

