library(pacman)

# Detección de ruido usando NoiseFilters ----------------------------------

p_load(NoiseFiltersR)

data(iris)

iris |> C45robustFilter(classColumn=5) -> filtro1
C45robustFilter(Species~.,iris) -> filtro1
filtro1$cleanData
filtro1$cleanData |> nrow(); iris |> nrow()
filtro1$remIdx
iris[filtro1$remIdx,]

iris |> C45votingFilter(classColumn=5,consensus = TRUE) -> filtro2a
filtro2a$remIdx
iris[filtro2a$remIdx,]

iris |> C45votingFilter(classColumn=5,consensus = FALSE) -> filtro2b
filtro2b$remIdx
iris[filtro2b$remIdx,]

iris |> C45iteratedVotingFilter(classColumn=5) -> filtro3
filtro3$remIdx
iris[filtro3$remIdx,]

iris |> CVCF(classColumn=5) -> filtro4
filtro4$remIdx
iris[filtro4$remIdx,]

iris |> dynamicCF(classColumn=5, consensus = TRUE) -> filtro5a # consenso
filtro5a$remIdx
iris[filtro5a$remIdx,]

iris |> dynamicCF(classColumn=5, consensus = FALSE) -> filtro5b # mayoría
filtro5b$remIdx
iris[filtro5b$remIdx,]

iris |> dynamicCF(classColumn=5, consensus = FALSE, m = 9) -> filtro5c
filtro5c$remIdx
filtro5c$extraInf
iris[filtro5c$remIdx,]

iris |> edgeBoostFilter(classColumn=5, consensus = TRUE) -> filtro6a
filtro6a$remIdx
iris[filtro6a$remIdx,]

iris |> edgeBoostFilter(classColumn=5, consensus = FALSE) -> filtro6b
filtro6b$remIdx
iris[filtro6b$remIdx,]

iris |> EF(classColumn=5) -> filtro7
filtro7$remIdx
iris[filtro7$remIdx,]

iris |> hybridRepairFilter(classColumn=5, consensus = FALSE, noiseAction = "remove") -> filtro8a
filtro8a$remIdx
filtro8a$cleanData |> str()

iris |> hybridRepairFilter(classColumn=5, consensus = FALSE, noiseAction = "repair") -> filtro8b
filtro8b$repIdx
filtro8b$cleanData |> str()

iris |> classifSF(classColumn=5) -> filtro9 # tarda
filtro9$remIdx
filtro9$cleanData |> str()

# Detección de outliers ---------------------------------------------------

# ================ #
# 04 - datos1.data #
# ================ #

# Conjunto de datos de 336 filas y 9 columnas referidas a sitios de localización 
# de proteínas. Más información en https://archive.ics.uci.edu/ml/datasets/Ecoli

read.table('04 - datos1.data') -> datos 

p_load(skimr)
datos |> skim()

datos |>
  dplyr::rename(
    Sequence = 1,
    mcg = 2,
    gvh = 3,
    lip = 4,
    chg = 5,
    aac = 6,
    alm1 = 7,
    alm2 = 8,
    type = 9
  ) |>
  dplyr::select(-lip, -chg) -> datos

# Gráficos exploratorios --------------------------------------------------

p_load(dplyr,psych,ggplot2,univOutl,purrr,aplpack,tidyr,ggstatsplot)

datos |> multi.hist() # error
datos |> select_if(is.numeric) |> multi.hist(global=FALSE)
datos |> select_if(is.numeric) |> multi.hist(global=FALSE,
                                             bcol = "gold", dcol = "darkblue")

datos$aac |> fivenum()
datos |> select(aac) |> pull() |> fivenum()
datos |> select_if(is.numeric) |> apply(2,fivenum) 
datos |> select_if(is.numeric) |> map(fivenum)

datos |> select(aac) |> boxplot()
datos |> ggplot(aes(y=aac))+
  geom_boxplot(fill = "gold",outlier.colour = "darkblue", outlier.shape = 8, 
               outlier.size = 0.5)+
  xlim(-1,1) + theme_minimal()
datos |> select(-type,-Sequence) %>%
  pivot_longer(cols = colnames(.),
               names_to = "variable",
               values_to = "value") |> 
  ggplot(aes(x = variable, y = value)) +  
  geom_boxplot(fill="gold")+
  theme_minimal()

datos |> select(aac) |> pull() |> boxB() -> result
result$outliers
datos |> select_if(is.numeric) |> map(.f=boxB) -> resultados_box
resultados_box$aac$outliers
resultados_box$gvh$outliers
resultados_box$alm2$outliers
resultados_box$mcg$outliers
resultados_box$alm1$outliers

datos |> select(aac,gvh) |> bagplot(pch=16,cex=2,show.outlier=TRUE)
datos |> select(alm1,alm2) |> bagplot(pch=16,cex=2,show.outlier=TRUE)

datos |> ggbetweenstats(x = type, y = aac, outlier.tagging = TRUE) 

# Filtros -----------------------------------------------------------------

zscore = function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm))/sd(x, na.rm = na.rm) > thres}    
datos |> select(aac) |> pull() |> zscore()
datos |> select_if(is.numeric) |> map(.f=zscore) -> res_zscore
which(res_zscore$aac==TRUE)
which(res_zscore$gvh==TRUE)
which(res_zscore$alm2==TRUE)

hampel = function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm))/stats::mad(x, na.rm = na.rm) > thres}
datos |> select_if(is.numeric) |> map(.f=hampel) -> res_hampel
which(res_hampel$aac==TRUE)
which(res_hampel$gvh==TRUE)
which(res_hampel$alm2==TRUE)

tukey = function(x, k = 1.5, na.rm = TRUE) {
  cuan = quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = na.rm)
  iqr  = diff(cuan)
  (x < cuan[1] - k * iqr) | (x > cuan[3] + k * iqr)}
datos |> select_if(is.numeric) |> map(.f=tukey) -> res_tukey
which(res_tukey$aac==TRUE)
which(res_tukey$gvh==TRUE)
which(res_tukey$alm2==TRUE)
which(tukey(datos$aac))

# Prueba de Grubbs --------------------------------------------------------

p_load(outliers)
#install.packages("outliers")
datos |> select(aac) |> pull() |> grubbs.test(opposite = FALSE) # min
datos |> select(aac) |> pull() |> grubbs.test(opposite = TRUE)
min(datos$aac);median(datos$aac);max(datos$aac)
datos |> select(gvh) |> pull() |> grubbs.test(opposite = FALSE)
datos |> select(gvh) |> pull() |> grubbs.test(opposite = TRUE)
min(datos$gvh);median(datos$gvh);max(datos$gvh)
datos |> select_if(is.numeric) |> apply(2,grubbs.test,opposite = FALSE) -> res_grubbs1
datos |> select_if(is.numeric) |> apply(2,grubbs.test,opposite = TRUE) -> res_grubbs2
res_grubbs1$aac;res_grubbs2$aac
res_grubbs1$gvh;res_grubbs2$gvh
res_grubbs1$alm2;res_grubbs2$alm2

# Prueba de Dixon ---------------------------------------------------------

datos |> select(mcg)  |> pull() |> dixon.test()
set.seed(2222)
datos |> sample_n(25) -> datos_parte
datos_parte |> select(aac) |> pull() |> dixon.test(opposite = FALSE)
datos_parte |> select(aac) |> pull() |> dixon.test(opposite = TRUE)
min(datos_parte$aac);median(datos_parte$aac);max(datos_parte$aac)
datos_parte |> select_if(is.numeric) |> map(.f=dixon.test, opposite=FALSE)
datos_parte |> select_if(is.numeric) |> map(.f=dixon.test, opposite=TRUE)

# Prueba de Rosner --------------------------------------------------------

p_load(EnvStats)

boxplot.stats(datos$aac)$out |> length() -> n_outliers
datos |> select(aac) |> pull() |> rosnerTest(n_outliers) -> prueba
prueba
prueba$all.stats |> filter(Outlier==TRUE)

boxplot.stats(datos$gvh)$out |> length() -> n_outliers
datos |> select(gvh) |> pull() |> rosnerTest(n_outliers) -> prueba
prueba$all.stats |> filter(Outlier==TRUE)

length(boxplot.stats(datos$alm2)$out) -> n_outliers
datos |> select(alm2) |> pull() |> rosnerTest(n_outliers) -> prueba

# Distancia de Mahalanobis ------------------------------------------------

p_load(mvoutlier)

set.seed(222)
data.frame(x=rnorm(50),y=rnorm(50))-> datos_random
datos_random |> aq.plot() -> reporte
reporte$outliers

set.seed(222)
data.frame(x=rnorm(50),y=rnorm(50),z=rnorm(50)) -> datos_random
datos_random |> aq.plot() -> reporte
reporte$outliers
which(reporte$outliers)
datos_random[reporte$outliers,]

datos |> select_if(is.numeric) |> data.frame() |> aq.plot() -> reporte
reporte$outliers
which(reporte$outliers==TRUE)

p_load(tidymodels)
datos |> 
  recipe(.~.) |> 
  step_BoxCox(all_numeric()) |> 
  prep() |> 
  juice()  -> datos_boxcox
datos_boxcox |> select_if(is.numeric) |> aq.plot() -> reporte_boxcox
which(reporte_boxcox$outliers==TRUE)

# Winsorizing -------------------------------------------------------------

p_load(DescTools)

datos |> pull(aac) |> hist()
datos |> pull(aac) |> Winsorize() |> hist()

datos |> pull(aac) |> fivenum()
datos |> pull(aac) |> Winsorize() |> fivenum()

datos |> mutate(aac2 = Winsorize(aac)) 

datos |> transmute(aac2 = Winsorize(aac)) 

datos |> select_if(is.numeric) |> transmute_all(~Winsorize(.))

datos |> select_if(is.numeric) |> transmute_all(~Winsorize(.)) |> multi.hist(global=FALSE, bcol = "gold", dcol = "darkblue")

datos |> select_if(is.numeric) |> multi.hist(global=FALSE, bcol = "gold", dcol = "darkblue")

# Detección de duplicados -------------------------------------------------

x = c(1,1,1,1,2,2,2,3,3,3,3,7,4,5,6)
y = c(1,1,2,2,3,4,5,6,7,7,7,7,7,8,8)
d = data.frame(x,y)
d
d |> distinct()
d |> distinct(x)
d |> distinct(x,.keep_all = TRUE)
d |> unique()