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

iris |> dynamicCF(classColumn=5, consensus = TRUE) -> filtro5a
filtro5a$remIdx
iris[filtro5a$remIdx,]

iris |> dynamicCF(classColumn=5, consensus = FALSE) -> filtro5b
filtro5b$remIdx
iris[filtro5b$remIdx,]

iris |> dynamicCF(classColumn=5, consensus = FALSE, m = 7) -> filtro5c
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

# Conjunto de datos de 336 filasy 9 columnas referidas a sitios de localización de porteínas
# Más información en https://archive.ics.uci.edu/ml/datasets/Ecoli

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
  geom_boxplot(fill = "gold",outlier.colour = "darkblue", outlier.shape = 8, outlier.size = 1.5)+
  xlim(-1,1) + theme_minimal()
datos |> select(-type) |> 
  pivot_longer(cols = colnames(datos)[-c(1,7)],
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

datos |> select(aac,gvh) |> bagplot(pch=16,cex=2,show.outlier=TRUE)
datos |> select(alm1,alm2) |> bagplot(pch=16,cex=2,show.outlier=TRUE)

datos |> ggbetweenstats(x = type, y = aac, outlier.tagging = TRUE) 

