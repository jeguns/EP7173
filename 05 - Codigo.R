
# ================ #
# 05 - datos1.data #
# ================ #

# Se registran atributos a partir de imágenes digitalizadas de aspiración con
# aguja fina (FNA) de una masa mamaria.
# El número ID y el diagnóstico son atributos de cada registro, así como la media,
# el error estándar y la "peor" medición para 10 atributos en cada núcleo celular 
# (radio, textura, perímetro, área, suavidad, compacidad, concavidad, 
# número de concavidades, simetría, dimensión fractal)

# Lista de atributos:
# V1: ID Number
# V2: diagnosis: The diagnosis of breast tissues (M = malignant, B = benign)
# V3: radius_mean: mean of distances from center to points on the perimeter
# V4: texture_mean: standard deviation of gray-scale values
# V5: perimeter_mean: mean size of the core tumor
# V6: area_mean
# V7: smoothness_mean: mean of local variation in radius lengths
# V8: compactness_mean: mean of perimeter^2 / area - 1.0
# V9: concavity_mean: mean of severity of concave portions of the contour
# V10: concave points_mean: mean for number of concave portions of the contour
# V11: symmetry_mean
# V12: fractal_dimension_mean: mean for "coastline approximation" - 1
# V13: radius_se: standard error for the mean of distances from center to points on the perimeter
# V14: texture_se: standard error for standard deviation of gray-scale values
# V15: perimeter_se
# V16: area_se
# V17: smoothness_se: standard error for local variation in radius lengths
# V18: compactness_se: standard error for perimeter^2 / area - 1.0
# V19: concavity_se: standard error for severity of concave portions of the contour
# V20: concave points_se: standard error for number of concave portions of the contour
# V21: symmetry_se
# V22: fractal_dimension_se: standard error for "coastline approximation" - 1
# V23: radius_worst: "worst" or largest mean value for mean of distances from center to points on the perimeter
# V24: texture_worst: "worst" or largest mean value for standard deviation of gray-scale values
# V25: perimeter_worst
# V26: area_worst
# V27: smoothness_worst: "worst" or largest mean value for local variation in radius lengths
# V28: compactness_worst: "worst" or largest mean value for perimeter^2 / area - 1.0
# V29: concavity_worst: "worst" or largest mean value for severity of concave portions of the contour
# V30: concave points_worst: "worst" or largest mean value for number of concave portions of the contour
# V31: symmetry_worst
# V32: fractal_dimension_worst: "worst" or largest mean value for "coastline approximation" - 1
# Fuente: https://www.kaggle.com/uciml/breast-cancer-wisconsin-data

# Lectura -----------------------------------------------------------------

read.table('05 - datos1.data',sep=",") -> datos #(También puede leer el csv)

# Análisis exploratorio ---------------------------------------------------

library(pacman)
p_load(skimr, dplyr, psych, corrplot, ggcorrplot,REdaS,PerformanceAnalytics)

datos |> skim()

datos |> 
  select(-V1,-V2) -> datos

datos |> cor()

datos |> KMO()
datos |> cor() |> det()
datos |> select(V3,V7) |> cor() |> det()
datos |> select(V3,V5) |> cor() |> det()
datos |> bart_spher()
datos |> cortest.bartlett()
datos |> cor() |> cortest.bartlett(n=569)

datos |> cor() |> corrplot()
datos |> cor() |> corrplot(method = "color")
datos |> cor() |> corrplot(method = "ellipse")
datos |> cor() |> corrplot(order = "hclust")

datos |> cor() |> ggcorrplot(method="circle")
datos |> cor() |> ggcorrplot(hc.order=TRUE)

datos |> cor() |> heatmap()
datos |> cor() |> heatmap(col = colorRampPalette(c("blue", "white", "red"))(20))

datos |> chart.Correlation()
datos |> select(V3,V4,V5) |> chart.Correlation()
datos |> select(V3,V14,V17) |> chart.Correlation()

# Análisis de componentes principales -------------------------------------

datos |> prcomp() -> ComPri1
ComPri1 |> summary()
ComPri1 |> biplot()

datos |> prcomp(scale=TRUE) -> ComPri2
ComPri2 |> summary()
ComPri2 |> biplot()
ComPri2$center
ComPri2$scale
(ComPri2$sdev**2) |> sum()
ComPri2$rotation # cargas (coeficientes)
ComPri2$x # valores que se toman en los ejes (Scores)
ComPri2$x |> dim()
datos |> dim()
ComPri2 |> screeplot(type = "l", npcs = 15, main = "15 primeros componentes principales")
abline(h = 1, lty = 2, col = "darkblue")

p_load(factoextra)
ComPri2 |> fviz_eig(ncp=30, addlabels=TRUE) + 
  labs(x = "Dimensiones",
       y = "% de variabilidad explicada")

ComPri2 |> fviz_pca_ind(geom.ind = c("point","text"), repel = TRUE) +
  labs(title = "Scores de las observaciones",
       subtitle = "Dos componentes principales")

ComPri2 |> fviz_pca_ind(geom.ind = c("point","text"), repel = TRUE, 
                        axes = c(1,3)) +
  labs(title = "Scores de las observaciones",
       subtitle = "Componentes principales 1 y 3")

ComPri2 |> fviz_pca_var(repel = TRUE)
ComPri2 |> fviz_pca_var(axes = c(1,3),repel = TRUE)
ComPri2 |> fviz_pca_var(axes = c(19,20),repel = TRUE)
ComPri2 |> fviz_pca_var(axes = c(29,30),repel = TRUE)

ComPri2 |> fviz_pca_biplot(repel = TRUE)+
  labs(title = "Scores de las observaciones y variables",
       subtitle = "2 Componentes principales")
ComPri2 |> fviz_pca_biplot(repel = TRUE, axes = c(29,30))+
  labs(title = "Scores de las observaciones y variables",
       subtitle = "Componentes principales 29 y 30")

ComPri2$x |> cor() |> corrplot()

datos |> scale() |> as.matrix() |> dim()
ComPri2$rotation[,1:3] |> dim()

datos |> scale() |> as.matrix() -> X
ComPri2$rotation[,1:3] -> cargas
X %*% cargas -> Xcargas
Xcargas[1:4,]
ComPri2$x[1:4,1:3]

# Otros paquetes / funciones para componentes principales
# stats:: princomp
# FactoMineR::PCA
# psych::principal

# Análisis factorial ------------------------------------------------------

p_load(GPArotation)

datos |> fa(nfactors = datos |> ncol(), rotate = "none") -> AF1
AF1 |> fa.diagram()
AF1

datos |> fa(nfactors = 3, rotate = "none") -> AF2
AF2 |> fa.diagram()
AF2

AF1$communalities |> round(2); AF1$uniquenesses |> round(2)
AF2$communalities |> round(2); AF2$uniquenesses |> round(2)

AF2$model |> round(2) 
AF2$model |> diag() |> round(2)

AF2$loadings
AF2$loadings |> print(cutoff = 0)
AF2$loadings |> print(cutoff = 0.80)
AF2$loadings |> print(cutoff = 0.30) # recomendación

AF2$complexity
AF2$complexity |> mean()

AF2$Vaccounted

AF1$e.values 
AF1$e.values |> plot(type = "b", pch = 18)
AF1$e.values |> sum()

AF2$scores |> dim()
AF2$scores |> cor()
AF2$scores |> cor() |> corrplot()

# Escalamiento multidimensional -------------------------------------------

# ================ #
# 05 - datos2.xlsx #
# ================ #

# Este archivo contiene las distancias, en km, entre 7 distintas 
# ciudades de Perú

p_load(readxl, ggplot2, scatterplot3d, vegan)

(read_excel('05 - datos2.xlsx') -> datos2)
(datos2[,1] -> ciudades)
(datos2[,2:8] |> as.matrix() -> dist2)
(dist2 |> cmdscale() -> emd2)
data.frame(ciudades,emd2) |> 
  rename(Ciudad = 1,
         Dim1 = 2,
         Dim2 = 3) |> 
  ggplot(aes(x=Dim1,y=Dim2,label=Ciudad))+
  geom_point()+
  geom_text()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme_minimal()

# ================ #
# 05 - datos3.xlsx #
# ================ #

# Este archivo contiene puntajes referidos a programas de ciertos rubros
# (información, documentales, concursos, deportes, música) de 9 canales
# de televisión, a los que se les agrega el canal 10 PERFECTO y el canal 11
# EL PEOR DE TODOS

(read_excel('05 - datos3.xlsx') -> datos3)
(datos3 |> dist() -> dist3)
(dist3 |> cmdscale() -> emd3)
data.frame(datos3$Canal,emd3) |> 
  rename(Canal = 1,
         Dim1 = 2,
         Dim2 = 3) |> 
  ggplot(aes(x=Dim1,y=Dim2,label=Canal))+
  geom_text()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  theme_minimal()
data.frame(datos3[,-1],emd3) |> cor() |> round(2)

datos |> scale() |> dist()  |> cmdscale() -> emd1
emd1 |> data.frame() |> rename(eje1=1,eje2=2) |> 
  ggplot(aes(x=eje1,y=eje2))+
  geom_point()+
  theme_minimal()
emd1 |> cor() |> corrplot()
data.frame(datos,emd1) |> cor() |> round(2)
data.frame(datos,emd1) |> cor() |> round(2) |> corrplot(order = "hclust")
datos |> scale() |> dist() |> cmdscale(k=3) |> 
  data.frame() |> 
  scatterplot3d(type = "h", pch = 19, lty.hplot = 2)

# =============== #
# 05 - datos4.csv #
# =============== #

# Se recolectaron datos sobre la abundancia de 7 especies de crustáceros 
# herbívoros del puerto de Sydney, en 5 distintos hábitats y en dos momentos 
# diarios (día / noche)

read.csv('05 - datos4.csv') -> datos4
datos4 |> select(-Habitat,-DayNight,-Replicate,-Mass) -> datos_4
metaMDS(comm = datos_4, distance = "bray", k=2, trace = FALSE, trymax = 30) -> emd4
emd4$points 
data.frame(emd4$points, Habitat = datos4$Habitat, DiaNoche = datos4$DayNight) |> 
  ggplot(aes(x=MDS1,y=MDS2,color=Habitat))+
  geom_point()+
  theme_minimal()
data.frame(emd4$points, Habitat = datos4$Habitat, DiaNoche = datos4$DayNight) |> 
  ggplot(aes(x=MDS1,y=MDS2,color=DiaNoche))+
  geom_point()+
  theme_minimal()     
data.frame(emd4$points,datos4[,-c(1,2,3)]) |> 
  cor() |> 
  corrplot()
emd4$stress

# Local Linear Embedding --------------------------------------------------

# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("RDRToolbox")

# install.packages("lle")

library(RDRToolbox)
library(lle)
p_load(rgl, vegan)

# ====== #
# datos5 #
# ====== #

# datos5 es un conjunto de datos de expresión génica simulados
# con la función generateData del paquete RDRToolbox

d = generateData(samples=20, genes=1000); d[[1]] -> datos5
datos5 |> dim()
(datos5 |> LLE(dim=2, k=5) -> datos5_lle)
datos5_lle |> colMeans()
datos5_lle[,1] |> var();datos5_lle[,2] |> var()
datos5_lle |> plot(pch = 18);abline(h=0);abline(v=0)
datos5_lle |> dim()

# ====== #
# datos6 #
# ====== #

# datos6 es un conjunto de datos sintéticamente generado (simulado) de una
# curva S en 3 dimensiones

data(lle_scurve_data)
lle_scurve_data -> datos6
datos6 |> str()
plot3d(x=datos6[,1],y=datos6[,2],z=datos6[,3])
datos6 |> scatterplot3d(type = "h", pch = 19, lty.hplot = 2)
(datos6 |> LLE(dim = 2, k = 5) -> datos6_lle)
(datos6 |> lle(m = 2, k = 5) -> datos6_lle1)
plot(datos6_lle1$Y, main="K=5 data", xlab="Dim 1", ylab="Dim 2")

# ISOMAP ------------------------------------------------------------------

# ====== #
# datos7 #
# ====== #

# datos7 es un conjunto de datos referido al conteo de árboles en parcelas
# de una hectárea en la Isla de Barro Coloado e información asociada al sitio.

data(BCI) 
BCI -> datos7
datos7 |> dim()
datos7 |> vegdist() -> dist
dist |> isomap(k = 5, ndim = 4) -> datos7_isomap
datos7_isomap
(datos7_isomap$points -> puntos_isomap)
puntos_isomap |> head() 
datos7_isomap |> plot(main="isomap k=4")
puntos_isomap |> plot()
