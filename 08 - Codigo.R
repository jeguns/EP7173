
library(pacman)

p_load(dplyr,readr,tidyr,lubridate,ggplot2,funModeling,discretization,arules,arulesCBA,GridOnClusters)

# Discretización personalizada --------------------------------------------

# =============== #
# 08 - datos1.csv #
# =============== #

# Este conjunto de datos toma como referencia el universo de hospitalizados de 
# la f500 (en base al último registro de la fecha de hospitalización), 
# vinculando información de dosis de vacunas y fallecimiento por COVID (obtenida 
# por fallecimientos informados por el CDC).

datos = read_csv('08 - datos1.csv')
datos |> glimpse()
datos |> 
  filter(!is.na(edad) & edad>=0) |>  # en vez de esto, se podría imputar
  mutate(fecha_dosis1 = dmy(fecha_dosis1),
         fecha_dosis2 = dmy(fecha_dosis2),
         fecha_dosis3 = dmy(fecha_dosis3),
         dias_dosis1  = as.numeric(today()-fecha_dosis1),
         dias_dosis2  = as.numeric(today()-fecha_dosis2),
         dias_dosis3  = as.numeric(today()-fecha_dosis3),
         flag_vacuna  = as.factor(flag_vacuna)) -> datos

datos |> count(edad)

datos |>
  mutate(edad2 = cut(edad,
                     breaks = c(0, 11, 17, 29, 59, 120))) |>
  count(edad2)  # mal

datos |>
  mutate(edad2 = cut(edad,
                     breaks = c(0, 11, 17, 29, 59, 120),
                     include.lowest = TRUE)) |>
  count(edad2) 

datos |> 
  mutate(edad2 = cut(edad, 
                     breaks = c(0,12,18,30,60,120),
                     right  = FALSE)) |> 
  count(edad2) 

datos |> 
  mutate(edad2 = cut(edad, 
                     breaks = c(0,12,18,30,60,120),
                     labels=c("Niño","Adolescente","Joven","Adulto","Adulto mayor"),
                     right  = FALSE)) |> 
  count(edad2) 

datos |> 
  mutate(edad2 = cut(edad, 
                     breaks = c(0,12,18,30,60,120),
                     labels=c("Niño","Adolescente","Joven","Adulto","Adulto mayor"),
                     right  = FALSE)) |>
  select(edad2) |> 
  str()

datos |> 
  mutate(edad2 = cut(edad, 
                     breaks = c(0,12,18,30,60,120),
                     labels=c("Niño","Adolescente","Joven","Adulto","Adulto mayor"),
                     right  = FALSE,
                     ordered_result = TRUE)) |> 
  select(edad2) |> 
  str()

# Equal Freq --------------------------------------------------------------

(datos |> discretize_get_bins(input  = c("edad"), n_bins = 5) -> discret_equal_freq) # funModeling
datos |>
  discretize_df(data_bins=discret_equal_freq) |> 
  count(edad)

datos |> 
  mutate(edad2 = cut_number(edad,n=5)) |> # ggplot2
  count(edad2)

datos |> 
  mutate(edad2 = cut_number(edad,n=5,right=FALSE)) |> 
  count(edad2) 

datos |> 
  mutate(edad2 = arules::discretize(edad,method="frequency", breaks=5)) |> # arules
  count(edad2)

datos |> 
  mutate(edad2 = arules::discretize(edad,method="frequency", breaks=5, 
                                    right = TRUE)) |> 
  count(edad2)

datos |> 
  mutate(edad2 = arules::discretize(edad,method="frequency", breaks=5,
                                    infinity = TRUE)) |> 
  count(edad2)

datos |> 
  mutate(edad2 = arules::discretize(edad,method="frequency", breaks=5,
                                    infinity = TRUE, right=TRUE,
                                    labels=c("Grupo A","Grupo B","Grupo C","Grupo D","Grupo E"))) |> 
  count(edad2)

# Equal Width -------------------------------------------------------------

datos |> 
  mutate(edad2 = cut_width(edad,width=10)) |> # ggplot2
  count(edad2) 

datos|> 
  mutate(edad2 = cut_width(edad,width=10,closed="left")) |> 
  count(edad2) 

datos |> 
  mutate(edad2 = cut_interval(edad,n=10)) |>  
  count(edad2) 

datos |> 
  mutate(edad2 = cut_interval(edad,n=10,right = FALSE)) |>  
  count(edad2) 

datos |>
  mutate(edad2 = arules::discretize(edad, 
                                    method   = "interval",
                                    breaks   = 10,
                                    infinity = FALSE,
                                    right    = TRUE)) |>
  count(edad2) 

# CAIM --------------------------------------------------------------------

# ================ #
# 08 - datos2.data #
# ================ #

rm(list=ls()) # borrar los objetos ya creados
datos = read.table('08 - datos2.data', sep = ",", na.strings = "?")

# V1 = Evaluación BI-RADS: 1 a 5 (ordinal)
# V2 = Edad: edad del paciente en años (número entero) 
# V3 = Forma: forma de masa: redonda = 1 ovalada = 2 lobular = 3 irregular = 4 (nominal) 
# V4 = Margen: margen de masa: circunscrito = 1 microlobulado = 2 oscurecido = 3 mal definido = 4 espiculado = 5 (nominal)
# V5 = Densidad: densidad de masa alta (ordinal)
# V6 = Gravedad: benigna = 0 o maligna = 1 (binaria, 53.7% 0 – 46.3% 1).

datos |> glimpse()
datos |> 
  drop_na() |> # en vez de esto se podría imputar
  mutate(V1 = as.numeric(V1),
         V2 = as.numeric(V2),
         V5 = as.numeric(V5),
         V6 = as.factor(V6)) |> 
  select(V1,V2,V5,V6) -> datos # V3 y V4 bastaría con convertirlas en factor
datos |> glimpse()

datos |> disc.Topdown(method = 1) -> discretiza_caim # discretization
discretiza_caim$Disc.data |> head()
discretiza_caim$Disc.data |> 
  mutate_if(is.numeric,as.factor) -> datos_caim
discretiza_caim$cutp -> cortes_caim

cortes_caim[[1]]
datos_caim |> count(V1) 
datos |> filter(V1>=0 & V1<4.5) |> count()
datos |> filter(V1>=4.5 & V1<=55) |> count()
cortes_caim[[2]]
datos_caim |> count(V2) 
datos |> filter(V2>=18 & V2<57.5) |> count()
datos |> filter(V2>=57.5 & V2<=96) |> count()
cortes_caim[[3]]
datos_caim |> count(V5) 
datos |> filter(V5>=1 & V5<2.5) |> count()
datos |> filter(V5>=2.5 & V5<=4) |> count()

datos |> discretizeDF.supervised(formula = V6~., ., method="caim") -> datos_caim_2 # arulesCBA
datos_caim_2 |> count(V1)
datos_caim_2 |> count(V2)
datos_caim_2 |> count(V5)

# CACC --------------------------------------------------------------------

datos |> disc.Topdown(method = 2) -> discretiza_cacc # discretization
discretiza_cacc$Disc.data |> head()
discretiza_cacc$Disc.data |> 
  mutate_if(is.numeric,as.factor) -> datos_cacc
discretiza_cacc$cutp -> cortes_cacc

cortes_cacc[[1]]
datos_cacc |> count(V1) 
datos |> filter(V1>=0 & V1<4.5) |> count()
datos |> filter(V1>=4.5 & V1<=55) |> count()
cortes_cacc[[2]]
datos_cacc |> count(V2) 
datos |> filter(V2>=18 & V2<57.5) |> count()
datos |> filter(V2>=57.5 & V2<=96) |> count()
cortes_cacc[[3]]
datos_cacc |> count(V5) 
datos |> filter(V5>=1 & V5<2.5) |> count()
datos |> filter(V5>=2.5 & V5<=4) |> count()

datos |> discretizeDF.supervised(formula = V6~.,.,method="cacc") -> datos_cacc_2 # arulesCBA
datos_cacc_2 |> count(V1)
datos_cacc_2 |> count(V2)
datos_cacc_2 |> count(V5)

# AMEVA -------------------------------------------------------------------

datos |> disc.Topdown(method = 3) -> discretiza_ameva # discretization
discretiza_ameva$Disc.data |> head()
discretiza_ameva$Disc.data |> 
  mutate_if(is.numeric,as.factor) -> datos_ameva
discretiza_ameva$cutp -> cortes_ameva

cortes_ameva[[1]]
datos_ameva |> count(V1) 
datos |> filter(V1>=0 & V1<4.5) |> count()
datos |> filter(V1>=4.5 & V1<=55) |> count()
cortes_ameva[[2]]
datos_ameva |> count(V2) 
datos |> filter(V2>=18 & V2<57.5) |> count()
datos |> filter(V2>=57.5 & V2<=96) |> count()
cortes_ameva[[3]]
datos_ameva |> count(V5) 
datos |> filter(V5>=1 & V5<2.5) |> count()
datos |> filter(V5>=2.5 & V5<=4) |> count()

datos |> discretizeDF.supervised(formula = V6~.,.,method="cacc") -> datos_ameva_2 # arulesCBA
datos_ameva_2 |> count(V1)
datos_ameva_2 |> count(V2)
datos_ameva_2 |> count(V5)

# MDLP --------------------------------------------------------------------

datos |> mdlp() -> discretiza_mdlp # discretization
discretiza_mdlp$Disc.data |> head()
discretiza_mdlp$Disc.data |> 
  mutate_if(is.numeric,as.factor) -> datos_mdlp
discretiza_mdlp$cutp -> cortes_mdlp

cortes_mdlp[[1]]
datos_mdlp |> count(V1) 
datos |> filter(V1<4.5) |> count()
datos |> filter(V1>=4.5) |> count()
cortes_mdlp[[2]]
datos_mdlp |> count(V2) 
datos |> filter(V2<39.5) |> count()
datos |> filter(V2>=39.5 & V2<57.5) |> count()
datos |> filter(V2>=57.5 & V2<66.5) |> count()
datos |> filter(V2>=66.5) |> count()
cortes_mdlp[[3]]
datos_mdlp |> count(V5) 
datos |> nrow()

datos |> discretizeDF.supervised(formula = V6~.,.,method="mdlp") -> datos_mdlp_2 # arulesCBA
datos_mdlp_2 |> count(V1)
datos_mdlp_2 |> count(V2)
datos_mdlp_2 |> count(V5)

# ChiMerge ----------------------------------------------------------------

datos |> chiM(alpha=0.05) -> discretiza_chimerge # discretization
discretiza_chimerge$Disc.data |> head()
discretiza_chimerge$Disc.data |> 
  mutate_if(is.numeric,as.factor) -> datos_chimerge
discretiza_chimerge$cutp -> cortes_chimerge

cortes_chimerge[[1]]
datos_chimerge |> count(V1) 
datos |> filter(V1<1) |> count()
datos |> filter(V1>=1 & V1<4.5) |> count()
datos |> filter(V1>=4.5) |> count()
cortes_chimerge[[2]]
datos_chimerge |> count(V2) 
cortes_chimerge[[3]]
datos_chimerge |> count(V5) 

datos |> discretizeDF.supervised(formula = V6~.,.,method="chimerge") -> discretiza_chimerge_2 # arulesCBA
discretiza_chimerge_2 |> count(V1)
discretiza_chimerge_2 |> count(V2)
discretiza_chimerge_2 |> count(V5)

# Chi2 --------------------------------------------------------------------

data.frame(X1 = c(1,1,1,1,1,1,2,2,2) ,
           X2 = c(1,1,1,1,1,1,2,2,2) ,
           X3 = c(4,4,4,4,4,4,5,5,5) ,
           Y  = c(0,0,1,2,0,2,1,1,0) |> as.factor()) |> 
incon()

datos |> chi2() -> discretiza_chi2
discretiza_chi2$Disc.data |> head()
discretiza_chi2$Disc.data |> 
  mutate_if(is.numeric,as.factor) -> datos_chi2
discretiza_chi2$cutp -> cortes_chi2

cortes_chi2[[1]]
datos_chi2 |> count(V1)
datos |> filter(V1<1.5) |> count()
datos |> filter(V1>=1.5 & V1<2.5) |> count()
datos |> filter(V1>=2.5 & V1<3.5) |> count()
datos |> filter(V1>=3.5 & V1<4.5) |> count()
datos |> filter(V1>=4.5 & V1<5.5) |> count()
datos |> filter(V1>=5.5) |> count()
cortes_chi2[[2]]
cortes_chi2[[3]]

datos |> select(V1,V6) |> chi22() -> discretiza_chi2
discretiza_chi2$Disc.data |> mutate_if(is.numeric,as.factor) -> datos_chi2
discretiza_chi2$cutp -> cortes_chi2
cortes_chi2
datos_chi2 |> count(V1)
datos |> filter(V1<1) |> count()
datos |> filter(V1>=1 & V1<2.5) |> count()
datos |> filter(V1>=2.5 & V1<4.5) |> count()
datos |> filter(V1>=4.5 & V1<5.5) |> count()
datos |> filter(V1>=5.5) |> count()

datos |> select(V2,V6) |> chi22() -> discretiza_chi2
discretiza_chi2$Disc.data |> mutate_if(is.numeric,as.factor) -> datos_chi2
discretiza_chi2$cutp -> cortes_chi2
cortes_chi2
datos_chi2 |> count(V2)
datos |> filter(V2<27.5) |> count()
datos |> filter(V2>=27.5 & V2<28.5) |> count()
datos |> filter(V2>=28.5 & V2<30.5) |> count()

datos |> select(V5,V6) |> chi22() -> discretiza_chi2
discretiza_chi2$Disc.data |> mutate_if(is.numeric,as.factor) -> datos_chi2
discretiza_chi2$cutp -> cortes_chi2
cortes_chi2
datos_chi2 |> count(V5)
datos |> filter(V5<1.5) |> count()
datos |> filter(V5>=1.5 & V5<2.5) |> count()
datos |> filter(V5>=2.5) |> count()

datos |> discretizeDF.supervised(formula = V6~.,.,method="chi2") -> discretiza_chi22 # arulesCBA
discretiza_chi22 |> count(V1)
discretiza_chi22 |> count(V2)
discretiza_chi22 |> count(V5)

# modChi2 -----------------------------------------------------------------

datos |> modChi2() -> discretiza_modchi2
discretiza_modchi2$Disc.data |> head()
discretiza_modchi2$Disc.data |> 
  mutate_if(is.numeric,as.factor) -> datos_modchi2
discretiza_modchi2$cutp -> cortes_modchi2

cortes_modchi2[[1]]
datos_modchi2 |> count(V1)
datos |> filter(V1<1.5) |> count()
datos |> filter(V1>=1.5) |> count()
cortes_modchi2[[2]]
cortes_modchi2[[3]]

datos |> select(V1,V6) |>  modChi22() -> discretiza_modchi2
discretiza_modchi2$Disc.data |> mutate_if(is.numeric,as.factor) -> datos_modchi2
discretiza_modchi2$cutp -> cortes_modchi2
cortes_modchi2
datos_modchi2 |> count(V1)
datos |> filter(V1<4.5) |> count()
datos |> filter(V1>=4.5) |> count()

datos |> select(V2,V6) |>  modChi22() -> discretiza_modchi2
discretiza_modchi2$Disc.data |> mutate_if(is.numeric,as.factor) -> datos_modchi2
discretiza_modchi2$cutp -> cortes_modchi2
cortes_modchi2
datos_modchi2 |> count(V2)
datos |> filter(V2<39.5) |> count()
datos |> filter(V2>=39.5 & V2<64.5) |> count()
datos |> filter(V2>=64.5) |> count()

datos |> select(V5,V6) |>  modChi22() -> discretiza_modchi2
discretiza_modchi2$Disc.data |> mutate_if(is.numeric,as.factor) -> datos_modchi2
discretiza_modchi2$cutp -> cortes_modchi2
cortes_modchi2
datos_modchi2 |> count(V5)

datos |> discretizeDF.supervised(formula = V6~.,.,method="modchi2") -> discretiza_modchi22 # arulesCBA
discretiza_modchi22 |> count(V1)
discretiza_modchi22 |> count(V2)
discretiza_modchi22 |> count(V5)

# extendedChi2 ------------------------------------------------------------

datos |> extendChi2() -> discretiza_extchi2 # discretization
discretiza_extchi2$Disc.data |> head()
discretiza_extchi2$Disc.data |> 
  mutate_if(is.numeric,as.factor) -> datos_extchi2
discretiza_extchi2$cutp -> cortes_extchi2

cortes_extchi2[[1]]
datos_extchi2 |> count(V1)
datos |> filter(V1<1.5) |> count()
datos |> filter(V1>=1.5) |> count()
cortes_extchi2[[2]]
cortes_extchi2[[3]]

datos |> select(V1,V6) |>  extendChi22() -> discretiza_extchi2
discretiza_extchi2$Disc.data |> mutate_if(is.numeric,as.factor) -> datos_extchi2
discretiza_extchi2$cutp -> cortes_extchi2
cortes_extchi2
datos_extchi2 |> count(V1)
datos |> filter(V1<4.5) |> count()
datos |> filter(V1>=4.5) |> count()

datos |> select(V2,V6) |>  extendChi22() -> discretiza_extchi2
discretiza_extchi2$Disc.data |> mutate_if(is.numeric,as.factor) -> datos_extchi2
discretiza_extchi2$cutp -> cortes_extchi2
cortes_extchi2
datos_extchi2 |> count(V2)
datos |> filter(V2<39.5) |> count()
datos |> filter(V2>=39.5 & V2<64.5) |> count()
datos |> filter(V2>=64.5) |> count()

datos |> select(V5,V6) |>  extendChi22() -> discretiza_extchi2
discretiza_extchi2$Disc.data |> mutate_if(is.numeric,as.factor) -> datos_extchi2
discretiza_extchi2$cutp -> cortes_extchi2
cortes_extchi2
datos_extchi2 |> count(V5)

datos |> discretizeDF.supervised(formula = V6~.,.,method="extendedchi2") -> discretiza_extchi22 # arulesCBA 
discretiza_extchi22 |> count(V1)
discretiza_extchi22 |> count(V2)
discretiza_extchi22 |> count(V5)

# Discretización basada en clustering -------------------------------------

# Clustering univariado
datos |> 
  mutate(v1 = arules::discretize(V1,method="cluster", breaks=3),
         v2 = arules::discretize(V2,method="cluster", breaks=2),
         v5 = arules::discretize(V5,method="cluster", breaks=2)) -> discretiza_cluster # arules
discretiza_cluster |> count(v1)
discretiza_cluster |> count(v2)
discretiza_cluster |> count(v5)

# Clustering multivariado
datos |> 
  select_if(is.numeric) |> 
  discretize.jointly(k = c(2:8), min_level = 2) -> discretiza_cluster # GridOnClusters
datos |> head()
discretiza_cluster$D |> head()
discretiza_cluster$grid[[1]]
discretiza_cluster$D |> count(V1)
datos |> filter(V1<4) |> count()
datos |> filter(V1>=4 & V1<4.5) |> count()
datos |> filter(V1>=4.5) |> count()
discretiza_cluster$grid[[2]]
discretiza_cluster$D |> count(V2)
datos |> filter(V2<38.5) |> count()
datos |> filter(V2>=38.5 & V2<48.0) |> count()
datos |> filter(V2>=48.0 & V2<54.5) |> count()
datos |> filter(V2>=54.5 & V2<60.5) |> count()
datos |> filter(V2>=60.5 & V2<64.5) |> count()
datos |> filter(V2>=64.5 & V2<69.0) |> count()
datos |> filter(V2>=69) |> count()
discretiza_cluster$grid[[3]]
discretiza_cluster$D |> count(V5)
datos |> filter(V5<3) |> count()
datos |> filter(V5>=3) |> count()

