library(pacman)
p_load(readxl,dplyr) # Feature Selection in R

# Seleccion de variables en dplyr -----------------------------------------

# ================ #
# 06 - datos1.xlsx #
# ================ #

# Es un conjunto de datos sintético que contiene 31 registros y
# 11 variables
# X_A: atributo numérico positivo
# X_B: atributo numérico positivo
# X_C: atributo numérico positivo
# Y_A: atributo cualitativo que toma los valores SI y NO	
# Y_B: atributo cualitativo que toma los valores SI y NO
# Y_C: atributo cualitativo que toma los valores SI y NO	
# z1: proporción (valores entre 0 y 1)
# Z2: proporción (valores entre 0 y 1)	
# z3: proporción (valores entre 0 y 1)	
# Z4: proporción (valores entre 0 y 1)	
# Z5: proporción (valores entre 0 y 1)

read_xlsx('06 - datos1.xlsx') -> datos
datos |> colnames()

datos[,c(1,3)]
datos[,seq(1,7,3)]
datos |> dplyr::select(X_A)
datos |> dplyr::select(X_A:Y_A)
datos |> dplyr::select(-(X_C:Y_B))
datos |> dplyr::select(X_A,X_C:Y_B)
datos |> dplyr::select(starts_with("X"))
datos |> dplyr::select(starts_with(c("x","z")))
datos |> dplyr::select(ends_with("b"))
datos |> dplyr::select(!ends_with("a")) 
datos |> dplyr::select(starts_with("X") | ends_with("b")) 
datos |> dplyr::select(starts_with("X") & ends_with("b")) 
datos |> dplyr::select(contains("X_"))
datos |> dplyr::select(matches("X_"))
datos |> dplyr::select(contains("M"))
datos |> dplyr::select(matches("M"))
datos |> dplyr::select(contains("[XY]_")) # busca la expresión exacta
datos |> dplyr::select(matches("[XY]_")) # admite expresiones regulares (patrón)
datos |> dplyr::select(num_range("Z",range=1:4))
datos |> dplyr::select(everything())
datos |> dplyr::select_all()
datos |> dplyr::select_if(is.numeric)
datos |> dplyr::select_if(is.integer)
datos |> mutate(X_AA = as.integer(X_A)) |> dplyr::select_if(is.integer)
datos |> dplyr::select_if(is.double)
datos |> dplyr::select_if(is.character)
datos |> dplyr::select_if(is.logical)
datos |> dplyr::select_at(vars(contains("X"),starts_with("Z")),tolower)
datos |> dplyr::select_at(vars(contains("X"),starts_with("Z")),toupper)

# Selección de variables por fuerza bruta ---------------------------------

p = 3
S=0;for(i in 0:p){S=S+choose(p,i)};S
2^p

p = datos |> ncol()
choose(p,3)
S=0;for(i in 0:11){S=S+choose(p,i)};S
2^p

p = 40
S=0;for(i in 0:p){S=S+choose(p,i)};S
2^p



# Direccionalidad de la búsqueda ------------------------------------------

p_load(caret,FSinR,FSelector,funModeling)

data(GermanCredit)
# 1000 personas clasificadas como personas de riesgo crediticio o no
# Variable respuesta: Class
# Class: 1: Bueno, 2: Malo
# Duration: Duración, en meses, del último préstamo
# Amount: Monto de crédito
# InstallmentRatePercentage: Tasa de pago a plazos en porcentaje de la renta disponible
# ResidenceDuration: Número de años en la residencia actual
# Age: Edad, en años
# NumberExistingCredits: Número de créditos en el banco
# etc

datos6_1 = GermanCredit |> 
  dplyr::select(Duration,Amount,InstallmentRatePercentage,ResidenceDuration,Age,NumberExistingCredits,Class)

# Funciones para búsqueda directa
directSearchAlgorithm(directSearcher = "selectKBest", list(k = 2)) -> Busqueda_KMejores
selectKBest(k = 2) -> busqueda_kmejores
directSearchAlgorithm(directSearcher = "selectPercentile", list(percentile = 40)) -> Busqueda_Percentil
selectPercentile(percentile = 40) -> busqueda_percentiles

# Funciones para búsqueda óptima
searchAlgorithm(searcher = "breadthFirst") -> Busqueda_BFS
breadthFirst() -> busqueda_bfs
searchAlgorithm(searcher = "deepFirst") -> Busqueda_DFS
deepFirst() -> busqueda_dfs

# Funciones para búsqueda secuencial
searchAlgorithm(searcher = "sequentialForwardSelection") -> Busqueda_SFS
sequentialForwardSelection() -> busqueda_sfs
searchAlgorithm(searcher = "sequentialBackwardSelection") -> Busqueda_SBS
sequentialBackwardSelection() -> busqueda_sbs
searchAlgorithm(searcher = "sequentialFloatingForwardSelection") -> Busqueda_SFFS
sequentialFloatingForwardSelection() -> busqueda_sffs
searchAlgorithm(searcher = "sequentialFloatingBackwardSelection") -> Busqueda_SFBS
sequentialFloatingBackwardSelection() -> busqueda_sfbs

# Funciones para búsqueda probabilística
searchAlgorithm(searcher = "LasVegas") -> Busqueda_LasVegas
LasVegas() -> busqueda_lasvegas

# Funciones para búsqueda local
searchAlgorithm(searcher = "hillClimbing") -> Busqueda_HC
hillClimbing() -> busqueda_hc

# Funciones para búsqueda meta heurística
searchAlgorithm(searcher = "tabu") -> Busqueda_Tabu
tabu() -> busqueda_tabu
searchAlgorithm(searcher = "antColony") -> Busqueda_AntC
antColony() -> busqueda_antc
searchAlgorithm(searcher = "geneticAlgorithm") -> Busqueda_GA
geneticAlgorithm() -> busqueda_ga
searchAlgorithm(searcher = "simulatedAnnealing") -> Busqueda_SA
simulatedAnnealing() -> busqueda_sa
searchAlgorithm(searcher = "whaleOptimization") -> Busqueda_Whale
whaleOptimization() -> busqueda_whale


# Filtros individuales ----------------------------------------------------

# Evaluación con filtros individuales para búsqueda directa
filterEvaluator(filter = 'cramer') -> Evaluador_Cramer
cramer() -> evaluador_cramer
Evaluador_Cramer(datos6_1, 'Class', 'Duration')
Evaluador_Cramer(datos6_1, 'Class', 'Amount')
Evaluador_Cramer(datos6_1, 'Class', 'Age')
Evaluador_Cramer(datos6_1, 'Class', 'InstallmentRatePercentage')
Evaluador_Cramer(datos6_1, 'Class', 'NumberExistingCredits')
evaluador_cramer(datos6_1, 'Class', 'NumberExistingCredits')
directFeatureSelection(datos6_1, 'Class', Busqueda_KMejores, Evaluador_Cramer)$bestFeatures
directFeatureSelection(datos6_1, 'Class', busqueda_kmejores, Evaluador_Cramer)$bestFeatures
directFeatureSelection(datos6_1, 'Class', Busqueda_KMejores, evaluador_cramer)$bestFeatures
directFeatureSelection(datos6_1, 'Class', busqueda_kmejores, evaluador_cramer)$bestFeatures
directFeatureSelection(datos6_1, 'Class', busqueda_percentiles, Evaluador_Cramer)$bestFeatures
directFeatureSelection(datos6_1, 'Class', busqueda_percentiles, Evaluador_Cramer)$featuresSelected
directFeatureSelection(datos6_1, 'Class', busqueda_percentiles, Evaluador_Cramer)$featuresSelected |> 
  as.simple.formula('Class')

filterEvaluator(filter = 'relief') -> Evaluador_Relief
FSinR::relief() -> evaluador_relief
directFeatureSelection(datos6_1, 'Class', busqueda_percentiles, Evaluador_Relief)$bestFeatures
directFeatureSelection(datos6_1, 'Class', busqueda_percentiles, Evaluador_Relief)$time
directFeatureSelection(datos6_1, 'Class', busqueda_percentiles, FSinR::relief())$time
directFeatureSelection(datos6_1, 'Class', busqueda_percentiles, evaluador_relief)$time

filterEvaluator(filter = 'chiSquared') -> Evaluador_Chi2
directFeatureSelection(datos6_1, 'Class', Busqueda_KMejores, Evaluador_Chi2)$bestFeatures
directFeatureSelection(datos6_1, 'Class', Busqueda_KMejores, Evaluador_Chi2)$time

indicadores = chi.squared(Class~., datos6_1)
indicadores |> print()
indicadores |> cutoff.k(2) 
indicadores |> cutoff.k(2) |> as.simple.formula("Class") 

dependiente = "Class"
predictores = setdiff(names(datos6_1), dependiente)
cross_plot(datos6_1, 
           input     = predictores,
           target    = dependiente,
           plot_type = "percentual") 

# Evaluación con filtros colectivos para otras estrategias de búsqueda
# Medidas de información
featureSelection(datos6_1, 'Class', Busqueda_SFS, Evaluador_Cramer) -> result0
filterEvaluator(filter = 'giniIndex') -> Evaluador_Gini
featureSelection(datos6_1, 'Class', Busqueda_SFS, Evaluador_Gini) -> result1
featureSelection(datos6_1, 'Class', Busqueda_SFFS, Evaluador_Gini) -> result2
featureSelection(datos6_1, 'Class', Busqueda_SBS, Evaluador_Gini) -> result3
featureSelection(datos6_1, 'Class', Busqueda_SFBS, Evaluador_Gini) -> result4
featureSelection(datos6_1, 'Class', Busqueda_BFS, Evaluador_Gini) -> result5
featureSelection(datos6_1, 'Class', Busqueda_Tabu, Evaluador_Gini) -> result6 # 
result1$bestFeatures
result2$bestFeatures
result3$bestFeatures
result4$bestFeatures
result5$bestFeatures
result6$bestFeatures
result1$time
result2$time
result3$time
result4$time
result5$time
result6$time

# ¿Si el conjunto de variables es unitario? 

Evaluador_Gini(datos6_1, 'Class', 'Duration')
Evaluador_Gini(datos6_1, 'Class', 'Amount')
Evaluador_Gini(datos6_1, 'Class', 'Age')
Evaluador_Gini(datos6_1, 'Class', 'InstallmentRatePercentage')
Evaluador_Gini(datos6_1, 'Class', 'ResidenceDuration')
Evaluador_Gini(datos6_1, 'Class', 'NumberExistingCredits')

# En vez de Evaluador_Gini, se pueden utilizar otros criterios de filtro
# para conjunto de variables: medidas de consistencia, de dependencia, 
# de distancia

# Evaluación con filtros colectivos
# Medida de consistencia usando FSelector::consistency
(consistency(Class~., datos6_1) -> subset)
as.simple.formula(subset, "Class")
# Más sobre FSelector: https://cran.r-project.org/web/packages/FSelector/FSelector.pdf

# Evaluación con filtros híbridos 
filterEvaluator(filter = 'chiSquared') -> Evaluador_Chi2 # individual
filterEvaluator(filter = 'giniIndex') -> Evaluador_Gini # conjuntos
hybridSearchAlgorithm('LCC') -> Busqueda_Hibrida
hybridFeatureSelection(datos6_1, 'Class', Busqueda_Hibrida, Evaluador_Gini, Evaluador_Chi2) # error
hybridFeatureSelection(datos6_1, 'Class', Busqueda_Hibrida, Evaluador_Chi2, Evaluador_Gini) # (indiv, conj)
hybridFeatureSelection(datos6_1, 'Class', Busqueda_Hibrida, Evaluador_Chi2, Evaluador_Chi2) # error
hybridFeatureSelection(datos6_1, 'Class', Busqueda_Hibrida, Evaluador_Gini, Evaluador_Gini) # (conj, conj)

# Evaluación con wrapper --------------------------------------------------

p_load(arm, rpart, MASS, kernlab, elasticnet, mlbench, Boruta)

wrapperEvaluator(learner          = "bayesglm", 
                 resamplingParams = list(method = "repeatedcv", 
                                         repeats = 3)) -> Evaluador_bglm
featureSelection(datos6_1, 'Class', Busqueda_SFS, Evaluador_bglm) -> resultado_bglm
resultado_bglm$bestFeatures
resultado_bglm$time

# resamplingParams permite definir los mismos argumentos que trainControl en caret
# fittingParams permite indicar valores para tuning de hiperparámetros 
# no todos los métodos / modelos tienen hiperparámetros

validacion = list(method = "repeatedcv", repeats = 3)
tuning     = list(tuneGrid = expand.grid(k = c(1:12)))
wrapperEvaluator(learner          = "knn",
                 resamplingParams = validacion,
                 fittingParams    = tuning) -> Evaluador_KNN
featureSelection(datos6_1, 'Class', Busqueda_SFFS, Evaluador_KNN) -> resultado_knn
resultado_knn$bestFeatures
resultado_knn$time

validacion = list(method = "repeatedcv", repeats = 5)
tuning     = list(tuneGrid = expand.grid(cp = seq(0,0.1,0.01)))
wrapperEvaluator(learner          = 'rpart',
                 resamplingParams = validacion,
                 fittingParams    = tuning) -> Evaluador_rpart
featureSelection(datos6_1, 'Class', Busqueda_SFS, Evaluador_rpart) -> resultado_rpart
resultado_rpart$bestFeatures
resultado_rpart$time

validacion = list(method = "repeatedcv", repeats = 10)
wrapperEvaluator(learner          = 'lda',
                 resamplingParams = validacion) -> Evaluador_lda
featureSelection(datos6_1, 'Class', Busqueda_SFS, Evaluador_lda) -> resultado_lda
resultado_lda$bestFeatures
resultado_lda$time

tuning = list(tuneGrid = expand.grid(mtry = round(sqrt(ncol(datos6_1)))),trace = FALSE)
wrapperEvaluator(learner = 'rf',
                 fittingParams = tuning) -> Evaluador_rf
featureSelection(datos6_1, 'Class', Busqueda_SFS, Evaluador_rf) -> resultado_rf
resultado_rf$bestFeatures
resultado_rf$time

wrapperEvaluator('enet') -> Evaluador_enet
featureSelection(datos6_1, 'Class', Busqueda_SFS, Evaluador_enet) -> resultado_enet

# BORUTA ------------------------------------------------------------------

Boruta(Class ~ ., data = datos6_1, doTrace = 0, maxRuns = 500) -> boruta
boruta |> print()
boruta |> plot(las = 2, cex.axis = 0.7)
boruta |> attStats()
boruta |> plotImpHistory()
boruta$timeTaken

Boruta(Class ~ ., data = datos6_1, doTrace = 0, maxRuns = 1500) -> boruta2
boruta2 |> print()
boruta2 |> plot(las = 2, cex.axis = 0.7)
boruta2 |> attStats()
boruta2 |> plotImpHistory()
boruta2$timeTaken

Boruta(Class ~ ., data = datos6_1, doTrace = 0, maxRuns = 1500, pValue = 0.01/10) -> boruta3
boruta3 |> print()
boruta3 |> plot(las = 2, cex.axis = 0.7)
boruta3 |> attStats()
boruta3 |> plotImpHistory()
boruta3$timeTaken

boruta2 |> TentativeRoughFix() -> boruta4
boruta4 |> print()
boruta4 |> plot(las = 2, cex.axis = 0.7)
boruta4 |> attStats()
boruta4 |> plotImpHistory()
boruta4$originalDecision
boruta4$finalDecision

# Recursive Feature Elimination -------------------------------------------

rfeControl(functions = rfFuncs,
           method    = "loocv",
           number    = 10) -> control
rfe(x = datos6_1[, 1:6],
    y = datos6_1$Class,
    sizes = c(1:6),
    rfeControl = control) -> resultado
resultado |> print()
resultado |> plot(type = c("g", "o"))
resultado

# =================== #
# PimaIndiansDiabetes #
# =================== #

# 768 observaciones x 9 variables
# pregnant: Número de veces que ha estado embarazada
# glucose: Concentración de glucosa en plasma
# pressute: presión diastólica de la sangre (mm Hg)
# triceps: Espesor del pliegue cutáneo del tríceps (mm)
# insulin: Insulina sérica de 2 horas (mm U/ml)
# mass: IMC (peso en kg/(altura en cm)^2)
# pedigree: función que cuantifica la presencia de casos de diabetes en la familia
# age: edad en años
# diabetes: resultado en la prueba de diabetes

library(mlbench)
data(PimaIndiansDiabetes)
PimaIndiansDiabetes -> datos6_2
rfeControl(functions=rfFuncs, method="cv", number=10) -> control
rfe(x=PimaIndiansDiabetes[,1:8], y=PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control) -> resultado
resultado |> print()
resultado |> plot(type=c("g","o"))
