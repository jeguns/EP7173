library(pacman)
p_load(readxl,dplyr) # Feature Selection in  R

# Todos estos procedimientos deben realizarse con los
# datos de entrenamiento

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
datos |> dplyr::select(num_range("z",range=1:4))
datos |> dplyr::select(everything())
datos |> dplyr::select_all()
datos |> dplyr::select_if(is.numeric)
datos |> dplyr::select_if(is.integer)
datos |> dplyr::select_if(is.double)
datos |> dplyr::select_if(is.character)
datos |> dplyr::select_if(is.logical)
datos |> dplyr::select_at(vars(contains("X"),starts_with("Z")),tolower)
datos |> dplyr::select_at(vars(contains("Z"),starts_with("X")),toupper)

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

p_load(vctrs,caret,FSinR,FSelector,funModeling)

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

datos6 = GermanCredit |> 
  dplyr::select(Duration,Amount,InstallmentRatePercentage,
                ResidenceDuration,Age,NumberExistingCredits,Class)

# Lo siguiente solo se debería trabajar con el conjunto de datos de entrenamiento

## Selección directa ------------------------------------------------------------
Busqueda_KMejores  <- directSearchAlgorithm(directSearcher = "selectKBest", list(k = 2)) 
Busqueda_KMejores  <- selectKBest(k = 2)
Busqueda_Percentil <- directSearchAlgorithm(directSearcher = "selectPercentile", list(percentile = 40)) 
Busqueda_Percentil <- selectPercentile(percentile = 40) 

## Búsqueda óptima -------------------------------------------------------------
Busqueda_BFS <- searchAlgorithm(searcher = "breadthFirst") 
Busqueda_BFS <- breadthFirst()
Busqueda_DFS <- searchAlgorithm(searcher = "deepFirst") 
Busqueda_DFS <- deepFirst() 

## Búsqueda secuencial ---------------------------------------------------------
Busqueda_SFS  <- searchAlgorithm(searcher = "sequentialForwardSelection")  
Busqueda_SFS  <- sequentialForwardSelection() 
Busqueda_SBS  <- searchAlgorithm(searcher = "sequentialBackwardSelection") 
Busqueda_SBS  <- sequentialBackwardSelection() 
Busqueda_SFFS <- searchAlgorithm(searcher = "sequentialFloatingForwardSelection")
Busqueda_SFFS <- sequentialFloatingForwardSelection() 
Busqueda_SFBS <- searchAlgorithm(searcher = "sequentialFloatingBackwardSelection") 
Busqueda_SFBS <-sequentialFloatingBackwardSelection() 

## Búsqueda probabilística -----------------------------------------------------
Busqueda_LasVegas <- searchAlgorithm(searcher = "LasVegas") 
Busqueda_LasVegas <- LasVegas() 

## Búsqueda local --------------------------------------------------------------
Busqueda_HC <- searchAlgorithm(searcher = "hillClimbing") 
Busqueda_HC <- hillClimbing() 

## Búsqueda meta heurística ----------------------------------------------------
Busqueda_Tabu  <- searchAlgorithm(searcher = "tabu") 
Busqueda_Tabu  <- tabu() 
Busqueda_AntC  <- searchAlgorithm(searcher = "antColony") 
Busqueda_AntC  <- antColony() 
Busqueda_GA    <- searchAlgorithm(searcher = "geneticAlgorithm") 
Busqueda_GA    <- geneticAlgorithm() 
Busqueda_SA    <- searchAlgorithm(searcher = "simulatedAnnealing") 
Busqueda_SA    <- simulatedAnnealing() 
Busqueda_Whale <- searchAlgorithm(searcher = "whaleOptimization") 
Busqueda_Whale <- whaleOptimization() 


# Filtros individuales ----------------------------------------------------

## Medidas de atributos individuales --------------------------------------

Evaluador_Cramer <- filterEvaluator(filter = 'cramer') 
Evaluador_Cramer <- cramer()
Evaluador_Cramer(datos6, 'Class', 'Duration')
Evaluador_Cramer(datos6, 'Class', 'Amount')
Evaluador_Cramer(datos6, 'Class', 'Age')
Evaluador_Cramer(datos6, 'Class', 'InstallmentRatePercentage')
Evaluador_Cramer(datos6, 'Class', 'NumberExistingCredits')
directFeatureSelection(datos6, 'Class', Busqueda_KMejores, Evaluador_Cramer)$bestFeatures
directFeatureSelection(datos6, 'Class', Busqueda_Percentil, Evaluador_Cramer)$bestFeatures
directFeatureSelection(datos6, 'Class', Busqueda_Percentil, Evaluador_Cramer)$featuresSelected
directFeatureSelection(datos6, 'Class', Busqueda_Percentil, Evaluador_Cramer)$featuresSelected |> 
  as.simple.formula('Class')

# En resumen, debe definirise el data frame, la variable dependiente de clase,
# el algoritmo de búsqueda y el filtro
busqueda_kmejores <- selectKBest(k = 2) 
evaluador_cramer  <- cramer() 
directFeatureSelection(datos6, 'Class', busqueda_kmejores, evaluador_cramer)$bestFeatures

## Medidas de conjuntos de atributos --------------------------------------

Evaluador_Relief <- filterEvaluator(filter = 'relief')  
Evaluador_Relief <- FSinR::relief() 
directFeatureSelection(datos6, 'Class', Busqueda_Percentil, Evaluador_Relief)$bestFeatures
directFeatureSelection(datos6, 'Class', Busqueda_Percentil, Evaluador_Relief)$time

Evaluador_Chi2 <- filterEvaluator(filter = 'chiSquared') 
directFeatureSelection(datos6, 'Class', Busqueda_KMejores, Evaluador_Chi2)$bestFeatures
directFeatureSelection(datos6, 'Class', Busqueda_KMejores, Evaluador_Chi2)$time

indicadores <- chi.squared(Class~., datos6) # Fselector package
indicadores |> print()
indicadores |> cutoff.k(2) 
indicadores |> cutoff.k(2) |> as.simple.formula("Class") 

dependiente = "Class"
predictores = setdiff(names(datos6), dependiente)
cross_plot(datos6, 
           input     = predictores,
           target    = dependiente,
           plot_type = "percentual") # funModeling package

# Filtros colectivos -----------------------------------------------------------

result0 <- featureSelection(datos6, 'Class', Busqueda_SFS, Evaluador_Cramer)  # error
Evaluador_Gini <- filterEvaluator(filter = 'giniIndex') # defino el filtro de Gini
result1 <- featureSelection(datos6, 'Class', Busqueda_SFS, Evaluador_Gini) 
result2 <- featureSelection(datos6, 'Class', Busqueda_SFFS, Evaluador_Gini) 
result3 <- featureSelection(datos6, 'Class', Busqueda_SBS, Evaluador_Gini) 
result4 <- featureSelection(datos6, 'Class', Busqueda_SFBS, Evaluador_Gini) 
result5 <- featureSelection(datos6, 'Class', Busqueda_BFS, Evaluador_Gini) 
result6 <- featureSelection(datos6, 'Class', Busqueda_Tabu, Evaluador_Gini)  
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

Evaluador_Gini(datos6, 'Class', 'Duration')
Evaluador_Gini(datos6, 'Class', 'Amount')
Evaluador_Gini(datos6, 'Class', 'Age')
Evaluador_Gini(datos6, 'Class', 'InstallmentRatePercentage')
Evaluador_Gini(datos6, 'Class', 'ResidenceDuration')
Evaluador_Gini(datos6, 'Class', 'NumberExistingCredits')

# En vez de Evaluador_Gini, se pueden utilizar otros criterios de filtro  para 
# conjunto de variables: medidas de consistencia, de dependencia, de distancia. 
# Se puede buscar en la ayuda ?filterEvaluator
?filterEvaluator
Evaluador_Gini <- filterEvaluator(filter = 'giniIndex') # filtro con medida de información 
Evaluador_RSC  <- filterEvaluator(filter = "roughsetConsistency") # filtro con medida de consistencia
Evaluador_R2   <- filterEvaluator(filter = "determinationCoefficient") # filtro con medida de dependencia

# Medida de consistencia usando FSelector::consistency
(subset <- consistency(Class~., datos6))
as.simple.formula(subset, "Class")
# Más sobre FSelector: https://cran.r-project.org/web/packages/FSelector/FSelector.pdf

# Filtros híbridos -----------------------------------------------------------

Evaluador_Chi2 <- filterEvaluator(filter = 'chiSquared') # individual
Evaluador_Gini <- filterEvaluator(filter = 'giniIndex')  # conjuntos
Busqueda_Hibrida <- hybridSearchAlgorithm('LCC') # Esto es Busqueda, no filtro
hybridFeatureSelection(datos6, 'Class', Busqueda_Hibrida, Evaluador_Gini, Evaluador_Chi2) # error
hybridFeatureSelection(datos6, 'Class', Busqueda_Hibrida, Evaluador_Chi2, Evaluador_Gini) # (indiv, conj)
hybridFeatureSelection(datos6, 'Class', Busqueda_Hibrida, Evaluador_Chi2, Evaluador_Chi2) # error
hybridFeatureSelection(datos6, 'Class', Busqueda_Hibrida, Evaluador_Gini, Evaluador_Gini) # (conj, conj)
# el orden debe ser (data frame, variable Y, algoritmo hibrido, filtro ind o conj, filtro conj)

# Evaluación con wrapper --------------------------------------------------

p_load(arm, rpart, MASS, kernlab, elasticnet, mlbench, Boruta)

Evaluador_bglm <- wrapperEvaluator(learner          = "bayesglm", 
                                   resamplingParams = list(method = "repeatedcv", 
                                                           repeats = 3)) 
resultado_bglm <- featureSelection(datos6, 'Class', Busqueda_SFS, Evaluador_bglm) 
resultado_bglm$bestFeatures
resultado_bglm$time

# resamplingParams permite definir los mismos argumentos que trainControl en caret
# fittingParams permite indicar valores para tuning de hiperparámetros 
# no todos los métodos / modelos tienen hiperparámetros

validacion    <- list(method = "repeatedcv", repeats = 3)
tuning        <- list(tuneGrid = expand.grid(k = c(1:12)))
Evaluador_KNN <- wrapperEvaluator(learner          = "knn",
                                  resamplingParams = validacion,
                                  fittingParams    = tuning) 
resultado_knn <- featureSelection(datos6, 'Class', Busqueda_SFFS, Evaluador_KNN) 
resultado_knn$bestFeatures
resultado_knn$time

validacion      <- list(method = "repeatedcv", repeats = 5)
tuning          <- list(tuneGrid = expand.grid(cp = seq(0,0.1,0.01)))
Evaluador_rpart <- wrapperEvaluator(learner          = 'rpart',
                                    resamplingParams = validacion,
                                    fittingParams    = tuning) 
resultado_rpart <- featureSelection(datos6, 'Class', Busqueda_SFS, Evaluador_rpart) 
resultado_rpart$bestFeatures
resultado_rpart$time

validacion    <- list(method = "repeatedcv", repeats = 10)
Evaluador_lda <- wrapperEvaluator(learner          = 'lda',
                                  resamplingParams = validacion)
resultado_lda <- featureSelection(datos6, 'Class', Busqueda_SFS, Evaluador_lda)
resultado_lda$bestFeatures
resultado_lda$time

tuning       <- list(tuneGrid = expand.grid(mtry = round(sqrt(ncol(datos6)))),trace = FALSE)
Evaluador_rf <- wrapperEvaluator(learner = 'rf',
                                 fittingParams = tuning) 
resultado_rf <- featureSelection(datos6, 'Class', Busqueda_SFS, Evaluador_rf)
resultado_rf$bestFeatures
resultado_rf$time

# BORUTA ------------------------------------------------------------------

boruta <- Boruta(Class ~ ., data = datos6, doTrace = 0, maxRuns = 200) 
boruta |> print()
boruta |> plot(las = 2, cex.axis = 0.7)
boruta |> attStats()
boruta |> plotImpHistory()
boruta$timeTaken

boruta2 <- Boruta(Class ~ ., data = datos6, doTrace = 0, maxRuns = 1500) 
boruta2 |> print()
boruta2 |> plot(las = 2, cex.axis = 0.7)
boruta2 |> attStats()
boruta2 |> plotImpHistory()
boruta2$timeTaken

boruta3<- Boruta(Class ~ ., data = datos6, doTrace = 0, maxRuns = 1500, pValue = 0.01/10) 
boruta3 |> print()
boruta3 |> plot(las = 2, cex.axis = 0.7)
boruta3 |> attStats()
boruta3 |> plotImpHistory()
boruta3$timeTaken

boruta4 <- boruta |> TentativeRoughFix() 
boruta4 |> print()
boruta4 |> plot(las = 2, cex.axis = 0.7)
boruta4 |> attStats()
boruta4 |> plotImpHistory()
boruta4$originalDecision
boruta4$finalDecision

# Recursive Feature Elimination -------------------------------------------

control   <- rfeControl(functions = rfFuncs, #lmFuncs, #nbFuncs
                        method    = "cv", #cv, loocv, boot
                        number    = 10) 
resultado <- rfe(x = datos6[, 1:6],
                 y = datos6$Class,
                 sizes = c(1:6),
                 rfeControl = control)
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
datos6_2  <- PimaIndiansDiabetes 
control   <- rfeControl(functions=rfFuncs, method="cv", number=10) 
resultado <- rfe(x=PimaIndiansDiabetes[,1:8], y=PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
resultado |> print()
resultado |> plot(type=c("g","o"))

# RESUMEN DE INDICADORES
# https://paperswithcode.com/paper/fsinr-an-exhaustive-package-for-feature/review/