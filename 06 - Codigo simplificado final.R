

# Ejemplo -----------------------------------------------------------------

datos6_3 = read.csv2('06 - datos3.csv')
datos6_3 |> 
  dplyr::select(-Hour..Coded.) |> 
  janitor::clean_names() -> datos6_3

# 135 observaciones, 17 variables
# 1. Hour
# 2. Immobilized bus
# 3. Broken Truck
# 4. Vehicle excess
# 5. Accident victim
# 6. Running over
# 7. Fire Vehicles
# 8. Occurrence involving freight
# 9. Incident involving dangerous freight
# 10. Lack of electricity
# 11. Fire
# 12. Point of flooding
# 13. Manifestations
# 14. Defect in the network of trolleybuses
# 15. Tree on the road
# 16. Semaphore off
# 17. Intermittent Semaphore
# 18. Slowness in traffic (%) (Target)

# Paquete: FSinR
# Direccionalidad: Búsqueda directa. Algoritmo: Elegir los 3 mejores
# Filtro: individual. Evaluador: Chi cuadrado
directSearchAlgorithm(directSearcher = "selectKBest", list(k = 3)) -> Busqueda_KMejores 
filterEvaluator('chiSquared') -> Evaluador_chi2
directFeatureSelection(datos6_3,'slowness_in_traffic',Busqueda_KMejores,Evaluador_chi2)$featuresSelected -> seleccionados1
seleccionados1 |> as.simple.formula("slowness_in_traffic") 

# Paquete: FSinR
# Direccionalidad: Búsqueda directa. Algoritmo: Elegir el mejor 10%
# Filtro: conjunto. Evaluador: coeficiente de determinación (R^2)
directSearchAlgorithm(directSearcher = "selectPercentile", list(percentile = 0.10)) -> Busqueda_p10 
filterEvaluator('determinationCoefficient') -> Evaluador_R2
directFeatureSelection(datos6_3,'slowness_in_traffic',Busqueda_p10,Evaluador_R2)$featuresSelected -> seleccionados2
seleccionados2 |> as.simple.formula("slowness_in_traffic") 

# Paquete: FSelector
# Direccionalidad: Este paquete no especifica algoritmo de búsqueda
# Filtro: conjunto. Evaluador: Correlación lineal
(linear.correlation(slowness_in_traffic~., datos6_3) -> importancia)
(importancia |>  cutoff.k(3) -> seleccionados3)
seleccionados3 |> as.simple.formula("slowness_in_traffic") 

# Paquete: FSinR
# Direccionalidad: Búsqueda metaheurística. Algoritmo: Whale Optimization
# Filtro: conjunto. Evaluador: Rough sets consistency
searchAlgorithm(searcher = "whaleOptimization") -> Busqueda_Whale
filterEvaluator('roughsetConsistency') -> Evaluador_rsc
featureSelection(datos6_3,'slowness_in_traffic',Busqueda_Whale,Evaluador_rsc)$bestFeatures -> seleccionados4
seleccionados4 |> t() |> data.frame() |> 
  dplyr::rename(selec=1) |> dplyr::filter(selec==1) |> row.names() |> 
  as.simple.formula("slowness_in_traffic") 

# Paquete: FSinR
# Direccionalidad: Búsqueda probabilística. Algoritmo: Las Vegas
# Wrapper: Elastic Net
searchAlgorithm(searcher = "LasVegas") -> Busqueda_LV
wrapperEvaluator('enet') -> Evaluador_enet
featureSelection(datos6_3,'slowness_in_traffic',Busqueda_LV,Evaluador_enet)$bestFeatures -> selecionados5
selecionados5 |> t() |> data.frame() |> 
  dplyr::rename(selec=1) |> dplyr::filter(selec==1) |> row.names() |> 
  as.simple.formula("slowness_in_traffic") 
