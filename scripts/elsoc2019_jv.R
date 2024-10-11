
# Cargar liberías
pacman::p_load(tidyverse)

# Cargar base
elsoc <- load(url("https://dataverse.harvard.edu/api/access/datafile/4606521"))

# Procesar base

elsoc <- elsoc_2019 %>% dplyr::select(
  # idencuesta,
  # Variables dependientes
  ## Justificacion violencia por el control social
  jv_control = f05_03, # Carabineros reprimen marchas

  ## Justificacon violencia por el cambio social
  jv_cambio = f05_07, # Estudiantes tiren piedras
  
  ## Justicia distributiva
  salario_perc_ceo = d03_01_rev, # Salario percibido CEO
  salario_perc_obrero = d03_02_rev, # Salario percibido obrero
  salario_just_ceo = d04_01_rev, # Salario justo CEO
  salario_just_obrero = d04_02_rev, # Salario justo obrero
  ingreso_justo = m15_rev, # Ingreso considerado justo
  
  # Variables de control (literatura)
  
  ## Pertenencia a grupos desventajados
  
  ## Nota: Las variables relacionadas a a ingresos que contengan un "rev" están revisadas por el equipo que creo la encuesta, por lo que están exentas de problemas de digitacion y similares.
  
  ingreso = m13_rev, # Ingreso mensual (monto)
  ingreso_tramos = m14, # Ingreso mensual (tramos)
  educ = m01, # Nivel educacional del entrevistado
  ingreso_satisfact = m16, # Satisfaccion con los ingresos
  sexo = m0_sexo, # Sexo del entrevistado
  indigena = m53, # Pertenencia a pueblos indigenas
  
  ## Ideología (SDO y RWA)
  sdo_soc_ideal = c18_01, # Una sociedad ideal requiere que algunos grupos estén en una posicion superior y otros en una posicion inferior
  sdo_grupos_inferiores = c18_12, # Algunos grupos de personas son simplemente inferiores a otros grupos
  sdo_oportunidades_iguales = c18_02, # Debiéramos trabajar para dar a todos los grupos la misma oportunidad de tener éxito
  sdo_condiciones_iguales = c18_03, # Deberíamos hacer todo lo posible por igualar las condiciones de diferentes grupos
  rwa_gobierno_firme = c18_04, # En vez de tanta preocupacion por los derechos de las personas, lo que este país necesita es un gobierno firme
  rwa_mandatario_fuerte = c18_05, # Lo que nuestro país necesita es un mandatario/a fuerte con la determinacion para llevarnos por el camino correcto
  rwa_valores_ninos = c18_06, # La obediencia y el respeto por la autoridad son los valores más importantes que los niños debieran aprender
  rwa_obediencia_discplina = c18_07, # Las verdaderas claves para tener una buena vida son la obediencia y la disciplina
  
  ## Percepcion trato justo
  trato_carab = c35_03, # Trato respetuoso carabineros
  
  # Variables de control (exploratoria)
  edad = m0_edad, # Edad del entrevistado
  pos_pol = c15, # Posicion política
  frec_marcha = c08_02, # Frecuencia participacion en marchas
  conf_carab = c05_03 # Confianza en carabineros
) %>% 
  mutate(
    across(everything(), ~if_else(. %in% c(-666, -777, -888, -999), NA, .)),
    sj_gerente = log(salario_perc_ceo/salario_just_ceo),
    sj_gerente = if_else(sj_gerente < 0, 0, sj_gerente),
    sj_gerente_rec = case_when(
      sj_gerente > 0 ~ 1,
      sj_gerente <= 0 ~ 0
    ),
    jv_cambio_rec = case_when(
      jv_cambio %in% c(2:5) ~ 1,
      jv_cambio == 1 ~ 0
      ), 
    jv_control_rec = case_when(
      jv_control %in% c(2:5) ~ 1,
      jv_control == 1 ~ 0
    ),
    ingreso_rec = if_else(ingreso > 800000, 1, 0), # El ingreso mediano en chile al 2019 era de 401.000
    educ_rec = case_when(
      educ %in% c(1:8) ~ 0,
      educ %in% c(9:10) ~ 1,
      )
    
    ) %>% labelled::set_variable_labels(
      ingreso = "Ingresos del entrevistado",
      ingreso_rec = "Ingresos mayores a 800.000 (binario)",
      educ = "Nivel educacional del entrevistado",
      educ_rec = "Educación universitaria o más (binario)",
      jv_cambio = "Justificación de la violencia por el cambio social",
      jv_cambio_rec = "Justificación de la violencia por el cambio social (binario)",
      jv_control = "Justificación de la violencia por el control social",
      jv_control_rec = "Justificación de la violencia por el control social (binario)",
      sj_gerente = "Sentido de ijusticia gerente",
      sj_gerente_rec = "Sentido de injusticia gerente (binario)"
    )

# Etiquetar valores
elsoc$ingreso_rec <-  sjlabelled::set_labels(elsoc$ingreso_rec, labels =  c("Menor a 800k" = 0, "Mayor a 800k" = 1))
elsoc$educ_rec <-  sjlabelled::set_labels(elsoc$educ_rec, labels =  c("No universitaria" = 0, "Universitaria o más" = 1))
elsoc$jv_cambio_rec <-  sjlabelled::set_labels(elsoc$jv_cambio_rec, labels =  c("No justifica" = 0, "Justifica" = 1))
elsoc$jv_control_rec <- sjlabelled::set_labels(elsoc$jv_control_rec, labels = c("No justifica" = 0, "Justifica" = 1))
elsoc$sj_gerente_rec <- sjlabelled::set_labels(elsoc$sj_gerente_rec, labels = c("Justo" = 0, "Injusto" = 1))

# Frecuencias
sjmisc::frq(elsoc$ingreso_rec)
sjmisc::frq(elsoc$educ_rec)

sjmisc::frq(elsoc$jv_control)
sjmisc::frq(elsoc$jv_control_rec)

sjmisc::frq(elsoc$jv_cambio)
sjmisc::frq(elsoc$jv_cambio_rec)

sjmisc::frq(elsoc$sj_gerente)
sjmisc::frq(elsoc$sj_gerente_rec)

# Guardar

save(elsoc, file = "elsoc2019_jv.RData")
