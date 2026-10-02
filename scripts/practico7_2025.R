library(dplyr)

# ELSOC

load(url("https://dataverse.harvard.edu/api/access/datafile/10797987"))  # Carga el objeto elsoc_long_2016_2023

elsoc_2023 <- elsoc_long_2016_2023 |>
  filter(ola == 7)

save(elsoc_2023, file="elsoc2023.rdata")

# CASEN

load(url("https://observatorio.ministeriodesarrollosocial.gob.cl/storage/docs/casen/2024/casen_2024.RData"))  # Carga el objeto casen_2024

set.seed(2026)  # Hace que todas y todos obtengamos la misma muestra

datos_casen <- casen_2024 |>
  select(edad, sexo, esc, educc, ytrabajocor, dau, o10, numper,
         ind_hacina, v36b, v36d, v37a) |>
  mutate(
    across(everything(), haven::zap_labels),        # Quitamos las etiquetas
    across(everything(), ~ ifelse(.x < 0, NA, .x))  # -88 (No sabe) pasa a NA
  ) |>
  filter(edad >= 18) |>                             # Solo personas de 18 años o más
  slice_sample(n = 3000) |>                         # Muestra aleatoria de 3.000 casos
  transmute(
    edad,
    mujer = ifelse(sexo == 2, 1, 0),                # 0 = Hombre, 1 = Mujer
    escolaridad = esc,
    nivel_educ = educc,
    ingreso_trabajo = ytrabajocor,
    decil = dau,
    horas_trabajo = o10,
    n_personas = numper,
    hacinamiento = ind_hacina,
    barrio_drogas = v36b,
    barrio_peleas = v36d,
    barrio_ruidos = v37a
  )

save(datos_casen, file="casen_barrio2024.rdata")

### CEP

url_cep <- "https://static.cepchile.cl/uploads/cepchile/2026/06/17-215609_gn0q_bases-96-2026.zip"

archivo_zip <- tempfile(fileext = ".zip")                       # Archivo temporal para la descarga
download.file(url_cep, archivo_zip, mode = "wb")                # Descarga el .zip
unzip(archivo_zip, files = "bases/base96.RDS", exdir = tempdir()) # Extrae solo la base en formato R

cep <- readRDS(file.path(tempdir(), "bases", "base96.RDS"))     # Carga la base

datos_cep <- cep |>
  select(edad, sexo, esc_nivel_1_c, info_enc_20_c, educacion_104_a, iden_pol_2,
         interes_pol_1_b, confianza_6_j, confianza_6_h, confianza_6_c, confianza_6_i,
         confianza_6_k, democracia_20, democracia_38, ciudadania_29_a, ciudadania_30_b,
         iden_nacional_8_a) |>
  mutate(
    across(everything(), haven::zap_labels),         # Quitamos las etiquetas
    across(everything(), ~ ifelse(.x < 0, NA, .x))   # -8 (No sabe) y -9 (No contesta) pasan a NA
  ) |>
  transmute(
    edad,
    mujer = ifelse(sexo == 2, 1, 0),                 # 0 = Hombre, 1 = Mujer
    educ = esc_nivel_1_c,
    ingreso_tramo = info_enc_20_c,
    estatus = educacion_104_a,
    pos_pol = iden_pol_2,
    interes_pol = 6 - interes_pol_1_b,               # Invertimos: 1 = nada … 5 = muy interesado
    conf_partidos = 5 - confianza_6_j,               # Invertimos: 1 = nada … 4 = mucha confianza
    conf_carab = 5 - confianza_6_h,
    conf_ffaa = 5 - confianza_6_c,
    conf_gobierno = 5 - confianza_6_i,
    conf_congreso = 5 - confianza_6_k,
    democracia_func = democracia_20,
    seguridad_libertad = democracia_38,
    just_marcha = 6 - ciudadania_29_a,               # Invertimos: 1 = nunca … 5 = siempre se justifica
    just_fuerza_carab = 6 - ciudadania_30_b,
    inmig_crimen = 6 - iden_nacional_8_a             # Invertimos: 1 = muy en desacuerdo … 5 = muy de acuerdo
  )

save(cep, file="cep_2026.rdata")


## LAPOP

archivo_lapop <- tempfile(fileext = ".rda")    # Archivo temporal para la descarga
download.file("https://raw.githubusercontent.com/lapop-central/lapop/main/data/ym23.rda",
              archivo_lapop, mode = "wb")
load(archivo_lapop)                             # Carga el objeto ym23

datos_lapop <- ym23 |>
  mutate(pais_name = as.character(haven::as_factor(pais))) |>  # Nombre de cada país
  mutate(across(-pais_name, haven::zap_labels)) |>             # Quitamos las etiquetas
  filter(wave == 2023) |>                                      # La base también trae la ronda 2018/19
  transmute(
    pais_name,
    conf_ffaa = b12,
    conf_policia = b18,
    apoyo_dem = ing4,
    satisf_dem = 5 - pn4,                                      # Invertimos: 1 = muy insatisfecho … 4 = muy satisfecho
    educ = edre,
    riqueza = wealth,
    mujer = case_when(q1tc_r == 2 ~ 1, q1tc_r == 1 ~ 0)        # 0 = Hombre, 1 = Mujer
  ) |> 
  slice_sample(n = 10000)

save(datos_lapop, file="subset_lapop2023.rdata")
