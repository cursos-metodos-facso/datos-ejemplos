# Función para crear dataset con cantidades de NSNR fijas y pasar NSNR a NA
crear_dataset <- function(seed, n, cor_sig, chi_sig) {
    # Librerías
    library(dplyr)

    # Setear semilla
    set.seed(seed)

    # Crear variables
    id <- 1:n # Identificador de encuesta (correlativo)
    clase_subj <- sample(1:5, n, replace = TRUE) # Clase subjetiva
    autoritarismo <- sample(1:5, n, replace = TRUE) # Autoritarismo
    educ <- sample(1:11, n, replace = TRUE) # Educación (sin NSNR)
    pos_pol <- sample(1:10, n, replace = TRUE) # Posición política

    # Crear data.frame
    data <- data.frame(id, clase_subj, autoritarismo, educ, pos_pol)

    # Recodificaciones
    data <- data %>% mutate(
        educ_rec = ifelse(educ >= 8, 1, 0), # 1 = Universitaria o más, 0 = No universitaria
        pos_pol_rec = case_when(
            pos_pol %in% 1:3 ~ 1, # Izquierda
            pos_pol %in% 4:7 ~ 2, # Centro
            pos_pol %in% 8:10 ~ 3 # Derecha
        )
    )

    # Ajuste para correlaciones específicas (Spearman)
    if (cor_sig) {
        data <- data %>% mutate(
            autoritarismo = if_else(clase_subj == 5, 5, autoritarismo) # Si la persona es de clase alta, entonces darle puntaje mayor de autoritarismo
        )
    }

    # Ajuste para Chi-cuadrado
    if (chi_sig) {
        data <- data %>% mutate(
            pos_pol_rec = if_else(educ >= 8, 3, pos_pol_rec) # Si la persona tiene educación universitaria, entonces es de derecha
        )
    }

    # Crear NA's en las cantidades requeridas
    set.seed(seed) # Asegurar reproducibilidad para los NA
    data <- data %>%
        mutate(
            clase_subj = replace(clase_subj, sample(1:n, 20), NA), # 20 casos NA en clase_subj
            autoritarismo = replace(autoritarismo, sample(1:n, 10), NA), # 10 casos NA en autoritarismo
            pos_pol = replace(pos_pol, sample(1:n, 150), NA), # 150 casos NA en pos_pol
            pos_pol_rec = if_else(is.na(pos_pol), NA, pos_pol_rec) # Emular NA en pos_pol_rec según pos_pol
        )

    return(data)
}

etiquetar <- function(data) {
    # Etiquetar
    data <- data %>% labelled::set_variable_labels(
        clase_subj = "Clase social subjetiva",
        autoritarismo = "Autoritarismo",
        educ = "Nivel educacional",
        pos_pol = "Posición política",
        educ_rec = "Nivel educacinal recodificado (universitario o no)",
        pos_pol_rec = "Posición política recodificada"
    )

    # Etiquetar variables usando sjlabelled::set_labels() para mantenerlas numéricas
    data <- data %>%
        mutate(
            # Etiquetas para clase_subj
            clase_subj = sjlabelled::set_labels(clase_subj,
                labels = c(
                    "Clase baja" = 1,
                    "Clase media-baja" = 2,
                    "Clase media" = 3,
                    "Clase media-alta" = 4,
                    "Clase alta" = 5
                )
            ),

            # Etiquetas para autoritarismo
            autoritarismo = sjlabelled::set_labels(autoritarismo,
                labels = c(
                    "Muy en desacuerdo" = 1,
                    "En desacuerdo" = 2,
                    "Ni en desacuerdo ni de acuerdo" = 3,
                    "De acuerdo" = 4,
                    "Muy de acuerdo" = 5
                )
            ),

            # Etiquetas para educ
            educ = sjlabelled::set_labels(educ,
                labels = c(
                    "Sin estudios" = 1,
                    "Basica incompleta" = 2,
                    "Basica completa" = 3,
                    "Media incompleta" = 4,
                    "Media completa" = 5,
                    "Tecnica superior incompleta" = 6,
                    "Tecnica superior completa" = 7,
                    "Universitaria incompleta" = 8,
                    "Universitaria completa" = 9,
                    "Magister o doctorado incompleto" = 10,
                    "Magister o doctorado completo" = 11
                )
            ),

            # Etiquetas para pos_pol
            pos_pol = sjlabelled::set_labels(pos_pol,
                labels = c("Izquierda" = 1, "Derecha" = 10)
            ),

            # Etiquetas para educ_rec
            educ_rec = sjlabelled::set_labels(educ_rec,
                labels = c(
                    "No universitaria" = 0,
                    "Universitaria o más" = 1
                )
            ),

            # Etiquetas para pos_pol_rec
            pos_pol_rec = sjlabelled::set_labels(pos_pol_rec,
                labels = c(
                    "Izquierda" = 1,
                    "Centro" = 2,
                    "Derecha" = 3
                )
            )
        )
}

# Crear datasets
dataset1 <- crear_dataset(seed = 1, n = 1000, cor_sig = TRUE, chi_sig = FALSE) %>% etiquetar(.)
dataset2 <- crear_dataset(seed = 1, n = 1000, cor_sig = FALSE, chi_sig = TRUE) %>% etiquetar(.)
dataset3 <- crear_dataset(seed = 1, n = 1000, cor_sig = FALSE, chi_sig = FALSE) %>% etiquetar(.)
dataset4 <- crear_dataset(seed = 1, n = 1000, cor_sig = TRUE, chi_sig = TRUE) %>% etiquetar(.)

# Revisar
sjmisc::frq(dataset1$clase_subj)
sjmisc::frq(dataset2$clase_subj)
sjmisc::frq(dataset3$clase_subj)
sjmisc::frq(dataset4$clase_subj)

sjmisc::frq(dataset1$autoritarismo)
sjmisc::frq(dataset2$autoritarismo)
sjmisc::frq(dataset3$autoritarismo)
sjmisc::frq(dataset4$autoritarismo)

sjmisc::frq(dataset1$educ)
sjmisc::frq(dataset2$educ)
sjmisc::frq(dataset3$educ)
sjmisc::frq(dataset4$educ)

sjmisc::frq(dataset1$pos_pol)
sjmisc::frq(dataset2$pos_pol)
sjmisc::frq(dataset3$pos_pol)
sjmisc::frq(dataset4$pos_pol)

sjmisc::frq(dataset1$educ_rec)
sjmisc::frq(dataset2$educ_rec)
sjmisc::frq(dataset3$educ_rec)
sjmisc::frq(dataset4$educ_rec)

sjmisc::frq(dataset1$pos_pol_rec)
sjmisc::frq(dataset2$pos_pol_rec)
sjmisc::frq(dataset3$pos_pol_rec)
sjmisc::frq(dataset4$pos_pol_rec)

# Test estadísticos
# dataset1
cor.test(dataset1$clase_subj, dataset1$autoritarismo, complete.obs = TRUE, method = "spearman")
chisq.test(dataset1$educ_rec, dataset1$pos_pol_rec)

# dataset2
cor.test(dataset2$clase_subj, dataset2$autoritarismo, complete.obs = TRUE, method = "spearman")
chisq.test(dataset2$educ_rec, dataset2$pos_pol_rec)

# dataset3
cor.test(dataset3$clase_subj, dataset3$autoritarismo, complete.obs = TRUE, method = "spearman")
chisq.test(dataset3$educ_rec, dataset3$pos_pol_rec)

# dataset4
cor.test(dataset4$clase_subj, dataset4$autoritarismo, complete.obs = TRUE, method = "spearman")
chisq.test(dataset4$educ_rec, dataset4$pos_pol_rec)

# Guardar
save(dataset1, file = "dataset1.RData")
save(dataset2, file = "dataset2.RData")
save(dataset3, file = "dataset3.RData")
save(dataset4, file = "dataset4.RData")
