# ============================================================
# Grtavitty model and DiD
# ============================================================

options(repos = c(CRAN = "https://cloud.r-project.org"))
pkgs <- c("cSEM", "dplyr", "ggplot2", "broom", "ggraph", 
          "tidyr", "fixest", "stringr", "tibble", "scales")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

cat("\n[OK] Paquetes listos.\n")

# For DiD
dat_2 <- read.csv("dat_2_all.csv")

dat_2[] <- lapply(dat_2[], function(x) {
  as.numeric(gsub(",", ".", as.character(x), fixed = TRUE))
})

#For Gravity model
dat_1 <- read.csv("dat_1.csv")

dat_1 <- dat_1 %>%
  mutate(ln_xeu = log(xeu),
         ln_xch = log(xch),
         ln_meu = log(meu),
         ln_mch = log(mch),
         ln_yeu = log(yeu),
         ln_ych = log(ych),
         ln_ype = log(ype),
         ln_dis = log(dis))
head(dat_1)

# ========================Gravity model==============================

modelo_xeu <- lm(ln_xeu ~ ln_ype + ln_yeu + ln_dis_eu , data = dat_1)#valid but be careful
summary(modelo_xeu)

modelo_xeu <- lm(ln_xeu ~ ln_ype + ln_yeu + ln_dis_eu , data = dat_1) #fixe/incomplete
summary(modelo_xeu)


ggplot(dat_1, aes(x = año, y = xch)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Evolución de Exportaciones de Perú a China",
       x = "Año", y = "Exportaciones (millones USD?)") +
  theme_minimal()

#Gráfico en logaritmos
ggplot(dat_1, aes(x = año, y = ln_xch)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  labs(title = "Exportaciones Perú-China (Logaritmo)",
       x = "Año", y = "ln(Exportaciones)") +
  theme_minimal()


modelo_basico <- lm(ln_xch ~ ln_ych + ln_ype + ln_dis_ch, data = dat_1)

summary(modelo_basico)

resultados <- tidy(modelo_basico)
resultados

cat("Gravity model - basic - PERÚ vs CHINA ===\n")
cat("Ecuación estimada:\n")
cat("ln(Exportaciones) =", coef(modelo_basico)[1], "+",
    coef(modelo_basico)[2], "*ln(PIB_China) +",
    coef(modelo_basico)[3], "*ln(PIB_Peru) +",
    coef(modelo_basico)[4], "*ln(Distancia)\n\n")

cat("Interpretación:\n")
cat("- Elasticidad PIB China:", round(coef(modelo_basico)[2], 3),
    "(Un 1% ↑ en PIB China →", round(coef(modelo_basico)[2], 3), "% ↑ en exportaciones)\n")
cat("- Elasticidad PIB Perú:", round(coef(modelo_basico)[3], 3),
    "(Un 1% ↑ en PIB Perú →", round(coef(modelo_basico)[3], 3), "% ↑ en exportaciones)\n")
cat("- Elasticidad Distancia:", round(coef(modelo_basico)[4], 3),
    "(Un 1% ↑ en distancia →", round(coef(modelo_basico)[4], 3), "% ↓ en exportaciones)\n")

par(mfrow = c(2, 2))
plot(modelo_basico)
par(mfrow = c(1, 1))

# PREDICCIONES VS VALORES REALES # importante
datos_china$predichas <- exp(predict(modelo_basico))

ggplot(datos_china, aes(x = año)) +
  geom_line(aes(y = xch, color = "Real"), linewidth = 1) +
  geom_line(aes(y = predichas, color = "Predicho"), linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c("Real" = "blue", "Predicho" = "red")) +
  labs(title = "Exportaciones Reales vs Predichas - Perú a China",
       x = "Año", y = "Exportaciones", color = "") +
  theme_minimal()

# CALCULAR BONDAD DE AJUSTE EN NIVELES (no en logaritmos)
correlacion <- cor(datos_china$xch, datos_china$predichas, use = "complete.obs")
cat("\nCorrelación entre valores reales y predichos:", round(correlacion, 3))

#======================== DiD =============================================
head(dat_2)

tendencia <- dat_2 %>%
  group_by(año, tratado) %>%
  summarise(export_media = mean(export))

ggplot(tendencia, aes(x = año, y = export_media, color = as.factor(tratado))) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  geom_point() +
  labs(title = "Tendencias de Exportaciones: Grupo Tratamiento vs Control",
       subtitle = "¿Tendencias paralelas antes del tratamiento?",
       y = "Exportaciones Promedio",
       color = "Grupo") +
  scale_color_manual(labels = c("China (Control)", "EE.UU. (Tratamiento)"),
                     values = c("blue", "red")) +
  theme_minimal()

#Model
modelo_did <- feols(export ~ post + tratado + did, data = datos)
summary(modelo_did)



