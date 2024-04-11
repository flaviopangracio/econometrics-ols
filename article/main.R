install.packages("readxl")

library(readxl)
library(dplyr)

df_regic <- readxl::read_xlsx(
  path = "article/data/REGIC2018 Cidades v2.xlsx",
  sheet="Base de dados por Cidades"
) |>
  dplyr::filter(
    UF == "MG"
  ) |>
  dplyr::select(
    "COD_CIDADE",
    "NOME_CIDADE",
    "VAR01",
    "VAR03",
    "VAR23",
    "VAR29",
    "VAR85",
    "VAR89"
  ) |>
  dplyr::rename(
    "populacao" = "VAR01",
    "pib" = "VAR03",
    "cige" = "VAR23",
    "cgp" = "VAR29"
  ) |>
  dplyr::mutate(
    "populacao" = as.numeric(populacao),
    "pib" = as.numeric(pib),
    "cige" = as.numeric(cige),
    "cgp" = as.numeric(cgp),
    "banco_publico" = ifelse(VAR85 | VAR89, 1, 0),
    "log_cige" = ifelse(as.numeric(cige) < 1, 0, log(as.numeric(cige))),
    "log_cgp" = ifelse(as.numeric(cgp) < 1, 0, log(as.numeric(cgp)))
  )


df_regic[is.na(df_regic)] <- 0

df_regic$pib_pc <- df_regic$pib / df_regic$populacao

# Análise Descritiva

## PIB per capita
hist(df_regic$pib_pc, freq=FALSE, ylim=c(0,.07))
lines(density(df_regic$pib_pc), lwd=2)
summary(df_regic$pib_pc)

## Coeficiente de Intensidade da Gestão Empresarial (CI)
hist(df_regic$log_cige, freq=FALSE)
lines(density(df_regic$log_cige), lwd=2)
summary(df_regic$log_cige)

## Centralidade de Gestão Pública (CGP)
hist(df_regic$log_cgp, freq=FALSE)
lines(density(df_regic$log_cgp), lwd=2)
summary(df_regic$log_cige)

## Presença de banco público
table(df_regic$banco_publico)
barplot(table(df_regic$banco_publico))
summary(df_regic$banco_publico)

## Correlação entre PIBpc e CGP
cor(df_regic$pib_pc, df_regic$cgp)

## Correlação entre PIBpc e CIGE
cor(df_regic$pib_pc, df_regic$cige)

## Correlação entre PIBpc e Presença de Banco Público
cor(df_regic$pib_pc, df_regic$banco_publico)

## Correlação entre as variáveis explicativas:
cor(df_regic$cgp, df_regic$cige)
cor(df_regic$cgp, df_regic$banco_publico)
cor(df_regic$cige, df_regic$banco_publico)

## Boxplot
boxplot(df_regic$pib_pc~df_regic$banco_publico)

## Scatter Plot
plot(y = log(df_regic$pib_pc), x = df_regic$log_cige)

## MQO manual
covxy <- cov(df_regic$log_cige, log(df_regic$pib_pc))
varx <- var(df_regic$log_cige)
mediay <- mean(log(df_regic$pib_pc))
mediax <- mean(df_regic$log_cige)
b1 <- covxy/varx

b0 <- mediay - b1*mediax

formula <- log(pib_pc) ~ log_cige + log_cgp + banco_publico

mqo <- lm(formula, df_regic)
summary(mqo)

plot(resid(mqo) ~ df_regic$log_cige)
