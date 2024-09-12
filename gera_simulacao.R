library(readr)
library(tidyverse)

info_consolidadas_dividas <- read_delim("20240912-11-info-consolidadas-dividas.csv", 
                                                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                                                         grouping_mark = ".", encoding = "LATIN1"), 
                                                     trim_ws = TRUE)

info_consolidadas_dividas <- janitor::clean_names(info_consolidadas_dividas)

dividas_estados<-
  info_consolidadas_dividas %>%
  filter(tipo_de_ente == "Estado",  
         tipo == "União",
         consolidado_por == "Tipo de credor",
         !uf %in% c("RS","MG"))

# Definir as probabilidades para 0, 1 e 2
probabilidades <- c(0.90, 0.09, 0.01)
probabilidades_mg <- c(0.19,0.01,0.8)

# Gerar um vetor de 100 elementos com base nas probabilidades definidas
vetor <- sample(c(0, 1, 2), size = 100, replace = TRUE, prob = probabilidades)

vetor_mg <- sample(c(0, 1, 2), size = 100, replace = TRUE, prob = probabilidades_mg)

num_rodadas<- 1000

rodadas_estados_s_mg<-
map_dfr(1:num_rodadas, function(rodada){
  map2_dfr(dividas_estados$uf, sample(c(0, 1, 2), size = 25, replace = TRUE, prob = probabilidades), function(uf, juros){
    tibble(uf= uf, rodada= rodada,  juros = juros )
  })
  
  
})


rodadas_mg<-
  tibble(uf= rep("MG",num_rodadas), rodada= 1:num_rodadas, juros= sample(c(0, 1, 2), size = num_rodadas, replace = TRUE, prob = probabilidades_mg) )


rodadas_estados<-
  bind_rows(rodadas_estados_s_mg, rodadas_mg)

simulacao<-
  rodadas_estados %>%
  inner_join(
    dividas_estados %>%
      select(uf, valor_r)
  ) %>%
  mutate(valor_pagamento_juros = valor_r * juros/100,
         valor_devido = valor_r * 0.062,
         valor_subsidio = valor_devido - valor_pagamento_juros)
  
simulacao_agregada<-
  simulacao %>%
  summarise(subsidio_total = sum(valor_subsidio),
            .by = rodada)

mean(simulacao_agregada$subsidio_total)
median(simulacao_agregada$subsidio_total)
sd(simulacao_agregada$subsidio_total)


29.319.098.533
1.630.772.706

hist(simulacao_agregada$subsidio_total)
boxplot(simulacao_agregada$subsidio_total)

boxplot(simulacao$valor_subsidio)

t.test(simulacao_agregada$subsidio_total)
