library(simmer)
library(simmer.plot)

# levantamento estatistico
FREQ_FALHA_BASE= 4 # frequencia de pane no ano
ESFORCO_BASE = 485 # horas de utilização para a freq de panes
TAT=15 # turn around time (meses)

# configuracao
ANVS = 2 # numero de aeronaves
QPA = 2 # qtd de ites por aeronave
GIRO = 5 # qtd de itens ativos

# esforco para simulacao
ESFORCO = 485
FREQ_FALHA = FREQ_FALHA_BASE*ESFORCO_BASE/ESFORCO

aeronaves <- paste0("anv_",1:ANVS)

set.seed(42)

env <- simmer("Esforco")

esforco <- function(anv) 
  trajectory() %>%
    log_("utilizar") %>%
    seize("item", QPA) %>%  
    timeout(1000) %>%    
    log_("reiniciar") %>%
    rollback(2,Inf)

# PREEMPT trajetória "aplica"
reparo <- trajectory() %>%  
  seize("item",1) %>%
  log_("reparar item") %>%
  timeout(TAT) %>% # tempo de reparo em meses
  log_("liberar item") %>%
  release("item",1)  

# o gerador "reparo" tem prioridade sobre o "anv_x", assim, se não houver
# disponibilidade de item, uma falha faz com que o item de "anv_x" seja
# remanejado para "reparo"

env %>% add_resource("item", GIRO,preemptive=TRUE) %>% # qtd de itens
add_generator("reparo", reparo, function() FREQ_FALHA, priority=1) # freq de falhas em meses

for (anv in aeronaves) 
  env %>% add_generator(paste0(anv,"_aplicacao"), esforco(anv), at(0), restart = TRUE)


env %>% reset() %>% run(30) # em meses

#env %>% get_mon_resources() %>% head()
#env %>% get_mon_arrivals() %>% head()
#plot(get_mon_resources(env), metric = "usage", items="queue",steps=TRUE)
