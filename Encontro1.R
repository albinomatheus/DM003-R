# Encontro 1 
# Matheus Alves Albino
#pacman::p_load(tidyverse, read.dbc, haven)

# Tipos básicos de classes de dados
# Character (tipo texto simples)
a <- "Brasil"
# Numeric (tipo número real)
b <- 1.25
# Integer (tipo número inteiro)
c <- 2L
# Logical (tipo lógico TRUE/FALSE)
d <- TRUE

# Operações entre vetores
x <- c("Brasil", "Chile", "Argentina")
y <- c(213.3, 18.95, 44.94)
z <- c(1000, 100, 10)
y*z
y/z
z3 <- exp(z2)
z4 <- log(y)
z5 <- z4 < 4.2

# Funções
max(y)
min(y)
sum(y)
length(y)
sd(y)

# Sequências
s <- seq(-1, 1, by = 0.2)
s1 <- 1:10
# Data frame
m <- data.frame(nome = x,
                pop = y, 
                log_pop = z4)
n <- m[-1] # Deleta colunas
n <- m[,-1] # Deleta linhas
m$nome # Variável NOME do banco M
m$exp_pop <- exp(m$pop) # Nova variável
# Fatores
f <- y < 100
m$f <- factor(f, labels = c("Maior que 100 milhões", "Menor que 100 milhões"))

rm(list = ls())
#########################################
### Abertura de base de dados
#########################################
# install.packages("tidyverse")
library(tidyverse) # Algumas aplicações com e sem os pacotes do tidyverse

### Leitura de banco de dados
# Lendo um arquivo TXT
data <- read.table("preston.txt", sep = '\t', header = T)
data <- read_tsv("preston.txt") # No tidyverse: readr
# Lendo um arquivo CSV
data2 <- read.table("preston.csv", sep = ',', header = T)
data2 <- read_csv("preston.csv") # No tidyverse: readr
# Lendo um CSV separado por ponto e vírgula
data3 <- read.table("preston2.csv", sep = ',', header = T) # Erro comum
data3 <- read.table("preston2.csv", sep = ';', header = T)
data3 <- read_csv2("preston2.csv")
# Lendo um arquivo de STATA
# library(haven) ou library(foreign) 
# read_dta()
# Lendo um arquivo de SPSS
data5 <- read_sav("BOLIVIA.sav")
# Lendo um arquivo de microdados do SINASC
# library(read.dbc)
data6 <- read.dbc("SINASC.dbc")

#########################################
### Manipulações básicas usando tidyverse
#########################################
# O "pipe": outra forma de escrever funções compostas
n = 2
sqrt(n) # Raiz quadrada de w
log(sqrt(n)) # Log da raiz de w
n %>% sqrt %>% log

# O "verbo" mutate: adiciona variáveis que são transformações de variáveis existentes
data %>% mutate(rate = pop/deaths * 1000)

# O "verbo" select: seleciona variáveis baseadas em seus nomes
data %>% select(country, pop)

# O "verbo" filter: filtra os casos baseados em seus valores
data %>% filter(country == "Sweden")

# O "verbo" rename: renomeia as variáveis
data %>% rename(pais = country, idade = age, populacao = pop, mortes = deaths)

# O "verbo" summarise: resume as informações do banco de dados
data %>% group_by(country) %>% summarise(media_pop = mean(pop))

#########################################
### Aplicações em Demografia
#########################################
## Estrutura etária da Suécia
SWE <- data %>% 
  filter(country == "Sweden") %>% 
  select(age,pop)

## Taxas Específicas de Mortalidade
TEM_SWE <- data %>% 
  filter(country == "Sweden") %>% 
  mutate(rate = deaths/pop * 1000)

## Taxas Brutas de Mortalidade
TBM <- data %>% 
  group_by(country) %>% 
  mutate(rate = deaths/pop * 1000) %>% 
  summarize(TBM = weighted.mean(rate, pop))
  
## Taxas de Mortalidade Padronizadas
# Passo 1: Calcular estrutura etária percentual
POP <- data %>% 
  group_by(country) %>% 
  mutate(rate = deaths/pop * 1000) %>% 
  mutate(pct = pop/sum(pop))
# Passo 2: Usar estrutura padrão (média das duas populações)
STD <- POP %>% 
  group_by(age) %>% 
  summarize(pop = mean(pct), 
            rate = mean(rate))
# Passo 3: Taxa de Mortalidade Padronizada
POP %>% summarize(tbm_p = weighted.mean(rate, STD$pop))     

## Padronização Indireta
# Se o Cazaquistão tivesse as taxas específicas da Suécia:  
POP %>% summarise(TBM = weighted.mean(rate,pop), # Taxa Bruta de Mortalidade
                  TBM_PI = weighted.mean(SWE$rate, pop), # Taxa de Mortalidade Padronizada
                  SMR = TBM/TBM_PI) # Razão de Mortalidade Padronizada

# Taxa de Mortalidade é 77% maior do que seria se o Cazaquistão apresentasse as taxas da Suécia                  
