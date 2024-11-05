#Resolução prova 

matriz_a <- matrix(data = c(28,32,8,9,49,7,21,35,28,10,47,43,15,34,2,48,42,19,32,26,45,44,39,50,26), byrow = TRUE, ncol = 5, nrow = 5)
matriz_a

matriz_b <- matrix(data = c(0,26,3,8,30,35,12,19,27,27,27,24,12,17,29,31,36,40,35,8,24,43,31,21,39), byrow = TRUE, ncol = 5, nrow= 5)
matriz_b

c <- solve(matriz_a%*%t(matriz_b))
View(c)
#A (a) Considere a matriz de projeção P. A soma de seus autovetores é dada por: -1,1193.


p <- matriz_b%*%(t(matriz_b)%*%(matriz_b))%*%t(matriz_b)
p_autovalores <- eigen(p)$values
sum(p_autovalores)

p_autovetores <-eigen(p)$vectors
sum(p_autovetores)
#B A soma dos valores absolutos da diagonal da matriz C é 0,0722
sum(abs(diag(c)))

#C A soma de uma matriz triangular inferior para a matriz A é 233
sum(c[lower.tri(c)])

#D O log10 do valor absoluto do determinante de A é 6,335. O log10 do valor absoluto
#do determinante de B é 6,7168. O log10 do valor absoluto do determinante da matriz
#resultante do produto matricial entre A e B é 13,0518.
log10(abs(det(matriz_a)))


log10(abs(det(matriz_b)))

log10(abs(det((matriz_a%*%matriz_b))))

#E O maior elemento da diagonal do inverso da matriz resultante do produto matricial
#entre A e o transposto de B é 0,026.

diag(solve((t(matriz_b)%*%matriz_a)))

# Somente B e D são verdadeiras


### Questão 2
require(data.table)
df2 <- fread('/chocolate.csv')
head(df2)


# a) existem 2443 países que produzem chocolate

length(unique(df2$local_compania))

# b) Existem 104 chocolates com 4 ingredientes que são descritos por 2 características
require(tidyverse)
df2<-df2 %>% separate(ingredientes,into = c("n_ingredientes","ing"), sep = '-')

df2 <- df2 %>% mutate(n_caracteristicas = str_count(caracteristicas, ",")+ 1)

df2 %>% filter(n_ingredientes == 4 & n_caracteristicas==2)


# c)A frequência absoluta para chocolates que contenham 5 ingredientes é 750.

dim(df2 %>% filter(n_ingredientes == 5))

# d) As 8 caracterististicas mais marcantes dos chocolates são sweet, nutty, cocoa, roasty,
#creamy, earthy, sandy e fatty e juntas correspondem a 1663 descrições dos chocolates.

df2<-df2 %>% separate(caracteristicas,into = c("carac1","carac2","carac3","carac4","carac5","carac6"), sep = ',')
df2 <- df2 %>% mutate(carac2 = str_squish(carac2))%>% mutate(carac3 = str_squish(carac3))%>% mutate(carac4 = str_squish(carac4))%>% 
  mutate(carac5 = str_squish(carac5)) %>% mutate(carac6 = str_squish(carac6))
require(dplyr)
teste1<- count(df2, carac1) %>% arrange(desc(n))
teste2 <-count(df2, carac2) %>% arrange(desc(n))
teste3 <- count(df2, carac3) %>% arrange(desc(n))
teste4 <- count(df2, carac4) %>% arrange(desc(n))
teste5 <- count(df2, carac5) %>% arrange(desc(n))
teste6 <- count(df2, carac6) %>% arrange(desc(n))


teste <- full_join(teste1,teste2, by = c("carac1"="carac2"))
teste <- full_join(teste,teste3, by = c("carac1"="carac3"))
teste <- full_join(teste,teste4, by = c("carac1"="carac4"))
teste <- full_join(teste,teste5, by = c("carac1"="carac5"))
teste <- full_join(teste,teste6, by = c("carac1"="carac6"))
resp <- (teste %>% group_by(carac1) %>% summarise(n = sum(n.x,n.y, n.x.x, n.y.y, n.x.x.x, n.y.y.y, na.rm= TRUE)) %>% arrange(desc(n)))
resp <- resp[1:9,] %>% filter(!is.na(carac1))
sum(resp$n)


# Existem 81 chocolates que incluem o ingrediente Adoçante em sua composição


sum(str_count(df2$ing,"S\\*"))

#### Questão 3
#Para esse exercício você deverá utilizar os banco de dados Art.csv.gz e
#Art_Moma.csv.gz. Desconsidere artistas sem nacionalidade e/ou sem nome.

art <- fread('Art.csv')
art_moma <- fread('Art_Moma.csv')


# a) Os 3 artista(s) com mais exposições no The Whitney classificados em ordem decrescente
#de exposições são: Edward Hopper, Georgia O’Keeffe e Stuart Davis

head(art)
resp_a <- art_moma %>% inner_join(art %>% select(artist_unique_id,artist_name ), by = c("artist_unique_id"="artist_unique_id"))

View(resp_a %>% group_by(artist_name) %>% summarise( n_vezes = sum(whitney_count_to_year, na.rm = TRUE))) %>% arrange(desc(n_vezes))
View(resp_a)

# b) Do total de artistas, 152 são Swiss, Mexican ou Japanese. 

View(art)

count(art,artist_nationality) %>% arrange(desc(n))


# c) Apenas 6 artista(s) com a nacionalidade Swiss tiveram entre 0 e 1 exposições no The
#Whitney.

resp_c <- art_moma %>% inner_join(art %>% select(artist_unique_id,artist_name,artist_nationality ), by = c("artist_unique_id"="artist_unique_id"))
View(resp_c %>% filter(artist_nationality == "Swiss") %>% group_by(artist_name) %>% summarise( exp = unique(whitney_count_to_year )))


# d) A diferença entre a média de páginas para artistas Brancos e Não Brancos no ano de
# 2007 é -0,24
resp_d <- art_moma %>% inner_join(art %>% select(artist_unique_id,artist_name,artist_nationality,artist_race ), by = c("artist_unique_id"="artist_unique_id"))
resp_d

view(resp_d)
brancos <- resp_d %>% filter(artist_race == "White" & year == 2007)
nao_brancos <- resp_d %>% filter(!artist_race == "White" & !artist_race == "N/A" & year == 2007)
unique(nao_brancos$artist_race)


mean(brancos$space_ratio_per_page_total) - mean(nao_brancos$space_ratio_per_page_total)

# e) Dos artista(s) que expuseram no The Whitney, apenas 164 aparecem nos livros ‘Gardner’
#e ‘Janson’.

resp_e <- unique(resp_d %>% filter(whitney_count_to_year >= 1) %>% select(artist_name))
dim(resp_e)



### Questão 4

#Para esse exercício você deverá utilizar os banco de dados refugiados_pais.csv.gz
#e refugiados.csv.gz. Considere apenas observações completas.


refugiados  <- fread('refugiados.csv')
refugiados_pais  <- fread('refugiados_pais.csv')


#A matriz de migração [origem, destino] intercontinental do ano 2006 é dada por:

View(refugiados)
View(refugiados_pais)
refugiados_pais <- refugiados_pais %>% mutate(id1 = as.character(id))


refugiados <- refugiados %>% left_join(refugiados_pais %>% select(id,id1,regiao,subregiao), by = c("id_origem" = "id"))
refugiados <- refugiados %>% left_join(refugiados_pais %>% select(id,id1,regiao,subregiao), by = c("id_destino" = "id"))


colnames(refugiados)

colnames(refugiados)[6] = "regiao_origem"
colnames(refugiados)[7] = "subregiao_origem"
colnames(refugiados)[9] = "regiao_destino"
colnames(refugiados)[10] = "subregiao_destino"

refugiados_2006 <- refugiados %>% filter(ano == 2006) %>% group_by()

summary(refugiados_2006)
# Fazendo uma pivot table. Names_from são as novas coluna, values_from são com o que os valores serão preenchidos
# values_fn aplica uma função nos valores preenchidos. Nesse caso estamos somando os valores de refugiados na migração
View(refugiados_2006 %>% select(regiao_origem,regiao_destino,refugiados)%>% pivot_wider(names_from = regiao_destino, 
                                                                                   values_from = refugiados,
                                                                                   values_fn = sum
                                                                                   ))


#A partir de 1972 houveram 172075 refugiados partindo do país: Afghanistan para o
#país: Canada, e 219920 refugiados partindo do país: Pakistan para o país: Canada.

refugiados %>% filter(ano >= 1972 & id_origem == "AFG" & id_destino == "CAN") %>% group_by(id_origem) %>% summarise(n = sum(refugiados))
refugiados %>% filter(ano >= 1972 & id_origem == "PAK" & id_destino == "CAN") %>% group_by(id_origem) %>% summarise(n = sum(refugiados))

#Os 5 países que mais enviaram refugiados no ano de 1965 pertencem às subregiões
#Sub-Saharan Africa e Southern Europe

View(refugiados %>% filter(ano == 1965) %>% group_by(subregiao_origem) %>% summarise(n_refugiados = sum(refugiados)))


#Os 6 países que mais receberam refugiados a partir de 1982 receberam juntos 19523
#refugiados.


resp_c<-refugiados %>% filter(ano == 1982) %>% group_by(id_destino) %>% summarise(ref = sum(refugiados,na.rm= TRUE)
                                                                                  ) %>% arrange(desc(ref))
resp_c                                                                                                                       


# Existem pelo menso 27 países que receberam pelo menos 5382652 refugiados. 

resp_d<-refugiados %>% group_by(id_destino) %>% summarise(ref = sum(refugiados,na.rm = TRUE)) %>% arrange(desc(ref))
dim(resp_d %>% filter(ref >= 5382652 ))
