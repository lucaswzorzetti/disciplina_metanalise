###Packages
library(devtools)
library(litsearchr)
library(revtools)
library(stringi)
library(stringr)
library(dplyr)


#### Litsearch

#importing
naive_search <- import_results(file = c("savedrecs_teste.txt",
                                        "scopus_teste.bib"))

#Removing duplicates

naive_dedup <- remove_duplicates(df = naive_search, field = "title",
                                 method = "exact")
naive_dedup2 <- remove_duplicates(df = naive_search, field = "title",
                                  method = "string_osa")

#Extracting terms

raked_keywords <- extract_terms(
  text = paste(naive_dedup2$title, naive_dedup2$abstract))

taggedkeywords <- extract_terms(
  text = paste(naive_dedup2$keywords, naive_dedup2$author_keywords)
)

all_keywords <- unique(append(taggedkeywords, raked_keywords))

#creating a dictionary of possible keywords

naive_dic <- create_dfm(
  elements = paste(naive_dedup2$title, naive_dedup2$abstract),
  features = all_keywords
)

#Creating a keyword co-ocurrence network

naive_graph <- create_network(
  search_dfm = as.matrix(naive_dic), min_studies = 5, min_occ = 3
)

#Creating a cutoff on node strength

cutoff <- find_cutoff(graph = naive_graph, method = "cumulative")
cutoff

#Applying the cutoff

reduced_graph <- reduce_graph(graph = naive_graph, 
                              cutoff_strength = cutoff[1])

#The search terms
search_terms <- get_keywords(reduced_graph)
head(search_terms, n = 20)

View(search_terms)

#Creating a spreedsheet

write.csv(search_terms, "search_terms_teste.csv", row.names = FALSE)

##grouping manually
#...

#grouped search terms

grouped_terms <- read.csv(file = "search_terms_grouped.csv", header = TRUE,sep = ";")

View(grouped_terms)

#extracting grouping
pop_terms <- grouped_terms$x[grep("populacao", grouped_terms$group)]
inter_terms <- grouped_terms$x[grep("intervencao", grouped_terms$group)]
tipo_terms <- grouped_terms$x[grep("tipo", grouped_terms$group)]
controle_terms <- grouped_terms$x[grep("controle", grouped_terms$group)]
resp_terms <- grouped_terms$x[grep("resposta", grouped_terms$group)]

#Creating a list
mysearchterms <- list(pop_terms, inter_terms,
                      tipo_terms, controle_terms, resp_terms)

#Writing a search

my_search <- write_search(groupdata = mysearchterms, exactphrase = TRUE,
                          writesearch = FALSE, languages = "English", closure = "left",
                          stemming = TRUE, verbose = TRUE)

my_search


#New search:

#(("trophic* interact*" OR predat* OR "consum* resourc*" OR 
#"consum* and resourc*" OR consumer-resourc*) AND ("climat* chang*" OR 
#"global* warm*" OR "temperatur* effect*" OR "water* temperatur*" OR 
#"climat* warm*" OR "higher* temperatur*" OR "increas* temperatur*" OR 
#"lower* temperatur*" OR "temperatur* increas*" OR "temperatur* influenc*"
#OR "temperatur* regim*" OR warm* OR "optim* temperatur*" OR "thermal* 
#environ*" OR "temperatur* depend*") AND (ectotherm* OR poikilotherm*)
#AND ("function* respons*" OR "consumpt* rate*" OR "feed* rate*" OR "predat* 
#rate*" OR "kill* rate*" OR "consum* rate*" OR "clearanc* rate*" OR "attack*
#rate*" OR captur* OR "handl* time*" OR "prey* manipul* time*" OR "prey* 
#handl*" OR "growth* rate*"))

#In Web of Science and Scopus

#New search results

definitive_search <- import_results(file = c("savedrecs_definitivo.txt",
                                        "scopus_definitivo.bib"))
#Deduplicating
def_dedup <- remove_duplicates(df = definitive_search, field = "title",
                                 method = "exact")
def_dedup2 <- remove_duplicates(df = definitive_search, field = "title",
                                  method = "string_osa")

#Salvando
write.csv(def_dedup2, "busca_definitivo.csv", row.names = FALSE)


#### Revtools
table <- screen_abstracts(def_dedup2)
View(table)

screen_topics(def_dedup2)


screened <- screen_abstracts()
View(screened)


screened_selected <- screened %>% filter(screened_abstracts == "selected")
View(screened_selected)

write.csv(screened_selected, "selected.csv", row.names = FALSE)

selected <- read.csv(file = "selected.csv", header = T)
screen_titles(selected)





##### CALCULANDO EFFECT SIZE #####
library(dplyr)
library(metafor)

tabela <- read.csv("codificacaoR.csv", header = TRUE, sep = ";")
tabela
tabela$z_fisher <- as.numeric(tabela$z_fisher)
#View(tabela)

tabela <- tabela %>% mutate(variancia = (1/(n-3)),
                            cor = abs(sqrt(r2)) ) 
tabela <- tabela %>% mutate(ano = c(rep(2020, 8), 2020, 2007, 1982))
tabela <- tabela %>% mutate(autores = c(rep("Twardochleb et al", 8), 
                                        "Ryan et al", "Ding-Xu et al",
                                        "Gresens et al"))

View(tabela)

str(tabela)

### Modelo aleatorio
res <- rma(yi = z_fisher, vi = variancia, vtype = "UB", data = tabela,
                      weighted = TRUE, knha=FALSE, level=95, digits=4)
res
summary(res)

#forest plot do modelo aleatorio
forest(res, slab = paste(tabela$autores, tabela$ano, sep = ", "),
       header = "Author(s) and Year", mlab="")
text(-12, -1, pos=4, cex=0.75, bquote(paste("RE Model (Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
#Heterogeneidade
funnel(res) #bem heterogeneo

radial(res)

qqnorm(res)

influence.rma.uni(res) #parecem normais os residuos e suas distancias

regtest(res) #é assimetrico entao (?)

#fail safe number
fsn(yi = tabela$z_fisher, vi = tabela$variancia, type = "Rosenthal")
fsn(yi = tabela$z_fisher, vi = tabela$variancia, type = "Rosenberg")
fsn(yi = tabela$z_fisher, vi = tabela$variancia, type = "Orwin")

#
trimfill(res)

cumul(res)
forest(cumul(res))

rstudent.rma.uni(res)

###Com moderadores
##estrategia predacao
#modelo com intercepto p/ obter heterogeneidade
res_mod <- rma(yi = z_fisher, vi = variancia,
               mods = ~factor(estrategia_predacao),
               vtype = "UB", data = tabela,
               weighted = TRUE, knha=FALSE, level=95, digits=4)
res_mod
confint(res_mod) #valores das heterogeidades
anova(res_mod, type = "II") #nao significativo

#sem intercepto
res_mod_noint <- rma(yi = z_fisher, vi = variancia,
               mods = ~factor(estrategia_predacao)-1,
               vtype = "UB", data = tabela,
               weighted = TRUE, knha=FALSE, level=95, digits=4)
res_mod_noint
confint(res_mod_noint)
forest(res_mod_noint)

anova(res_mod_noint) #significativo

library(ggplot2)













ad












res <- rma(yi = z_fisher, vi = variancia, measure = "ZCOR",
           data = tabela, slab = autores)

res
summary(res)

forest(res, header = c("studies", "Confidence Interval"), mlab = "summary")

#com moderadores
model_mod <- rma(yi = z_fisher, se = tabela$z_fisher_SE, measure = "ZCOR",
           mods = cbind(estrategia_predacao, clima, Taxa, ano, ambiente),
           data = tabela, slab = autores)
summary(model_mod)
model_mod

forest(model_mod)


forest(model_mod,
        slab = paste(tabela$autores, tabela$ano, sep = ", "))
preds <- predict.rma(model_mod, newmods = c("senta-espera", "busca ativa"))
addpoly(preds$pred, sei = preds$se, atransf = exp,
           mlab = c("10 Degrees", "30 Degrees", "50 Degrees"))
text(-9, 15, "Author(s) and Year", pos = 4, font = 2)
text(7, 15, "Relative Risk [95% CI]", pos = 2, font = 2)
abline(h = 0)




#heterogeneidade
funnel(model_mod)
funnel(res)



#cummulative met
cumul <- cumul.rma.uni(x = res, order = order(tabela$ano), 
                       data = tabela)

forest.cumul.rma(cumul, header = TRUE, ilab = tabela$ano, ilab.xpos = 1)












