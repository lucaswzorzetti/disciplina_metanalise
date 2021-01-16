###Packages
library(devtools)
library(litsearchr)
library(revtools)
library(stringi)
library(stringr)
library(dplyr)


#### Litsearch #####

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
library(ggplot2)

tabela <- read.csv("codificacaoR.csv", header = TRUE, sep = ";")
tabela
tabela$z_fisher <- as.numeric(tabela$z_fisher)
tabela$estrategia_predacao <- as.factor(tabela$estrategia_predacao)
#View(tabela)

tabela <- tabela %>% mutate(variancia = (1/(n-3)),
                            cor = abs(sqrt(r2)) ) 
tabela <- tabela %>% mutate(ano = c(rep(2020, 8), 2020, 2007, 1982))
tabela <- tabela %>% mutate(autores = c(rep("Twardochleb et al", 8), 
                                        "Ryan et al", "Ding-Xu et al",
                                        "Gresens et al"))
tabela <- tabela %>% mutate(ci_I = z_fisher - z_fisher_SE,
                            ci_U = z_fisher + z_fisher_SE)

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
confint(res_mod_noint) #nao há diferença alguma entre os grupos
forest(res_mod_noint)

anova(res_mod_noint) #significativo

library(ggplot2)




### Plot com moderadores

library(effsize)

##criando os modelos

#todos os estudos

model1 <- rma(yi = z_fisher, vi = variancia, vtype = "UB", data = tabela,
              weighted = TRUE, knha=FALSE, level=95, digits=4)
model1

#teste de moderacao com estrategia de pred

model2 <- rma(yi = z_fisher, vi = variancia,
              mods = ~ factor(estrategia_predacao),
              vtype = "UB", data = tabela,
              weighted = TRUE, knha=FALSE, level=95, digits=4)
model2

#teste de mod com ambiente

model3 <- rma(yi = z_fisher, vi = variancia,
              mods = ~ factor(ambiente),
              vtype = "UB", data = tabela,
              weighted = TRUE, knha=FALSE, level=95, digits=4)
model3

#teste de mod com Taxa

model4 <- rma(yi = z_fisher, vi = variancia,
              mods = ~ factor(Taxa),
              vtype = "UB", data = tabela,
              weighted = TRUE, knha=FALSE, level=95, digits=4)
model4

#teste de mod com variavel operacional

model5 <- rma(yi = z_fisher, vi = variancia,
              mods = ~ factor(taxa_medida_vo),
              vtype = "UB", data = tabela,
              weighted = TRUE, knha=FALSE, level=95, digits=4)
model5


## Plot da estrategia de predacao

model_sentesp <- rma(yi = z_fisher, vi = variancia,
                     vtype = "UB",
                     data = subset(tabela, tabela$estrategia_predacao=="senta-espera"),
                     weighted = TRUE, knha=FALSE, level=95, digits=4)
model_sentesp

model_buscativa <- rma(yi = z_fisher, vi = variancia,
                       vtype = "UB",
                       data = subset(tabela, tabela$estrategia_predacao=="busca ativa"),
                       weighted = TRUE, knha=FALSE, level=95, digits=4)
model_buscativa


plot_estrategia <- data.frame(ord = 1:14, 
                              study = c("Overral RE Model",
                                        "Overral RE Model:Senta-espera",
                                        "Overral RE Model:Busca ativa",
                                        paste(tabela$autores, tabela$ano, sep = ",")
                                                    ),
                              z = c(model1$zval,
                                    model_sentesp$zval,
                                    model_buscativa$zval,
                                    tabela$z_fisher),
                              var = c(NA, NA, NA, tabela$variancia),
                              ci_I = c(model1$ci.lb, model_sentesp$ci.lb,
                                       model_buscativa$ci.lb,
                                       tabela$ci_I
                                       ),
                              ci_U = c(model1$ci.ub, model_sentesp$ci.ub,
                                       model_buscativa$ci.ub,
                                       tabela$ci_U),
                              estrategia = c("All", "All", "All",
                                             rep("Senta-espera", 8),
                                             rep("Busca ativa", 2),
                                             "Senta-espera"))
plot_estrategia

View(plot_estrategia)

#plot estrategia

estrategia <- ggplot(data=plot_estrategia,
                     aes(y=ord, x=z, xmin=ci_I,
                         xmax=ci_U, color = estrategia)) +
  theme_classic()+
  geom_point(size = 2)+
  geom_point(data=subset(plot_estrategia, estrategia=="All"),
             size=10, shape = 18)+
  geom_errorbarh(height=.8, size =1)+
  scale_x_continuous(name="Fisher's z", breaks = c(-4:15),
                     labels = c(-4:15))+
  scale_y_continuous(name = "Studies", breaks=1:14,
                     labels = plot_estrategia$study, trans="reverse")+
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5)+
  facet_grid(estrategia~., scales = "free", space = "free")+
  ggtitle("Effect of Temperature in Performance of Predators")+
  theme(text=element_text(size=16, color="black"))
estrategia
  
  
##Plot do ambiente

model_aq <- rma(yi = z_fisher, vi = variancia,
                     vtype = "UB",
                     data = subset(tabela, tabela$ambiente=="água doce"),
                     weighted = TRUE, knha=FALSE, level=95, digits=4)
model_aq

model_ter <- rma(yi = z_fisher, vi = variancia,
                       vtype = "UB",
                       data = subset(tabela, tabela$ambiente=="terrestre"),
                       weighted = TRUE, knha=FALSE, level=95, digits=4)
model_ter


plot_ambiente <- data.frame(ord = 1:14, 
                              study = c("Overral RE Model",
                                        "Overral RE Model:Água Doce",
                                        "Overral RE Model:Terrestre",
                                        paste(tabela$autores, tabela$ano, sep = ",")
                              ),
                              z = c(model1$zval,
                                    model_aq$zval,
                                    model_ter$zval,
                                    tabela$z_fisher),
                              var = c(NA, NA, NA, tabela$variancia),
                              ci_I = c(model1$ci.lb, model_aq$ci.lb,
                                       model_ter$ci.lb,
                                       tabela$ci_I
                              ),
                              ci_U = c(model1$ci.ub, model_aq$ci.ub,
                                       model_ter$ci.ub,
                                       tabela$ci_U),
                              ambiente = c("Todos", "Todos", "Todos",
                                           rep("Água doce", 8),
                                           "Terrestre", "Terrestre",
                                           "Água doce"))
plot_ambiente

View(plot_ambiente)


#Plot do ambiente

ambiente <- ggplot(data=plot_ambiente,
                     aes(y=ord, x=z, xmin=ci_I,
                         xmax=ci_U, color = ambiente)) +
  theme_classic()+
  geom_point(size = 2)+
  geom_point(data=subset(plot_ambiente, ambiente=="Todos"),
             size=10, shape = 18)+
  geom_errorbarh(height=.8, size =1)+
  scale_x_continuous(name="Fisher's z", breaks = c(-4:15),
                     labels = c(-4:15))+
  scale_y_continuous(name = "Studies", breaks=1:14,
                     labels = plot_ambiente$study, trans="reverse")+
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5)+
  facet_grid(ambiente~., scales = "free", space = "free")+
  ggtitle("Effect of Temperature in Performance of Predators")+
  theme(text=element_text(size=16, color="black"))
ambiente


#Plot do taxa
model_odon <- rma(yi = z_fisher, vi = variancia,
                vtype = "UB",
                data = subset(tabela, tabela$Taxa=="Odonata"),
                weighted = TRUE, knha=FALSE, level=95, digits=4)
model_odon

model_thy <- rma(yi = z_fisher, vi = variancia,
                 vtype = "UB",
                 data = subset(tabela, tabela$Taxa=="Thysanoptera"),
                 weighted = TRUE, knha=FALSE, level=95, digits=4)
model_thy

model_ara <- rma(yi = z_fisher, vi = variancia,
                 vtype = "UB",
                 data = subset(tabela, tabela$Taxa=="Arachnida"),
                 weighted = TRUE, knha=FALSE, level=95, digits=4)
model_ara


plot_taxa <- data.frame(ord = 1:15, 
                            study = c("Overral RE Model",
                                      "Overral RE Model:Odonata",
                                      "Overral RE Model:Thysanoptera",
                                      "Overral RE Model:Arachnida",
                                      paste(tabela$autores, tabela$ano, sep = ",")
                            ),
                            z = c(model1$zval,
                                  model_odon$zval,
                                  model_thy$zval,
                                  model_ara$zval,
                                  tabela$z_fisher),
                            var = c(NA, NA, NA, NA, tabela$variancia),
                            ci_I = c(model1$ci.lb, model_odon$ci.lb,
                                     model_thy$ci.lb, model_ara$ci.lb,
                                     tabela$ci_I
                            ),
                            ci_U = c(model1$ci.ub, model_odon$ci.ub,
                                     model_thy$ci.ub, model_ara$ci.ub,
                                     tabela$ci_U),
                            taxa = c("Todos", "Todos", "Todos", "Todos",
                                     rep("Odonata", 8),
                                     "Thysanoptera", "Arachnida", "Odonata"))
plot_taxa

View(plot_taxa)
plot_taxa$taxa
plot_taxa <- plot_taxa[-c(3,4),]


#Plot do ambiente

taxa <- ggplot(data=plot_taxa,
                   aes(y=ord, x=z, xmin=ci_I,
                       xmax=ci_U, color = taxa)) +
  theme_classic()+
  geom_point(size = 2)+
  geom_point(data=subset(plot_taxa, ambiente=="Todos"),
             size=10, shape = 18)+
  geom_errorbarh(height=.8, size =1)+
  scale_x_continuous(name="Fisher's z")+
  scale_y_continuous(name = "Studies", breaks=1:13,
                     labels = plot_taxa$study, trans="reverse")+
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5)+
  facet_grid(taxa~., scales = "free", space = "free")+
  ggtitle("Effect of Temperature in Performance of Predators")+
  theme(text=element_text(size=16, color="black"))
taxa


#Variaveis operacionais

model_ataq <- rma(yi = z_fisher, vi = variancia,
                  vtype = "UB",
                  data = subset(tabela, tabela$taxa_medida_vo=="taxa de ataque"),
                  weighted = TRUE, knha=FALSE, level=95, digits=4)
model_ataq

model_cap <- rma(yi = z_fisher, vi = variancia,
                 vtype = "UB",
                 data = subset(tabela, tabela$taxa_medida_vo=="sucesso captura"),
                 weighted = TRUE, knha=FALSE, level=95, digits=4)
model_cap

model_manip <- rma(yi = z_fisher, vi = variancia,
                 vtype = "UB",
                 data = subset(tabela, tabela$taxa_medida_vo=="tempo de manipulação"),
                 weighted = TRUE, knha=FALSE, level=95, digits=4)
model_manip

model_cresc <- rma(yi = z_fisher, vi = variancia,
                   vtype = "UB",
                   data = subset(tabela, tabela$taxa_medida_vo=="taxa de crescimento"),
                   weighted = TRUE, knha=FALSE, level=95, digits=4)
model_cresc

model_cons <- rma(yi = z_fisher, vi = variancia,
                   vtype = "UB",
                   data = subset(tabela, tabela$taxa_medida_vo=="taxa de consumo"),
                   weighted = TRUE, knha=FALSE, level=95, digits=4)
model_cons

tabela$taxa_medida_vo <- as.character(tabela$taxa_medida_vo)

plot_var <- data.frame(ord = 1:17, 
                        study = c("Overral RE Model",
                                  "Overral RE Model:taxa de ataque",
                                  "Overral RE Model:sucesso captura",
                                  "Overral RE Model:tempo de manipulação da presa",
                                  "Overral RE Model:taxa de crescimento",
                                  "Overral RE Model:taxa de consumo",
                                  paste(tabela$autores, tabela$ano, sep = ",")
                        ),
                        z = c(model1$b[1,1],
                              model_ataq$b[1,1],
                              model_cap$b[1,1],
                              model_manip$b[1,1],
                              model_cresc$b[1,1],
                              model_cons$b[1,1],
                              tabela$z_fisher),
                        var = c(NA, NA, NA, NA, NA, NA, tabela$variancia),
                        ci_I = c(model1$ci.lb, model_ataq$ci.lb,
                                 model_cap$ci.lb, model_manip$ci.lb,
                                 model_cresc$ci.lb,
                                 model_cons$ci.lb,
                                 tabela$ci_I
                        ),
                        ci_U = c(model1$ci.ub, model_ataq$ci.ub,
                                 model_cap$ci.ub, model_manip$ci.ub,
                                 model_cresc$ci.ub, model_cons$ci.ub,
                                 tabela$ci_U),
                        variavel = c("Todos", "Todos", "Todos", "Todos",
                                 "Todos", "Todos",tabela$taxa_medida_vo))
plot_var

View(plot_var)


#Plot do var

var <- ggplot(data=plot_var,
               aes(y=ord, x=z, xmin=ci_I,
                   xmax=ci_U, color = variavel)) +
  theme_classic()+
  geom_point(size = 2)+
  geom_point(data=subset(plot_var, variavel=="Todos"),
             size=10, shape = 18)+
  geom_errorbarh(height=.8, size =1)+
  scale_x_continuous(name="Fisher's z")+
  scale_y_continuous(name = "Studies", breaks=1:17,
                     labels = plot_var$study, trans="reverse")+
  geom_vline(xintercept=0, color="black", linetype="dashed", alpha=.5)+
  facet_grid(variavel~., scales = "free", space = "free")+
  ggtitle("Effect of Temperature in Performance of Predators")+
  theme(text=element_text(size=16, color="black"))
var



model_ataq







