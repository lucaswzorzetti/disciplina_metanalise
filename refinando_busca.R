###Packages
library(devtools)
library(litsearchr)
library(revtools)
library(stringi)
library(stringr)


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


#New search results

new_search <- import_results(file = c("savedrecs_new1.txt",
                                      "savedrecs_new2.txt",
                                      "savedrecs_new3.txt",
                                      "savedrecs_new4.txt",
                                      "savedrecs_new5.txt",
                                        "scopus_new.bib"))
#Deduplicating
new_dedup <- remove_duplicates(df = new_search, field = "title",
                                 method = "exact")
new_dedup2 <- remove_duplicates(df = new_search, field = "title",
                                  method = "string_osa")

#### Revtools




