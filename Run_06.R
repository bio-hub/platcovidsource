#df for articles
#### INDEX ####
# iniciar na pasta corona
number_of_article = length(abstracts@PMID) # Number of Articles
number_abstract = nrow(pmid_abs) # Number of Abstracts

box.1 = sapply(db[which(db$pmid %in% pmid_abs$pmid),"type"], function(x) sub("\\Journal Article\\ \\|\\ Journal Article", "Journal Article",x))
box.1 = sapply(box.1, function(x) sub("\\Editorial\\ \\|\\ Editorial", "Editorial",x))
box.1 = sapply(box.1, function(x) sub("\\Editorial\\ \\|\\ Journal Article", "Journal Article | Editorial",x))
box.1 = sapply(box.1, function(x) sub("\\Editorial\\ \\|\\ Review", "Review | Editorial",x))
box.1 = sapply(box.1, function(x) sub("\\Editorial\\ \\|\\ \\Letter", "Letter | Editorial",x))
box.1 = sapply(box.1, function(x) sub("\\Letter\\ \\|\\ Letter", "Letter",x))
box.1 = sapply(box.1, function(x) sub("\\Letter\\ \\|\\ Journal Article", "Journal Article | Letter",x))
box.1 = sapply(box.1, function(x) sub("\\Review\\ \\|\\ Journal Article", "Journal Article | Review",x))
box.1 = sapply(box.1, function(x) sub("\\News\\ \\|\\ Journal Article", "Journal Article | News",x))
box.1 = sapply(box.1, function(x) sub("\\Comment\\ \\|\\ Journal Article", "Journal Article | Comment",x))
box.1 = sapply(box.1, function(x) sub("\\Comment\\ \\|\\ Letter", "Letter | Comment",x))
box.1 = sapply(box.1, function(x) sub("\\Comment\\ \\|\\ \\Editorial", "Editorial | Comment",x))
box.1 = as.data.frame(table(box.1))
box.1$perc = sapply(box.1$Freq, function(x) (round(x/nrow(pmid_abs)*100,2)))
box.1 =box.1[order(-box.1$Freq),][1:5,]

#box.1.2 = sapply(db[which(db$pmid %in% pmid_no_abs$pmid),"type"], function(x) sub("\\Journal Article\\ \\|\\ Journal Article", "Journal Article",x))
#box.1.2 = sapply(box.1.2, function(x) sub("\\Editorial\\ \\|\\ \\Editorial", "Editorial",x))
#box.1.2 = sapply(box.1.2, function(x) sub("\\Editorial\\ \\|\\ \\Journal\\ \\Article", "Journal Article | Editorial",x))
#box.1.2 = sapply(box.1.2, function(x) sub("\\Editorial\\ \\|\\ Review", "Review | Editorial",x))
#box.1.2 = sapply(box.1.2, function(x) sub("\\Editorial\\ \\|\\ \\Letter", "Letter | Editorial",x))
#box.1.2 = sapply(box.1.2, function(x) sub("\\Letter\\ \\|\\ \\Journal\\ \\Article", "Journal Article | Letter",x))
#box.1.2 = sapply(box.1.2, function(x) sub("\\Letter\\ \\|\\ \\Letter", "Letter",x))
#box.1.2 = sapply(box.1.2, function(x) sub("\\Review\\ \\|\\ \\Journal\\ \\Article", "Journal Article | Review",x))
#box.1.2 = sapply(box.1.2, function(x) sub("\\News\\ \\|\\ \\Journal\\ \\Article", "Journal Article | News",x))
#box.1.2 = sapply(box.1.2, function(x) sub("\\Comment\\ \\|\\ \\Journal\\ \\Article", "Journal Article | Comment",x))
#box.1.2 = sapply(box.1.2, function(x) sub("\\Comment\\ \\|\\ \\Letter", "Letter | Comment",x))
#box.1.2 = sapply(box.1.2, function(x) sub("\\Comment\\ \\|\\ \\Editorial", "Editorial | Comment",x))
#box.1.2 = as.data.frame(table(box.1.2))
#box.1.2$perc = sapply(box.1.2$Freq, function(x) (round(x/nrow(pmid_no_abs)*100,2)))
#box.1.2 =box.1.2[order(-box.1.2$Freq),][1:5,]

#Graph 1
p # Plot with the number of articles by 

#Source of scientific information
top5Affiliation=CountryAffiliation_abs[1:5,]
top5Affiliation$perc = sapply(top5Affiliation$Freq, function(x) (round(x/sum(CountryAffiliation_abs$Freq)*100,2)))

#word tonekization
nrow(wordAll) #Global words  from abstract
wordAll.filtred # Filtred lobal words  from abstract
wordAllTop50[1:50,] # Filtred Top 50 Global words  from abstract
nrow(excluded.words) # Global words exlcuded from abstract

#Categories
flux
d1;lg;g
box.category

#Sentences 
context_sentence = paste0(as.character(length(context[["context"]][["g1"]][["context"]])) ,", ",
                         as.character(length(context[["context"]][["g2"]][["context"]])),", ",
                         as.character(length(context[["context"]][["g3"]][["context"]])),", ",
                         as.character(length(context[["context"]][["g4"]][["context"]]))," and ",
                         as.character(length(context[["context"]][["g5"]][["context"]])))


source("scripts/creat_flux2.R")

flux2

a = wordAll.g.Top50$g1
b = wordAll.g.Top50$g2
c = wordAll.g.Top50$g3
d = wordAll.g.Top50$g4
e = wordAll.g.Top50$g5

WriteXLS::WriteXLS(x = c("wordAll.filtred",
                         "a",
                         "b",
                         "c",
                         "d",
                         "e"), 
                  SheetNames = c("gobal",
                  "diagnosis",
                  "treatment",
                  "epidemiology",
                  "transmission",
                  "signs"),
                   ExcelFileName = "PlatCOVID/source/static/files/word_atomization.xlsx",FreezeRow = 1,AdjWidth = T)

WriteXLS::WriteXLS(x ="excluded.words", SheetNames = "excluded_words",ExcelFileName = "PlatCOVID/source/static/files/excluded_word.xlsx",
                   FreezeRow = 1,AdjWidth = T)
rm(a,b,c,d,e)
save(number_of_article,p,number_abstract,box.1,top5Affiliation, 
     wordAll,wordAllTop50,excluded.words,map1.1,
     flux,d1,lg,g,box.category,
     context_sentence, flux2,
     file = "updates/current/Rdata/syntax.Rdata")




#### Gene panel ####
a = NULL
for(i in 1:length(gene)){
a = rbind(a,nrow(unique(gene[[i]])))
  }

gene_list_new = data.frame(gene_list,a,entreid = gene.view.ALL$ENTREZID, stringsAsFactors = FALSE)
gene_list_new = gene_list_new[-grep("\\IMPAC",x = gene_list_new$Gene_symbol),]
gene_list_new$Gene_symbol = as.character(gene_list_new$Gene_symbol)
for (i in 1:nrow(gene_list_new)){
gene_list_new[i,1] = paste0("<a href='/genepanel/",gene_list_new[i,5],"/info/'>",gene_list_new[i,1],"</a>")
gene_list_new[i,4] = paste0("<a href='/genepanel/",gene_list_new[i,5],"/gene_context/'>",gene_list_new[i,4],"</a>")
}

gene.view.ALL_new = gene.view.ALL[-grep("\\IMPAC",x = gene.view.ALL$name),]
gene_new = gene[-grep("\\IMPAC",x = gene)]

for (i in 1:length(gene_new)){
    for (j in as.character(gene_new[[i]][,1])){
      if(is.na(db.abstract$selected[which(db.abstract$pmid %in% j)])==TRUE || identical(db.abstract$selected[which(db.abstract$pmid %in% j)], numeric(0))==TRUE)  {
        gene_new[[i]][which(as.character(gene_new[[i]][,1]) %in% j),1] = paste0("<a href='https://pubmed.ncbi.nlm.nih.gov/",j,"' target ='_blank'>",j,"</a>")
      } else {
        gene_new[[i]][which(as.character(gene_new[[i]][,1]) %in% j),1] = paste0("<a href='/publication/",j,"' target ='_blank'>",j,"</a>")}
    }
}


save(gene_list_new,
     gene.view.ALL_new,
     gene.conection.ALL,
     gene.expression.ALL,
     gene_new,
  file = "updates/current/Rdata/genePanel.Rdata")


#### Drug panel ####

for (i in 1:nrow(drugs.view)){
  drugs.view[i,10] = paste0("<a href='/drugpanel/",drugs.view[i,1],"/info/'>",drugs.view[i,1],"</a>")
}

drugs.view = drugs.view[order(-drugs.view$`Citation Frequency`),]

for (i in 1:7){drugs.view[,i] = as.character(drugs.view[,i])}

save(drugs,drugs.view, db.abstract,
  file = "updates/current/Rdata/drugPanel.Rdata")

#### Tissue and Cell panel ####

for (i in 1:nrow(tissue_list)){
  tissue_list[i,3] = paste0("<a href='/tissuepanel/",sub(" ","_",tissue_list[i,1]),"/info/'>",tissue_list[i,1],"</a>")
}


for (i in 1:nrow(cell_list)){
  cell_list[i,3] = paste0("<a href='/cpanel/",sub(" ","_",cell_list[i,1]),".html'>",cell_list[i,1],"</a>")
}

tissue.db$pmid = as.character(tissue.db$pmid)

for (i in 1:nrow(tissue.db)){
  for (j in as.character(tissue.db[i,"pmid"])){
    if(is.na(db.abstract$selected[which(db.abstract$pmid %in% j)])==TRUE || is.null(db.abstract$selected[which(db.abstract$pmid %in% j)])==TRUE)  {
      tissue.db[i,"pmid"] = paste0("<a href='https://pubmed.ncbi.nlm.nih.gov/",j,"' target ='_blank'>",j,"</a>")
    } else {
      tissue.db[i,"pmid"]  = paste0("<a href='/publication/",j,"' target ='_blank'>",j,"</a>")}
  }
}

cells.db$pmid = as.character(cells.db$pmid)

for (i in 1:nrow(cells.db)){
  for (j in as.character(cells.db[i,"pmid"])){
    if(is.na(db.abstract$selected[which(db.abstract$pmid %in% j)])==TRUE || is.null(db.abstract$selected[which(db.abstract$pmid %in% j)])==TRUE)  {
      cells.db[i,"pmid"] = paste0("<a href='https://pubmed.ncbi.nlm.nih.gov/",j,"' target ='_blank'>",j,"</a>")
    } else {
      cells.db[i,"pmid"]  = paste0("<a href='/publication/",j,"' target ='_blank'>",j,"</a>")}
  }
}

save(tissue.db,tissue_list,
     cells.db,cell_list,
     file = "updates/current/Rdata/tissuePanel.Rdata"
)


### Supp info ####

save(box.2,box.3,box.4,
     file = "updates/current/Rdata/supp.Rdata"
)

save.image("platcovid_28jun20_Run7.RData")

rm(a,a1,a2,a3,a4,a5,i,j,d1,b,cd,g,lg,fix.geneID,gene.view.fun)

save.image("updates/current/Rdata/final.RData")
