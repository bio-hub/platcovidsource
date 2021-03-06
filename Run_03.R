### ### ### ### ### ##
###### 1 Plot ########
### ### ### ### ### ##
#filter dates
a = country.date.pmid[,c("pmid","date")] %>% filter(between(date, as.Date("2020-02-01"), Sys.Date()))
#ploty
library(plotly)
p <- plot_ly() %>%
  add_histogram(x=a$date, 
                #type = "histogram", 
                name = "Cumulative\nPublications",
                cumulative = list(enabled=T), 
                marker = list(color = "lightgreen"), 
                opacity = 0.8) %>%
  
  add_histogram(x = a$date,
                name = "Daylly\nPublication", 
                yaxis = "y2",
                marker = list(color = "red"), 
                opacity = 0.2) %>%
  
  layout(barmode = "overlay",
         bargap = 0.1,
         xaxis = list(title=element_blank(),
                      tickangle = 90,
                      tickmode = "linear",
                      range=c(Sys.Date()-9,Sys.Date()-2)),
         yaxis = list(title = "Cumulative", 
                      color = "green",
                      tickfont = list(color = "green")),
         yaxis2 = list(title = "Dally",
                       color = "red",
                       tickfont = list(color = "red"),
                       overlaying = "y",
                       side = "right")
  )


### ### ### ### ### ##
###### 2 Map ########
### ### ### ### ### ##
setwd("updates/current/")
b = spData::world
library(tmap)
### ### ### ### ### ### ### ### ### ### ### ### ##
### ### ### Map 1 for Overall Affliation ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ##
nCountryAffiliation = apply(CountryAffiliation,  2, function(x) gsub("\\bRussia\\b", "Russian Federation", x))
nCountryAffiliation = apply(nCountryAffiliation, 2, function(x) gsub("\\bSouth Korea\\b", "Republic of Korea", x))
nCountryAffiliation = apply(nCountryAffiliation, 2, function(x) gsub("\\bBrunei\\b", "Brunei Darussalam", x))
nCountryAffiliation = apply(nCountryAffiliation, 2, function(x) gsub("\\bGambia\\b", "The Gambia", x))
nCountryAffiliation = as.data.frame(nCountryAffiliation)
nCountryAffiliation$Freq = as.character(nCountryAffiliation$Freq)
nCountryAffiliation$Freq = as.numeric(nCountryAffiliation$Freq)

c = merge(b, nCountryAffiliation, by.x = "name_long", by.y='country', all.x = T, all.y = T)

range = c(1,40,125,310,624,max(c$Freq,na.rm = T))
map1 = tm_shape(c) + tm_layout(legend.text.size = 0.5, legend.title.size = 0.6, legend.position = c(0.01,0.2)) +
  tm_fill(col='Freq',title = "Publications",style = 'cont', breaks =range, palette = 'YlOrBr', colorNA = "white") + tm_borders()
tmap_save(map1, 'export/map1_affiliation.png',  width=1920, height=1080, asp=0)

tmap_save(map1, 'export/paper/map1_affiliation.tiff',  width=1920, height=1080, asp=0,dpi = 300) 

### ### ### ### ### ### ### 
###### 3 Fluxograma #######
### ### ### ### ### ### ##

df = data.frame(pmid =abstracts@PMID, abs = abstracts@Abstract)

pmid_abs = df[grep(pattern = "No Abstract Found",x = df$abs,value = F,invert = T),]
  db.abstract = db[db$pmid %in% pmid_abs$pmid, c("pmid","doi","title","jabbrv","category")]

pmid_no_abs = df[grep(pattern = "No Abstract Found",x = df$abs,value = F,invert = F),]
  db.no_abstract = db[db$pmid %in% pmid_no_abs$pmid, c("pmid","doi","title","jabbrv")]

pmid_category     = eval(parse(text = paste0("unique(c(",paste0("category$abstract$g",1:5,"@PMID",collapse = ","),"))"))) 

pmid_not_category = db.abstract[-which(db.abstract$pmid %in% pmid_category),c("pmid")]


flux = DiagrammeR::grViz(paste0("digraph flowchart {

             node [fontname = Helvetica, shape = 'box', fillcolor = 'PaleGreen', style = 'filled', color = 'Honeydew']        
             Total [label = '",nrow(df)," \\n (Total)']
             Abstract [href= '../tables', target = '_blank', label = '",length(pmid_abs$pmid),"\\n (Abstract)']
             Category [label = '",length(pmid_category_new)+length(pmid_category_old),"\\n (Category)']
             g1 [href= '../tables/abstract/diagnose', target = '_blank', label = '",length(category$abstract$g1@PMID),"\\n Diagnosis \\n (Term 1)']
             g2 [href= '../tables/abstract/treatment',target = '_blank',label = '",length(category$abstract$g2@PMID),"\\n Treatment\\n (Term 2)']
             g3 [href= '../tables/abstract/epidemiology',target = '_blank',label = '",length(category$abstract$g3@PMID),"\\n Epidemiology \\n (Term 3)']
             g4 [href= '../tables/abstract/transmission',target = '_blank',label = '",length(category$abstract$g4@PMID),"\\n Transmission \\n(Term 4)']
             g5 [href= '../tables/abstract/signs',target = '_blank',label = '",length(category$abstract$g5@PMID),"\\n Clinical, Signs and Symptoms \\n(Term 5)']
             
             node [fontname = Helvetica, shape = 'record', fillcolor = 'Azure', color= 'grey']       
             No_abst [href= '../tables/abstract/no_abstract/',target = '_blank', label = '",length(pmid_no_abs$pmid),"\\n (No Abstract)']
             not_cat [href= '../tables/abstract/no_category',target = '_blank', label = '",length(pmid_not_category),"\\n (No Category)']


             # edge definitions with the node IDs
             Total -> {Abstract No_abst}
             Abstract -> {Category not_cat}
             Category -> {g1 g2 g3 g4 g5}
             
             }
             "))

### ### ### ### ### # 
###### 4 Venn #######
### ### ### ### ### #
x =eval(parse(text = paste0("list(",paste0("g",1:5,"= category$abstract$g",1:5,"@PMID", collapse = ","),")")))
library(grid)
color=c('yellow', 'purple', 'green','blue','orange','gray','red','brown','cyan','magenta')
d=VennDiagram::venn.diagram(x,
               category.names = c("","","","",""),
               filename = NULL,
               fill = color[1:5])

d1 = gplots::venn(x,show.plot = F)

lg = legendGrob(labels=c("Diagnosis","Treatment","Epidemiology","Transmission","Clinical, Signs & \nSymptons"),
                pch=rep(19,length(c("Diagnosis","Treatment","Epidemiology","Transmission","Clinical, Signs & Symptons"))),
                gp=gpar(col=color, fill="black"),
                byrow=TRUE)
g =  gTree(children = gList(d))

gridExtra::grid.arrange(g, lg, ncol = 2, widths = c(4,1))

png(filename = "export/Figure_1_category_venn.png",height=500,width=600)
  gridExtra::grid.arrange(g, lg, ncol = 2, widths = c(4,1))
dev.off()


tiff(filename = "export/paper/Figure_1_category_venn.tiff",height=5,width=6, unit = "in", res = 300)
  gridExtra::grid.arrange(g, lg, ncol = 2, widths = c(4,1))
dev.off()


rm(x,color)

setwd('../../')
