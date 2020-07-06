#pmid_select = gdata::read.xls(xls = "updates/current/export/pmids_por_termos.xlsx",sheet = "1")
pmid_select = gdata::read.xls(xls = "updates/17abril2020//export/pmids_por_termos.xlsx",sheet = "1")
#length(pmid_select$diagnosis)

a1 = merge(conclusion_df$g1,context$context$g1)
a2 = merge(conclusion_df$g2,context$context$g2)
a3 = merge(conclusion_df$g3,context$context$g4)
a4 = merge(conclusion_df$g4,context$context$g5)
a5 = merge(conclusion_df$g5,context$context$g5)


flux2 = DiagrammeR::grViz(paste0("digraph flowchart {

             node [fontname = Helvetica, shape = 'box', fillcolor = 'Gold', style = 'filled', color = 'Honeydew']        
             g1 [href= '../tables/abstract/diagnose', target = '_blank', label = '",length(category$abstract$g1@PMID),"\\n Diagnosis']
             g1_sel [label = '",length(na.omit(unique(a1$pmid))),"\\n Selected']
             g1_cur [href= '../category/diagnosis/',label = '",nrow(na.omit(unique(subset(pmid_select, !deletar=="x", select = "diagnosis")))),"\\n Curated']
             g1_to_cur [href= '../category/diagnosis_to_curate/',label = '",length(na.omit(unique(a1$pmid)))-nrow(unique(subset(pmid_select, !deletar=="x", select = "diagnosis"))),"\\n Curating']
             
             node [fontname = Helvetica, shape = 'box', fillcolor = 'MediumOrchid', style = 'filled', color = 'Honeydew']        
             g2 [href= '../tables/abstract/treatment', target = '_blank', label = '",length(category$abstract$g2@PMID),"\\n Treatment']
             g2_sel [label = '",length(na.omit(unique(a2$pmid))),"\\n Selected']
             g2_cur [href= '../category/treatment/',label = '",nrow(unique(subset(pmid_select, !deletar.4=="x", select = "treatment"))),"\\n Curated']
             g2_to_cur [href= '../category/treatment_to_curate/',label = '",length(na.omit(unique(a2$pmid)))-nrow(unique(subset(pmid_select, !deletar.4=="x", select = "treatment"))),"\\n Curating']
             
             node [fontname = Helvetica, shape = 'box', fillcolor = 'PaleGreen', style = 'filled', color = 'Honeydew']        
             g3 [href= '../tables/abstract/epidemiology', target = '_blank', label = '",length(category$abstract$g3@PMID),"\\n Epidemiology']
             g3_sel [label = '",length(na.omit(unique(a3$pmid))),"\\n Selected']
             g3_cur [href= '../category/epidemiology/',label = '",nrow(unique(subset(pmid_select, !deletar.1=="x", select = "epidemiology"))),"\\n Curated']
             g3_to_cur [href= '../category/epidemiology_to_curate/',label = '",length(na.omit(unique(a3$pmid)))-nrow(unique(subset(pmid_select, !deletar.1=="x", select = "epidemiology"))),"\\n Curating']
             
             node [fontname = Helvetica, shape = 'box', fillcolor = 'RoyalBlue', style = 'filled', color = 'Honeydew']        
             g4 [href= '../tables/abstract/transmission', target = '_blank', label = '",length(category$abstract$g4@PMID),"\\n Transmission']
             g4_sel [label = '",length(na.omit(unique(a4$pmid))),"\\n Selected']
             g4_cur [href= '../category/transmission/',label = '",nrow(unique(subset(pmid_select, !deletar.2=="x", select = "transmission"))),"\\n Curated']
             g4_to_cur [href= '../category/transmission_to_curate/',label = '",length(na.omit(unique(a4$pmid)))-nrow(unique(subset(pmid_select, !deletar.2=="x", select = "transmission"))),"\\n Curating']
             
             node [fontname = Helvetica, shape = 'box', fillcolor = 'Orange', style = 'filled', color = 'Honeydew']        
             g5 [href= '../tables/abstract/signs', target = '_blank', label = '",length(category$abstract$g5@PMID),"\\n Symptoms']
             g5_sel [label = '",length(na.omit(unique(a5$pmid))),"\\n Selected']
             g5_cur [href= '../category/signs/',label = '",nrow(unique(subset(pmid_select, !deletar.3=="x", select = "symptoms"))),"\\n Curated']
             g5_to_cur [href= '../category/signs_to_curate/',label = '",length(na.omit(unique(a5$pmid)))-nrow(unique(subset(pmid_select, !deletar.3=="x", select = "symptoms"))),"\\n Curating']
             
             
             # edge definitions with the node IDs
             g1 -> g1_sel g1_sel ->  g1_to_cur -> g1_cur
             g2 -> g2_sel g2_sel ->  g2_to_cur -> g2_cur
             g3 -> g3_sel g3_sel ->  g3_to_cur -> g3_cur
             g4 -> g4_sel g4_sel ->  g4_to_cur -> g4_cur
             g5 -> g5_sel g5_sel ->  g5_to_cur -> g5_cur
             }
             "))
