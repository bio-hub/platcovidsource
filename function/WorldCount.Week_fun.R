# Week Country count ---
Week.contries.count = function(df_name = "Output df name",abs_date_name = "vector name of the list created by abs.date_fun()"){
    for(j in names(abs_date)){
    assign(paste0("wordWeek_",j), eval(parse(text = paste0("word_atomizations(",abs_date_name,"$",j,")"))))
    
    #Limpeza 1 geral e add os EUA, Reino Unido e Arabia
    a = gsub(pattern = "\\(|\\)",replacement = "", x = get(paste0("wordWeek_",j))$words)
    a = gsub(pattern =paste0(0:10,collapse = "|"),replacement = "", x = a)
    a = gsub(pattern ="/new",replacement = "", x = a)
    a = gsub(pattern ="united",replacement = "United States", x = a)
    a = gsub(pattern ="Kingdom",replacement = "United Kingdom", x = a)
    a = gsub(pattern ="arabia",replacement = "Saudi Arabia", x = a)
    a = gsub(pattern ="sars-cov-/|\\//|\\[]",replacement = "", x = a)
    assign(paste0("wordWeek_",j,".1"),  data.frame(word = a, Freq = get(paste0("wordWeek_",j))$Freq))
    
    #Grep nome dos paises
    df.1 = get(paste0("wordWeek_",j,".1"))
    a.1 = grep(pattern = CountrWord, x = df.1[,1],value = T,ignore.case = T,invert = F)
    assign(paste0("wordWeek_",j,".1"), as.data.frame(df.1[which(df.1[,1] %in% a.1),]))

        if(length(paste0("wordWeek_",j,".1"))>=2 ){
      eval(parse(text = paste0("wordWeek_",j,".1[,1]=as.character(wordWeek_",j,".1[,1])")))
      eval(parse(text = paste0("wordWeek_",j,".1[,2]=as.numeric(wordWeek_",j,".1[,2])")))
    } else {
      eval(parse(text = paste0("wordWeek_",j,".1[,1]=as.character(wordWeek_",j,".1[,1])")))
    }
    
    assign(paste0("WorldWeekCount",j), NULL)
    for(i in unique(get(paste0("wordWeek_",j,".1"))[,1])){
      assign(paste0("WorldWeekCount",j), 
        rbind(get(paste0("WorldWeekCount",j)),
              data.frame(word = i, Freq =sum(as.numeric(get(paste0("wordWeek_",j,".1"))[grep(pattern = i, value = F,get(paste0("wordWeek_",j,".1"))[,1]),2]))))
      )
      
    }
}
    eval(parse(text = paste0(df_name,"<<- Reduce(function(x, y) merge(x, y, by='word', all=TRUE),list(",paste0(names(get(abs_date_name)),'=WorldWeekCount',names(get(abs_date_name)),collapse = ','),"))"))) #verificar se precisa do get    
    eval(parse(text = paste0("colnames(",df_name,")<<-c('country','",paste0(names(get(abs_date_name)),collapse = "','"),"')")))
   } 