options(shiny.maxRequestSize=30*1024^2)
##### Server #####
server = function(input, output, session){
  
#####  Function setting ##### 
  
  ## Call function
  # Put R files in the same folder
  filePath <- ""
  getFilePath <- function(fileName) {
    path <- setwd(getwd())   # path <- setwd("~") # Absolute path of project folder
    filePath <<- paste0(path ,"/" , fileName)    # Combine strings without gaps  
    sourceObj <- source(filePath)  #? Assigning values to global variable
    return(sourceObj)
  }
  
  getFilePath("FUN_XML_to_df.R") # Load file
  getFilePath("FUN_JASON_to_df.R") # Load file
  
##### Main reactive #####
  df_reactive_XML = reactive({
    #XML.df <- XML_to_df(input$file1$datapath,input$word_select)
    Output_Sum <- XML_to_df(input$file1$datapath)
    XML.df <- Output_Sum[["XML.df"]]
  })
  
  df_reactive_JASON = reactive({
    #JASON.df <- JASON_to_df(input$file2$datapath,input$word_select)
    Output_Sum <- JASON_to_df(input$file2$datapath)
    JASON.df <- Output_Sum[["JASON.df"]]
  })
  
  
  Keyword_reactive_Ori_XML = reactive({
    Output_Sum <- XML_to_df(input$file1$datapath)
    Abs.All_df.Word.C <- Output_Sum[["Abs.All_df.Word.C"]]
    ##
    # Dynamic Programming (by Biostrings pairwiseAlignment) Loop
    Keyword = tolower(input$word_select)
    
    Abs.All_df.Word.C.Score <- Abs.All_df.Word.C
    for (x in 1:length(Abs.All_df.Word.C$word)) {
      if (abs(nchar(as.character(Abs.All_df.Word.C$word[x]))-nchar(Keyword))<=1) {
        DP <- pairwiseAlignment(Keyword, as.character(Abs.All_df.Word.C$word[x]),scoreOnly=TRUE)
        Abs.All_df.Word.C.Score[x,3] <- DP
      }else
        Abs.All_df.Word.C.Score[x,3] <- -999 
    }
    
    colnames(Abs.All_df.Word.C.Score)[3] <- c("Score")
    Abs.All_df.Word.C.Score <- Abs.All_df.Word.C.Score[order(Abs.All_df.Word.C.Score$Score,decreasing = TRUE),]
    NewKeyword_Ori <- as.character(Abs.All_df.Word.C.Score$word[1])
    
    # ## Dynamic Programming (by Biostrings pairwiseAlignment) sapply Save time slitly
    # # https://www.biostars.org/p/15688/
    # # TTT <- sapply(as.character(Abstract.All_df.Word.C$word), function(x) pairwiseAlignment(toupper(x), Keyword, scoreOnly=TRUE)) 
    # Abstract.All_df.Word.C.Score <- Abstract.All_df.Word.C
    # DPScore <- sapply(as.character(Abstract.All_df.Word.C$word), function(x) pairwiseAlignment(tolower(x), tolower(Keyword), scoreOnly=TRUE)) 
    # Abstract.All_df.Word.C.Score["Score"] <- as.numeric(as.character(DPScore))
    # Abstract.All_df.Word.C.Score <- Abstract.All_df.Word.C.Score[order(Abstract.All_df.Word.C.Score$Score,decreasing = TRUE),]
    # NewKeyword <- as.character(Abstract.All_df.Word.C.Score$word[1])
    
  })


  Keyword_reactive_Stem_XML = reactive({
    Output_Sum <- XML_to_df(input$file1$datapath)
    Abs.All_df.Word.Stem.C <- Output_Sum[["Abs.All_df.Word.Stem.C"]]
    ##
    # Dynamic Programming (by Biostrings pairwiseAlignment) Loop
    Keyword = tolower(input$word_select)
    #Keyword = TryKeyWord()
    Abs.All_df.Word.Stem.C.Score <- Abs.All_df.Word.Stem.C
    for (x in 1:length(Abs.All_df.Word.Stem.C$stem)) {
    if (abs(nchar(as.character(Abs.All_df.Word.Stem.C$stem[x]))-nchar(Keyword))<=1) {
      DP <- pairwiseAlignment(Keyword, as.character(Abs.All_df.Word.Stem.C$stem[x]),scoreOnly=TRUE)
      Abs.All_df.Word.Stem.C.Score[x,3] <- DP
    }else
      Abs.All_df.Word.Stem.C.Score[x,3] <- -999 
    }
  
    colnames(Abs.All_df.Word.Stem.C.Score)[3] <- c("Score")
    Abs.All_df.Word.Stem.C.Score <- Abs.All_df.Word.Stem.C.Score[order(Abs.All_df.Word.Stem.C.Score$Score,decreasing = TRUE),]
    NewKeyword_Stem <- as.character(Abs.All_df.Word.Stem.C.Score$stem[1])
  })
  Keyword_reactive_Ori_JASON = reactive({
    Output_Sum <- JASON_to_df(input$file2$datapath)
    Abs.All_df.Word.C <- Output_Sum[["Abs.All_df.Word.C"]]
    ##
    # Dynamic Programming (by Biostrings pairwiseAlignment) Loop
    Keyword = tolower(input$word_select)
    
    Abs.All_df.Word.C.Score <- Abs.All_df.Word.C
    for (x in 1:length(Abs.All_df.Word.C$word)) {
      if (abs(nchar(as.character(Abs.All_df.Word.C$word[x]))-nchar(Keyword))<=1) {
        DP <- pairwiseAlignment(Keyword, as.character(Abs.All_df.Word.C$word[x]),scoreOnly=TRUE)
        Abs.All_df.Word.C.Score[x,3] <- DP
      }else
        Abs.All_df.Word.C.Score[x,3] <- -999 
    }
    
    colnames(Abs.All_df.Word.C.Score)[3] <- c("Score")
    Abs.All_df.Word.C.Score <- Abs.All_df.Word.C.Score[order(Abs.All_df.Word.C.Score$Score,decreasing = TRUE),]
    NewKeyword <- as.character(Abs.All_df.Word.C.Score$word[1])
    
  })
  
  Keyword_reactive_Stem_JASON = reactive({
    Output_Sum <- JASON_to_df(input$file2$datapath)
    Abs.All_df.Word.Stem.C <- Output_Sum[["Abs.All_df.Word.Stem.C"]]
    ##
    # Dynamic Programming (by Biostrings pairwiseAlignment) Loop
    Keyword = tolower(input$word_select)
    Abs.All_df.Word.Stem.C.Score <- Abs.All_df.Word.Stem.C
    for (x in 1:length(Abs.All_df.Word.Stem.C$stem)) {
      if (abs(nchar(as.character(Abs.All_df.Word.Stem.C$stem[x]))-nchar(Keyword))<=1) {
        DP <- pairwiseAlignment(Keyword, as.character(Abs.All_df.Word.Stem.C$stem[x]),scoreOnly=TRUE)
        Abs.All_df.Word.Stem.C.Score[x,3] <- DP
      }else
        Abs.All_df.Word.Stem.C.Score[x,3] <- -999 
    }
    
    colnames(Abs.All_df.Word.Stem.C.Score)[3] <- c("Score")
    Abs.All_df.Word.Stem.C.Score <- Abs.All_df.Word.Stem.C.Score[order(Abs.All_df.Word.Stem.C.Score$Score,decreasing = TRUE),]
    NewKeyword_Stem <- as.character(Abs.All_df.Word.Stem.C.Score$stem[1])
  })

  TryKeyWord  = eventReactive(c(input$SearchKW,input$file1$datapath,input$file2$datapath), {
    if (input$word_select==""){ButKeyword =NULL
    }else{
    ButKeyword = tolower(input$word_select)
    }
    })
  OriKeyWord  =  reactive({
      ButKeyword = tolower(input$word_select)
  })
  
  
##### summary graph #####  
  output$HisFig <- renderPlot({
    if (length(input$file1)>0 && length(input$file2)==0){
      ## Bar plot for XML
      Output_Sum <- XML_to_df(input$file1$datapath)
      Abs.All_df.Word.C <- Output_Sum[["Abs.All_df.Word.C"]]
      Abs.All_df.Word.Stem.C <- Output_Sum[["Abs.All_df.Word.Stem.C"]]
      Abs.All_df.Word.Stem.RmSW.C <- Output_Sum[["Abs.All_df.Word.Stem.RmSW.C"]]
      
      TryKey <- TryKeyWord()
      OriKey <- OriKeyWord()
      
      NewKeyword_Ori <- Keyword_reactive_Ori_XML()
      NewKeyword_Stem <- Keyword_reactive_Stem_XML()
      

      p1 <- ggplot(data = Abs.All_df.Word.C, aes(x = word, y = n)) +
        geom_bar(stat="identity", fill="#f5e6e8", colour="#f5e6e8") +
        #geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        xlab("Rank order of terms") + ylab("Frequency")+
        theme(axis.text.x = element_blank()) + #theme(axis.text.x = element_text(angle=90, hjust=1)) +
        theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold"))+
        ggtitle("Original")+ theme(
        plot.title = element_text(color="black", size=14, face="bold.italic"))
      
      p2 <- ggplot(data = Abs.All_df.Word.Stem.C, aes(x = stem, y = n)) +
        geom_bar(stat="identity", fill="#d5c6e0", colour="#d5c6e0")+
        #geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        xlab("Rank order of terms") + ylab("Frequency")+
        theme(axis.text.x = element_blank()) +
        theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold"))+
        ggtitle("Porter")+ theme(
        plot.title = element_text(color="black", size=14, face="bold.italic"))

      
      p3 <- ggplot(data = Abs.All_df.Word.Stem.RmSW.C, aes(x = stem, y = n)) +
        geom_bar(stat="identity", fill="#aaa1c8", colour="#aaa1c8")+
        #geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        xlab("Rank order of terms") + ylab("Frequency")+
        theme(axis.text.x = element_blank()) +
        theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold"))+
        ggtitle("Porter+RmSW")+ theme(
          plot.title = element_text(color="black", size=14, face="bold.italic"))
      
      
      p4 <- ggplot(df_reactive_XML(), aes(x=df_reactive_XML()[,2], y=df_reactive_XML()[,9])) + geom_bar(stat="identity")+
        geom_bar(stat="identity", fill="#967aa1", colour="black")+
        xlab("PMID") + ylab("Number of Search Words")+ 
        theme(axis.text.x = element_text(angle=90, hjust=1)) +
        theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold")) 
      if (is.null(TryKey)){
        grid.arrange(p1, p2 ,p3 , nrow = 1)
      }else if (TryKey != OriKey){
        grid.arrange(p1, p2 ,p3 , nrow = 1)
      }else{
        p1_2 <- p1+
          geom_vline(xintercept = NewKeyword_Ori,color="#d90d6c", size=1, alpha = 0.8)+ # http://www.sthda.com/english/wiki/ggplot2-add-straight-lines-to-a-plot-horizontal-vertical-and-regression-lines
          annotate(geom = "text", x = NewKeyword_Ori, y = Abs.All_df.Word.C[Abs.All_df.Word.C$word %in% NewKeyword_Ori,2]+1, 
                   label = paste0(NewKeyword_Ori,",",Abs.All_df.Word.C[Abs.All_df.Word.C$word %in% NewKeyword_Ori,2]), hjust = "left",size=6)
        
        p2_2 <- p2+
          geom_vline(xintercept = NewKeyword_Stem,color="#d90d6c", size=1, alpha = 0.8)+ 
          annotate(geom = "text", x = NewKeyword_Stem, y = Abs.All_df.Word.Stem.C[Abs.All_df.Word.Stem.C$stem %in% NewKeyword_Stem,2]+1, 
                   label = paste0(NewKeyword_Stem,",",Abs.All_df.Word.Stem.C[Abs.All_df.Word.Stem.C$stem %in% NewKeyword_Stem,2]), hjust = "left",size=6)
        p3_2 <- p3+
          geom_vline(xintercept = NewKeyword_Stem,color="#d90d6c", size=1, alpha = 0.8)+ 
          annotate(geom = "text", x = NewKeyword_Stem, y = Abs.All_df.Word.Stem.RmSW.C[Abs.All_df.Word.Stem.RmSW.C$stem %in% NewKeyword_Stem,2]+1, 
                   label = paste0(NewKeyword_Stem,",",Abs.All_df.Word.Stem.RmSW.C[Abs.All_df.Word.Stem.RmSW.C$stem %in% NewKeyword_Stem,2]), hjust = "left",size=6)
        grid.arrange(p1_2, p2_2 ,p3_2 , nrow = 1)
      }
      
    }else if(length(input$file1)==0 && length(input$file2)>0){
      ## Bar plot for JASON
      
      Output_Sum <- JASON_to_df(input$file2$datapath)
      Abs.All_df.Word.C <- Output_Sum[["Abs.All_df.Word.C"]]
      Abs.All_df.Word.Stem.C <- Output_Sum[["Abs.All_df.Word.Stem.C"]]
      Abs.All_df.Word.Stem.RmSW.C <- Output_Sum[["Abs.All_df.Word.Stem.RmSW.C"]]
      
      TryKey <- TryKeyWord()
      OriKey <- OriKeyWord()
      
      NewKeyword_Ori <- Keyword_reactive_Ori_JASON()
      NewKeyword_Stem <- Keyword_reactive_Stem_JASON()  
      p5 <- ggplot(data = Abs.All_df.Word.C, aes(x = word, y = n)) +
        geom_bar(stat="identity", fill="#f5e6e8", colour="#f5e6e8") +
        #geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        xlab("Rank order of terms") + ylab("Frequency")+
        theme(axis.text.x = element_blank()) + #theme(axis.text.x = element_text(angle=90, hjust=1)) +
        theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold"))+
        ggtitle("Original")+ theme(
          plot.title = element_text(color="black", size=14, face="bold.italic"))
      
      
      p6 <- ggplot(data = Abs.All_df.Word.Stem.C, aes(x = stem, y = n)) +
        geom_bar(stat="identity", fill="#e9d8a6", colour="#e9d8a6")+
        #geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        xlab("Rank order of terms") + ylab("Frequency")+
        theme(axis.text.x = element_blank()) +
        theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold"))+
        ggtitle("Porter")+ theme(
          plot.title = element_text(color="black", size=14, face="bold.italic"))
      
      
      p7 <- ggplot(data = Abs.All_df.Word.Stem.RmSW.C, aes(x = stem, y = n)) +
        geom_bar(stat="identity", fill="#94d2bd", colour="#94d2bd")+
        #geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+

        xlab("Rank order of terms") + ylab("Frequency")+
        theme(axis.text.x = element_blank()) +
        theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold"))+
        ggtitle("Porter+RmSW")+ theme(
          plot.title = element_text(color="black", size=14, face="bold.italic"))
      
      
      p8 <- ggplot(df_reactive_JASON(), aes(x=df_reactive_JASON()[,1], y=df_reactive_JASON()[,18])) + geom_bar(stat="identity")+
        geom_bar(stat="identity", fill="#0a9396", colour="black")+
        xlab("User name") + ylab("Number of Search stems")+ 
        theme(axis.text.x = element_text(angle=90, hjust=1)) +
        theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold")) 
      
      if (is.null(TryKey)){
        grid.arrange(p5, p6 ,p7 , nrow = 1)
      }else if (TryKey != OriKey){
        grid.arrange(p5, p6 ,p7 , nrow = 1)
      }else{
        p5_2 <- p5 +
          geom_vline(xintercept = NewKeyword_Ori,color="#d90d6c", size=1, alpha = 0.8)+ # http://www.sthda.com/english/wiki/ggplot2-add-straight-lines-to-a-plot-horizontal-vertical-and-regression-lines
          annotate(geom = "text", x = NewKeyword_Ori, y = Abs.All_df.Word.C[Abs.All_df.Word.C$word %in% NewKeyword_Ori,2]+1, 
                   label = paste0(NewKeyword_Ori,",",Abs.All_df.Word.C[Abs.All_df.Word.C$word %in% NewKeyword_Ori,2]), hjust = "left",size=6)
        p6_2 <- p6 +
          geom_vline(xintercept = NewKeyword_Stem,color="#d90d6c", size=1, alpha = 0.8)+ 
          annotate(geom = "text", x = NewKeyword_Stem, y = Abs.All_df.Word.Stem.C[Abs.All_df.Word.Stem.C$stem %in% NewKeyword_Stem,2]+1, 
                   label = paste0(NewKeyword_Stem,",",Abs.All_df.Word.Stem.C[Abs.All_df.Word.Stem.C$stem %in% NewKeyword_Stem,2]), hjust = "left",size=6)
        
        p7_2 <- p7 +
        geom_vline(xintercept = NewKeyword_Stem,color="#d90d6c", size=1, alpha = 0.8)+ 
          annotate(geom = "text", x = NewKeyword_Stem, y = Abs.All_df.Word.Stem.RmSW.C[Abs.All_df.Word.Stem.RmSW.C$stem %in% NewKeyword_Stem,2]+1, 
                   label = paste0(NewKeyword_Stem,",",Abs.All_df.Word.Stem.RmSW.C[Abs.All_df.Word.Stem.RmSW.C$stem %in% NewKeyword_Stem,2]), hjust = "left",size=6)
        
        grid.arrange(p5_2, p6_2 ,p7_2 ,p8 , nrow = 1)
        
      
      }

    }else{
      p1 <- ggplot()
      p2 <- ggplot()
      p3 <- ggplot()
      p4 <- ggplot()
      grid.arrange(p1, p2 ,p3,p4 , nrow = 1)
    }

  })
  
##### Summary table #####  
  output$SumTable <- renderTable({
    if (length(input$file1)>0 && length(input$file2)==0){
      df_reactive_XML()[,c(1:3,6:9)]
    }else if(length(input$file1)==0 && length(input$file2)>0){
      df_reactive_JASON()[,c(19,1,5,15,16,17,18)]
    }else{
      XML.df0 <- data.frame(matrix(nrow = 0,ncol = 10))
      colnames(XML.df0) <- c("NO.","ID","Time","Text","CHAR","WORD","SENT","Search Word","FileNo","LitNo")
      XML.df0
    }
    
  },digits=0)  

  
##### Searching the Keywords #####
  # Reference # https://newbedev.com/highlight-word-in-dt-in-shiny-based-on-regex
  df_reactive_HL = reactive({
    if(length(input$file1)>0 && length(input$file2)==0){
      # XML.df.Hl <- XML_to_df(input$file1$datapath,NewKeyword)
      Output_Sum <- XML_to_df(input$file1$datapath)
      XML.df.Hl <- Output_Sum[["XML.df"]]
      NewKeyword <- Keyword_reactive_Ori_XML()
      XML.df.Hl[,c(2,4,5)] %>%
        # Filter if input is anywhere, even in other words.
        filter_all(any_vars(grepl(NewKeyword, ., T, T))) %>% 
        # Replace complete words with same in XML.
        mutate_all(~ gsub(
          paste(c("\\b(", NewKeyword, ")\\b"), collapse = ""),
          "<span style='background-color:#d0d1ff;color:#7251b5;font-family: Calibra, Arial Black;'>\\1</span>", # font-family: Lobster, cursive
          ., TRUE, TRUE ))
      
    }else if(length(input$file1)==0 && length(input$file2)>0){
      Output_Sum <- JASON_to_df(input$file2$datapath)
      JASON.df <- Output_Sum[["JASON.df"]]
      NewKeyword <- Keyword_reactive_Ori_JASON()
      JASON.df[,c(1,2,4,5,6)] %>%
        # Filter if input is anywhere, even in other words.
        filter_all(any_vars(grepl(NewKeyword, ., T, T))) %>% 
        # Replace complete words with same in HTML.
        mutate_all(~ gsub(
          paste(c("\\b(", NewKeyword, ")\\b"), collapse = ""),
          "<span style='background-color:#e9d8a6;color:#005f73;font-family: Calibra, Arial Black;'>\\1</span>",
          ., TRUE, TRUE ))
      
    }else{
      XML.df0 <- data.frame(matrix(nrow = 0,ncol = 3))
      colnames(XML.df0) <- c("PMID","Title","Abstract")
      XML.df0
    }
  })

  output$table <- renderDataTable({
    datatable(df_reactive_HL(), escape = F, options = list(searchHighlight = TRUE,dom = "lt"))
  })

}
