library(shiny)
library("shinythemes")


#Loading A different Data-Set
#data1 <- read.csv("Infinite_warfare_comments_YouTube_Clean.csv", stringsAsFactors = FALSE)
#print(str(data1))  #to check if data load is error free
#data1 <- data1$commentText


shinyServer(
  function(input,output)
  {
    
    matadj <- reactive({
      tdm <-TermDocumentMatrix(r_stats_text_corpus, control = list(wordLenghts = c(1, Inf)))
      idx <-which(dimnames(tdm)$Terms == "call") ##change the terms to be searched
      tdm2 <- removeSparseTerms(tdm, sparse = input$sparse)
      m2 <- as.matrix(tdm2)
      m2[m2 >= 1] <- 1
      m2 <- m2 %*% t(m2) ##Adjaceny Matrix - how often words co-occur in a sentence
      m2
    })
    
    fit1 <- reactive({
      fit1 <- hclust(dist(matadj()))
    })
    
    fmrlayout <- reactive({
      set.seed(input$fmrseed)
      g <- graph.adjacency(matadj(), weighted = T, mode = "undirected")
      g <- simplify(g)
      V(g)$label <- V(g)$name
      #V(g)$degree <- degree(g)
      V(g)$color <- "pink"
      layout <- layout.lgl(g)
      rv <- list()
      rv$g <- g
      rv$layout <- layout
      rv
    })
    
    radialnet <- reactive({
      set.seed(input$fmrseed)
      radial <- as.radialNetwork(fit1())
    })  
    
    ###Different Social Network Graphics
    
    #Radial Network
    output$radial <- renderRadialNetwork({
      radialNetwork(radialnet())
    })
    output$radial1 <- renderRadialNetwork({
      radialNetwork(radialnet())
    })
    
    #Diagonal Network
    output$diagonal <- renderDiagonalNetwork({
      diagonalNetwork(
        radialnet(),
        height = NULL,
        width = NULL,
        fontSize = 10,
        fontFamily = "serif",
        linkColour = "#ccc",
        nodeColour = "#fff",
        nodeStroke = "steelblue",
        #textColour = "#ccc",
        opacity = 0.9,
        margin = NULL
      )
    })
    
    output$diagonal1 <- renderDiagonalNetwork({
      diagonalNetwork(
        radialnet(),
        height = NULL,
        width = NULL,
        fontSize = 10,
        fontFamily = "serif",
        linkColour = "#ccc",
        nodeColour = "#fff",
        nodeStroke = "steelblue",
        #textColour = "#ccc",
        opacity = 0.9,
        margin = NULL
      )
    })
    
    #Dendro Network
    output$dendro <- renderDendroNetwork({
      dendroNetwork(
        fit1(),
        height = 500,
        width = 1000,
        fontSize = 10,
        linkColour = "#ccc",
        nodeColour = "#fff",
        nodeStroke = "steelblue",
        #textColour = "#ccc",
        textOpacity = 0.9,
        textRotate = NULL,
        opacity = 0.9,
        margins = NULL,
        linkType = c("elbow", "diagonal"),
        treeOrientation = c("horizontal", "vertical"),
        zoom = TRUE
      )
    })
    
    output$dendro1 <- renderDendroNetwork({
      dendroNetwork(
        fit1(),
        height = 500,
        width = 1000,
        fontSize = 10,
        linkColour = "#ccc",
        nodeColour = "#fff",
        nodeStroke = "steelblue",
        #textColour = "#ccc",
        textOpacity = 0.9,
        textRotate = NULL,
        opacity = 0.9,
        margins = NULL,
        linkType = c("elbow","diagonal"),
        treeOrientation = c("horizontal","vertical"),
        zoom = TRUE
      )
    })
    
    # Fruchterman-Reingold Network
    output$fmr <- renderPlot({
      rv <- fmrlayout()
      plot(rv$g, layout = rv$layout)
    })
    
      
      
      output$introduction<- renderText({
      print("Analysing the pleothora of textual data available via API'S like Reddit and YouTube, Textual Analytics and Network Mining are two approaches which provide great insights to social Media Data

Identifying the key themes set around a variety of Conversations Involves Processing Corpus of Textual Documents and finding different Influencers Networks ")
               
      })
      
      
      output$introduction_LDA <- renderText({
        print("In machine learning and natural language processing A topic model is a type of statistical model for discovering the abstract topics that occur in a collection of documents. 

Topic modeling finds its usage for discovery of hidden semantic structures in a text body")
        
        })
      

      
      output$introduction_COR <- renderText({
        print("If you have a term in mind that you have found to be particularly meaningful to analysis, then you may find it helpful to identify the words that most highly correlate with that term.
If words always appear together, then correlation=1.0")
        
      })
      
      output$introduction_FREQ <- renderText({
        print("The graph takes the users input and displays the most frequent words along with the frequency of occurence.
It is easier to identify what words appears frequently in the text")
        })   
      
      output$introductionWN<- renderText({
        print("Cluster analysis is a field of data analysis that extracts underlying patterns in data
              
Hierarchial Cluster : - Assumes a similarity function for determining the similarity of two instances. It Starts with all instances in a separate cluster and then repeatedly joins the two clusters that are most similar
until there is only one cluster.This history of merging forms a binary tree or hierarchy.
              
For textual data Hierarchial Clustering Typically use normalized, TF/IDF-weighted vectors and cosine similarity")
        
      })
      
      output$introductionCM<- renderText({
        print("
Betweeness Centrality : Based on the distances between vertices. It is (roughly) the number of geodesic paths that pass through any given node. Vertices with a high betweenness score will often act as bridging nodes between one or more communities

Closeness Centrality : Calculated as the sum of the length of the shortest paths between the node and all other nodes in the graph. Thus the more central a node is, the closer it is to all other nodes.

Eigenvector Centrality : The Eigenvector corresponding to the largest Eigenvalue of the adjacency matrix gives a high score to vertices that either have a lot of connections, or are connected to someone with a lot of connections.")
        
      })
      


      output$introductionCD1<- renderText({
        print("The network data pose challenges to classical clustering method disucssed  above and may at time unable to cluster communities or interest .Clustering works on the distance or similarity matrix (hierarchical clustering).Network data tends to be  discrete leading to algorithms using the graph property directly")
      })
      
      
  
      
      output$introductionCD<- renderText({
        print("
Edge Betweenness Community : is a hierarchical decomposition process where edges are removed in the decreasing order of their edge betweenness scores i.e. the number of shortest paths that pass through a given edge

spinglass Community :  is an approach from statistical physics, based on the so-called Potts model. In this model, each particle (i.e. vertex) can be in one of c spin states, and the interactions between the particles (i.e. the edges of the graph) specify which pairs of vertices would prefer to stay in the same spin state and which ones prefer to have different spin states

Leading Eigenvector Community : a top-down hierarchical approach that optimizes the modularity function again. 

In each step, the graph is split into two parts in a way that the separation itself yields a significant increase in the modularity. The split is determined by evaluating the leading eigenvector of the so-called modularity matrix, and there is also a stopping condition which prevents tightly connected groups to be split further


Label propagation :  is a simple approach in which every node is assigned one of k labels. The method then proceeds iteratively and re-assigns labels to nodes in a way that each node takes the most frequent label of its neighbors in a synchronous manner. The method stops when the label of each node is one of the most frequent labels in its neighborhood

Louvain :

Add a button to display the modularity of each Algorithm and choose the Modularity with the heighest value Compare the Algorithms Performance using the Modularity Scores")
        
      })
      
      

      output$introductionSA<- renderText({
        print("Sentiment analysis is extremely useful in social media monitoring as it allows us to gain an overview of the wider public opinion behind certain topics it has the ability to quickly understand consumer attitudes.

Most tool Inclusive BrandWatch Categorised Consumer Voice in 3 tones Mainly Positive Negative Neutral , Syuzhet (includes the NRC Word-Emotion Association Lexicon algorithm) classify emotions in 7 broad categories")
      }) 
      
      
      
      output$str <- renderPrint({
        str(data1)
      })
      
      
      output$data <- renderTable({
        #data1[,"Full.Text"]  #visualising data pertaining to a specific coloumn
        head(data1,50)
      })
      
      
      output$summary <- renderPrint({
        summary(data1)
      })      
      
      
#Word Cloud
      
      wordcloud_rep <- repeatable(wordcloud)
      
      output$plot <- renderPlot(
        {
          par(bg = 'grey')
          if(input$colourword =="Pallete1"){
            
            wordcloud_rep(r_stats_text_corpus,random.order=F,scale=c(5,2),colors= rev(colorRampPalette(brewer.pal(9,"Blues"))(32)[seq(8,32,6)]),min.freq = input$freq, max.words=input$max)
          }
          
          if(input$colourword == "Pallete2"){
            wordcloud_rep(r_stats_text_corpus,random.order=F,scale=c(5,2),colors= rainbow(50),min.freq = input$freq, max.words=input$max)
          }
          
          if(input$colourword == "Pallete3"){
            wordcloud_rep(r_stats_text_corpus,random.order=F,scale=c(5,2),colors=brewer.pal(8,"Dark2"),min.freq = input$freq, max.words=input$max)
          }
          
        })

#End of Word Cloud             
      

#Sentiment Analysis
            output$plot2 <- renderPlot({
      ggplot(senti_data_analysis, aes(x=sentiment, y=count, fill=sentiment))+ geom_bar(stat = "identity") + labs(x="Sentiments", y="Frequency", title="Sentiment Analysis ")
      })

#End of Sentiment Analysis
            
#Topic Modelling    
      output$plot3 <- renderPlot({
        qplot(names(topics), ..count.. , data=topics, geom="bar", fill=term[topic],position="dodge", xlab = "Topics", ylab="Count", main= "        LDA Topic Modelling  Data  Exploration         ")
        
      })
      
#End of Topic Modelling       
      
#Most Frequent Words
      
        output$plot4 <- renderPlot({
        ggplot(df[1:input$freqnum,], aes(x = term, y = freq)) + geom_bar(stat = "identity") +xlab("Terms") + ylab("Count") + coord_flip()
      })
      
#End of Most Frequent Words  
            
##Correlation Analysis 
      
      output$corelation <- renderPlot({ 
      toi <- input$term
      corlimit <- 0.5 #  lower correlation bound limit.
      cod_0.1 <- data.frame(corr = findAssocs(tdm, toi, corlimit)[[1]],terms = names(findAssocs(tdm, toi, corlimit)[[1]]))
      #Create a factor to allow ggplot to sort the dataframe
      cod_0.1$terms <- factor(cod_0.1$terms ,levels = cod_0.1$terms)
      ggplot(cod_0.1,aes( y = terms  ) ) + geom_point(aes(x = corr), data = cod_0.1) +theme(panel.spacing.y = unit(1000,"cm"),axis.text.y = element_text(colour="grey20",size=12) )+xlab(paste0("Correlation with the term ", "\"", toi, "\""))
        })

#End of Correlation Analysis 
      

##Word Network        

      output$graph2 <- renderPlot({
        if (input$n3 == "layout.fruchterman.reingold") { 
          plot(g,layout= layout.fruchterman.reingold)
          
        }
        
        if (input$n3 == "kawai") { 
          
          plot(g, layout= layout.lgl )
          #layout.kamada.kawai
          #layout.lgl  
        }
        
      }) 
       
      
###End of Word Network
      
      
##Hierarchial Clustering       
      output$graph1 <- renderPlot(
        {
          if(input$clusterplot =="dendrogram")
          {
            hcd = as.dendrogram(fit)
            plot(hcd,cex=2)
            
          }  
            
          if(input$clusterplot== "fan_Layout")  
          {
            plot(as.phylo(fit), type = "fan")
          }
          
          }
        
      )
##End of Hierarchial Clustering       

      
      
##Influencer Identification
      
      output$centrality <- renderPlot(
        {
          
          if(input$centrality1 =="Tweet_Network")
          {
            
            degree1 <- degree(rt_graph)
            coords <- layout.kamada.kawai(rt_graph)
            plot(rt_graph, layout = coords,vertex.size = log(degree1+1L)+2L, vertex.label.cex = plotrix::rescale(degree1, c(.7, 2)))
            title("Social Data Network :- Twitter") 
            
          }  
          
          if(input$centrality1 =="Betweenness")
          {
            bc <- betweenness(rt_graph)
            coords <- layout.kamada.kawai(rt_graph)
            plot(rt_graph, layout = coords,vertex.size = log(bc+1L)+2L, vertex.label.cex = plotrix::rescale(bc, c(.7, 2)))
          }  
          
          if(input$centrality1== "Eigen_Centrality")  
          {
            ec <- eigen_centrality(rt_graph)
            coords <- layout.kamada.kawai(rt_graph)
            plot(rt_graph, layout = coords, vertex.size = ec$vector*10L,vertex.label.cex = plotrix::rescale(ec$vector, c(.7, 2)))
          }
          
          if(input$centrality1 == "Closeness")
          { 
            close <- closeness(rt_graph)
            coords <- layout.kamada.kawai(rt_graph) #coords <- layout.auto(rt_graph)
            plot(rt_graph, layout = coords, vertex.size = close*10L, vertex.label.cex = plotrix::rescale(close, c(.9, 1)))
            
          }
        }
      )
      
##End of Influencer Identification
      
      
      
      
##Interest Graph Analysis  
     
      output$democommunity <- renderPlot(
        {
          if(input$datacommunity =="edge.betweenness.community")
          {
            edgeBetweenessWC <- edge.betweenness.community(g)
            plot(g, vertex.size=13, vertex.color=membership(edgeBetweenessWC),layout=layout.lgl)
          }  
          
          if(input$datacommunity== "louvain")  
          {
            louvainwc <- cluster_louvain(g)
            #modularity(louvainwc)
            plot(g, vertex.size=13, vertex.color=membership(louvainwc),layout=layout.lgl)
            
          }
          
          
          if(input$datacommunity== "Label Propogation")  
          {
            propagationwc <- label.propagation.community(g)
            #modularity(propagationwc)
            plot(g, vertex.size=13, vertex.color=membership(propagationwc),layout=layout.lgl)
            
          }
          
          
          if(input$datacommunity== "spinglass.community")  
          {
            spinglasswc <- spinglass.community(g)
            #modularity(spinglasswc)
            plot(g, vertex.size=13, vertex.color=membership(spinglasswc),layout=layout.lgl)
            
          }
          
          
          if(input$datacommunity == "leading.eigenvector.community")
          { 
            eigenvectorwc <- leading.eigenvector.community(g)
            #modularity(eigenvectorwc)
            plot(g, vertex.color=membership(eigenvectorwc),vertex.size= membership(eigenvectorwc),layout=layout.lgl)
            
            
          }
        }
      )
      
      
      
##End of Interest Graph Analysis      
      
      


      
##Download Handler      
       output$downloadPlot <-  downloadHandler(
        filename=function(){
          paste("image","png",sep=".")
        },
        
        content = function(file){
          png(file)
          output$sna
          dev.off()
          
        }
        
      )
    }
      
)


