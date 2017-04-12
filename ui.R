
library (shiny)
library(shinydashboard)
library("shinythemes")

w <- "1500px"
h <- "600px"

shinyUI(fluidPage(theme = shinytheme("lumen"),
                  
                  
                  titlePanel(title = "TAG:Combining Text Analytics and Network Mining "),
                  br(),
                  br(),
                  
                  sidebarLayout( position = "right", 
                                 fixedPanel(
                                 
                                           ),
                                
                                 mainPanel(
                                     navlistPanel(
                                     tabPanel(h4("Introduction"),
                                              #tags$style(type='text/css', '#introduction {background-color: turquoise; color: blue;}'),
                                              
                                              
                                              fluidRow(h3("Introduction",align="center"),tags$img(src="TextAnalytics.jpg",height=300,width=600),align="center"),
                                              verbatimTextOutput("introduction")),
                                     
                                    
                                     tabPanel(h4("Upload the File"),br(),fileInput("data1",label=("Upload the Unstructured Textual data To be analysed")),
                                              
                                              fluidRow(column(width=6,h3("Summary of Data Uploaded"),verbatimTextOutput("summary")),
                                                       column(width=6,h3("Structure of Data Uploaded"),verbatimTextOutput("str")))),
                                              
                                              
                                     tabPanel(h4("Overview"),fluidRow(h2("Overview Of Data Uploaded"),align="center"),tableOutput("data")),
                                     
                                     tabPanel(h4("Word Cloud"),
                                              
                                              fluidRow(
                                                column(3,
                                                       sliderInput("freq","select the Frequency",min = 0,max = 500,value = 4)
                                                
                                                ),
                                                
                                                column(4, 
                                                       sliderInput("max","select max number of words ",min = 0,max = 4000,value = 100)
                                                ),
                                                column(5,
                                                       radioButtons("colourword","select the pallete of word cloud ",c("Pallete1","Pallete2","Pallete3"),inline=T)
                                                )),
                                              
                                              fluidRow(h3("Word Cloud of the Textual Document",align="center"),plotOutput("plot",width=1000,height=800)),
                                              fluidRow(column(width=6,downloadButton('downloadPlot', 'Download Plot')))),
                                           
                                     tabPanel(h4("Sentiment Analysis"),
                                     verbatimTextOutput("introductionSA"),
                                              
                                     fluidRow(h3("Evaluating the sentiment of the text",align="center"),value=5,plotOutput("plot2",width=1000,height=600))),
                                                       
                                     tabPanel(h4("Topic Modelling"),
                                              #tags$style(type='text/css', '#introduction_LDA {background-color: royalblue; color: black;}'),
                                              fluidRow(h3("Understanding the main Topic present in the Textual Document",align="center"),value=6,plotOutput("plot3",width=1000,height=600)),
                                              verbatimTextOutput("introduction_LDA")),
                                              
                                     tabPanel(h4("Word Frequency"),
                                              tags$style(type='text/css', '#introduction_FREQ {background-color: royalblue; color: black;}'),
                                              fluidRow(h3("Word Frequency",align="center")),
                                              fluidRow(
                                                splitLayout(cellWidths = c("100%", "60%"),verbatimTextOutput("introduction_FREQ"), textInput("freqnum","Enter the Number of terms to be displayed",value=50))
                                              ),
                                              br(),
                                              br(),
                                              #sidebarPanel(textInput("freqnum","Enter the Number of terms to be displayed",value=50)),
                                              fluidRow(plotOutput("plot4",width=1000,height=800))),
                                          
                                     
                                     
                                     tabPanel(h4("Correlation Analysis"),
                                              tags$style(type='text/css', '#introduction_COR {background-color: royalblue; color: black;}'),
                                         
                                              fluidRow(h3("Term Correlations",align="center")),
                                              fluidRow(
                                                splitLayout(cellWidths = c("100%", "60%"),verbatimTextOutput("introduction_COR"), textInput("term","Enter the word to find correlation with",value="new"))
                                              ),
                                              br(),
                                              br(),
                                              
                                              #sidebarPanel(textInput("term","Enter the word to find correlation with",value="new")),
                                              fluidRow(plotOutput("corelation",width=1000,height=2000))),
                                     
                                     
                                     
                                     tabPanel(h4("Word Network"),
                                              
                                              sidebarLayout(
                                                position = "right",fluid = TRUE,
                                                sidebarPanel(
                                                  #h2("Controls"),
                                                  sliderInput("sparse", "Sparsity:", 0.9, 1, 0.994,0.002),
                                                  numericInput("fmrseed", "F-R Seed:", 1234, 1, 10000, 1)
                                                ),
                                                
                                                
                                                
                                                
                                                mainPanel(
                                                  h2("Network Graphs"),
                                                  verbatimTextOutput("introductionNG"),
                                                  fluidRow(column(width=10,h4("SEMANTIC NETWORK OF EXTRACTED TERMS"),verbatimTextOutput("introductionWN"))),
                                                  
                                                  tabsetPanel(
                                                    tabPanel("Fruchterman-Reingold", plotOutput("fmr",height = 1000,width = 1000)),
                                                    tabPanel("Dendro", dendroNetworkOutput("dendro",height = 1500,width = 800)),
                                                    tabPanel("Diagonal", diagonalNetworkOutput("diagonal")),
                                                    tabPanel("Radial",radialNetworkOutput("radial"))
                                                    
                                                  )
                                                )
                                              )),
                                     
                                    
                                     
                                     tabPanel(h4("Interest Graph Analysis"),
                                              fluidRow(column(width=8,h3("Detect Communities of Correlated Topics"),verbatimTextOutput("introductionCD1"))),
                                              verbatimTextOutput("introductionCD"),
                                              #h2("A Link to Community detection algorithms and how they cluster nodes based on different similarity metrics most algorithms use Node Similarity and not edge based similarity https://www.r-bloggers.com/summary-of-community-detection-algorithms-in-igraph-0-6"),
                                              sidebarPanel(radioButtons("datacommunity","Choose a community detection algorithm ",c("edge.betweenness.community","spinglass.community","leading.eigenvector.community","louvain","Label Propogation"))),
                                              fluidRow(plotOutput("democommunity",width=2000,height=1000))),
                                     
                                     


                                     tabPanel(h4("Influencer Identification"),
                                             fluidRow(column(width=12,h3("Centraility Measures"),verbatimTextOutput("introductionCM"))),
                                             fluidRow(
                                               splitLayout(cellWidths = c("80%", "70%"), tags$img(src="Centraility1.png",height=300,width=600), tags$img(src="Centrality2.png",height=300,width=400))
                                             ),
                                             br(),
                                             br(),
                                              #textInput("twitterTerm","Search Twitter for",value="evian"),
                                              
                                              sidebarPanel(radioButtons("centrality1","Select a Centrality Measure ",c("Betweenness","Eigen_Centrality","Closeness"))),
                                              fluidRow(plotOutput("centrality",width=2000,height=1000))),
                                      
                                     
                                             
                                     
                                     
                                     id="tabselected"
                                     
                                   )
                                 ))
                  
                  
)
)










