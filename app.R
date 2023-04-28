#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggfortify)#
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(DT)
library(readr)
library(gmodels)#Matrice de confusion
library(shiny)
library(rpart)#arbre de decision
library(rpart.plot)
library(nnet)#reseau de neurone
library(arules)
library(e1071)#SVM
library(DMwR2)#plus voisin




datas <- read.table("LasVegasTripAdvisorReviews-Dataset.csv", header=T, dec='.',na.strings = c('?'), sep=',')
datasup <- datas
datasup[ , c('User.country','Score','Traveler.type','Period.of.stay','Pool','Gym','Tennis.court','Spa','Casino','Free.internet','Nr..rooms','User.continent','Review.month','Review.weekday')] <- list(NULL)
#Preparation des données pour la classification ascendante hierarchique
i=c(-1,-5, -6,-7, -8, -9, -10, -11, -12, -13,-14, -16, -17, -19, -20)
sup <-datas[,i]
sup <- sup[,-4]
str(sup)
#centrer et reduire les données 
df.cr = scale(as.matrix(sup), center = TRUE, scale = TRUE)
row.names(df.cr)<-datas[,3]#ici, attribution de la classe Nr..Hotel.review comme etiquette aux données centrés et reduites 
print(df.cr)
#matrice de Distance entre les individus
df.dist <- dist(df.cr, method = "euclidean")

#modelsvm <- svm(Hotel.name~.,datasup)



shinyApp(
  ui = dashboardPage(
    dashboardHeader(
      title = h2("TP DE DATAMINING"),
      titleWidth = 400
      
    ),
    dashboardSidebar(
      sidebarMenu(
        
        menuItem("HOME", tabName = "manipulation de donnees", icon = icon("home")),
        
        menuItem("manipulation de donnees", tabName = "Manipulation de donnees", icon = icon("gears"),
                 menuSubItem("Donnee initial",tabName = "err", icon = icon("dashboard")),
                 menuSubItem("Suppression des donnees",tabName = "dfd", icon = icon("trash")),
                 menuSubItem("Valeurs manquantes",tabName = "dhdh", icon = icon("filter"))
                 ),
        
        menuItem("visualisation de donnees", tabName = "Visualisation de donnees", icon = icon("eye"),
                 menuSubItem("Statistiques",tabName = "ery", icon = icon("chart-line")),
                 menuSubItem("Histogrammes",tabName = "eri", icon = icon("chart-simple")),
                 menuSubItem("Diagramme en batons",tabName = "ert", icon = icon("square-poll-vertical")),
                 menuSubItem("Boite a moutache",tabName = "erp", icon = icon("square")),
                 menuSubItem("Nuage de points",tabName = "er", icon = icon("cloud"))
                 ),
        
        menuItem("extraction de donnees",icon = icon("database"),
                 tabName = "edd"
                 ),
        
        menuItem("classification supervisee",tabName = "classification supervisee",icon = icon("cubes"),
                 menuSubItem("Reseau de neuronnes",tabName = "eer", icon = icon("code-merge")),
                 menuSubItem("Plus proche voisins",tabName = "dfv", icon = icon("person")),
                 menuSubItem("SVM",tabName = "df", icon = icon("dashboard")),
                 menuSubItem("Arbre de decision",tabName = "erk", icon = icon("tree"))
                 ),
        
        menuItem("classification non supervisee",tabName = "classification non supervisee",icon = icon("cube"),
                 menuSubItem("Methode Kmeans",tabName = "Kmeans", icon = icon("dashboard")),
                 menuSubItem("Classification hierachique",tabName = "hier", icon = icon("goto"))
                 )
        
        
        
        )
    ),
    dashboardBody(
      tabItems(
        
        #presentation des donnees initailes
        tabItem(
          "Donnee initial",tabName = "err",
          DTOutput("donne_initial"),
          downloadButton('save_data', 'save to csv')),
        #suppression de donnees
        tabItem(
          "Suppression des donnees",tabName = "dfd",
          h2('Suppression des attributs peu pertinente'),
          h4('Les attributs que nous jugeons peu pertinente pour notre analyse sont les attributs suivant: User country, Score, Period of stay, Pool, Gym, tennis court, spa, Casino, Free internet, Hotel stars, Nr rooms, User continent Review month, Review weekday'),
          DTOutput("donne_new"),
          downloadButton('save_data1', 'save to csv')
              ),
        
        #remplacement des valeurs manquantes
        tabItem(
          "Remplacement des valeurs manquantes",tabName = "dhdh",
          h2("Notre jeu de donnnes ne possedent pas de valeurs manquantes")
        ),
        
        #statistiques
        tabItem(
          "Statistique",tabName = "ery",
          h2("Statistiques", align = 'center'),
          verbatimTextOutput('statistique')
        ),
        
        
        #histogrammes
        tabItem(
          "Histogrammes",tabName = "eri",
          h2("Histogramme", align = 'center'),
          selectInput('varh', 'Choisis une variable numerique :', choices = names(datasup)),
          plotOutput("hist")
        ),
        
        #nuage de points
        tabItem(
          "Nuage de points",tabName = "er",
          h2("Nuage des points", align = 'center'),
          selectInput('var1', 'Choisis une variable numerique :', choices = names(datasup)),
          selectInput('var2', 'Choisis une 2e variable numerique :', choices = names(datasup)),
          plotOutput("nuage")
        ),
        
        #boite a moustache
        tabItem(
          "Boite a moutache",tabName = "erp",
          h2("Boite a moustache", align = 'center'),
          selectInput('varb', 'Choisis une variable numerique :', choices = names(datasup)),
          plotOutput('Boite')
          
        ),
        
        #diagramme en batons
        tabItem(
          "Diagramme en batons",tabName = "ert",
          h2("Diagramme en baton", align = 'center'),
          selectInput('vard', 'Choisis une variable numerique :', choices = names(datasup)),
          plotOutput('baton')
        ),
        
        #extraction de donnees
        tabItem(
          "extraction de donnees",tabName = "edd",
          h2("Affichage des regles d'association", align = 'center'),
          verbatimTextOutput('regles'),
          h2("Commentaires des regles d'association", align = 'center')
        ),
        
        #arbre de decision
        tabItem(
          "Arbre de decision et Matrice de confusion",tabName = "erk",
          h2("Arbre de decision", align = 'center'),
          plotOutput('arbre'),
          h2("Matrice de confusion", align = 'center'),
          verbatimTextOutput('MatriceA'),
          verbatimTextOutput('evalA')
          
           ),
        
        #Reseau de neurones
        tabItem(
          "Reseau de neurones",tabName = "eer",
          h2("Reseau de neurones", align = 'center'),
          numericInput('varR', "Entrer le nombre K de couche cachee", value = 1, max = 100, step  = 1),
          verbatimTextOutput('neurone'),
          h2("Matrice de confusion", align = 'center'),
          verbatimTextOutput('MatriceR')
          
        ),
        
        #Plus proche voisin
        tabItem(
          "Plus proche voisin",tabName = "dfv",
          h2("Plus proche voisin", align = 'center'),
          numericInput('varP', "Entrer le nombre de poid de K", value = 1, max = 100, step  = 1),
          plotOutput('voisin'),
          h2("Matrice de confusion", align = 'center'),
          verbatimTextOutput('MatriceP')
          
        ),
        
        #SVM
        tabItem(
          "SVM",tabName = "df",
          h2("SVM", align = 'center'),
          verbatimTextOutput('SVM'),
          h2("Matrice de confusion", align = 'center'),
          verbatimTextOutput('MatriceS')
        ),
        
        
        #Methode Kmeans
        tabItem(
          "Methode Kmeans",tabName = "Kmeans",
          h2("Methode Kmeans", align = 'center'),
          numericInput('varK', "Entrer le nombre de clusters", value = 1, max = 100, step  = 1),
          plotOutput('Kmeans')
        ),
        
        #Classification Hierachique
        tabItem(
          "Classification Hierachique ascendante",tabName = "hier",
          h2("Classification Hierachique ascendante", align = 'center'),
          numericInput('varH', "Entrer le nombre de groupe", value = 2, max = 500, step  = 1),
          plotOutput('Hierachique')
        )
        
       )
      
      
      
      ),
      
      
     
      
    
    title = "LasVegasTripAdvisorReviews",
    skin = "yellow"
  ),
  
  
  server = function(input, output) {
    
    df <- reactive({
      datas
    })
    output$donne_initial <- renderDT({
      df()
    })
    df1 <- reactive({
      datasup
    })
    output$donne_new <- renderDT({
      df1()
    })
    #sauvegarde de la df au format csv
    output$save_data <- downloadHandler(
      filename <- function(){
        paste("data_", Sys.Date(), ".csv", sep = ',')
      },
      content <- function(file){
        write.csv(df(), file)
      }
    )
    output$save_data1 <- downloadHandler(
      filename <- function(){
        paste("New_data_", Sys.Date(), ".csv", sep = ',')
      },
      content <- function(file){
        write.csv(df1(), file)
      }
    )
    #Resumer statistique
    output$statistique <- renderPrint({
      summary(datasup)
    })
    #Boite a moustache
    output$Boite <- renderPlot({
      
      boxplot(datasup[,input$varb], main = 'Boite a moustache', xlab = input$varb)
      abline(datasup)
    })
    #Diagramme en baton
    output$baton <- renderPlot({
      
      barplot(datasup[,input$vard], main = 'Diagramme en baton', xlab = input$vard)
    })
    #Histogramme
    output$hist <- renderPlot({
      hist(datasup[,input$varh], main = 'Histogramme', xlab = input$varh)
    })
    #Nuage de point
    output$nuage <- renderPlot({
      plot(datasup[,input$var1], datasup[,input$var2],
           xlab = input$var1, ylab = input$var2)
    })
    
    #Extraction des regles d'association
    output$regles <- renderPrint({
      dataset = read.transactions('LasVegasTripAdvisorReviews-Dataset.csv', 
                                  rm.duplicates = TRUE)
      set.seed = 220 # Setting seed
      associa_rules = apriori(data = dataset,parameter = list(support = 0.004,confidence = 0.2))
      paste0(inspect(sort(associa_rules, by = 'lift')[1:10]))
    })
    
    #datasupProche = datas[ , c('User.country','Score','Period.of.stay','Hotel.name','Traveler.type','Pool','Gym','Tennis.court','Spa','Casino','Free.internet','Nr..rooms','User.continent','Review.month','Review.weekday')] <- list(NULL)
    datasup$Hotel.name = as.factor(datasup$Hotel.name)#Transformer la classe en variable categorielle
    nt = sample(1:nrow(datasup), 0.7*nrow(datasup))#choisir aleatoirement les 70% des lignes de iris
    train = datasup[nt,]#nouveau jeu de donnee
    test = datasup[-nt,]#pour le jeu de test
    
    #arbre de decision
    output$arbre<-renderPlot({
      train$Hotel.name = as.factor(train$Hotel.name)
      AD = rpart(Hotel.name~.,data=train)
      #Plot de l'arbre de decision
      rpart.plot(AD)
    })
    output$MatriceA<- renderPrint({
      AD = rpart(Hotel.name~.,data=train)
      pred = predict(AD, test, type = c("class"))#valeur predite du jeu de donnee test
      M = CrossTable(pred, test[,4])
    })
    output$evalA <- renderPrint(
      {
        AD = rpart(Hotel.name~.,data=train)
        pred = predict(AD, test, type = c("class"))#valeur predite du jeu de donnee test
        M = table(pred, test[,4])
        eval <- function(M){
          gener = sum(diag(M))/sum(M)
          n = dim(M)
          i=0
          rappel =  0
          for (i in 1:n) {
            rappel = M[i,i]/sum(M[i,]) + rappel
          }
          rappel = rappel/n
          pres = 0
          m = dim(M)
          for (i in 1:m) {
            pres = M[i,i]/sum(M[,i]) + pres
          }
          pres = pres/m
          
          return (c(gener, rappel, pres))
        }
        eval(M)
      }
    )
    
    
    #SVM
    output$SVM<-renderPrint({
      #SVM et test
      
      #dt = subset(train, select = c(4))
      modelsvm <- svm(Hotel.name~., data = train, scale = FALSE)
      modelsvm
    })
    output$MatriceS<- renderPrint({
      dt = subset(train, select = c(4))
      modelsvm <- svm(Hotel.name~., data = train, scale = FALSE)
      predn = predict(modelsvm, test ,type = c("class"))
      M = CrossTable(predn, test[,4])
    })
    
    #Plus proche voisin
    output$voisin <- renderPlot({
      knn = kNN(Hotel.name~., train,test, stand = FALSE, k = input$varP)
      plot(knn)
    })
    output$MatriceP<- renderPrint({
      knn = kNN(Hotel.name~., train,test, stand = FALSE, k = input$varP)
      #pred = predict(knn, test, type = c("class"))
      M = CrossTable(knn, test[,2])
    })
    
    
    #Reseau de neurone
    output$neurone<-renderPrint({
      #Reseau de neurone et test
      nn <- nnet(Hotel.name ~., train, size = input$varR)#reseau de neurone
      nn
    })
    output$MatriceR<- renderPrint({
      nn <- nnet(Hotel.name ~., train, size = input$varR)#reseau de neurone
      predn = predict(nn, test,type = c("class"))
      M = table(predn, test[,2])
      M
    })
    
    #Kmeans
    output$Kmeans <- renderPlot({
      #NOMBRE DE CLUSTERS DYNAMIQUE 
      df.kmeans <- kmeans(df.dist,input$varK)
      attributes(df.kmeans)
      df.kmeans$cluster
      table(df.kmeans$cluster)
      #plot(df.kmeans$cluster)
      #AFFICHAGE DU GRAPHE
      autoplot(df.kmeans, df.dist,frame= TRUE)
      
    })
    
    #Classification Hierachique ascendant
    output$Hierachique <- renderPlot({
      #Classification hierarchique ascendante
      cah.df <- hclust(df.dist,method = "ward.D")
      #Affichage du dectogramme 
      plot(cah.df)
      rect.hclust(cah.df,k=input$varH,border = 2)
      #decoupage des données en groupe
      groupes.cah <- cutree(cah.df, k = 3)
      table(groupes.cah)
      datas_tot <- cbind(datasup,groupes.cah)
      head(datas_tot)
    })
    
    
  }
)

