library(shiny)
library(RMySQL)
library(tidyverse)
library(DT)
library(shinythemes)



############## Connection à la base de données SQL ##############
con <- dbConnect(MySQL(), host="localhost", user="root", password="rootroot", dbname="evaluation")
dbGetQuery(con, "SET NAMES 'utf8' ")
print("Connecté")


### Partie FORMATION ###
formation <- tbl(con, sql("SELECT DISTINCT choix.libelle, choix.id
                    FROM choix
                    inner join reponse on choix_id = choix.id
                    inner join session on session.id = reponse.session_id
                    where reponse.question_id = 14")) %>%
    collect()
#View(formation)

question <- tbl(con, sql('select choix.id, reponse_formation.session_id,
                            page.titre, question.libelle, reponse.score, reponse.texte
                            FROM choix
                            INNER JOIN reponse as reponse_formation
                            ON reponse_formation.choix_id = choix.id
                            AND reponse_formation.question_id = 14
                            INNER JOIN reponse
                            ON reponse.session_id = reponse_formation.session_id
                            INNER JOIN question
                            ON question.id = reponse.question_id
                            INNER JOIN page
                            ON page.id = question.page_id')) %>% 
    collect()

#View(question)

question_formation <- inner_join(formation, question, by = c("id" = "id"), 
                                 suffix = c(".formation", ".question"))
#View(question_formation)


### Partie ETUDIANT ###

noms <- tbl(con, sql("SELECT session_id, texte as nom FROM question 
                     INNER JOIN reponse ON question.id = reponse.question_id 
                     WHERE  question.id =2")) %>% 
    collect()
#View(noms)

reponse <- tbl(con, sql("select session_id, reponse.id as reponse_id, 
                        question.libelle, question.id as question_id, texte, score
                        from reponse
                        inner join question
                        on reponse.question_id = question.id")) %>% 
    collect()
#View(reponse)

reponse_etudiant <- inner_join(noms, reponse, by=c("session_id" = "session_id"))

#Infos épurées pour l'affichage de l'étudiant séléctionné :
reponse_etudiant <- subset(reponse_etudiant, select=-c(reponse_id))

?selectInput()

############## UI ############## 

ui <- fluidPage(theme = shinytheme("united"),
                
                # Titre
                headerPanel(list(HTML('<p style="color:#FF6733"> <img src="https://cibul.s3.amazonaws.com/event_cefim-jpo_166798.jpg" height = 50 />
                                       Bilan de formation </p> <br>')),
                            windowTitle = "Bilan de formation"),
                
                # Un menu de 2 pages 
                tabsetPanel(
                    tabPanel("Formation", # Partie FORMATION, ajouter 2 filtres: formation et question 
                             sidebarLayout(
                                 sidebarPanel(
                                     p("Instruction : Choisir la formation puis la question "),
                                     selectInput("formation",
                                                 "Choisir la formation :",
                                                 choices = unique(formation$libelle),
                                                multiple = T),
                                     selectInput("question",
                                                 "Choisir la question :",
                                                 choices = NULL)
                                 ),
                                 
                                 # Afficher le tableau et le graphique en fonction du type de question = 'score'
                                 mainPanel(
                                     dataTableOutput("table_formation"),
                                     htmlOutput("HTML"),
                                     conditionalPanel(
                                         condition = "output.panelStatus",
                                         plotOutput("histo"),
                                         downloadButton('downloadPlot','Télécharger')
                                     ),
                                 )
                             )
                    ),
                    tabPanel("Etudiant", # Partie ETUDIANT, filtre par le nom de l'etudiant
                             sidebarLayout(
                                 sidebarPanel(
                                     selectInput("nom",
                                                 "Choisir le nom de l'étudiant :",
                                                 choices = unique(reponse_etudiant$nom))
                                 ),
                                 
                                 # Afficher le tableau
                                 mainPanel(
                                     dataTableOutput("table_etudiant")
                                 )
                             )
                    )
                )
)


############## SERVER ##############  
server <- function(input, output, session) {
    
    # Fonction pour couper la connection quand bouton 'Stop' appuyé
    onStop(function(){
        dbDisconnect(con)
        print("Déconnecté")
    })
    
    # Filtre question dynamique par rapport avec le filtre formation
    
    formation_filter <- reactive({
        question_formation %>% 
            filter(libelle.formation %in% input$formation)
    })
    
    # Reprendre la valeur dans le filtre 'formation' et chercher les questions correspondantes
    observeEvent(formation_filter(), {
        question_filter <- unique(formation_filter()$libelle.question)
        updateSelectInput(session, "question", choices = question_filter)
    })
    
    # Table formation apres les filtres
    df_formation <- reactive({
        question_formation %>% 
            filter(libelle.formation %in% input$formation,
                   libelle.question == input$question) %>% 
            select(libelle.formation, titre, libelle.question, score, texte)
    })
    
    # Afficher le tableau Formation
    output$table_formation <- renderDataTable({
        datatable(
            df_formation(), extensions = 'Buttons', 
            options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            )
        )
    })
    
    # Affichage conditionnel une histogramme
    
    output$panelStatus <- reactive({
        !is.na(df_formation() %>% 
                   select(score))[1]
    }) 
    outputOptions(output, "panelStatus", suspendWhenHidden = FALSE)
    
    
    # Histogramme pour les questions 'score'
    
    plot_histo <- reactive({
        df_formation() %>%
        ggplot() +
        geom_histogram(aes(x = score, fill = libelle.formation), alpha=0.8, position = 'dodge', bins = 10) +
        labs(title = "Répartition des scores en fonction de la formation", 
             y="Nombre d'étudiants", x="Scores", fill = "Formation")+
        theme(plot.title = element_text(color="#FF6733", size = 16, face = "bold", hjust = 0.5),
              axis.title.x = element_text(face = 'italic'),
              axis.title.y = element_text(face = 'italic')
            )
        })
    
    # Ajouter le bouton pour télécharger le graphique sous form PNG
    
    output$histo <- renderPlot({
        print(plot_histo())
    })
    
    output$downloadPlot <- downloadHandler(
        filename = function() {paste(input$histo,'histogramme.png',sep='')},
        content = function(histo){
            ggsave(histo,plot_histo())
            }
        )

    
    # Afficher les reponses des etudiants
    df_etudiant <- reactive({
        reponse_etudiant %>% 
            filter(nom == input$nom)
    })
    
    output$table_etudiant <- renderDataTable({
        datatable(df_etudiant(), 
                  extensions = 'Buttons',
                  options = list(pageLength=5, dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
                  )
        })
    
    #HTML pour détacher la table avec le graphique
    output$HTML <- renderText({paste("<br> <br> <br>")})
}

# Run the application 
shinyApp(ui = ui, server = server)