dashboardPage(
  skin = "red",
  
##### l'entête
  
  dashboardHeader(
    title = span(img(src ="https://upload.wikimedia.org/wikipedia/commons/f/fe/Flag_of_C%C3%B4te_d%27Ivoire.svg", height = 25), "ASSURANCE CI"),titleWidth = 260),
  
##### Gestion de la barre

  dashboardSidebar(
    width = 260,
    sidebarMenu(
      #---------------------------------------------------------------------#
      menuItem("DATA",tabName = "bd"),
      #---------------------------------------------------------------------#
      menuItem("GRAPHES", tabName = "autre"),
      
      #---------------------------------------------------------------------#
      menuItem("DASHBOARD", tabName = "dashboard",
               menuSubItem("CHIFFRE D'AFFAIRE",tabName = "ca"),
               menuSubItem("PRESTATION DE SINISTRE",tabName = "pres"))
    )
  ),
  
##### le corps 
  dashboardBody(
    (
      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 18px;
      }
    ')))
    ),
    tabItems(
      #---------------------------------------------------------------------#
      tabItem("bd",
              fluidRow(
                box(width = 11.5, status = "warning", solidHeader = TRUE,
                    title = h3("BASE FINALE DES COMPAGNIES D'ASSURANCE DU MARCHE IVOIRIEN SUR LA PERIODE 1980-2022.",width = 8),
                    DTOutput("table",width = "100%",height = 500)
                )
            )
       ),
      #---------------------------------------------------------------------#
      tabItem("ca",
              fluidRow(
                valueBoxOutput("ca"),
                valueBoxOutput("percent_vie"),
                valueBoxOutput("percent_nonvie")
              ),
              fluidRow(
                box(
                  width = 8, status = "primary", solidHeader = TRUE,
                  title = h3("Evolution du chiffre d'affaire des assurances en CI de 1987 à 2020"),
                  imageOutput("Plot", width = "100%", height = 500)
                ),
                box(
                  width = 4, status = "primary", solidHeader = TRUE,
                  title = h2("Selection"),
                  sliderTextInput("year1",
                                  label = h5("Choix de l'année"),
                                  choices = unique(year1$ANNEE),
                                  selected = year1 %>% slice(1),
                                  grid = FALSE,
                                  animate = animationOptions(interval = 800, loop = FALSE)
                  ),
                  textOutput("max_taux"),
                  textOutput("annee"),
                  textOutput("nom")
                )
                
              )
      ),
      #---------------------------------------------------------------------#
      tabItem("pres",
              fluidRow(
                valueBoxOutput("pres"),
                valueBoxOutput("pres_vie"),
                valueBoxOutput("pres_nonvie")
              ),
              fluidRow(
                box(
                  width = 8, status = "warning", solidHeader = TRUE,
                  title = h2                                                                                                                                                                                                                                                                                                                                                                                                               ("Evolution des prestations de sinistre"),
                  imageOutput("Plot_pres", width = "100%", height = 500)
                ),
                box(
                  width = 4, status = "warning", solidHeader = TRUE,
                  title = h2("Selection"),
                  sliderTextInput("year2",
                                  label = h5("Choix de l'année"),
                                  choices = unique(year2$ANNEE),
                                  selected = year2 %>% slice(1),
                                  grid = FALSE,
                                  animate = animationOptions(interval = 800, loop = FALSE)
                  )
                )
                
            )
        ),
     #---------------------------------------------------------------------#
      tabItem("autre",
              fluidRow(
                box(
                  width = 8, status = "success", solidHeader = TRUE,
                  title = h3("Evolution du chiffre d'affaire par branche des entreprises"),
                  plotOutput("Plot_ca_branche", width = "100%", height = 600)
                ),
                box(
                  width = 4, status = "success", solidHeader = TRUE,
                  title = h3("Selection"),
                  selectInput(
                    "ca_branche",
                    "Choisir une compagnie",
                    unique(data$NOM_SOCIETE),
                    selected = data$NOM_SOCIETE[1]
                  )
               )
          )
      )
     #-------------------------- Fin dernier tabItem -----------------------#
    )
    #-------------------------- Fin tabItems -----------------------#
  )
)
