function(input, output, session){
  
  year_1 = reactive({as.numeric(input$year1)})
  year_2 = reactive({as.numeric(input$year2)})
  branche = reactive(input$ca_branche)
  entreprise = reactive(input$ca_entreprise)
  bd_1 = reactive({ data %>% filter(ANNEE==year_1())})
  bd_2 = reactive({ df2 %>% filter(ANNEE==year_2())})
  champion = reactive({evolution(df1_evolution,input$year1)})
  
#####------------------- Les différents outputs--------------------------####
  
  ## Table de la data
  output$table <- renderDT({
    My_data
  })
  
  
  ## Chiffres d'affaires
  output$ca <- renderValueBox({
    valueBox(
      value = paste(global_ca(bd_1()), " FCFA"),
      subtitle = "CHIFFRE D'AFFAIRE GLOBAL",
      icon = icon("money"),
      color = "orange"
    
    )
  })
  
  ## Pourcentage non vie
  output$percent_nonvie <- renderValueBox({
    valueBox(
      value = paste(percentage_non_vie(bd_1()), " %"),
      subtitle = "POURCENTAGE NON VIE",
      icon = icon("download"),
      color = "green" 
    )
  })
  
  ## Pourcentage non vie
  output$percent_vie <- renderValueBox({
    valueBox(
      value = paste(percentage_vie(bd_1()), " %"),
      subtitle = "POURCENTAGE VIE",
      icon = icon("download"),
      color = "maroon" 
    )
  })
  
  ## Autres choses 
  output$percent_vie1 <- renderValueBox({
    valueBox(
      value = paste(percentage_vie(bd_1()), " %"),
      subtitle = "POURCENTAGE VIE",
      icon = icon("download"),
      color = "green" 
    )
  })
  
  
  output$Plot = renderImage({
    anim = ggplot(df1 %>% filter(ANNEE == year_1()), aes(rank, group = NOM_SOCIETE, fill = as.factor(NOM_SOCIETE), color = as.factor(NOM_SOCIETE))) +
            geom_tile(aes(y = Ch_Aff/2, height = Ch_Aff, width = 0.9), alpha = 0.8, color = NA) +
            geom_text(aes(y = 0, label = paste(NOM_SOCIETE, " ")), vjust = 0.2, hjust = 1, position = position_dodge(0.9)) +
            geom_text(aes(y=Ch_Aff,label = Ch_Aff, hjust=0)) +
            coord_flip(clip = "off", expand = FALSE) +
            scale_x_reverse()+
            theme(axis.title = element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  legend.position="none",
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  panel.grid.major.x = element_line( size=.1, color="grey" ),
                  panel.grid.minor.x = element_line( size=.1, color="grey" ),
                  plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
                  plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
                  plot.margin = margin(1,2, 1, 5, "cm")) +
              transition_manual(ANNEE)
           
    anim_save("outfile.gif", animate(anim,renderer = gifski_renderer())) 
    list(src = "outfile.gif",
         contentType = 'image/gif',
          width = 700,
          height = 450
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
 # Commentaires 
  
  output$max_taux = renderText({
    paste("TAUX MAXIMAL D'EVOLUTION : ", champion()$taux," %")
  })
  output$annee = renderText({
    paste("ANNEE : ", champion()$ANNEE)
  })
  output$nom = renderText({
    paste("SOCIETE : ", champion()$NOM_SOCIETE)
  })
  # Prestation de sinistre
  
  output$pres <- renderValueBox({
    valueBox(
      value = paste(global_pres(bd_2()), " FCFA"),
      subtitle = "Chiffre d'affaire global",
      icon = icon("download"),
      color = "teal"
      
    )
  })
  
  output$pres_nonvie <- renderValueBox({
    valueBox(
      value = paste(pres_non_vie(bd_2()), " %"),
      subtitle = "Pourcentage NON VIE",
      icon = icon("download"),
      color = "light-blue" 
    )
  })
  
  output$pres_vie <- renderValueBox({
    valueBox(
      value = paste(pres_vie(bd_2()), " %"),
      subtitle = "Pourcentage  VIE",
      icon = icon("download"),
      color = "olive" 
    )
  })
  
  output$Plot_pres = renderImage({
    
    anim = ggplot(df2 %>% filter(ANNEE == year_2()), aes(rank, group = NOM_SOCIETE, fill = as.factor(NOM_SOCIETE), color = as.factor(NOM_SOCIETE))) +
      geom_tile(aes(y = PRESTATION/2, height = PRESTATION, width = 0.9), alpha = 0.8, color = NA) +
      geom_text(aes(y = 0, label = paste(NOM_SOCIETE, " ")), vjust = 0.2, hjust = 1, position = position_dodge(0.9)) +
      geom_text(aes(y=PRESTATION,label = PRESTATION, hjust=0)) +
      coord_flip(clip = "off", expand = FALSE) +
      scale_x_reverse()+
      theme(axis.title = element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.grid.major.x = element_line( size=.1, color="grey" ),
            panel.grid.minor.x = element_line( size=.1, color="grey" ),
            plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
            plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
            plot.margin = margin(1,2, 1, 5, "cm")) +
      transition_manual(ANNEE)
    
    anim_save("outfile_press.gif", animate(anim,renderer = gifski_renderer())) 
    list(src = "outfile_press.gif",
         contentType = 'image/gif',
         width = 700,
         height = 450
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)

  # Votre partie ici 
  
  output$Plot_ca_branche <- renderPlot({
    plot_ca_branche(data, branche())
      })

} 
