library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme.scss")
  ),
  
  tags$img(src = "grid_uatuma.jpg", 
           style = "width: 100%; max-height: 300px; object-fit: cover; margin-bottom: 20px;"),
  
  titlePanel(" "),
  
  div(
    style = "margin-bottom: 30px;",
    p(
      "Este aplicativo foi criado para facilitar a visualização dos dados coletados durante o ",
      tags$b("1º Curso de Campo em Biodiversidade"),
      ", do Instituto Federal Farroupilha - ",
      tags$em("Campus"),
      " Júlio de Castilhos. 
      Ele permite inserir dados sobre as espécies registradas em diferentes ambientes e gera visualizações que ajudam a analisar os padrões de riqueza e abundância."
    ),
    p("Para cada ambiente, informe:"),
    tags$ul(
      tags$li("O nome do ambiente (ex: floresta, plantação)"),
      tags$li("Os nomes das espécies separados por vírgula (ex: formiga1, formiga2)"),
      tags$li("Os valores de abundância de cada espécie, também separados por vírgula (ex: 5, 3, 1)")
    ),
    p("Os gráficos gerados incluem:"),
    tags$ul(
      tags$li("Gráfico de ranqueamento de espécies por abundância"),
      tags$li("Gráfico de riqueza de espécies observada"),
      tags$li("Gráfico de abundância total de indivíduos")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("inputs_ui"),
      actionButton("add_env", "Adicionar Ambiente"),
      checkboxInput("show_species_names", "Mostrar nomes das espécies no gráfico de ranqueamento", value = FALSE),
      checkboxInput("fix_y_rank", "Manter os mesmos valores no eixo Y no gráfico de ranqueamento", value = FALSE)
    ),
    mainPanel(
      h4("Informações por ambiente"),
      uiOutput("resumo_texto"),
      h4("Gráfico de ranqueamento de espécies"),
      uiOutput("rank_plot_ui"),
      h4("Gráfico de riqueza de espécies"),
      uiOutput("barplot_riqueza_ui"),
      h4("Gráfico de abundância de indivíduos"),
      uiOutput("barplot_abundancia_ui")
    )
  )
)

server <- function(input, output, session) {
  n_env <- reactiveVal(2)
  
  output$inputs_ui <- renderUI({
    lapply(1:n_env(), function(i) {
      instrucao_ambiente <- switch(
        as.character(i),
        "1" = "Insira o nome do ambiente 1 (ex: floresta)",
        "2" = "Insira o nome do ambiente 2 (ex: plantação)",
        paste0("Insira o nome do ambiente ", i, " (ex: ambiente ", i, ")")
      )
      
      div(
        style = "margin-bottom: 30px; padding-bottom: 10px; border-bottom: 1px solid #fff;",
        strong(paste("Nome do ambiente", i)),
        textInput(paste0("nome_", i), label = NULL, value = ""),
        div(style = "margin-top: -10px; margin-bottom: 10px;", tags$small(instrucao_ambiente)),
        
        div(
          style = "margin-bottom: 5px;",
          textInput(paste0("species_names_", i), "Nome das espécies registradas", value = ""),
          div(style = "margin-top: -10px; margin-bottom: 15px;", tags$small("Insira o nome das espécies separados por vírgula (ex: formiga1, formiga2)"))
        ),
        
        div(
          style = "margin-bottom: 5px;",
          textAreaInput(paste0("abund_", i), "Número de indivíduos por espécie (abundância)", rows = 2),
          div(style = "margin-top: -10px;", tags$small("Insira os valores de abundância das espécies separados por vírgula (ex: 1, 2, 3)"))
        )
      )
    })
  })
  
  observeEvent(input$add_env, {
    n_env(n_env() + 1)
  })
  
  dados_processados <- reactive({
    req(n_env() > 0)
    res <- list()
    abund_list <- list()
    
    for (i in 1:n_env()) {
      abund_str <- input[[paste0("abund_", i)]]
      nome <- input[[paste0("nome_", i)]]
      species_names_str <- input[[paste0("species_names_", i)]]
      
      if (!is.null(abund_str) && abund_str != "") {
        abund_vec <- suppressWarnings(as.numeric(unlist(strsplit(abund_str, ","))))
        abund_vec <- abund_vec[!is.na(abund_vec)]
        
        if (length(abund_vec) > 0) {
          riqueza_obs <- sum(abund_vec > 0)
          abund_total <- sum(abund_vec)
          
          if (!is.null(species_names_str) && species_names_str != "") {
            species_names_vec <- unlist(strsplit(species_names_str, ","))
            species_names_vec <- trimws(species_names_vec)
            if (length(species_names_vec) < length(abund_vec)) {
              species_names_vec <- c(species_names_vec, paste0("Sp", (length(species_names_vec) + 1):length(abund_vec)))
            } else if (length(species_names_vec) > length(abund_vec)) {
              species_names_vec <- species_names_vec[1:length(abund_vec)]
            }
          } else {
            species_names_vec <- paste0("Sp", seq_along(abund_vec))
          }
          
          if (is.null(nome) || nome == "") {
            nome <- paste("Ambiente", i)
          }
          
          res[[i]] <- data.frame(
            Ambiente = nome,
            Riqueza_Observada = riqueza_obs,
            Abundancia_Total = abund_total,
            stringsAsFactors = FALSE
          )
          abund_list[[i]] <- data.frame(
            Ambiente = nome,
            Especie = species_names_vec,
            Abundancia = abund_vec
          )
        }
      }
    }
    
    list(
      resumo = if (length(res) > 0) bind_rows(res) else NULL,
      abundancias = if (length(abund_list) > 0) bind_rows(abund_list) else NULL
    )
  })
  
  output$resumo_texto <- renderUI({
    df <- dados_processados()$resumo
    if (is.null(df)) {
      return(
        wellPanel(
          p("Preencha os campos ao lado para visualizar as informações dos ambientes.")
        )
      )
    }
    
    output_list <- lapply(1:nrow(df), function(i) {
      ambiente <- df$Ambiente[i]
      riqueza <- df$Riqueza_Observada[i]
      abundancia <- df$Abundancia_Total[i]
      
      wellPanel(
        h4(ambiente),
        strong("Número de espécies (riqueza):"), span(riqueza), br(),
        strong("Número de indivíduos (abundância):"), span(abundancia)
      )
    })
    
    do.call(tagList, output_list)
  })
  
  output$rank_plot_ui <- renderUI({
    df_abund <- dados_processados()$abundancias
    if (is.null(df_abund)) {
      wellPanel(p("Insira os dados para visualizar o gráfico de ranqueamento."))
    } else {
      plotOutput("rank_plot")
    }
  })
  
  output$barplot_riqueza_ui <- renderUI({
    df <- dados_processados()$resumo
    if (is.null(df)) {
      wellPanel(p("Insira os dados para visualizar o gráfico de riqueza."))
    } else {
      plotOutput("barplot_riqueza")
    }
  })
  
  output$barplot_abundancia_ui <- renderUI({
    df <- dados_processados()$resumo
    if (is.null(df)) {
      wellPanel(p("Insira os dados para visualizar o gráfico de abundância."))
    } else {
      plotOutput("barplot_abundancia")
    }
  })
  
  output$rank_plot <- renderPlot({
    df_abund <- dados_processados()$abundancias
    req(df_abund)
    
    df_rank <- df_abund %>%
      group_by(Ambiente) %>%
      arrange(desc(Abundancia)) %>%
      mutate(Rank = row_number()) %>%
      ungroup()
    
    p <- ggplot(df_rank, aes(x = Rank, y = Abundancia)) +
      geom_col(fill = "#969696") +
      facet_wrap(~Ambiente, scales = ifelse(input$fix_y_rank, "fixed", "free_y")) +
      xlab("Espécies ranqueadas (1º = mais abundante)") +
      ylab("Abundância") +
      theme_bw(base_size = 20) +
      theme(panel.grid = element_blank(),
            panel.border = element_rect(colour = "black"),
            axis.title = element_text(colour = "black", face = "bold"),
            axis.text = element_text(colour = "black"),
            axis.ticks = element_line(colour = "black", size = 0.25),
            plot.margin = margin(0.5, 1.5, 0.5, 1.5, "cm"),
            legend.position = "none")
    
    if (input$show_species_names) {
      p <- p + geom_text(aes(label = Especie), vjust = -0.3, size = 3)
    }
    
    p
  })
  
  output$barplot_riqueza <- renderPlot({
    df <- dados_processados()$resumo
    req(df)
    
    ggplot(df, aes(x = Ambiente, y = Riqueza_Observada)) +
      geom_bar(stat = "identity", width = 0.6, fill = "#969696") +
      ylab("Riqueza de espécies observada") +
      xlab(" ") +
      theme_bw(base_size = 20) +
      theme(panel.grid = element_blank(),
            panel.border = element_rect(colour = "black"),
            axis.title = element_text(colour = "black", face = "bold"),
            axis.text = element_text(colour = "black"),
            axis.ticks = element_line(colour = "black", size = 0.25),
            plot.margin = margin(0.5, 1.5, 0.5, 1.5, "cm"),
            legend.position = "none")
  })
  
  output$barplot_abundancia <- renderPlot({
    df <- dados_processados()$resumo
    req(df)
    
    ggplot(df, aes(x = Ambiente, y = Abundancia_Total)) +
      geom_bar(stat = "identity", width = 0.6, fill = "#969696") +
      ylab("Abundância total de indivíduos") +
      xlab(" ") +
      theme_bw(base_size = 20) +
      theme(panel.grid = element_blank(),
            panel.border = element_rect(colour = "black"),
            axis.title = element_text(colour = "black", face = "bold"),
            axis.text = element_text(colour = "black"),
            axis.ticks = element_line(colour = "black", size = 0.25),
            plot.margin = margin(0.5, 1.5, 0.5, 1.5, "cm"),
            legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)