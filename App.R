# Load required libraries
#install.packages("gridExtra")
library(gridExtra)

library(shiny)
if (!require(shinydashboard)) install.packages("shinydashboard")
library(shinydashboard)
library(dplyr)
library(plotly)
library(DT)
library(shinyjs)
library(shinyBS) 
library(reticulate)


library(rsconnect)


# Define SNP groups
group1_types <- c("GWAS", "eeQTL", "aseQTL", "ieQTL", "seQTL", "RNAseq")
group2_types <- c("coding", "chipseqQTL", "asbQTL", "ATAC", "Histone-related")
group3_types <- c("eeQTLGWAS", "eeQTLGWASasb", "eeQTLGWASasbase", "AllSNP", "PBLUP")

# Define trait order
traitorder <- c("Fat_percent", "Protein_percent", "Milk_volume", "Fat_yield", "Protein_yield")

# Define functional variant data
functional_variant_data <- data.frame(
  Functional_class = c("GWAS", "RNA-seq", "RNA-seq", "RNA-seq", "RNA-seq", "Histone-related", "Histone-related", "Histone-related", "Coding", "Combined sets", "Combined sets", "Combined sets", "Combined sets", "Combined sets", "All"),
  Variant_type = c("GWAS", "eeQTL", "aseQTL", "ieQTL", "seQTL", "asbQTL", "ChIP-seq QTL", "ATAC-QTL", "Coding", "RNA-seq (all)", "Histone-related (all)", "eeQTL + GWAS", "eeQTL + GWAS + asbQTL", "eeQTL + GWAS + asbQTL + aseQTL", "All"),
  Initially_identified = c(1847, 4042, 5625, 1856, 821, 6937, 6121, 12303, 2515, 12344, 25361, 5889, 12826, 18451, 43319),
  Before_filtering = c(1248, 5900, 4200, 1600, 690, 5704, 4940, 10368, 1499, 11310, 18523, 7111, 12712, 16658, 34927),
  After_filtering = c(1124, 5328, 4154, 903, 608, 5682, 4496, 9068, 1389, 10576, 17407, 6416, 11996, 15932, 32595)
)
functional_variant_data$Variant_type <- ifelse(functional_variant_data$Variant_type == "ChIP-seq QTL", "chipseqQTL", functional_variant_data$Variant_type)
# Define custom color palette
custom_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

ui <- dashboardPage(
  dashboardHeader(title = "Genomic Data Analysis"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap');
        body { font-family: 'Roboto', sans-serif; }
        .main-sidebar { background-color: #2c3e50 !important; }
        .sidebar-menu>li>a { color: #ecf0f1 !important; transition: all 0.3s ease; }
        .sidebar-menu>li>a:hover { background-color: #34495e !important; }
        .sidebar-menu>li.active>a { background-color: #34495e !important; }
        .content-wrapper { background-color: #f5f5f5; }
        .box { border-top: 3px solid #3498db; transition: all 0.3s ease; }
        .box:hover { box-shadow: 0 5px 15px rgba(0,0,0,0.1); }
        .box-header { background-color: #f8f9fa; }
        .box-title { font-weight: 300; letter-spacing: 0.5px; }
        .form-control { border-radius: 3px; transition: all 0.3s ease; }
        .form-control:focus { border-color: #3498db; box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25); }
        .btn-primary { background-color: #3498db; border-color: #3498db; transition: all 0.3s ease; }
        .btn-primary:hover { background-color: #2980b9; border-color: #2980b9; }
        .animate__animated { animation-duration: 0.5s; }
        .loading-spinner {
          position: absolute;
          top: 50%;
          left: 50%;
          border: 16px solid #f3f3f3;
          border-radius: 50%;
          border-top: 16px solid #3498db;
          width: 120px;
          height: 120px;
          animation: spin 2s linear infinite;
          z-index: 9999;
        }
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        .loading-overlay {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background-color: rgba(255, 255, 255, 0.7);
          display: flex;
          justify-content: center;
          align-items: center;
          z-index: 9998;
        }
      "))
    ),
    sidebarMenu(
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Grouped Visualization", tabName = "groupedVisualization", icon = icon("object-group")),
      menuItem("Results Table", tabName = "resultsTable", icon = icon("table")),
      menuItem("Functional Variants", tabName = "functionalVariants", icon = icon("dna")),
      menuItem("Study Summary & Audio Guide", tabName = "studySummary", icon = icon("book-reader")),
      menuItem("How to Use the App", tabName = "howToUse", icon = icon("question-circle"))
      
    )
  ),
  dashboardBody(
    useShinyjs(),
    div(id = "loading-overlay", class = "loading-overlay",
        div(class = "loading-spinner")
    ),
    extendShinyjs(text = "
      var speech = null;
      shinyjs.speak = function(text) {
        if ('speechSynthesis' in window) {
          if(speech) {
            window.speechSynthesis.cancel();
          }
          speech = new SpeechSynthesisUtterance(text);
          speech.onend = function(event) {
            console.log('Speech finished');
          };
          speech.onerror = function(event) {
            console.error('Speech error:', event.error);
          };
          console.log('Starting speech');
          window.speechSynthesis.speak(speech);
        } else {
          console.error('Text-to-speech not supported');
          alert('Text-to-speech not supported in your browser');
        }
      }
      shinyjs.pause = function() {
        if ('speechSynthesis' in window) {
          console.log('Pausing speech');
          window.speechSynthesis.pause();
        }
      }
      shinyjs.resume = function() {
        if ('speechSynthesis' in window) {
          console.log('Resuming speech');
          window.speechSynthesis.resume();
        }
      }
      shinyjs.stop = function() {
        if ('speechSynthesis' in window) {
          console.log('Stopping speech');
          window.speechSynthesis.cancel();
          speech = null;
        }
      }
    ",
                  functions = c("speak", "pause", "resume", "stop")
    ),
    tags$head(
      tags$script(src="https://www.googletagmanager.com/gtag/js?id=G-KWLZ4KWXCM", async=NA),
      tags$script(HTML("
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());

      gtag('config', 'G-KWLZ4KWXCM');
    "))
    ),
    tags$title("Genomic Data Analysis") , # Sets the browser tab tit
    tabItems(
      # Visualization Tab
      tabItem(tabName = "visualization",
              fluidRow(
                box(title = "Controls", status = "primary", solidHeader = TRUE, width = 4,
                    class = "animate__animated animate__fadeInLeft",
                    tags$div(
                      class = "well",
                      style = "margin-bottom: 15px; background-color: #f8f9fa; border: 1px solid #e9ecef; border-radius: 5px; padding: 15px;",
                      tags$h4("Usage Tips", style = "margin-top: 0; color: #2c3e50;"),
                      tags$ul(
                        tags$li("You can select multiple SNP types and traits."),
                        tags$li("To deselect an item, click on it and press backspace or delete."),
                        tags$li("Hover over bars in the plot for more information.")
                      )
                    ),
                    selectInput("snpTypeInput", "Select SNP Type:", choices = NULL, multiple = TRUE),
                    bsTooltip("snpTypeInput", "You can select multiple SNP types. Click on a selected type to deselect it.", 
                              placement = "right", trigger = "hover"),
                    selectInput("traitInput", "Select Trait(s):", choices = NULL, multiple = TRUE),
                    bsTooltip("traitInput", "You can select multiple traits. Click on a selected trait to deselect it.", 
                              placement = "right", trigger = "hover"),
                    selectInput("metricInput", "Select Metric:", choices = c("cor" = "Correlation", "bias" = "Bias"))
                ),
                box(title = "Plot", status = "primary", solidHeader = TRUE, width = 8,
                    class = "animate__animated animate__fadeInRight",
                    div(class = "loading-spinner", id = "loading-metricPlot"),
                    uiOutput("dynamicMetricPlotOutput")
                )
              )
      ),
      # Grouped Visualization Tab
      tabItem(tabName = "groupedVisualization",
              fluidRow(
                box(title = "Controls", status = "primary", solidHeader = TRUE, width = 4,
                    class = "animate__animated animate__fadeInLeft",
                    selectInput("snpGroupInput", "Select SNP Group:", 
                                choices = c("Group 1" = "group1", 
                                            "Group 2" = "group2", 
                                            "Group 3" = "group3")),
                    selectInput("groupMetricInput", "Select Metric:", 
                                choices = c("cor" = "Correlation", "bias" = "Bias"))
                ),
                box(title = "Grouped Plot", status = "primary", solidHeader = TRUE, width = 8,
                    class = "animate__animated animate__fadeInRight",
                    div(class = "loading-spinner", id = "loading-groupedPlot"),
                    uiOutput("dynamicGroupedPlotOutput")
                )
              )),
      
      # Results Table Tab
      tabItem(tabName = "resultsTable",
              fluidRow(
                box(title = "Results Table", status = "primary", solidHeader = TRUE, width = 12,
                    class = "animate__animated animate__fadeInUp",
                    div(class = "loading-spinner", id = "loading-resultsTable"),
                    dataTableOutput("resultsTable")
                )
              )),
      
      # Functional Variants Tab
      tabItem(tabName = "functionalVariants",
              fluidRow(
                box(title = "Functional Variants Table", status = "primary", solidHeader = TRUE, width = 12,
                    class = "animate__animated animate__fadeInUp",
                    div(class = "loading-spinner", id = "loading-functionalVariantsTable"),
                    dataTableOutput("functionalVariantsTable")
                )
              )),
      
      # Study Summary & Audio Guide Tab
      tabItem(tabName = "studySummary",
              fluidRow(
                box(title = "Study Summary & Audio Guide", status = "primary", solidHeader = TRUE, width = 12,
                    class = "animate__animated animate__fadeInUp",
                    div(style = "margin-bottom: 20px;",
                        h4("Interactive Audio Controls", style = "color: #2c3e50;"),
                        div(style = "display: flex; gap: 10px; align-items: center;",
                            actionButton("readOverview", "Play All", icon = icon("play"), 
                                         class = "btn-primary"),
                            actionButton("pauseReading", "Pause", icon = icon("pause"), 
                                         class = "btn-info"),
                            actionButton("resumeReading", "Resume", icon = icon("step-forward"), 
                                         class = "btn-success"),
                            actionButton("stopReading", "Stop", icon = icon("stop"), 
                                         class = "btn-danger")
                        )
                    ),
                    div(id = "overviewText",
                        h3("Introduction", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                        div(style = "display: flex; align-items: center;",
                            actionButton("playIntro", "", icon = icon("play"), class = "btn-sm btn-link"),
                            p(id = "intro", class = "lead", "Genomic selection has become an integral part of modern dairy cattle breeding programs. While traditionally relying on markers from SNP chips, recent research suggests that incorporating causal variants could enhance the accuracy of genomic prediction. This study explores the potential of using functional variants identified through various methodologies to improve genomic prediction accuracy in dairy cattle.")
                        ),
                        h3("Study Objectives", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                        div(style = "display: flex; align-items: center;",
                            actionButton("playObj1", "", icon = icon("play"), class = "btn-sm btn-link"),
                            p(id = "obj1", class = "lead", "1. Assess the impact of functional sequence variants on genomic prediction accuracy for fat percentage, protein percentage, milk volume, fat yield, and protein yield in lactating dairy cattle.")
                        ),
                        div(style = "display: flex; align-items: center;",
                            actionButton("playObj2", "", icon = icon("play"), class = "btn-sm btn-link"),
                            p(id = "obj2", class = "lead", "2. Compare the genomic prediction accuracy obtained using each class of functional variants with matched numbers of SNPs randomly selected from the Illumina 50k SNP chip.")
                        ),
                        div(style = "display: flex; align-items: center;",
                            actionButton("playObj3", "", icon = icon("play"), class = "btn-sm btn-link"),
                            p(id = "obj3", class = "lead", "3. Evaluate the potential of using specific combinations of functional variants to achieve comparable or improved genomic prediction accuracy with fewer SNPs than traditional methods.")
                        ),
                        h3("Conclusion", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
                        div(style = "display: flex; align-items: center;",
                            actionButton("playConclusion", "", icon = icon("play"), class = "btn-sm btn-link"),
                            p(id = "conclusion", class = "lead", "The integration of functional variants showed modest improvements in genomic prediction accuracy for most traits studied. Carefully chosen combinations of functional variants provided comparable accuracies to larger SNP panels while using fewer SNPs. These findings suggest potential for more focused SNP panels in genomic prediction, though further research is needed to evaluate their broad applicability in practical breeding programs.")
                        )
                    )
                )
              )
      ),
      
      
      # New tab for "How to Use the App"
      tabItem(
        tabName = "howToUse",
        fluidRow(
          box(
            title = "How to Use the App", 
            status = "primary", 
            solidHeader = TRUE, 
            width = 12,
            style = "padding: 0;",  # Remove padding to make video full width
            tags$div(
              style = "position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden;",  # 16:9 aspect ratio
              tags$iframe(
                style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;",  # Full width and height
                src = "https://www.youtube.com/embed/AtdDHpjFF3U",  # Replace with your YouTube video ID
                frameborder = "0",
                allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
                allowfullscreen = TRUE
              )
            ),
            p("This video provides a step-by-step guide on how to use the app.")  # Optional description
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  # Show loading overlay
  shinyjs::show("loading-overlay")
  
  # Load the data
  data <- reactive({
    df <- read.table("www/Massey_LIC_paper_result.txt", # ## Note that please create directory www inside that put the result file Massey_LIC_paper_result.txt
                     header = TRUE, sep = " ", check.names = FALSE)
    df$trait <- factor(df$trait, levels = traitorder)
    df$SNPtype <- ifelse(df$SNPtype == "chipQTL", "chipseqQTL", df$SNPtype)
    df$SNPtype <- factor(df$SNPtype)
    df$Category <- factor(df$Category)
    df
  })
  
  # Update choices for SNP types, traits, and categories after data is loaded
  observe({
    req(data())
    snp_types <- unique(data()$SNPtype)
    traits <- unique(data()$trait)
    
    updateSelectInput(session, "snpTypeInput", choices = snp_types, selected = "GWAS")
    updateSelectInput(session, "traitInput", choices = traits, selected = "Protein_percent")
    
    # Hide loading overlay
    shinyjs::hide("loading-overlay")
  })
  
  ### New one 
  create_individual_plot <- function(data, x, y, error, color, title, y_title, is_grouped = FALSE, show_legend = FALSE) {
    y_range <- if(y_title == "Bias") c(0, 1.5) else c(0, 1)
    
    present_snp_types <- unique(data[[x]])
    
    p <- plot_ly(data, x = ~factor(get(x), levels = present_snp_types), y = ~get(y),
                 error_y = list(array = ~get(error), color = '#000000', thickness = 0.5, width = 2),
                 color = ~get(color), type = "bar",
                 colors = c("Functional_GS" = "#1f77b4", "Current_GS" = "#ff7f0e"),
                 showlegend = show_legend) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "SNP Type", 
                     tickangle = 45, 
                     tickfont = list(size = 12),  # Increase font size for x-axis labels
                     categoryorder = "array",
                     categoryarray = present_snp_types),
        yaxis = list(title = y_title, 
                     range = y_range,
                     tickformat = ".2f",
                     titlefont = list(size = 14)),  # Increase font size for y-axis title
        title = list(text = title, font = list(size = 14)),  # Increase title font size
        margin = list(t = 50, b = 100, l = 80, r = 20)  # Increase margins
      )
    
    return(p)
  }
  
  create_grouped_plot <- function(data, x, y, error, color, title, y_title) {
    y_range <- if (y_title == "Bias") c(0, 1.5) else c(0, 1)
    
    present_snp_types <- unique(data[[x]])
    
    p <- ggplot(data, aes(x = factor(get(x), levels = present_snp_types), y = get(y), fill = get(color))) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_errorbar(aes(ymin = get(y) - get(error), ymax = get(y) + get(error)), 
                    width = 0.2, position = position_dodge(0.9)) +
      scale_fill_manual(values = custom_palette) +
      labs(title = title, x = "SNP Type", y = y_title, fill = "Category") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 16, hjust = 0.5),
        legend.position = "bottom"
      ) +
      facet_wrap(~trait, scales = "free_x")  # Facet by trait to create subplots
    
    return(p)
  }
  metricPlot <- reactive({
    req(data(), input$snpTypeInput, input$traitInput, input$metricInput)
    
    filteredData <- data() %>%
      filter(SNPtype %in% input$snpTypeInput, trait %in% input$traitInput)
    
    y_col <- ifelse(input$metricInput == "Correlation", "cor", "bias")
    
    if (nrow(filteredData) == 0 || !(y_col %in% colnames(filteredData))) {
      shinyjs::hide("loading-metricPlot")
      return(plotly_empty())
    }
    
    plot_data <- filteredData %>%
      group_by(SNPtype, trait, Category) %>%
      summarise(
        cor_mean = mean(cor, na.rm = TRUE),
        cor_se = sd(cor, na.rm = TRUE) / sqrt(n()),
        bias_mean = mean(bias, na.rm = TRUE),
        bias_se = sd(bias, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop'
      )
    
    plot_list <- list()
    
    for (t in unique(plot_data$trait)) {
        # Debug: Print the trait name
      trait_data <- plot_data %>% filter(trait == t)
      plot_list[[t]] <- create_individual_plot(
        trait_data, 
        "SNPtype", 
        ifelse(y_col == "cor", "cor_mean", "bias_mean"),
        ifelse(y_col == "cor", "cor_se", "bias_se"),
        "Category", 
        t,
        input$metricInput,
        is_grouped = FALSE,
        show_legend = (t == unique(plot_data$trait)[1])
      )
    }
    
    n_plots <- length(plot_list)
    n_cols <- min(3, n_plots)
    n_rows <- ceiling(n_plots / n_cols)
    
    p <- subplot(plot_list, nrows = n_rows, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
      layout(
        title = list(text = paste("Distribution of", input$metricInput, "by SNP Type and Trait"),
                     x = 0.5, y = 0.99, font = list(size = 16)),
        showlegend = TRUE,
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.05, traceorder = "normal")
      ) %>%
      config(scrollZoom = FALSE, displayModeBar = FALSE)
    
    p$x$data <- unique(p$x$data)
    
    shinyjs::hide("loading-metricPlot")
    return(p)
  })
  
  output$metricPlot <- renderPlotly({
    p <- metricPlot()
    p %>% layout(autosize = TRUE)
  })
  
  output$groupedPlot <- renderPlot({
    req(data(), input$snpGroupInput, input$groupMetricInput)
    
    selected_group <- switch(input$snpGroupInput,
                             "group1" = group1_types,
                             "group2" = group2_types,
                             "group3" = group3_types)
    
    filteredData <- data() %>%
      filter(SNPtype %in% selected_group)
    
    y_col <- ifelse(input$groupMetricInput == "Correlation", "cor", "bias")
    
    if (nrow(filteredData) == 0 || !(y_col %in% colnames(filteredData))) {
      shinyjs::hide("loading-groupedPlot")
      return(ggplot())
    }
    
    
    plot_data <- filteredData %>%
      group_by(SNPtype, trait, Category) %>%
      summarise(
        cor_mean = mean(cor, na.rm = TRUE),
        cor_se = sd(cor, na.rm = TRUE) / sqrt(n()),
        bias_mean = mean(bias, na.rm = TRUE),
        bias_se = sd(bias, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop'
      )
    plot_list <- list()
    
    for (t in unique(plot_data$trait)) {
      trait_data <- plot_data %>% filter(trait == t)
      plot_list[[t]] <- create_grouped_plot(
        trait_data, 
        "SNPtype", 
        ifelse(y_col == "cor", "cor_mean", "bias_mean"),
        ifelse(y_col == "cor", "cor_se", "bias_se"),
        "Category", 
        t,
        input$groupMetricInput
      )
    }
    
    n_plots <- length(plot_list)
    n_cols <- min(3, n_plots)
    n_rows <- ceiling(n_plots / n_cols)
    
    p <- create_grouped_plot(
      plot_data, 
      "SNPtype", 
      ifelse(y_col == "cor", "cor_mean", "bias_mean"),
      ifelse(y_col == "cor", "cor_se", "bias_se"),
      "Category", 
      paste("Distribution of", input$groupMetricInput, "by SNP Type and Trait"),
      input$groupMetricInput
    )
    
    shinyjs::hide("loading-groupedPlot")
    return(p)
  })
  
  output$resultsTable <- renderDataTable({
    req(data())
    shinyjs::show("loading-resultsTable")
    
    tryCatch({
      summary_data <- data() %>%
        group_by(SNPtype, trait, Category) %>%
        summarise(
          `Genomic Prediction Accuracy` = mean(cor, na.rm = TRUE),
          `Standard Deviation` = sd(cor, na.rm = TRUE),
          `Genomic Prediction Accuracy with SD` = sprintf("%.3f ± %.3f", `Genomic Prediction Accuracy`, `Standard Deviation`),
          .groups = 'drop'
        ) %>%
        rename(
          `SNP Type` = SNPtype,
          Trait = trait
        )
      
      summary_data$`Genomic Prediction Accuracy` <- round(summary_data$`Genomic Prediction Accuracy`, 3)
      summary_data$`Standard Deviation` <- round(summary_data$`Standard Deviation`, 3)
      
      dt <- datatable(summary_data,
                      extensions = c('Buttons', 'FixedHeader'),
                      options = list(
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                        pageLength = 15,
                        scrollX = TRUE,
                        fixedHeader = TRUE,
                        processing = TRUE,
                        serverSide = FALSE,
                        order = list(list(0, 'asc'), list(1, 'asc'), list(2, 'asc'))
                      ),
                      filter = 'top',
                      rownames = FALSE
      ) %>%
        formatStyle(
          'SNP Type',
          backgroundColor = '#e6f3ff',
        ) %>%
        formatStyle(
          'Trait',
          backgroundColor = '#fff0f5',
        ) %>%
        formatStyle(
          'Category',
          backgroundColor = '#f0fff0',
        ) %>%
        formatStyle(
          'Genomic Prediction Accuracy',
          backgroundColor = styleInterval(c(0.3, 0.6), c('#FFF9C4', '#C8E6C9', '#BBDEFB')),
          color = styleInterval(c(0.3, 0.6), c('black', 'black', 'white'))
        ) %>%
        formatStyle(
          'Standard Deviation',
          backgroundColor = '#fff5ee'
        ) %>%
        formatStyle(
          'Genomic Prediction Accuracy with SD',
          backgroundColor = '#f5f5f5'
        )
      
      shinyjs::hide("loading-resultsTable")
      dt
    }, error = function(e) {
      shinyjs::hide("loading-resultsTable")
      output$dataTableError <- renderPrint({
        cat("Error in rendering DataTable:\n")
        print(e)
        cat("\nData summary:\n")
        print(summary(data()))
      })
      return(NULL)
    })
  })
  output$functionalVariantsTable <- renderDataTable({
    shinyjs::show("loading-functionalVariantsTable")
    
    tryCatch({
      dt <- datatable(functional_variant_data,
                      extensions = c('Buttons', 'FixedHeader'),
                      options = list(
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                        pageLength = 15,
                        scrollX = TRUE,
                        fixedHeader = TRUE,
                        processing = TRUE,
                        serverSide = FALSE,
                        order = list(list(0, 'asc'), list(1, 'asc'))
                      ),
                      filter = 'top',
                      rownames = FALSE
      ) %>%
        formatStyle(
          'Functional_class',
          backgroundColor = '#e6f3ff',
        ) %>%
        formatStyle(
          'Variant_type',
          backgroundColor = '#fff0f5',
        ) %>%
        formatStyle(
          'Initially_identified',
          backgroundColor = '#f0fff0'
        ) %>%
        formatStyle(
          'Before_filtering',
          backgroundColor = '#fff5ee'
        ) %>%
        formatStyle(
          'After_filtering',
          backgroundColor = '#f5f5f5'
        )
      
      shinyjs::hide("loading-functionalVariantsTable")
      dt
    }, error = function(e) {
      shinyjs::hide("loading-functionalVariantsTable")
      output$functionalVariantsError <- renderPrint({
        cat("Error in rendering Functional Variants Table:\n")
        print(e)
        cat("\nFunctional variant data summary:\n")
        print(summary(functional_variant_data))
      })
      return(NULL)
    })
  })
  text_content <- reactiveValues(
    intro = "Genomic selection has become an integral part of modern dairy cattle breeding programs. While traditionally relying on markers from SNP chips, recent research suggests that incorporating causal variants could enhance the accuracy of genomic prediction. This study explores the potential of using functional variants identified through various methodologies to improve genomic prediction accuracy in dairy cattle.",
    obj1 = "Assess the impact of functional sequence variants on genomic prediction accuracy for fat percentage, protein percentage, milk volume, fat yield, and protein yield in lactating dairy cattle.",
    obj2 = "Compare the genomic prediction accuracy obtained using each class of functional variants with matched numbers of SNPs randomly selected from the Illumina 50k SNP chip.",
    obj3 = "Evaluate the potential of using specific combinations of functional variants to achieve comparable or improved genomic prediction accuracy with fewer SNPs than traditional methods.",
    conclusion = "The integration of functional variants showed modest improvements in genomic prediction accuracy for most traits studied. Carefully chosen combinations of functional variants provided comparable accuracies to larger SNP panels while using fewer SNPs. These findings suggest potential for more focused SNP panels in genomic prediction, though further research is needed to evaluate their broad applicability in practical breeding programs."
  )
  
  observeEvent(input$readOverview, {
    overview_text <- paste(
      "Introduction.", text_content$intro,
      "Study Objectives.",
      "Objective 1:", text_content$obj1,
      "Objective 2:", text_content$obj2,
      "Objective 3:", text_content$obj3,
      "Conclusion.", text_content$conclusion
    )
    js$speak(overview_text)
  })
  
  observeEvent(input$pauseReading, {
    js$pause()
  })
  
  observeEvent(input$resumeReading, {
    js$resume()
  })
  
  observeEvent(input$stopReading, {
    js$stop()
  })
  
  observeEvent(input$playObj1, { 
    js$speak(paste("Objective 1:", text_content$obj1))
  })
  observeEvent(input$playObj2, { 
    js$speak(paste("Objective 2:", text_content$obj2))
  })
  observeEvent(input$playObj3, { 
    js$speak(paste("Objective 3:", text_content$obj3))
  })
  observeEvent(input$playIntro, { 
    js$speak(paste("Introduction:", text_content$intro))
  })
  observeEvent(input$playConclusion, { 
    js$speak(paste("Conclusion:", text_content$conclusion))
  })
  
  # Dynamic UI for plot outputs
  output$dynamicMetricPlotOutput <- renderUI({
    plotlyOutput("metricPlot", height = paste0(300 * ceiling(length(input$traitInput) / 3) + 100, "px"))
  })
  
  output$dynamicGroupedPlotOutput <- renderUI({
    n_traits <- length(unique(data()$trait))
    plot_height <- 300 * ceiling(n_traits / 3) + 100  # Adjust height based on number of traits
    plotOutput("groupedPlot", height = paste0(plot_height, "px"))
  })
}
# Run the application
shinyApp(ui = ui, server = server)
