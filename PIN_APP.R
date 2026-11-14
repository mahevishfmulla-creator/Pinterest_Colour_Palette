library(shiny)
library(jpeg)
library(ggplot2)

ui <- fluidPage(
  # Custom CSS for that dark-mode glassmorphism cyber-aesthetic vibe
  tags$head(
    tags$style(HTML("
      body {
        background-color: #0b0f19;
        color: #e2e8f0;
        font-family: 'SF Pro Display', -apple-system, sans-serif;
      }
      .well {
        background: rgba(30, 41, 59, 0.7);
        border: 1px solid rgba(255, 255, 255, 0.1);
        backdrop-filter: blur(12px);
        border-radius: 16px;
        box-shadow: 0 8px 32px 0 rgba(0, 0, 0, 0.37);
      }
      h2 {
        color: #f472b6;
        font-weight: 700;
        letter-spacing: -0.5px;
      }
      .shiny-input-container {
        color: #f1f5f9;
      }
    "))
  ),
  
  titlePanel("✨ Pinterest Aesthetic Color Extractor"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Moodboard Controls", style = "color: #cbd5e1; border-bottom: 1px solid rgba(255,255,255,0.1); padding-bottom: 8px;"),
      
      # File uploader for any image (.jpg or .jpeg)
      fileInput("moodboard_file", "Upload Moodboard Image (.jpg):",
                accept = c('.jpg', '.jpeg')),
      
      # Interactive slider for number of colors (clusters)
      sliderInput("num_clusters", "Palette Size (Clusters):", 
                  min = 3, max = 8, value = 5),
      
      p("Upload any Y2K or moodboard image to instantly extract its dominant aesthetic hex palette using K-Means machine learning.", 
        style = "font-size: 12px; color: #94a3b8; margin-top: 20px;")
    ),
    
    mainPanel(
      wellPanel(
        style = "background: rgba(15, 23, 42, 0.8); border: 1px solid rgba(255,255,255,0.08);",
        h3("Generated Vibe Palette", style = "color: #f472b6; font-size: 20px; text-align: center; margin-bottom: 20px;"),
        plotOutput("palette_plot", height = "350px")
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$palette_plot <- renderPlot({
    req(input$moodboard_file)
    
    # 1. Read the user-uploaded image dynamically
    img <- readJPEG(input$moodboard_file$datapath)
    
    img <- readJPEG("moodboard.jpeg")
    rgb <- data.frame(
      R = as.vector(img[,,1]),
      G = as.vector(img[,,2]),
      B = as.vector(img[,,3])
    )
    set.seed(42)
    k_result <- kmeans(rgb, centers = 5, iter.max = 20)
    palette <- k_result$centers
    palette_df <- data.frame(
      cluster = factor(1:5),
      R = palette[, "R"],
      G = palette[, "G"],
      B = palette[, "B"],
      hex = rgb(palette)
    )
    
    result <- ggplot(data =palette_df, aes(x = cluster, y = 1, fill = hex) ) + 
      geom_tile(color = "white", linewidth = 1) +
      scale_fill_identity() +
      theme_minimal() +
      labs(title = "Pinterest Moodboard Vibe Palette", x = "", y = "")
    print(result)
    
  })
}

shinyApp(ui = ui, server = server)