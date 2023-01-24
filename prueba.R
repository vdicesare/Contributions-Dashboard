library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

# Import data
df <- fread ('https://zenodo.org/record/3891055/files/plos_contribution_data_set.csv?download=1')

# Create authors' positions variables
df$au_first <- ifelse(df$au_count == 1, 1, 0)

df$au_middle <- ifelse(df$au_count > 1 & df$au_count != df$n_authors, 1, 0)

df$au_last <- ifelse(df$au_count == df$n_authors, 1, 0)

# Remove n_authors = 1 and p_age outliers
df_filtered <- filter(df, n_authors > 1 & p_age > 0 & p_age < 50)

# Group contributions and positions by p_age
df_grouped <- aggregate(cbind(wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments, au_first, au_middle, au_last) ~ p_age,
                        data = df_filtered, FUN = sum)


### APP
# User interface
ui <- fluidPage(
  
  titlePanel("Contributions Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h5("Academic age"),
      sliderInput("age", min = 1, max = 25, value = c(10, 15)),
      h5("Contribution type"),
      checkboxInput("contribution_WR", label = "Wrote paper", value = TRUE),
      checkboxInput("contribution_AD", label = "Analyzed data", value = TRUE),
      checkboxInput("contribution_CE", label = "Conceived experiments", value = TRUE),
      checkboxInput("contribution_CT", label = "Contributed tools", value = TRUE),
      checkboxInput("contribution_PE", label = "Performed experiments", value = TRUE),
      h5("Author position"),
      checkboxInput("position_first", label = "1st author", value = TRUE),
      checkboxInput("position_middle", label = "2nd/middle author", value = TRUE),
      checkboxInput("position_last", label = "Last author", value = TRUE)),
    
    mainPanel(
      
      plotOutput("contributions_dashboard")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  output$contributions_dashboard <- renderPlot({
    gather(data = df_grouped[which(df_grouped$p_age >= input$age[1] & df_grouped$p_age <= input$age[2]),],
           key, value, wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments, au_first, au_middle, au_last) %>%
      
      {if (!input$contribution_WR) filter(., key!='wrote_paper') else filter(.)} %>%
      {if (!input$contribution_AD) filter(., key!='analyzed_data') else filter(.)} %>%
      {if (!input$contribution_CE) filter(., key!='conceived_experiments') else filter(.)} %>%
      {if (!input$contribution_CT) filter(., key!='contributed_tools') else filter(.)} %>%
      {if (!input$contribution_PE) filter(., key!='performed_experiments') else filter(.)} %>%
      {if (!input$position_first) filter(., key!='au_first') else filter(.)} %>%
      {if (!input$position_middle) filter(., key!='au_middle') else filter(.)} %>%
      {if (!input$position_last) filter(., key!='au_last') else filter(.)} %>%
      
      ggplot(aes(x = p_age, y = value, colour = key, linetype = key)) +
      geom_line() +
      xlab("Age") +
      ylab("Count") +
      scale_colour_discrete("References", breaks = c("wrote_paper", "analyzed_data", "conceived_experiments", "contributed_tools", "performed_experiments", "au_first", "au_middle", "au_last"),
                            labels = c("Wrote paper", "Analyzed data", "Conceived experiments", "Contributed tools", "Performed experiments", "1st author", "2nd/middle author", "Last author")) +
      scale_linetype_manual("References", values = c("solid", "solid", "solid", "solid", "solid", "dashed", "dashed", "dashed"),
                            breaks = c("wrote_paper", "analyzed_data", "conceived_experiments", "contributed_tools", "performed_experiments", "au_first", "au_middle", "au_last"),
                            labels = c("Wrote paper", "Analyzed data", "Conceived experiments", "Contributed tools", "Performed experiments", "1st author", "2nd/middle author", "Last author"))
  })
}

# Run app
shinyApp(ui = ui, server = server)
