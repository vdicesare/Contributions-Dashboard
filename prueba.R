library(shiny)
library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)

# Import data
df <- fread ('https://zenodo.org/record/3891055/files/plos_contribution_data_set.csv?download=1')

# Create au_position variable
df$au_position <- ifelse(df$au_count == 1, "1st author",
                         ifelse(df$au_count > 1 & df$au_count != df$n_authors, "2nd/middle author",
                                ifelse(df$au_count == df$n_authors, "Last author", NA)
                                )
                         )

# Convert au_position variable to factor
df$au_position <- as.factor(df$au_position)

# Remove n_authors = 1 and p_age outliers
df_filtered <- filter(df, n_authors > 1 & p_age > 0 & p_age < 50)

# Group contributions by p_age
df_grouped_contributions <- aggregate(cbind(wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments) ~ p_age,
                        data = df_filtered, FUN = sum)

# Group au_position by p_age
df_grouped_positions <- df_filtered %>% group_by(p_age, au_position) %>% 
  summarise(cases = n(),.groups = 'drop') %>%
  as.data.frame()

### APP
# User interface
ui <- fluidPage(
  
  titlePanel("Contributions Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h5("Academic age"),
      sliderInput("age", label = "Academic age", min = 1, max = 25, value = c(10, 15)),
      h5("Contribution type"),
      checkboxInput("contribution_WR", label = "Wrote paper", value = TRUE),
      checkboxInput("contribution_AD", label = "Analyzed data", value = TRUE),
      checkboxInput("contribution_CE", label = "Conceived experiments", value = TRUE),
      checkboxInput("contribution_CT", label = "Contributed tools", value = TRUE),
      checkboxInput("contribution_PE", label = "Performed experiments", value = TRUE)),
    
    mainPanel(
      
      plotOutput("contributionByAge")
    )
  )
)

# Server logic
server <- function(input, output) {
  
  output$contributionByAge <- renderPlot({
    gather(data = df_grouped_contributions[which(df_grouped_contributions$p_age >= input$age[1] & df_grouped_contributions$p_age <= input$age[2]),], key, value, wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments) %>%
      
      {if (!input$contribution_WR) filter(., key!='wrote_paper') else filter(.)} %>%
      {if (!input$contribution_AD) filter(., key!='analyzed_data') else filter(.)} %>%
      {if (!input$contribution_CE) filter(., key!='conceived_experiments') else filter(.)} %>%
      {if (!input$contribution_CT) filter(., key!='contributed_tools') else filter(.)} %>%
      {if (!input$contribution_PE) filter(., key!='performed_experiments') else filter(.)} %>%
      
      ggplot(aes(x = p_age, y = value, colour = key)) +
      geom_line()
  })
}

# Run app
shinyApp(ui = ui, server = server)

### Add authors' positions widget

gather(data = dfByAgeClean, key, value, wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments) %>%
  ggplot(aes(x = p_age, y = value, colour = key)) +
  geom_line()
###
