library(shiny)
library(shinythemes)
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

df$au_position <- ifelse(df$au_count == 1, "1st author",
                         ifelse(df$au_count > 1 & df$au_count != df$n_authors, "2nd/middle author",
                                ifelse(df$au_count == df$n_authors, "Last author", NA)))

# Remove n_authors = 1 and p_age outliers
df_filtered <- filter(df, n_authors > 1 & p_age > 0 & p_age < 50)

# Group contributions and positions by p_age
df_grouped_age <- aggregate(cbind(wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments, au_first, au_middle, au_last) ~ p_age,
                            data = df_filtered, FUN = sum)

# Group contributions and positions by n_authors
df_grouped_number_authors <- aggregate(cbind(wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments, au_first, au_middle, au_last) ~ n_authors,
                                       data = df_filtered, FUN = sum)

# Group contributions by au_position
df_grouped_position <- aggregate(cbind(wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments) ~ au_position,
                                       data = df_filtered, FUN = sum)


### APP
# User interface
ui <- navbarPage(
  
  # Application title
  theme = shinytheme("lumen"),
  title="Contributions Types in Science",
  
  # Tab panels
  tabPanel("By age",
  h2("Contribution type and author position by academic age"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", label = h4("Academic age"), min = 1, max = 25, value = c(10, 15)),
      h4("Contribution type"),
      checkboxInput("contribution_WR1", label = "Wrote paper", value = TRUE),
      checkboxInput("contribution_AD1", label = "Analyzed data", value = TRUE),
      checkboxInput("contribution_CE1", label = "Conceived experiments", value = TRUE),
      checkboxInput("contribution_CT1", label = "Contributed tools", value = TRUE),
      checkboxInput("contribution_PE1", label = "Performed experiments", value = TRUE),
      h4("Author position"),
      checkboxInput("position_first1", label = "1st author", value = TRUE),
      checkboxInput("position_middle1", label = "2nd/middle author", value = TRUE),
      checkboxInput("position_last1", label = "Last author", value = TRUE)),
    
    mainPanel(
      plotOutput("contribution_position_age")
      )
    )
  ),

  tabPanel("By number of authors",
  h2("Contribution type and author position by number of authors"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("number_authors", label = h4("Number of authors"), min = 2, max = 46, value = c(15, 30)),
      h4("Contribution type"),
      checkboxInput("contribution_WR2", label = "Wrote paper", value = TRUE),
      checkboxInput("contribution_AD2", label = "Analyzed data", value = TRUE),
      checkboxInput("contribution_CE2", label = "Conceived experiments", value = TRUE),
      checkboxInput("contribution_CT2", label = "Contributed tools", value = TRUE),
      checkboxInput("contribution_PE2", label = "Performed experiments", value = TRUE),
      h4("Author position"),
      checkboxInput("position_first2", label = "1st author", value = TRUE),
      checkboxInput("position_middle2", label = "2nd/middle author", value = TRUE),
      checkboxInput("position_last2", label = "Last author", value = TRUE)),
             
    mainPanel(
      plotOutput("contribution_position_number_authors")
      )
    )
  ),
  
  tabPanel("By author position",
  h2("Contribution type by author position"),
  sidebarLayout(
    sidebarPanel(
      h4("Contribution type"),
      checkboxInput("contribution_WR3", label = "Wrote paper", value = TRUE),
      checkboxInput("contribution_AD3", label = "Analyzed data", value = TRUE),
      checkboxInput("contribution_CE3", label = "Conceived experiments", value = TRUE),
      checkboxInput("contribution_CT3", label = "Contributed tools", value = TRUE),
      checkboxInput("contribution_PE3", label = "Performed experiments", value = TRUE)),
             
    mainPanel(
     plotOutput("contribution_position")
     )
    )
  ),

  tabPanel("About",
  h2("About"),
  fluidRow(column(width=12,
    HTML("The visualizations and data available here are part of the work <a href='https://doi.org/10.5281/zenodo.6974732'><b>The relation between contribution statements and academic age</b></a>, developed as part of the project <a href='https://compare-project.eu/about/'><b>COntextual Mapping of academic Pathways Analysis for Research Evaluation (COMPARE)</b></a> – PID2020-117007RA-I00."),
    HTML("Preliminary results were presented at the <i>26th International Conference on Science and Technology Indicators: From Local Indicators to Global Applications</i> organized by the Faculty of Communication and Documentation (UGR) in collaboration with the European Network of Indicator Developers (ENID), which took place on 7-9th September, 2022, in Granada."),
  h2("Authors"),
  fluidRow(
    column(width=6,
      h3("Manuel Escabias"),
        HTML("Completar")),
    column(width=6,
      h3("Nicolás Robinson-García"),
        HTML('Nicolás Robinson-García is a researcher in the field of bibliometrics and research evaluation. He currently enjoys a Ramón y Cajal grant at the University of Granada (Spain). He worked previously as a Marie Sklodowska-Curie Fellow at Delft Institute of Applied Mathematics, TU Delft (Netherlands), the School of Public Policy at Georgia Institute of Technology and at INGENIO (CSIC-UPV) in Spain. He holds a PhD on Social Sciences at the University of Granada. He is member of the Steering Committee of the European Summer School for Scientometrics. He is Associate Editor on Open Science and New Metrics of the journal Scientometrics and member of the editorial board of Research Evaluation and Quantitative Science Studies.'))),
  fluidRow(column(width=12,
    tags$div(
      tags$img(src = 'https://raw.githubusercontent.com/Wences91/science-of-humanities/main/images/compare_logo.png', width=150, style='padding-right:25px'),
      tags$img(src = 'https://raw.githubusercontent.com/Wences91/science-of-humanities/main/images/ugr_logo.png', height=50, style='padding-right:25px'),
      style='padding-top:75px;text-align:right')))
      )
    )
  )
) 

# Server logic
server <- function(input, output) {
  
  output$contribution_position_age <- renderPlot({
    gather(data = df_grouped_age[which(df_grouped_age$p_age >= input$age[1] & df_grouped_age$p_age <= input$age[2]),],
           key, value, wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments, au_first, au_middle, au_last) %>%
      
      {if (!input$contribution_WR1) filter(., key!='wrote_paper') else filter(.)} %>%
      {if (!input$contribution_AD1) filter(., key!='analyzed_data') else filter(.)} %>%
      {if (!input$contribution_CE1) filter(., key!='conceived_experiments') else filter(.)} %>%
      {if (!input$contribution_CT1) filter(., key!='contributed_tools') else filter(.)} %>%
      {if (!input$contribution_PE1) filter(., key!='performed_experiments') else filter(.)} %>%
      {if (!input$position_first1) filter(., key!='au_first') else filter(.)} %>%
      {if (!input$position_middle1) filter(., key!='au_middle') else filter(.)} %>%
      {if (!input$position_last1) filter(., key!='au_last') else filter(.)} %>%
      
      ggplot() +
      geom_line(aes(x = p_age, y = value, colour = key, linetype = key)) +
      theme_minimal() +
      xlab("Age") +
      ylab("Count") +
      scale_colour_discrete("References", breaks = c("wrote_paper", "analyzed_data", "conceived_experiments", "contributed_tools", "performed_experiments", "au_first", "au_middle", "au_last"),
                            labels = c("Wrote paper", "Analyzed data", "Conceived experiments", "Contributed tools", "Performed experiments", "1st author", "2nd/middle author", "Last author")) +
      scale_linetype_manual("References", values = c("solid", "solid", "solid", "solid", "solid", "dashed", "dashed", "dashed"),
                            breaks = c("wrote_paper", "analyzed_data", "conceived_experiments", "contributed_tools", "performed_experiments", "au_first", "au_middle", "au_last"),
                            labels = c("Wrote paper", "Analyzed data", "Conceived experiments", "Contributed tools", "Performed experiments", "1st author", "2nd/middle author", "Last author"))
  })

  output$contribution_position_number_authors <- renderPlot({
    gather(data = df_grouped_number_authors[which(df_grouped_number_authors$n_authors >= input$number_authors[1] & df_grouped_number_authors$n_authors <= input$number_authors[2]),],
           key, value, wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments, au_first, au_middle, au_last) %>%
      
      {if (!input$contribution_WR2) filter(., key!='wrote_paper') else filter(.)} %>%
      {if (!input$contribution_AD2) filter(., key!='analyzed_data') else filter(.)} %>%
      {if (!input$contribution_CE2) filter(., key!='conceived_experiments') else filter(.)} %>%
      {if (!input$contribution_CT2) filter(., key!='contributed_tools') else filter(.)} %>%
      {if (!input$contribution_PE2) filter(., key!='performed_experiments') else filter(.)} %>%
      {if (!input$position_first2) filter(., key!='au_first') else filter(.)} %>%
      {if (!input$position_middle2) filter(., key!='au_middle') else filter(.)} %>%
      {if (!input$position_last2) filter(., key!='au_last') else filter(.)} %>%
      
      ggplot() +
      geom_line(aes(x = n_authors, y = value, colour = key, linetype = key)) +
      theme_minimal() +
      xlab("Number of authors") +
      ylab("Count") +
      scale_colour_discrete("References", breaks = c("wrote_paper", "analyzed_data", "conceived_experiments", "contributed_tools", "performed_experiments", "au_first", "au_middle", "au_last"),
                            labels = c("Wrote paper", "Analyzed data", "Conceived experiments", "Contributed tools", "Performed experiments", "1st author", "2nd/middle author", "Last author")) +
      scale_linetype_manual("References", values = c("solid", "solid", "solid", "solid", "solid", "dashed", "dashed", "dashed"),
                            breaks = c("wrote_paper", "analyzed_data", "conceived_experiments", "contributed_tools", "performed_experiments", "au_first", "au_middle", "au_last"),
                            labels = c("Wrote paper", "Analyzed data", "Conceived experiments", "Contributed tools", "Performed experiments", "1st author", "2nd/middle author", "Last author"))
  })

  output$contribution_position <- renderPlot({
    gather(data = df_grouped_position, key, value, wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments) %>%
      
      {if (!input$contribution_WR3) filter(., key!='wrote_paper') else filter(.)} %>%
      {if (!input$contribution_AD3) filter(., key!='analyzed_data') else filter(.)} %>%
      {if (!input$contribution_CE3) filter(., key!='conceived_experiments') else filter(.)} %>%
      {if (!input$contribution_CT3) filter(., key!='contributed_tools') else filter(.)} %>%
      {if (!input$contribution_PE3) filter(., key!='performed_experiments') else filter(.)} %>%
      
      ggplot(aes(x = au_position, y = value, fill = key)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      xlab("Author position") +
      ylab("Count") +
      scale_fill_discrete("References", breaks = c("wrote_paper", "analyzed_data", "conceived_experiments", "contributed_tools", "performed_experiments"),
                            labels = c("Wrote paper", "Analyzed data", "Conceived experiments", "Contributed tools", "Performed experiments"))
  })
}

# Run app
shinyApp(ui = ui, server = server)
