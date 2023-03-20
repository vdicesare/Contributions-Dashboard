library(curl)
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

# Group contributions by p_age and au_position
df_grouped_authors_position <- aggregate(cbind(wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments) ~ au_position + n_authors,
                                     data = df_filtered, FUN = sum)

# Compute proportion of authors in each contribution category per n_authors in df_grouped_authors_position
df_grouped_authors_position$total_cases <- df_grouped_authors_position$wrote_paper + df_grouped_authors_position$analyzed_data + df_grouped_authors_position$conceived_experiments + df_grouped_authors_position$contributed_tools + df_grouped_authors_position$performed_experiments
df_grouped_authors_position$proportion_WR <- df_grouped_authors_position$wrote_paper / df_grouped_authors_position$total_cases
df_grouped_authors_position$proportion_AD <- df_grouped_authors_position$analyzed_data / df_grouped_authors_position$total_cases
df_grouped_authors_position$proportion_CE <- df_grouped_authors_position$conceived_experiments / df_grouped_authors_position$total_cases
df_grouped_authors_position$proportion_CT <- df_grouped_authors_position$contributed_tools / df_grouped_authors_position$total_cases
df_grouped_authors_position$proportion_PE <- df_grouped_authors_position$performed_experiments / df_grouped_authors_position$total_cases

# Group contributions by p_age and au_position
df_grouped_age_position <- aggregate(cbind(wrote_paper, analyzed_data, conceived_experiments, contributed_tools, performed_experiments) ~ au_position + p_age,
                                     data = df_filtered, FUN = sum)

# Compute proportion of authors in each contribution category per age and position in df_grouped_age_position
df_grouped_age_position$total_cases <- df_grouped_age_position$wrote_paper + df_grouped_age_position$analyzed_data + df_grouped_age_position$conceived_experiments + df_grouped_age_position$contributed_tools + df_grouped_age_position$performed_experiments
df_grouped_age_position$proportion_WR <- df_grouped_age_position$wrote_paper / df_grouped_age_position$total_cases
df_grouped_age_position$proportion_AD <- df_grouped_age_position$analyzed_data / df_grouped_age_position$total_cases
df_grouped_age_position$proportion_CE <- df_grouped_age_position$conceived_experiments / df_grouped_age_position$total_cases
df_grouped_age_position$proportion_CT <- df_grouped_age_position$contributed_tools / df_grouped_age_position$total_cases
df_grouped_age_position$proportion_PE <- df_grouped_age_position$performed_experiments / df_grouped_age_position$total_cases


### APP
# User interface
ui <- navbarPage(
  
  # Application title
  theme = shinytheme("lumen"),
  title="Contributions Types in Science",
  
  tabPanel("By number of authors and position",
           h2("Contribution type by number of authors and position"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("number_authors", label = h4("Number of authors"), min = 2, max = 46, value = c(15, 30)),
               h4("Contribution type"),
               checkboxInput("contribution_WR2", label = "Wrote paper", value = TRUE),
               checkboxInput("contribution_AD2", label = "Analyzed data", value = TRUE),
               checkboxInput("contribution_CE2", label = "Conceived experiments", value = TRUE),
               checkboxInput("contribution_CT2", label = "Contributed tools", value = TRUE),
               checkboxInput("contribution_PE2", label = "Performed experiments", value = TRUE)),
             
             mainPanel(
               plotOutput("contribution_authors_position")
             )
           ),
           
           hr(),
           helpText(HTML("By Victoria Di Césare & Nicolás Robinson-García | <a href='https://github.com/VictoriaDiCesare/Contributions-Dashboard'>GitHub</a>"))),
  
  tabPanel("By age and author position",
           h2("Contribution type by age and author position"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("age", label = h4("Academic age"), min = 1, max = 25, value = c(10, 15)),
               h4("Contribution type"),
               checkboxInput("contribution_WR3", label = "Wrote paper", value = TRUE),
               checkboxInput("contribution_AD3", label = "Analyzed data", value = TRUE),
               checkboxInput("contribution_CE3", label = "Conceived experiments", value = TRUE),
               checkboxInput("contribution_CT3", label = "Contributed tools", value = TRUE),
               checkboxInput("contribution_PE3", label = "Performed experiments", value = TRUE)),
             
             mainPanel(
               plotOutput("contribution_age_position")
             )
           ),
           
           hr(),
           helpText(HTML("By Victoria Di Césare & Nicolás Robinson-García | <a href='https://github.com/VictoriaDiCesare/Contributions-Dashboard'>GitHub</a>"))),
  
  tabPanel("About",
           h2("About"),
           fluidRow(column(width=12,
                           HTML("The visualizations available here were developed using the <a href='https://zenodo.org/record/3891055/#.Y9jsEOzMLos'><b>dataset 1 on PLOS contribution and bibliometric data</b></a>, as part of the project <a href='https://compare-project.eu/about/'><b>COntextual Mapping of academic Pathways Analysis for Research Evaluation (COMPARE)</b></a> – PID2020-117007RA-I00."),
                           h2("Authors"),
                           fluidRow(
                             column(width=6,
                                    h3("Victoria Di Césare"),
                                    HTML("<a href='https://sites.google.com/view/vdicesare'>Victoria Di Césare</a> is a PhD student in Social Sciences at the University of Granada (Spain), supported by a FPI fellowship awarded by the Spanish Ministry of Science and Innovation. She is a member of the scientific teams at EC3 Research Group and COMPARE Project. Her academic interests focus on Scientometrics and Social Studies of Science, specifically in relation to the theoretical and empirical characterization of science as a social activity.")),
                             column(width=6,
                                    h3("Nicolás Robinson-García"),
                                    HTML("<a href='https://nrobinsongarcia.com/'>Nicolás Robinson-García</a> is a researcher in the field of bibliometrics and research evaluation. He currently enjoys a Ramón y Cajal grant at the University of Granada (Spain). He worked previously as a Marie Sklodowska-Curie Fellow at Delft Institute of Applied Mathematics, TU Delft (Netherlands), the School of Public Policy at Georgia Institute of Technology and at INGENIO (CSIC-UPV) in Spain. He holds a PhD on Social Sciences at the University of Granada. He is member of the Steering Committee of the European Summer School for Scientometrics. He is Associate Editor on Open Science and New Metrics of the journal Scientometrics and member of the editorial board of Research Evaluation and Quantitative Science Studies."))),
                           fluidRow(column(width=12,
                                           tags$div(
                                             tags$img(src = 'https://raw.githubusercontent.com/Wences91/science-of-humanities/main/images/compare_logo.png', width=150, style='padding-right:25px'),
                                             tags$img(src = 'https://raw.githubusercontent.com/Wences91/science-of-humanities/main/images/ugr_logo.png', height=50, style='padding-right:25px'),
                                             style='padding-top:75px;text-align:right')))
           )
           ),
           
           hr(),
           helpText(HTML("By Victoria Di Césare & Nicolás Robinson-García | <a href='https://github.com/VictoriaDiCesare/Contributions-Dashboard'>GitHub</a>")))
  
) 

# Server logic
server <- function(input, output) {
  
  output$contribution_authors_position <- renderPlot({
    gather(data = df_grouped_authors_position[which(df_grouped_authors_position$n_authors >= input$number_authors[1] & df_grouped_authors_position$n_authors <= input$number_authors[2]),],
           key, value, proportion_WR, proportion_AD, proportion_CE, proportion_CT, proportion_PE) %>%
      
      {if (!input$contribution_WR2) filter(., key!='proportion_WR') else filter(.)} %>%
      {if (!input$contribution_AD2) filter(., key!='proportion_AD') else filter(.)} %>%
      {if (!input$contribution_CE2) filter(., key!='proportion_CE') else filter(.)} %>%
      {if (!input$contribution_CT2) filter(., key!='proportion_CT') else filter(.)} %>%
      {if (!input$contribution_PE2) filter(., key!='proportion_PE') else filter(.)} %>%
      
      ggplot(aes(x = au_position, y = value, fill = key)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      xlab("Author position") +
      ylab("Proportion") +
      scale_fill_discrete("References", breaks = c("proportion_WR", "proportion_AD", "proportion_CE", "proportion_CT", "proportion_PE"),
                          labels = c("Wrote paper", "Analyzed data", "Conceived experiments", "Contributed tools", "Performed experiments"))
  })
  
  output$contribution_age_position <- renderPlot({
    gather(data = df_grouped_age_position[which(df_grouped_age_position$p_age >= input$age[1] & df_grouped_age_position$p_age <= input$age[2]),],
           key, value, proportion_WR, proportion_AD, proportion_CE, proportion_CT, proportion_PE) %>%
      
      {if (!input$contribution_WR3) filter(., key!='proportion_WR') else filter(.)} %>%
      {if (!input$contribution_AD3) filter(., key!='proportion_AD') else filter(.)} %>%
      {if (!input$contribution_CE3) filter(., key!='proportion_CE') else filter(.)} %>%
      {if (!input$contribution_CT3) filter(., key!='proportion_CT') else filter(.)} %>%
      {if (!input$contribution_PE3) filter(., key!='proportion_PE') else filter(.)} %>%
      
      ggplot(aes(x = au_position, y = value, fill = key)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      xlab("Author position") +
      ylab("Proportion") +
      scale_fill_discrete("References", breaks = c("proportion_WR", "proportion_AD", "proportion_CE", "proportion_CT", "proportion_PE"),
                          labels = c("Wrote paper", "Analyzed data", "Conceived experiments", "Contributed tools", "Performed experiments"))
  })
}

# Run app
shinyApp(ui = ui, server = server)