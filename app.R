#
# shiny app exploring fear of earthquakes by US region
# visualize worry response with column chart
# simple table showing proportion of regional populations 
# that have experienced an earthquake


# shiny app exploring fear of earthquakes by US region

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinybusy)

#library(fivethirtyeight)
# I don't think I need to open the entire 538 library
san_andreas <- fivethirtyeight::san_andreas

# clean and update the san andreas data
# add sex column based on female
# changed prepared column from logical to character
# simplify past experience column
san_andreas <- san_andreas |> 
  dplyr::mutate(sex = case_when(female == TRUE ~ "female",
                                female == FALSE ~ "male"),
                sex = as.factor(sex),
                prepared2 = case_when(prepared == TRUE ~ "yes",
                                      prepared == FALSE ~ "no"),
                prepared2 = as.factor(prepared2),
                experience2 = case_when(experience == "No" ~ "No",
                                        experience == "Yes, one or more minor ones" ~ "Yes (minor)",
                                        experience == "Yes, one or more major ones" ~ "Yes (major)"),
                experience2 = factor(experience2, levels = c("No", "Yes (minor)", "Yes (major)"), ordered = TRUE),
                'household income' = hhold_income)

san_andreas_traits <- san_andreas |> 
  select(age, `household income`, region, sex)

# missing data by columns
#sapply(san_andreas, function(x) sum(is.na(x)))

#attach(san_andreas)

# Define UI for application that draws a histogram
ui <- navbarPage(theme = "yeti",
                 title = NULL,
                 
                 # first panel: general earthquake worry
                 tabPanel(("Worry"),
                           # Application title
                          titlePanel("Earthquake Worry by U.S. Region"),

                          # Sidebar with a slider input for number of bins 
                          # this function doesn't rely on calling a data object
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(inputId = "selected.worry",
                                          label = "Select type of earthquake worry for plot",
                                          selected = "General",
                                          choices = c("General", "The Big One"),
                                          multiple = FALSE),
                              p("'The Big One' refers to a massive, catastrophic earthquake."),
                              hr(),
                              # select regions for table
                              selectInput(inputId = "selected.regions",
                                          label = "Select regions for table",
                                          selected = unique(san_andreas$region)[1:2],
                                          choices = unique(san_andreas$region),
                                          multiple = TRUE),
                              p(tags$a(href = "https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf",
                                       "Statistical grouping of states as defined by the US Census Bureau.")),
                              p(tags$a(href = "https://github.com/fivethirtyeight/data/tree/master/san-andreas",
                                       "Data sourced from fivethirtyeight R package."))
                              ),
                            # Show a plot of the generated distribution
                            mainPanel(
                              add_busy_spinner(spin = "half-circle",
                                               position = "top-right"),
                              plotOutput(outputId = "responsePlot1", width = "100%"),
                              hr(),
                              DT::dataTableOutput("responseTable2")
                              )
                            )
                          ),
                 
                 # second panel: preparedness
                 tabPanel(("Preparedness"),
                          # application title
                          titlePanel("Earthquake Preparedness in Response to Past Experience"),
                          sidebarLayout(
                            sidebarPanel(
                              varSelectInput(inputId = "selected.trait",
                                    label = "Select trait",
                                    data = san_andreas_traits,
                                    selected = "age"),
                              hr(),
                              p(tags$a(href = "https://www2.census.gov/geo/pdfs/reference/GARM/Ch6GARM.pdf",
                              "Statistical grouping of states as defined by the US Census Bureau.")),
                              p(tags$a(href = "https://github.com/fivethirtyeight/data/tree/master/san-andreas",
                                       "Data sourced from fivethirtyeight R package."))
                              ),
                            # Show a plot of the generated distribution
                            mainPanel(
                              add_busy_spinner(spin = "half-circle",
                                               position = "top-right"),
                              plotOutput(outputId = "responsePlot2", width = "100%"),
                              hr(),
                              DT::dataTableOutput("responseTable3")
                              )
                            )
                          )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # make a column plot of the proportion of general worry responses by region
  # use selected.regions.data2
  output$responsePlot1 <- renderPlot({
    
    worry <- sym(req(input$selected.worry))
    
    df2 <- san_andreas |> 
      rename("General" = worry_general) |> 
      rename("The Big One" = worry_bigone) |>
      # select simplified experience2
      select(region, experience2, !!worry) |> 
      # remove NA values 
      dplyr::filter(region != is.na(region)) |> 
      dplyr::filter(!!worry != is.na(!!worry))
    
    ggplot(df2,
           aes(x = !!worry)) +
      geom_bar(aes(fill = experience2)) +
      coord_flip() +
      theme_bw() +
      theme(legend.position = "top") +
      facet_wrap(vars(region), ncol = 3) +
      xlab("Worry Intensity") +
      ylab("Worry Response Count") +
      guides(fill = guide_legend(title = "Have you ever experienced an earthquake?",
                                 title.position = "top")) +
      theme(legend.position = "top",
            axis.text.x = element_text(face = "bold", size = 12),
            axis.text.y = element_text(face = "bold", size = 12),
            axis.title.x = element_text(size = 14, margin = margin(t = 15)),
            axis.title.y = element_text(size = 14, margin = margin(r = 15)),
            strip.text.x = element_text(size = 10),
            legend.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 14, face = "bold")) 
  })
  
  # use selected.regions.data2
  
  # Need to fix sex response from logical to character, else male response won't visualize
  output$responsePlot2 <- renderPlot({
    
    trait <- sym(req(input$selected.trait))
    
    # make another column named sex based on the logical female column
    # use sex column for traits
    df3 <- san_andreas |> 
      select(!!trait, experience2, prepared2) |> 
      dplyr::group_by(!!trait, experience2) |> 
      dplyr::count(prepared2) |> 
      dplyr::mutate(proportion = round(n/sum(n),2)) |> 
      dplyr::filter(experience2 != is.na(experience2)) |>
      dplyr::filter(!!trait != is.na(!!trait))
    
    ggplot(df3, 
           aes(x = experience2,
               y = proportion)) +
      geom_col(aes(fill = prepared2),
               alpha = 0.85) +
      xlab("Past Experience with Earthquakes") + 
      ylab("Proportion of Respondents Prepared for an Earthquake ") +
      coord_flip() +
      facet_wrap(vars(!!trait)) +
      # adding bw theme to align with other plot
      theme_bw() +
      theme(legend.position = "top",
            axis.text.x = element_text(face = "bold", size = 10),
            axis.text.y = element_text(face = "bold", size = 12),
            axis.title.x = element_text(size = 14, margin = margin(t = 15)),
            axis.title.y = element_text(size = 14, margin = margin(r = 15)),
            strip.text.x = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 14, face = "bold")) +
      guides(fill = guide_legend(title = "Have you taken any precautions for an earthquake?",
                                 title.position = "top")) +
      # adding stronger color contrast
      scale_fill_viridis_d()
  })
  
  
  # make a dataset that is reactive to regions selected
  selected.regions.data <- reactive(
    san_andreas |> 
      dplyr::filter(region != is.na(region)) |> 
      dplyr::filter(region %in% input$selected.regions))
  
  
  # output table showing proportion of region that has
  # experienced an earthquake
  # need to fix s
  output$responseTable2 <- DT::renderDataTable(DT::datatable(selected.regions.data() |> 
                                                              select(region, experience2, prepared2) |> 
                                                              dplyr::group_by(region, experience2) |> 
                                                              count(prepared2),
                                                             colnames = c("Region","Past Experience","Prepared?","Count"),
                                                            caption = "Past experience with earthquakes increases likelihood of taking safety precautions.",
                                                            options = list(searching = TRUE,
                                                                           lengthChange = FALSE,
                                                                           bPaginate = FALSE)
                                                            )
                                               )

  
  # output response table for preparedness tab
  # putting the reactive element within the renderDataTable function as a 
  # counter example to responseTable2
  output$responseTable3 <- DT::renderDataTable({
    
    trait <- sym(req(input$selected.trait))
    
    # note: need to eval filtering APP fxn in general
    df3 <- san_andreas |> 
      # trait selected from dropdown input in sidebar
      select(!!trait, experience2, prepared2) |> 
      dplyr::group_by(!!trait, experience2) |> 
      dplyr::count(prepared2) |> 
      dplyr::filter(experience2 != is.na(experience2)) |>
      dplyr::filter(!!trait != is.na(!!trait))
    
    
    # removed filter option b/c it's not working
    #
    DT::datatable(df3,
                  caption = "Past experience with earthquakes increases likelihood of taking safety precautions regardless of other traits.",
                  options = list(searching = TRUE,
                                 lengthChange = FALSE,
                                 bPaginate = TRUE),
                  colnames = c("Past Experience" = "experience2",
                               "Prepared?" = "prepared2",
                               "Count" = "n")
                  )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

