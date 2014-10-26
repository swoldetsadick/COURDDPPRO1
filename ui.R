library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("Illustrating Central Limit Theorem (CLT)"),
        sidebarPanel(
                radioButtons("distri", label = h4("Distribution Type"),
                        choices = list("Normal Distribution" = 1, "Uniform Distribution" = 2, 
                                        "Student Distribution" = 3, "Exponential Distribution" = 4, 
                                       "Weibull Distribution" = 5), selected = 1),
                conditionalPanel(condition = "input.distri == 1", label = h4("Distribution Parameters Choice"),
                                 numericInput("one", "Mean of Distribution:", 0),
                                 numericInput("two", "Standard Deviation of Distribution:", 1, min = 0)),
                
                conditionalPanel(condition = "input.distri == 5", label = h4("Distribution Parameters Choice"),
                                 numericInput("three", "Shape of Distribution:", 1, min = 1),
                                 numericInput("four", "Scale of Distribution:", 1, min = 1)),
                
                conditionalPanel(condition = "input.distri == 3", label = h4("Distribution Parameters Choice"),
                                 numericInput("five", "Degree of Freedom of Distribution:", 2, min = 2)),
                
                conditionalPanel(condition = "input.distri == 4", label = h4("Distribution Parameters Choice"),
                                 numericInput("six", "Rate of Distribution:", 0)),
                
                sliderInput('nval', 'Number of Observations (n)', 2, min = 1, max = 100, step = 1,),
                
                numericInput('id1', 'Number of Simulations', 1000, min = 0, max = 100000, step = 100)  
        ),
        
        mainPanel(
                h4('Illustrating outputs'),
                h6('The chosen distribution is:'),
                verbatimTextOutput("odistri"),
                h6('Number of simulations chosen is:'), 
                verbatimTextOutput("oid1"),
                plotOutput("xbar")
                
        )
))