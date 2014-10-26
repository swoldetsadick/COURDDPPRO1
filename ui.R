library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("Illustrating Central Limit Theorem (CLT)"),
        sidebarPanel(
                img(src="plevy.png", height = 400, width = 400),

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
                                 numericInput("five", "Degree of Freedom of Distribution:", 3, min = 2)),
                
                conditionalPanel(condition = "input.distri == 4", label = h4("Distribution Parameters Choice"),
                                 numericInput("six", "Rate of Distribution:", 1, min =1)),
                
                sliderInput('nval', 'Number of Observations (n)', 2, min = 1, max = 100, step = 1,),
                
                numericInput('id1', 'Number of Simulations', 1000, min = 0, max = 100000, step = 100)
                
        ),
        
        mainPanel(
                h6('In probability theory, the central limit theorem (CLT) states that, the arithmetic mean of a sufficiently large number of iterates of independent random variables, each with a well-defined expected value and well-defined variance, will be approximately normally distributed, regardless of the underlying distribution.'),
                h6('To illustrate this theorem, choose any distribution from the list in the side panel. Set the number of sampling iteration in the box labeled "Number of Simulation" then set parameters, whose number depend on the distribution selected.'),
                h6('Use the slider in the side bar to raise or lower the number to observations in a sample, and observe in the graph how this affects the distribution of the arithmetic averages of the sample. The density for this distribution is drawn in full red line, with a histogram set for arithmetic averages.'),
                h6('Observe how as you raise the number of observations in samples, or number of iteration the density of said distribution closes to the density of its corresponding central limit theorem normal distribution, drawn in full blue line here.'),
                h6('Expected value of these averages approaches (also shown in graph by a vertical dashed blue line) that of the real value of the mean of the distribution from which these observations are sampled of.'),
                h6('The chosen distribution is:'),
                verbatimTextOutput("odistri"),
                h6('Number of simulations chosen is:'), 
                verbatimTextOutput("oid1"),
                plotOutput("xbar")
        )
))