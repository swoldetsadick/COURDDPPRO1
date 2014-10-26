library(shiny)
library(ggplot2)
#
shinyServer(
        function(input, output) {
                output$odistri <- renderPrint({ifelse(input$distri == 1, "The Normal Distribution", 
                                            ifelse(input$distri == 2, "The Uniform Distribution", 
                                            ifelse(input$distri == 3, "The Student Distribution", 
                                            ifelse(input$distri == 4, "The Exponential Distribution", "The Weibull Distribution"))))
                                            })
                output$oid1 <- renderPrint({input$id1})
                output$xbar <- renderPlot({ if(input$distri == 1){
                        a <- matrix(rnorm((input$id1 * input$nval), input$one, input$two), input$id1, input$nval)
                        b <- apply(a, 1, mean)
                        c <- round(mean(apply(a, 1, mean)), digits = 2)
                        
                        title <- 'Distribution of Sample Means (Theoretical vs Simulated)'
                        subtitle <- 'Samples drawn from Normal Distribution'
                        
                        m <- ggplot(data.frame(b), aes(b))
                        m <- m + geom_histogram(aes(y =..density..), binwidth = (round(max(b)-min(b))/40), color = "black", fill = "white")
                        m <- m + xlab("Sample Averages") + ylab("Density") + ggtitle(bquote(atop(.(title), atop(italic(.(subtitle)), "")))) 
                        m <- m + geom_density(col="red", alpha=.2, fill="#FF6666") + theme(plot.title = element_text(size=19,lineheight=.8,vjust=-1))
                        m <- m + stat_function(fun = dnorm, args = list(mean = input$one, sd = (input$two/sqrt(input$nval))), color = "blue")
                        m <- m + geom_text(x = Inf, y = Inf, label = paste(paste(paste("Real Mean mu =", input$one, sep=" "),"\nExpected Value of Averages =", sep=" "), c, sep = " "), hjust = 1, vjust = 1)
                        m <- m + geom_vline(aes(xintercept = round(mean(data.frame(b)$b), digits = 2)), color = "blue", linetype = "dashed", size = 1)
                        print(m)} else if (input$distri == 5) {
                                
                        a <- matrix(rweibull((input$id1 * input$nval), input$three, input$four), input$id1, input$nval)
                        b <- apply(a, 1, mean)
                        c <- round(mean(apply(a, 1, mean)), digits = 2)
                        d <- input$four*gamma(1+(1/input$three))
                        e <- input$four * sqrt(gamma(1+(2/input$three))-(gamma(1+(1/input$three)))^2)
                                
                        title <- 'Distribution of Sample Means (Theoretical vs Simulated)'
                        subtitle <- 'Samples drawn from Weibull Distribution'
                                
                        m <- ggplot(data.frame(b), aes(b))
                        m <- m + geom_histogram(aes(y =..density..), binwidth = (round(max(b)-min(b))/40), color = "black", fill = "white")
                        m <- m + xlab("Sample Averages") + ylab("Density") + ggtitle(bquote(atop(.(title), atop(italic(.(subtitle)), "")))) 
                        m <- m + geom_density(col="red", alpha=.2, fill="#FF6666") + theme(plot.title = element_text(size=19,lineheight=.8,vjust=-1))
                        m <- m + stat_function(fun = dnorm, args = list(mean = d, sd = (e/sqrt(input$nval))), color = "blue")
                        m <- m + geom_text(x = Inf, y = Inf, label = paste(paste(paste("Real Mean mu =", d, sep=" "),"\nExpected Value of Averages =", sep=" "), c, sep = " "), hjust = 1, vjust = 1)
                        m <- m + geom_vline(aes(xintercept = round(mean(data.frame(b)$b), digits = 2)), color = "blue", linetype = "dashed", size = 1)        
                        print(m)} else if (input$distri == 3) {
                                
                        a <- matrix(rt((input$id1 * input$nval), input$five), input$id1, input$nval)
                        b <- apply(a, 1, mean)
                        c <- round(mean(apply(a, 1, mean)), digits = 2)
                        d <- 0
                        e <- sqrt(input$five/(input$five - 2))
                                
                        title <- 'Distribution of Sample Means (Theoretical vs Simulated)'
                        subtitle <- 'Samples drawn from Student (t) Distribution'
                                
                        m <- ggplot(data.frame(b), aes(b))
                        m <- m + geom_histogram(aes(y =..density..), binwidth = (round(max(b)-min(b))/40), color = "black", fill = "white")
                        m <- m + xlab("Sample Averages") + ylab("Density") + ggtitle(bquote(atop(.(title), atop(italic(.(subtitle)), "")))) 
                        m <- m + geom_density(col="red", alpha=.2, fill="#FF6666") + theme(plot.title = element_text(size=19,lineheight=.8,vjust=-1))
                        m <- m + stat_function(fun = dnorm, args = list(mean = d, sd = (e/sqrt(input$nval))), color = "blue")
                        m <- m + geom_text(x = Inf, y = Inf, label = paste(paste(paste("Real Mean mu =", d, sep=" "),"\nExpected Value of Averages =", sep=" "), c, sep = " "), hjust = 1, vjust = 1)
                        m <- m + geom_vline(aes(xintercept = round(mean(data.frame(b)$b), digits = 2)), color = "blue", linetype = "dashed", size = 1)        
                        print(m)} else if (input$distri == 4) {
                                
                        a <- matrix(rexp((input$id1 * input$nval), input$six), input$id1, input$nval)
                        b <- apply(a, 1, mean)
                        c <- round(mean(apply(a, 1, mean)), digits = 2)
                        d <- 1/input$six
                        e <- 1/input$six
                                
                        title <- 'Distribution of Sample Means (Theoretical vs Simulated)'
                        subtitle <- 'Samples drawn from Exponential Distribution'
                                
                        m <- ggplot(data.frame(b), aes(b))
                        m <- m + geom_histogram(aes(y =..density..), color = "black", fill = "white")
                        m <- m + xlab("Sample Averages") + ylab("Density") + ggtitle(bquote(atop(.(title), atop(italic(.(subtitle)), "")))) 
                        m <- m + geom_density(col="red", alpha=.2, fill="#FF6666") + theme(plot.title = element_text(size=19,lineheight=.8,vjust=-1))
                        m <- m + stat_function(fun = dnorm, args = list(mean = d, sd = (e/sqrt(input$nval))), color = "blue")
                        m <- m + geom_text(x = Inf, y = Inf, label = paste(paste(paste("Real Mean mu =", d, sep=" "),"\nExpected Value of Averages =", sep=" "), c, sep = " "), hjust = 1, vjust = 1)
                        m <- m + geom_vline(aes(xintercept = round(mean(data.frame(b)$b), digits = 2)), color = "blue", linetype = "dashed", size = 1)        
                        print(m)} else {
                                
                        a <- matrix(runif((input$id1 * input$nval)), input$id1, input$nval)
                        b <- apply(a, 1, mean)
                        c <- round(mean(apply(a, 1, mean)), digits = 2)
                        d <- 1/2
                        e <- sqrt(1/12)
                                
                        title <- 'Distribution of Sample Means (Theoretical vs Simulated)'
                        subtitle <- 'Samples drawn from Uniform Distribution'
                                
                        m <- ggplot(data.frame(b), aes(b))
                        m <- m + geom_histogram(aes(y =..density..), color = "black", fill = "white")
                        m <- m + xlab("Sample Averages") + ylab("Density") + ggtitle(bquote(atop(.(title), atop(italic(.(subtitle)), "")))) 
                        m <- m + geom_density(col="red", alpha=.2, fill="#FF6666") + theme(plot.title = element_text(size=19,lineheight=.8,vjust=-1))
                        m <- m + stat_function(fun = dnorm, args = list(mean = d, sd = (e/sqrt(input$nval))), color = "blue")
                        m <- m + geom_text(x = Inf, y = Inf, label = paste(paste(paste("Real Mean mu =", d, sep=" "),"\nExpected Value of Averages =", sep=" "), c, sep = " "), hjust = 1, vjust = 1)
                        m <- m + geom_vline(aes(xintercept = round(mean(data.frame(b)$b), digits = 2)), color = "blue", linetype = "dashed", size = 1)        
                        print(m)}
                })
                
                
        })