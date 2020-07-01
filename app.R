library(shiny)
library(tidyverse)
library(gridExtra)
library(ggthemes)
ui <- pageWithSidebar(
  
  # Title ----
  headerPanel("Robustness of Assumptions for Two Sample Inference"),
  
  # Sidebar ----
  sidebarPanel(
    helpText("Select the characteristics of each population. What happens to their shape, the shape of the differences, and the accuracy of the resulting confidence intervals?"),
    # Population 1 input
    h1("Population 1 (Red)"),
    
    radioButtons("dist1", "Distribution:",
                 c("Normal" = "rnorm",
                   "Right skewed" = "rexp",
                   "Left skewed" = "rbeta"),
                 selected = "rnorm"),
    
    uiOutput("skew1"),
    
    br(),
    
    #Population 2 input
    h1("Population 2 (Blue)"),
    
    radioButtons("dist2", "Distribution:",
                 list("Normal" = "rnorm",
                      "Right skewed" = "rexp",
                      "Left skewed" = "rbeta")),
    uiOutput("sd2"),
    uiOutput("skew2"),
    
    br()
  ),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        title = "Population Distribution",
        # Population plot ----
        plotOutput("pop.dist", height = "500px"),
        br()
      ),
       tabPanel(
         title = "Distribution of Differences",
         # Population plot ----
         plotOutput("diff.dist", height = "500px"),
         br()
       ),
      tabPanel(
        title = "Intervals of Differences",
        fluidRow(
          column(width = 12,
                 plotOutput("int.dist", height = "500px"),
                 br())
        ),
        fluidRow(
          column(width =12,
                 div(textOutput("descr"), align = "justify"))
        )
        
        )
      )
    )
  )



server <- function(input, output, session) {

  output$sd2 = renderUI(
    {
      if(input$dist1=="rnorm" & input$dist2=='rnorm'){
        selectInput(inputId="sd2",
                    label = "Ratio of SD of Population 1 to Population 2:",
                    choices = c("1/4" = as.numeric(.25),
                                "1/2" = as.numeric(.5),
                                "1" = as.numeric(1),
                                "2" = as.numeric(2),
                                "4" = as.numeric(4)),
                    selected = "1")
      }
    }
  )
  output$skew1 = renderUI(
    {
      if (input$dist1 == "rexp" | input$dist1 == "rbeta"){
        selectInput(inputId = "skew1",
                    label = "Skew:",
                    choices = c("Low skew" = "low",
                                "Medium skew" = "med",
                                "High skew" = "high"),
                    selected = "low")
      }
    })
  
  output$skew2 = renderUI(
    {
      if (input$dist2 == "rexp" | input$dist2 == "rbeta"){
        selectInput(inputId = "skew2",
                    label = "Skew:",
                    choices = c("Low skew" = "low",
                                "Medium skew" = "med",
                                "High skew" = "high"),
                    selected = "low")
      }
    })
  
  rand_draw <- function(dist, n, sd, skew){
  
    vals = NULL
    
    if (dist == "rbeta"){
      req(skew)
      if (skew == "low"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=2))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=1.5))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n=n, shape1=5, shape2=1))
      }
    }

    else if (dist == "rnorm"){
      req(sd)
      vals = do.call(dist, list(n=n, mean=0, sd=sd))
    }

    else if (dist == "rexp"){
      req(skew)
      if (skew == "low"){
        vals = do.call(dist, list(n=n, rate = 1))
      }
      else if (skew == "med"){
        vals = do.call(dist, list(n=n, rate = 2))
      }
      else if (skew == "high"){
        vals = do.call(dist, list(n=n, rate = 3))
      }
    }

    return(vals)
  }
  
  mean_calc <- function(dist,skew){
    if (dist == "rbeta"){
      req(skew)
      if (skew == "low"){
        mean = 5/(5+2)
      }
      else if (skew == "med"){
        mean= 5/(5+1.5)
      }
      else if (skew == "high"){
        mean=5/(5+1)
      }
    }
    
    else if (dist == "rnorm"){
      mean=0
    }
    
    else if (dist == "rexp"){
      req(skew)
      if (skew == "low"){
        mean = 1
      }
      else if (skew == "med"){
        mean =1/2
      }
      else if (skew == "high"){
        mean=1/3
      }
    }
    return(mean)
  }
  mean1 =reactive({return(mean_calc(input$dist1,input$skew1))})
  mean2 =reactive({return(mean_calc(input$dist2,input$skew2))})
  
  rep_rand_draw = repeatable(rand_draw)
  
  sd1 = reactiveVal(1)
  
  parent1 = reactive({

    return(rep_rand_draw(input$dist1, 1000, sd1(), input$skew1))
  })
  
  parent2 = reactive({
  sd2 = reactiveVal(as.numeric(input$sd2))
    return(rep_rand_draw(input$dist2, 1000, sd2(), input$skew2))
  })
  
  samples1 = reactive({
    
    pop1 = parent1()
    
    return(replicate(1000, sample(pop1, 100, replace=TRUE)))
  })
  
  samples2 = reactive({
    
    pop2 = parent2()
    
    return(replicate(1000, sample(pop2, 100, replace=TRUE)))
  })
  
  output$diff.dist = renderPlot({
    
    y = samples1()-samples2()
    x = (samples1() - samples2())%>% as_tibble()
    
    plots = list(rep(NA, 8))
    
    for(i in 1:8){
      
      mean = round(mean(y[,i]), 2)
      sd = round(sd(y[,i]), 2)
      
      x_range = max(y[,i]) - min(y[,i])
      pdens = density(y[,i])
      
      x_pos = ifelse(input$dist == "rbeta", min(y[,i]) + 0.1*x_range, 
                     max(y[,i]) - 0.1*x_range)
      
      plots[[i]] = ggplot(x, aes_string(x = paste0("V", i))) +
        geom_dotplot(alpha = 0.8, dotsize = 0.7) +
        labs(title = paste("Sample", i), x = "", y = "") +
        theme_light(base_size = 13) +
        annotate("text", x = x_pos, y = 1.8,
                 label = paste("x_bar", "=", bquote(.(mean)),
                               "\n", "SD", "=", bquote(.(sd))),
                 color = "black", size = 3) +
        scale_y_continuous(limits = c(0,2), breaks = NULL) +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) + theme_economist()
    }
    
    grid.arrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]],
                 plots[[6]], plots[[7]], plots[[8]], ncol = 4)
  })
  
  output$pop.dist = renderPlot({
    
    pop1 = parent1()
    pop2 = parent2()

    m_pop1 =  round(mean(pop1), 2)
    sd_pop1 = round(sd(pop1), 2)
    m_pop2 =  round(mean(pop2), 2)
    sd_pop2 = round(sd(pop2), 2)
    
    diff_pop =  m_pop1-m_pop2
    
    pop1 = tibble(samples1 = pop1)
    pdens1 = density(pop1$samples1)
    pop2 = tibble(samples2 = pop2)
    pdens2 = density(pop2$samples2)

    
    ggplot() + 
        geom_histogram(data = pop1, bins = 7, aes(x = samples1, y = ..density.. ), fill = "red", alpha = 0.2) + 
        geom_histogram(data = pop2, bins = 7, aes(x = samples2, y = ..density..), fill = "blue", alpha = .2)+
        
      labs(title="Population Distribution", x = "x")+
        theme_light(base_size = 10) + # better than doing title sizes inside theme().
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) + theme_economist()
    
  })
  
  output$int.dist = renderPlot({
    y = samples1()-samples2()
    m1 = mean1()
    m2 = mean2()
    ci = apply(y, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
    means = rowMeans(y)
    df<- as.data.frame(t(ci))
    df<- cbind(df, means)
    df$ID <- seq.int(nrow(df))
    included =with(df, V1 <= (m1-m2) & V2 >= (m1-m2))
    df <- cbind(df,included)
    ggplot(df, aes(x = ID, y = means)) +
      geom_point(size = 1, aes(color=included)) +
      geom_errorbar(aes(ymax = V1, ymin = V2,color=included))+
      labs(x="")+
      scale_colour_manual(values = c("red","black")) + theme_economist()
    
  })
  output$descr = renderText({
    
    m1 = mean1()
    m2 = mean2()

    paste0("The true mean difference is ", m1-m2 ,". The most serious violations
           against robustness may arise when standard deviations of the two populations
           are very different. In this case, the pooled estimate of standard deviations
           does not estimate any population parameter and the standard error formula,
           which uses the pooled estimate of standard deviation, no longer estimates 
           the standard deviation of the difference between sample averages. As a
           result, the t-ratio does not have a t-distribution. The plot above shows
           95% confidence intervals for each of our 100 samples. As you can see, 
           changing the standard deviation and skew of the two populations will 
           affect how many confidence intervals actually contain the true difference between the populations")
  })

}
shinyApp(ui, server)