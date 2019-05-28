
library(shiny)
library(DT)


shinyServer(function(input, output) {
  library(wooldridge)
  library(dplyr)
  library(RColorBrewer)
  library(ggplot2)
  library(ggthemes)
  library(grid)
  library(gridExtra)
  library(sjPlot)

  wage1 <- wage1 %>%
    mutate(nonwhite = factor(nonwhite, labels = c('white','nonwhite')),
           female = factor(female, labels = c('male', 'female')),
           married = factor(married, labels = c('non married', 'married')),
           smsa = as.factor(smsa),
           northcen = as.factor(northcen),
           south = as.factor(south),
           west = as.factor(west),
           construc = as.factor(construc),
           ndurman = as.factor(ndurman),
           trcommpu = as.factor(trcommpu),
           trade = as.factor(trade),
           services = as.factor(services),
           profserv = as.factor(profserv),
           profocc = as.factor(profocc),
           clerocc = as.factor(clerocc),
           servocc = as.factor(servocc))

  jtrain <- jtrain %>%
    mutate(union = factor(union, labels = c('not unionized', 'unionized')),
           grant = factor(grant, labels = c('not granted', 'granted')),
           d89 = as.factor(d89),
           d88 = as.factor(d88))


  datasetInput <- reactive({
    switch(input$dataset,
           "wage1" = wage1,
           "jtrain" = jtrain
    )
  })


  page <- reactive({
    switch(input$dataset,
           "wage1" = 'https://justinmshea.github.io/wooldridge/reference/wage1.html',
           "jtrain" = 'https://justinmshea.github.io/wooldridge/reference/jtrain.html'
    )
  })

  output$o_iframe <- renderUI({
    tags$iframe(src = page(), width = '100%', height = 600)
  })

  output$o_data_table <- renderDT({
    datatable(datasetInput(), rownames = FALSE, style = 'bootstrap',
              filter = 'top',
              options = list(
                pageLength = 10, autoWidth = TRUE, lengthChange = FALSE
                )
              ) %>% formatRound(names(datasetInput()), digits = 4)
    })

  output$o_data_nfc <- renderPrint({
    dim(datasetInput())
  })

  output$o_data_var_names <- renderPrint({
    names(datasetInput())
  })

  output$o_data_str_df <- renderPrint({
    str(datasetInput())
  })

  output$o_data_summary_df <-  renderPrint({
    summary(datasetInput())
  })

  output$o_data_head5_df <-
    renderTable({head( datasetInput(), n = 5 )},
                hover = TRUE, spacing = 'xs',
                digits = 4)

  output$o_data_tail5_df <-
    renderTable({tail( datasetInput(), n = 5 )},
                hover = TRUE, spacing = 'xs',
                digits = 4)

  output$categorica_input <- renderUI({
    selectInput("cat_oui", "Seleccionar variable:", as.list(datasetInput() %>% select_if(is.factor) %>% names))
  })

  output$cat_plot <- renderPlot({
    barplot(table(datasetInput()[,input$cat_oui]), xlab = input$cat_ui, ylab = 'N', main = '',
            col = brewer.pal(length(levels(datasetInput()[,input$cat_oui])), "Set1"))
  })

  output$tabla_cruzada_cat <- renderPrint({
    prop.table(table(datasetInput()[,input$cat_oui]))
  })

  output$test_prop_cat <- renderPrint({
    prop.test(as.vector(table(datasetInput()[,input$cat_oui])),
              rep(sum(table(datasetInput()[,input$cat_oui])),
                  length(table(datasetInput()[,input$cat_oui]))),
              alternative = input$tipo_cont_cat,
              conf.level = as.numeric(input$int_conf_cat))
  })

  output$medida_input <- renderUI({
    selectInput("medida_var", "Seleccionar variable:", as.list(datasetInput() %>% select_if(is.numeric) %>% names))
  })

  output$medidas_3m <-
    renderPrint({summary(datasetInput()[,input$medida_var])})

  output$medidas_sd <-
    renderPrint({sd(datasetInput()[,input$medida_var], na.rm = TRUE)})

  output$medidas_var <-
    renderPrint({var(datasetInput()[,input$medida_var], na.rm = TRUE )})

  output$medidas_ri <-
    renderPrint({IQR(datasetInput()[,input$medida_var], na.rm = TRUE )})


  output$medidas_moda <-
    renderPrint({
      uniqv <- unique(datasetInput()[,input$medida_var])
      uniqv[which.max(tabulate(match(datasetInput()[,input$medida_var], uniqv)))]
    })

  output$medidas_asimetria <-
    renderPrint({
      m3 = mean((datasetInput()[,input$medida_var] - mean(datasetInput()[,input$medida_var]))^3)
      skew = m3/(sd(datasetInput()[,input$medida_var])^3)
      skew
    })
  output$medidas_cur <-
  renderPrint({
    m4 = mean((datasetInput()[,input$medida_var] - mean(datasetInput()[,input$medida_var]))^4)
    kurt = m4/(sd(datasetInput()[,input$medida_var])^4) - 3
    kurt
  })

  output$medidas_rm <-
    renderPrint({
      (max(datasetInput()[,input$medida_var], na.rm = TRUE) +
     min(datasetInput()[,input$medida_var], na.rm = TRUE ))/2
    })

  output$ttest_input <- renderUI({
    selectInput("ttest_var", "Seleccionar variable:", as.list(datasetInput() %>% select_if(is.numeric) %>% names))
  })



  output$o_1m_ttest <- renderPrint({
    tmp <- t.test(datasetInput()[,input$ttest_var], mu = input$media_1m_ttest)
    tmp$data.name <- input$ttest_var
    tmp
  })

  output$o_1m_ztest <- renderPrint({
    tmp <- BSDA::z.test(datasetInput()[,input$ttest_var], sigma.x = input$sd_1m_ztest, mu = input$media_1m_ztest)
    tmp$data.name <- input$ttest_var
    tmp
  })

  # output$o_power_ttest <- renderPrint({
  #   power.t.test(n = length(datasetInput()[,input$ttest_var]), delta = input$diff_media_ttest, alternative = input$tipo_cont_ttest)
  #   # La alternativa usada por DIDAT es pwr::pwr.t.test
  # })

  # output$o_power_ztest <- renderPrint({
  #   asbio::power.z.test(sigma = input$sd_power_ztest,n = length(datasetInput()[,input$ttest_var]), test = 'one.tail', effect = input$media_power_ztest)
  # })

  output$wilcoxon_input <- renderUI({
    selectInput("wilcoxon_var", "Seleccionar variable:", as.list(datasetInput() %>% select_if(is.numeric) %>% names))
  })

  output$o_wil_test <- renderPrint({
    tmp <- wilcox.test(datasetInput()[,input$wilcoxon_var], mu = input$dif_median_wil,
                alternative = input$tipo_cont_wil,
                conf.level = as.numeric(input$int_conf_wil),
                conf.int = TRUE)
    tmp$data.name <- input$wilcoxon_var
    tmp
  })

  output$violin_input <- renderUI({
    selectInput("violin_var", "Seleccionar variable:", as.list(datasetInput() %>% select_if(is.numeric) %>% names))
  })

  output$o_boxplot_violin <- renderPlot(
    ggplot(data = datasetInput(), aes(x = 1, y = get(input$violin_var))) +
      geom_boxplot(fill = brewer.pal(3,"Set1")[1]) +
      #geom_jitter(alpha = 0.3) +
      labs(y = '', x = input$violin_var) +
      theme_tufte() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  )

  output$o_violin_violin <- renderPlot(
    ggplot(data = datasetInput(), aes(x = 1, y = get(input$violin_var))) +
      geom_violin(fill = brewer.pal(3,"Set1")[1]) +
      #geom_jitter(alpha = 0.3) +
      labs(y = '', x = input$violin_var) +
      theme_tufte() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  )

  output$dist_input <- renderUI({
    selectInput("dist_var", "Seleccionar variable:", as.list(datasetInput() %>% select_if(is.numeric) %>% names))
  })


  output$o_hist_dist <- renderPlot(
    ggplot(data = datasetInput(), aes(get(input$dist_var))) +
      geom_histogram(fill = brewer.pal(3,"Set1")[1], bins = 20, color = 'black') +
      geom_rug() +
      labs(y = 'Frecuencia absoluta', x = input$violin_var) +
      theme_tufte()
  )

  output$o_qqplot_dist <- renderPlot(
    ggplot(data = datasetInput(), aes(sample = get(input$dist_var))) +
      stat_qq() + stat_qq_line() +
      labs(y = 'Muestra', x = 'Teórica') +
      theme_tufte()
  )

  output$lsp_input <- renderUI({
    selectInput("lsp_var", "Seleccionar variable:", as.list(datasetInput() %>% select_if(is.numeric) %>% names))
  })

  output$o_retardos_lsp <- renderPlot(
    ggplot(data = data.frame(x = head(datasetInput()[,input$lsp_var],-1),
                             y = tail(datasetInput()[,input$lsp_var],-1)),
           aes(x = x, y = y)) +
      geom_point() + geom_rangeframe() +
      labs(x = paste0(input$lsp_var,'[1:',length(datasetInput()[,input$lsp_var]) - 1,']'),
           y = paste0(input$lsp_var,'[2:',length(datasetInput()[,input$lsp_var]),']')) +
      theme_tufte()
  )

  output$o_secuencial_lsp <- renderPlot(
    ggplot(data = datasetInput(), aes(x = 1:length(datasetInput()[,input$lsp_var]),
                                      y = get(input$lsp_var))) +
      geom_line() + geom_rangeframe() +
      labs(x = 'Retardo',
           y = input$lsp_var) +
      theme_tufte()
  )


  output$tba_input <- renderUI({
    selectInput("tba_var", "Seleccionar variable:", as.list(datasetInput() %>% select_if(is.numeric) %>% names))
  })

  fitdist <- reactive({
    MASS::fitdistr(datasetInput()[, input$tba_var], input$i_curva_tba)
  })

  output$o_momentos_tba <- renderPrint({
    fitdist()
  })

  ll.normal <- function(param)
  {mu <- param[1]
  sigma <- param[2]
  llValue <- dnorm(datasetInput()[, input$tba_var], mean = mu, sd = sigma, log = TRUE)
  return(sum(llValue))}

  ll.cauchy <- function(param) {
    location <- param[1]
    scale <- param[2]
    llValue <- dcauchy(datasetInput()[, input$tba_var], location, scale, log = TRUE)
    return(sum(llValue))
  }

  ll.logistic <- function(param) {
    location <- param[1]
    scale <- param[2]
    llValue <- dlogis(datasetInput()[, input$tba_var], location, scale, log = TRUE)
    return(sum(llValue))
  }

  ll.exponential <- function(param) {
    rate <- param[1]
    llValue <- dexp(datasetInput()[, input$tba_var], rate, log = TRUE)
    return(sum(llValue))
  }

  ll.weibull <- function(param) {
    shape <- param[1]
    scale <- param[2]
    llValue <- dweibull(datasetInput()[, input$tba_var], shape, scale, log = TRUE)
    return(sum(llValue))
  }



  output$o_max_ver_tba <- renderPrint({
    switch(input$i_curva_tba,
           normal = {summary(maxLik::maxLik(ll.normal, start = c(mu = 0, sigma = 1)))},
           cauchy = {summary(maxLik::maxLik(ll.cauchy, start = c(location = 0, scale = 1)))},
           logistic = {summary(maxLik::maxLik(ll.logistic, start = c(location = 0, scale = 1)))},
           exponential = {summary(maxLik::maxLik(ll.exponential, start = c(rate = 1)))},
           weibull = {summary(maxLik::maxLik(ll.weibull, start = c(shape = 1, scale = 1)))}
    )
  })

  output$o_ks_tba <- renderPrint({
    tmp <- switch(input$i_curva_tba,
            normal = {ks.test(datasetInput()[, input$tba_var], 'pnorm', fitdist()$estimate[1], fitdist()$estimate[2])},
            cauchy = {ks.test(datasetInput()[, input$tba_var], 'pcauchy', fitdist()$estimate[1], fitdist()$estimate[2])},
            logistic = {ks.test(datasetInput()[, input$tba_var], 'plogis', fitdist()$estimate[1], fitdist()$estimate[2])},
            exponential = {ks.test(datasetInput()[, input$tba_var], 'pexp', fitdist()$estimate[1])},
            weibull = {ks.test(datasetInput()[, input$tba_var], 'pweibull', fitdist()$estimate[1], fitdist()$estimate[2])}
    )
    tmp$data.name <- input$tba_var
    tmp

  })

  output$o_bondad_tba <- renderPlot({
    g <- datasetInput()[, input$tba_var]
    h <- hist(g, freq = FALSE)
    xfit <- seq(min(g), max(g), length = 40)
    yfit <- switch(input$i_curva_tba,
                    normal = {dnorm(xfit, fitdist()$estimate[1], fitdist()$estimate[2])},
                    cauchy = {dcauchy(xfit, fitdist()$estimate[1], fitdist()$estimate[2])},
                    logistic = {dlogis(xfit, fitdist()$estimate[1], fitdist()$estimate[2])},
                    exponential = {dexp(xfit, fitdist()$estimate[1])},
                    weibull = {dweibull(xfit, fitdist()$estimate[1], fitdist()$estimate[2])}
    )
    # yfit <- yfit * diff(h$mids[1:2]) * length(g)

    lines(xfit, yfit, col = "black", lwd = 1.5)
    })


  output$nnbiv_input1 <- renderUI({
    selectInput("nnbiv_var1", "Seleccionar variable X:", as.list(datasetInput() %>% select_if(is.numeric) %>% names))
  })

  output$nnbiv_input2 <- renderUI({
    selectInput("nnbiv_var2", "Seleccionar variable Y:",
                as.list(datasetInput() %>% select_if(is.numeric) %>% select(-one_of(input$nnbiv_var1)) %>% names))
  })

  output$o_bihist_bi <- renderPlot({
    out <- Hmisc::histbackback(datasetInput()[, input$nnbiv_var1], datasetInput()[, input$nnbiv_var2],
                               xlab = c(input$nnbiv_var1, input$nnbiv_var2), probability = TRUE)

    barplot(-out$left, col = brewer.pal(3,"Set1")[1], horiz = TRUE, space = 0, add = TRUE, axes = FALSE)
    barplot(out$right, col = brewer.pal(3,"Set1")[2], horiz = TRUE, space = 0, add = TRUE, axes = FALSE)
  })

  output$o_sp_bi <- renderPlot({
    ggplot(data = datasetInput(), aes(x = datasetInput()[,input$nnbiv_var1],
                                      y = datasetInput()[,input$nnbiv_var2])) +
      geom_point() +
      geom_smooth(method = 'lm', se = FALSE ) +
      geom_rangeframe() +
      labs(x = input$nnbiv_var1, y = input$nnbiv_var2) +
      theme_tufte()
  })

  output$o_sp_coef_bi <- renderPrint(
    cor(datasetInput()[,input$nnbiv_var1], datasetInput()[,input$nnbiv_var2])
  )

  fm_slm <- reactive({
    paste(input$nnbiv_var2, '~', input$nnbiv_var1)
  })

  output$o_rl_bi <- renderPrint(
    summary(lm(as.formula(fm_slm()),
               data = datasetInput()))
  )


  diagPlot <- function(model){
    p1 <- ggplot(model, aes(.fitted, .resid)) + geom_point()
    p1 <- p1 + stat_smooth(method = "loess") + geom_hline(yintercept = 0, col = "red", linetype = "dashed")
    p1 <- p1 + xlab("Fitted values") + ylab("Residuals")
    p1 <- p1 + ggtitle("Residual vs Fitted Plot") + theme_bw()

    # p2<-ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
    # p2<-p2+geom_abline(aes(qqline(.stdresid)))+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
    # p2<-p2+ggtitle("Normal Q-Q")+theme_bw()

    p2 <- ggplot(model, aes( sample = .stdresid)) + stat_qq() + stat_qq_line()
    p2 <- p2 + xlab("Theoretical Quantiles") + ylab("Standardized Residuals")
    p2 <- p2 + ggtitle("Normal Q-Q") + theme_bw()

    p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
    p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
    p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
    p3<-p3+ggtitle("Scale-Location")+theme_bw()

    p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
    p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
    p4<-p4+ggtitle("Cook's distance")+theme_bw()

    p5<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
    p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
    p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
    p5<-p5+ggtitle("Residual vs Leverage Plot")
    p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
    p5<-p5+theme_bw()+theme(legend.position="bottom")

    p6<-ggplot(model, aes(.hat, .cooksd))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
    p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
    p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
    p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
    p6<-p6+theme_bw()

    return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
  }

  output$o_rl_plot_bi <- renderPlot({
    diagPlts <- diagPlot(lm(datasetInput()[,input$nnbiv_var2] ~ datasetInput()[,input$nnbiv_var1],
                            data = datasetInput()))
    do.call(grid.arrange, c(diagPlts, top = "Diagnostic Plots", ncol = 3))
  })

  output$o_ftest_vartst <- renderPrint({
    tmp <- var.test(datasetInput()[,input$nnbiv_var1], datasetInput()[,input$nnbiv_var2],
             alternate = input$tipo_cont_vartst, conf.level = as.numeric(input$int_conf_vartst))
    tmp$data.name <- paste0(input$nnbiv_var1, ' and ', input$nnbiv_var2)
    tmp
  })

  output$o_plot_tst_bi <- renderPlot({
    boxplot(datasetInput()[,input$nnbiv_var1], datasetInput()[,input$nnbiv_var2],
            names = c(input$nnbiv_var1, input$nnbiv_var2),
            col = c(brewer.pal(3,"Set1")[1], brewer.pal(3,"Set1")[2]))
    points(c(mean(datasetInput()[,input$nnbiv_var1]),mean(datasetInput()[,input$nnbiv_var2])),
           col = 'black', pch = 18, cex = 1.5)
  })

  output$o_tst_bi <- renderPrint({
    tmp <- t.test(datasetInput()[,input$nnbiv_var1], datasetInput()[,input$nnbiv_var2],
           alternate = input$tipo_cont_tst_bi, conf.level = as.numeric(input$int_conf_tst_bi),
           var.equal = input$variance_tst_bi, paired = input$paired_tst_bi, mu = input$mean_dif_tst_bi)
    tmp$data.name <- paste0(input$nnbiv_var1, ' and ', input$nnbiv_var2)
    tmp
  })

  output$o_plot_wil_bi <- renderPlot({
    boxplot(datasetInput()[,input$nnbiv_var1], datasetInput()[,input$nnbiv_var2],
            names = c(input$nnbiv_var1, input$nnbiv_var2),
            col = c(brewer.pal(3,"Set1")[1], brewer.pal(3,"Set1")[2]))

  })

  output$o_wil_bi <- renderPrint({
    tmp <- wilcox.test(datasetInput()[,input$nnbiv_var1], datasetInput()[,input$nnbiv_var2],
                alternative = input$tipo_cont_wil_bi, conf.level = as.numeric(input$int_conf_wil_bi),
                paired = input$paired_wil_bi, mu = input$mean_dif_wil_bi, conf.int = TRUE)
    tmp$data.name <- paste0(input$nnbiv_var1, ' and ', input$nnbiv_var2)
    tmp
  })

  output$cn_input1 <- renderUI({
    selectInput("cn_var1", "Seleccionar variable categórica:", as.list(datasetInput() %>% select_if(is.factor) %>% names))
  })

  output$cn_input2 <- renderUI({
    selectInput("cn_var2", "Seleccionar variable numérica:", as.list(datasetInput() %>% select_if(is.numeric) %>% names))
  })

  b_col <- reactive({
    if (length(levels(datasetInput()[,input$cn_var1])) <= 2)
      brewer.pal(3, "Set1")[1:length(levels(datasetInput()[,input$cn_var1]))]
    else
      brewer.pal(length(levels(datasetInput()[,input$cn_var1])), "Set1")
  })

  output$o_boxplot_cn <- renderPlot({
    boxplot(datasetInput()[, input$cn_var2] ~ datasetInput()[, input$cn_var1],
            col = b_col(), xlab = input$cn_var2)
  })

  output$o_density_cn <- renderPlot({
    ggplot(data = datasetInput(), aes_string(x = input$cn_var2,
                                             fill = input$cn_var1)) +
      geom_density(alpha = 0.7) +
      scale_fill_brewer(type = 'qual', palette = 6) +
      geom_rangeframe() +
      labs(x = input$cn_var2, y = 'densidad') +
      theme_tufte() +
      theme(legend.position = 'bottom')
  })

  fm_lm <- reactive({
    paste(input$cn_var2, '~', input$cn_var1)
  })

  output$o_rl_cn <- renderPrint({
    summary(lm(as.formula(fm_lm()),
               data = datasetInput()))
  })



  diagPlot4 <- function(model){
    p1 <- ggplot(model, aes(.fitted, .resid)) + geom_point()
    p1 <- p1 + stat_smooth(method = "loess") + geom_hline(yintercept = 0, col = "red", linetype = "dashed")
    p1 <- p1 + xlab("Fitted values") + ylab("Residuals")
    p1 <- p1 + ggtitle("Residual vs Fitted Plot") + theme_bw()

    # p2<-ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
    # p2<-p2+geom_abline(aes(qqline(.stdresid)))+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
    # p2<-p2+ggtitle("Normal Q-Q")+theme_bw()

    p2 <- ggplot(model, aes( sample = .stdresid)) + stat_qq() + stat_qq_line()
    p2 <- p2 + xlab("Theoretical Quantiles") + ylab("Standardized Residuals")
    p2 <- p2 + ggtitle("Normal Q-Q") + theme_bw()

    p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
    p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
    p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
    p3<-p3+ggtitle("Scale-Location")+theme_bw()


    p5<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
    p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
    p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
    p5<-p5+ggtitle("Residual vs Leverage Plot")
    p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
    p5<-p5+theme_bw()+theme(legend.position="bottom")


    return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3,  rvlevPlot=p5 ))
  }

  output$o_rl_plot_cn <- renderPlot({
    diagPlts <- diagPlot4(lm(datasetInput()[,input$cn_var2] ~ datasetInput()[,input$cn_var1],
                             data = datasetInput()))
    do.call(grid.arrange, c(diagPlts, top = "Diagnostic Plots", ncol = 2))
  })

  fm_aov <- reactive({paste0(input$cn_var2,'~',input$cn_var1)})

  output$o_anova_cn <- renderPrint({
    summary(aov(as.formula(fm_aov()), data = datasetInput()))
  })

  output$o_anova_tukey_cn <- renderPrint({
    a1 <- aov(as.formula(fm_aov()), data = datasetInput())
    TukeyHSD(x = a1, conf.level = 0.95)
  })

  output$o_krustal_cn <- renderPrint({
    kruskal.test(as.formula(fm_aov()),
                 data = datasetInput())
  })

  output$cc_input1 <- renderUI({
    selectInput("cc_var1", "Seleccionar primera variable:",
                as.list(datasetInput() %>% select_if(is.factor) %>% names))
  })

  output$cc_input2 <- renderUI({
    selectInput("cc_var2", "Seleccionar segunda variable:",
                as.list(datasetInput() %>% select_if(is.factor) %>% select(-one_of(input$cc_var1)) %>% names))
  })

  output$o_tab_cruzada_bi <- renderUI({
    custom_table <- sjPlot::sjt.xtab(datasetInput()[, input$cc_var1], datasetInput()[, input$cc_var2],
                                     var.labels = c(input$cc_var1,input$cc_var2),

                                     show.summary = F,
                                     show.cell.prc	= T,

                                     show.row.prc	= T,

                                     show.col.prc = T,

                                     show.na = F,
                                     wrap.labels = 50,
                                     #tdcol.col = "#00688B",

                                     #emph.total = T,
                                     #emph.color = "#3aaee5",
                                     use.viewer = T#,
                                     #CSS = list(css.table = "border: 1px solid;",
                                     #            css.tdata = "border: 1px solid;")
    )

    HTML(custom_table$knitr)

  })


  output$o_barplot_cc_bi <- renderPlot({
    datasetInput() %>%
      group_by(.dots = c(input$cc_var1, input$cc_var2)) %>%
      summarise(counts = n()) %>%
      ggplot(aes_string(x = input$cc_var1, y = quo(counts),
                        color = input$cc_var2, fill = input$cc_var2)) +
      geom_bar(

        stat = "identity", position = position_dodge(0.8),
        width = 0.7
      ) +
      scale_color_brewer(type = 'qual', palette = 6) +
      scale_fill_brewer(type = 'qual', palette = 6) +
      geom_text(
        aes_string(label = quo(counts), group = input$cc_var2),
        position = position_dodge(0.8),
        vjust = -0.3, size = 3.5) +

      theme_tufte() +
      geom_rangeframe(color = 'black')
  })

  cont_table <- reactive({
    table(datasetInput()[, input$cc_var1], datasetInput()[, input$cc_var2])
  })

  chisq <- reactive({
    chisq.test(cont_table())
  })

  output$o_x2_cc_bi <- renderPrint({
    chisq()
  })

  output$o_x2_xp_cc_bi <- renderPrint({
    round(chisq()$expected,2)
  })

  output$o_x2_res_cc_bi <- renderPrint({
    round(chisq()$residuals, 3)
  })

  output$o_x2_std_cc_bi <- renderPrint({
    round(chisq()$stdres, 3)
  })

  output$o_fisher_c_bi <- renderPrint({
    fisher.test(cont_table())
  })

  output$mreg_input1 <- renderUI({
    selectInput("mreg_var1", "Seleccionar variable dependiente:", as.list(datasetInput() %>% select_if(is.numeric) %>% names))
  })

  output$mreg_input2 <- renderUI({
    selectInput("mreg_var2", "Seleccionar regresores:", as.list(datasetInput() %>% select(-one_of(input$mreg_var1)) %>% names),
                multiple = TRUE, selected = datasetInput() %>% select(-one_of(input$mreg_var1)) %>% names)
  })

  fm <- reactive({
    paste(input$mreg_var1, '~', paste(input$mreg_var2,collapse = '+'))
  })
  mlr <- reactive({
    lm(as.formula(fm()), data = datasetInput())
  })

  output$o_mreg <- renderPrint({
    summary(mlr())
  })


  output$o_mreg_hip <- renderPlot({
    diagPlts <- diagPlot4(mlr())
    do.call(grid.arrange, c(diagPlts,  ncol = 2))
  })

  output$o_mreg_int_conf  <- renderPrint({
    confint(mlr())
  })

  output$lreg_input1 <- renderUI({
    selectInput("lreg_var1", "Seleccionar variable dependiente:", as.list(datasetInput() %>% select_if(is.factor) %>%
                                                                            select_if(.predicate = function(x) length(levels(x))==2) %>%
                                                                            names))
  })

  output$lreg_input2 <- renderUI({
    selectInput("lreg_var2", "Seleccionar regresores:", as.list(datasetInput() %>% select(-one_of(input$mreg_var1)) %>% names),
                multiple = TRUE, selected = datasetInput() %>% select(-one_of(input$mreg_var1)) %>% names)
  })

  fm_l <- reactive({
    paste(input$lreg_var1, '~', paste(input$lreg_var2,collapse = '+'))
  })
  lr <- reactive({
    glm(as.formula(fm_l()), family = "binomial", data = datasetInput())
  })

  output$o_model_log_multi <- renderPrint({
    summary(lr())
  })

  output$o_ci_log_multi <- renderPrint({
    confint(lr())
  })

  output$o_or_log_multi <- renderPrint({
    exp(coef(lr()))
  })





  })

