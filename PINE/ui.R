

library(shiny)
library(DT)

fluidPage(
  titlePanel(
    fluidRow(
      column(9,
             'Panel Interactivo de aNálisis Econométrico (PINE)'),
      column(3,
             img(height = 55, width = 200, src = "ulpgc.jpg"))
      ),
    'PINE'),
  theme = shinythemes::shinytheme("cosmo"),
  navbarPage(
    "PINE",
    tabPanel('Introducción',
             div(
             p('Herramienta interactiva diseñada para dar soporte a la docencia de las asignaturas
             de estadística y econometría.'),
             p('Recoge los principales análisis univariantes, bivariantes y multivariantes de bases
               de datos tratadas en ',
             a(href = 'https://www.cengage.co.uk/books/9781305270107/',
               'Introductory Econometrics: A Modern Approach (Jeffrey Wooldrige,2016)'),
             'y recogidas en la librería ',
             a(href = 'https://justinmshea.github.io/wooldridge/', 'wooldridge'), 'de ',
             a(href = 'https://cran.r-project.org/', 'R'),'.'),
             br(),
             div(img(src = 'econometrics-image-1.jpg', height = 225, width = 480), style = "text-align: center;"),
             br(),
             p('Desarrollo en',
               a(href = 'https://shiny.rstudio.com/', ' Shiny'),
             img(src = 'shiny_logo.jpg', height = 28.5, width = 33),
             'creado por los profesores Christian González, Jaime Pinilla, Miguel Ángel Negrín
               del Departamento de ',
             a(href = 'http://www.dmc.ulpgc.es/', 'Métodos Cuantitativos en Economía y Gestión '),
             'y José María Pérez del ',
             a(href = 'https://www2.ulpgc.es/index.php?pagina=daea&ver=inicio', 'Departamento de
               Análisis Económico'),
             ', todos ellos pertencientes a la ',
             a(href = 'https://www.ulpgc.es/node', 'Universidad de Las Palmas de Gran Canaria')
             ),
             style = "font-size: large")
             ),
    tabPanel("Dataset",
             sidebarLayout(
               sidebarPanel(
                 selectInput('dataset',label = h3("Seleccionar dataset"),
                             choices = c('wage1', 'jtrain'))),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Tabla",
                            DTOutput("o_data_table")
                            ),
                   tabPanel('Descripción variables',
                            htmlOutput('o_iframe')
                   ),
                   tabPanel("Análisis descriptivo",
                            h4('Número de filas y columnas'),
                            verbatimTextOutput('o_data_nfc'),
                            h4('Variables'),
                            verbatimTextOutput('o_data_var_names'),
                            h4('Metadata'),
                            verbatimTextOutput('o_data_str_df'),
                            h4('Descriptivos'),
                            verbatimTextOutput('o_data_summary_df')),
                   tabPanel("Primeros y últimos datos",
                            h4('Primeros 5 datos'),
                            tableOutput('o_data_head5_df'),
                            h4('Últimos 5 datos'),
                            tableOutput('o_data_tail5_df')
                            )
                   )
                 )
               )
             ),
    tabPanel("Univariante",
             tabsetPanel(
               tabPanel('Categóricas',
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput('categorica_input')
                           ),
                           mainPanel(
                             fluidRow(
                               column(7,
                                      plotOutput('cat_plot')
                                      ),
                               column(5,
                                      h4('Tabla cruzada'),
                                      verbatimTextOutput('tabla_cruzada_cat'),
                                      selectInput('int_conf_cat','Nivel de confianza',choices = c(0.99,0.95,0.90)),
                                      selectInput('tipo_cont_cat','Tipo de contraste',
                                                  choices = c('dos colas' = 'two.sided', 'mayor' = 'greater', 'menor' = 'less')),
                                      h4('Test de proporciones'),
                                      verbatimTextOutput('test_prop_cat')
                                      )
                               )
                             )
                          )
                        ),
             tabPanel('Descriptivos',
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('medida_input')
                        ),
                        mainPanel(
                          fluidRow(
                            column(6,
                                   h4('Cuartiles, media, máximo y mínimo'),
                                   verbatimTextOutput('medidas_3m'),
                                   column(4, h4('Desviación típica'),
                                          verbatimTextOutput('medidas_sd')),
                                   column(4, h4('Varianza'),
                                          verbatimTextOutput('medidas_var')),
                                   column(4, h4('Rango Intercuartíl'),
                                          verbatimTextOutput('medidas_ri'))
                            ),
                            column(6,
                                   h4('Moda'),
                                   verbatimTextOutput('medidas_moda'),
                                   column(4, h4('Asimetría'),
                                          verbatimTextOutput('medidas_asimetria')),
                                   column(4, h4('Curtosis'),
                                          verbatimTextOutput('medidas_cur')),
                                   column(4, h4('Rango medio'),
                                          verbatimTextOutput('medidas_rm'))

                            )
                            )
                          )
                        )
                      ),
             tabPanel('T-test',
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('ttest_input')
                        ),
                        mainPanel(
                          fluidRow(
                            column(6,
                                   h4('Test de la t de Student para una muestra'),
                                   numericInput('media_1m_ttest','H0: Media = ',0),
                                   verbatimTextOutput('o_1m_ttest')

                            ),
                            column(6,
                                   h4('Prueba Z para una muestra'),
                                   numericInput('media_1m_ztest','H0: Media = ',0),
                                   numericInput('sd_1m_ztest','Desviación típica ',1),
                                   verbatimTextOutput('o_1m_ztest')
                                   # h4('Potencia del test de la t de Student'),
                                   # numericInput('diff_media_ttest','Diferencia en media',0),
                                   # selectInput('tipo_cont_ttest','Tipo de contraste',
                                   #             choices = c('dos colas' = 'two.sided', 'una cola' = 'one.side')),
                                   # verbatimTextOutput('o_power_ttest'),
                                   # h4('Potencia de la prueba Z'),
                                   # numericInput('media_power_ztest','H0: Diferencia de la media = ',0),
                                   # numericInput('sd_power_ztest','Desviación típica ',1),
                                   # verbatimTextOutput('o_power_ztest')
                            )
                          )
                        )
                      )
                      ),
             tabPanel('Wilcoxon',
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('wilcoxon_input')
                        ),
                        mainPanel(
                          h4('Prueba de los rangos con signo de Wilcoxon'),
                          numericInput('dif_median_wil','Diferencia en mediana',0),
                          selectInput('tipo_cont_wil','Tipo de contraste',
                                      choices = c('dos colas' = 'two.sided', 'mayor' = 'greater', 'menor' = 'less')),
                          selectInput('int_conf_wil','Nivel de confianza',
                                      choices = c('0.99' = 0.99, '0.95' = 0.95, '0.90' = 0.90)),
                          verbatimTextOutput('o_wil_test')
                        )
                      )
                      ),
             tabPanel('Diagrama de caja y violín',
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('violin_input')
                          ),
                        mainPanel(
                          column(6,
                                 plotOutput('o_boxplot_violin')
                                 ),
                          column(6,
                                 plotOutput('o_violin_violin')
                                 )
                          )
                        )
                      ),
             tabPanel('Histograma y q-q plot',
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('dist_input')
                          ),
                        mainPanel(
                          column(6,
                                 h4('Histograma'),
                                 plotOutput('o_hist_dist')
                                 ),
                          column(6,
                                 h4('Gráfico QQ'),
                                 plotOutput('o_qqplot_dist')
                                 )
                          )
                        )
                      ),
             tabPanel('Gráfico secuencial y de retardos',
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('lsp_input')
                        ),
                        mainPanel(
                          column(6,
                                 h4('Gráfico de retardos'),
                                 plotOutput('o_retardos_lsp')),
                          column(6,
                                 h4('Gráfico secuencial'),
                                 plotOutput('o_secuencial_lsp'))

                        )
                        )
                      ),
             tabPanel('Test de bondad del ajuste',
                      sidebarLayout(
                        sidebarPanel(
                          uiOutput('tba_input'),
                          selectInput('i_curva_tba','Distribución',
                                      choices = c('Normal' = 'normal', 'Cauchy' = 'cauchy',
                                                  'Logística' = 'logistic', 'Exponencial' = 'exponential',
                                                  'Weibull' = 'weibull'))
                          ),
                        mainPanel(
                          column(6,
                                 h3('Ajuste de distribuciones'),
                                 h4('Método de los momentos'),
                                 verbatimTextOutput('o_momentos_tba'),
                                 h4('Método de máxima verosimilitud'),
                                 verbatimTextOutput('o_max_ver_tba'),
                                 h3('Test de bondad del ajuste'),
                                 h4('Kolmogorov-Smirnov Test'),
                                 verbatimTextOutput('o_ks_tba')
                          ),
                          column(6,
                                 h4('Ajuste de la curva'),
                                 plotOutput('o_bondad_tba')
                                 # h4('Test de la Chi-Cuadrado'),
                                 # verbatimTextOutput('o_chi_tba')
                          )
                          )
                        )
                      )
             )
             ),
    tabPanel("Bivariante",
             tabsetPanel(
               tabPanel('Numérica - Numérica',
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput('nnbiv_input1'),
                            uiOutput('nnbiv_input2')
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel('Bihistograma',
                                       plotOutput('o_bihist_bi')
                                       ),
                              tabPanel('Diagrama de dispersión',
                                       column(8,
                                              plotOutput('o_sp_bi')),
                                       column(4,
                                              h4('Coeficiente de correlación'),
                                              verbatimTextOutput('o_sp_coef_bi'))
                                       ),
                              tabPanel('Regresión lineal',
                                       column(4,
                                              verbatimTextOutput('o_rl_bi')),
                                       column(8,
                                              plotOutput('o_rl_plot_bi'))
                                       ),
                              tabPanel('Prueba de igualdad de varianzas',
                                       column(6,

                                              selectInput('tipo_cont_vartst','Tipo de contraste',
                                                          choices = c('dos colas' = 'two.sided', 'mayor' = 'greater', 'menor' = 'less'))
                                       ),
                                       column(6,

                                              selectInput('int_conf_vartst','Nivel de confianza',
                                                          choices = c('0.99' = 0.99, '0.95' = 0.95, '0.90' = 0.90))
                                       ),
                                       h4('F Test'),
                                       verbatimTextOutput('o_ftest_vartst')
                                       ),
                              tabPanel('Prueba de la t de Student',
                                       h4('Prueba t de Student para dos muestras'),
                                       fluidRow(
                                         column(4,

                                                selectInput('tipo_cont_tst_bi','Tipo de contraste',
                                                            choices = c('dos colas' = 'two.sided', 'mayor' = 'greater', 'menor' = 'less')),
                                                checkboxInput('paired_tst_bi', '¿Muestras relacionadas?', value = FALSE)
                                         ),
                                         column(4,

                                                selectInput('int_conf_tst_bi','Nivel de confianza',
                                                            choices = c('0.99' = 0.99, '0.95' = 0.95, '0.90' = 0.90)),
                                                checkboxInput('variance_tst_bi', '¿Igualdad de varianzas?', value = FALSE)
                                         ),
                                         column(4,

                                                numericInput('mean_dif_tst_bi','Diferencia en media',
                                                             value = 0)
                                         )

                                       ),
                                       hr(),
                                       h4('Resultados'),
                                       fluidRow(
                                         column(6,
                                                plotOutput('o_plot_tst_bi')
                                         ),
                                         column(6,
                                                verbatimTextOutput('o_tst_bi')
                                         )
                                         # Se podría incluir también densidad de la t y ver si la diferencia en medias
                                         # cae fuera del nivel de significación

                                       )
                                       ),
                              tabPanel('Prueba de la suma de rangos de Wilconxon',
                                       h4('Prueba Mann-Whitney-Wilcoxon'),
                                       fluidRow(
                                         column(4,

                                                selectInput('tipo_cont_wil_bi','Tipo de contraste',
                                                            choices = c('dos colas' = 'two.sided', 'mayor' = 'greater', 'menor' = 'less')),
                                                checkboxInput('paired_wil_bi', '¿Muestras relacionadas?', value = FALSE)
                                         ),
                                         column(4,

                                                selectInput('int_conf_wil_bi','Nivel de confianza',
                                                            choices = c('0.99' = 0.99, '0.95' = 0.95, '0.90' = 0.90))

                                         ),
                                         column(4,

                                                numericInput('mean_dif_wil_bi','Diferencia en mediana',
                                                             value = 0)
                                         )
                                       ),
                                       hr(),
                                       h4('Resultados'),
                                       fluidRow(
                                         column(6,
                                                plotOutput('o_plot_wil_bi')
                                         ),
                                         column(6,
                                                verbatimTextOutput('o_wil_bi')
                                         )
                                       )
                                       )

                            )
                          )
                        )
                        ),
               tabPanel('Categórica - Numérica',
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput('cn_input1'),
                            uiOutput('cn_input2')
                            ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel('Diagrama de cajas',
                                       plotOutput('o_boxplot_cn')
                                       ),
                              tabPanel('Gráfico de densidad',
                                       plotOutput('o_density_cn')
                                       ),
                              tabPanel('Regresión',
                                       fluidRow(
                                         column(6, verbatimTextOutput('o_rl_cn')
                                         ),
                                         column(6, plotOutput('o_rl_plot_cn')
                                         )
                                       )
                                       ),
                              tabPanel('ANOVA',
                                       h4('Anova (paramétrico)'),
                                       verbatimTextOutput('o_anova_cn'),
                                       h4('Anova (paramétrico). Tukey test.'),
                                       verbatimTextOutput('o_anova_tukey_cn'),
                                       h4('Krustal (no paramétrico)'),
                                       verbatimTextOutput('o_krustal_cn')
                                       )
                            )
                          )

                        )
                        ),
               tabPanel('Categórica - Categórica',
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput('cc_input1'),
                            uiOutput('cc_input2')
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel('Tabulación cruzada',
                                       br(),
                                       h4('Tabla Cruzada'),
                                       htmlOutput('o_tab_cruzada_bi', align = "center")
                                       ),
                              tabPanel('Gráfico de barras',
                                       plotOutput('o_barplot_cc_bi')
                                       ),
                              tabPanel('Prueba de la chi - cuadrado',
                                       h4('Prueba de la Chi Cuadrado'),
                                       verbatimTextOutput('o_x2_cc_bi'),
                                       h4('Esperados'),
                                       verbatimTextOutput('o_x2_xp_cc_bi'),
                                       h4('Residuos'),
                                       verbatimTextOutput('o_x2_res_cc_bi'),
                                       h4('Residuos estandarizados'),
                                       verbatimTextOutput('o_x2_std_cc_bi')
                                       ),
                              tabPanel('Prueba exacta de Fisher',
                                       h4('Prueba exacta de Fisher'),
                                       verbatimTextOutput('o_fisher_c_bi')
                                       )
                            )
                          )
                        )
                        )
             )
             ),
    tabPanel("Multivariante",
             tabsetPanel(
               tabPanel('Regresión lineal múltiple',
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput('mreg_input1'),
                            helpText('Por defecto están seleccionadas el resto de variables como regresores.'),
                            helpText('Para eliminar una variable como regresor, se debe seleccionar y pulsar suprimir.'),
                            uiOutput('mreg_input2')
                          ),
                          mainPanel(
                            fluidRow(
                              column(6,
                                     h4('Regresión lineal múltiple'),
                                     verbatimTextOutput('o_mreg')
                              ),
                              column(6,
                                     h4('Comprobación de las hipótesis de modelo'),
                                     plotOutput('o_mreg_hip')
                              )),
                            hr(),
                            fluidRow(
                              column(6,
                                     h4('Nivel de confianza'),
                                     verbatimTextOutput('o_mreg_int_conf')
                              )
                            )
                          )
                        )
                        ),
               #tabPanel('Análisis discriminante', 'Intencionadamente en blanco'),
               #tabPanel('Análisis de componentes principales', 'Intencionadamente en blanco'),
               #tabPanel('Análisis cluster', 'Intencionadamente en blanco'),
               tabPanel('Regresión logística',
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput('lreg_input1'),
                            helpText('Por defecto están seleccionadas el resto de variables como regresores.'),
                            helpText('Para eliminar una variable como regresor, se debe seleccionar y pulsar suprimir.'),
                            uiOutput('lreg_input2')
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel('Modelo',
                                       verbatimTextOutput('o_model_log_multi')
                                       ),
                              #tabPanel('Análisis de los residuos',
                              #         verbatimTextOutput()
                              #         ),
                              tabPanel('Odds ratio',
                                       h4('Intervalos de confianza'),
                                       verbatimTextOutput('o_ci_log_multi'),
                                       h4('Odd ratios'),
                                       verbatimTextOutput('o_or_log_multi')
                                       )
                            )
                          )
                        )
                        )
             )
             )
    )
)