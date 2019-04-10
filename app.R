library(shiny)
library(shinythemes)
library(pwr)
library(ggplot2)
library(dplyr)
library(DT)
library(tidyverse)

biases <- list(
  "0.1" = list(name = "Low (0.1) (e.g. high quality RCT)", value = 0.1),
  "0.3" = list(name = "Medium (0.3) (e.g. epidemiological study)", value = 0.3),
  "0.5" = list(name = "High (0.5)", value = 0.5),
  "0.8" = list(name = "Very High (0.8) (e.g. high multiplicity exploratory or poorly performed RCT)", value = 0.8)
)

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel(
    fluidRow(
      column(
        12,
        h2("Reproducibility and Grant ROI"),
        h5(
          a(href="http://www.rationally.io", "By Rationally."),
          tags$span(" Calculations based on "),
          a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.0020124", "Ioannidis 2005.")
        )
      )
    ),
    windowTitle = "Study Power, reproducibility and grant ROI"
  ),
  hr(),
  sidebarPanel(
    tabsetPanel(type = "pills",
      tabPanel(
        "Main", 
        hr(),
        sliderInput(
          "TDV",
          label="True discovery value (TDV)",
          min = 0,
          max = 100,
          post  = "m $",
          value = 50,
          step = 5
        ),
        sliderInput(
          "TNV",
          label="True negative value (TNV)",
          min = 0,
          max = 10,
          post  = "m $",
          value = 1,
          step = 0.5
        ),
        sliderInput(
          "RWFDC",
          label="Cost of a false discovery in the real world (RWFDC)",
          min = 0,
          max = 100,
          post  = "m $",
          value = 30,
          step = 5
        ),
        hr(),
        fluidRow(
          column(
            6,
            selectInput(
              "es",
              "Effect size (Cohen's d):",
              c(
                "Very Small (0.1)" = 0.1,
                "Small (0.3)" = 0.3,
                "Medium (0.5)" = 0.5,
                "Large (0.8)" = 0.8,
                "Very Large (1.2)" = 1.2
              ),
              selected = "0.5"
            )
          ),
          column(
            6,
            selectInput(
              "biases",
              "Bias values to show",
              lapply(biases, function(x) { x$value }),
              multiple = TRUE,
              selected = c(0.1, 0.3, 0.5, 0.8)
            )
          )
        ),
        selectInput(
          "R",
          "Pre-study odds (R):",
          c(
            "1:1 (e.g. phase III RCT)" = 1,
            "1:5 (e.g. phase I/II RCT)" = 0.2,
            "1:10 (e.g. exploratory epidemiological study)" = 0.1,
            "1:100" = 0.01,
            "1:1,000 (e.g. discovery-oriented exploratory research)" = 0.001
          ),
          selected = 0.1
        )   
      ),
      tabPanel(
        "Research Funnel",
        hr(),
        sliderInput(
          "SRP",
          label="Max subsequent research phases (SRP)",
          min = 0,
          max = 10,
          value = 5,
          step = 1
        ),
        sliderInput(
          "BCR",
          label="Bias correction rate (BCR)",
          min = 0,
          max = 0.5,
          value = 0.1,
          step = 0.05
        ),
        sliderInput(
          "PCR",
          label="Power correction rate (PCR)",
          min = 0,
          max = 0.5,
          value = 0.1,
          step = 0.05
        ),
        sliderInput(
          "CPPE",
          label="Per-subject cost escalation (CPPE)",
          min = 0,
          max = 200,
          value = 50,
          post  = " %",
          step = 10
        ),
        sliderInput(
          "PGE",
          label="Percent grant for per-subject costs escalation (PGE)",
          min = 0,
          max = 100,
          value = 20,
          post  = " %",
          step = 5
        ),
        numericInput("maxCPP", "Max cost per subject ($)", value = 5000)
      ),
      tabPanel(
        "Grant", 
        hr(),
        fluidRow(
          column(
            6,
            numericInput("minGrant", "Min grant ($)", value = 150000)
          ),
          column(
            6,
            numericInput("maxGrant", "Max grant ($)", value = 1500000)
          )
        ),
        numericInput("cpp", "Per-subject cost ($)", value = 1000),
        sliderInput(
          "percentGrant",
          label="Percent grant for per-subject costs (percentGrant)",
          min = 0,
          max = 100,
          value = 50,
          post  = " %",
          step = 10
        )
      )
    )
  ),

  mainPanel(
    tabsetPanel(type = "tabs",
      tabPanel(
        "Plot",
        br(),
        plotOutput("graph", click = "graph_click"),
        hr(),
        tags$samp(
          htmlOutput("showInput")
        ),
        hr(),
        h5("Point data"),
        tableOutput("infoTable"),
        hr(),
        h6("False discovery funnel"),
        tableOutput("funnelTable") 
      ),
      tabPanel("Data Table", br(), dataTableOutput("table")),
      tabPanel(
        "About",
        h3("About"),
        p("Use this model to explore the relationship between study power, bias and grant ROI.
          Underpowered studies (i.e. too few participants) can result in negative ROI and
          overpowered studies aren't cost effective. Bias decreases ROI, but only when one
          considers the downstream costs of a false positive."),
        h3("Parameters"),
        tags$ul(
          tags$li(
            tags$b("True discovery value (TDV) - "),
            tags$span("what is the value of a discovery that is in fact true?")
          ),
          tags$li(
            tags$b("True negative value (TNV) - "),
            tags$span("what is the value a true negative?")
          ),
          tags$li(
            tags$b("Effect size - "),
            tags$span("average effect size measured in funded research")
          ),
          tags$li(
            tags$b("Pre-study odds (R) - "),
            tags$span("Prior probability that the tested hypothesis is true")
          ),
          tags$li(
            tags$b("Bias (u) - "),
            tags$span("% of positive findings arising because of design, analysis or reporting bias")
          ),
          tags$li(
            tags$b("Cost of a false discovery in the real world (RWFDC) - "),
            tags$span("what is the cost if a false discovery makes it past all research stages and into the real world, e.g. a pharmaceutical with no real clinical value making it to market?")
          ),
          tags$li(
            tags$b("Subsequent research phases (SRP) - "),
            tags$span("assuming a positive result, how many more phases of research before finding is released into the real world?")
          ),
          tags$li(
            tags$b("Power correction rate (PCR) - "),
            tags$span("for each subsequent research phase, how much higher is the power than the previous?")
          ),
          tags$li(
            tags$b("Bias correction rate (BCR) - "),
            tags$span("for each subsequent research phase, how much is biased reduced?")
          )
        ), 
        h3("Calculation of value"),
        tags$h5("Value = Positive Finding Value (PFV) + Negative Finding Value (NFV)"),
        tags$span("where"),
        tags$ul(
          tags$li(
            tags$h5("PFV = PFR * ((PPV * TDV) - (FDR * (WRC + RFDIRL * RWFDC)))"),
            tags$ul(
              tags$li(
                tags$span("PFR = positive finding rate = TPR + FPR"),
                tags$ul(
                  tags$li("TPR = true positive rate = ([1 - β]R + uβR) / (R + 1)"),
                  tags$li("FPR = false positive rate = (α + u(1 - α)) / (R + 1)")
                )
              ),
              tags$li(
                tags$span("PPV = positive predictive value = ([1 - β]R + uβR)/(R + α − βR + u − uα + uβR)")        
              ),
              tags$li("FDR = false discovery rate = 1 - PPV"),
              tags$li("WRC = wasted research cost = recursively calculated costs of subsequent research due to false discovery"),
              tags$li("RFDIRL = Risk of false discovery getting past all research stages and into the real world"),
              tags$li("RWFDC = Cost of a false discovery in the real world")
            )
          )
        ),
        tags$ul(
          tags$li( 
            tags$h5("NFV = (TNR * TNV) - (FNR * TDV)"),
            tags$ul(
              tags$li("TNR = true negative rate = ((1 - u) * (1 - α)) / (R + 1)"),
              tags$li("FNR = false negative rate = (1 - u)βR / (R + 1)"),
              tags$li("TDV = value of a true discovery, effectively lost due to false negative")
            )
          )
        ),
        tags$div(
          tags$span("Power calculation done with "),
          a(href="https://www.rdocumentation.org/packages/pwr/versions/1.2-2", "R library pwr")
        ),
        hr(),
        h3("Model limitations"),
        tags$ul(
          tags$li("Assumes one experiment per grant, which is highly unlikely for exploratory research."),
          tags$li("Assumes entire grant funds per-subject direct costs - i.e. this model ignores overhead of all sorts."),
          tags$li("Does not consider the costs of effect size inflation (common in the case of false discovery in an underpowered studies)."),
          tags$li("The value of true discoveries is hard to estimate, yet this model relies on it heavily."),
          tags$li("The value of true negatives and costs of false discoveries are even harder to estimate, yet this model is quite sensitive to these parameters."),
          tags$li("Research funnel costs are not considered in the true discovery value calculation.")
        ),
        h3("Conceptual limitations"),
        tags$ul(
          tags$li("'Average' does not capture the likely nonlinearity of costs/benefits of false/true discoveries, e.g. non-linear increase in epistemic and reputation costs in relation to the density of false positives."),
          tags$li("Type I and Type II errors are a limited and flawed assessment of study results."),
          tags$li("Relatedly, this model suffers from dichotomania: for simplicity, it treats trueness/falsity as binary designations rather than as a continuum."),
          tags$li("Relatedly, the very notion of 'negative finding' conflates absence of evidence with evidence of absence.")
        ),
        h3("Other Assumptions"),
        tags$ul(
          tags$li("2-sample (50/50) non-paired design evaluated with a T-test of means"),
          tags$li("α = .05")
        ),
        hr(),
        tags$blockquote(
          p("All models are wrong, but some are useful."),
          tags$small("George E.P. Box")
        ),
        hr(),
        h4("Citations"),
        p(
          tags$span("Ioannidis, J. P. A. (2005). Why Most Published Research Findings Are False. PLoS Medicine, 2(8), e124. "),
          tags$a(href="https://doi.org/10.1371/journal.pmed.0020124", "10.1371/journal.pmed.0020124")
        ),
        hr(),
        h5(
          tags$span("If you see an error or want to say hi for another reason, please "),
          a(href="mailto:kristin@rationally.io", "email me.")
        )
      )
    )
  )
)

# Constants
α <- 0.05
EffectSize <- 0.50
StudyArmCount <- 2

MaxDots <- 40
Data <- tibble()

## start true effect
# True positives
TP <- function(power, R, u) {
  β <- Beta(power)

  # ([1 - β]R + uβR) / (R + 1)
  numer <- (power * R) + (u * β * R)
  return(numer / (R + 1));
}

# True positive rate
TPR <- function(power, R, u) {
  return(TP(power, R, u) / Total(power, R, u))
}

# False negatives
FN <- function(power, R, u) {
  β <- Beta(power)

  # (1 - u)βR/(R + 1)
  numer <- (1 - u) * β * R
  return(numer / (R + 1))
}

# False negative rate
FNR <- function(power, R, u) {
  return(FN(power, R, u) / Total(power, R, u))
}
## end true effect

## a null effect
# False positives
FP <- function(R, u) {
  # (α + u(1 - α)) / (R + 1)
  numer <- α + (u * (1 - α))
  return(numer / (R + 1))
}

# False positive rate
FPR <- function(power, R, u) {
  return(FP(R, u) / Total(power, R, u));
}

# True negatives
TN <- function(R, u) {
  # ((1 - u) * (1 - α)) / (R + 1)
  numer <- (1 - u) * (1 - α)
  return(numer / (R + 1))
}

# True negative rate
TNR <- function(power, R, u) {
  return(TN(R, u) / Total(power, R, u))
}
## end null effect

# Total positive findings
PF <- function(power, R, u) {
  # (R + α - βR + u - uα + uβR) / (R + 1)
  return(TP(power, R, u) + FP(R, u))
}

# Total negative findings
NF <- function(power, R, u) {
  return(TN(R, u) + FN(power, R, u))
}

Total <- function(power, R, u) {
  return(PF(power, R, u) + NF(power, R, u))
}

PFR <- function(power, R, u) {
  return(PF(power, R, u) / Total(power, R, u))
}

Beta <- function(power) {
  return(1 - power)
}

PPV <- function(power, R, u) {
  # 10.1371/journal.pmed.0020124
  # ([1 - β]R + uβR)/(R + α − βR + u − uα + uβR)

  β <- Beta(power)

  # ([1 - β]R + uβR)
  numer <- power * R + (u * β * R);

  # (R + α − βR + u − uα + uβR)
  denom <- R + α - (β * R) + u - (u * α) + (u * β * R);
  return(numer / denom);
}

## false discovery rate
FDR <- function(power, R, u) {
  return(1 - PPV(power, R, u))
}

ExpectedValue <- function(Tot.FDC, Tot.TDV, Tot.FNC, Tot.TNV) {
  return(fix(Tot.TDV - Tot.FDC - Tot.FNC + Tot.TNV))
}

NextPhaseCosts <- function(...) {
  pcData <- tibble(
    remaining = integer(),
    grant = numeric(),
    cpp = numeric(),
    R = numeric(),
    bias = numeric(),
    power = numeric(),
    fdr = numeric(),
    waste = numeric(),
    note = "",
  )

  inner <- function(power, grant, R, u, cpp, SRP, PCR, BCR, CPPE, percentGrant, PGE, maxCPP, ...) {
    waste <- 0
    nextR <- PPV(power, R, u)
    nextCPP <- min(maxCPP, (CPPE + 1) * cpp)
    nextPower = min(0.9, power + PCR)
    nextPercentGrant = min(0.9, percentGrant + PGE)
    nextGrant <- (nextCPP * SampleSizeByPower(nextPower) * StudyArmCount) / nextPercentGrant
    nextU <- max(0.1, u - BCR)
    currentFDR <- FDR(power, R, u)
    nextFDR <- FDR(nextPower, nextR, nextU)
    
    # more research is pointless
    # likely due to good behavior (low bias, high power)
    earlyExit <- SRP > 0 && ((currentFDR - nextFDR) * RWFDC) < nextGrant
    exit <- earlyExit

    if (SRP > 0 && !earlyExit) {
      Recall(
        power = nextPower,
        R = nextR,
        u = nextU,
        grant = nextGrant,
        cpp = nextCPP,
        SRP = SRP - 1,
        PCR = PCR,
        BCR = BCR,
        CPPE = CPPE,
        percentGrant = nextPercentGrant,
        PGE = PGE,
        maxCPP = maxCPP
      )
      waste <- nextGrant
    } else {
      # final waste/cost for false discoveries that make it through
      # all stages of research and into the real world
      exit <- TRUE
      waste <- nextFDR * RWFDC
    }
    pcData <<- add_row(
      pcData,
      remaining = as.integer(SRP),
      bias = nextU,
      power = nextPower,
      grant = ifelse(!exit, nextGrant, 0),
      R = nextR,
      fdr = nextFDR,
      cpp = ifelse(!exit, nextCPP, 0),
      waste = waste,
      note = ifelse(earlyExit, "early exit", "")
    )
    return(waste)
  }

  inner(...)
  return(pcData)
}

TrueDiscoveryValue <- function(power, TDV, R, u, ...) {
  return(PPV(power, R, u) * TDV * PFR(power, R, u))
}

FalseNegCost <- function(power, TDV, R, u, ...) {
  return(TDV * FNR(power, R, u))
}

TrueNegValue <- function(power, TNV, R, u, ...) {
  return(TNR(power, R, u) * TNV)
}

Power <- function(ss) {
  return(fix(pwr.t.test(
    n=ss,
    d=EffectSize,
    sig.level=α,
    type="two.sample",
    alternative="two.sided"
  )$power))
}

SampleSizeByPower <- function(power) {
  return(fix(pwr.t.test(
    power=power,
    d=EffectSize,
    sig.level=α,
    type="two.sample",
    alternative="two.sided"
  )$n))
}

SampleSize <- function(grant, cpp, percentGrant, ...) {
  total <- (grant * percentGrant) / cpp
  return (total / StudyArmCount)
}

fix <- function(x) {
  return(ifelse(!is.na(x), x, 0))
}

round_any <- function(x, accuracy, f = round){
  f(x / accuracy) * accuracy
}

getDataPerU <- function (data, R, u, ...) {
  return(data %>% mutate(
    R = R,
    u = u,
    TPR = TPR(power, R = R, u = u),
    FPR = FPR(power, R = R, u = u),
    TNR = TNR(power, R = R, u = u),
    FNR = FNR(power, R = R, u = u),
    Tot.TDV = TrueDiscoveryValue(power, R = R, u = u, ...),
    Tot.FNC = FalseNegCost(power, R = R, u = u, ...),
    Tot.TNV = TrueNegValue(power, R = R, u = u, ...)
  ))
}

getPerBiasData <- function(data, cpp, selectedBiases, ...) {
  datalist = list()

  for (i in biases) {
    if (is.element(paste(i$value), selectedBiases)) {
      u = paste(i$value)
      dat <- getDataPerU(
        u = i$value,
        data = data,
        cpp = cpp,
        ...
      )
      dat$u <- u
      datalist[[u]] <- dat
    }
  }

  return(do.call(rbind, datalist))
}

getData <- function(cpp, maxGrant, minGrant, ...) {
  data <- tibble(grant = seq.int(
    from=minGrant,
    to=maxGrant,
    by=max(cpp, round_any((maxGrant / MaxDots), cpp))
  ))
  data <- data %>% mutate(power = Power(SampleSize(grant, cpp, ...)))
  data <- getPerBiasData(data = data, cpp = cpp, ...)

  # rowwise because of recursion... maybe there is a better way?
  data <- data %>% rowwise() %>% mutate(
    Tot.FDC = sum(NextPhaseCosts(
      power = power,
      grant = grant,
      u = as.numeric(u),
      cpp = cpp,
      ...
    )$waste) * FPR
  )

  Data <<- data %>% mutate(
    ExpectedValue = ExpectedValue(Tot.FDC, Tot.TDV, Tot.FNC, Tot.TNV) - grant,
    ROI = ExpectedValue / grant
  )
}

getPlot <- function(cpp) {
  return (
    ggplot2::ggplot(Data, ggplot2::aes(x=grant, y=ROI, group=u, color=u)) +
    ggplot2::ylab("Net ROI") +
    ggplot2::scale_y_continuous(
      # trans = asinh_trans(),
      labels = scales::percent,
      limits = c(min(0, min(Data$ROI)), max(Data$ROI))
    ) +
    ggplot2::geom_line(size=1, na.rm = TRUE) +
    ggplot2::geom_point(size=1.75, na.rm = TRUE) +
    ggplot2::xlab("Grant Amount") +
    ggplot2::geom_vline(
      xintercept = Data$grant[min(which(Data$power > 0.8))],
      linetype=2,
      size=0.5,
      colour="gray"
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::dollar,
      limits = c(min(Data$grant), max(Data$grant) + cpp)
    ) +
    scale_color_manual(
      values = c(
          "0.1" = "springgreen4",
          "0.3" = "springgreen3",
          "0.5" = "orange",
          "0.8" = "red"
      )
    )
  )
}

formatRound <- function(x, places) {
  format(round(x, places), nsmall = places, scientific = FALSE, big.mark=",")
}

clean <- function(x, digits) {
  count <- nchar(formatRound(x, 2))
  if (!is.null(count) && length(count) > 0 && count > 6) {
    return(formatRound(x, 0))
  }
  return(formatRound(x, digits))
}

formatInput <- function(inputs) {
  inp = c()
  for (name in names(inputs)) {
    if (!grepl("graph_click", name) && !grepl("biases", name) && !grepl("table", name) && !is.null(inputs[[name]])) {
      inp <- c(inp, paste(tags$b(name), inputs[[name]]))
    }
  }
  return(paste(inp, "|"))
}

server <- function(input, output, session) {
  data <- reactive({
    EffectSize <<- as.numeric(input$es)
    RWFDC <<- (input$RWFDC * 1000 * 1000)
    getData(
      cpp = input$cpp,
      maxGrant = input$maxGrant,
      minGrant = input$minGrant,
      TDV = input$TDV * 1000 * 1000,
      TNV = input$TNV * 1000 * 1000,
      R = as.numeric(input$R),
      selectedBiases = input$biases,
      PCR = input$PCR,
      SRP = input$SRP,
      BCR = input$BCR,
      CPPE = input$CPPE / 100,
      PGE = input$PGE / 100,
      percentGrant = input$percentGrant / 100,
      maxCPP = input$maxCPP
    )
  })

  output$graph <- renderPlot({
    data()
    getPlot(input$cpp) + theme_light(base_size = 18)
  })

  output$showInput <- renderUI({
    i <- reactiveValuesToList(input)
    inputDisp <- formatInput(i)
    HTML(inputDisp)
  })

  toKeep <- c("grant", "u", "power", "TPR", "FPR", "TNR", "FNR", "Tot.TDV", "Tot.TNV", "Tot.FDC", "Tot.FNC", "ROI")
  output$table <- renderDataTable({
    datatable(data() %>% select(one_of(toKeep)) %>% mutate_if(is.numeric, formatRound, 2))
  })

  output$infoTable <- renderTable({
    row <- nearPoints(data(), input$graph_click, threshold = 10, maxpoints = 1, addDist = FALSE)
    if (nrow(row) > 0) {
      row %>% select(one_of(toKeep)) %>% mutate_if(is.numeric, clean, 2)
    }
  })

  output$funnelTable <- renderTable({
    row <- nearPoints(data(), input$graph_click, threshold = 10, maxpoints = 1, addDist = FALSE)
    if (nrow(row) > 0) {
      NextPhaseCosts(
        power = row$power,
        grant = row$grant,
        R = as.numeric(input$R),
        u = as.numeric(row$u),
        cpp = input$cpp,
        SRP = input$SRP,
        PCR = input$PCR,
        BCR = input$BCR,
        CPPE = input$CPPE / 100,
        PGE = input$PGE / 100,
        percentGrant = input$percentGrant / 100,
        maxCPP = input$maxCPP
      ) %>% mutate_if(is.numeric, clean, 2)
    }
  })
}

shinyApp(ui, server)
