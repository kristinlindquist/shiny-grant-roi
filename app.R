library(shiny)
library(shinythemes)
library(pwr)
library(ggplot2)
library(dplyr)
library(DT)
library("scales")

P1 <- c(name = "1:1 (e.g. phase III RCT)", remaining = 0, R = 1)
P2 <- c(name = "1:5 (e.g. phase I/II RCT)", remaining = 1, R = 0.2)
P3 <- c(name = "1:10 (e.g. exploratory epidemiological study)", remaining = 2, R = 0.1)
P4 <- c(name = "1:100", remaining = 3, R = 0.01)
P5 <- c(name = "1:1,000 (e.g. discovery-oriented exploratory research)", remaining = 5, R = 0.001)

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
        h2("Study Power, reproducibility and grant ROI"),
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
      "RWFDC",
      label="Cost of a false discovery in the real world (RWFDC)",
      min = 0,
      max = 100,
      post  = "m $",
      value = 10,
      step = 1
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
    ),
    selectInput(
      "phase",
      "Pre-study odds (R):",
      c(
        "1:1 (similar to that of a phase III RCT)" = "P1",
        "1:5 (similar to that of a phase I/II RCT)" = "P2",
        "1:10 (similar to that of an epidemiological study)" = "P3",
        "1:100" = "P4",
        "1:1,000 (e.g. discovery-oriented exploratory research)" = "P5"
      ),
      selected = "P3"
    ),
    selectInput(
      "biases",
      "Bias values to show",
      lapply(biases, function(x) { x$value }),
      multiple = TRUE,
      selected = c(0.1, 0.3, 0.5, 0.8)
    ),
    hr(),
    fluidRow(
      column(
        6,
        numericInput("minGrant", "Min grant ($)", value = 50000)
      ),
      column(
        6,
        numericInput("maxGrant", "Max grant ($)", value = 1000000)
      )
    ),
    numericInput("cpp", "Per-subject total cost ($)", value = 1000),
    sliderInput(
      "pctSample",
      label="% Grant going to per-subject costs",
      min = 0,
      max = 100,
      post  = " %",
      value = 50,
      step = 5
    )
  ),

  mainPanel(
    plotOutput("graph", click = "graph_click"),
    hr(),
    tags$samp(
      htmlOutput("showInput")
    ),
    hr(),
    verbatimTextOutput("info"),
    hr(),
    h3("About"),
    p("This is a simple model with which to explore the relationship between study power and grant ROI.
      Funding underpowered studies (i.e. too few participants) can have negative ROI. At the same time, overpowered studies
      aren't cost effective either. Note that this model relies on hard-to-predict but impactful parameters such as
      base rate, effect size and downstream costs of false negatives and positives. The point of this model is to understand
      how these parameters may relate, not to draw any hard conclusions."),
    tags$h5(
      tags$b(
        tags$span("Concept and PPV calculation from "),
        a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.0020124", "Ioannidis 2005")
      )
    ),
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
          tags$li("FNR = false negative rate = (1 - u)βR/(R + 1)"),
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
      tags$li("Assumes one experiment per grant, which is highly unlikely for exploratory research"),
      tags$li("Does not factor effect size into the value assessment of a true discovery; effect size is only used for Power calculation"),
      tags$li("Does not consider the costs of effect size inflation (common in the case of false discovery in an underpowered studies)")
    ),
    h3("Conceptual limitations"),
    tags$ul(
      tags$li("'Average' does not capture the likely nonlinearity of costs/benefits of false/true discoveries"),
      tags$li("Type I and Type II errors are a limited and flawed view into study results")
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
    h5(
      tags$span("If you see an error or want to say hi for another reason, please "),
      a(href="mailto:kristin@rationally.io", "email me.")
    )
  )
)

# Constants
α <- 0.05
PctSample <- 0.50
EffectSize <- 0.50

MaxDots <- 40
Data <- data.frame()

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

TotalValue <- function(Total.FDC, Total.TDV, Total.FNC, Total.TNV) {
  return(fix(Total.TDV - Total.FDC - Total.FNC + Total.TNV))
}

SubsequentCosts <- function(power, R, u, grant, cpp, remainingStages) {
  total <- 0

  nextStageR <- PPV(power, R, u)
  nextGrant <- grant * 2
  nextPower <- Power(SampleSize(nextGrant, cpp))
  nextU <- max(0.1, u - 0.2)
  
  nextStageFDR <- FDR(nextPower, nextStageR, nextU)
  
  if (remainingStages > 0) {
    remainingStages = remainingStages - 1
    nextStageCost = SubsequentCosts(nextPower, nextStageR, nextU, nextGrant, cpp, remainingStages)
    total = (nextGrant * nextStageFDR) + nextStageCost
  } else {
    # final cost for false discoveries that make it through
    # all stages of research and into the real world
    total = total + (nextStageFDR * RWFDC)
  }
  return(total);
}

FalseDiscoveryCost <- function(power, R, u, grant, cpp, remainingStages) {
  # FDR = prior odds for the next study that costs 3x.
  # the likelihood of failure * costs is the FDC.
  return(SubsequentCosts(power, R, u, grant, cpp, remainingStages) * FPR(power, R, u))
}

TrueDiscoveryValue <- function(power, TDV, R, u) {
  return(PPV(power, R, u) * TDV * PFR(power, R, u))
}

FalseNegCost <- function(power, TDV, R, u) {
  # the cost to get a new pos discovery in false neg's place
  # since *this* finding isn't valuable so much as *a* finding
  return(TDV * FNR(power, R, u))
}

TrueNegValue <- function(power, TNV, R, u) {
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

SampleSize <- function(grant, cpp) {
  studyArmCount <- 2
  total <- (grant * PctSample) / cpp
  return (total / studyArmCount)
}

fix <- function(x) {
  return(ifelse(!is.na(x), x, 0))
}

round_any <- function(x, accuracy, f = round){
  f(x / accuracy) * accuracy
}

getDataPer <- function (u, data, cpp, R, remainingStages, TDV, TNV) {
  return(data %>% mutate(
    Total.FDC = FalseDiscoveryCost(power, R, u, grant, cpp, remainingStages),
    Total.TDV = TrueDiscoveryValue(power, TDV, R, u),
    Total.FNC = FalseNegCost(power, TDV, R, u),
    Total.TNV = TrueNegValue(power, TNV, R, u),
    Total.Value = TotalValue(Total.FDC, Total.TDV, Total.FNC, Total.TNV),
    TPR = TPR(power, R, u),
    FPR = FPR(power, R, u),
    TNR = TNR(power, R, u),
    FNR = FNR(power, R, u),
    ROI = (Total.Value - grant) / grant
  ))
}

getData <- function(cpp, maxGrant, minGrant, selectedBiases, ...) {
  grant <- seq.int(
    from=minGrant,
    to=maxGrant,
    by=max(cpp, round_any((maxGrant / MaxDots), cpp))
  )
  data <- data.frame(grant)
  data <- data %>% mutate(power = Power(SampleSize(grant, cpp)))

  datalist = list()

  for (i in biases) {
    if (is.element(paste(i$value), selectedBiases)) {
      u = paste(i$value)
      dat <- getDataPer(u = i$value, data = data, cpp = cpp, ...)
      dat$u <- u
      datalist[[paste(u)]] <- dat
    }
  }
  Data <<- do.call(rbind, datalist)
}

getPlot <- function(cpp) {
  return (
    ggplot2::ggplot(Data, ggplot2::aes(x=grant, y=ROI, color=u)) +
    ggplot2::ylab("Net ROI") +
    ggplot2::scale_y_continuous(
      # trans = asinh_trans(),
      labels = scales::percent,
      limits = c(min(0, min(Data$ROI)), max(Data$ROI))
    ) +
    ggplot2::geom_line(size=0.75, na.rm = TRUE) +
    ggplot2::geom_point(colour="#474747", size=1, na.rm = TRUE) +
    ggplot2::xlab("Grant Amount") +
    ggplot2::geom_vline(
      xintercept = Data$grant[min(which(Data$power > 0.8))],
      linetype=2,
      size=0.5,
      colour="gray"
    ) +
    ggplot2::scale_x_continuous(
      labels=scales::dollar,
      limits = c(min(Data$grant), max(Data$grant) + cpp)
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
    if (!grepl("graph_click", name) && !grepl("biases", name) && !is.null(inputs[[name]])) {
      inp <- c(inp, paste(tags$b(name), inputs[[name]]))
    }
  }
  return(paste(inp, "|"))
}

server <- function(input, output) {
  data <- reactive({
    PctSample <<- (input$pctSample / 100)
    EffectSize <<- as.numeric(input$es)
    RWFDC <<- (input$RWFDC * 1000 * 1000)
    getData(
      cpp = input$cpp,
      maxGrant = input$maxGrant,
      minGrant = input$minGrant,
      TDV = input$TDV * 1000 * 1000,
      TNV = input$TNV * 1000 * 1000,
      R = as.numeric(get(input$phase)["R"]),
      remainingStages = as.numeric(get(input$phase)["remaining"]),
      selectedBiases = input$biases
    )
  })

  # (cpp, TDV, maxGrant, minGrant, R)

  output$graph <- renderPlot({
    data()
    getPlot(input$cpp) + theme_light(base_size = 18)
  })

  output$showInput <- renderUI({
    i <- reactiveValuesToList(input)
    inputDisp <- formatInput(i)
    HTML(inputDisp)
  })

  toKeep <- c("grant", "u", "power", "TPR", "FPR", "TNR", "FNR", "Total.TDV", "Total.TNV", "Total.FDC", "Total.FNC", "ROI")
  output$table <- renderDataTable({
    datatable(data() %>% select(one_of(toKeep)) %>% mutate_if(is.numeric, formatRound, 2))
  })

  output$info <- renderPrint ({
    row <- nearPoints(data(), input$graph_click, threshold = 10, maxpoints = 1, addDist = FALSE)
    return(row %>% select(one_of(toKeep)) %>% mutate_if(is.numeric, clean, 2))
  })
}

shinyApp(ui, server)
