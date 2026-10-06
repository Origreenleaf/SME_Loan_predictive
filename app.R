library(googlesheets4)
library(probably)
library(tidymodels)
library(gt)
library(DT)
library(shiny)
library(shinyvalidate)
library(bslib)
library(plotly)
library(e1071)
library(forcats)
library(readr)
library(FSelectorRcpp)
library(rsample)
library(tidyverse)


sa_path <- "service_account.json"

json <- Sys.getenv("GSHEETS_SERVICE_ACCOUNT_JSON")

if (!file.exists(sa_path) && nzchar(Sys.getenv("GSHEETS_SERVICE_ACCOUNT_JSON"))) {
  sa_path <- tempfile(fileext = ".json")
  writeLines(Sys.getenv("GSHEETS_SERVICE_ACCOUNT_JSON"), sa_path)
}

gs4_auth(path = sa_path)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1sGGWXEikLWgtwRVKm-uwQe-iLeIe7ux80YaSN6H05wI", 1)

df1 <- df %>%
  select(business_description, loan_purpose_description, repayment_plan_description,
         target_missed_payment, sector, monthly_profit_bdt, existing_loans,
         days_payable_outstanding, repayment_confidence, late_payment_history,
         owner_education)

df1 <- df1 %>%
  extract(business_description, into = "business_type", regex = "^(.+?) in ", remove = FALSE) %>%
  extract(business_description, into = c("location", "division"),
          regex = "in (.+?), (.+?) division\\.", remove = FALSE) %>%
  extract(business_description, into = c("years_operating", "employees_count"),
          regex = "Operating (\\d+) years with (\\d+) employees\\.", convert = TRUE, remove = FALSE) %>%
  extract(business_description, into = c("area_type", "market"),
          regex = "^.*? (Rural|Urban|Semi-urban) area serving (.+?)\\.", remove = FALSE) %>%
  extract(business_description, into = "monthly_revenue",
          regex = "Monthly revenue approximately (\\d+)K BDT\\.", convert = TRUE, remove = FALSE) %>%
  extract(business_description, into = "peak_sales",
          regex = "Peak sales during (.+?)\\.", remove = FALSE) %>%
  extract(business_description, into = "strengths",
          regex = "(Strong supplier relationships and customer loyalty built over years)\\.", remove = FALSE) %>%
  extract(business_description, into = "challenges",
          regex = "Challenges include (.+?)\\.", remove = FALSE) %>%
  extract(loan_purpose_description, into = "loan_amount",
          regex = "Requesting (\\d+)K BDT", convert = TRUE, remove = FALSE) %>%
  extract(loan_purpose_description, into = "loan_purpose",
          regex = "Requesting \\d+K BDT for (.+?)\\.", remove = FALSE) %>%
  extract(loan_purpose_description, into = "operations_capacity",
          regex = "Current operations (.+?)\\.", remove = FALSE) %>%
  extract(loan_purpose_description, into = "funding_need",
          regex = "Need funds to (.+?)\\.", remove = FALSE) %>%
  extract(loan_purpose_description, into = "market_opportunity",
          regex = "Market opportunity (.+?) with", remove = FALSE) %>%
  extract(loan_purpose_description, into = "projected_growth",
          regex = "with (\\d+)% projected revenue growth", convert = TRUE, remove = FALSE) %>%
  extract(loan_purpose_description, into = "supplier_discounts",
          regex = "Supplier (.+?)\\.", remove = FALSE) %>%
  extract(loan_purpose_description, into = "repayment_period",
          regex = "Repayment over (.+?) months from", convert = TRUE, remove = FALSE) %>%
  extract(loan_purpose_description, into = "repayment_source",
          regex = "from (.+?)\\.", remove = FALSE) %>%
  extract(repayment_plan_description, into = "monthly_installment",
          regex = "Monthly installment (\\d+)K BDT", convert = TRUE, remove = FALSE) %>%
  extract(repayment_plan_description, into = "installment_source",
          regex = "from (.+?)\\.", remove = FALSE) %>%
  extract(repayment_plan_description, into = "repayment_cushion",
          regex = "Peak season sales (.+?)\\.", remove = FALSE) %>%
  extract(repayment_plan_description, into = "revenue_for_servicing",
          regex = "Maintain (\\d+)% of revenue", convert = TRUE, remove = FALSE) %>%
  extract(repayment_plan_description, into = "previous_loans_repaid",
          regex = "(\\d+) previous loans repaid successfully", convert = TRUE, remove = FALSE) %>%
  extract(repayment_plan_description, into = "cashflow_stability",
          regex = "Cashflow stability rated (\\d+)/5", convert = TRUE, remove = FALSE) %>%
  extract(repayment_plan_description, into = "collateral_level",
          regex = "Owner committed with (.+?) collateral", remove = FALSE) %>%
  extract(repayment_plan_description, into = "growth_repayment",
          regex = "Business growth (.+?)\\.", remove = FALSE)

cols_to_factor <- c("market", "location", "division", "business_type", "challenges", "strengths",
                    "area_type", "peak_sales", "supplier_discounts", "repayment_source",
                    "market_opportunity", "funding_need", "operations_capacity",
                    "loan_purpose", "growth_repayment", "collateral_level", "repayment_cushion",
                    "installment_source", "sector", "existing_loans", "late_payment_history",
                    "owner_education", "target_missed_payment", "repayment_confidence",
                    "cashflow_stability")
df1[cols_to_factor] <- lapply(df1[cols_to_factor], factor)

df1$monthly_revenue       <- df1$monthly_revenue * 1000
df1$projected_growth      <- df1$projected_growth * 0.01
df1$loan_amount           <- df1$loan_amount * 1000
df1$monthly_installment   <- df1$monthly_installment * 1000
df1$revenue_for_servicing <- df1$revenue_for_servicing * 0.01


fmt_k <- function(val, type = "bdt") {
  if (type == "pct") return(scales::percent(val, accuracy = 0.1))
  vapply(val, function(v) {
    if (is.na(v)) return(NA_character_)
    if (abs(v) >= 1e4) {
      paste0(sub("\\.0$", "", scales::comma(v / 1e3, accuracy = 0.1)), "K")
    } else {
      scales::comma(v, accuracy = if (type == "bdt") 1 else 0.01)
    }
  }, character(1))
}

df_final <- df1 %>%
  select(sector, owner_education, monthly_revenue, years_operating, employees_count,
         location, division, business_type, repayment_period, projected_growth,
         funding_need, loan_purpose, loan_amount, collateral_level, cashflow_stability,
         previous_loans_repaid, revenue_for_servicing, monthly_installment,
         target_missed_payment, monthly_profit_bdt, existing_loans,
         days_payable_outstanding, repayment_confidence, late_payment_history)


df_final <- df_final %>%
  mutate(
    installment_to_revenue = monthly_installment / monthly_revenue,
    loan_to_profit         = loan_amount / pmax(monthly_profit_bdt, 1),
    loan_to_revenue        = loan_amount / monthly_revenue
  )

df_final$cashflow_stability <- fct_recode(df_final$cashflow_stability,
                                          "One" = "1", "Two" = "2", "Three" = "3",
                                          "Four" = "4", "Five" = "5")
df_final$repayment_confidence <- fct_recode(df_final$repayment_confidence,
                                            "One" = "1", "Two" = "2", "Three" = "3",
                                            "Four" = "4", "Five" = "5")
df_final$collateral_level <- fct_recode(df_final$collateral_level,
                                        "High" = "high",
                                        "Moderate" = "moderate",
                                        "Low" = "low")
df_final$funding_need <- fct_recode(df_final$funding_need,
                                    "Expand working capital"    = "expand working capital",
                                    "Open new branch"           = "open new branch",
                                    "Purchase modern equipment" = "purchase modern equipment",
                                    "Renovate premises"         = "renovate premises",
                                    "Stock seasonal inventory"  = "stock seasonal inventory")
df_final$target_missed_payment <- fct_recode(df_final$target_missed_payment,
                                             "No" = "0",
                                             "Yes" = "1")
df_final$target_missed_payment <- fct_relevel(df_final$target_missed_payment, "Yes", "No")

df_final$collateral_level <- fct_relevel(df_final$collateral_level, "High", "Moderate", "Low")


cont_spec <- list(
  list(var = "monthly_profit_bdt",        label = "Monthly Profit",            icon = "sack-dollar",         type = "bdt"),
  list(var = "loan_amount",               label = "Loan Amount",               icon = "hand-holding-dollar", type = "bdt"),
  list(var = "revenue_for_servicing",     label = "Revenue for Servicing",     icon = "percent",             type = "pct"),
  list(var = "monthly_installment",       label = "Monthly Installment",       icon = "file-invoice-dollar", type = "bdt"),
  list(var = "days_payable_outstanding",  label = "Days Payable Outstanding",  icon = "clock",               type = "num"),
  list(var = "repayment_period",          label = "Repayment Period (months)", icon = "calendar-days",       type = "num"),
  list(var = "projected_growth",          label = "Projected Growth",          icon = "arrow-trend-up",      type = "pct"),
  list(var = "monthly_revenue",           label = "Monthly Revenue",           icon = "coins",               type = "bdt"),
  list(var = "years_operating",           label = "Years Operating",           icon = "store",               type = "num"),
  list(var = "installment_to_revenue", label = "Installment to Revenue (Ratio)", icon = "chart-pie", type = "num"),
  list(var = "loan_to_profit", label = "Loan to Profit (Ratio)", icon = "hand-holding-dollar", type = "num"),
  list(var = "loan_to_revenue", label = "Loan to revenue (Ratio)", icon = "coins", type = "num")
)

# ------------------------------------------------------------------
# Information gain (categorical features only)
# ------------------------------------------------------------------
ig <- df_final %>%
  select(-c(monthly_revenue, years_operating, employees_count, repayment_period,
            projected_growth, loan_amount, previous_loans_repaid, revenue_for_servicing,
            monthly_installment, monthly_profit_bdt, days_payable_outstanding)) %>%
  information_gain(target_missed_payment ~ ., data = .)


continuous_df_final <- df_final %>%
  select(monthly_revenue, years_operating, employees_count, repayment_period,
         projected_growth, loan_amount, previous_loans_repaid,
         revenue_for_servicing, monthly_installment, monthly_profit_bdt,
         days_payable_outstanding, installment_to_revenue, loan_to_profit, loan_to_revenue, target_missed_payment)

continuous_vars <- setdiff(names(continuous_df_final), "target_missed_payment")

grp_no  <- continuous_df_final %>% filter(target_missed_payment == "No")
grp_yes <- continuous_df_final %>% filter(target_missed_payment == "Yes")

# look up the format type for a variable (defaults to "num")
var_type <- function(v) {
  hit <- Filter(function(s) s$var == v, cont_spec)
  if (length(hit)) hit[[1]]$type else "num"
}

mean_sd <- function(x, type) {
  sprintf("%s (%s)",
          fmt_k(mean(x, na.rm = TRUE), type),
          fmt_k(sd(x, na.rm = TRUE), type))
}

med_iqr <- function(x, type) {
  q <- quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
  sprintf("%s (%s)", fmt_k(q[2], type), fmt_k(q[3] - q[1], type))
}

table1_df <- map_dfr(continuous_vars, function(v) {
  ty <- var_type(v)
  tibble(
    Variable = str_to_title(str_replace_all(v, "_", " ")),
    mean_no  = mean_sd(grp_no[[v]],  ty),
    mean_yes = mean_sd(grp_yes[[v]], ty),
    med_no   = med_iqr(grp_no[[v]],  ty),
    med_yes  = med_iqr(grp_yes[[v]], ty),
    p_value  = wilcox.test(grp_no[[v]], grp_yes[[v]], exact = FALSE)$p.value
  )
})


make_cont_card <- function(s) {
  card(
    full_screen = TRUE,
    fill = FALSE,
    card_header(
      class = "d-flex align-items-center gap-2 fw-bold",
      icon(s$icon, style = "color:#006199;"),
      s$label
    ),
    uiOutput(paste0("stats_", s$var)),
    plotlyOutput(paste0("plot_", s$var), height = "220px")
  )
}

custom_css <- "
  /* Sticky sidebar */
  .bslib-sidebar-layout > .sidebar {
    position: sticky;
    top: 0;
    align-self: start;
    min-height: 90vh;
    overflow-y: auto;
  }
  /* Hide resize handle */
  .bslib-sidebar-resize-handle {
    display: none !important;
  }
  /* Sticky toggle button */
  .bslib-sidebar-layout > .collapse-toggle {
    position: sticky;
    top: 10px;
    align-self: start;
    z-index: 999;
  }
  .leaflet-container {
    background: #ffffff !important;
  }
  .bslib-sidebar-layout > .sidebar .accordion-item {
    background-color: transparent !important;
    border-left: none !important;
    border-right: none !important;
  }
  .bslib-sidebar-layout > .sidebar .accordion-button,
  .bslib-sidebar-layout > .sidebar .accordion-button:not(.collapsed) {
    background-color: transparent !important;
    box-shadow: none !important;
  }
  .bslib-sidebar-layout > .sidebar .accordion-body {
    background-color: transparent !important;
  }
  .bslib-sidebar-layout > .sidebar .accordion {
    --bs-accordion-bg: transparent;
  }
"
logistic_fit <- readRDS("sme_log_fit.rds")
logistic_model <- logistic_fit$workflow %>% extract_fit_engine()

logistic_model_tbl <- tidy(logistic_model) %>%
  filter(term != "(Intercept)") %>% transmute(
    Term = str_to_title(str_replace_all(str_replace(term, "([a-z])([A-Z])", "\\1: \\2"), "_", " ")),
    odds_ratio = exp(-estimate),
    lower = exp(-(estimate + 1.96 * std.error)),
    upper = exp(-(estimate - 1.96 * std.error)),
    p_value = p.value
  )

# ------------------------------------------------------------------
# Model performance (test set): threshold + predictions come from the
# training script's saved RDS
# ------------------------------------------------------------------
final_thr  <- logistic_fit$threshold
test_preds <- logistic_fit$test_preds

perf_tbl <- metric_set(accuracy, sensitivity, specificity, roc_auc)(
  test_preds, truth = target_missed_payment, .pred_Yes, estimate = .pred_class
) %>%
  mutate(Metric = c(accuracy = "Accuracy", sensitivity = "Sensitivity",
                    specificity = "Specificity", roc_auc = "ROC AUC")[.metric]) %>%
  select(Metric, Estimate = .estimate)

cm_df <- conf_mat(test_preds, truth = target_missed_payment, estimate = .pred_class) %>%
  pluck("table") %>% as_tibble() %>%
  group_by(Truth) %>% mutate(pct = n / sum(n)) %>% ungroup()

roc_df <- roc_curve(test_preds, target_missed_payment, .pred_Yes)
op_pt  <- tibble(
  fpr = 1 - yardstick::spec(test_preds, target_missed_payment, estimate = .pred_class)$.estimate,
  tpr = yardstick::sens(test_preds, target_missed_payment, estimate = .pred_class)$.estimate
)


int_validate <- function(value) {
  if (is.null(value) || !nzchar(trimws(value))) return(NULL)
  v <- gsub(",", "", trimws(value))
  if (!grepl("^(0|[1-9][0-9]*)$", v)) "Enter a whole number (e.g 25000, 50000 etc.)"
}

# text inputs used by the prediction form
int_inputs <- c("monthly_rev", "years_ops", "loan_amount", "monthly_profit", "loan_payment_hist", "days_payable")

# input ID -> df_final column the dropdown choices come from
cat_inputs <- c(
  late_payment_hist     = "late_payment_history",
  cashflow_stability_in = "cashflow_stability",
  collateral_level_in   = "collateral_level",
  existing_loans_in     = "existing_loans"
)

num <- function(x) as.numeric(gsub(",", "", trimws(x)))


ui <- page_navbar(
  title  = "SME Micro-credit Project",
  id     = "navbar",
  theme  = bs_theme(
    preset    = "lumen",
    base_font = font_collection(
      font_google("Poppins", local = FALSE),
      "Roboto", "sans-serif"
    )
  ),
  header = tags$head(tags$style(HTML(custom_css))),
  
  nav_panel(
    "Data Overview",
    layout_sidebar(
      sidebar = sidebar(
        open         = TRUE,
        width        = 350,
        position     = "left",
        bg           = "#ffffff",
        border       = TRUE,
        border_color = "#dee2e6",
        selectInput("business_type_select", "Select Sector",
                    choices  = c("ALL", levels(df_final$sector)),
                    multiple = TRUE, selected = "ALL"),
        selectInput("loan_purpose", "Loan Purpose",
                    choices  = c("ALL", levels(df_final$loan_purpose)),
                    multiple = TRUE, selected = "ALL"),
        selectInput("collateral_level", "Level of Collateral Commitment:",
                    choices  = c("ALL", levels(df_final$collateral_level)),
                    multiple = TRUE, selected = "ALL")
      ),
      accordion(
        id   = "data_visual",
        open = TRUE,
        accordion_panel(
          "Summary Statistics",
          card(
            fill = FALSE,
            card_header("Summary of Continuous Variables"),
            do.call(
              layout_columns,
              c(list(col_widths = c(4, 4, 4), fill = FALSE),
                lapply(cont_spec, make_cont_card))
            )
          ),
          card(
            gt_output("categorical_vars")
          )
        )
      )
    )
  ),
  nav_panel(
    "Predictive Modelling",
    layout_sidebar(
      sidebar = sidebar(
        open         = TRUE,
        width        = 450,
        position     = "left",
        bg           = "#ffffff",
        border       = TRUE,
        border_color = "#dee2e6",
        h4("Model Input"),
        accordion(
          id="predictive_model",
          open=FALSE,
          accordion_panel(
            "Info Form:",
            icon = icon("clipboard-list"),
            card(
              card_header(div(class = "d-flex justify-content-end gap-2",
                              actionButton("reset_form", "Reset", icon = icon("rotate-left"),
                                           class = "btn-primary btn-sm"))
              ),
              
              layout_columns(
                col_widths = c(6,6),
                selectInput("late_payment_hist", "Late Payment History:", choices = c(levels(df_final$late_payment_history))),
                selectInput("cashflow_stability_in", "Cashflow Stability:", choices = c(levels(df_final$cashflow_stability))),
                selectInput("collateral_level_in", "Collateral Commitment:", choices = c(levels(df_final$collateral_level))),
                selectInput("existing_loans_in", "Previous loans:", choices = c(levels(df_final$existing_loans)))
              ),
              card(
                layout_columns(
                  col_widths = c(6,6),
                  textInput("monthly_rev", "Applicants' Monthly Revenue:", placeholder = "Values only"),
                  textInput("years_ops", "Business operation years:", placeholder = "In years"),
                  textInput("loan_amount", "Loan Amount:", placeholder = "Integers only"),
                  textInput("monthly_profit", " Business Monthly Profit:", placeholder = "Values only..."),
                  textInput("days_payable", "Days Payable Outstanding:", placeholder = "In days"),
                  textInput("loan_payment_hist", "Count of Repayment of loan:", placeholder = "Count only..")
                )
              ),
              card_footer(
                div(class = "d-flex justify-content-end gap-2",
                    actionButton("predict", "Model Prediction", icon = icon("paper-plane"),
                                 class = "btn-primary btn-sm"))
              )
            )
            
          )
        )

      ),
      accordion(
        id   = "model_acc",
        open = FALSE,
        accordion_panel(
          "Information gain and Bivariate association",
          card(
            gt_output("ig_table")
          ),
          card(
            gt_output("continuous_vars_df")
          )
        ),
        accordion_panel(
          "Model Summary",
          card(
            gt_output("model_summary")
          )
        ),
        
        accordion_panel(
          "Model Performance (Test Results)",
          card(gt_output("perf_table")),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Confusion Matrix"), plotlyOutput("cm_plot", height = "380px")),
            card(card_header("ROC Curve"),        plotlyOutput("roc_plot", height = "380px"))
          )
        ),
        accordion_panel(
          "Prediction Result", value = "pred_panel",
          value_box(
            title    = "Model Output",
            value    = textOutput("pred_text"),
            showcase = icon("chart-line"),
            theme    = value_box_theme(fg = "#093C5D", bg = "#F5F5F5")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  model_input <- InputValidator$new()
  for (id in int_inputs) model_input$add_rule(id, int_validate)
  model_input$enable()
  
  model_input_1 <- InputValidator$new()
  for (id in int_inputs) model_input_1$add_rule(id, sv_required("This field is required"))
  
  
  pred_rv <- reactiveVal(NULL)
  
  observeEvent(input$predict, {
    
    model_input_1$enable()
    req(model_input$is_valid(), model_input_1$is_valid())
    
 
    newdata <- tibble(
      late_payment_history  = factor(input$late_payment_hist,     levels = levels(df_final$late_payment_history)),
      cashflow_stability    = factor(input$cashflow_stability_in, levels = levels(df_final$cashflow_stability)),
      collateral_level      = factor(input$collateral_level_in,   levels = levels(df_final$collateral_level)),
      existing_loans        = factor(input$existing_loans_in,     levels = levels(df_final$existing_loans)),
      monthly_revenue       = num(input$monthly_rev),
      years_operating       = num(input$years_ops),
      loan_amount           = num(input$loan_amount),
      previous_loans_repaid = num(input$loan_payment_hist),
      days_payable_outstanding = num(input$days_payable),
      monthly_profit_bdt    = num(input$monthly_profit)
    ) %>%
      mutate(
        loan_to_profit  = loan_amount / pmax(monthly_profit_bdt, 1),
        loan_to_revenue = loan_amount / monthly_revenue
      )
    
    if (newdata$monthly_revenue <= 0) {
      showNotification("Monthly revenue must be greater than 0.", type = "error")
      return()
    }
    
    wf <- logistic_fit$workflow
    res <- tryCatch({
      x   <- hardhat::forge(newdata, workflows::extract_mold(wf)$blueprint)$predictors
      fit <- workflows::extract_fit_parsnip(wf)
      bind_cols(predict(fit, x, type = "prob"),
                predict(fit, x, type = "conf_int", level = 0.95))
    }, error = function(e) {
      showNotification(paste("Prediction failed:", cli::ansi_strip(conditionMessage(e))),
                       type = "error", duration = 10)
      NULL
    })
    if (is.null(res)) return()
    
    pct <- function(v) scales::percent(v, accuracy = 0.1)
    outcome <- if (res$.pred_Yes >= final_thr) "Missed payment" else "No missed payment"
    prob    <- pct(res$.pred_Yes)
    ci      <- paste0(pct(res$.pred_lower_Yes), " \u2013 ", pct(res$.pred_upper_Yes))
    thr     <- sprintf("%.2f", final_thr)
    
    pred_rv(paste0("Predicted outcome: ", outcome,"
                   Probability : ", prob, " 95% CI: (", ci, ")","
                   Decision threshold: ", thr))
    accordion_panel_open("model_acc", "pred_panel")
    
   
    model_input$disable()
    model_input_1$disable()
  })
  
  output$pred_text <- renderText({
    if (is.null(pred_rv())) {
      "No output provided by the model"
    } else {
      pred_rv()
    }
  })
  
 
  observeEvent(input$reset_form, {
    for (id in int_inputs) updateTextInput(session, id, value = "")
    for (id in names(cat_inputs)) {
      updateSelectInput(session, id,
                        selected = levels(df_final[[cat_inputs[[id]]]])[1])
    }
    model_input_1$disable()
    pred_rv(NULL)
  })
  
  filtered_data <- reactive({
    data <- df_final
    
    if (!is.null(input$business_type_select) && !"ALL" %in% input$business_type_select) {
      data <- data %>% filter(sector %in% input$business_type_select)
    }
    if (!is.null(input$loan_purpose) && !"ALL" %in% input$loan_purpose) {
      data <- data %>% filter(loan_purpose %in% input$loan_purpose)
    }
    if (!is.null(input$collateral_level) && !"ALL" %in% input$collateral_level) {
      data <- data %>% filter(collateral_level %in% input$collateral_level)
    }
    
    data
  })
  
  filtered_data_1 <- reactive({
    filtered_data() %>%
      select(existing_loans, repayment_confidence, cashflow_stability,
             late_payment_history, target_missed_payment, sector,
             division, funding_need, collateral_level) %>%
      rename(`Existing Loans`        = existing_loans,
             `Repayment Confidence`  = repayment_confidence,
             `Cashflow Stability`    = cashflow_stability,
             `Late Payment History`  = late_payment_history,
             `Missed Payment`        = target_missed_payment,
             `Sector`                = sector,
             `Division`              = division,
             `Funding Need`          = funding_need,
             `Collateral Level`      = collateral_level) %>%
      mutate(across(everything(), as.character)) %>%   # factors with different levels can't be stacked safely
      pivot_longer(everything(),
                   names_to = "Data Features", values_to = "Categories") %>%
      count(`Data Features`, Categories, name = "Count") %>%
      group_by(`Data Features`) %>%
      mutate(Prop = Count / sum(Count)) %>%
      arrange(`Data Features`, desc(Prop), .by_group = TRUE) %>%
      ungroup()
  })
  

  lapply(cont_spec, function(s) {
    v    <- s$var
    type <- s$type
    
    output[[paste0("stats_", v)]] <- renderUI({
      x <- filtered_data()[[v]]
      x <- x[!is.na(x)]
      req(length(x) > 1)
      q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
      
      item <- function(lbl, txt, width = "col-6") {
        div(class = paste(width, "mb-2"),
            div(class = "text-muted small", lbl),
            div(class = "fw-bold", txt))
      }
      
      div(class = "row g-0 px-1",
          item("Mean",      fmt_k(mean(x), type)),
          item("Std. Dev.", fmt_k(sd(x), type)),
          item("Median",    fmt_k(q[2], type)),
          item("IQR",       fmt_k(q[3] - q[1], type)),
          item("Range",     paste(fmt_k(min(x), type), "\u2013", fmt_k(max(x), type)),
               width = "col-12")
      )
    })
    
    output[[paste0("plot_", v)]] <- renderPlotly({
      x <- filtered_data()[[v]]
      x <- x[!is.na(x)]
      req(length(x) > 1)
      q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
      
      xaxis <- list(title = "", fixedrange = TRUE)
      if (type == "pct") {
        xaxis$tickformat <- ".0%"
      } else {
        tv <- pretty(range(x), n = 6)
        xaxis$tickmode <- "array"
        xaxis$tickvals <- tv
        xaxis$ticktext <- fmt_k(tv, type)
      }
      
      plot_ly(
        x = x,
        type = "histogram",
        marker = list(color = "#8B9A6E", line = list(color = "#2C2C2C", width = 0.8)),
        hovertemplate = "Range: %{x}<br>Borrowers: %{y}<extra></extra>"
      ) %>%
        layout(
          dragmode = FALSE,
          bargap   = 0.02,
          margin   = list(t = 10, b = 40, l = 40, r = 10),
          xaxis = xaxis,
          yaxis = list(title = "Borrowers", fixedrange = TRUE),
          shapes = list(
            # shaded IQR band (Q1 to Q3)
            list(type = "rect", x0 = q[1], x1 = q[3],
                 y0 = 0, y1 = 1, yref = "paper",
                 fillcolor = "rgba(0,97,153,0.18)", line = list(width = 0),
                 layer = "below"),
            # median line
            list(type = "line", x0 = q[2], x1 = q[2],
                 y0 = 0, y1 = 1, yref = "paper",
                 line = list(color = "#d9534f", width = 2, dash = "dash"))
          ),
          annotations = list(
            list(
              x = 0.98, y = 0.98, xref = "paper", yref = "paper",
              xanchor = "right", yanchor = "top", showarrow = FALSE, align = "left",
              text = sprintf("Shaded: IQR (%s to %s)<br>Dashed: median",
                             fmt_k(q[1], type), fmt_k(q[3], type)),
              bgcolor = "white", bordercolor = "#006199",
              borderwidth = 1, borderpad = 4, font = list(size = 10)
            )
          )
        ) %>%
        config(displayModeBar = FALSE, scrollZoom = FALSE, doubleClick = FALSE)
    })
  })
  
 
  output$ig_table <- render_gt({
    tbl <- ig %>%
      arrange(desc(importance)) %>%
      mutate(attributes = str_to_title(str_replace_all(attributes, "_", " ")))
    req(nrow(tbl) > 0)
    
    tbl %>%
      gt() %>%
      tab_header(
        title    = md("**Information gain on Categorical Variable**"),
        subtitle = "How much each feature tells us about missed payment"
      ) %>%
      fmt_number(columns = importance, decimals = 4) %>%
      data_color(
        columns = importance,
        fn = scales::col_numeric(
          palette = c("#F7F2EB", "#006199"),
          domain  = c(0, max(tbl$importance))
        )
      ) %>%
      cols_label(attributes = "Attribute", importance = "Importance") %>%
      cols_align(align = "left", columns = attributes) %>%
      tab_style(
        style     = cell_text(align = "left"),
        locations = list(cells_title(groups = "title"), cells_title(groups = "subtitle"))
      ) %>%
      tab_options(
        table.width                = pct(100),
        table.font.size            = px(14),
        heading.title.font.size    = px(20),
        heading.subtitle.font.size = px(14),
        column_labels.font.weight  = "bold",
        table.border.top.style     = "hidden",
        table_body.hlines.color    = "#E5E5E5"
      )
  })

  output$continuous_vars_df <- render_gt({
    p_fmt <- function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
    
    table1_df %>%
      gt() %>%
      tab_header(
        title    = md("**Bivariate Association**"),
        subtitle = "Wilcoxon rank-sum test"
      ) %>%
      tab_spanner(label = "Mean (SD)",
                  columns = c(mean_no, mean_yes)) %>%
      tab_spanner(label = "Median (IQR)",
                  columns = c(med_no, med_yes)) %>%
      cols_label(
        Variable = "Variable",
        mean_no  = sprintf("No missed payment (n=%d)", nrow(grp_no)),
        mean_yes = sprintf("Missed payment (n=%d)",    nrow(grp_yes)),
        med_no   = sprintf("No missed payment (n=%d)", nrow(grp_no)),
        med_yes  = sprintf("Missed payment (n=%d)",    nrow(grp_yes)),
        p_value  = "P-value"
      ) %>%
      fmt(columns = p_value, fns = p_fmt) %>%
      data_color(
        columns = p_value,
        fn = scales::col_numeric(
          palette = c("#006199", "#F7F2EB"),
          domain  = c(0, max(table1_df$p_value))
        )
      ) %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body(columns = Variable)
      ) %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body(columns = p_value, rows = p_value < 0.05)
      ) %>%
      tab_style(
        style     = cell_text(align = "left"),
        locations = list(cells_title(groups = "title"), cells_title(groups = "subtitle"))
      ) %>%
      tab_options(
        table.width               = pct(100),
        table.font.size           = px(14),
        column_labels.font.weight = "bold",
        table.border.top.style    = "hidden",
        table_body.hlines.color   = "#E5E5E5"
      )
  })
  
  
  output$categorical_vars <- render_gt({
    tbl <- filtered_data_1()
    req(nrow(tbl) > 0)
    
    tbl %>%
      gt(groupname_col = "Data Features") %>%
      tab_header(
        title    = md("**Summary of Categorical Features**"),
        subtitle = "Share of borrowers in each category"
      ) %>%
      fmt_percent(columns = Prop, decimals = 1) %>%
      fmt_number(columns = Count, decimals = 0, use_seps = TRUE) %>%
      data_color(
        columns = Prop,
        fn = scales::col_numeric(
          palette = c("#F7F2EB", "#006199"),
          domain  = c(0, max(tbl$Prop))
        )
      ) %>%
      cols_label(Categories = "Category", Count = "Borrowers", Prop = "Share") %>%
      cols_align(align = "left", columns = Categories) %>%
      tab_style(
        style     = list(cell_text(weight = "bold"), cell_fill(color = "#EAF1F6")),
        locations = cells_row_groups()
      ) %>%
      tab_style(
        style     = cell_text(align = "left"),
        locations = list(cells_title(groups = "title"), cells_title(groups = "subtitle"))
      ) %>%
      tab_options(
        table.width                = pct(100),
        table.font.size            = px(14),
        heading.title.font.size    = px(20),
        heading.subtitle.font.size = px(14),
        column_labels.font.weight  = "bold",
        row_group.font.weight      = "bold",
        table.border.top.style     = "hidden",
        table_body.hlines.color    = "#E5E5E5"
      )
  })
  
  
  output$model_summary <- render_gt({
    p_fmt <- function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
    logistic_model_tbl %>% gt() %>% tab_header(
      title = md("**Logistic Regression: Missed Payment**"),
      subtitle = "Odds ratio above 1 = higher odds of a missed payment (categories compared with the first level)"
    ) %>%
      fmt_number(columns = c(odds_ratio, lower, upper), decimals = 2) %>%
      fmt(columns = p_value, fns = p_fmt) %>%
      cols_merge(columns = c(lower, upper), pattern = "({1}-{2})") %>%
      cols_label(Term = "Variable", odds_ratio = "Odds Ratio", lower = "95% CI", p_value = "P-Value") %>%
      tab_style(
        style     = cell_text(weight = "bold"),
        locations = cells_body(columns = p_value, rows = p_value < 0.05)
      ) %>%
      tab_style(
        style     = cell_text(align = "left"),
        locations = list(cells_title(groups = "title"), cells_title(groups = "subtitle"))
      ) %>%
      tab_options(
        table.width               = pct(100),
        table.font.size           = px(14),
        column_labels.font.weight = "bold",
        table.border.top.style    = "hidden",
        table_body.hlines.color   = "#E5E5E5"
      )
    
  })
  
  
  output$perf_table <- render_gt({
    perf_tbl %>% gt() %>%
      tab_header(title = md("**Test-set Performance**"),
                 subtitle = sprintf("Classification threshold = %.2f (maximises J-index in CV)", final_thr)) %>%
      fmt_number(columns = Estimate, decimals = 3) %>%
      tab_style(cell_text(align = "left"),
                list(cells_title("title"), cells_title("subtitle"))) %>%
      tab_options(table.width = pct(100), table.font.size = px(14),
                  column_labels.font.weight = "bold",
                  table.border.top.style = "hidden",
                  table_body.hlines.color = "#E5E5E5")
  })
  
  output$cm_plot <- renderPlotly({
    plot_ly(cm_df, x = ~Truth, y = ~Prediction, z = ~pct, type = "heatmap",
            colorscale = list(c(0, "#F7F2EB"), c(1, "#006199")),
            showscale = FALSE, hoverinfo = "skip") %>%
      layout(
        xaxis = list(title = "Actual", categoryorder = "array", categoryarray = c("Yes", "No")),
        yaxis = list(title = "Predicted", categoryorder = "array", categoryarray = c("No", "Yes")),
        annotations = lapply(seq_len(nrow(cm_df)), function(i) list(
          x = cm_df$Truth[i], y = cm_df$Prediction[i], showarrow = FALSE,
          text = sprintf("%d<br>(%.1f%%)", cm_df$n[i], cm_df$pct[i] * 100),
          font = list(size = 16, color = if (cm_df$pct[i] > 0.5) "white" else "#2C2C2C")
        )), dragmode = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  output$roc_plot <- renderPlotly({
    plot_ly() %>%
      add_lines(x = c(0, 1), y = c(0, 1), line = list(dash = "dash", color = "grey"),
                hoverinfo = "skip", showlegend = FALSE) %>%
      add_lines(data = roc_df, x = ~(1 - specificity), y = ~sensitivity,
                line = list(color = "#006199", width = 2.5), name = "ROC") %>%
      add_markers(data = op_pt, x = ~fpr, y = ~tpr,
                  marker = list(color = "#d9534f", size = 11),
                  name = sprintf("Threshold %.2f", final_thr)) %>%
      layout(xaxis = list(title = "1 - Specificity"), yaxis = list(title = "Sensitivity"),
             legend = list(x = 0.6, y = 0.1), dragmode = FALSE) %>%
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui = ui, server = server)
