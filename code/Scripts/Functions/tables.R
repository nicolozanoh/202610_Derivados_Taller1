library(knitr)
library(kableExtra)

calibration_table <- function(s_0, n, delta, q_star, r, dias_por_ano, sd_yearly, u, d, b_0, ko){

  params_table <- data.frame(
    Category = c(
      "Market \\& Tree", "", "", "", "",
      "Volatility", "", "", "",
      "Bond", "",
      "Options", "", ""
    ),
    Parameter = c(
      "Initial stock price", "Steps (periods)", "Time step", 
      "Risk-neutral prob.", "Risk-free rate",
      "Trading days/year", "Annualized volatility", 
      "Up factor", "Down factor",
      "Initial bond price", "Bond growth rate",
      "Strike (Put Am. \\& KO)", "Knock-out barrier", "Strike (Chooser)"
    ),
    Symbol = c(
      "$S_0$", "$n$", "$\\delta$", "$q^*$", "$r$",
      "--", "$\\sigma$", "$u$", "$d$",
      "$B_0$", "$r$",
      "$K$", "$KO$", "$K$"
    ),
    Value = c(
      s_0, n, round(delta, 4), q_star, r,
      dias_por_ano, sd_yearly, round(u, 6), round(d, 6),
      b_0, r,
      300, ko, 310
    )
  )
  
  latex_table <- kable(
    params_table,
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    col.names = c("Category", "Parameter", "Symbol", "Value"),
    caption   = "Model Parameters -- Binomial Tree Option Pricing",
    label     = "tab:parameters"
  ) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    collapse_rows(columns = 1, latex_hline = "major", valign = "middle") %>%
    pack_rows("Market & Tree", 1, 5) %>%
    pack_rows("Volatility",    6, 9) %>%
    pack_rows("Bond",         10, 11) %>%
    pack_rows("Options",      12, 14) %>%
    footnote(
      general = paste0(
        "All options have maturity $T = 1$ year. ",
        "Volatility corresponds to AAPL over 2025-01-01 to 2026-02-13."
      ),
      escape         = FALSE,
      general_title  = "Notes:",
      footnote_as_chunk = TRUE
    )
  
  # Print to console
  cat(latex_table)
  
  # Or save to file
  writeLines(latex_table, "output/parameters_table.tex")
}