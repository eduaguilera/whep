# Suppress R CMD check warnings for dplyr NSE (Non Standard Evaluation)
utils::globalVariables(c(
  "sex", "No", "Trade", "Timeline_Start", "Timeline_End",
  "Timeline_Freq", "Info_Format", "SACO_link", "Imp/Exp",
  "Name", "Year"
))
