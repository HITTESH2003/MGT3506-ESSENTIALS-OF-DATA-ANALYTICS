library(tidyquant)
library(openxlsx)
library(ggplot2)

stock_data <- tq_get("SUNPHARMA.NS", from = Sys.Date() - 30, to = Sys.Date())

write.xlsx(stock_data, "SUNPHARMA_Stock_Data_1Month.xlsx")
cat("Stock data for Sunpharma (last 1 month) has been saved to SUNPHARMA_Stock_Data_1Month.xlsx\n")

close_prices <- stock_data$close
max_lag <- 20
acf_values <- acf(close_prices, lag.max = max_lag, plot = FALSE)

acf_data <- data.frame(
  Lag = acf_values$lag,
  ACF = acf_values$acf
)
ggplot(acf_data, aes(x = Lag, y = ACF)) +
  geom_segment(aes(xend = Lag, yend = 0), color = "#0073e6", size = 1.5) + 
  geom_point(color = "#ff5733", size = 4) + 
  labs(title = "Autocorrelation of Sunpharma Stock (Close Price)",
       subtitle = paste("ACF with a maximum lag of", max_lag),
       x = "Lag",
       y = "Autocorrelation") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f9f9f9", color = NA)
  ) +
  scale_color_manual(values = c("blue", "red")) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

cat("\nAutocorrelation values for each lag:\n")
print(acf_data)