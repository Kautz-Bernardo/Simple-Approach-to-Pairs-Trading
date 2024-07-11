#Work authored by Bernardo Kautz. Follow on LinkedIn: https://www.linkedin.com/in/bernardo-kautz/.
dev.off()
rm(list = ls())
cat("\014")

#Packages
install.packages("quantmod")
install.packages("purrr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("urca")
install.packages("lubridate")
library("quantmod")
library("purrr")
library("dplyr")
library("ggplot2")
library("reshape2")
library("urca")
library("lubridate")

#Reading
tickers <- c("PETR3.SA", "PETR4.SA", "GGBR3.SA", "GGBR4.SA", "BBDC3.SA", "BBDC4.SA", "KLBN3.SA", "KLBN4.SA", "SAPR3.SA", "SAPR4.SA", "TRPL3.SA", "TRPL4.SA", "OIBR3.SA", "OIBR4.SA", "SANB3.SA", "SANB4.SA", "TAEE3.SA", "TAEE4.SA", "ALPA3.SA", "ALPA4.SA", "ITSA3.SA", "ITSA4.SA", "TASA3.SA", "TASA4.SA")
getSymbols(tickers, from = Sys.Date() - years(4), to = Sys.Date()) #✎ Adjustable parameter(s)
prices <- map(tickers, ~ Ad(get(.x))) %>% #✎ Adjustable parameter(s)
  reduce(merge) %>%
  `colnames<-`(tickers)
tail(prices)

#Looking for matches
ggplot(data = melt(cor(prices, use = "complete.obs")), aes(Var1, Var2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), name="Correlation") +
  labs(x = "", y = "", color = "Pair") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 2.5)

diff_prices <- data.frame(
  date = index(prices),
  PETR = prices$PETR3.SA - prices$PETR4.SA,
  GGBR = prices$GGBR3.SA - prices$GGBR4.SA,
  BBDC = prices$BBDC3.SA - prices$BBDC4.SA,
  KLBN = prices$KLBN3.SA - prices$KLBN4.SA,
  SAPR = prices$SAPR3.SA - prices$SAPR4.SA,
  TRPL = prices$TRPL3.SA - prices$TRPL4.SA,
  OIBR = prices$OIBR3.SA - prices$OIBR4.SA,
  SANB = prices$SANB3.SA - prices$SANB4.SA,
  TAEE = prices$TAEE3.SA - prices$TAEE4.SA,
  ALPA = prices$ALPA3.SA - prices$ALPA4.SA,
  ITSA = prices$ITSA3.SA - prices$ITSA4.SA,
  TASA = prices$TASA3.SA - prices$TASA4.SA)

ggplot(data = melt(diff_prices, id.vars = "date", variable.name = "Pair", value.name = "Price Difference"), aes(x = date, y = `Price Difference`, color = Pair)) +
  geom_line() +
  facet_wrap(~ Pair, scales = "free_y", ncol = 3, labeller = as_labeller(setNames(names(diff_prices), names(diff_prices)))) +
  labs(x = "Date", y = "Difference", color = "Pair") +
  theme_minimal() +
  theme(legend.position = "none")

cat("Variable with greater constancy (lower variance):", names(which.min(apply(diff_prices[, -1], 2, var))), "\n")
cat("Variable with mean closest to zero:", names(which.min(abs(apply(diff_prices[, -1], 2, mean)))), "\n")

index = 1
while(index < length(tickers)){
  if (startsWith(tickers[index], substr(names(which.min(apply(diff_prices[, -1], 2, var))), 1, nchar(names(which.min(apply(diff_prices[, -1], 2, var))))-4))){
    break
  }
  index = index + 1
}

finest <- lm(paste("prices$", tickers[index], "~ prices$", tickers[index + 1], sep = ""))
summary(finest)
summary(ur.df(finest$residuals, type = "none"))

#Getting your hands dirty
synthetic <- prices[, tickers[index]] / prices[, tickers[index + 1]]
tail(synthetic)

moving_avg <- rollapply(synthetic, width = 21, FUN = mean, fill = NA, align = "right") #✎ Adjustable parameter(s)
std_dev <- rollapply(synthetic, width = 21, FUN = sd, fill = NA, align = "right") #✎ Adjustable parameter(s)
upper_band <- moving_avg + 1.25 * std_dev #✎ Adjustable parameter(s)
lower_band <- moving_avg - 1.25 * std_dev #✎ Adjustable parameter(s)

stuff <- data.frame(
  date = index(synthetic),
  synthetic = coredata(synthetic),
  moving_avg = coredata(moving_avg),
  upper_band = coredata(upper_band),
  lower_band = coredata(lower_band))

ggplot(stuff, aes(x = date)) +
  geom_line(aes(y = synthetic, color = "Synthetic")) +
  geom_line(aes(y = moving_avg, color = "Moving Average")) +
  geom_line(aes(y = upper_band, color = "Upper Band")) +
  geom_line(aes(y = lower_band, color = "Lower Band")) +
  labs(x = "Date", y = "Value", color = "") +
  theme_minimal() +
  scale_color_manual(values = c("Synthetic" = "darkblue", "Moving Average" = "darkorange", "Upper Band" = "red", "Lower Band" = "green"))

if (tail(synthetic, 1) > tail(upper_band, 1)) {
  print("Signal to sell the ratio (exit the position upon returning to the moving average)")
} else if (tail(synthetic, 1) < tail(lower_band, 1)) {
  print("Signal to buy the ratio (exit the position upon returning to the moving average)")
} else {
  print("Nothing to do")
}