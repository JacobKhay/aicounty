

ggplot(economics, aes(x = psavert)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
  labs(
    title = "Histogram of Personal Savings Rate",
    x = "Personal Savings Rate (%)",
    y = "Number of Months"
  )