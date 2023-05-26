library(scplot)


dat <- exampleAB$Johanna
scplot(dat) |>
  add_statline("trend")
dat2 <- dat[[1]]
dat2

fit <- lm(values ~ mt*phase, data = dat2)
pred <- predict(fit, interval = "confidence")
dat2 <- cbind(dat2, pred)

ggplot(dat2, aes(x = mt, y = values, color = phase)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey95", color = "grey95") +
  geom_point() +
  geom_line(aes(y = fit), data = dat2)

?geom_ribbon

?geom_ribbon

?geom_smooth
