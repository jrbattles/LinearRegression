## sample linear regression code.

plot(jitter(child, 4) ~ parent, galton)
regrline <- lm(child ~ parent, galton)
summary(regrline)