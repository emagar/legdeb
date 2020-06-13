












##########################
## SIMULATE AND PREDICT ##
##########################
# std error version
sims2 <- with(data,
              data.frame(ev.pot.dys=median(ev.pot.dys),
                         dmaj = NA, 
                         size.maj = rep(seq(from=-49, to=10, length.out = 60), 2),
                         dleader = 0,
                         dchair = 0,
                         dsmd = 1, 
                         dsmd64 = c(rep(0,60),rep(1,60)), 
                         seniority = 0,
                         dsup = 0,
                         dfem = 1,
                         d62 = c(rep(1,60),rep(0,60)),
                         d64 = c(rep(0,60),rep(1,60))
                         )
              )
sims2$dmaj <- as.numeric(sims2$size.maj > .5) # fix dmaj
tail(sims2,10)
sims2$pr <- predict(fit2, newdata = sims2, type = "response")
sims2 <- cbind(sims2, predict(fit2, newdata = sims2, type="link", se=TRUE))
sims2 <- within(sims2, {
  PredictedWords <- exp(fit)
  LL <- exp(fit - (1.96 * se.fit))
  UL <- exp(fit + (1.96 * se.fit))
})
#sims2$size.maj <- seq(from=-50, to=10, length.out = 120) # for plot
head(sims2,10)
library(ggplot2)
#pdf (file = "../plots/predictedPr.pdf", width = 7, height = 4)
ggplot(sims2, aes(x = size.maj, y = PredictedWords)) +
    geom_ribbon(aes(ymin = LL, ymax = UL, fill = factor(dsmd64)), alpha = .2) +
    geom_line(aes(colour = factor(dsmd64)), size=1) +
    labs(fill = "Reelection legit", colour = "Reelection legit",
         x = "Party size",
         y = "Predicted words in period") +
    scale_x_continuous(breaks=seq(from=-50, to=10, length.out=7), labels=seq(from=0, to=60, by=10))
#dev.off()




############################
# Average marginal effects #
############################
library(margins)
mar2 <- margins(fit2)
mar2 <- summary(mar2)
#tmp <- c(6,4,2,3,1,9,10,11,7,8,5); mar3 <- mar3[order(tmp),] # sort rows so coefs appear in same order as in table  *with dmocion*
#tmp <-  c(1,2,3,4,5,6,7,8,9,10,11,12); mar2 <- mar2[order(tmp),] # sort rows so coefs appear in same order as in table
tmp <-  c(11,12,5,9,4,2,6,7,10,1,8,3); #mar2 <-  # sort rows so coefs appear in same order as in table
mar2 <- mar2[order(tmp),]
tmp <- c("Tenure",
         "Majority",
         "Party size",
         "Party leader",
         "Comm. chair",
         "SMD",
         "SMD x reelect",
         "Seniority",
         "Suplente",
         "Female",
         "62nd Leg.",
         "64th Leg.")

#pdf (file = "../plots/avgMgEffects.pdf", width = 7, height = 5)
par(mar=c(4,2,2,2)+0.1) # drop title space and left space
plot(x=c(-10000,1850),#)(-8250,1850),
     y=-c(1,nrow(mar2)),
     type="n", axes = FALSE,
     xlab = "Average marginal effect (thousands of words)",
     ylab = "")
abline(v=seq(-.2, .35, .05), col = "gray70")
abline(v=0, lty=2)
abline(h=seq(-1,-nrow(mar2),-1), col = "gray70")
axis(1, at = seq(-8000, 2000, 1000), labels = seq(-8,2,1))
for (i in c(-1:-nrow(mar2))){
    points(y=i, x=mar2$AME[-i], pch=20, cex=1.5, col = "black")
    lines(y=rep(i, 2), x=c(mar2$lower[-i],mar2$upper[-i]), lwd = 2, col = "black")
}
#mar2$factor
polygon(x= c(-11000,-8500,-8500,-11000), y=c(-12,-12,0,0), col = "white", border = "white")
text(x=-10500, y=-1:-nrow(mar2), labels=tmp, pos=4)
#dev.off()




