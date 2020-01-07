library(ggplot2)

# Inferring the likely proportion of true hypotheses studied in the literature
# and statistical power based on the observed proportion of significant results

# t = proportion of true hypotheses
# a = alpha
# b = beta; i.e. 1-b = power
# PRR = positive result rate (proportion of significant results in the literature)
truehyps <- function(PRR, a, pwr){-(PRR - a)/((1-pwr) + a - 1)}
pwrfun <- function(t, a, PRR){(a*t+PRR-a)/(t)}

a <- 0.05

### 3 groups: original SRs, original RRs, all RRs

pwrprior.df <-  data.frame(t=c(1:1000)/1000)
pwrprior.df$group <- "all RRs (PRR = .4366, N = 71)"
pwrprior.df$lower <- pwrfun(t = pwrprior.df$t, a = a, PRR = min(RR.binom$conf.int))
pwrprior.df$estimate <- pwrfun(t = pwrprior.df$t, a = a, PRR = prop.support.RR)
pwrprior.df$upper <- pwrfun(t = pwrprior.df$t, a = a, PRR = max(RR.binom$conf.int))   

pwrprior.origRR <- data.frame(t=c(1:1000)/1000)
pwrprior.origRR$group <- "original RRs (PRR = .5, n = 30)"
pwrprior.origRR$lower <- pwrfun(t = pwrprior.origRR$t, a = a, PRR = min(RR.orig.binom$conf.int))
pwrprior.origRR$estimate <- pwrfun(t = pwrprior.origRR$t, a = a, PRR = prop.orig.support.RR)
pwrprior.origRR$upper <- pwrfun(t = pwrprior.origRR$t, a = a, PRR = max(RR.orig.binom$conf.int))   

pwrprior.origSR <- data.frame(t=c(1:1000)/1000)
pwrprior.origSR$group <- "original SRs (PRR = .9595, n = 148)"
pwrprior.origSR$lower <- pwrfun(t = pwrprior.origSR$t, a = a, PRR = min(SR.orig.binom$conf.int))
pwrprior.origSR$estimate <- pwrfun(t = pwrprior.origSR$t, a = a, PRR = prop.orig.support.SR)
pwrprior.origSR$upper <- pwrfun(t = pwrprior.origSR$t, a = a, PRR = max(SR.orig.binom$conf.int))   

pwrprior.df <- rbind(pwrprior.df, pwrprior.origRR, pwrprior.origSR)

pwrprior.df$lower[pwrprior.df$lower > 1 & pwrprior.df$upper > 1] <- NA
pwrprior.df$lower[pwrprior.df$lower > 1] <- 1
pwrprior.df$upper[pwrprior.df$upper > 1] <- 1

pwrprior.df$group <- factor(pwrprior.df$group, 
                            levels = c("original SRs (PRR = .9595, n = 148)",
                                       "original RRs (PRR = .5, n = 30)", 
                                       "all RRs (PRR = .4366, N = 71)"))

power.baserate.plot <- ggplot(pwrprior.df, aes(x = t, y = estimate, colour = group, 
                                               fill = group, linetype = group)) +
                              geom_line(aes(y = estimate), size = 1) + 
                              geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .42, linetype = "blank") +
                              scale_x_continuous(name="proportion of true hypotheses", 
                                                 limits=c(0.25, 1), 
                                                 breaks = c(seq(0.3, 1, 0.1)),
                                                 expand = c(0, 0)) +
                              scale_y_continuous(name="statistical power", 
                                                 limits=c(0.25, 1), 
                                                 breaks = c(seq(0.3, 1, 0.1)),
                                                 expand = c(0, 0)) +
                              scale_colour_manual(values = c("#0073C0", "#ED443F", "#2D132C"), name = NULL) +
                              scale_fill_manual(values = c("#0073C0", "#ED443F", "#2D132C"), name = NULL) +
                              scale_linetype_manual(values = c("solid", "dashed", "dotted"), name = NULL) +
                              theme_bw()