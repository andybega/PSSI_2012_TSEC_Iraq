setwd('~/Dropbox/Research/MINERVA 2013/WhitePaper')

# Create latent mean

n <- 20
set.seed(11240)
mu <- c(2, rep(NA, n-1))
for (i in 2:n) {
  mu[i] <- rnorm(1, mu[i-1], 0.5)
}
y <- rnorm(n, mu, 1.5)

mu <- data.frame(t=1:n, mu=mu, y=y)

# Plot
library(ggplot2)
library(grid)

p <- ggplot(data=mu, aes(t, mu)) + geom_line() + theme_bw() +
  geom_point(aes(x=t[11:20], y=y[11:20]), color='black', size=4) + 
  geom_point(aes(x=t[11:20], y=y[11:20]), color='white', size=3) +
  geom_segment(aes(x=t[11:20], y=mu[11:20], xend=t[11:20], yend=y[11:20]), color='grey')
# Misc formatting
p <- p + annotate('rect', xmin=0.5, xmax=10.4, ymin=-0.5, ymax=5.5, alpha=0.15) +
  annotate('rect', xmin=10.6, xmax=20.5, ymin=-0.5, ymax=5.5, alpha=0.15) +
  scale_y_continuous(name=expression(list(mu, y))) + 
  theme(axis.ticks.y=element_blank(), axis.text.y=element_blank())
# Process expl.
p <- p + annotate('text', x=5, y=5, label='mu[t] == g(mu[t-1])', parse=T, size=6) +
  annotate('text', x=3.4, y=(mu$mu[3]+0.1), label='mu[3]', parse=T) +
  annotate('text', x=4.4, y=(mu$mu[4]+0.1), label='mu[4]', parse=T) + 
  geom_segment(aes(x=3, y=(mu[3]-0.1), xend=4, yend=(mu[4]-0.1)), color='blue',
               arrow=arrow(angle=25, length=unit(0.2, 'cm'), type='closed')) +
  annotate('text', x=3.2, y=1.8, label='g()')
# Observation expl.
p <- p + annotate('text', x=16, y=5, label='y[t] == h(mu[t])', parse=T, size=6) +
  annotate('text', x=11.6, y=(mu$mu[11]+0.1), label='mu[11]', parse=T) +
  annotate('text', x=11.6, y=(mu$y[11]-0.2), label='y[11]', parse=T) + 
  geom_segment(aes(x=11.1, y=(mu[11]), xend=11.1, yend=(y[11])), color='blue',
               arrow=arrow(angle=25, length=unit(0.2, 'cm'), type='closed')) +
  annotate('text', x=11.6, y=4, label='h()')

png('StateSpace.png', width=1024, height=512)
p
dev.off()