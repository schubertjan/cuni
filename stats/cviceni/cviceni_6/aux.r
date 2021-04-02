# dodelat hypotezy 

# ukazat t.test

# dodelat cohenovo d
# < 0.2 | 0.2 - 0.5 | 0.5 - 0.8 | 0.8 - 1.2 | > 1.2
# d = (bodovy odhad - H0) / smerodatna odchylka


# stejny rozptyl
# s.v. n1+n1-2
# sdruzeny rozptyl pro cohenovo d
## Máme 2 skupiny pacientu, kazda asi 100. Chceme vedet, jestli lek funguje a porovnat prumerny pocet onemocneni. 
## https://raw.githubusercontent.com/schubertjan/cuni/master/stats/data/viral_load.csv
mu1 <- 100
mu2 <- 80
sigma <- 15
n <- 100
x1 <- rnorm(n, mean = mu1, sd = sigma)
x2 <- rnorm(n, mean = mu2, sd = sigma)
df <- data.frame(group = rep(c(0, 1), 200), viral_particles = round(c(x1, x2)))
aggregate(viral_particles ~ group, data = df, FUN = mean)
#write.csv(df, "/home/schubertj/cuni/stats/data/viral_load.csv", row.names = FALSE)

t.test(viral_particles ~ group, data = df, mu = 0, var.equal=TRUE, alternative = "greater")
df2 <- df
# otocit skupiny
df2$group <- abs(df2$group - 1)
t.test(viral_particles ~ group, data = df2, mu = 0, var.equal=TRUE, alternative = "less")


# rozdilny rozptyl
# s.v. (s2_1/n1 + s2_2/n2)^2 / (s1^4 / n1^2*(n1-1) + s2^4 / n2^2*(n2-1))
# cohenovo d = (rozdil - h0) / sqrt((s2_1 + s2_2) / 2)
## zajima nas jestli jsou tezsi jednodenni zavody nebo etapove zavody. Mame 18 etapovych a 25 jednodennich zavodu. 
## Zajima nas, zda se prumerny pocet zavodniku lisi. Kolik zavodniku avode nedojede.
# https://raw.githubusercontent.com/schubertjan/cuni/master/stats/data/cyklo_zavody.csv
mu1 <- 30
mu2 <- 40
sigma1 <- 5
sigma2 <- 15
x1 <- round(rnorm(18, mean = mu1, sd = sigma1))
x2 <- round(rnorm(25, mean = mu2, sd = sigma2))

df <- data.frame(pocet_nedokoncilo = abs(c(x1, x2)), typ_zavodu = c(rep("etapovy", 18), rep("jednodenni", 25)))
#write.csv(df, "/home/schubertj/cuni/stats/data/cyklo_zavody.csv", row.names = FALSE)
aggregate(df$pocet_nedokoncilo ~ df$typ_zavodu, data = df, FUN = mean)
# jsou jednodenni tezsi
t.test(df$pocet_nedokoncilo ~ df$typ_zavodu, data = df, var.equal = FALSE, alternative = "less", mu = 0)

# parovy
# s.v. n-1
# rozptyl d
## prumerny pocet rozpoznanych reklam pred a po intervenci
# https://raw.githubusercontent.com/schubertjan/cuni/master/stats/data/ads.csv
n <- 20
pre <- rpois(n, lambda = 4)
post <- rpois(n, lambda = 5)
df <- data.frame(id = c(1:n), pre = pre, post = post)
#write.csv(df, "/home/schubertj/cuni/stats/data/ads.csv", row.names = FALSE)
y <- c(df$pre, df$post)
x <- c(rep(0, 20), rep(1, 20))
aggregate(y ~ x, FUN = mean)

#H0 >= -1
#H1 < -1
t.test(y ~ x, paired = TRUE, mu = -1, alternative = "less")

##############

set.seed(1)
y <- rnorm(n=200)
x <- rbinom(n=200, size = 1, prob = c(0.5,0.5))

n <- table(x)
s2_1 <- var(y[x==0])
s2_2 <- var(y[x==1])

s2_p <- ((n[1]-1) * s2_1 + (n[2]-1) * s2_2 )/ (sum(n) - 2)

s_d <- sqrt(s2_p / n[1] + s2_p / n[2]) 


vyberova_prumery <- aggregate(y ~ x, FUN=mean)[, 2]
d <- vyberova_prumery[1] - vyberova_prumery[2]


t_statistika <- (d - 0) / s_d
t <- qt(0.11, df = (sum(n) - 2))
p <- pt(t_statistika, df = (sum(n) - 2))

### pomoci t.test funkce
t_test <- t.test(y ~ x, alternative = "less", mu = 0, var.equal = TRUE, conf.level = 0.89)
is_d <- d + qt(c(0.11, 1-0.11), df = (sum(n) - 2)) * s_d

#standard error
s_d <- t_test$stderr
kriticka_mez <- qt(0.11, df = sum(n) -2)
mu <- 0
x <- seq(-4, 4, by=0.001)
pdf <- dt(x, df= sum(n)-2)
t_statistika <- t_test$statistic

plot(x, pdf, type = "l")
abline(v=kriticka_mez, col = "red")
abline(v=t_statistika)
