
prumer_iq <- 100
sd_iq <- 15
x <- seq(60, 140, by = 0.001)
hustota_p <- dnorm(x = x, mean = prumer_iq, sd = sd_iq)

plot(x, 
     hustota_p, 
     xlab = "IQ", 
     ylab = "f(x)", 
     main = "N~(100, 15)", 
     type = "l")

pnorm(120, mean = prumer_iq, sd = sd_iq)

# centralni limitni veta
vek <- read.csv("https://raw.githubusercontent.com/schubertjan/cuni/master/stats/data/vek.csv")

plot(vek$vek, vek$pocet, type = "h", 
     xlab = "Vek", ylab = "Cetnost", main = "Rozlozeni veku v CR")

prumerny_vek_populace <- sum(vek$vek * vek$pocet) / sum(vek$pocet)

N <- 1000
p <- vek$pocet / sum(vek$pocet)
vyber <- sample(vek$vek, size = N, replace = TRUE, prob = p)

mean(vyber)


N <- 10000
n <- 1000
vyberovy_prumer <- rep(0, N)
for(i in c(1:N)) {
  vyber <- sample(vek$vek, size = n, replace = TRUE, prob = p)
  vyberovy_prumer[i] <- mean(vyber)
}

hist(vyberovy_prumer, 
     xlab = "Vyberovy prumer", 
     ylab = "Cetnost", 
     main = "Rozlozeni vyberovych prumeru")

smerodatna_chyba <- sd(vyber) / sqrt(n)
mean(vyberovy_prumer)

x <- seq(38,43,by = 0.001)

par(mfrow = c(2,1), mar = c(4,2,2,2))
hist(vyberovy_prumer, 
     xlab = "Vyberovy prumer", 
     ylab = "Cetnost", 
     main = "Rozlozeni vyberovych prumeru", 
     xlim = c(38, 43))
plot(x, dnorm(x, mean(vyberovy_prumer), smerodatna_chyba), type = "l")


# IS = vyberovy_prumer +/- hladina_spolehlivosti * smerodatna_chyba
head(vyber)
hladiva_spolehlivosti <- qnorm(0.025, mean = 0, sd = 1)
smerodatna_chyba <- sd(vyber) / sqrt(n)

mean(vyber) + qnorm(c(0.025, 0.975), mean = 0, sd = 1) * smerodatna_chyba

mean(vyber) + hladiva_spolehlivosti * smerodatna_chyba
mean(vyber) + -1 * hladiva_spolehlivosti * smerodatna_chyba


