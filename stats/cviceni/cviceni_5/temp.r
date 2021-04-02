n <- 5
vyberovy_prumer <- 41
smerodatna_odchylka <- 22
smerodatna_chyba <- smerodatna_odchylka / sqrt(n)
stupne_volnosti <- n-1

x <- seq(from = vyberovy_prumer - 4*smerodatna_chyba, 
         to = vyberovy_prumer + 4*smerodatna_chyba, 
         by = 0.001)

x <- seq(-4,4,by = 0.01)
pdf_n <- dnorm(x = x, mean = 0, sd = 1)
pdf_t <- dt(x = x, df = stupne_volnosti)

plot(x, pdf_n, type = "l", ylab = "f(x)", xlab = "X")
lines(x, pdf_t, col = "red")


pdf_t30 <- dt(x = x, df = 30)

lines(x, pdf_t30, col = "green")


qnorm(0.025, mean = 0, sd = 1)

# is = prumer +/- * t * smerodatna_chyba
# hladina spolehlivost = 95%
t <- qt(c(0.025, 0.975), df = stupne_volnosti)
vyberovy_prumer + t * smerodatna_chyba


### 1 vyzkumna otazka/nasbirani dat
ess <- read.csv("https://raw.githubusercontent.com/schubertjan/cuni/master/stats/data/int_use.csv")
dim(ess)
ess_bez_na <- ess[!is.na(ess$int_use_day_mins), ]


### 2. formulace hypotez
# levostranna
## H_0 >= 150
## H_1 < 150


# pravostranna
## H_0 <= 150
## H_1 > 150

# oboustranna
## H_0 = 150
## H_1 != 150


### 3. testovaci statistika
# testovaci_statistika = bodovy_odhad - nulova_hypoteza / smerodatou_chyba_odhadu
bodovy_odhad <- mean(v, na.rm = TRUE)
nulova_hypoteza <- 150
n <- sum(!is.na(ess$int_use_day_mins))
smerodatou_chyba_odhadu <- sd(ess$int_use_day_mins, na.rm = TRUE) / sqrt(n)
testovaci_statistika <- (bodovy_odhad - nulova_hypoteza) / smerodatou_chyba_odhadu


### 4. interpretace
alpha <- 0.11
kritickou_mez_levostranny <- qt(alpha, df = n-1)
x <- seq(-3,3,by=0.01)
pdf <- dt(x, df = n-1)

# levostranna
plot(x, pdf, 
     xlab = "Vyberovy prumer", 
     ylab = "f(x)", 
     main = "Rozdeleni vyberoveho prumeru za platnosti H0: levostranny",
     type = "l")
abline(v = kritickou_mez_levostranny, col = "red")
abline(v = testovaci_statistika, lty = 2)
p_hodnota <- pt(testovaci_statistika, df = n-1)

# pravostrannou
kritickou_mez_pravostranny <- qt(1-alpha, df = n-1)
plot(x, pdf, 
     xlab = "Vyberovy prumer", 
     ylab = "f(x)", 
     main = "Rozdeleni vyberoveho prumeru za platnosti H0: pravostranny",
     type = "l")
abline(v = kritickou_mez_pravostranny, col = "red")
abline(v = testovaci_statistika, lty = 2)
p_hodnota <- 1-pt(testovaci_statistika, df = n-1)


# oboustranny
kritickou_mez_oboustranny <- qt(c(alpha/2, 1-alpha/2), df = n-1)
plot(x, pdf, 
     xlab = "Vyberovy prumer", 
     ylab = "f(x)", 
     main = "Rozdeleni vyberoveho prumeru za platnosti H0: oboustranny",
     type = "l")
abline(v = kritickou_mez_oboustranny, col = "red")
abline(v = testovaci_statistika, lty = 2)
p_hodnota <- pt(testovaci_statistika, df = n-1) * 2

# d = (bodovy odhad - H0) / smerodatna odchylka
d = (vyberovy_prumer - nulova_hypoteza) / sd(ess$int_use_day_mins, na.rm = TRUE)


t_test <- t.test(ess_bez_na$int_use_day_mins, alternative = "less", mu = 150, conf.level = 0.89)

## oboustranny
## mu1, mu2
### H0: mu1-mu2 = 0
### H1: mu1-mu2 != 0

## levostranny
### H0: mu1-mu2 >= 0
### H1: mu1-mu2 < 0

## pravostranny
### H0: mu1-mu2 <= 0
### H1: mu1-mu2 > 0


#### t-test
## H0: sk0-sk1 <= 0
## H1: sk0-sk1 > 0
d1 <- read.csv("https://raw.githubusercontent.com/schubertjan/cuni/master/stats/data/viral_load.csv")
aggregate(viral_particles ~ group, data = d1, FUN = mean)
t_test <- t.test(viral_particles ~ group, 
                data = d1,
                alternative = "greater", 
                mu = 0, 
                var.equal=TRUE)

alpha <- 0.05
x <- seq(-4, 4, by=0.001)
pdf <- dt(x, df=398)
plot(x, pdf, type = "l")
kriticka_mez <- qt(1-alpha, df = 398)
abline(v=kriticka_mez, col= "red")
abline(v=t_test$statistic, lty = 2)


#######
## H0: mu_eta - mu_jed = 0
## H1: mu_eta - mu_jed != 0
d2 <- read.csv("https://raw.githubusercontent.com/schubertjan/cuni/master/stats/data/cyklo_zavody.csv")
aggregate(pocet_nedokoncilo ~ typ_zavodu, data = d2, FUN = mean)
alpha <- 0.05
t_test <- t.test(pocet_nedokoncilo ~ typ_zavodu, 
                 data = d2,
                 alternative = "two.sided", 
                 mu = 0, 
                 var.equal=FALSE, 
                 conf.level = 1-alpha)
x <- seq(-4, 4, by=0.001)
pdf <- dt(x, df=t_test$parameter)
plot(x, pdf, type = "l")
kriticka_mez <- qt(c(alpha/2, 1-alpha/2), df = t_test$parameter)
abline(v=kriticka_mez, col= "red")
abline(v=t_test$statistic, lty = 2)

