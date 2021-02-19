# Cviceni 1: VEKTORY
# ------------------------------------------------

### R muze slouzit take jako kalkulacka
# predstaveni nejcasteji pouzivanych numerickych operatoru v R
5+6 # scitani
19-11 # odcitani
5*6 # nasobeni
19/11 # deleni
5^2 # umocnovani
5**2 # umocnovani


### Vytvareni objektu/promennych v R
## Skalar
### ----------------------------------------------
### VYZKOUSEJTE VYTVORIT SI VLASTNI OBJEKTY
### ----------------------------------------------
honza <- 1987
ivan <- 1986
print(honza)
honza # vysledej je stejny

### ----------------------------------------------
### VYZKOUSEJTE SI VYPOCITAT SVUJ VEK
### ----------------------------------------------
vek_honza <- 2020 - honza
vek_ivan <- 2020 - ivan
print(vek_honza)

## Vektor
# pouziti prvni funkce c() - kratky uvod k funkcim v R
# vol??n?? funkce m?? v R n??sleduj??c?? obecnou formu: n??zev_funkce(argumenty_funkce)
?c # zobrazeni napovedy k funkci c()
### ----------------------------------------------
### VYZKOUSEJTE SI VYTVORIT VEKTOR SE JMENY SVYCH RODINNYCH PRISLUSNIKU A 
### JEJICH ROKEM NAROZENI, VYSKOU A JMENEM 
### ----------------------------------------------
rok_narozeni_vector <- c(1987, 1986, 2001, 2004)
vyska_vector <- c(163, 184, 180, 190)
jmena_vector <- c("Honza", "Ivan", "Petr", "Tonda") # character vector

print(rok_narozeni_vector)
# dve dulezite funkce pro zjistovani vlastnosti vektoru (class obecneji objektu v R)
class(rok_narozeni_vector)
length(rok_narozeni_vector) # delka vektoru (length) neni to same jako velikost vektoru (probirana na prednasce)



# ------------------------------------------------
# OPERACE S VEKTORY
# ------------------------------------------------

## Skalar
# ------------------------------------------------
### odcitani
vek_vector <- 2020 - rok_narozeni_vector
print(vek_vector)

### scitani
vek_konec_bakalare_vector <- vek_vector + 3
print(vek_konec_bakalare_vector)

### deleni
vyska_stopa_vector <- vyska_vector / 30.48

### nasobeni
vyska_palec_vector <- vyska_stopa_vector * 12

## Vector
# ------------------------------------------------
### scitani vektoru
### ----------------------------------------------
### VYZKOUSEJTE SI VYTVORIT VLASTNI PRIKLAD PRICTENI VEKTORU
### ----------------------------------------------
delka_studia <- c(3, 4, 5, 3)
vek_konec_bakalare_individualni_vector <- vek_vector + delka_studia

### odcitani
delka_studia_korekce <- c(0, 1, 0, 1)
vek_konec_bakalare_individualni_vector2 <- vek_konec_bakalare_individualni_vector - delka_studia_korekce

### skalarni soucin vektoru 
### Dotaz: jsou vektory "vek" a "delka studia" ortogonalni?
# vektory jsou ortogonalni, kdyz je jejich skalarni soucin rovny 0
# V R-ku muzeme skalarni soucin vektoru vypocitat dvema zpusoby: maticovym nasobenim (operator %*%) nebo s vyuzitim funkce sum
vek_vector %*% delka_studia # v??sledkem je matice 1x1
sum(vek_vector * delka_studia) # vysledkem je skalar



### Linearni ne/zavislost vektoru
# Reseni prikladu 3 ze zadani cviceni 

### priklad 3a
# vytvoreni tri vektoru ze zadani
u <- c(1, 2, 0)
v <- c(-1, -2, 1)
w <- c(1, 1, 1)

# vytvoreni nuloveho vektoru
null <- c(0, 0, 0)

# slouceni tri vektoru do matice
matice_a <- cbind(u, v, w)

# reseni systemu tri linearnich rovnic se tremi neznamymi
solve(matice_a, null)
# resenim soustavy je nulovy vektor (jedna se tedy o trivialni linearni kombinaci vektoru)
# vektory jsou linearne NEZAVISLE


### priklad 3b
# vytvoreni tri vektoru ze zadani
x <- c(1, -1, 1)
y <- c(2, 3, 0)
z <- c(-1, -4, 1)

# slouceni tri vektoru do matice
matice_b <- cbind(x, y, z)
det(matice_b) # matice_b je singularni (determinant teto matice je 0)

# reseni systemu tri linearnich rovnic se tremi neznamymi
solve(matice_b, null)
# existuje nekonencny pocet netrivialnich linearnich kombinaci
# vektory jsou linearne ZAVISLE

# Priklady netrivialnich linearnich kombinaci splnujici reseni
reseni1 <- c(-1, 1, 1)
reseni2 <- c(-2, 2, 2)
reseni3 <- -1.5*c(-2, 2, 2)


# zkouska spravnoti
matice_b%*%reseni1
matice_b%*%reseni2
matice_b%*%reseni3
