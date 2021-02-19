5*4*3*2*1
factorial(5)

choose(10, 5)

### Pascaluv trojuhelnik
for(n in 0:10){
  print(choose(n, k = 0:n))
}

# Ekvivalence kombinacniho cisla a upraveneho produktu
choose(10, 5)
prod(10:6)/prod(5:1)

choose(100, 20) == prod(100:81)/prod(20:1)


##### Cilem dvou nasledujicich prikladu je predstavit funkci "combn" a procvicit for loop
# Z matematickeho hlediska je cilem procvicit uvazovani o kombinacich a pocitani pravdepodobnosti jevu (priklad 5)
combn(x, m, FUN = NULL, simplify = TRUE, ...)
##### Priklad 4: Kolika zpusoby lze rozdelit 4 divky a 8 chlapcu do dvou sesticlennych druzstev tak, aby v kazdem druzstvu byla 2 devcata a chlapci?
### A.) Vypocet s vyuzitim funkce "combn"
# Vytvorime vektor o delce 12, ktery bude obsahovat 4 devcata a 12 chlapcu
deti <- c(rep("d", 4), rep("ch", 8))
# S vyuzitim funkce combn vypocitame vsechny mozne sesticlenne druzstva (jedna se o kombinace bez opakovani: 1 devce/chlapec muze byt v jednom druzstvu pouze jednou)
mozna_druzstva <- t(combn(deti, 6))
class(mozna_druzstva)
dim(mozna_druzstva)

# S vyuzitim for loop vypocitame, kolik devcat je v kazde kombinaci
cetnosti <- matrix(rep(NA, nrow(mozna_druzstva)*2), ncol = 2)
for(i in 1:nrow(mozna_druzstva)){
  cetnosti[i, 1] <- as.numeric(sum(mozna_druzstva[i, ] == "d"))
  cetnosti[i, 2] <- as.numeric(sum(mozna_druzstva[i, ] == "ch"))
}

# Nakonec spocitame odpoved na otazku ze zadani: zajimame se pouze o druzstva, kde jsou dve devcata a ctyri kluci
table(cetnosti[,1] == 2)

### B.) Analyticky vypocet: vybirame dve devcata ze ctyr [choose(4, 2)] a ctyri kluky z osmy [choose(8, 4)]
choose(4, 2)*choose(8, 4)





##### Priklad 5: Pokud bychom tymy z predchoziho prikladu rozdelovali nahodne, jaka je pravdepodobnost, ze v jednom tymu budou 3 devcata a 3 chlapci?
### A.) Vypocet s vyuzitim funkce "combn" - prebirame objekty z predchazejiciho prikladu
# Delime tedy pocet druzstev ve kterych jsou prave tri divky celkovym pocterm sesticlennych druzstev
table(cetnosti[,1] == 3)[2]/nrow(mozna_druzstva)

### B.) Analyticky vypocet: vybirame tri devcata ze ctyr [choose(4, 3)] a tri kluky z osmy [choose(8, 4)]
# Pocet techto vyhovujicich kombinaci musime podelit poctem vsech kombinaci [choose(12,6)] - poctem vsech moznych sesticlennych druzstev
(choose(4, 3)*choose(8, 3))/choose(12,6)



### Problem s narozeninami
# Jaka je pravdepodobnost, ze alespon tri studenti z naseho kurzu slavi narozeniny ve stejny den?
?pbirthday

pbirthday(n = 133, coincident = 3)

# Vypocitejte pravdepodobnosti, ze alespon tri studenti slavi narozeniny ve stejny den pro 3 az 133 studentu.
studenti <- 3:133
pravdepodobnosti <- rep(NA, length(3:133))

for(i in 1:length(studenti)){
  pravdepodobnosti[i] <- pbirthday(i, coincident = 3)
}

tabulka <- data.frame(studenti, pravdepodobnosti)
View(tabulka)


### Napad na domaci ukol: naprogramujte funkci pro pocitani variaci bez opakovani
variace_bez_opakovani <- function(k, n){
  if(k <= 0 | k %% 1 !=0) stop("Prvni argument k musi byt prirozene cislo vetsi nez 0")
  if(n <= 0 | n %% 1 !=0) stop("Druhy argument n musi byt prirozene cislo vetsi nez 0")
  if(k > n) stop("Cislo n musi byt vetsi nebo rovne k (n >= k)")
  return(prod(n:(n-k+1)))
}

variace <- function(k, n, opakovani){
  if(k <= 0 | k %% 1 != 0) stop("Prvni argument k musi byt prirozene cislo vetsi nez 0")
  if(n <= 0 | n %% 1 != 0) stop("Druhy argument n musi byt prirozene cislo vetsi nez 0")
  if(k > n) stop("Cislo n musi byt vetsi nebo rovne k (n >= k)")
  if(missing(opakovani)) stop("Treti argument opakovani neni definovan (vyberte prosim: TRUE/FALSE)")
  if(opakovani == FALSE){
    return(prod(n:(n-k+1)))
  } else if(opakovani == TRUE)
    return(n^k)
}

### testy
variace(0, 5, TRUE)
variace(5, 0, TRUE)
variace(2, 5)
variace(5, 2, TRUE)
variace(2, 5, TRUE)
variace(2, 5, FALSE)
variace(15, 1000, TRUE)
