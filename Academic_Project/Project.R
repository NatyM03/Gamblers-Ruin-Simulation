#Proiect PMDS: Simularea Stochastică a problemei "Ruina Jucătorului".

capital_initial=50   #cu cati bani vine jucatorul
tinta_finala=100  #la ce suma vrea sa ajunga
p_castig=0.48 #probabilitatea de a castiga o runda (48% - tipic cazino)
nr_simulari=1000 #cati jucatori simulam (cu cat mai multi, cu atat mai precis)

# Functie care joaca un singur joc pana la final
joaca_pana_la_final=function(start, target, p) {
  bani=start
  istoric=c(start) 
  
  while(bani>0&&bani<target) { 
    rezultat=rbinom(1, 1, p)
    
    if(rezultat==1) {
      bani=bani+1
    } else {
      bani=bani-1
    }
    istoric=c(istoric,bani)
  }
  return(istoric)
}
set.seed(123) 
rezultate_lista=replicate(nr_simulari, 
                             joaca_pana_la_final(capital_initial, tinta_finala, p_castig), 
                             simplify = FALSE)
#Verificam pentru fiecare joc cum s-a terminat (0 sau tinta)
rezultate_finale=sapply(rezultate_lista, function(x) tail(x, 1))
jocuri_pierdute=sum(rezultate_finale==0)
jocuri_castigate=sum(rezultate_finale==tinta_finala)
prob_simulata=jocuri_pierdute/nr_simulari
#Calculam probabilitatea teoretica 
q=1-p_castig
ratio=q/p_castig
prob_teoretica_castig=(1-ratio^capital_initial)/(1-ratio^tinta_finala)
prob_teoretica_ruina=1-prob_teoretica_castig

cat("Capital Initial:", capital_initial, "| Tinta:", tinta_finala, "| Probabilitate Castig:", p_castig, "\n")
cat("Numar total simulari:", nr_simulari, "\n")
cat("Jucatori ruinati:", jocuri_pierdute, "\n")
cat("Jucatori castigatori:", jocuri_castigate, "\n")
cat("Probabilitatea Ruinei (SIMULATA): ", round(prob_simulata * 100, 2), "%\n", sep="")
cat("Probabilitatea Ruinei (TEORETICA):", round(prob_teoretica_ruina * 100, 2), "%\n", sep="")

#Grafic1:Traiectoria primilor 5 jucatori
#Alegem cateva jocuri interesante pentru a le desena
culori=c("red","blue","green","orange","purple")

plot(1, type="n", xlab="Numar de pariuri (timp)", ylab="Capital(bani)", 
     xlim=c(0, 500), ylim=c(0, tinta_finala), 
     main="Evolutia capitalului pentru 5 jucatori")

abline(h=capital_initial, col="gray", lty=2) #Linia de start
abline(h=tinta_finala, col="darkgreen", lwd=2) #Linia de castig
abline(h=0, col="darkred", lwd=2) #Linia de ruina
text(10, tinta_finala - 5, "Tinta(castig)", col="darkgreen", adj=0)
text(10, 5, "Ruina(faliment)", col="darkred", adj=0)

#Desenam liniile pentru 5 jocuri
for(i in 1:5) {
  lines(rezultate_lista[[i]], col=culori[i], lwd=2)
  final_x=length(rezultate_lista[[i]])
  final_y=tail(rezultate_lista[[i]], 1)
  points(final_x, final_y, pch=19, col=culori[i])
}

#Grafic2:Histogramă-Cât durează un joc?
durate=sapply(rezultate_lista, length)
hist(durate, breaks=30, col="lightblue", border="white",
     main="Distributia duratei jocului",
     xlab="Numar de pariuri pana la final",
     ylab="Frecventa")
abline(v=mean(durate), col="red", lwd=2, lty=2)

legend("topright", legend=paste("Durata Medie:", round(mean(durate))), 
       col="red", lty=2, lwd=2)

#Cum influenteaza "Norocul" (p) sansa de a da faliment? 
#Definim un interval de probabilitati (de la 40% la 60%)
lista_p=seq(0.40,0.60,by=0.02)
ruina_probabilitati=numeric(length(lista_p))


for(i in 1:length(lista_p)) {
  p_curent=lista_p[i]
  
  #Rulam simularea pentru p_curent
  rezultate_temp=replicate(500,
                              joaca_pana_la_final(capital_initial, tinta_finala, p_curent), 
                              simplify = FALSE)
  
  #Calculam rata ruinei
  finaluri=sapply(rezultate_temp, function(x) tail(x, 1))
  ruina_probabilitati[i]=sum(finaluri==0)/500
}

#GRAFIC3:Curba de senzitivitate
plot(lista_p, ruina_probabilitati, type="b", pch=19, col="blue", lwd=2,
     xlab="Probabilitatea de castig la o runda (p)",
     ylab="Probabilitatea totala de ruina",
     main="Impactul avantajului casei asupra ruinei")

#Adaugam linii de referinta
abline(v=0.5, col="red", lty=2) # Jocul Corect (Fair Game)
text(0.5, 0.5, "Joc Corect (p=0.5)", pos=4, col="red")
grid()

#Separăm duratele în funcție de rezultat
durate_ruina=durate[rezultate_finale==0]
durate_castig=durate[rezultate_finale==tinta_finala]

#Boxplot comparativ
boxplot(list(Ruina = durate_ruina, Castig = durate_castig),
        col = c("salmon", "lightgreen"),
        main = "Durata jocului: Cei care pierd vs Cei care castiga",
        ylab = "Numar de pariuri")

# Definim mize diferite: 1 leu, 2 lei, 5 lei, 10 lei, 25 lei (Miza Mica vs. Miza Mare)
mize=c(1, 2, 5, 10, 25)
rezultate_mize=numeric(length(mize))

#Functie modificata pentru a accepta miza variabila
joaca_cu_miza=function(start,target,p,miza) {
  bani=start
  while (bani>0&&bani<target) {
    #Daca mai avem mai putin decat miza, pariem tot ce avem (All in)
    pariu_curent=min(miza,bani) 
    
    if(rbinom(1,1,p)==1) {
      bani=bani+pariu_curent
    } else {
      bani=bani-pariu_curent
    }
  }
  return(bani) 
}

for(i in 1:length(mize)) {
  miza_crt=mize[i]
  #Facem 1000 de simulari pentru fiecare tip de miza
  finaluri=replicate(1000, joaca_cu_miza(capital_initial, tinta_finala, p_castig, miza_crt))
  
  #Calculam cati au dat faliment
  rezultate_mize[i]=sum(finaluri==0)/1000
}

#GRAFIC4:Barplot Comparativ
barplot(rezultate_mize, names.arg = mize,
        col = "darkorange", border = "white",
        ylim = c(0, 1),
        xlab = "Suma pariata pe runda (Miza)",
        ylab = "Probabilitatea de ruina",
        main = "Strategie: Prudent vs. Agresiv (p=0.48)")

text(x=1:5*1.2-0.5,y=rezultate_mize+0.05, 
     labels=round(rezultate_mize,2),col="black",font=2)