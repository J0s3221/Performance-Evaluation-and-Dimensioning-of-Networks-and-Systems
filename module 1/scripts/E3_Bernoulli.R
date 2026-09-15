p <- 0.7            # Probabilidade Sucesso
n_samples <- 1000   # Amostras

u <- runif(n_samples) #Ramdom Number

bernoulli_numbers <- ifelse(u < p, 1, 0)

hist(bernoulli_numbers, 
     breaks = c(-0.5, 0.5, 1.5), 
     main = "Histograma de Números de Bernoulli (p = 0.7)", 
     xlab = "Resultado", 
     ylab = "Frequência",
     col = "lightblue", 
     xaxt = "n")
  
axis(1, at = c(0, 1))