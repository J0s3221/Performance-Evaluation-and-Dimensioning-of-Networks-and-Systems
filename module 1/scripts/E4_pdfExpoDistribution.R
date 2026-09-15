lambda <- 1.5         # Taxa da distribuição exponencial
n_samples <- 10000   # Número de amostras

u <- runif(n_samples)
exp_numbers <- -log(1 - u) / lambda

hist(exp_numbers, 
     breaks = 50, 
     prob = TRUE, 
     main = "Distribuição Exponencial (lambda = 1.5)", 
     xlab = "Valor", 
     ylab = "Densidade",
     col = "lightgray", 
     border = "white")

x_vals <- seq(0, max(exp_numbers), length.out = 500)
pdf_vals <- dexp(x_vals, rate = lambda)

lines(x_vals, pdf_vals, col = "red", lwd = 2)