# Exercício 8: probabilidade de obter exatamente duas caras
# ao lançar cinco moedas viciadas.

p_head <- 0.7
n_coins <- 5
n_experiments <- 1000

set.seed(8)

# Cada linha representa uma experiência com cinco lançamentos.
random_values <- matrix(
  runif(n_experiments * n_coins),
  nrow = n_experiments,
  ncol = n_coins
)

coin_tosses <- random_values < p_head
number_of_heads <- rowSums(coin_tosses)

# Estimativa obtida por simulação.
simulation_probability <- mean(number_of_heads == 2)

# Valor exato: P(X = 2), com X ~ Binomial(5, 0.7).
exact_probability <- choose(n_coins, 2) *
  p_head^2 *
  (1 - p_head)^(n_coins - 2) 

cat("Probabilidade estimada por simulação:", simulation_probability, "\n")
cat("Probabilidade exata:", exact_probability, "\n")
cat("Erro absoluto:", abs(simulation_probability - exact_probability), "\n")
