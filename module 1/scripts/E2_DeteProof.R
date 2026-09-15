set.seed(123)
seq1 <- runif(5)

seq_random <- runif(5)

set.seed(123)
seq2 <- runif(5)

identical(seq1, seq2)