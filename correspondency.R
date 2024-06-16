library(reshape2)
library(factoextra)

# Wczytaj dane z pliku CSV
dane <- read.csv("https://raw.githubusercontent.com/KacperWardzala/correspondence_analysis/main/KULT_2159_CTAB_20240614100327%20-%20KULT_2159_CTAB_20240614100327.csv.csv", sep=",", header=TRUE)

data <- dane[3:6]

dane_vector <- as.vector(t(data))
data <- matrix(dane_vector, nrow=16, ncol=4, byrow=TRUE)

wojewodztwa <- dane$Wojewodztwo
sporty <- colnames(dane[3:6])

rownames(data) <- wojewodztwa
colnames(data) <- sporty

chisq.test(data)

total_sum <- sum(data)
row_sums <- rowSums(data)
col_sums <- colSums(data)

P <- data/total_sum
PP <- outer(rowSums(P),colSums(P))
E = (P-PP)/sqrt(PP)
heatmap(E, scale='none', Colv = NA)

# Dekompozycja SVD

svd_result <- svd(E)
U <- svd_result$u
S <- diag(svd_result$d)
V <- svd_result$v

plot(rbind(U[,1:2], V[,1:2]), lwd = 3)
biplot(MASS::corresp(data, nf = 2))

library(ca)
plot(ca::ca(data), mass=c(T,T))
summary(ca::ca(data))