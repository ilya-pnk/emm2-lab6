# лаб №6

set.seed(123) 

# Параметры
S0 = 100
a = 0.5
sigma = 0.8
delta = 0.01
n = 1000

# Генерируем шум для B
eps = rnorm(n+1, mean = 0, delta)

# вектор для броуновского движения
B = numeric(n+1)
for (k in 2:(n+1)) {
  B[k] <- B[k - 1] + eps[k]
}

# вектора для GBD
S = numeric(n+1)
S[1] = S0 

# создание GBD
for (k in 1:n) {
  S[k + 1] <- S[1] * exp((a - 0.5 * sigma^2 ) * (k * delta^2) + sigma * B[k + 1])
}

# График процесса
plot(seq(0, n * delta^2, by = (delta^2)), S, type = "l",
     col = "darkgreen", xlab = "t", ylab = "S(t)",
     main = "геометрическое броуновское движение")

# Задание 2

vec = numeric(length(S) - 1)  #вектор приращений

# лог разность соседних значений процесса
for (k in 2:length(S)) {
  vec[k - 1] <- log(S[k] / S[k - 1]) 
}

# Оценки пар-ов  
mu_new = sum(vec) / length(vec)

sigma_new = sum((vec - mu_new) ^ 2) / length(vec)

# Преобразовываем оценки
sigma_GBD = sigma_new / delta^2 
a_GBD = mu_new / delta^2 + sigma_GBD / 2 

cat("Оценка волатильности:", sigma_GBD, "\n")
cat("Оценка сноса:", a_GBD, "\n")
