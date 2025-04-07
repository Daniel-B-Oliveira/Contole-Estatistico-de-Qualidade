#item b)
gera.grafico.R <- function (dados){
  library (qcc)
  
  X <- dados
  
  repeat {
    grafico <- qcc (X, type = "R", plot = TRUE)
    pontos.fora <- grafico$violations$beyond.limits  
    if (length(pontos.fora) == 0) break
    X <- X[-pontos.fora, ]
  }
  
  return (list(grafico = grafico, novos.dados = X))
}

gera.grafico.X <- function (dados, std.dev.in ){
  library (qcc)
  
  X <- dados
  
  repeat {
    grafico <- qcc (X, type = "xbar", std.dev = std.dev.in, plot =TRUE)
    pontos.fora <- grafico$violations$beyond.limits
    if (length(pontos.fora) == 0) break
    X <- X[-pontos.fora, ]
  }
  
  return (list(grafico = grafico, novos.dados = X))
}

diametros <- read.csv ("diametro_rotor.csv", header = TRUE, sep = ",")

grafico.e.dados.R <- gera.grafico.R (diametros)

grafico.R <- grafico.e.dados.R$grafico
diametros.R <- grafico.e.dados.R$novos.dados

std.dev.r <- grafico.R$std.dev

grafico.e.dados.Xbar <- gera.grafico.X (diametros.R, std.dev.r)

grafico.Xbar <- grafico.e.dados.Xbar$grafico
diametros.Xbar <- grafico.e.dados.Xbar$novos.dados

center.X <- grafico.Xbar$center

#item c)
diametros.2 <- diametros.Xbar
rownames (diametros.2) <- NULL

grafico.R.2 <- qcc (diametros.2, type = "R", std.dev = std.dev.r)
grafico.R.2 <- qcc (diametros.2, type = "xbar", std.dev = std.dev.r)

#item d)
calcula.NMA.0 <- function(k) {
  return(1/(2*pnorm(-k)))
}

calcula.NMA.0(3)


#item e)
calcula.poder <- function (k, n, delta, lambda){
  return (pnorm ((-k- (delta*sqrt (n))) / lambda) +
            pnorm ((-k+ (delta*sqrt(n))) / lambda))
}

calcula.mu.1 <- function (mu.0, sigma.0, delta){
  return (mu.0 + delta*sigma.0)
}

calcula.sigma.1 <- function(sigma.0, lambda){
  return (sigma.0*lambda)
}

#aplicação
k <- 3
n <- 5

mu.0 <- center.X
sigma.0 <- std.dev.r

M <- matrix (NA, ncol = 6, nrow = 4)

deltas <- c(1.5, 2, 3.5, 4)
lambdas <- c(1.5,2.5, 2.5 , 3)

for (i in 1:nrow(M)){
  M[i,1] <- deltas[i]
  M[i,2] <- lambdas[i]
  M[i,3] <- calcula.mu.1(mu.0, sigma.0, deltas[i])
  M[i,4] <- calcula.sigma.1(sigma.0, lambdas[i])
  M[i,5] <- calcula.poder(k, n, deltas[i], lambdas[i])
  M[i,6] <- 1 / M[i,5]
}

#item g)
set.seed (123)

diametros.fase.1 <- diametros.2
diametros.fase.2 <- diametros.fase.1[0,]

for (i in 1:25){
  diametros.amostra <- rnorm (5, mu.0, sigma.0)
  diametros.fase.2 <- rbind (diametros.fase.2, diametros.amostra)
}

grafico.R.fase.2 <- qcc (diametros.fase.1,
                         type = "R",
                         std.dev = sigma.0,
                         newdata = diametros.fase.2)
grafico.Xbar.fase.2 <- qcc (diametros.fase.1,
                            type = "xbar",
                            std.dev = sigma.0,
                            newdata = diametros.fase.2)

#Gráficos Juntos:
set.seed (123)

mu.1 <- M[,3]
sigma.1 <- M[,4]

for (j in 1:nrow(M)){
  diametros.fase.2 <- diametros.fase.1[0,]
  
  for (i in 1:25){
    diametros.amostra <- rnorm (5, mu.1[j], sigma.1[j])
    diametros.fase.2 <- rbind (diametros.fase.2, diametros.amostra)
  }
  
  grafico.R.fase.2 <- qcc(diametros.fase.1,
                          type = "R",
                          newdata = diametros.fase.2,
                          std.dev = sigma.0,
                          plot = FALSE)
  
  grafico.Xbar.fase.2 <- qcc(diametros.fase.1,
                             type = "xbar",
                             newdata = diametros.fase.2,
                             std.dev = sigma.0,
                             plot = FALSE)
  
  plot (grafico.R.fase.2,
        chart.all = FALSE,
        title = paste("Gráfico R - mdia ",
                      round(mu.1[j],2),
                      "e desvio ",
                      round(sigma.1[j],2)))
  
  plot (grafico.Xbar.fase.2,
        chart.all = FALSE,
        title = paste("Gráfico Xbar - mdia ",
                      round(mu.1[j],2),
                      "e desvio ",
                      round(sigma.1[j],2)))
}