# DADOS -------------------------------------------------------------------
library(ggplot2); library(reshape2)
list.files()
dados = read.csv('dados.csv')
names(dados)
ano = function(vetor){
  retval = as.POSIXlt(as.Date(vetor))$year + 1900
  return(retval)
}
mes = function(vetor){
  retval = as.POSIXlt(as.Date(vetor))$mon + 1
  return(retval)  
}
summary(dados)
dados$RISCO[is.na(dados$RISCO)] = 0
# PARAMETROS POPULACIONAIS ------------------------------------------------
m1 = .750 #probabilidades de perda da populacao de alto risco
m2 = .5 #probabilidades de perda da populacao de baixo risco
lambda = -20 #parametro de demanda (propensao de entrada de pessoas na carteira) 
tau = 1 #especialidade de demanda (ver HAO,2015)

# SELECAO ADVSERSA E LOSS COVERAGE --------------------------------------------------------
advsl_lc = function(m1,m2,tau,lambda){
  risk_H = dados$COD_USR[dados$RISCO >= m1]
  risk_L = dados$COD_USR[dados$RISCO < m2]
  
  p1 = length(risk_H) / length(dados$COD_USR)
  p2 = length(risk_L) / length(dados$COD_USR)
  
  pi1 = m1*p1
  pi2 = m2*p2
  
  expected_loss = (m1*p1) + (m2*p2)
  
  demand_function = function(lambda){
    tau = 1
    d1 = tau*(mean(dados$premio_medio[dados$COD_USR %in% risk_H]) / mean(dados$RISCO[dados$COD_USR %in% risk_H]))**-lambda
    d2 = tau*(mean(dados$premio_medio[dados$COD_USR %in% risk_L]) / mean(dados$RISCO[dados$COD_USR %in% risk_L]))**-lambda
    return(c(d1,d2))
  }
  
  expected_coverage = demand_function(lambda)[1]*p1*pi1 + demand_function(lambda)[2]*p2*pi2
  expected_loss_coverage = demand_function(lambda)[1]*p1*pi1*m1 + demand_function(lambda)[2]*p2*pi2*m2
  
  rho = expected_loss_coverage/expected_coverage
  
  as = rho / expected_loss
  
  lc = c(expected_loss_coverage, expected_loss)
  
  advsl = c(as, expected_coverage, expected_loss_coverage, rho)
  
  return(list(advsl, lc))
}

# TAXA DE SELECAO ADVERSA -------------------------------------------------
adverse_selection_ratio = function(m1,m2,tau,lambda){
  risk_L = dados$COD_USR[dados$RISCO <= m1]
  risk_H = dados$COD_USR[dados$RISCO >= m2]
  
  p1 = length(risk_L) / length(dados$COD_USR)
  p2 = length(risk_H) / length(dados$COD_USR)
  
  pi1 = m1*p1
  pi2 = m2*p2
  
  expected_loss = (m1*p1) + (m2*p2)
  
  demand_function = function(lambda){
    tau = 1
    d1 = tau*(mean(dados$premio_medio[dados$COD_USR %in% risk_L]) / mean(dados$RISCO[dados$COD_USR %in% risk_L]))**-lambda
    d2 = tau*(mean(dados$premio_medio[dados$COD_USR %in% risk_H]) / mean(dados$RISCO[dados$COD_USR %in% risk_H]))**-lambda
    alfa1 = (tau*p1)/(tau*p1 + tau*p2)
    alfa2 = (tau*p2)/(tau*p1 + tau*p2)
    return(c(d1,d2,alfa1,alfa2))
  }
  
  expected_coverage = demand_function(lambda)[1]*p1*pi1 + demand_function(lambda)[2]*p2*pi2
  expected_loss_coverage = demand_function(lambda)[1]*p1*pi1*m1 + demand_function(lambda)[2]*p2*pi2*m2
  
  rho = expected_loss_coverage/expected_coverage
  
  as_ratio = rho / (demand_function(lambda)[3]*m1 + demand_function(lambda)[4]*m2)
  
  return(as_ratio)
}

# FUNCOES DE DEMANDA ------------------------------------------------------
demand_function_premio =function(m1,m2 = (1-m1),tau,lambda, premio1,premio2){
  risk_H = dados$COD_USR[dados$RISCO >= m1]
  risk_L = dados$COD_USR[dados$RISCO < m2]
  
  p1 = length(risk_H) / length(dados$COD_USR)
  p2 = length(risk_L) / length(dados$COD_USR)
  
  pi1 = m1*p1
  pi2 = m2*p2

  tau = 1
  d1 = tau*(premio1 / mean(dados$RISCO[dados$COD_USR %in% risk_H]))**-lambda
  d2 = tau*(premio2 / mean(dados$RISCO[dados$COD_USR %in% risk_L]))**-lambda
  alfa1 = (tau*p1)/(tau*p1 + tau*p2)
  alfa2 = (tau*p2)/(tau*p1 + tau*p2)
  return(c(d1,d2,alfa1,alfa2))
}  #DEMANDA EM FUNCAO DO PREMIO
demand_function_lambda =function(m1,m2 = (1-m1),tau,lambda){
  risk_H = dados$COD_USR[dados$RISCO >= m1]
  risk_L = dados$COD_USR[dados$RISCO < m2]
  
  p1 = length(risk_H) / length(dados$COD_USR)
  p2 = length(risk_L) / length(dados$COD_USR)
  
  pi1 = m1*p1
  pi2 = m2*p2
  
  expected_loss = (m1*p1) + (m2*p2)
  
  tau = 1
  d1 = tau*(mean(dados$premio_medio[dados$COD_USR %in% risk_H]) / mean(dados$RISCO[dados$COD_USR %in% risk_H]))**-lambda
  d2 = tau*(mean(dados$premio_medio[dados$COD_USR %in% risk_L]) / mean(dados$RISCO[dados$COD_USR %in% risk_L]))**-lambda
  alfa1 = (tau*p1)/(tau*p1 + tau*p2)
  alfa2 = (tau*p2)/(tau*p1 + tau*p2)
  return(c(d1,d2,alfa1,alfa2))
} #DEMANDA EM FUNCAO DO LAMBDA
demand_function_risco =function(m1,m2 = (1-m1),tau,lambda){
  risk_H = dados$COD_USR[dados$RISCO >= m1]
  risk_L = dados$COD_USR[dados$RISCO < m2]
  
  p1 = length(risk_H) / length(dados$COD_USR)
  p2 = length(risk_L) / length(dados$COD_USR)
  
  pi1 = m1*p1
  pi2 = m2*p2
  
  tau = 1
  d1 = tau*(mean(dados$premio_medio[dados$COD_USR %in% risk_H]) / mean(dados$RISCO[dados$COD_USR %in% risk_H]))**-lambda
  d2 = tau*(mean(dados$premio_medio[dados$COD_USR %in% risk_L]) / mean(dados$RISCO[dados$COD_USR %in% risk_L]))**-lambda
  alfa1 = (tau*p1)/(tau*p1 + tau*p2)
  alfa2 = (tau*p2)/(tau*p1 + tau*p2)
  return(c(d1,d2,alfa1,alfa2))
} #DEMANDA EM FUNCAO DO RISCO


# FUNCOES DE PLOTAGEM --------------------------------------------
plot_advs = function(m1,m2,lambda){
  d = data.frame(matrix(NA, nrow = 100, ncol = 3))
  names(d) = c('m1','m2','as')
  
  d$m1 = seq(m1,1, length.out = 100)
  d$m2 = seq(0,m2, length.out = 100)
  
  for (i in 1:100) {
    d$as[i] = advsl_lc(d$m1[i], d$m2[i], tau, lambda)[[1]][1]
  }
  
  plot(d$m1, d$as, t = 'l', lwd = 1, 
       main = paste0('Alto risco >= ', m1, ', baixo risco < ', m2, ', lambda = ', lambda), xlab = TeX('$\\mu_{1}$'), ylab = 'advs',  cex.main=0.81)
  abline(h = 1, col  = 'red', lty = 'dotted')
}
plot_demanda_premio = function(m1, lambda){
  demanda = data.frame(matrix(NA, nrow = 100, 4))
  names(demanda) = c('premio1','premio2','d1','d2')
  demanda$premio2 = seq(0.01,0.40,length.out = 100)
  demanda$premio1 = seq(0.65,1,length.out = 100)
  for (i in 1:100) {
    demanda$d1[i] = demand_function_premio(m1,(1-m1),1,lambda,demanda$premio1[i],demanda$premio2[i])[1]
    demanda$d2[i] = demand_function_premio(m1,(1-m1),1,lambda,demanda$premio1[i],demanda$premio2[i])[2]
  }
  
  par(mfrow = c(1,2))
  plot(demanda$premio1, demanda$d1, t = 'l', xlab = expression(Pi[1]), ylab = expression(d[1]), main = 'Demanda em função do prêmio')
  mtext(bquote(mu[1]==.(m1)~lambda == .(lambda)), side = 3, line = 0)
  plot(demanda$premio2, demanda$d2, t = 'l', xlab = expression(Pi[2]), ylab = expression(d[2]), main = 'Demanda em função do prêmio')
  mtext(bquote(mu[1]==.(m1)~lambda == .(lambda)), side = 3, line = 0)
  
} #DEMANDA EM FUNCAO DO PREMIO
plot_demanda_lambda = function(m1){
  demanda = data.frame(matrix(NA, nrow = 100, 3))
  names(demanda) = c('lambda','d1','d2')
  demanda$lambda = seq(-1,1,length.out = 100)
  for (i in 1:100) {
    demanda$d1[i] = demand_function_lambda(m1,(1-m1),1,demanda$lambda[i])[1]
    demanda$d2[i] = demand_function_lambda(m1,(1-m1),1,demanda$lambda[i])[2]
  }
  
  plot(demanda$lambda, demanda$d1, t = 'l', xlab = expression(lambda), ylab = 'demanda', main = bquote(.('Demanda em função de ')~lambda))
  lines(demanda$lambda, demanda$d2, col = 'red')
  text(x=-0.58, y=150, expression(d[1]), col = "red")
  text(x=-1, y=150, expression(d[2]), col = "black")
  mtext(bquote(mu[1]==.(m1)), side = 3, line = 0)
  
} #DEMANDA EM FUNCAO DO LAMBDA
plot_demanda_risco = function(lambda){
  demanda = data.frame(matrix(NA, nrow = 100, 4))
  names(demanda) = c('risco_alto','risco_baixo','d1','d2')
  demanda$risco_alto = seq(0.60,0.95,length.out = 100)
  demanda$risco_baixo = 1-demanda$risco_alto
  for (i in 1:100) {
    demanda$d1[i] = demand_function_risco(demanda$risco_alto[i],demanda$risco_baixo[i],1,lambda)[1]
    demanda$d2[i] = demand_function_risco(demanda$risco_alto[i],demanda$risco_baixo[i],1,lambda)[2]
  }
  
  par(mfrow = c(1,2))
  plot(demanda$risco_alto, demanda$d1, t = 'l', xlab = expression(mu[1]), ylab = expression(d[1]), main = 'Demanda em função do risco')
  mtext(bquote(lambda==.(lambda)), side = 3, line = 0)
  plot(demanda$risco_baixo, demanda$d2, t = 'l', xlab = expression(mu[2]), ylab = expression(d[2]), main = 'Demanda em função do risco')
  mtext(bquote(lambda==.(lambda)), side = 3, line = 0)
} #DEMANDA EM FUNCAO DO RISCO

# GRAFICOS DE DEMANDA EM FUNCAO DO LAMBDA -----------------------------------------------------
plot_demanda_lambda(m1 = .65)
# GRAFICOS DE DEMANDA EM FUNCAO DO PREMIO -----------------------------------------------------
plot_demanda_premio(m1 = .65,lambda = -0.5)

# GRAFICOS DE DEMANDA EM FUNCAO DO RISCO -----------------------------------------------------
plot_demanda_risco(lambda = -0.5)
  
# GRAFICOS TAXA DE SELECAO ADVERSA ----------------------------------------------------------------
  d = data.frame(matrix(NA, nrow = 20, ncol = 4))
  names(d) = c('lambda',paste0('ratio_advsl_C',1:3))
  
  d$lambda = seq(0,5.5, length.out = 20)
  m1 = c(.01,.04,.07)
  
  for (i in 1:20) {
    d$ratio_advsl_C1[i] = adverse_selection_ratio(m1[1],m1[1]*6,tau,d$lambda[i])
    d$ratio_advsl_C2[i] = adverse_selection_ratio(m1[2],m1[2]*6,tau,d$lambda[i])
    d$ratio_advsl_C3[i] = adverse_selection_ratio(m1[3],m1[3]*6,tau,d$lambda[i])
  }
  
  labs = c(expression(paste(mu[1] == .01,' ,', ~ mu[2] == 0.06)),
           expression(paste(mu[1] == .04,' ,', ~ mu[2] == 0.24)),
           expression(paste(mu[1] == .07,' ,', ~ mu[2] == 0.42)))
  
  ggplot(melt(d, id.vars = 'lambda', value.name = 'ratio', variable.name = 'cenario'), 
    aes(lambda, ratio, group = cenario)) + 
    # geom_line(aes(linetype = cenario)) + 
     geom_line() +
     geom_point(aes(shape = cenario), size = 4) + scale_shape_manual(values = c(15,16,17), labels=labs) +
     scale_linetype_manual(name = 'Cenário', labels=labs, values=c("solid", 'twodash', 'dotted')) +
    theme_bw() + 
    theme(axis.line = element_line(size=1, colour = "black"),
          panel.grid.major = element_line(), panel.grid.minor = element_blank(),
          panel.border = element_blank(), panel.background = element_blank(), 
          axis.title.x = element_text(size = 16), axis.text.x = element_text(size = 16, color ='black'),
          axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 16, color ='black'),
    legend.title = element_blank(),
    legend.text = element_text(size = 16, color ='black'),
    legend.key.size = unit(1, 'cm'))+
    xlab(expression(lambda)) + ylab('Taxa de seleção adversa')  +
    geom_hline(yintercept = c(max(d$ratio_advsl_C1),max(d$ratio_advsl_C2),max(d$ratio_advsl_C3)), col = 'red', linetype = 'dashed', size = .7) +
    geom_text(x=5.68, y=max(d$ratio_advsl_C1)-.01, label=as.character(round(max(d$ratio_advsl_C1),2)), col = 'red', family = 'mono') +
    geom_text(x=5.68, y=max(d$ratio_advsl_C2)-.01, label=as.character(round(max(d$ratio_advsl_C2),2)), col = 'red', family = 'mono') +
    geom_text(x=5.68, y=max(d$ratio_advsl_C3)-.01, label=as.character(round(max(d$ratio_advsl_C3),2)), col = 'red', family = 'arial')

    
  

