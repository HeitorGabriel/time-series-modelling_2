library(tidyverse)
library(plotly)
library(ggpubr)
# tidyverts core
library(tsibble)
library(fable)
library(feasts)
# tidyverts improvements tools
library(tsibbletalk)
library(fable.prophet)
library(fabletools)
# Multivariáveis
library(vars)
library(tsDyn)

load("workspace.RData")

graphics.off()  # clear all graphs
rm(list = ls()) # remove all files from your workspace

# DIÁRIO: ___ 00 de aaa ________________________________________________________

# ______________________________________________________________________________

# 1) Tratamento da Base =============================================

dds <- read_csv("dds.csv")
dds |> glimpse()

dds <- dds |> filter(!is.na(petro))

dds$cambio <- dds$cambio |>
	str_replace(pattern = ",", replacement = ".") |>
	as.numeric()

dds$dt <- seq(from = lubridate::ym('1990 Jan'),
				  to   = lubridate::ym('2022 Jul'), # alternativa: length=
				  by   = "month")

dds <- dds |> mutate("ptr_br_log"      = log(cambio) + log(petro),
							"ipca_log"        = log(ipca),
							"petro_log"       = log(petro),
							"cambio_log"      = log(cambio),
							"petro_brent_log" = log(petro_brent),
							"petro_dubai_log" = log(petro_dubai),
							"petro_texas_log" = log(petro_texas))
dds |> glimpse()
dds |> View()

dds <- dds |> filter(dt >= lubridate::ym("1994 Jul"))
dd <- dds |>
	dplyr::select(dt, ptr_br_log, ipca_log, cambio_log, petro_log) |> 
	dplyr::mutate(dt = tsibble::yearmonth(as.character(dt))) |>
	as_tsibble(index = dt)

dd |> interval()

# 2) Visualizações ==================================================

ggplotly(dd |>
				ggplot() +
				geom_line(aes(x=dt, y=ipca_log),
							 color='seagreen4') +
				geom_line(aes(x=dt, y=ptr_br_log),
							 color='deepskyblue3') +
				ylab('Logaritmo') +
				xlab('Tempo')+
				ggtitle("Logaritmo do Índice do IPCA e do Preço Internacional do Petróleo em Reais."))

plt_cambio <- plot_ly(dds,type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~dt, y = ~cambio,
				 name = "Câmbio",
				 line = list(color = "#30d5c8", width = 2)) |> 
	layout(showlegend = F,
			 title='Taxa de Câmbio - R$/US$ - Comercial - Compra - Média - R$ ',
			 xaxis = list(rangeslider = list(visible = T),
			 				 title = "Data",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 yaxis = list(title = "R$/US$",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')
plt_cambio

plt_ipca <- plot_ly(dds,type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~dt, y = ~ipca,
				 name = "IPCA",
				 line = list(color = "#551a8b", width = 2)) |> 
	layout(showlegend = F,
			 title='índice de Preços ao Consumidor Amplo - IPCA, Brasil, de Jan/90 a Jul/2022.',
			 xaxis = list(rangeslider = list(visible = T),
			 				 title = "Data",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 yaxis = list(title = "Índice",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')
plt_ipca

plt_petro <- plot_ly(dds,type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~dt, y = ~petro,
				 name = "Petroleum",
				 line = list(color = "#2E8B57", width = 2)) |> 
	layout(showlegend = F,
			 title='Média do Preço Internacional do Petróleo',
			 xaxis = list(rangeslider = list(visible = T),
			 				 title = "Data",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 yaxis = list(title = "Preço em Dólar",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')
plt_petro

plt_petro_n <- plot_ly(dds,type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~dt, y = ~petro*cambio,
				 name = "Petroleum (R$)",
				 line = list(color = "#073320", width = 2)) |> 
	layout(showlegend = F,
			 title='Média do Preço Internacional do Petróleo, em Reais',
			 xaxis = list(rangeslider = list(visible = T),
			 				 title = "Data",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 yaxis = list(title = "Preço em Reais",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')
plt_petro_n

plt2 <- plot_ly(dd,type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~dt, y = ~ipca_log,
				 name = "Log(IPCA)",
				 line = list(color = "#2E8B57", width = 2)) |> 
	add_trace(x = ~dt, y = ~ptr_br_log,
				 name = "Log(Petro)",
				 line = list(color = "#009acd", width = 2)) |> 
	layout(showlegend = T,
			 title='IPCA e do Preço Internacional do Petróleo em Reais.',
			 xaxis = list(rangeslider = list(visible = T),
			 				 title = "Data",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 yaxis = list(title = "Índices Log",
			 				 zerolinecolor = '#ffff',
                      zerolinewidth = 2,
                      gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')

	
plt2

# 3) Tendências e Raíz Unitária =====================================

dd |> gg_subseries(ptr_br_log) |> plotly_build()

dd |> 
	dplyr::select(ptr_br_log, dt) |>
	model(classical_decomposition(
		ptr_br_log,
		type = "multiplicative")) |>
	components() |>
	autoplot() |> 
	plotly_build() |> 
	layout(title = htmltools::HTML("Decomposição Multiplicativa Clássica:\npetro_br_log = trend * seasonal * randon"))

dd |> feasts::ACF(ptr_br_log, lag_max = 36) |> autoplot()
dd |> feasts::ACF(ipca_log, lag_max = 36) |> autoplot()

# KPSS unit root test H0: is trend-stationary (is stationary around a deterministic trend) or series has no unit root, I(0).
#			 "in the KPSS test, the absence of a unit root is not a proof of stationarity but, by design, of trend-stationarity. This is an important distinction since it is possible for a time series to be non-stationary, have no unit root yet be trend-stationary"
#  types specify as deterministic component either a constant "mu" or a constant with linear trend "tau".

# PP test H0: ϕ=1 or I(1). H1: ϕ < 1

dd |> features( ptr_br_log, c(unitroot_kpss, unitroot_pp))
        # KPSS: há raíz unitária. PP: estacionário
dd |> features( ipca_log, c(unitroot_kpss, unitroot_pp))
        # KPSS: há raíz unitária. PP: raíz unitária
dd |> features( petro_log, c(unitroot_kpss, unitroot_pp))
        # KPSS: há raíz unitária. PP: estacionário
dd |> features( cambio_log, c(unitroot_kpss, unitroot_pp))
        # KPSS: há raíz unitária. PP: estacionário

dd <- dd |> 
	dplyr::mutate(
		ptr_br_log_dif   = difference(ptr_br_log, lag = 1),
		ipca_log_dif     = difference(ipca_log, lag = 1),
		petro_log_dif    = difference(petro_log, lag = 1),
		cambio_log_dif   = difference(cambio_log, lag = 1),
		ptr_br_log_dif12 = difference(ptr_br_log, lag = 12),
		ipca_log_dif12   = difference(ipca_log, lag = 12),
		petro_log_dif12  = difference(petro_log, lag = 12),
		cambio_log_dif12 = difference(cambio_log, lag = 12)) |>
	dplyr::filter(!is.na(petro_log_dif12))

dd |> features(ptr_br_log_dif, c(unitroot_kpss, unitroot_pp))
dd |> features(ipca_log_dif, c(unitroot_kpss, unitroot_pp))
dd |> features(petro_log_dif, c(unitroot_kpss, unitroot_pp))
dd |> features(cambio_log_dif, c(unitroot_kpss, unitroot_pp))
dd |> features(ptr_br_log_dif12, c(unitroot_kpss, unitroot_pp))
dd |> features(ipca_log_dif12, c(unitroot_kpss, unitroot_pp))
dd |> features(petro_log_dif12, c(unitroot_kpss, unitroot_pp))
dd |> features(cambio_log_dif12, c(unitroot_kpss, unitroot_pp))
#		Novamente, exceto em petro_log_dif, os testes estão "contradizendo-se",
#		uma possível explicação é que:
#		"Reject unit root, reject stationarity: both hypotheses are component hypotheses – heteroskedasticity in a series may make a bigs difference; if there is structural break it will affect inference." (https://stats.stackexchange.com/questions/30569/what-is-the-difference-between-a-stationary-test-and-a-unit-root-test)
#		Portanto, vamos ver os gráficos:
attach(dd)
for (i in c("ptr_br_log_dif","ipca_log_dif","petro_log_dif","cambio_log_dif")) {
	plot.default( x=as.Date(dt), y=get(i),ylab=i, type="l")
}
# Vamos retirar de julho de 99 pra baixo, quando o Brasil adotou o regime flutuante para o câmbio:

dd <- dd |> filter(dt >= lubridate::my("Jul 1999"))
attach(dd)
#		Realizando novamente os testes KPSS e PP acima, temos que aceitar e rejeitar H0, respectivamente. Ou seja, ambos apontam estacionariedade.

ggarrange(
dd |> feasts::ACF(ptr_br_log_dif, lag_max = 36) |> ggplot2::autoplot(),
dd |> feasts::PACF(ptr_br_log_dif, lag_max = 36) |> ggplot2::autoplot()
#,labels = "F.A.'s de Dif( Log( Preço do Petro em Reais ))"
)
ggarrange(
dd |> feasts::ACF(ipca_log_dif, lag_max = 36) |> autoplot(),
dd |> feasts::PACF(ipca_log_dif, lag_max = 36) |> autoplot()
#,labels = "F.A.'s  de Dif( Log( IPCA ))"
) # Maior influnência do passado na contemporânea
ggarrange(
dd |> feasts::ACF(petro_log_dif, lag_max = 36) |> autoplot(),
dd |> feasts::PACF(petro_log_dif, lag_max = 36) |> autoplot()
#,labels = "F.A.'s de Dif( Log(  Preço do Petro ))"
)
ggarrange(
dd |> feasts::ACF(cambio_log_dif, lag_max = 36) |> autoplot(),
dd |> feasts::PACF(cambio_log_dif, lag_max = 36) |> autoplot()
#,labels = "F.A.'s de Dif( Log( R$/US$ ))"
)

# Os FAC e FACP de dif_12 mostram alta persistência dos erros e significância de vários lags múltiplos de 6. É horrível diferenciar em 12.

# 4) VAR p selection and Model ======================================
var1_p_select <- VARselect(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log_dif, petro_log_dif, cambio_log_dif),
	lag.max = 12,
	season = 12,
	type = "both")
var1_p_select$selection

var2_p_select <- VARselect(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log_dif, ptr_br_log_dif),
	lag.max = 12,
	season = 12,
	type = "both")
var2_p_select$selection

var1 <- VAR(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log_dif, cambio_log_dif, petro_log_dif),
	p = 1,
	type = "both",
	season = NULL,
	exog = NULL)
var11 <- VAR(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log_dif, cambio_log_dif, petro_log_dif),
	p = 4,
	type = "both",
	season = NULL,
	exog = NULL)
var111 <- VAR(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log_dif, cambio_log_dif, petro_log_dif),
	p = 1,
	type = "both",
	season = 12L,
	exog = NULL)

var2 <- VAR(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log_dif, ptr_br_log_dif),
	p = 1,
	type = "both",
	season = NULL,
	exog = NULL)
var22 <- VAR(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log_dif, ptr_br_log_dif),
	p = 4,
	type = "both",
	season = NULL,
	exog = NULL)

summary(var1)
summary(var11)
summary(var111)
summary(var2)
summary(var22)
#		Há diferenças significativas entre var1 e var2, com "season = 12L". Principalmente para a eq do IPCA, da dummie 4 a 8.
#		p=4 parece não ser parcimonioso pelas significâncias dos coefs, mas produz um R^2 ajust melhor (maior) que em p=1.
#		Foi testado tbm com p=2, nenhum lag 2 é significativo.
#		De const e trend, somente const de ipca foi significativo
#		Vamos refazer var1 com const e var2 com o par ipca e ptr_br:

var1 <- vars::VAR(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log_dif, cambio_log_dif, petro_log_dif),
	p = 1,
	type = "const",
	season = NULL,
	exog = NULL)

var2 <- vars::VAR(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log_dif, ptr_br_log_dif),
	p = 1,
	type = "const",
	season = NULL,
	exog = NULL)

realz_fit <- dplyr::tibble(
	dd |> dplyr::as_tibble() |> dplyr::select(dt),
	var1[["y"]] |> as.data.frame(),
	var2$y |> as.data.frame() |> dplyr::select(ptr_br_log_dif),
	"v1_fitted_ipca" = c(NA, var1$varresult$ipca_log_dif$fitted.values),
	"v1_fitted_cambio" = c(NA, var1$varresult$cambio_log_dif$fitted.values),
	"v1_fitted_petro" = c(NA, var1$varresult$petro_log_dif$fitted.values),
	"v2_fitted_ipca"= c(NA,var2$varresult$ipca_log_dif$fitted.values),
	"v2_fitted_ptr"= c(NA,var2$varresult$ptr_br_log_dif$fitted.values)				  ) |> 
	dplyr::filter(!is.na(v2_fitted_ptr))


cor(realz_fit$ipca_log_dif, realz_fit$v1_fitted_ipca)
cor(realz_fit$ipca_log_dif, realz_fit$v2_fitted_ipca)

resids <- dplyr::tibble(
	dd |> dplyr::as_tibble() |> dplyr::select(dt) |> tail(-1),
	"v1_ipca" = var1[["varresult"]][["ipca_log_dif"]][["residuals"]],
	"v1_cambio" = var1[["varresult"]][["cambio_log_dif"]][["residuals"]],
	"v1_petro" = var1[["varresult"]][["petro_log_dif"]][["residuals"]],
	"v2_ipca" = var2[["varresult"]][["ipca_log_dif"]][["residuals"]],
	"v2_ptr" = var2[["varresult"]][["ptr_br_log_dif"]][["residuals"]]
)

dplyr::tibble(realz_fit$ipca_log_dif - realz_fit$v1_fitted_ipca,
				  c(NA,resids$v1_ipca))
# portanto, as datas e os NA's colocados em fitted estão certos!

summary(var1)
summary(var1, equation="ipca_log_dif")
Acoef(var1)
summary(var2)
summary(var2, equation="ipca_log_dif")

# 5) Diagnóstico dos Resíduos do VAR  ===============================

# 5.1.1) Autocorrelação: Breusch-Godfrey?
#		H0: os coeficientes do lag do erro sao insignificantes.

test_autocorr_var1 <- serial.test(
	var1,
	lags.pt = 12, type = "BG")
test_autocorr_var1
#		p-valor: 0.04735, i.e., há autocorrelação dos erros.

test_autocorr_var2 <- serial.test(
	var2,
	lags.pt = 12, type = "BG")
test_autocorr_var2
#		p-valor: 0.1228, i.e., os coeficientes do lag do erro sao insignificantes.

# 5.1.2) Autocorrelação: Ljung-Box
#		LB: null hypothesis of independence in a given time series.

resids$v1_ipca |> ljung_box(lag = 12)
resids$v1_cambio |> ljung_box(lag = 12)
resids$v1_petro |> ljung_box(lag = 12)

resids$v2_ipca |> ljung_box(lag = 12)
resids$v2_ptr |> ljung_box(lag = 12)

# 5.1.3) Autocorrelação: FAC e FACP nos Resíduos
ggarrange(
	ACF(resids |> 
		 	dplyr::select(dt, v1_ipca) |> 
		 	as_tsibble(),
		 lag_max = 36) |> 
		autoplot()
	,PACF(resids |> 
				dplyr::select(dt, v1_ipca) |> 
				as_tsibble(),
			lag_max = 36) |> 
		autoplot() 
)

ggarrange(
	ACF(resids |> 
		 	dplyr::select(dt, v1_cambio) |> 
		 	as_tsibble(),
		 lag_max = 36) |> 
		autoplot()
	,PACF(resids |> 
				dplyr::select(dt, v1_cambio) |> 
				as_tsibble(),
			lag_max = 36) |> 
		autoplot() 
)

ggarrange(
	ACF(resids |> 
		 	dplyr::select(dt, v1_petro) |> 
		 	as_tsibble(),
		 lag_max = 36) |> 
		autoplot()
	,PACF(resids |> 
				dplyr::select(dt, v1_petro) |> 
				as_tsibble(),
			lag_max = 36) |> 
		autoplot() 
)

ggarrange(
	ACF(resids |> 
		 	dplyr::select(dt, v2_ipca) |> 
		 	as_tsibble(),
		 lag_max = 36) |> 
		autoplot()
	,PACF(resids |> 
				dplyr::select(dt, v2_ipca) |> 
				as_tsibble(),
			lag_max = 36) |> 
		autoplot() 
)

ggarrange(
	ACF(resids |> 
		 	dplyr::select(dt, v2_ptr) |> 
		 	as_tsibble(),
		 lag_max = 36) |> 
		autoplot()
	,PACF(resids |> 
				dplyr::select(dt, v2_ptr) |> 
				as_tsibble(),
			lag_max = 36) |> 
		autoplot() 
)



# 5.2) Heterocedasticidade: ARCH Multiplicador de Lagrange
#		H0: coeficients of the past vech() dont explain the actual vech()

test_var1_arch <- arch.test(var1,
									 lags.multi = 12,
									 multivariate.only = TRUE)
test_var1_arch
#		p-value = 1.368e-08: Não há heterocedasticidade nos resíduos.

test_var2_arch <- 
	arch.test(var2, lags.multi = 12, multivariate.only = TRUE)
test_var2_arch
#		p-value = 0.001181: Não há heterocedasticidade nos resíduos.

# 5.3) Normalidade: Teste de JB
#		H0: aproxima-se de uma normal em assimetria e curtose

test_var1_norm <- 
	normality.test(var1,multivariate.only = TRUE)
test_var1_norm
#		rejeita H0 de normalidade

test_var2_norm <- 
	normality.test(var2,multivariate.only = TRUE)
test_var2_norm
#		rejeita H0 de normalidade

# 5.4) Quebra Estrutural: Teste CUSUM

test_var1_cusum <- stability(var1, type = "OLS-CUSUM")
test_var1_cusum |> plot()

test_var2_cusum <- stability(var2, type = "OLS-CUSUM")
test_var2_cusum |> plot()

ggplot(resids) +
	stat_qq(aes(sample=v1_ipca), color="steelblue") 
ggplot(resids) +
	stat_qq(aes(sample=v1_cambio), color="seagreen")
ggplot(resids) +
	stat_qq(aes(sample=v1_petro), color="tomato")
	

# 6) VAR Estrutural =================================================

# 6.1) Análise Impulso-Resposta
# 6.2) Decomposição da Variância
# 6.3) Causalidade de Granger

# 7) Teste de Cointegração de Johansen ==============================

johansen_var1_trc <- ca.jo(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log, cambio_log, petro_log),
	type = "trace",
	ecdet = "const",
	K = 2,
	spec = "transitory",
	season = 12,
	dumvar = NULL
)
johansen_var1_max <- ca.jo(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log, cambio_log, petro_log),
	type = "eigen",
	ecdet = "const",
	K = 2,
	spec = "transitory",
	season = 12,
	dumvar = NULL
)

johansen_var1_trc |> summary()
johansen_var1_max |> summary()
# por ambos os testes, a estatística 6.86 indica que H0: r <= 1 pode ser aceita,
# mas a estatística de H0: r = 0 não pode ser aceita.
# Ou seja, há pelo menos 1 vetor de cointegração independente.

johansen_var2_trc <- ca.jo(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log, ptr_br_log),
	type = "trace",
	ecdet = "none",
	K = 2,
	spec = "transitory",
	season = 12,
	dumvar = NULL
)
johansen_var2_max <- ca.jo(
	dd |> dplyr::as_tibble() |>
		dplyr::select(ipca_log, ptr_br_log),
	type = "eigen",
	ecdet = "none",
	K = 2,
	spec = "transitory",
	season = 12,
	dumvar = NULL
)

johansen_var2_trc |> summary()
johansen_var2_max |> summary()

# Johansen test model 2 with trend, const and none.
#		const: not cointegrated
#		trend: r=0
#		none:  r=1

# 8) VECM ===========================================================

vecm1 <- tsDyn::VECM(
	dd |> dplyr::as_tibble() |> dplyr::select(ipca_log, cambio_log,petro_log),
	lag = 1,
	r=1,
	include = "const",
	estim = "2OLS",
	LRinclude = "none"
)

vecm2 <- tsDyn::VECM(
	dd |> dplyr::as_tibble() |> dplyr::select(ipca_log, cambio_log,petro_log),
	lag = 1,
	r=1,
	include = "const",
	estim = "ML",
	LRinclude = "none"
)

vecm3 <- tsDyn::VECM(
	dd |> dplyr::as_tibble() |> dplyr::select(ipca_log, cambio_log,petro_log),
	lag = 1,
	r=1,
	include = "none",
	estim = "2OLS",
	LRinclude = "none"
)
vecm3 |> tsDyn::plot_ECT()
#vecm3 |> toLatex()
vecm3 |> residuals()
vecm3 |> vcov()

vecm4 <- tsDyn::VECM(
	dd |> dplyr::as_tibble() |> dplyr::select(ipca_log, cambio_log,petro_log),
	lag = 1,
	r=1,
	include = "both",
	estim = "2OLS",
	LRinclude = "none"
)

vecm5 <- tsDyn::VECM(
	dd |> dplyr::as_tibble() |> dplyr::select(ipca_log, cambio_log,petro_log),
	lag = 1,
	r=1,
	include = "none",
	estim = "2OLS",
	LRinclude = "both"
)

vecm1 |> summary()
vecm2 |> summary()
vecm3 |> summary()
vecm3 |> summary(equation="ipca_log")
vecm4 |> summary()
vecm5 |> summary()

# Error Correction Term

ect_vecm3 <- dplyr::tibble(
	dd |> dplyr::as_tibble() |> dplyr::select(dt),
	"ect" = vecm3[["model"]][["ECT"]]
)


plt_ECT_vecm3 <- plot_ly(ect_vecm3,
								  type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~as.Date(dt), y = ~ect,
				 name = "Error Correction Term",
				 line = list(color = "#551a8b", width = 2)) |> 
	layout(showlegend = T,
			 title='Termo de Ajuste do Curto-prazo em VECM3',
			 xaxis = list(rangeslider = list(visible = T),
			 				 title = "Data",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 yaxis = list(title = "Dif Log",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')
plt_ECT_vecm3

ect_vecm3 |> as_tsibble() |> 
	model(classical_decomposition(
		ect,
		type = "multiplicative")) |>
	components() |>
	autoplot() |> 
	plotly_build() |> 
	layout(title = htmltools::HTML("Decomposição Multiplicativa Clássica:\nECT = trend * seasonal * randon"))

# Comparing IPCA
realz_fit_vecm3 <- dplyr::tibble(
	dd |> dplyr::as_tibble() |> dplyr::select(dt),
	"realiz_ipca" = vecm3[["model"]] |> 
		dplyr::as_tibble() |> pull(`ipca_log -1`),
	"fit_ipca" = c(0,0,0,vecm3[["fitted.values"]] |>
							dplyr::as_tibble() |> pull(ipca_log))
)

plt_ipca_vecm3 <- plot_ly(realz_fit_vecm3,
								  type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~as.Date(dt), y = ~realiz_ipca,
				 name = "IPCA",
				 line = list(color = "#551a8b", width = 2)) |> 
	add_trace(x = ~as.Date(dt), y = ~fit_ipca,
				 name = "Fitted",
				 line = list(color = "#0e0417", width = 2, dash = 'dot')) |> 
	layout(showlegend = T,
			 title='Dif(Log(IPCA)) Realizado e Estimado em VECM3',
			 xaxis = list(rangeslider = list(visible = T),
			 				 title = "Data",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 yaxis = list(title = "Dif Log IPCA",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')
plt_ipca_vecm3

# 9) Formas Estruturais =================================================

# Matriz de coeficientes estimados. para modelos -A ou -AB do VAR
A_var <- matrix(NA, nrow=2, ncol=2)
A_var[2,1] <- 0 
A_var

# Matriz de impacto de longo prazo estimada do VECM.
LR_vecm <- matrix(NA, nrow=3, ncol=3) 
LR_vecm[3,1:2] <- 0
LR_vecm[2,1] <- 0
LR_vecm

# Matriz de impacto contemporâneo estimado do VECM.
SR_vecm <- matrix(NA, nrow=3, ncol=3)
SR_vecm

svar <- SVAR(var2, Amat=A_var, estmethod="direct", lrtest=F)
svec <-  SVEC(johansen_var1_max,
				  LR = LR_vecm, SR = SR_vecm, r = 1,
				  lrtest=F, boot=TRUE,runs=1000)

svar |> summary()
svec |> summary()

# 10) Função Impulso-resposta ===========================================

ir_svar <- irf(svar, response = "ipca_log_dif", n.ahead = 12, boot = TRUE)
ir_svar |> plot()
ir_dd_svar <- tibble(
	"ir_ipca_u_log_dif" = ir_svar[["Upper"]][["ipca_log_dif"]][,1],
	"ir_ipca_log_dif"   = ir_svar[["irf"]][["ipca_log_dif"]][,1],
	"ir_ipca_l_log_dif" = ir_svar[["Lower"]][["ipca_log_dif"]][,1],
	"ir_ptr_u_log_dif"  = ir_svar[["Upper"]][["ptr_br_log_dif"]][,1],
	"ir_ptr_log_dif"    = ir_svar[["irf"]][["ptr_br_log_dif"]][,1],
	"ir_ptr_l_log_dif"  = ir_svar[["Lower"]][["ptr_br_log_dif"]][,1]
)

ir_svar_plt1 <- plot_ly(ir_dd_svar, type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~1:13, y = ~ir_ipca_u_log_dif,
				 name = "IC Sup",
				 showlegend = FALSE,
				 line = list(color = 'transparent')) |> 
	add_trace(x = ~1:13, y = ~ir_ipca_l_log_dif,
				 name = "IC Inf",
				 showlegend = FALSE,
				 fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
				 line = list(color = 'transparent')) |> 
	add_trace(x = ~1:13, y = ~ir_ipca_log_dif,
				 name = "IR-IPCA",
				 showlegend = FALSE,
				 line = list(color='rgb(0,100,80)')) |> 
	layout(title='SVAR Impulso-resposta de IPCA no IPCA',
			 xaxis = list(#rangeslider = list(visible = T),
			 				 title = "Meses",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 yaxis = list(title = "Impacto em Dif(Log(IPCA))",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')

ir_svar_plt1

ir_svar_plt2 <- plot_ly(ir_dd_svar, type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~1:13, y = ~ir_ptr_u_log_dif,
				 name = "IC Sup",
				 showlegend = FALSE,
				 line = list(color = 'transparent')) |> 
	add_trace(x = ~1:13, y = ~ir_ptr_l_log_dif,
				 name = "IC Inf",
				 showlegend = FALSE,
				 fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)',
				 line = list(color = 'transparent')) |> 
	add_trace(x = ~1:13, y = ~ir_ptr_log_dif,
				 name = "IR-IPCA",
				 showlegend = FALSE,
				 line = list(color='rgb(0,100,80)')) |> 
	layout(title='SVAR Impulso-resposta de R$ Preto no IPCA',
			 xaxis = list(#rangeslider = list(visible = T),
			 				 title = "Meses",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 yaxis = list(title = "Impacto em Dif(Log(IPCA))",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')

ir_svar_plt2



ir_svec <- irf(svec, response = "ipca_log", n.ahead = 36, boot = TRUE)
ir_svec |> plot()
ir_dd_svec <- tibble(
	"ir_ipca_u_log" = ir_svec[["Upper"]][["ipca_log"]][,1],
	"ir_ipca_log"   = ir_svec[["irf"]][["ipca_log"]][,1],
	"ir_ipca_l_log" = ir_svec[["Lower"]][["ipca_log"]][,1],
	"ir_petro_u_log"  = ir_svec[["Upper"]][["petro_log"]][,1],
	"ir_petro_log"    = ir_svec[["irf"]][["petro_log"]][,1],
	"ir_petro_l_log"  = ir_svec[["Lower"]][["petro_log"]][,1],
	"ir_cambio_u_log"  = ir_svec[["Upper"]][["cambio_log"]][,1],
	"ir_cambio_log"    = ir_svec[["irf"]][["cambio_log"]][,1],
	"ir_cambio_l_log"  = ir_svec[["Lower"]][["cambio_log"]][,1]
)

ir_svec_plt1 <- plot_ly(ir_dd_svec, type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~1:37, y = ~ir_ipca_u_log,
				 name = "IC Sup",
				 showlegend = FALSE,
				 line = list(color = 'transparent')) |> 
	add_trace(x = ~1:37, y = ~ir_ipca_l_log,
				 name = "IC Inf",
				 showlegend = FALSE,
				 fill = 'tonexty', fillcolor='rgba(0, 104, 201,0.2)',
				 line = list(color = 'transparent')) |> 
	add_trace(x = ~1:37, y = ~ir_ipca_log,
				 name = "IR-IPCA",
				 showlegend = FALSE,
				 line = list(color='rgb(0, 104, 201)')) |> 
	layout(title='SVEC Impulso-resposta de IPCA no IPCA',
			 xaxis = list(#rangeslider = list(visible = T),
			 	title = "Meses",
			 	zerolinecolor = '#ffff',
			 	zerolinewidth = 2,
			 	gridcolor = 'ffff'),
			 yaxis = list(title = "Impacto em Dif(Log(IPCA))",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')

ir_svec_plt1

ir_svec_plt2 <- plot_ly(ir_dd_svec, type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~1:37, y = ~ir_cambio_u_log,
				 name = "IC Sup",
				 showlegend = FALSE,
				 line = list(color = 'transparent')) |> 
	add_trace(x = ~1:37, y = ~ir_cambio_l_log,
				 name = "IC Inf",
				 showlegend = FALSE,
				 fill = 'tonexty', fillcolor='rgba(0, 104, 201,0.2)',
				 line = list(color = 'transparent')) |> 
	add_trace(x = ~1:37, y = ~ir_cambio_log,
				 name = "IR-Câmbio",
				 showlegend = FALSE,
				 line = list(color='rgb(0, 104, 201)')) |> 
	layout(title='SVEC Impulso-resposta de Câmbio no IPCA',
			 xaxis = list(#rangeslider = list(visible = T),
			 	title = "Meses",
			 	zerolinecolor = '#ffff',
			 	zerolinewidth = 2,
			 	gridcolor = 'ffff'),
			 yaxis = list(title = "Impacto em Dif(Log(IPCA))",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')

ir_svec_plt2

ir_svec_plt3 <- plot_ly(ir_dd_svec, type = 'scatter', mode = 'lines') |> 
	add_trace(x = ~1:37, y = ~ir_petro_u_log,
				 name = "IC Sup",
				 showlegend = FALSE,
				 line = list(color = 'transparent')) |> 
	add_trace(x = ~1:37, y = ~ir_petro_l_log,
				 name = "IC Inf",
				 showlegend = FALSE,
				 fill = 'tonexty', fillcolor='rgba(0, 104, 201,0.2)',
				 line = list(color = 'transparent')) |> 
	add_trace(x = ~1:37, y = ~ir_petro_log,
				 name = "IR-Câmbio",
				 showlegend = FALSE,
				 line = list(color='rgb(0, 104, 201)')) |> 
	layout(title='SVEC Impulso-resposta de US$ Petro no IPCA',
			 xaxis = list(#rangeslider = list(visible = T),
			 	title = "Meses",
			 	zerolinecolor = '#ffff',
			 	zerolinewidth = 2,
			 	gridcolor = 'ffff'),
			 yaxis = list(title = "Impacto em Dif(Log(IPCA))",
			 				 zerolinecolor = '#ffff',
			 				 zerolinewidth = 2,
			 				 gridcolor = 'ffff'),
			 plot_bgcolor='#e5ecf6')

ir_svec_plt3

# 11) Decomposição da Variância =========================================

fevd_svar_ipca <- fevd(svar, n.ahead = 24)$ipca_log_dif
fevd_svar_ipca <- fevd_svar_ipca |> 
	dplyr::as_tibble() |> add_column("m" = 1:24)

fevd_svar_plt <- plot_ly(fevd_svar_ipca, type = "bar") |> 
	add_trace(x = ~m, y = ~ipca_log_dif,   name="IPCA",
				 marker = list(color = '#009779')) |> 
	add_trace(x = ~m, y = ~ptr_br_log_dif, name="Petro",
				 marker = list(color = '#ca0028')) |> 
	layout(title = "Decomposição da Variância SVAR no IPCA",
			 barmode = 'stack',
			 xaxis = list(title = "Meses"),
			 yaxis = list(title = "Dif(Log( IPCA ))"))
fevd_svar_plt

fevd_svec_ipca <- fevd(svec, n.ahead = 24)$ipca_log
fevd_svec_ipca <- fevd_svec_ipca |> 
	dplyr::as_tibble() |> add_column("m" = 1:24)

fevd_svec_plt <- plot_ly(fevd_svec_ipca, type = "bar") |> 
	add_trace(x = ~m, y = ~ipca_log,   name="IPCA",
				 marker = list(color = '#009ac9')) |> 
	add_trace(x = ~m, y = ~cambio_log, name="Câmbio",
				 marker = list(color = '#00c950')) |> 
	add_trace(x = ~m, y = ~petro_log, name="Petro",
				 marker = list(color = '#ff922c')) |> 
	layout(title = "Decomposição da Variância SVEC no IPCA",
			 barmode = 'stack',
			 xaxis = list(title = "Meses"),
			 yaxis = list(title = "Dif(Log( IPCA ))"))
fevd_svec_plt

# 12) Decomposição Histórica ============================================

# carregar a função de Daniel Ryback (https://stackoverflow.com/questions/36950491/historical-decomposition-in-r)

VARmakexy <- function(DATA,lags,c_case){
	
	nobs <- nrow(DATA)
	
	#Y matrix 
	Y <- DATA[(lags+1):nrow(DATA),]
	Y <- DATA[-c(1:lags),]
	
	#X-matrix 
	if (c_case==0){
		X <- NA
		for (jj in 0:(lags-1)){
			X <- rbind(DATA[(jj+1):(nobs-lags+jj),])
		} 
	} else if(c_case==1){ #constant
		X <- NA
		for (jj in 0:(lags-1)){
			X <- rbind(DATA[(jj+1):(nobs-lags+jj),])
		}
		X <- cbind(matrix(1,(nobs-lags),1), X) 
	} else if(c_case==2){ # time trend and constant
		X <- NA
		for (jj in 0:(lags-1)){
			X <- rbind(DATA[(jj+1):(nobs-lags+jj),])
		}
		trend <- c(1:nrow(X))
		X <-cbind(matrix(1,(nobs-lags),1), t(trend))
	}
	A <- (t(X) %*% as.matrix(X)) 
	B <- (as.matrix(t(X)) %*% as.matrix(Y))
	
	Ft <- ginv(A) %*% B
	
	retu <- list(X=X,Y=Y, Ft=Ft)
	return(retu)
}

companionmatrix <- function (x) 
{
	if (!(class(x) == "varest")) {
		stop("\nPlease provide an object of class 'varest', generated by 'VAR()'.\n")
	}
	K <- x$K
	p <- x$p
	A <- unlist(Acoef(x))
	companion <- matrix(0, nrow = K * p, ncol = K * p)
	companion[1:K, 1:(K * p)] <- A
	if (p > 1) {
		j <- 0
		for (i in (K + 1):(K * p)) {
			j <- j + 1
			companion[i, j] <- 1
		}
	}
	return(companion)
}

VARhd <- function(Estimation){
	
	## make X and Y
	nlag    <- Estimation$p   # number of lags
	DATA    <- Estimation$y   # data
	QQ      <- VARmakexy(DATA,nlag,1)
	
	
	## Retrieve and initialize variables 
	invA    <- t(chol(as.matrix(summary(Estimation)$covres)))   # inverse of the A matrix
	Fcomp   <- companionmatrix(Estimation)                      # Companion matrix
	
	#det     <- c_case                                           # constant and/or trends
	F1      <- t(QQ$Ft)                                         # make comparable to notes
	eps     <- ginv(invA) %*% t(residuals(Estimation))          # structural errors 
	nvar    <- Estimation$K                                     # number of endogenous variables
	nvarXeq <- nvar * nlag                                      # number of lagged endogenous per equation
	nvar_ex <- 0                                                # number of exogenous (excluding constant and trend)
	Y       <- QQ$Y                                             # left-hand side
	#X       <- QQ$X[,(1+det):(nvarXeq+det)]                    # right-hand side (no exogenous)
	nobs    <- nrow(Y)                                          # number of observations
	
	
	## Compute historical decompositions
	
	# Contribution of each shock
	invA_big <- matrix(0,nvarXeq,nvar)
	invA_big[1:nvar,] <- invA
	Icomp <- cbind(diag(nvar), matrix(0,nvar,(nlag-1)*nvar))
	HDshock_big <- array(0, dim=c(nlag*nvar,nobs+1,nvar))
	HDshock <- array(0, dim=c(nvar,(nobs+1),nvar))
	
	for (j in 1:nvar){  # for each variable
		eps_big <- matrix(0,nvar,(nobs+1)) # matrix of shocks conformable with companion
		eps_big[j,2:ncol(eps_big)] <- eps[j,]
		for (i in 2:(nobs+1)){
			HDshock_big[,i,j] <- invA_big %*% eps_big[,i] + Fcomp %*% HDshock_big[,(i-1),j]
			HDshock[,i,j] <-  Icomp %*% HDshock_big[,i,j]
		} 
		
	} 
	
	HD.shock <- array(0, dim=c((nobs+nlag),nvar,nvar))   # [nobs x shock x var]
	
	for (i in 1:nvar){
		
		for (j in 1:nvar){
			HD.shock[,j,i] <- c(rep(NA,nlag), HDshock[i,(2:dim(HDshock)[2]),j])
		}
	}
	
	return(HD.shock)
	
}

dh_var <- VARhd(var2)

dh_ipca_svar_dd <- dplyr::tibble(
	"hd_ipca" = dh_var[,,1][,1],
	"hd_br_ptr" = dh_var[,,1][,2],
	"m" = c(0:276)) |> 
	filter(!is.na(hd_ipca))

dh_svar_plt <- plot_ly(dh_ipca_svar_dd |> dplyr::slice(1:36)
							  , type = "bar") |> 
	add_trace(x = ~m, y = ~hd_ipca,   name="IPCA",
				 marker = list(color = '#009779')) |> 
	add_trace(x = ~m, y = ~hd_br_ptr, name="Petro",
				 marker = list(color = '#ca0028')) |> 
	layout(title = "Decomposição da Histórica de 3 Anos do SVAR no IPCA",
			 barmode = 'stack',
			 xaxis = list(title = "Meses"),
			 yaxis = list(title = "Dif(Log( IPCA ))"))
dh_svar_plt

save.image("workspace.RData")
