#####################    ANALYSE NUEX    ##########################
df_2ben <- df_20_big %>%
  filter(site == "nu_ex") %>%
  pull(data) %>%
  as.data.frame() %>% 
  as_tibble()

### régression laggée ###
ccf_data_plot_keddy <- ccf(log(df_2ben$keddy+0.01), df_2ben$conc_od,lag.max = 30)
ccf_data_plot_temp <- ccf(df_2ben$temp, df_2ben$conc_od,lag.max = 30)

ccf_data_keddy <- bind_cols(data.frame(cor = ccf_data_plot_keddy[[1]]),data.frame(lag = ccf_data_plot_keddy$lag))
ccf_data_temp <- bind_cols(data.frame(cor = ccf_data_plot_temp[[1]]),data.frame(lag = ccf_data_plot_temp$lag))

vec_lag_keddy <- fun_select_lag(ccf_data_keddy, n.lag = 6) %>% pull(lag)
vec_lag_temp <- fun_select_lag(ccf_data_temp, n.lag = 6) %>% pull(lag)

filter(ccf_data_keddy,lag %in% vec_lag_keddy) %>% arrange(desc(cor))
filter(ccf_data_temp,lag %in% vec_lag_temp) %>% arrange(desc(cor))

### plot séries brutes + tendance ###
plot(scale(df_2ben$keddy),type="l",col="red",ylim=c(-3,6))
lines(scale(df_2ben$conc_od),size=1)
# oxy
ts.plot(df_2ben$conc_od) 
mod_trend_ox <- lm(conc_od ~ date, data = df_2ben)
summary(mod_trend_ox) # tendance présente, variance moyennement stable= log ? sqrt ? ARCH ?
mod_trend_ox <- lm(conc_od ~ date, data = df_2ben)$res # on enlève la tendance
ts.plot(mod_trend_ox) 

summary(mod_trend_ox)
#keddy
ts.plot(df_2ben$keddy) #baisse dde la variance importante entre 300 et 600 environs
mod_trend_keddy <- lm(keddy ~ date, data = df_2ben)
summary(mod_trend_keddy) # tendance ; variance ca va ; saisonnalité 24 heures (vérifier sur ondelette)
# temp
ts.plot(df_2ben$temp) #baisse dde la variance importante entre 300 et 600 environs
mod_trend_temp <- lm(temp ~ date, data = df_2ben)
summary(mod_trend_temp)#tendance




### ACF et PACF sur données brutes + modèles sarima associé ###
# keddy
acf2(df_2ben$keddy)
acf2(diff(df_2ben$keddy))

auto.arima(log(df_2bev$keddy+0.01),stepwise = FALSE,stationary = TRUE,seasonal = TRUE,d=1,D = 1)

sarima_keddy <- sarima(df_2bev$keddy, p = 2, d = 0, q=2, P=0, D=1, Q =2, S=24) 
sarima_keddy

# conc_od
acf2(df_2ben$conc_od)
acf2(diff(df_2ben$conc_od,24),max.lag = 100)

auto.arima(df_2ben$conc_od,D=1)
sarima_conc_od <- sarima(log(df_2ben$conc_od), p = 1, d = 0, q=0, P=0, D=1, Q =1, S=24) 
sarima_conc_od

# temp
acf2(diff(df_2ben$temp))
auto.arima(df_2ben$temp,D=1)
sarima_temp <- sarima(df_2ben$temp, p = 3, d = 2, q=0, P=0, D=1, Q =1, S=24) 
sarima_temp




### CCF normal et pré blanchissage des données pour CCF
lag2.plot(df_2ben$temp,df_2ben$conc_od,max.lag = 15)
lag2.plot(log(df_2ben$keddy+1),df_2ben$conc_od,max.lag = 10)
ccf(df_2ben$keddy,df_2ben$conc_od,lag.max = 100)

res_keddy <-residuals(keddy_diff$fit)

ccf_data_plot_keddy <- ccf(df_2ben$keddy, df_2ben$conc_od,lag.max = 30)
ccf_data_plot_temp <- ccf(df_2ben$temp, df_2ben$conc_od,lag.max = 30)

ccf_data_keddy <- bind_cols(data.frame(cor = ccf_data_plot_keddy[[1]]),data.frame(lag = ccf_data_plot_keddy$lag))
ccf_data_temp <- bind_cols(data.frame(cor = ccf_data_plot_temp[[1]]),data.frame(lag = ccf_data_plot_temp$lag))

vec_lag_keddy <- fun_select_lag(ccf_data_keddy, n.lag = 6) %>% pull(lag)
vec_lag_temp <- fun_select_lag(ccf_data_temp, n.lag = 6) %>% pull(lag)



df_lag_date <- fun_df_lag(df_2ben,lagged = "keddy",vec_lag = vec_lag_keddy,var = 1:2) %>% mutate(date=df_2ben$date) %>% na.omit()
df_lag <- fun_df_lag(df_2ben,lagged = "keddy",vec_lag = vec_lag_keddy,var = 1:2) %>% na.omit()
df_lag_temp <- fun_df_lag(df_2ben,lagged = "temp",vec_lag = vec_lag_temp, var = 1:2) %>% 
  bind_cols(fun_df_lag(df_2ben,lagged = "keddy",vec_lag = vec_lag_keddy,var = 0))%>% na.omit() %>% dplyr::select(-conc_od1)

df_lag_log <- mutate_at(df_lag,vars(matches("keddy")),~log(.+0.001))
df_lag_log_temp <- mutate_at(df_lag_temp,vars(matches("keddy")),~log(.+0.001))






indice <- nrow(df_lag_log) - 300:nrow(df_lag_log)
df_train_base <- df_lag_log[indice,]
df_test_base <- df_lag_log[-indice,]
df_date <- df_lag_date[-indice,] %>% pull(date)
ts.plot(df_train_temp$conc_od)

acf2(df_train_base$conc_od)
auto.arima(df_train_base$conc_od)
wt.image(analyze.wavelet(df_train_base,"conc_od",dt=1/24))
sarima_base <-
  sarima(
    df_train_base$conc_od,
    p = 2,
    d = 1,
    q = 2,
    P = 0,
    D = 0,
    Q = 0,
    S = 0
  )
sarima_base

sarima_base_pred <-
  sarima.for(
    df_train_base$conc_od,
    n.ahead = 300,
    p = 2,
    d = 1,
    q = 2,
    P = 0,
    D = 0,
    Q = 0,
    S = 0
  )
rmse_base <- RMSE(sarima_base_pred$pred %>% as.vector(),df_test_base$conc_od)
mae_base <- MAE(sarima_base_pred$pred %>% as.vector(),df_test_base$conc_od)
sse_base <- var(df_test_base$conc_od-sarima_base_pred$pred %>% as.vector())


################################SARIMAX########################################
############################## sans température ###############################


auto.arima(df_train_base$conc_od,xreg =df_train_base[, -c(1:3)] %>% as.matrix )

sarimax_conc_od <-
  sarima(
    df_train_base$conc_od,
    p = 2,
    d = 1,
    q = 1,
    P = 0,
    D = 0,#2
    Q = 0,#2
    S = 0,
    xreg = df_train_base[, -c(1:3)] 
  )
sarimax_conc_od
acf2(residuals(sarimax_conc_od$fit)^2)

sarimax_pred_2bev <- sarima.for(
  df_train_base$conc_od,
  p = 2,
  d = 1,
  q = 1,
  P = 0,
  D = 0,
  Q = 0,
  S = 0,
  plot.all = FALSE,
  n.ahead = 300,
  newxreg =df_test_base[, -c(1:3)],
  xreg = df_train_base[, -c(1:3)]
)

sse_st <- var(df_test_base$conc_od -sarimax_pred_2bev_temp$pred %>% as.vector() )
rmse_st <- RMSE(sarimax_pred_2bev$pred %>% as.vector(),df_test_base$conc_od)
mae_st <- MAE(sarimax_pred_2bev$pred %>% as.vector(),df_test_base$conc_od)



plot(sarimax_pred_2bev$pred %>% as.vector(),col='red',type ="l")
lines(df_test_base$conc_od)
lines(sarima_base_pred$pred %>% as.vector(),col="blue")


############################## avec température ###############################
indice <- nrow(df_lag_log_temp) - 300:nrow(df_lag_log_temp)
df_train_temp <- df_lag_log_temp[indice,]
df_test_temp <- df_lag_log_temp[-indice,]
auto.arima(df_train_temp$conc_od,xreg =df_train_temp[, -c(1:3)] %>% as.matrix)

sarimax_conc_od_temp <-
  sarima(
    df_train_temp$conc_od,
    p = 0,
    d = 1,
    q = 5,
    P = 0,
    D = 0,
    Q = 0,
    S = 0,
    xreg = df_train_temp[, -c(1:3)]
  )

sarimax_conc_od_temp 

df_train_temp <- df_train_temp %>% dplyr::select(-c(temp9,temp7,temp6,temp10,temp5))
df_test_temp <- df_test_temp %>% dplyr::select(-c(temp9,temp7,temp6,temp10,temp5))

sarimax_pred_2bev_temp <- sarima.for(
  df_train_temp$conc_od,
  p = 0,
  d = 1,
  q = 5,
  P = 0,
  D = 0,
  Q = 0,
  S = 0,
  plot.all = FALSE,
  n.ahead = 300,
  newxreg = df_test_temp[, -c(1:3)],
  xreg = df_train_temp[, -c(1:3)]
)

sse_temp <- var(df_test_temp$conc_od -sarimax_pred_2bev_temp$pred %>% as.vector() )
rmse_temp <- RMSE(sarimax_pred_2bev_temp$pred %>% as.vector(),df_test$conc_od)
mae_temp <- MAE(sarimax_pred_2bev_temp$pred %>% as.vector(),df_test_temp$conc_od)

plot(sarimax_pred_2bev$pred %>% as.vector(),col='red',type ="l",ylim=c(4,10))
lines(sarimax_pred_2bev_temp$pred %>% as.vector(),col='green',type ="l",ylim=c(4,10))
lines(df_test_temp$conc_od)
lines(sarima_base_pred_temp$pred %>% as.vector(),col="blue")

data_lacanauG_abve_20_sarima_pred_1 <- bind_cols(
  ox_sarima = sarima_base_pred$pred,
  ox_sarimax_temp = sarimax_pred_2bev_temp$pred %>% as.vector(),
  ox_sarimax_sans_temp = sarimax_pred_2bev$pred %>% as.vector(),
  conc_od = df_test_temp$conc_od,
  date = df_date
) %>% gather(key = "pred",value="value",-date)

ggplot(data_lacanauG_exve_20_sarima_pred_1,aes(x=date,y=value,color = pred))+geom_line()

reg_lag <- lm(conc_od~.,data=df_lag_temp)
summary(reg_lag)
acf2(reg_lag$res)
mod <- stepAIC(reg_lag)
acf2(mod$res)
mod_AIC <- lm(log(conc_od) ~ conc_od_lag_t1  + temp11 + temp12 + temp30 + keddy2 + keddy4 + keddy0, data = df_lag_temp)
summary(mod_AIC)
acf2(mod_AIC$res)
