df_2ban <- df_20_big %>%
  filter(site == "nu_ab") %>%
  pull(data) %>%
  as.data.frame() %>% 
  as_tibble()


### régression laggée ###
ccf_data_plot_keddy <- ccf(log(df_2ban$keddy+0.01), df_2ban$conc_od,lag.max = 30)
ccf_data_plot_temp <- ccf(df_2ban$temp, df_2ban$conc_od,lag.max = 30)

ccf_data_keddy <- bind_cols(data.frame(cor = ccf_data_plot_keddy[[1]]),data.frame(lag = ccf_data_plot_keddy$lag))
ccf_data_temp <- bind_cols(data.frame(cor = ccf_data_plot_temp[[1]]),data.frame(lag = ccf_data_plot_temp$lag))

vec_lag_keddy <- fun_select_lag(ccf_data_keddy, n.lag = 6) %>% pull(lag)
vec_lag_temp <- fun_select_lag(ccf_data_temp, n.lag = 6) %>% pull(lag)

filter(ccf_data_keddy,lag %in% vec_lag_keddy) %>% arrange(desc(cor))
filter(ccf_data_temp,lag %in% vec_lag_temp) %>% arrange(desc(cor))


### plot séries brutes + tendance ###
plot(scale(df_2ban$keddy),type="l",col="red",ylim=c(-3,6))
lines(scale(df_2ban$conc_od),size=1)
# oxy
ts.plot(diff(df_2ban$conc_od)) 
mod_trend_ox <- lm(conc_od ~ date, data = df_2ban)
summary(mod_trend_ox) # tendance présente, variance moyennement stable= log ? sqrt ? ARCH ?
df_2ban$conc_od <- lm(conc_od ~ date, data = df_2ban)$res # on enlève la tendance

summary(mod_trend_ox)
#keddy
ts.plot(df_2ban$keddy) #baisse dde la variance importante entre 300 et 600 environs
mod_trend_keddy <- lm(keddy ~ date, data = df_2ban)
summary(mod_trend_keddy) # tendance ; variance ca va ; saisonnalité 24 heures (vérifier sur ondelette)
# temp
ts.plot(df_2ban$temp) #baisse dde la variance importante entre 300 et 600 environs
mod_trend_temp <- lm(temp ~ date, data = df_2ban)
summary(mod_trend_temp)







### CCF normal et pré blanchissage des données pour CCF
lag2.plot(df_2ban$temp,df_2ban$conc_od,max.lag = 10)
lag2.plot(df_2ban$temp,df_2ban$keddy,max.lag = 10)
lag2.plot(log(df_2ban$keddy+1),df_2ban$conc_od,max.lag = 10)

ccf(log(df_2ban$keddy+1),df_2ban$conc_od,lag.max = 40)
ccf(df_2ban$temp,df_2ban$conc_od,lag.max = 100)

res_keddy <-residuals(keddy_diff$fit)

### régression laggée ###
ccf_data_plot_keddy <- ccf(log(df_2ban$keddy+0.01), df_2ban$conc_od,lag.max = 30)
ccf_data_plot_temp <- ccf(df_2ban$temp, df_2ban$conc_od,lag.max = 30)

ccf_data_keddy <- bind_cols(data.frame(cor = ccf_data_plot_keddy[[1]]),data.frame(lag = ccf_data_plot_keddy$lag))
ccf_data_temp <- bind_cols(data.frame(cor = ccf_data_plot_temp[[1]]),data.frame(lag = ccf_data_plot_temp$lag))

vec_lag_keddy <- fun_select_lag(ccf_data_keddy, n.lag = 6) %>% pull(lag)
vec_lag_temp <- fun_select_lag(ccf_data_temp, n.lag = 6) %>% pull(lag)

filter(ccf_data_keddy,lag %in% vec_lag_keddy) %>% arrange(desc(cor))
filter(ccf_data_temp,lag %in% vec_lag_temp) %>% arrange(desc(cor))

df_lag_date <- fun_df_lag(df_2ban,lagged = "keddy",vec_lag = vec_lag_keddy,var = 1:2) %>% mutate(date=df_2ban$date) %>% na.omit()
df_lag <- fun_df_lag(df_2ban,lagged = "keddy",vec_lag = vec_lag_keddy,var = 1:2) %>% na.omit()
df_lag_temp <- fun_df_lag(df_2ban,lagged = "temp",vec_lag = vec_lag_temp, var = 1:2) %>% 
  bind_cols(fun_df_lag(df_2ban,lagged = "keddy",vec_lag = vec_lag_keddy,var = 0)) %>%  na.omit()

df_lag_log <- mutate_at(df_lag,vars(matches("keddy")),~log(.+0.001))
df_lag_log_temp <- mutate_at(df_lag_temp,vars(matches("keddy")),~log(.+0.001))

indice <- nrow(df_lag_log) - 700:nrow(df_lag_log)
df_train_base <- df_lag_log[indice,]
df_test_base <- df_lag_log[-indice,]
df_date <- df_lag_date[-indice,] %>% pull(date)

WaveletComp::analyze.wavelet(df_lag_log,1,dt=1/24) %>% wt.image()
  
acf2(diff(df_train_base$conc_od),max.lag = 100)
auto.arima(df_train_base$conc_od)
sarima_base <-
  sarima(
    df_train_base$conc_od,
    p = 1,
    d = 0,
    q = 1,
    P = 0,
    D = 0,
    Q = 0,
    S = 0
  )

sarima_base
acf2(residuals(sarima_base$fit))

sarima_base_pred <-
  sarima.for(
    df_train_base$conc_od,
    n.ahead = 700,
    p = 1,
    d = 0,
    q = 1,
    P = 0,
    D = 0,
    Q = 0,
    S = 0
  )
rmse_base <- RMSE(sarima_base_pred$pred %>% as.vector() , df_test$conc_od)
mae_base <- MAE(sarima_base_pred$pred %>% as.vector() , df_test$conc_od)
sse_base <- sum((df_test_base$conc_od - sarima_base_pred$pred %>% as.vector())^2)


############################## sans température ###############################


auto.arima( df_train_base$conc_od,xreg = df_train_base[, -c(1:3)] %>% as.matrix())
sarimax_conc_od <-
  sarima(
    df_train_base$conc_od,
    p = 1,
    d = 0,
    q = 1,
    P = 0,
    D = 0,
    Q = 0,
    S = 0,
    xreg = df_train_base[, -c(1:3)]
  )
acf2(resid(sarimax_conc_od$fit))
sarimax_conc_od

sarimax_pred_2bev <- sarima.for(
  df_train_base$conc_od,
  p = 1,
  d = 0,
  q = 1,
  P = 0,
  D = 0,
  Q = 0,
  S = 0,
  plot.all = FALSE,
  n.ahead = 700,
  newxreg =df_test_base[, -c(1:3)],
  xreg = df_train_base[, -c(1:3)]
)

rmse_st <- RMSE(sarimax_pred_2bev$pred %>% as.vector(),df_test_base$conc_od);rmse_st
mae_st <- MAE(sarimax_pred_2bev$pred %>% as.vector(),df_test_base$conc_od);mae_st
sse_st <- sum((df_test_base$conc_od -sarimax_pred_2bev_temp$pred %>% as.vector() )^2);sse_st
sse(df_test_base$conc_od,sarimax_pred_2bev_temp$pred %>% as.vector())


plot(sarimax_pred_2bev$pred %>% as.vector(),col='red',type ="l",ylim=c(4,10))
lines(df_test_base$conc_od)
lines(sarima_base_pred$pred %>% as.vector(),col="blue")

model_abnu <- lm(df_test_base$conc_od~sarima_pred_2bev$pred)
model_abnu <- lm(df_test_base$conc_od~sarima_base_pred$pred)

summary(model_abnu)
acf2(model_abnu$residuals)

############################## avec température ###############################
indice <- nrow(df_lag_log_temp) - 700:nrow(df_lag_log_temp)
df_train_temp <- df_lag_log_temp[indice,] %>% dplyr::select(-conc_od1)
df_test_temp <- df_lag_log_temp[-indice,] %>% dplyr::select(-conc_od1)

sarimax_conc_od_temp <-
  sarima(
    df_train_temp$conc_od,
    p = 0,
    d = 1,
    q = 0,
    P = 1,
    D = 1,
    Q = 0,
    S = 24,
    xreg = df_train_temp[, -c(1:3)]
  )

sarimax_conc_od_temp 


auto.arima(df_train_temp$conc_od, xreg = df_train_temp[, -c(1:3)] %>% as.matrix)
sarimax_pred_2bev_temp <- sarima.for(
  df_train_temp$conc_od,
  p = 2,
  d = 1,
  q = 1,
  P = 1,
  D = 1,
  Q = 1,
  S = 24,
  plot.all = FALSE,
  n.ahead = 700,
  newxreg = df_test_temp[, -c(1:3)],
  xreg = df_train_temp[, -c(1:3)]
)

sse_temp <- var(df_test_temp$conc_od -sarimax_pred_2bev_temp$pred %>% as.vector() )
rmse_temp <- RMSE(sarimax_pred_2bev_temp$pred %>% as.vector(),df_test$conc_od)
mae_temp <- MAE(sarimax_pred_2bev_temp$pred %>% as.vector(),df_test_temp$conc_od)

plot(sarimax_pred_2bev$pred %>% as.vector(),col='red',type ="l",ylim=c(4,10))
lines(sarimax_pred_2bev_temp$pred %>% as.vector(),col='green',type ="l",ylim=c(4,10))
lines(df_test_temp$conc_od)
lines(sarima_base_pred$pred %>% as.vector(),col="blue")

data_lacanauG_abve_20_sarima_pred_1 <- bind_cols(
  ox_sarima = sarima_base_pred$pred,
  ox_sarimax_temp = sarimax_pred_2bev_temp$pred %>% as.vector(),
  ox_sarimax_sans_temp = sarimax_pred_2bev$pred %>% as.vector(),
  conc_od = df_test_temp$conc_od,
  date = df_date
) %>% gather(key = "pred",value="value",-date)

ggplot(data_lacanauG_abve_20_sarima_pred_1,aes(x=date,y=value,color = pred))+geom_line()


################################# modele linéaire ####################################"
df_lag_log_temp <- df_lag_log_temp %>% dplyr::select(-conc_od1)
model_base <- lm(conc_od~.,df_lag_log[,c(1:3)])
acf2(model_base$res)
summary(model_base)
plot(df_lag_log$conc_od~df_lag_log$keddy4)
model_keddy <- lm(conc_od~.,df_lag_log[,-c(2:3)])
acf2(model_keddy$res)
summary(model_keddy)
model_temp <- lm(conc_od~.,df_lag_log_temp)
summary(model_temp)
acf2(model_temp$res)
