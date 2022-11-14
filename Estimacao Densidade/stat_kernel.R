
stat_kernel <- function(X, h, x, kernel = c('gaussian','epanechnikov','rectangular','triangular','biweight','cosine')){
  kernel <- match.arg(kernel)
  n <- length(x)
  t <- (x-X)/h
  w <- switch(
    kernel
    , gaussian = (2*pi)^(-0.5)*exp(-0.5*t^2)
    , epanechnikov = ifelse(abs(t)<1,(3/4)*(1-t^2),0)
    , rectangular = ifelse(abs(t)<1,1/2,0)
    , triangular = ifelse(abs(t)<1,1 - abs(t),0)
    , biweight = ifelse(abs(t)<1,15*(1-t^2)^2/16,0)
    , cosine = ifelse(abs(t)<1,(pi/4)*cos(.5*pi*t),0)
    , stop("unknown 'kernel'")
  )
  f=(1/n)*(1/h)*sum(w)
  return(f)
}

X=c(-0.77, -0.60, -0.25, 0.14, 0.45, 0.64, 0.65, 1.19, 1.71, 1.74)
x_aux <- seq(-3,3,length.out = 100)
x <- numeric(100)

plt_hist <- function(x,x_aux){
  p <- tibble(
    x = x
    , x_aux = x_aux
  ) %>% 
    ggplot(aes(x = x_aux, y = x))+
    geom_line()+
    theme_light()+
    theme(
      panel.grid.major = element_blank()
      , panel.grid.minor = element_blank()
    )
  return(p)
}

# h = 0.25 ####

## gaussian

for( i in 1:100){
  x[i] <- stat_kernel(X=X, h=0.25, x = x_aux[i], kernel = 'gaussian')
}

plt <- tibble(
  x_aux = x_aux
  , x_gaus = x
)

## epanechnikov

for( i in 1:100){
  x[i] <- stat_kernel(X=X, h=0.25, x = x_aux[i], kernel = 'epanechnikov')
}

plt <- bind_cols(plt,x_epa = x)

## rectangular

for( i in 1:100){
  x[i] <- stat_kernel(X=X, h=0.25, x = x_aux[i], kernel = 'rectangular')
}

plt <- bind_cols(plt,x_ret = x)

## triangular

for( i in 1:100){
  x[i] <- stat_kernel(X=X, h=0.25, x = x_aux[i], kernel = 'triangular')
}

plt <- bind_cols(plt,x_tri = x)


## biweight

for( i in 1:100){
  x[i] <- stat_kernel(X=X, h=0.25, x = x_aux[i], kernel = 'biweight')
}

plt <- bind_cols(plt,x_biw = x)


## cosine

for( i in 1:100){
  x[i] <- stat_kernel(X=X, h=0.25, x = x_aux[i], kernel = 'cosine')
}

plt <- bind_cols(plt,x_cos = x)

plt %>% 
  mutate(
    x_aux = round(x_aux, 2) %>% factor()
  ) %>% 
  reshape2::melt() %>% 
  ggplot(aes(x = as.numeric(x_aux), y = value, color = variable))+
  geom_line()+
  xlab('')+
  ylab('')+
  scale_color_discrete(
    name = "Kernel"
    , labels = c('gaussian','epanechnikov','rectangular','triangular','biweight','cosine')
  )+
  theme_light()+
  theme(
    panel.grid.major = element_blank()
    , panel.grid.minor = element_blank()
    #, legend.position="bottom"
  )

