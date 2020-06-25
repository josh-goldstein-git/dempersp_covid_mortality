## don't think this file is necessary for repo

## gompertz simu with steeper epidemic mortality


alpha = 10^-4
beta = 1/10
x = 0:110
mx0 = alpha*exp(beta*x)
mx = mx0
Hx = cumsum(mx)
lx = exp(-Hx)
e0 = sum(lx)
beta_epi = 1/10 + 1/50 ## .12
## beta_epi = 1/10
z = seq(0, .99, .01)
e0.vec = NULL
for(i in 1:length(z))
{
    mx_epi = z[i] * alpha * exp(beta_epi  * x)
    ##     mx = mx0 + mx_epi
    mx = mx0 *exp(-.01 * i)
    Hx = cumsum(mx)
    lx = exp(-Hx)
    e0 = sum(lx)
    e0.vec[i] = e0
}
plot(z, e0.vec, type = 'o')
abline(lm(e0.vec ~ z), lty = 2)

