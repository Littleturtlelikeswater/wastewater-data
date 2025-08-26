# 安装并加载 rstan
# install.packages("rstan")
library(rstan)

# 1. 模拟一份数据 -----------------------
set.seed(123)

N <- 20   # 20周数据
wastewater <- rnorm(N, mean = 50, sd = 10)      # wastewater 浓度
true_alpha <- 5
true_beta <- 0.8
true_sigma <- 5

cases <- numeric(N)
cases[1] <- 30  # 第一周初始化
for (t in 2:N) {
  mu <- true_alpha + true_beta * wastewater[t-1]
  cases[t] <- rnorm(1, mu, true_sigma)
}

# 2. Stan 模型代码 -----------------------
stan_code <- "
data {
  int<lower=0> N;               // 周数
  vector[N] wastewater;         // wastewater 浓度
  vector[N] cases;              // 每周病例数
}
parameters {
  real alpha;                   // 截距
  real beta;                    // wastewater 对病例的影响
  real<lower=0> sigma;          // 残差标准差
}
transformed parameters {
  vector[N-1] mu;
  for (t in 1:(N-1)) {
    mu[t] = alpha + beta * wastewater[t];
  }
}
model {
  // 先验
  alpha ~ normal(0, 10);
  beta  ~ normal(0, 5);
  sigma ~ exponential(1);

  // 似然：病例数由前一周 wastewater 预测
  for (t in 1:(N-1)) {
    cases[t+1] ~ normal(mu[t], sigma);
  }
}
generated quantities {
  real next_week_pred;
  next_week_pred = normal_rng(alpha + beta * wastewater[N], sigma);
}
"

# 3. 编译模型 -----------------------
stan_model <- stan_model(model_code = stan_code)

# 4. 准备数据 -----------------------
stan_data <- list(
  N = N,
  wastewater = wastewater,
  cases = cases
)

# 5. 采样 -----------------------
fit <- sampling(stan_model, data = stan_data, 
                iter = 2000, chains = 4, seed = 123)

# 6. 查看结果 -----------------------
print(fit, pars = c("alpha", "beta", "sigma", "next_week_pred"))

# 7. 可视化结果 -----------------------
traceplot(fit, pars = c("alpha", "beta", "sigma"))
