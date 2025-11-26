# Herpes Virus Infection Probability Modeling

## Project Overview

This project investigates the probability of herpes virus infection in cell cultures under controlled laboratory conditions. The goal is to model the likelihood of infection as a function of virus dilution.

In the experiments, cells were exposed to viral dilutions ranging from $10^{-1}$, $10^{-2}$, ..., $10^{-7}$ . After an incubation period, cytopathic effects (CPE) were assessed, which indicate successful infection. Each dilution was tested in **4 replicates per experiment** to account for variability.

---

## Experimental Design

- **Virus dilutions:** $10^{-1}$, $10^{-2}$, ..., $10^{-7}$  
- **Replicates per dilution:** 4  (currently)
- **Outcome:** Presence or absence of cytopathic effect (binary)  

---

## Mathematical Model 1

We model the binary infection outcomes as following a Bernoulli distribution with parameter $p_i$:

$$y_i \sim \text{Bernoulli}(p_i)$$

where $p_i$ represents the probability of infection for observation $i$.

The infection probability $p_i$ is modeled using a logistic regression framework:

$$\text{logit}(p_i) = \alpha + \beta \cdot x_i$$

where:
- $x_i$ is the virus dilution (log₁₀ scale) for observation $i$
- $\alpha$ is the intercept (log-odds of infection when $x_i = 0$)
- $\beta$ is the fixed effect of virus dilution on log-odds of infection

In the Bayesian framework, we specify prior distributions for the parameters:

$$\alpha \sim \text{Normal}(\mu_\alpha, \sigma_\alpha)$$
$$\beta \sim \text{Normal}(\mu_\beta, \sigma_\beta)$$

## Hierarchical Model for the Different Experiments

This section describes the hierarchical structure used to model the variation between different experiments in the project.

Each experiment $j$ has its own intercept $\alpha_j$ and slope $\beta_j$. These experiment-specific parameters are modeled hierarchically using a multivariate normal (MVN) distribution:

$$\begin{pmatrix}
\alpha_j \\
\beta_j
\end{pmatrix}
\sim
\text{MVN}\\left(
\begin{pmatrix}
\alpha \\
\beta
\end{pmatrix},
\\Sigma
\right)$$

where:

$\alpha$ is the population-level (overall) intercept

$\beta$ is the population-level (overall) slope

$\Sigma$ is the $2 \times 2$ covariance matrix describing between-experiment variation

Covariance Matrix ($\Sigma$)

The covariance matrix $\Sigma$ is parameterized as:

$$\Sigma =
\begin{pmatrix}
\sigma_\alpha^2 &
\rho \, \sigma_\alpha \sigma_\beta \\
\rho \, \sigma_\alpha \sigma_\beta &
\sigma_\beta^2
\end{pmatrix}$$

where:

$\sigma_\alpha$ is the standard deviation of experiment-specific intercepts

$\sigma_\beta$ is the standard deviation of experiment-specific slopes

$\rho$ is the correlation between intercepts and slopes across experiments

### Hyperparameter Priors

Population Mean Parameters:

$$\alpha \sim \text{Normal}(\mu_\alpha, \sigma_\alpha)$$

$$\beta \sim \text{Normal}(\mu_\beta, \sigma_\beta)$$

Population Standard Deviation Parameters (Scale):

$$\sigma_\alpha \sim \text{Half-Normal}(0, \tau_\alpha)$$

$$\sigma_\beta \sim \text{Half-Normal}(0, \tau_\beta)$$

Correlation Parameter:

$$\rho \sim \text{LKJCorr}(2)$$



