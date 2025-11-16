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

## Mathematical Model

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

