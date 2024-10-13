library(ggplot2)
library(dplyr)

# ------------------------------
# 1. Classical Probability
# ------------------------------

# Example: Probability of a vaccine being effective based on equally likely outcomes

# Suppose a vaccine can either be 'Effective' or 'Not Effective'
# Assuming no prior information, both outcomes are equally likely

sample_space <- c("Effective", "Not Effective")
prob_classical <- rep(1/length(sample_space), length(sample_space))
names(prob_classical) <- sample_space

# Display Classical Probability
print("Classical Probability of Vaccine Effectiveness:")
print(prob_classical)

# ------------------------------
# 2. Empirical Probability
# ------------------------------

# Example: Empirical probability based on vaccine trial data

# Simulate trial data: 1000 patients received the vaccine
set.seed(123) # For reproducibility

# Assume in reality, 85% of vaccines are effective based on historical data
trial_size <- 1000
effective <- rbinom(trial_size, 1, 0.85) # 1 = Effective, 0 = Not Effective

# Calculate empirical probability
empirical_prob <- mean(effective)
print(paste("Empirical Probability of Vaccine Effectiveness:", round(empirical_prob, 4)))

# Visualize the results
df_trial <- data.frame(Effectiveness = factor(effective, levels = c(0,1), labels = c("Not Effective", "Effective")))

ggplot(df_trial, aes(x = Effectiveness)) +
  geom_bar(fill = "steelblue") +
  ggtitle("Empirical Probability of Vaccine Effectiveness") +
  ylab("Number of Patients") +
  xlab("Vaccine Outcome")

# ------------------------------
# 3. Subjective Probability
# ------------------------------

# Example: Subjective probability based on expert opinion

# Suppose experts estimate there's a 70% chance that the vaccine will receive FDA approval
subjective_prob <- 0.70
print(paste("Subjective Probability of FDA Approval:", subjective_prob))

# ------------------------------
# 4. Defining Events and Sample Space
# ------------------------------

# Example: Define events related to vaccine trials

# Sample Space: Possible outcomes for a patient in the vaccine trial
# Outcomes: No Side Effect, Mild Side Effect, Severe Side Effect

sample_space_events <- c("No Side Effect", "Mild Side Effect", "Severe Side Effect")
prob_no_side <- 0.75  # 75% have no side effects
prob_mild_side <- 0.20 # 20% have mild side effects
prob_severe_side <- 0.05 # 5% have severe side effects

prob_events <- c(prob_no_side, prob_mild_side, prob_severe_side)
names(prob_events) <- sample_space_events

print("Probability Distribution for Side Effects:")
print(prob_events)

# Define events
A <- "No Side Effect"
B <- "Mild Side Effect"
C <- "Severe Side Effect"

# ------------------------------
# 5. Complement of an Event
# ------------------------------

# Example: Probability of experiencing any side effect

# P(A') = 1 - P(A)
prob_any_side_effect <- 1 - prob_no_side
print(paste("Probability of Experiencing Any Side Effect:", prob_any_side_effect))

# ------------------------------
# 6. Joint and Conditional Probability
# ------------------------------

# Example: Joint and Conditional probabilities in vaccine trials

# Suppose among those who have mild side effects, 40% also experience mild fever
# Define two events:
# D: Mild Side Effect
# E: Mild Fever

# Joint Probability P(D ∩ E)
prob_D_and_E <- prob_mild_side * 0.40
print(paste("Joint Probability P(Mild Side Effect and Mild Fever):", prob_D_and_E))

# Conditional Probability P(E|D) = P(D ∩ E) / P(D)
prob_E_given_D <- prob_D_and_E / prob_mild_side
print(paste("Conditional Probability P(Mild Fever | Mild Side Effect):", prob_E_given_D))

# ------------------------------
# 7. Addition and Multiplication Rules
# ------------------------------

# Example: Applying Addition and Multiplication Rules

# Addition Rule: P(A or B) = P(A) + P(B) - P(A and B)
# Let's find the probability that a patient has No Side Effect or Severe Side Effect

# P(A or C) = P(A) + P(C) - P(A and C)
# Since No Side Effect and Severe Side Effect are mutually exclusive, P(A and C) = 0

prob_A_or_C <- prob_no_side + prob_severe_side - 0
print(paste("P(No Side Effect or Severe Side Effect):", prob_A_or_C))

# Multiplication Rule: For independent events, P(A and B) = P(A) * P(B)
# Suppose the occurrence of Mild Side Effect and Vaccine Effectiveness are independent

# Let P(Vaccine Effective) = 0.85 (from empirical probability)
# P(Mild Side Effect) = 0.20

prob_effective_and_mild <- empirical_prob * prob_mild_side
print(paste("P(Vaccine Effective and Mild Side Effect):", prob_effective_and_mild))

# ------------------------------
# 8. Visualizing Conditional Probability
# ------------------------------

# Example: Visualizing P(E|D) where E is Mild Fever and D is Mild Side Effect

# Simulate additional data for fever among those with mild side effects
# Out of 200 mild side effects, 80 have mild fever

n_mild <- 200
n_fever <- 80
prob_E_given_D_sim <- n_fever / n_mild
print(paste("Simulated Conditional Probability P(Mild Fever | Mild Side Effect):", prob_E_given_D_sim))

# Visualize conditional probability
df_conditional <- data.frame(
  Side_Effect = c("Mild Side Effect with Fever", "Mild Side Effect without Fever"),
  Count = c(n_fever, n_mild - n_fever)
)

ggplot(df_conditional, aes(x = Side_Effect, y = Count, fill = Side_Effect)) +
  geom_bar(stat = "identity") +
  ggtitle("Conditional Probability: Mild Fever Given Mild Side Effect") +
  ylab("Number of Patients") +
  xlab("Condition") +
  theme(legend.position = "none")
