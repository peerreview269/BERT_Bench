# install.packages(c("network", "ergm", "dplyr"))

library(network)
library(ergm)
library(dplyr)

set.seed(123)

# ------------------------------------------------------------
# 1. Create fake node-level data
# ------------------------------------------------------------

n <- 80

nodes <- data.frame(
  id = 1:n,
  gender = sample(c("F", "M"), n, replace = TRUE),
  org = sample(paste0("Org", 1:6), n, replace = TRUE),
  x = rnorm(n),
  y = rnorm(n)
)

# Numeric gender indicator for the logistic regression
nodes$female <- ifelse(nodes$gender == "F", 1, 0)

# Dyadic distance matrix
dist_mat <- as.matrix(dist(nodes[, c("x", "y")]))

# Standardize distance so coefficient sizes are easier to read
dist_mat <- dist_mat / sd(dist_mat[upper.tri(dist_mat)])


# ------------------------------------------------------------
# 2. Create all possible undirected dyads
# ------------------------------------------------------------

dyads <- expand.grid(i = 1:n, j = 1:n) %>%
  filter(i < j)

dyads <- dyads %>%
  mutate(
    female_i = nodes$female[i],
    female_j = nodes$female[j],
    female_sum = female_i + female_j,
    same_org = ifelse(nodes$org[i] == nodes$org[j], 1, 0),
    distance = dist_mat[cbind(i, j)]
  )


# ------------------------------------------------------------
# 3. Simulate ties from a logistic model
# ------------------------------------------------------------

# True data-generating model:
# - baseline tie probability is low
# - more female endpoints slightly increases tie probability
# - same organization increases tie probability
# - greater distance decreases tie probability

dyads <- dyads %>%
  mutate(
    logit_p = -3.0 +
      0.45 * female_sum +
      1.25 * same_org -
      1.00 * distance,
    p = plogis(logit_p),
    tie = rbinom(n(), size = 1, prob = p)
  )


# ------------------------------------------------------------
# 4. Fit regular logistic regression
# ------------------------------------------------------------

glm_fit <- glm(
  tie ~ female_sum + same_org + distance,
  data = dyads,
  family = binomial()
)

summary(glm_fit)


# ------------------------------------------------------------
# 5. Convert the same fake data into a network object
# ------------------------------------------------------------

adj <- matrix(0, n, n)

adj[cbind(dyads$i, dyads$j)] <- dyads$tie
adj[cbind(dyads$j, dyads$i)] <- dyads$tie

net <- network(adj, directed = FALSE, matrix.type = "adjacency")

set.vertex.attribute(net, "gender", nodes$gender)
set.vertex.attribute(net, "female", nodes$female)
set.vertex.attribute(net, "org", nodes$org)


# ------------------------------------------------------------
# 6. Fit the equivalent dyad-independent ERGM
# ------------------------------------------------------------

ergm_fit <- ergm(
  net ~ edges +
    nodecov("female") +
    nodematch("org") +
    edgecov(dist_mat),
  estimate = "MPLE"
)

summary(ergm_fit)


# ------------------------------------------------------------
# 7. Compare coefficients side by side
# ------------------------------------------------------------

coef(glm_fit)
coef(ergm_fit)