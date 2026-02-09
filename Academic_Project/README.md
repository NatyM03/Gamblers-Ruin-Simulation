# Stochastic Analysis of the "Gambler's Ruin" Problem

This folder contains the original materials developed for the university project focused on stochastic processes and Random Walk models.

### Contents:
* **Gambler_s_Ruin.pdf**: theoretical research paper investigating an "unfair game" scenario ($p=0.48$) with absorbing barriers.
* **Project.R**: initial R script employing Monte Carlo methods to analyze capital trajectories and game duration.

### Key Academic Findings:

* **Probability validation**: successfully validated the theoretical ruin probability of **98.21%** against an empirical result of **98.8%**.
  
![Trajectories](Academic_Project/trajectory_plot.jpeg)
* *Figure 1: Evolution of capital for 5 distinct gamblers showing the "gravitational pull" of the 0.48 win probability.*

* **Phase transition**: identified a critical phase transition at **p=0.5**, demonstrating how minor shifts in probability radically alter outcomes from certain failure to probable success.

![Sensitivity Analysis](Academic_Project/sensitivity_plot.jpeg)
* *Figure 2: Sensitivity analysis showing the "cliff" effect when the win probability drops below 0.5.*

* **Strategy optimization**: confirmed that in sub-fair environments ($p < 0.5$), aggressive betting (higher stakes) minimizes the risk of ruin by reducing game duration and exposure to the house edge.
