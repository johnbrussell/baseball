closeAllConnections()
rm(list = ls(all = TRUE))

library(stringr)
source("C://users/John/Google Drive/R libraries/Libraries.R")
source("C://users/John/Google Drive/Mind Games/Baseball/R files/baseball.R")

batting = as.data.frame(read.csv("C://users/John/Google Drive/Mind Games/Baseball/batting.csv", 
                                 stringsAsFactors = FALSE))
batting = batting[batting$yearID == 2004,]

pitching = as.data.frame(read.csv("C://users/John/Google Drive/Mind Games/Baseball/pitching.csv", 
                                  stringsAsFactors = FALSE))
pitching = pitching[pitching$yearID >= 2015,]

# DEFINE swobp: approximately a weighted average of the number of bases advanced per at bat
# (SLUGGING-WEIGHTED ON BASE PERCENTAGE)
batting$swobp = (batting$HBP + batting$BB + batting$IBB + batting$H +  # Measurable ways to get to 1st
                     batting$X2B +    # Add one extra base for a double (doubles already included in hits)
                     2*batting$X3B +  # Add two extra bases for triples
                     3*batting$HR) /  # Add three extra bases for home runs
    (batting$AB + batting$BB + batting$HBP + batting$IBB)

swobp_list = make_swobp_list() # List of swobp scores, one for each plate appearance.

swobp_average = mean(swobp_list) # Weighted average of player swobp by number of plate appearances.
swobp_sd = sd(swobp_list)  # Standard deviation of the list. 

best_z = get_best_z()  # Get z score for swobp of "median" player
best_swobp = swobp_average + best_z * swobp_sd  # Calculate that player's swobp.

replacement_z = qnorm(1/(1-pnorm(best_z))/162)
replacement_swobp = replacement_z * swobp_sd + swobp_average

replacement_swobp = find_replacement_swobp()

batting_adj = batting

batting_adj$AB_adj = batting_adj$AB
batting_adj$AB_adj = max(batting$AB)
batting_adj$H_adj = batting_adj$H
batting_adj$H_adj = (max(batting_adj$AB) - batting_adj$AB) * replacement_swobp + batting_adj$H_adj
batting_adj$predicted_swobp = (batting_adj$HBP + batting_adj$BB + batting_adj$IBB + batting_adj$H_adj + 
                                   batting_adj$X2B + 2*batting_adj$X3B + 3*batting_adj$HR) / 
    (batting_adj$AB_adj + batting_adj$BB + batting_adj$HBP + batting_adj$IBB)

winrepprob = win_probability(replacement_swobp, best_swobp)
games_for_one = 1 / winrepprob
