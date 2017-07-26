# This function analyzes the (global) data frame called batting.
# It calculates, for each row of the data frame, approximately the number of plate appearances 
#  (approximately because sacrifices and fielder's choices and the like are excluded).  
# For each such plate appearance, it appends the swobp of the player to a list.  
# Thus, it returns a list of player swobp scores, with one element corresponding to one plate appearance.
make_swobp_list <- function()
{
    swobp_list_local = NULL
    for(i in 1:nrow(batting))
    {
        mini_list = NULL
        total_valid = batting[i, which(colnames(batting) == "AB")] + 
            batting[i, which(colnames(batting) == "BB")] + batting[i, which(colnames(batting) == "HBP")] + 
            batting[i, which(colnames(batting) == "IBB")]
        if(total_valid > 0)
        {
            player_swobp = batting[i, which(colnames(batting) == "swobp")]
            mini_list = rep(player_swobp, times=total_valid)
            # for(j in 1:total_valid)
            #     mini_list = c(mini_list, batting[i, which(colnames(batting) == "swobp")])
            swobp_list_local = c(swobp_list_local, mini_list)
        }
    }
    return(swobp_list_local)
}


# When ordering the dataset from players with the best swobp to the worst, and counting the number of 
#  outs each player made, there will be a "median" player, such that all players whose swobp was
#  higher made approximately the same total number of outs as all players whose swobp was lower than 
#  that of the median player.  This function returns the z score of that median player's swobp.
get_best_z <- function()
{
    batting_local = batting[order(batting$swobp, decreasing=TRUE),]
    
    counter = 0
    player = 0
    while(counter < 4374) # 4374 == 162 * 27, so this is "while the players who made half of the outs 
    {                     # in the season haven't yet been seen."
        player = player + 1
        counter = counter + batting_local[player, which(colnames(batting_local) == "AB")] - 
            batting_local[player, which(colnames(batting_local) == "H")]
    }
    good_swobp = batting_local[player, which(colnames(batting_local) == "swobp")]
    return((good_swobp - swobp_average) / swobp_sd)
}



simulate_game <- function(swobp1, swobp2)
{
    p1 = swobp1 #pnorm((swobp1 - swobp_average) / swobp_sd)
    p2 = swobp2 #pnorm((swobp2 - swobp_average) / swobp_sd)
    runs1 = 0
    runs2 = 0
    for(i in 1:9)
    {
        outs1 = 0
        br1 = 0
        outs2 = 0
        br2 = 0
        while(outs1 < 3)
        {
            outcome = runif(1,0,1)
            if(outcome > p1)
                outs1 = outs1 + 1
            else if(br1 < 3)
                br1 = br1 + 1
            else runs1 = runs1 + 1
        }
        while(outs2 < 3)
        {
            outcome = runif(1,0,1)
            if(outcome > p2)
                outs2 = outs2 + 1
            else if(br2 < 3)
                br2 = br2 + 1
            else runs2 = runs2 + 1
        }
    }
    
    if(runs1 > runs2)
        return(swobp1)
    if(runs2 > runs1)
        return(swobp2)
    inning = 9
    while(runs1 == runs2)
    {
        #inning = inning + 1
        outs1 = 0
        br1 = 0
        outs2 = 0
        br2 = 0
        while(outs1 < 3)
        {
            outcome = runif(1,0,1)
            if(outcome > p1)
                outs1 = outs1 + 1
            else if(br1 < 3)
                br1 = br1 + 1
            else runs1 = runs1 + 1
        }
        while(outs2 < 3)
        {
            outcome = runif(1,0,1)
            if(outcome > p2)
                outs2 = outs2 + 1
            else if(br2 < 3)
                br2 = br2 + 1
            else runs2 = runs2 + 1
        }
        if(runs1 > runs2)
            return(swobp1)
        if(runs2 > runs1)
            return(swobp2)
    }
}

win_probability <- function(swobp1, swobp2)
{
    wins1 = 0
    wins2 = 0
    for(i in 1:8100)
    {
        winner_swobp = simulate_game(swobp1, swobp2)
        if(swobp1 == winner_swobp)
            wins1 = wins1 + 1
        else wins2 = wins2 + 1
    }
    win1prob = wins1/(wins1+wins2)
    return(win1prob)
}


find_replacement_swobp <- function()
{
    test_swobp = swobp_average 
    games = 1/(win_probability(test_swobp, best_swobp))
    gamesc = games
    while(games < 162 || gamesc < 162)
    {
        print(c(test_swobp, games))
        if(games >= 162 || gamesc >= 162)
            gamesc = games
        test_swobp = test_swobp - 10 / (games * games)
        games = 1/(win_probability(test_swobp, best_swobp))
    }
    return(test_swobp)
}