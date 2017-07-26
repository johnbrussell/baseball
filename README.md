Repository for a baseball game simulator.

It looks like I hastily created this with the intention of reverse engineering
what exactly "replacement level" is in baseball.

Notable contents and possible areas for improvement of this repository are:
-- Baseball game simulator: see function `simulate_game()` in `Baseball.R`.
-- Definition of SWOBP, or slugging-weighted on base percentage, the
    average number of bases a player advances each at bat.
-- Calculation of some nice summary statistics about SWOBP for the MLB's
    2004 season.
-- Area of improvement: add a 9-man lineup to the game simulator,
    rather than just a team SWOBP.
-- Area of improvement: try calculating SWOBP for players who play partial
    seasons (ie., potentially borderline players, or the players who would
    be the supposed "replacements") and compare players who
    play most of a season against these marginal players to determine
    replacement level baseball.  Might have to refer to more seasons than
    just '04 to determine if a player's partial season is a fluke due to
    injury rather than their entry/exit from the league.
-- Area of improvement: algorithmic thinking, commenting, documentation,
    the usual components of good code.
