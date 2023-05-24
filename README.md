# Coins and Dice

The Coins 'n' Dice (coinsndice) R package simulates flipping coins and rolling dice in repeated sets, and
conducts statistical inference (proportions tests or t tests, and/or their associated confidence intervals)
on each set. This allows one to get a sense for p-values and endpoints of confidence intervals as random
variables.

To simulate coin flipping and die rolling, the `flip_coin` and `roll_die` functions can be used. However,
the main functions in the package are the `flip` and `roll` functions, which display (numerically and/or
graphically) the results of applying statistical inference repeatedly to simulated coin flips or die rolls.
In these functions, `n` is the number of flips or rolls per set, and `N` is the number of sets to be
simulated. Statistical inference (specified by other arguments to the functions) is conducted on each
set of flips and rolls.

More information can be found in the man pages for these functions, and more information will soon be
posted here as well.


