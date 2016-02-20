# Overview #

This project contains [Haskell](http://www.haskell.org/) implementations of the two algorithms described in the paper ["Simple Search Methods for Find a Nash Equilibrium"](http://www.ryanwporter.com/papers.html), by Ryan Porter, Eugene Nudelman, and Yoav Shoham.

Note that these are NOT the implementations that produced the experimental results described in that paper.  Those, sadly, have been lost to the sands of time.  These implementations are similar, but slower, probably because the author is (as of this writing) less skilled at Haskell than at C++.

# Implementation #

This project is setup as a Haskell [Cabal](http://www.haskell.org/cabal/) package.

It makes use of several libraries that I found to extremely useful:
  * [Parsec](http://legacy.cs.uu.nl/daan/parsec.html) for parsing
  * [QuickCheck](http://hackage.haskell.org/package/QuickCheck-2.1.0.3) for testing
  * [levmar](http://hackage.haskell.org/package/levmar), which provides a Haskell interface to a C implementation of the [Levenberg-Marquardt](http://www.ics.forth.gr/~lourakis/levmar/) optimization algorithm
  * [hmatrix-glpk](http://hackage.haskell.org/package/hmatrix-glpk), which provides a Haskell interface to linear programming functions provided by [GLPK](http://www.gnu.org/software/glpk/).

# Status #

2010-05-15: The two-player solver seems robust, and the n-player solver works on most games.  However, it sometimes fails to find an equilibrium.  The most common symptom is a "SmallGradient" result from levmar library, but I do not know the underlying cause.  Two examples of problematic games are in the example-games directory (called "BadGame1.game" and "BadGame2.game").

# Notes #

The code has only been tested using GHC on Ubuntu.

Please contact [Ryan W. Porter](http://www.ryanwporter.com/) with any feedback.  Since a primary motivation for creating this project was to explore Haskell, I would particularly appreciate any Haskell-specific feedback.