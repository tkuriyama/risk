
# Risk

<img title="Risk Screenshot" width="500" src="https://tarokuriyama.com/projects/risk/images/risk_screenshot2.png"> 
    
## Link

[Link to visualization](https://tarokuriyama.com/projects/risk.php).

## Solver

The original solver, written in Haskell (resides in the `haskell` directory). 

The solver was then ported to Elm with minimal modifications, so it runs on the fly when the visualization is loaded (omitting the need for data loading and parsing...).

At the time of writing, there didn't seem to be a rational numbers library compatible with Elm 0.19.1. So the Elm source includes a hand-written `Rational.elm` library, built on [`elm-bigint 2.0.1`](https://package.elm-lang.org/packages/cmditch/elm-bigint/latest/).
    
    
## Visualization

The Elm code compiles an interactive(ish) SVG visualization. 

To (re)compile the visualization, run `elm make src/Main.elm --optimize --output=elm.js` from the `elm` directory.

To run tests using `elm-test`, run `elm-test` from the `elm` directory. Parts of the project were also reviewed using `elm-coverage`.

## TCR

This project was partly developed using the "Test && Commit || Revert" workflow (see [some notes on TCR here](https://learning.tarokuriyama.com/2020/09/trying-tcr.html), hence the `tcr` directory and associated scripts.

