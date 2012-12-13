rm -rf *.hi && rm -rf *.o rm -rf Main
ghc --make Main.hs && mv Main Trial
rm -rf *.hi && rm -rf *.o
