ghc -O2 -prof -auto-all --make -o Main Main.hs

./Main +RTS -p -hy -sstderr -RTS ../../test/selfcheck.dna

hp2ps -c Main.hp
