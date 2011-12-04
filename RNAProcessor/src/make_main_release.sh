ghc -O2  --make -o Main Main.hs

./Main +RTS -sstderr -A20M -RTS ../../test/selfcheck.dna

hp2ps -c Main.hp
