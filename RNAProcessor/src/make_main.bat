ghc -O2 -prof -auto-all --make -o main.exe main.hs

main.exe +RTS -p -hr -sstderr -A20M -RTS ../../test/selfcheck.dna

hp2ps -c main.hp
