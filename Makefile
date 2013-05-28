EXAMPLES = branch_diff

default: all

all: $(EXAMPLES:=.o)

%.o: %.hs
	ghc -threaded -O2 --make -fforce-recomp -with-rtsopts="-N" $(@:.o=)

clean:
	rm -f *.hi *.o $(EXAMPLES)
