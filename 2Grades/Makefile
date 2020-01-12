sg=1ShowGrades
ag=2AddGrade
all: $(sg) $(ag)
  
$(sg): 
	ghc $(sg).hs
$(ag): 
	ghc $(ag).hs

.PHONY: clean clean_
clean:
	rm -f *.hi *.o $(sg) $(ag)
clean_:
	rm -f *.hi *.o
