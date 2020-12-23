sg=ShowGrades
ag=AddGrade

all: $(sg) $(ag)
  
$(sg): 
	ghc $(sg).hs
$(ag): 
	ghc $(ag).hs

clean:
	rm -f *.hi *.o $(sg) $(ag)
clean_:
	rm -f *.hi *.o
