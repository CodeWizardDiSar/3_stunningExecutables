sr = $(shell find '../1_source' -name '*.hs')
ex = ../3_executable/exercises
nf = ../4_notForMe

all: $(sr)
	ghc -o $(ex) -odir $(nf) -hidir $(nf) $^ 

clean:
	rm -f $(nf)/* $(ex)

clean_:
	rm -f $(nf)/*
