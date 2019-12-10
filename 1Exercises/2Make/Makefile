sr=$(shell find '../1Code' -name '*.hs')
ex=../3Executable
nf=../4NotForMe

all: $(sr)
	ghc -o $(ex) -odir $(nf) -hidir $(nf) $^ 

clean:
	rm -f $(nf)/* $(ex)

clean_:
	rm -f $(nf)/*
