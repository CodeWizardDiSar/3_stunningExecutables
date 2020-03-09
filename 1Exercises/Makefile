sr=$(shell find '.' -name '*.hs')
ex=./4Executable
nf=./NotForMe

all: $(sr)
	ghc -XFlexibleInstances -XLambdaCase -o $(ex) -odir $(nf) -hidir $(nf) $^

clean:
	rm -f $(nf)/* $(ex)

clean_:
	rm -f $(nf)/*
