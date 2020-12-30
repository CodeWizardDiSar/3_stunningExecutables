sr = $(shell find '.' -name '*.hs')

ex = ./Executable
in = ./Intermediate

all: $(sr)
	mkdir -p Intermediate
	ghc -XFlexibleInstances -XLambdaCase -o $(ex) -odir $(in) -hidir $(in) $^

clean:
	rm -rf $(in) $(ex)

clean_:
	rm -rf $(in)
