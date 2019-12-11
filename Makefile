###########################################################################
files=hsh.hs Lib.hs GRM.cf
# List of goals not corresponding to file names.
.PHONY : default all 

# lab3 must be first goal!
default: hsh

all: hsh 

# The extra dependency GRM/Test ensures that the parser is built from GRM.cf
hsh: $(files) GRM/Test
	ghc --make hsh.hs -o hsh

# Rules to build the parser:

GRM/Test.hs GRM/Lex.x GRM/Layout.hs GRM/Par.y : GRM.cf
	bnfc --ghc -d $<

GRM/Par.hs: GRM/Par.y
	happy -gcai $<

GRM/Lex.hs: GRM/Lex.x
	alex -g $<

GRM/Test: GRM/Test.hs GRM/Par.hs GRM/Lex.hs
	ghc --make $< -o $@

# Rules for cleaning generated files:

clean:
	-rm -f GRM/*.log GRM/*.aux GRM/*.hi GRM/*.o GRM/*.dvi *.hi *.o

# EOF
