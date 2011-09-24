# Makefile

# OBJECT_PATH = ./obj

objects = mainframe.hs MetaData/clip.hs MetaData/operator.hs MetaData/types.hs UI/operator.hs UI/inputOperator.hs UI/outputOperator.hs UI/CUI/types.hs Utility/message.hs Utility/mthread.hs Utility/prim.hs Utility/like.hs Net/gate.hs Net/operator.hs

program = temp

# option = -g

$(program): $(objects)
	ghc -o $(program) $(objects)

.SUFFIXES: .hs .o

.hs.o:
	ghc -c $<

.PHONY : clean
clean :
	-rm $(program)
