# Makefile

# OBJECT_PATH = ./obj

SOURCES = mainframe.hs MetaData/clip.hs MetaData/operator.hs MetaData/types.hs UI/operator.hs UI/inputOperator.hs UI/outputOperator.hs UI/CUI/types.hs Utility/message.hs Utility/mthread.hs Utility/prim.hs Utility/like.hs Net/gate.hs Net/operator.hs

his = $(SOURCES:%.hs=%.hi)

objs = $(SOURCES:%.hs=%.o)

program = tEmp

# option = -g

$(program): $(SOURCES)
	ghc -o $(program) $(SOURCES)

.SUFFIXES: .hs .o

.hs.o:
	ghc -c $<

.PHONY : clean
clean :
	-rm -r $(program) $(his) $(objs)
