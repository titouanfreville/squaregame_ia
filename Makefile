## Variables
OC=ocamlc
EXEC=square-game
GRAPHICS=src/graphic
IA=src/ia
OS=linux
# Module to link in compilation. Order is important
SRC_LIST=graphics.cma $(GRAPHICS)/grid.ml src/main.ml
# commands
default: all

all: init $(EXEC) clean

init:
	-mkdir -p build/$(OS) 1> /dev/null 2> /dev/null > /dev/null

$(EXEC): library src/main.ml
	$(OC) -o build/$(OS)/$@ $(SRC_LIST)

library: graphic ia

graphic:
	$(OC) -c $(GRAPHICS)/grid.mli
	cp $(GRAPHICS)/*.cm* .
	$(OC) -c $(GRAPHICS)/grid.ml
	cp $(GRAPHICS)/*.cm* .

ia:
	echo "waiting IA implementation"

clean:
	rm -rf $(GRAPHICS)/*.cm* src/*.cm* 