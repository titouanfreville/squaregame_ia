## Variables
OC=ocamlc
EXEC=square-game
GRAPHICS=src/graphic
IA=src/ia
OS=linux
# Module to link in compilation. Order is important
SRC_LIST=graphics.cma $(GRAPHICS)/grid.ml $(IA)/base.ml src/main.ml
# commands
default: all install run_installed

all: init $(EXEC) clean

init:
	-@mkdir -p build/$(OS) 1> /dev/null 2> /dev/null > /dev/null

$(EXEC): library src/main.ml
	@$(OC) -o build/$(OS)/$@ $(SRC_LIST)

library: graphic ia

graphic:
	@$(OC) -c $(GRAPHICS)/grid.mli
	@cp $(GRAPHICS)/*.cm* .
	@$(OC) -c $(GRAPHICS)/grid.ml
	@cp $(GRAPHICS)/*.cm* .

ia: graphic
	@$(OC) -c $(IA)/base.mli
	@cp $(IA)/*.cm* .
	@$(OC) -c $(IA)/base.ml
	@cp $(IA)/*.cm* .

clean:
	@rm -rf src/*.cm*

clean_comp: clean
	@rm -rf src/**/*.cm* 

full_clean: clean clean_comp
	@rm -rf build 

run:
	@./build/$(OS)/$(EXEC)

run_installed:
	@$(EXEC)

install:
	sudo cp ./build/$(OS)/$(EXEC) /usr/bin