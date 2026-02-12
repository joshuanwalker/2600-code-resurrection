ifeq ($(OS),Windows_NT)
    DASM=bin/dasm.exe
    STELLA=bin/Stella.exe
    MKDIR=@if not exist out mkdir out
    DIFF=fc /b
    VERIFY=@if exist orig\$(1).bin (echo Verifying $(1)... & $(DIFF) out\$(1).bin orig\$(1).bin)
else
    DASM=dasm
    STELLA=stella
    MKDIR=@mkdir -p out
    DIFF=diff
    VERIFY=@if [ -f orig/$(1).bin ]; then echo "Verifying $(1)..."; $(DIFF) out/$(1).bin orig/$(1).bin; fi
endif

DASM_FLAGS=-Iinc -T1 -f3

# Add new games here
GAMES=raiders haunted shuttle

.PHONY: all clean run $(GAMES)

all: $(GAMES)

# Usage: make raiders
$(GAMES): %: out/%.bin

# Build rule for each game
define GAME_RULE
out/$(1).bin: src/$(1)/$(1).asm
	$$(MKDIR)
	$$(DASM) src/$(1)/$(1).asm $$(DASM_FLAGS) -sout/$(1).sym -Lout/$(1).lst -oout/$(1).bin
	$(VERIFY)
endef

$(foreach game,$(GAMES),$(eval $(call GAME_RULE,$(game))))

clean:
	-rm -r out

# Usage: make run GAME=haunted
GAME ?= raiders
run: out/$(GAME).bin
	$(STELLA) out/$(GAME).bin
