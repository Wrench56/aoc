CC       := clang
ASM      := nasm
CFLAGS   := -O2 -g -Wall -Wextra
ASMFLAGS := -f elf64

BUILD    := build
BIN      := $(BUILD)/solution

C_SRCS   := $(wildcard *.c)
ASM_SRCS := $(wildcard *.asm)

C_OBJS   := $(C_SRCS:%.c=$(BUILD)/%.o)
ASM_OBJS := $(ASM_SRCS:%.asm=$(BUILD)/%.o)
OBJS     := $(C_OBJS) $(ASM_OBJS)

INPUT    := input.txt

.PHONY: all run clean dirs

all: dirs $(BIN)

dirs:
	@mkdir -p $(BUILD)

$(BIN): $(OBJS)
	@printf "\033[1;34m[LD]\033[0m %s\n" $@
	@$(CC) $(CFLAGS) -no-pie -o $@ $(OBJS)

t(BUILD)/%.o: %.c
	@printf "\033[1;32m[CC]\033[0m %s\n" $<
	@$(CC) $(CFLAGS) -c $< -o $@

$(BUILD)/%.o: %.asm
	@printf "\033[1;35m[AS]\033[0m %s\n" $<
	@$(ASM) $(ASMFLAGS) -o $@ $<

run: all
	@printf "\033[1;33m[RN]\033[0m %s\n" $(BIN)
	@content="$$(cat '$(INPUT)')"; \
	len="$$(printf %s "$$content" | wc -c)"; \
	./$(BIN) "$$len" "$$content"
clean:
	rm -rf $(BUILD)
