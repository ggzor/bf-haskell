CC:=clang
CFLAGS:=-O3
CFORMATTER:=$(shell which clang-format &>/dev/null \
						&& echo clang-format \
						|| echo cat)

SRC:=$(wildcard ../*.bf)
SRC_NAMES:=$(notdir $(SRC))

C_SRCS:=$(SRC_NAMES:.bf=.c)
C_OUTS:=$(C_SRCS:.c=)

VPATH=..:.

.PHONY: all
all: $(C_SRCS) $(C_OUTS)

%.c: %.bf
	./c_compile.sh '$<' | $(CFORMATTER) > '$@'

%: %.c
	$(CC) $(CFLAGS) '$<' -o '$@'

clean:
	find . -type f \( '!' -name '*.*' '!' -name Makefile \
				   -or -name '*.c' \) -delete
