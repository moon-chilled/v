CFLAGS := -Ii -g -Wall -Wextra -Werror -Wno-unused-parameter -Wno-parentheses -std=c11
CFLAGS += -MMD
LFLAGS := -ltickit -lgc
CCLD ?= $(CC)
C := c/cv.c c/tb.c c/functions.c c/function.c c/v.c
O := $(patsubst %.c,o/%.o,$(C))

v: $(O)
	$(CCLD) $(LFLAGS) -o v $(O)

o/%.o: %.c
	@mkdir -p `dirname $@`
	$(CC) -c -o $@ $(CFLAGS) $<

include $(wildcard o/*/*.d)

clean:
	rm -rf o v
