CFLAGS := -Ii -Is7 -g -Wall -Wextra -Werror -Wno-unused-parameter -Wno-parentheses -Wno-missing-field-initializers -Wno-sign-compare -std=gnu11
LFLAGS := -ltickit -lgc -lm
ifneq ($(CC),tcc)
CFLAGS += -MMD
else
LFLAGS += -L/usr/local/lib
endif
CCLD ?= $(CC)
C := c/cv.c c/tb.c c/functions.c c/function.c c/v.c c/v7.c c/thunk.c
C += s7/s7.c
O := $(patsubst %.c,o/%.o,$(C))

v: $(O)
	$(CCLD) $(LFLAGS) -o v $(O)

o/%.o: %.c
	@mkdir -p `dirname $@`
	$(CC) -c -o $@ $(CFLAGS) $<
o/s7/s7.o: s7/s7.c
	@mkdir -p o/s7
	$(CC) -c -Ii -Is7 -g -std=c11 -o o/s7/s7.o s7/s7.c

include $(wildcard o/*/*.d)

clean:
	rm -rf o/c v
spotless: clean
	rm -rf o
