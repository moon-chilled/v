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

fetchs7:
	wget https://ccrma.stanford.edu/software/s7/s7.tar.gz
	tar xf s7.tar.gz s7/s7.c s7/s7.h
	rm -f s7.tar.gz
	patch -p1 < s7/s7.patch

include $(wildcard o/*/*.d)

clean:
	rm -rf o/c v
spotless: clean
	rm -rf o
