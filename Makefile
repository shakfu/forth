
.PHONY: all clean

all:
	@gcc -o forth forth.c

clean:
	@rm -f forth

