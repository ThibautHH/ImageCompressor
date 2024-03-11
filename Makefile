##
## EPITECH PROJECT, 2024
## B-FUN-400-REN-4-1-ImageCompresor-raphael.mabille
## File description:
## Makefile
##

NAME := imageCompressor

all:
	@stack build
	@ln -sf $(shell stack path --local-install-root)/bin/ImageCompressor-exe ./$(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

test: all
	./$(NAME)

re : fclean all
