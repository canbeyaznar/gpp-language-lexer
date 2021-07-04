ATTENTION!!

While running the makefile file, the error "clock skew detected" may be received. To fix this, please write "sudo touch * makefile" to the terminal. If not, please compile manually. I tried it on 2 different computers but couldn't fix this problem Here's how I run Flex:

> flex -o gpp_lexer.c gpp_lexer.l
> cc gpp_lexer.c -lfl -o gpp_lexer.out

The makefile is in my "Flex_Lexer" folder.
