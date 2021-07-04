%option noyywrap


%{

   #include <stdio.h>

   #include <stdlib.h>

   #include <string.h>

   void PrintToken(char *tokenType);

   void yyerror();

   FILE* outputFile = NULL;

   int isFile = 0;
   int isFirstRun = 1;
   int newlineControl = 0;
%}



digit      [0-9]

IDENTIFIER [a-zA-Z][a-zA-Z0-9]*

VALUE      "0"|[1-9]{digit}*

NEWLINE "\n"


%%

";;".* 		{ PrintToken("COMMENT"); }

"and"          	{ PrintToken("KW_AND"); }

"or"            { PrintToken("KW_OR"); }

"not"        	{ PrintToken("KW_NOT"); }

"equal"      	{ PrintToken("KW_EQUAL"); }

"less"          { PrintToken("KW_OR"); }

"nil"          	{ PrintToken("KEYWORD_NIL"); }

"list"	       	{ PrintToken("KEYWORD_LIST"); }

"append"	{ PrintToken("KEYWORD_APPEND"); }

"concat"  	{ PrintToken("KEYWORD_CONCAT"); }

"set"	       	{ PrintToken("KEYWORD_SET"); }

"deffun"	{ PrintToken("KEYWORD_DEFFUN"); }

"for"	       	{ PrintToken("KEYWORD_FOR"); }

"if"	       	{ PrintToken("KEYWORD_IF"); }

"exit"	       	{ PrintToken("KEYWORD_EXIT"); }

"load"      	{ PrintToken("KEYWORD_LOAD"); }

"disp"      	{ PrintToken("KEYWORD_DISP"); }

"true"      	{ PrintToken("KEYWORD_TRUE"); }

"false"      	{ PrintToken("KEYWORD_FALSE"); }



"+"     { PrintToken("OP_PLUS"); }

"-"     { PrintToken("OP_MINUS"); }

"/"	{ PrintToken("OP_DIV"); }

"*"	{ PrintToken("OP_MULT"); }

"("    	{ PrintToken("OP_OP"); }

")"     { PrintToken("OP_CP"); }

"**"	{ PrintToken("OP_DBLMULT"); }

"\""	{ PrintToken("OP_OC"); }

","    	{ PrintToken("OP_COMMA"); }


{IDENTIFIER}    { PrintToken("IDENTIFIER"); }

{VALUE}      	{ PrintToken("VALUE"); }

{NEWLINE} {
   newlineControl += 1;
   if( (isFile == 0) && 
      ((isFirstRun==1 && newlineControl==1) || (newlineControl==2)))  
      return 0; 
   }

[ \t\r\f]+

.	{ yyerror("SYNTAX_ERROR"); }


%%


void PrintToken(char *tokenType){
   
   isFirstRun = 0;
   newlineControl = 0;
   fprintf(outputFile,"%s\n", tokenType);
}


void yyerror(char *ErrorMessage){

   isFirstRun = 0;
   newlineControl = 0;
   fprintf(outputFile,"SYNTAX__ERROR : %s cannot be tokenized\n", yytext);
}


int main(int argc, char *argv[]){
   
   
   if(argc == 2)
   {
      isFile = 1;
      yyin = fopen(argv[1], "r");
      outputFile = fopen("parsed_cpp.txt", "w+");
      if(yyin == NULL)
      {
         fprintf(outputFile,"SYNTAX__ERROR : file could not opened\n");
         fclose(outputFile);
         return 0;
      }
      yylex();
      fclose(yyin);
   }
   
   outputFile = fopen("parsed_cpp.txt", "w+");
   yylex();
   fclose(outputFile);
   return 0;

}
