%{
#include "syntax.tab.h"
%}
INTD [1-9][0-9]*|[0]
INTH [0][Xx]([1-9a-fA-F][0-9a-fA-F]*|[0])
INTH_ERROR [0][Xx]([1-9a-zA-Z][0-9a-zA-Z]*|[0])
INTO [0][0-7]*
INTO_ERROR [0][0-9]*
FLOAT [0-9]+[.][0-9]+
SEMI [;]
COMMA [,]
ASSIGNOP [=]
RELOP [>]|[<]|[>][=]|[<][=]|[=][=]|[!][=]
PLUS [+]
MINUS [-]
STAR [*]
DIV [/]
AND [&][&]
OR [|][|]
DOT [.]
NOT [!]
TYPE int|float
LP \(
RP \)
LB \[
RB \]
LC \{
RC \}
STRUCT struct
RETURN return
IF if
ELSE else 
WHILE while
SPACE [ \n\t]
ID [a-zA-Z_][a-zA-Z_0-9]*
OTHER .
%%
{SEMI} {
	printf("SEMI %s\n", yytext);
}

{COMMA} {
	printf("COMMA %s\n", yytext);
}
{ASSIGNOP} {
	printf("ASSIGNOP %s\n", yytext);
}

{INTH} {
	printf("INTH %s\n",yytext);
}
{INTD} {
	printf("INTD %s\n", yytext);
}
{INTO} {
	printf("INTO %s\n",yytext);
}
{INTH_ERROR} {
	printf("Error type A at line %d : error hex number %s\n",yylineno,yytext);
}
{INTO_ERROR} {
	printf("Error type A at line %d : error hex number %s\n",yylineno,yytext);
}

{FLOAT} {
	printf("FLOAT %s\n", yytext);
}

{PLUS} {
	printf("PLUS %s\n",yytext);
} 

{MINUS} {
	printf("MINUS %s\n",yytext);
}
{DIV} {
	printf("DIV %s\n",yytext);
}
{STAR} {
	printf("STAR %s\n", yytext);
}

{RELOP} {
	printf("RELOP %s\n", yytext);
}

{AND} {
	printf("AND %s\n",yytext);
}
{OR} {
	printf("OR %s\n",yytext);
}
{NOT} {
	printf("NOT %s\n", yytext);
}

{DOT} {
	printf("DOT %s\n", yytext);
}
{STRUCT} {
	printf("STRUCT %s\n",yytext);
}
{RETURN} {
	printf("RETURN %s\n",yytext);
}
{IF} {
	printf("IF %s\n",yytext);
}
{ELSE} {
	printf("ELSE %s\n",yytext);
}
{WHILE} {
	printf("WHILE %s\n", yytext);
}

{TYPE} {
	printf("TYPE %s\n", yytext);
}

{LP} {
	printf("LP %s\n", yytext);
}
{RP} {
	printf("RP %s\n", yytext);
}
{LB} {
	printf("LB %s\n", yytext);
}
{RB} {
	printf("RB %s\n", yytext);
}
{LC} {
	printf("LC %s\n", yytext);
}
{RC} {
    printf("RC %s\n", yytext);
}

{SPACE} {
	if(yytext[0]=='\n')
	yylineno++;
/*ABANDON THESE CHARACTORS*/
}

{ID} {
    printf("ID %s\n", yytext);
}
{OTHER} {
printf(" Error type A at Line %d: Mysterious characters %s\n ",yylineno, yytext);
}
%%

