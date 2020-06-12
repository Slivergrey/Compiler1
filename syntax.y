%locations
%{
#include <stdio.h>
#include "lex.yy.c"
#include "errorcheck.h"
#include "node.h"
#include "midcode.h"
//#define YYDEBUG 1
int yycolumn = 1;
int ERROR=0;
//#define YY_USER_ACTION \
yylloc.first_line = yylloc.last_line = yylineno; \
yylloc.first_column = yycolumn; \
yylloc.last_column = yycolumn + yyleng - 1; \
yycolumn += yyleng;
int yyerror(char *s)
{
	extern int yylineno;
	extern char *yytext;
	int len=strlen(yytext);
	int i;
	fprintf(stderr, "Error type B near symbol '%s' on line %d.\n", yytext, yylineno);
	//yyparse();
	return 0;
}

%}
%union{
	struct node* np;
	double b;
};
%token <np> INT FLOAT ID
%token <np> SEMI COMMA
%token <np> RELOP
%token <np> ASSIGNOP
%token <np> PLUS MINUS

%token <np> STAR DIV

%token <np> AND OR NOT
%token <np> DOT TYPE
%token <np> LP RP LB RB LC RC 
%token <np> STRUCT RETURN IF ELSE WHILE OTHER 
%type <np> Exp
%type <np>  Program ExtDefList ExtDef ExtDecList Specifier StructSpecifier OptTag Tag VarDec FunDec VarList ParamDec CompSt StmtList Stmt DefList Def DecList Dec Args error
%left OR
%left AND
%left DOT RELOP ASSIGNOP

%left LP RP RB PLUS MINUS
%left STAR DIV 

%right NOT LB 
%nonassoc ELSE
%%
Program:
ExtDefList{
	$$=gentree("Program",$1,NULL);
	if(!ERROR){
	//	pre($$,0);
//		errorcheck($$);
		gencode($$);
		}
	else printf("more than %d error(s) found.\n",ERROR);
}

;
ExtDefList:
ExtDef ExtDefList{$$=gentree("ExtDefList",$1,$2);}
| {$$=NULL;}
;
ExtDef:
Specifier ExtDecList SEMI{$$=argtree("ExtDef",3,$1,$2,$3);}
| Specifier SEMI{$$=gentree("ExtDef",$1,$2);}
| Specifier FunDec CompSt{$$=argtree("ExtDef",3,$1,$2,$3);}
;
ExtDecList:
VarDec{$$=gentree("ExtDecList",$1,NULL);}
| VarDec COMMA ExtDecList{$$=argtree("ExtDecList",3,$1,$2,$3);}
;
Specifier:
 TYPE{$$=gentree("Specifier",$1,NULL);}
| StructSpecifier{$$=gentree("Specifier",$1,NULL);}
;
StructSpecifier:
 STRUCT OptTag LC DefList RC{$$=argtree("StructSpecifier",5,$1,$2,$3,$4,$5);}
| STRUCT Tag{$$=gentree("StructSpecifier",$1,$2);}
;
OptTag:
 ID{$$=gentree("OptTag",$1,NULL);}
|{$$=NULL;}
;
Tag: 
ID{$$=gentree("Tag",$1,NULL);}
;
VarDec:
 ID {$$=gentree("VarDec",$1,NULL);}
| VarDec LB INT RB{$$=argtree("VarDec",4,$1,$2,$3,$4);}
;
FunDec:
 ID LP VarList RP{$$=argtree("FunDec",4,$1,$2,$3,$4);}
| ID LP RP{$$=argtree("FunDec",3,$1,$2,$3);}
;
VarList:
 ParamDec COMMA VarList{$$=argtree("VarList",3,$1,$2,$3);}
| ParamDec{$$=gentree("VarList",$1,NULL);}
;
ParamDec:
 Specifier VarDec{$$=gentree("ParamDec",$1,$2);}
;
CompSt:
 LC DefList StmtList RC{$$=argtree("CompSt",4,$1,$2,$3,$4);}
|error RC{$$=gentree("CompSt",$1,$2);ERROR++;}
;
StmtList:
Stmt StmtList{$$=gentree("StmtList",$1,$2);}
|{$$=NULL;}
;
Stmt:
Exp SEMI{$$=gentree("Stmt",$1,$2);}
| CompSt{$$=gentree("Stmt",$1,NULL);}
| RETURN Exp SEMI{$$=argtree("Stmt",3,$1,$2,$3);}
| IF LP Exp RP Stmt{$$=argtree("Stmt",5,$1,$2,$3,$4,$5);}
| IF LP Exp RP Stmt ELSE Stmt{$$=argtree("Stmt",7,$1,$2,$3,$4,$5,$6,$7);}
| WHILE LP Exp RP Stmt{$$=argtree("Stmt",5,$1,$2,$3,$4,$5);}
| error SEMI{$$=gentree("Stmt",$1,$2);ERROR++;}
;
DefList:
Def DefList {$$=gentree("DefList",$1,$2);}
|{$$=NULL;}
;
Def:
 Specifier DecList SEMI{$$=argtree("Def",3,$1,$2,$3);}
;
DecList:
 Dec{$$=gentree("DecList",$1,NULL);}
| Dec COMMA DecList{$$=argtree("DecList",3,$1,$2,$3);}
;
Dec:
 VarDec{$$=gentree("Dec",$1,NULL);}
| VarDec ASSIGNOP Exp{$$=argtree("Dec",3,$1,$2,$3);}
;
Exp:
Exp AND Exp{$$=argtree("Exp",3,$1,$2,$3);}
| Exp OR Exp{$$=argtree("Exp",3,$1,$2,$3);}
| Exp RELOP Exp{$$=argtree("Exp",3,$1,$2,$3);}
| Exp DIV Exp{$$=argtree("Exp",3,$1,$2,$3);}
| Exp STAR Exp{$$=argtree("Exp",3,$1,$2,$3);}
| Exp PLUS Exp{$$=argtree("Exp",3,$1,$2,$3);}
| Exp MINUS Exp{$$=argtree("Exp",3,$1,$2,$3);}
| Exp ASSIGNOP Exp{$$=argtree("Exp",3,$1,$2,$3);}
| LP Exp RP{$$=argtree("Exp",3,$1,$2,$3);}
| MINUS Exp{$$=gentree("Exp",$1,$2);}
| NOT Exp{$$=gentree("Exp",$1,$2);}
| ID LP Args RP{$$=argtree("Exp",4,$1,$2,$3,$4);}
| ID LP RP{$$=argtree("Exp",3,$1,$2,$3);}
| Exp LB Exp RB{$$=argtree("Exp",4,$1,$2,$3,$4);}
| Exp DOT ID{$$=argtree("Exp",3,$1,$2,$3);}
| ID{$$=gentree("Exp",$1,NULL);}
| INT{$$=gentree("Exp",$1,NULL);}
| FLOAT{$$=gentree("Exp",$1,NULL);}
| error RP{$$=gentree("Exp",$1,$2);ERROR++;}
;
Args: Exp COMMA Args{$$=argtree("Args",3,$1,$2,$3);}
| Exp{$$=gentree("Args",$1,NULL);}
;
%%

