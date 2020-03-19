%{
#include "syntax.tab.h"
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
extern int yycolumn;
union data
{
	char* IDname;
	int ivalue;
	float fvalue;
};
struct node{
	int line;
	char name[10];
	struct node* left;
	struct node* right;
	int type;//0 for others, 1 for id ,2 for int,3 for float	
	union data value;
};
struct node* gennode(int line, char* name,struct node* left,struct node* right,int type,char* text)
{
//	printf("node generating:line %d, name %s\n",line,name);	
	struct node* newnode;
	newnode = (struct node*)malloc(sizeof(struct node));
	newnode->line=line;
	strcpy(newnode->name,name);
	newnode->left=left;
	newnode->right=right;
	newnode->type=type;
	if(type==1||!strcmp(name,"TYPE")||!strcmp(name,"RELOP"))
{
	newnode->value.IDname=(char*)malloc(strlen(text));
	strcpy(newnode->value.IDname,text);
}
	else if(type==2)
	newnode->value.ivalue=atoi(text);
	else if(type==3)
	newnode->value.fvalue=atof(text);
//	printf("node generate success:line %d, name %s\n",line,name);	
	return newnode;
}
struct node* argtree(char*name,int num,...)
{
	va_list list;
	struct node* newnode;
	newnode = (struct node*)malloc(sizeof(struct node));
	strcpy(newnode->name,name);
    	va_start(list,num);
	if(num==0)
	newnode->line=yylineno;
	if(num>0)
	{
		struct node* temp=va_arg(list, struct node*);
		newnode->left=temp;
		newnode->line=temp->line;
		if(num>=2) 
		{
		    for(int i=0; i<num-1; ++i)
		    {
			while(temp->right)temp=temp->right;		        
			temp->right=va_arg(list,struct node*);
		        temp=temp->right;
		    }
		}
	}
	newnode->type=4;
//	printf("arg generate success:line %d, name %s\n",newnode->line,name);
	return newnode;
}
struct node* gentree(char* name, struct node* left,struct node* right)
{
	if(left==NULL)
	argtree(name,0);
	else	
	argtree(name,2,left,right);
}

%}

INTD [1-9][0-9]*|[0]
INTD_ERROR [1-9][0-9a-zA-Z]*
INTH [0][Xx]([1-9a-fA-F][0-9a-fA-F]*|[0])
INTH_ERROR [0][Xx]([1-9a-zA-Z_][0-9a-zA-Z_S]*|[0])
INTO [0][0-7]*
INTO_ERROR [0][0-9a-zA-Z_]*
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
//	printf("SEMI %s\n", yytext);
	yylval.np=gennode(yylineno,"SEMI",NULL,NULL,0,yytext);
	return SEMI;
}

{COMMA} {
//	printf("COMMA %s\n", yytext);
	yylval.np=gennode(yylineno,"COMMA",NULL,NULL,0,yytext);
	return COMMA;
}
{ASSIGNOP} {
//	printf("ASSIGNOP %s\n", yytext);
	yylval.np=gennode(yylineno,"ASSIGNOP",NULL,NULL,0,yytext);
	return ASSIGNOP;
}

{INTH} {
//	printf("INTH %s\n",yytext);
	int value;
	sscanf(yytext,"%x",&value);
	sprintf(yytext,"%d",value);
	yylval.np=gennode(yylineno,"INT",NULL,NULL,2,yytext);
	return INT;
}
{INTD} {
//	printf("INTD %s\n", yytext);
	yylval.np=gennode(yylineno,"INT",NULL,NULL,2,yytext);
	return INT;
}
{INTO} {
//	printf("INTO %s\n",yytext);
	int value;
	sscanf(yytext,"%o",&value);
	sprintf(yytext,"%d",value);
	yylval.np=gennode(yylineno,"INT",NULL,NULL,2,yytext);
	return INT;
}
{INTH_ERROR} {
	printf("Error type A at line %d : Illegal hexadecimal number %s\n",yylineno,yytext);
}
{INTO_ERROR} {
	printf("Error type A at line %d : Illegal octal number %s\n",yylineno,yytext);
}
{INTD_ERROR} {
	printf("Error type A at line %d : Illegal number %s\n",yylineno,yytext);
}

{FLOAT} {
//	printf("FLOAT %s\n", yytext);
	yylval.np=gennode(yylineno,"FLOAT",NULL,NULL,3,yytext);
	return FLOAT;	
}

{PLUS} {
//	printf("PLUS %s\n",yytext);
	yylval.np=gennode(yylineno,"PLUS",NULL,NULL,0,yytext);
	return PLUS;
} 

{MINUS} {
//	printf("MINUS %s\n",yytext);
	yylval.np=gennode(yylineno,"MINUS",NULL,NULL,0,yytext);
	return MINUS;
}
{DIV} {
//	printf("DIV %s\n",yytext);
	yylval.np=gennode(yylineno,"DIV",NULL,NULL,0,yytext);
	return DIV;
}
{STAR} {
//	printf("STAR %s\n", yytext);
	yylval.np=gennode(yylineno,"STAR",NULL,NULL,0,yytext);
	return STAR;
}

{RELOP} {
//	printf("RELOP %s\n", yytext);
	yylval.np=gennode(yylineno,"RELOP",NULL,NULL,0,yytext);
	return RELOP;
}

{AND} {
//	printf("AND %s\n",yytext);
	yylval.np=gennode(yylineno,"AND",NULL,NULL,0,yytext);
	return AND;
}
{OR} {
//	printf("OR %s\n",yytext);
	yylval.np=gennode(yylineno,"OR",NULL,NULL,0,yytext);
	return OR;
}
{NOT} {
//	printf("NOT %s\n", yytext);
	yylval.np=gennode(yylineno,"NOT",NULL,NULL,0,yytext);
	return NOT;
}

{DOT} {
//	printf("DOT %s\n", yytext);
	yylval.np=gennode(yylineno,"DOT",NULL,NULL,0,yytext);
	return DOT;
}
{STRUCT} {
//	printf("STRUCT %s\n",yytext);
	yylval.np=gennode(yylineno,"STRUCT",NULL,NULL,0,yytext);
	return STRUCT;
}
{RETURN} {
//	printf("RETURN %s\n",yytext);
	yylval.np=gennode(yylineno,"RETURN",NULL,NULL,0,yytext);
	return RETURN;
}
{IF} {
//	printf("IF %s\n",yytext);
	yylval.np=gennode(yylineno,"IF",NULL,NULL,0,yytext);
	return IF;
}
{ELSE} {
//	printf("ELSE %s\n",yytext);
	yylval.np=gennode(yylineno,"ELSE",NULL,NULL,0,yytext);
	return ELSE;
}
{WHILE} {
//	printf("WHILE %s\n", yytext);
	yylval.np=gennode(yylineno,"WHILE",NULL,NULL,0,yytext);
	return WHILE;
}

{TYPE} {
//	printf("TYPE %s\n", yytext);
	yylval.np=gennode(yylineno,"TYPE",NULL,NULL,0,yytext);
	return TYPE;
}

{LP} {
//	printf("LP %s\n", yytext);
	yylval.np=gennode(yylineno,"LP",NULL,NULL,0,yytext);
	return LP;
}
{RP} {
//	printf("RP %s\n", yytext);
	yylval.np=gennode(yylineno,"RP",NULL,NULL,0,yytext);
	return RP;
}
{LB} {
//	printf("LB %s\n", yytext);
	yylval.np=gennode(yylineno,"LB",NULL,NULL,0,yytext);
	return LB;
}
{RB} {
//	printf("RB %s\n", yytext);
	yylval.np=gennode(yylineno,"RB",NULL,NULL,0,yytext);
	return RB;
}
{LC} {
//	printf("LC %s\n", yytext);
	yylval.np=gennode(yylineno,"LC",NULL,NULL,0,yytext);
	return LC;
}
{RC} {
//	printf("RC %s\n", yytext);
	yylval.np=gennode(yylineno,"RC",NULL,NULL,0,yytext);
	return RC;
}

{SPACE} {
	if(yytext[0]=='\n')
	{
		yylineno++;
		//yycolumn=1;
	}
}

{ID} {
//	printf("ID %s\n", yytext);
	yylval.np=gennode(yylineno,"ID",NULL,NULL,1,yytext);
	return ID;
}
{OTHER} {
	printf(" Error type A at Line %d: Mysterious characters %s\n ",yylineno, yytext);
	return OTHER;
}
%%

