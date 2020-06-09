#include"node.h"
#include<stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
extern int yylineno;
struct node* gennode(int line, char* name,struct node* left,struct node* right,int type,char* text)
{
	//printf("node generating:line %d, name %s\n",line,name);	
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
	//printf("node generate success:line %d, name %s\n",line,name);	
	return newnode;
}
struct node* argtree(char*name,int num,...)
{
	//printf("node generating:name=%s ,num =%d\n",name,num);	
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
			//if(temp->right)			
			//temp->right->line=yylineno;
		    }
		}
	}
	newnode->type=4;
	//printf("arg generate success:line %d, name %s\n",newnode->line,newnode->name);
	return newnode;
}
struct node* gentree(char* name, struct node* left,struct node* right)
{
	if(left==NULL)
	argtree(name,0);
	else	
	argtree(name,2,left,right);
}
void pre(struct node*a,int level)//level to mark father node and children node
{
    if(a!=NULL)
    {
	for(int i=level;i>0;i--)	
	printf("  ");
	printf("%d",level);
	printf("%s ",a->name);
	if(!strcmp(a->name,"RELOP"))
	{
	//	printf(":%s",a->value.IDname);
	}
	if(a->type==4&&strcmp(a->name,"TYPE"))    	
	{	
		printf("(%d)",a->line);               
	}
	else if(a->type==3)
	{
		printf(": %f",a->value.fvalue); 
	}
	else if(a->type==2)
	{
		printf(": %d",a->value.ivalue); 
	}
	else if(a->type==1)
	{
		printf(": %s",a->value.IDname); 
	}
	else if(!strcmp(a->name,"TYPE"))
	{
		printf(": %s",a->value.IDname); 
	}
	printf("\n");
        pre(a->left,level+1);
        pre(a->right,level);
    }
}
