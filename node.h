#ifndef NODE_H
#define NODE_H
#include<stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
union data
{
	char* IDname;
	int ivalue;
	float fvalue;
};
typedef struct node{
	int line;
	char name[16];
	struct node* left;
	struct node* right;
	int type;//0 for others, 1 for id ,2 for int,3 for float	
	union data value;
}node;
struct node* gennode(int line, char* name,struct node* left,struct node* right,int type,char* text);

struct node* argtree(char*name,int num,...);
struct node* gentree(char* name, struct node* left,struct node* right);
#endif
