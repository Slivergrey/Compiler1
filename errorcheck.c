#include<stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "node.h"
typedef enum {BASIC_I,BASIC_F,FUNC,STR,ARRAY,STRNAME,BASIC_I_STR,BASIC_F_STR,ERROR}Type;
struct variable;
struct func;
unsigned int hash(char* name)
{
	unsigned int val = 0, i;
	for (; *name; ++name)
	{
		val = (val << 2) + *name;
		if (i = val & ~0x3fff) val = (val ^ (i >> 12)) & 0x3fff;
	}
	return val;
}
typedef struct array{
	int dimension;
	Type type;
} array;
typedef struct Struct{
	char* strname;
	struct variable* struct_list;
}Struct;

typedef struct variable{
	Type type;
	char* name;	
	union{
		int val_i;
		float val_f;
		array* arrayinfo;
		Struct* structinfo;
		struct func* funcinfo;	
	     }val;
	struct variable* next_val;
	struct variable* val_next;
}variable;

typedef struct func{
	Type return_type;
	variable* func_val;
}func;
Type get_type(struct node* head)
{
	//printf("%s\n",head->left->left->name);	
	if(!strcmp(head->left->left->name,"StructSpecifier"))
		return STR;
	else if(!strcmp(head->left->left->value.IDname,"int"))//for int
		return BASIC_I;
	else if(!strcmp(head->left->left->value.IDname,"float"))
		return BASIC_F;
	else return ERROR;
}
char * read_id(struct node* head,int *line)
{	
	if(head==NULL)
		return NULL;
	//printf("%s\n",head->name);
	if(head->type==1)
	{			
		*line = head->line;
		return head->value.IDname;
	
	}
	if(head->left==NULL&&head->right==NULL)
		return NULL;	
	return read_id(head->left,line);
}
variable* get_val(char* IDname,variable** hash_map)
{
	//printf("searching variable:%s\n",IDname);
	int index=hash(IDname);
	if(hash_map[index]==NULL)
	return NULL;
	variable* temp=hash_map[index];
	while(temp)
	{
		if(!strcmp(IDname,temp->name))
		{
			return temp;
		}
		else temp=temp->next_val;
	}
	return NULL;
} 
Type get_vari_type(char* name,variable**hash_map);
Type exp_type(node*head,variable**hash_map,int*error);
void get_id(struct node* head,variable** hash_map,Type type,int *error,variable* instr);
int compare(node* n1,node* n2,variable**hash_map,int*error)
{
	Type type1,type2;
	if(!strcmp("VarDec",n1->name))
	{
		type1=get_vari_type(n1->left->value.IDname,hash_map);
	}
	else type1=exp_type(n1,hash_map,error);
	type2=exp_type(n2,hash_map,error);
	if(type2==FUNC&&!strcmp(n2->name,"Stmt"))
	{
		variable* fun=hash_map[hash(n2->left->value.IDname)];
		type2=fun->val.funcinfo->return_type;	
		//printf("type2=%d\n",type2);
	}
	//printf("type1=%d type2=%d\n",type1,type2);
	if((type1==BASIC_I||type1==BASIC_I_STR)&&(type2==BASIC_I||type2==BASIC_I_STR))
		return 1;
	else if((type1==BASIC_F||type1==BASIC_F_STR)&&(type2==BASIC_F||type2==BASIC_F_STR))
		return 1;
	else if(type1==STR&&type2==STR)
	{
		//printf("struct compare!\n");
		variable* v1,*v2;
		v1=get_val(read_id(n1,&n1->line),hash_map);
		v2=get_val(read_id(n2,&n2->line),hash_map);
		//printf("%s\n",v1->name);
		//printf("%s\n",v2->name);
		v1=v1->val.structinfo->struct_list;
		v2=v2->val.structinfo->struct_list;
		while(v1)
		{
			if(!v2)
			return 0;
			//printf("%d %d\n",v1->type,v2->type);
			if(v1->type!=v2->type)
				return 0;
			else
			{
				v1=v1->val_next;
				v2=v2->val_next;
			}
		}
		if(!v2)		
		return 1;
		else return 0;
	}
	else return 0;
}
variable* add_to_hash(variable**hash_map,char* name, Type type,int line,int *error,variable*instr)
{
		unsigned int index = hash(name);
		variable * newval=(variable*)malloc(sizeof(variable));
		newval->type=type;
		newval->name=name;
		newval->val.val_i=0;
		newval->next_val=NULL;
		newval->val_next=NULL;
		if(instr&&type==STR)
		{
			//printf("marking STRUCT!\n");
			newval->val.structinfo=instr->val.structinfo;
		}
		//printf("newval.ID=%s,newval.type=%d,newval.hash=%d\n",name,type,index);
		if(hash_map[index]==NULL)
		hash_map[index]=newval;
		else{		
			variable* temp=hash_map[index];
			while(temp!=NULL)
			{
				if(strcmp(temp->name,newval->name))			
				temp=temp->next_val;
				else{
					if(type==FUNC)
					printf("Error type 4 at Line %d: Redefined function \"%s\".\n",line,temp->name);
					else if(type==BASIC_I||type==BASIC_F)
					printf("Error type 3 at Line %d: Redefined variable \"%s\".\n",line,temp->name);
					else if(type==STRNAME)
					printf("Error type 16 at Line %d: Duplicated name \"%s\".\n",line,temp->name);
					else if(type==BASIC_I_STR||type==BASIC_F_STR)
					printf("Error type 15 at Line %d: Redefined field \"%s\".\n",line,temp->name);
					*error++;
					break;
				}
			}				
			temp=newval;
		}
		return newval;
		//printf("add success\n");	
}
variable* funclist(struct node* head,variable**hash_map,int*error)
{
	variable* funlist=NULL;
	variable* current=NULL;
	if(head==NULL)
	return NULL;
	else {
		node* VarList=head;
			//printf("read start\n");
		while(!strcmp("VarList",VarList->name))
		{
			int line=0;	
			Type type=get_type(VarList->left);
			//printf("read success\n");
			node* FVarDec=VarList->left->left->right;
			char* strname;
			variable* instr;
			if(!strcmp(VarList->left->left->left->name,"StructSpecifier"))
			{
				strname=VarList->left->left->left->left->right->left->value.IDname;
				instr=get_val(strname,hash_map);
				type=STR;			
			}
			char* IDname=read_id(FVarDec,&line);
			//printf("read IDname:%s\n",IDname);		
			if(!strcmp("ID",FVarDec->left->name))
				{
					if(funlist==NULL)
					{
					//	printf("funlist is NULL!\n");
						funlist=add_to_hash(hash_map,IDname,type,line,error,instr);				
					}
					else
					{
						variable* current=funlist;
						while(current->val_next)
							{
							//	printf("current:%s\n",current->name);
								current=current->val_next;
							}
							current->val_next=add_to_hash(hash_map,IDname,type,line,error,instr);
					}	
				}
			else 
			{				
				array* newarray=(array*)malloc(sizeof(array));
				int dimension=0;				
				while(!strcmp("VarDec",FVarDec->left->name))
				{				
					FVarDec=FVarDec->left;
					dimension++;				
				}
				newarray->dimension=dimension;
				newarray->type=type;
				if(funlist==NULL)
				{	
					funlist=add_to_hash(hash_map,IDname,ARRAY,line,error,instr);
					//printf("read an array!\n");
					funlist->val.arrayinfo=newarray;	
				}
				else
				{
					variable* current=funlist;
					while(current->val_next)
						{
						//	printf("current:%s\n",current->name);
							current=current->val_next;
						}
					current->val_next=add_to_hash(hash_map,IDname,type,line,error,instr);
					current->val_next->val.arrayinfo=newarray;
					//printf("read an array!\n");			
				}
			}				
			if(VarList->left->right)
			{
				VarList=VarList->left->right->right;
			//	printf("link to next!\n");			
			}
			else break;
		}
		
		return funlist;
	}
}
node* get_return_node(node*head)
{
	if(head==NULL)
	return NULL;
	if(!strcmp(head->name,"RETURN"))
		return head->right;
	else 
	{
		node *left=get_return_node(head->left);
		if(left)return left;
		else return get_return_node(head->right);
	}
}
int get_func(variable** hash_map,struct node* head,int*error)
{
	if(head==NULL)
		return 0;
	//printf("%s\n",head->name);
	if(!strcmp((head->name),"ExtDef")&&head->left->right&&!strcmp((head->left->right->name),"FunDec"))
	{
//TODO array and variable link
		//printf("start add func\n");
		variable* funlist=NULL;
		func* newfunc=(func*)malloc(sizeof(func));
		newfunc->return_type=get_type(head);
		node* FunDec=head->left->right;
		newfunc->func_val=funclist(FunDec->left->right->right,hash_map,error);
		//printf("newfunc->func_val=%d\n",newfunc->func_val);
		//variable link 		
		
		int line;	
		//printf("add func success\n");	
		char * IDname =read_id(head->left->right,&line);
					
		funlist=add_to_hash(hash_map,IDname,FUNC,line,error,funlist);
		funlist->val.funcinfo=newfunc;
		//printf("checking return!\n");
		node* return_node=get_return_node(head);
		int match=1;		
		if(return_node)
		{
			//printf("checking return!\n");
			Type type1=newfunc->return_type;
			//printf("%s\n",return_node->name);
			Type type2=exp_type(return_node,hash_map,error);
			//printf("get two return type!\n");
			if((type1==BASIC_I||type1==BASIC_I_STR)&&(type2==BASIC_I||type2==BASIC_I_STR))
			match=1;
			else if((type1==BASIC_F||type1==BASIC_F_STR)&&(type2==BASIC_F||type2==BASIC_F_STR))
			match=1;
			else match=0;
			if(match==0)
			printf("Error type 8 at Line %d: Type mismatched for return.\n",return_node->line);
		}	
	}
	get_func(hash_map,head->left,error);
	get_func(hash_map,head->right,error);
	return 0;
}
void get_id(struct node* head,variable** hash_map,Type type,int *error,variable* instr)
{
	//printf("in get_id\n");		
	if(head==NULL)
		return ;
	else if(!strcmp((head->name),"DecList"))
	{
		int line=0;		
		char * IDname = read_id(head->left,&line);
		node* FVarDec=head->left->left;
		if(!strcmp("VarDec",FVarDec->left->name))
				{				
					FVarDec=FVarDec->left;
					//newarray->size=FVarDec->left->right->right->value.ivalue;
					if(instr)
					{
						//printf("reach here!\n");
						Struct* temp=instr->val.structinfo;
						if(temp==NULL)
						{
							temp=(Struct*)malloc(sizeof(Struct));				
							temp->strname=instr->name;
							temp->struct_list=NULL;						
						}
						variable *current=temp->struct_list;        
						while(current)
						{
							current=current->val_next;
						}
						current=add_to_hash(hash_map,IDname,ARRAY,line,error,instr);				
						//printf("struct add success!\n");					
					}
					else add_to_hash(hash_map,IDname,ARRAY,line,error,instr);
					
				}

		else
		{			
			if(instr&&type==STR)
			{
				//printf("assign for the struct!\n");
				add_to_hash(hash_map,IDname,type,line,error,instr);
			}
			else if(instr)
			{
				//printf("the struct name:%s\n",instr->name);
				Struct* temp=instr->val.structinfo;
				if(temp==NULL)
				{
					//printf("temp is null!\n"); 
					temp=(Struct*)malloc(sizeof(Struct));				
					temp->strname=instr->name;
					temp->struct_list=NULL;	
					instr->val.structinfo=temp;					
				}
				variable *current=temp->struct_list;        
				if(current==NULL)
				{				
					current=add_to_hash(hash_map,IDname,type,line,error,instr);
					//printf("in get_id\n");
					temp->struct_list=current;
				}
				else
				{
					while(current->val_next)
					{
						current=current->val_next; 
					}
					current->val_next=add_to_hash(hash_map,IDname,type,line,error,instr);
					current->val_next->val_next=NULL;			
				}
			}	
			else add_to_hash(hash_map,IDname,type,line,error,instr);
			if(FVarDec->right&&FVarDec->right->right)
			{
				if(!compare(FVarDec,FVarDec->right->right,hash_map,error))
					printf("Error type 5 at Line %d: Type mismatched for assignment.\n",FVarDec->line); 
					*error++;
			}
		}	
	}
	get_id(head->left,hash_map,type,error,instr);
	get_id(head->right,hash_map,type,error,instr);
}

int add_val(variable** map,struct node* head,int *error,variable* instr)
{	
	if(head==NULL)return 0;
	else
	{
		//if(instr==1)
		//printf("instr%s\n",head->name);
		int line=0;
		if(!strcmp(head->name,"StructSpecifier")&&(head->left->right->right))
		{					
			//printf("%s\n",head->left->right->right->name);			
			
			//type=STR;//to be continue
			char * IDname = read_id(head->left->right,&line);
			//printf("reach here!\n");
			variable * str=add_to_hash(map,IDname,STRNAME,line,error,instr);	
			add_val(map,head->left,error,str);
			add_val(map,head->right,error,str);
			return 0;
		}
		if(!strcmp((head->name),"Def"))
			{
				//printf("%s\n",head->name);
				Type type;
				//printf("reach here!\n");
				//printf("%s\n",head->left->left->name);		
				if(!strcmp(head->left->left->name,"StructSpecifier"))
					{
						type=STR;
						char* str_type=read_id(head->left->left->left->right,&line);						
						instr=get_val(str_type,map);
						//printf("%s\n",instr->name);
					}			
				else if(!strcmp(head->left->left->value.IDname,"int"))
					{
						if(instr==NULL)type=BASIC_I;
						else type=BASIC_I_STR;
					}
				else if(!strcmp(head->left->left->value.IDname,"float"))
					{
						if(instr==NULL)type=BASIC_F;
						else type=BASIC_F_STR;
					}
				else ;
				get_id(head->left->right,map,type,error,instr);
			}

 		add_val(map,head->left,error,instr);
		add_val(map,head->right,error,instr);
		return 0;	
	}	
}
void show_map(variable** map)
{
	variable * temp;	
	for(int i=0;i<0x4000;i++)
	{
		temp=map[i];		
		while(temp!=NULL)
		{
			printf("variable name:%s ,type:%d\n",temp->name,temp->type);
			temp=temp->next_val;
		}	
	}
}

void check_exist(node* head,variable** hash_map,int * error,int in_str)
{
	if(head==NULL)
		return ;
	if(!strcmp(head->name,"STRUCT"))
	{
		in_str=1;
	}

	else if(!strcmp(head->name,"ID")&&(head->right==NULL))
	{
		if(!get_val(head->value.IDname,hash_map)&&!in_str)
		{
			printf("Error type 1 at Line %d: Undefined variable \"%s\".\n",head->line,head->value.IDname);
			*error++;
		}
		else if((!get_val(head->value.IDname,hash_map)&&in_str))
		{
			printf("Error type 17 at Line %d: Undefined structure \"%s\".\n",head->line,head->value.IDname);
			*error++;
		}
	}
	else if(!strcmp(head->name,"ID")&&(!strcmp(head->right->name,"LP")))
	{
		if(!get_val(head->value.IDname,hash_map))
		{
			printf("Error type 2 at Line %d: Undefined function \"%s\".\n",head->line,head->value.IDname);
		*error++;
		}
	}
	check_exist(head->left,hash_map,error,in_str);
	check_exist(head->right,hash_map,error,in_str);
}
Type get_vari_type(char* name,variable**hash_map)
{
	variable* ret=get_val(name,hash_map);
	if(ret==NULL)return ERROR;
	return ret->type;
}
Type exp_type(node*head,variable**hash_map,int*error)
{
	//printf("checking Exp!\n");
	if((!strcmp(head->left->name,"Exp"))&&(!strcmp(head->left->right->name,"DOT"))&&(!strcmp(head->left->right->right->name,"ID")))
	{
		//printf("This is a structure\n");
		if(get_vari_type(head->left->left->value.IDname,hash_map)!=STR)
		{
			printf("Error type 13 at Line %d: Illegal use of \".\"\n",head->left->line);
			*error++;			
			return ERROR;
		}
		else
		{
			//checking if field exist
			//printf("checking field!\n");
			variable* str=get_val(head->left->left->value.IDname,hash_map); 
			//printf("%s\n",head->left->left->value.IDname);
			Struct*temp=str->val.structinfo;
			variable* str_list=temp->struct_list;
			//printf("%s\n",head->left->name);
			char* fieldname=head->left->right->right->value.IDname;
			//printf("%s\n",head->left->right->right->value.IDname);
			while(str_list&&strcmp(str_list->name,fieldname))
			{
				str_list=str_list->val_next;			
			}
			if(str_list==NULL)
			{
				printf("Error type 14 at Line %d: Non-existent field \"%s\".\n",head->left->line,fieldname);
				return ERROR;
			}

		}
		return get_vari_type(head->left->right->right->value.IDname,hash_map);
	}
	if((!strcmp(head->left->name,"ID"))&&(head->left->right==NULL))
	{
		return get_vari_type(head->left->value.IDname,hash_map);
	}
	else if((!strcmp(head->left->name,"INT"))&&(head->left->right==NULL))
	{
		return BASIC_I;
	}
	else if((!strcmp(head->left->name,"FLOAT"))&&(head->left->right==NULL))
	{
		return BASIC_F;
	}
	else if(!strcmp(head->left->name,"Exp")&&!strcmp(head->left->right->name,"SEMI"))
	{
		return exp_type(head->left,hash_map,error);
	}
//TODO add more expression
	else if(!strcmp(head->left->name,"ID")&&!strcmp(head->left->right->name,"LP"))
	{
		//this is a function!
		if(get_vari_type(head->left->value.IDname,hash_map)!=FUNC)
		{
			printf("Error type 11 at Line %d: \"%s\" is not a function \n",head->left->line,head->left->value.IDname);
			*error++;
			return ERROR;	
		}
	}
	else if((!strcmp(head->left->name,"Exp"))&&(!strcmp(head->left->right->name,"ASSIGNOP"))&&(!strcmp(head->left->right->right->name,"Exp")))
	{
		if(!compare(head->left,head->left->right->right,hash_map,error))
		printf("Error type 5 at Line %d: Type mismatched for assignment.\n",head->left->line);	
		if(!strcmp(head->left->left->name,"ID")||(!strcmp(head->left->name,"Exp")&&!strcmp(head->left->right->name,"LB")&&!strcmp(head->left->right->right->name,"Exp")&&!strcmp(head->left->right->right->right->name,"RB"))||(!strcmp(head->left->name,"Exp")&&!strcmp(head->left->right->name,"DOT")&&!strcmp(head->left->right->right->name,"ID")));
		else
		printf("Error type 6 at Line %d: The left-hand side of an assignment must be a variable.\n",head->left->line);	
	}
	else if((!strcmp(head->left->name,"Exp"))&&(!strcmp(head->left->right->name,"ASSIGNOP"))&&(!strcmp(head->left->right->right->name,"Stmt")))
	{
		if(!compare(head->left,head->left->right->right,hash_map,error))
		printf("Error type 5 at Line %d: Type mismatched for assignment.\n",head->left->line);
	}
	else if((!strcmp(head->left->name,"Stmt"))&&(!strcmp(head->left->right->name,"ASSIGNOP"))&&(!strcmp(head->left->right->right->name,"Exp")))
	{
		if((!strcmp(head->left->left->name,"Exp")||!strcmp(head->left->left->name,"Stmt"))&&(!strcmp(head->left->left->right->name,"LB"))&&(!strcmp(head->left->left->right->right->name,"Exp"))&&(!strcmp(head->left->left->right->right->right->name,"RB")))
		{
			node* Exp=head->left;
			int dimension=0;
			while(strcmp(Exp->left->name,"ID"))
				{
					Exp=Exp->left;
					dimension++;					
				}
			//printf("%d\n",dimension);
			variable* array=get_val(Exp->left->value.IDname,hash_map);
			//printf("%d\n",array->val.arrayinfo->type);
			if(array->val.arrayinfo->dimension==dimension)
				return array->val.arrayinfo->type;
			else 
			printf("Error type 5 at Line %d: Type mismatched for assignment.\n",head->left->line);			
			return ERROR;
		}
	}
	else if((!strcmp(head->left->name,"Stmt"))&&(!strcmp(head->left->right->name,"RELOP"))&&(!strcmp(head->left->right->right->name,"Exp")))
	{
		if((!strcmp(head->left->left->name,"Exp")||!strcmp(head->left->left->name,"Stmt"))&&(!strcmp(head->left->left->right->name,"LB"))&&(!strcmp(head->left->left->right->right->name,"Exp"))&&(!strcmp(head->left->left->right->right->right->name,"RB")))
		{
			node* Exp=head->left;
			int dimension=0;
			while(strcmp(Exp->left->name,"ID"))
				{
					Exp=Exp->left;
					dimension++;					
				}
			//printf("%d\n",dimension);
			variable* array=get_val(Exp->left->value.IDname,hash_map);
			//printf("%d\n",array->val.arrayinfo->type);
			if(array->val.arrayinfo->dimension==dimension)
				return array->val.arrayinfo->type;
			else 
			printf("Error type 7 at Line %d: Type mismatched for operands.\n",head->left->line);			
			return ERROR;
		}
	}
	else if((!strcmp(head->left->name,"Exp")||!strcmp(head->left->name,"Stmt"))&&(!strcmp(head->left->right->name,"LB"))&&(!strcmp(head->left->right->right->name,"Exp"))&&(!strcmp(head->left->right->right->right->name,"RB")))
		{
			node* Exp=head->left;
			int dimension=1;
			while(strcmp(Exp->left->name,"ID"))
				{
					Exp=Exp->left;
					dimension++;					
				}
			//printf("%d\n",dimension);
			variable* array=get_val(Exp->left->value.IDname,hash_map);
			//printf("%d\n",array->val.arrayinfo->type);
			if(array->val.arrayinfo->dimension==dimension)
				return array->val.arrayinfo->type;
			else return ERROR;
		}
	else if((!strcmp(head->left->name,"Exp"))&&(head->left->right->right)&&(!strcmp(head->left->right->right->name,"Exp")))
	{
		if((!strcmp(head->left->name,"Exp"))&&(!strcmp(head->left->right->name,"LB"))&&(!strcmp(head->left->right->right->name,"Exp"))&&(!strcmp(head->left->right->right->right->name,"RB")))
		{
			node* Exp=head->left;
			int dimension=1;
			while(strcmp(Exp->left->name,"ID"))
				{
					Exp=Exp->left;
					dimension++;					
				}
			printf("%d\n",dimension);
			variable* array=get_val(Exp->left->value.IDname,hash_map);
			//printf("%d\n",array->val.arrayinfo->type);
			if(array->val.arrayinfo->dimension==dimension)
				return array->val.arrayinfo->type;
			else return ERROR;
		}
		else if(!compare(head->left,head->left->right->right,hash_map,error))
		printf("Error type 7 at Line %d: Type mismatched for assignment.\n",head->left->line);	
		return ERROR;
	}
	else 	
	{
		node* current=head->left;
		Type type;		
		while(current)
		{
			if(!strcmp(current->name,"Exp"))
			type=exp_type(current,hash_map,error);
			current=current->right;
		}
		return type;
	}
}
int check_stmt(node* head,variable** hash_map,int * error)
{
	if(head==NULL)
		return 0;
	else if(!strcmp(head->name,"Stmt"))
	{
		if(!strcmp(head->left->name,"Exp")&&!strcmp(head->left->right->name,"SEMI"))
		{
			exp_type(head->left,hash_map,error);
		}
		if(head->left&&head->left->right&&head->left->right->right&&head->left->right->right->right)
		{
			//printf("This Stmt has more than 4 subnodes.\n");
			if((!strcmp(head->left->name,"Exp")||!strcmp(head->left->name,"Stmt"))&&(!strcmp(head->left->right->name,"LB"))&&(!strcmp(head->left->right->right->name,"Exp"))&&(!strcmp(head->left->right->right->right->name,"RB")))
			{ 
				node* Exp=head;
				int dimension=0;
				while(strcmp(Exp->left->name,"ID"))
					{
						Exp=Exp->left;
						dimension++;					
					}
				if(exp_type(Exp,hash_map,error)!=ARRAY)
				{
					//printf("%d\n",exp_type(head->left,hash_map,error));
					printf("Error type 10 at Line %d: \"%s\" is not an array.\n",head->left->line,read_id(head,&head->left->line));
					*error++;
				}
				else if(exp_type(head->left->right->right,hash_map,error)==BASIC_F)
				{
					printf("Error type 12 at Line %d: \"%f\" is not an integer\n",head->left->right->right->line,head->left->right->right->left->value.fvalue);
					*error++;
				}				
			}
			else if(!strcmp(head->left->name,"ID")&&(!strcmp(head->left->right->name,"LP"))&&(!strcmp(head->left->right->right->name,"Args"))&&(!strcmp(head->left->right->right->right->name,"RP")))
			{
				variable* function=get_val(head->left->value.IDname,hash_map);
				if(function==NULL)return 0;
				func* thisfunc=function->val.funcinfo;
				function=thisfunc->func_val;
				node*Exp=head->left->right->right->left;
				int match=1;
				//printf("this function name:%s\n",function->name);
				while(Exp)
				{
					//printf("once!\n");
					if(exp_type(Exp,hash_map,error)!=function->type)
						{
							match=0;
							break;
						}
					else{
							if(Exp->right)
							{						
								Exp=Exp->right->right->left;
								function=function->val_next;
							}
							else 
							{
								function=function->val_next;
								break;
							}							
							if(!function)
							{
								match=0;
								break;
							}
						}				
				}
				if(function)match=0;
				if(!match)
				printf("Error type 9 at Line %d: Function is not applicable for arguments.\n",head->line);
			}			
			else
			{
				//deal with every stmt and exp
				node* current=head->left;
				while(current)
				{
					if(!strcmp(current->name,"Exp"))
					exp_type(current,hash_map,error);
					current=current->right;
				}
			}
		}
	}
	check_stmt(head->left,hash_map,error);
	check_stmt(head->right,hash_map,error);
	return 0;
}
void check_stmtList(node* head,variable** hash_map,int * error)
{
	if(head==NULL)return;
	else if(!strcmp(head->name,"StmtList"))
	{
		//check for i[0]
		//printf("checking StmtList!\n");
		check_stmt(head->left,hash_map,error);
		check_stmtList(head->right,hash_map,error);
	}
		check_stmtList(head->left,hash_map,error);
		check_stmtList(head->right,hash_map,error);
}
int errorcheck(struct node* head)
{
	variable* hash_map[0x4000]={0};
	//func hash_map_func[0x4000]={0};
	//memset(hash_map_val,NULL,0x4000);
	//memset(hash_map_func,NULL,0x4000);
	int error=0;
	//printf("reach here!\n");
	struct node* origin=head;
	int valnum=add_val(hash_map,origin,&error,0);
	origin=head;	
	get_func(hash_map,origin,&error);	
	//show_map(hash_map);
	check_exist(head,hash_map,&error,0);
	check_stmtList(head,hash_map,&error);
	return 0;
}
