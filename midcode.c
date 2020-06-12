#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>
#include "node.h"
int MAX_SYM=0x4000;
int label=1;
int offsetnow=0;
extern char* addrout;
int checkstr(node* head)
{
	if(head==NULL)
		return 0;
	else if(!strcmp(head->name,"STRUCT"))
		return 1;
	else return checkstr(head->left)+checkstr(head->right);
}
struct arraynode{
	int size;
	struct arraynode* next;
};

typedef struct arraynode an;

an* reverseList(an* head){
    if (head == NULL || head->next == NULL)
        return head;
    an *pre = head;
    an *cur = head->next;
    an *tmp = head->next->next;
    while(cur)
    {
        tmp = cur->next;
        cur->next = pre;
        pre = cur;
        cur = tmp;
    }
    head->next = NULL;
    return pre;
}
typedef struct Operand_* Operand;
typedef struct InterCode ic;
typedef struct InterCodes ics;
typedef struct Sym sym;
struct Sym{
	enum{UNUSED,INT,FLOAT,ARRAY,FUNC,USING,STARINT,FUNARRAY}type;
	char* ID;
	an* node;
	int offset;
};
struct Operand_ {
enum { VARIABLE, CONSTANT, ADDRESS,STAR,FUNCNAME,FUNPARAM} kind;
union {
	int var_no;
	int value_i;
	float value_f;
	} u;
};
struct InterCode
{
	enum { ASSIGN,ADDR,DECADDR,ADDRMORE, ADD, SUB, MUL, DIV,DEC,NEG,PARAM,GOTO,RETURN,READ,WRITE,CALL,COND_N,COND_O,LABEL,ARGS} kind;
	union {
		struct {Operand to_neg;}neg;
		struct { Operand dec_val;}dec_val;
		struct { Operand right, left; } assign;
		struct { Operand result, op1, op2; } binop;
		struct {int t1,t2,lt,lf;char* op;} cond_nval;
		struct {int t1,lt,lf;}cond_oval;
		struct {int label_num;}label;
		struct {int args_num;}args;
		struct {Operand right,left,bias;}addr;
		struct {Operand right,left;an* bias;}addrmore;
		struct {int valid,size;}dec_addr;
	} u;
};
int get_valid(char* ID,sym* table_sym)
{
	//printf("get id of %s!\n",ID);
	for(int i=0;i<MAX_SYM;i++)
	{ 
		if(table_sym[i].ID&&!strcmp(ID,table_sym[i].ID))
		return i;
	}
}
struct InterCodes { ic code; ics *prev, *next; };
ics* translate_func(node* FunDec,sym *table_sym,ics*head);
ics* translate_Exp(node* Exp,ics*head,sym* talbe_sym,int topid);
ics* translate_Def(node* Def,ics* current,sym*table_sym);
ics* translate_DefList(node* DefList,ics* current,sym*table_sym);
ics* translate_StmtList(node* StmtList,ics* current,sym*table_sym);
ics* translate_CompSt(node* CompSt,ics* current,sym*table_sym);
ics* translate_Stmt(node* Stmt,ics* current,sym*table_sym);
ics* addcode(ic* code,ics *head)
{
	//ics* head=*inhead;
	if(head==NULL)
	{
		head=(ics*)malloc(sizeof(ics));
		head->prev=NULL;
		head->code=*code;
		head->next=NULL;

		return head;
	}
	else{
		ics* prev=head;
		ics* current=head->next;
		while(current)
		{
			prev=prev->next;
			current=current->next;
		}
		current=(ics*)malloc(sizeof(ics));
		prev->next=current;
		current->prev=prev;
		current->code=*code;
		current->next=NULL;
		return head;
	}
}
int new_id(sym* table_sym)
{
	int dec_id=0;
	for(int i=0;i<MAX_SYM;i++)
	{
		if(table_sym[i].type!=UNUSED)
		continue;
		else {
			dec_id=i;
			break;
		}
	}
	table_sym[dec_id].type=USING;
	table_sym[dec_id].offset=offsetnow;
	offsetnow+=4;
	return dec_id;
}
ics* translate_param(node* VarList,sym* table_sym,ics* head)
{
	ic param;
	param.kind=PARAM;
	int param_id=new_id(table_sym);
	table_sym[param_id].ID=VarList->left->left->right->left->value.IDname;
	if(VarList->left->left->right->left->right)
	{
		table_sym[param_id].ID=VarList->left->left->right->left->left->value.IDname;
		table_sym[param_id].type=FUNARRAY;
		param.u.dec_val.dec_val=(Operand)malloc(sizeof(struct Operand_));
		param.u.dec_val.dec_val->kind=FUNPARAM;
		param.u.dec_val.dec_val->u.var_no=param_id;
//		printf("recognize array! name:%s ID:%d\n",table_sym[param_id].ID,param_id);
		head=addcode(&param,head);
	}
	else if(!strcmp(VarList->left->left->left->value.IDname,"int"))
	{
		table_sym[param_id].ID=VarList->left->left->right->left->value.IDname;
		table_sym[param_id].type=INT;	
		param.u.dec_val.dec_val=(Operand)malloc(sizeof(struct  Operand_));
		param.u.dec_val.dec_val->kind=FUNPARAM;
		param.u.dec_val.dec_val->u.var_no=param_id;
//		printf("recognize int! name:%s ID:%d\n",table_sym[param_id].ID,param_id);
		head=addcode(&param,head);
	}
	if(VarList->left->right)
	{
		head=translate_param(VarList->left->right->right,table_sym,head);
	}
	return head;
}
ics* translate_func(node* FunDec,sym* table_sym,ics* head)
{
	ic fundec;
	fundec.kind=DEC;
	//find an index
	int dec_id=new_id(table_sym);
	table_sym[dec_id].ID=FunDec->left->value.IDname;
	table_sym[dec_id].type=FUNC;
	fundec.u.dec_val.dec_val=(Operand)malloc(sizeof(struct Operand_));
	fundec.u.dec_val.dec_val->kind=FUNCNAME;
	fundec.u.dec_val.dec_val->u.var_no=dec_id;
//	printf("recognize function! name: %s, ID:%d\n",table_sym[dec_id].ID,dec_id);
	head=addcode(&fundec,head);
	//printf("%d\n",head->code.kind);
	if(!strcmp(FunDec->left->right->right->name,"VarList"))
	{
		head=translate_param(FunDec->left->right->right,table_sym,head);
	}
	head=translate_CompSt(FunDec->right,head,table_sym);
	//printf("hit func\n");
	return head;
}
ics * translate_Cond(node* Exp,ics*current,int labelt,int labelf,sym* table_sym)
{
	//printf("in translate_Cond!\n");
	if(!strcmp(Exp->left->name,"NOT")&&!strcmp(Exp->left->right->name,"Exp"))
	{
		//printf("find NOT!\n");
		return translate_Cond(Exp->left->right,current,labelf,labelt,table_sym);
	}
	else if(!strcmp(Exp->left->name,"Exp")&&!strcmp(Exp->left->right->name,"RELOP")&&!strcmp(Exp->left->right->right->name,"Exp"))
	{
		//printf("find RELOP!\n");
		int t1=new_id(table_sym);
		int t2=new_id(table_sym);
		current=translate_Exp(Exp->left,current,table_sym,t1);
		current=translate_Exp(Exp->left->right->right,current,table_sym,t2);

		char * op = Exp->left->right->value.IDname;
		ic temp;
		temp.kind=COND_N;
		temp.u.cond_nval.lf=labelf;
		temp.u.cond_nval.lt=labelt;
		temp.u.cond_nval.t1=t1;
		temp.u.cond_nval.t2=t2;
		temp.u.cond_nval.op=op;
		//printf("condition ! RELOP: %s\n",op);
		current=addcode(&temp,current);
	}
	else if(!strcmp(Exp->left->name,"Exp")&&!strcmp(Exp->left->right->name,"AND")&&!strcmp(Exp->left->right->right->name,"Exp"))
	{
		//printf("find AND!\n");
		int label1=label++;
		current=translate_Cond(Exp->left,current,label1,labelf,table_sym);
		ic temp;
		temp.kind=LABEL;
		temp.u.label.label_num=label1;
		current=addcode(&temp,current);
		current=translate_Cond(Exp->left->right->right,current,labelt,labelf,table_sym);
	}
	else if(!strcmp(Exp->left->name,"Exp")&&!strcmp(Exp->left->right->name,"OR")&&!strcmp(Exp->left->right->right->name,"Exp"))
	{
		//printf("find OR!\n");
		int label1=label++;
		current=translate_Cond(Exp->left,current,labelt,label1,table_sym);
		ic temp;
		temp.kind=LABEL;
		temp.u.label.label_num=label1;
		current=addcode(&temp,current);
		current=translate_Cond(Exp->left->right->right,current,labelt,labelf,table_sym);
	}
	else if(!strcmp(Exp->left->name,"LP"))
	{
		current=translate_Cond(Exp->left->right,current,labelt,labelf,table_sym);
	}
	else 
	{
		int label1=label++;
		int t1=new_id(table_sym);
		current=translate_Exp(Exp,current,table_sym,t1);
		ic temp;
		temp.kind=COND_O;
		temp.u.cond_oval.lf=labelf;
		temp.u.cond_oval.lt=labelt;
		temp.u.cond_oval.t1=t1;
		current=addcode(&temp,current);
	}

	return current;
}
ics* translate_Args(node* Args,ics* current,sym* table_sym,int topid)
{
	if(!Args->left->right)//only one!
	{
		int t1=new_id(table_sym);
		current=translate_Exp(Args->left,current,table_sym,t1);
		if(topid==-2)
		{
			ic temp;
			temp.kind=WRITE;
			temp.u.args.args_num=t1;
			current=addcode(&temp,current);
		}
		else{
			ic temp;
			temp.kind=ARGS;
			temp.u.args.args_num=t1;
			current=addcode(&temp,current);
		}
	}
	else 
	{
		int t1=new_id(table_sym);
		current=translate_Exp(Args->left,current,table_sym,t1);
		current=translate_Args(Args->left->right->right,current,table_sym,topid);
		ic temp;
		temp.kind=ARGS;
		temp.u.args.args_num=t1;
		current=addcode(&temp,current);
	}
	return current;
}
ics* translate_Array(node* Exp,ics* current,sym* table_sym,int topid)
{
//	printf("in ARRAY!");
	if(!Exp->left->left->left)
	{//d=1
		ic temp;
		table_sym[topid].type=STARINT;
		int sec_expid=new_id(table_sym);
		int sec_temp=new_id(table_sym);
		int val_id=get_valid(Exp->left->left->value.IDname,table_sym);
		current=translate_Exp(Exp->left->right->right,current,table_sym,sec_expid);
		temp.kind=ADDR;
		temp.u.addr.bias=(Operand)malloc(sizeof(struct Operand_));
		temp.u.addr.bias->kind=VARIABLE;
		temp.u.addr.bias->u.var_no=sec_expid;
		temp.u.addr.right=(Operand)malloc(sizeof(struct Operand_));
		temp.u.addr.right->kind=ADDRESS;
		temp.u.addr.right->u.var_no=val_id;
		temp.u.addr.left=(Operand)malloc(sizeof(struct Operand_));
		temp.u.addr.left->kind=ADDRESS;
		temp.u.addr.left->u.var_no=topid;
		current=addcode(&temp,current);
	}
	else
	{
		ic temp;
		table_sym[topid].type=STARINT;
		int sec_expid=0;
		int sec_temp=new_id(table_sym);
		node* cur=Exp;
		while(cur->left)
		{
			cur=cur->left;
		}
		int val_id=get_valid(cur->value.IDname,table_sym);
		temp.kind=ADDRMORE;
		cur=Exp;
		temp.u.addrmore.bias=(an*)malloc(sizeof(an));
		an* ancur=temp.u.addrmore.bias;
		while(Exp->left->right)
		{
			sec_expid=new_id(table_sym);
			current=translate_Exp(Exp->left->right->right,current,table_sym,sec_expid);
			ancur->size=sec_expid;
			ancur->next=(an*)malloc(sizeof(an));
			ancur=ancur->next;
			ancur->next=NULL;
		}
		temp.u.addrmore.bias=reverseList(ancur);
		temp.u.addrmore.right=(Operand)malloc(sizeof(struct Operand_));
		temp.u.addrmore.right->kind=ADDRESS;
		temp.u.addrmore.right->u.var_no=val_id;
		temp.u.addrmore.left=(Operand)malloc(sizeof(struct Operand_));
		temp.u.addrmore.left->kind=ADDRESS;
		temp.u.addrmore.left->u.var_no=topid;
		current=addcode(&temp,current);
	}
	
}
ics* translate_Exp(node* Exp,ics* current,sym* table_sym,int topid)
{
	//printf("in Exp!,line:%d\n",Exp->line);

//	if(Exp->left)
//	printf("Exp->left->name:%s\n",Exp->left->name);
	if(!strcmp(Exp->left->name,"INT"))
	{
		int value=Exp->left->value.ivalue;
		ic temp;
		temp.kind=ASSIGN;
		temp.u.assign.right=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.right->kind=CONSTANT;
		temp.u.assign.right->u.value_i=value;
		temp.u.assign.left=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.left->kind=VARIABLE;
		temp.u.assign.left->u.var_no=topid;
		table_sym[topid].type=INT;
	//	printf("recognize int %d\n",value);
		current=addcode(&temp,current);
	}
	else if(!strcmp(Exp->left->name,"FLOAT"))
	{
		float value=Exp->left->value.fvalue;
		ic temp;
		temp.kind=ASSIGN;
		temp.u.assign.right=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.right->kind=CONSTANT;
		temp.u.assign.right->u.value_f=value;
		temp.u.assign.left=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.left->kind=VARIABLE;
		temp.u.assign.left->u.var_no=topid;
		table_sym[topid].type=FLOAT;
		//printf("recognize int %f\n",value);
		current=addcode(&temp,current);
	}
	else if(!strcmp(Exp->left->name,"ID")&&!Exp->left->right)
	{
		int valid=get_valid(Exp->left->value.IDname,table_sym);
		ic temp;
		temp.kind=ASSIGN;
		temp.u.assign.right=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.right->kind=VARIABLE;
		temp.u.assign.right->u.var_no=valid;
		temp.u.assign.left=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.left->kind=VARIABLE;
		temp.u.assign.left->u.var_no=topid;
		table_sym[topid].type=table_sym[valid].type;
	//	printf("recognize IDname %s, id: %d\n",Exp->left->value.IDname,valid);
		current=addcode(&temp,current);
	}
	else if(!strcmp(Exp->left->name,"Exp")&&!strcmp(Exp->left->right->name,"ASSIGNOP")&&!strcmp(Exp->left->right->right->name,"Exp")&&!strcmp(Exp->left->left->name,"ID"))
	{
		ic temp;
		temp.kind=ASSIGN;
		int temp1=get_valid(Exp->left->left->value.IDname,table_sym);
		int temp2=new_id(table_sym);
//		printf("here!\n");
		current=translate_Exp(Exp->left->right->right,current,table_sym,temp2);
		temp.u.assign.left=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.left->u.var_no=temp1;
		if(table_sym[temp1].type==STARINT)
		temp.u.assign.right->kind=STAR;
		else 
		temp.u.assign.left->kind=VARIABLE;
		temp.u.assign.right=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.right->u.var_no=temp2;
		if(table_sym[temp2].type==STARINT)
		temp.u.assign.right->kind=STAR;
		else 
		temp.u.assign.right->kind=VARIABLE;
		current=addcode(&temp,current);
//		printf("ASSIGN END!\n");
	}
	else if(!strcmp(Exp->left->name,"Exp")&&!strcmp(Exp->left->right->name,"ASSIGNOP")&&!strcmp(Exp->left->right->right->name,"Exp"))
	{
		//in this case , Exp_1 should be Exp->id!
		//printf("here!\n");
		ic temp;
		temp.kind=ASSIGN;
		int temp1=new_id(table_sym);
		int temp2=new_id(table_sym);
		current=translate_Exp(Exp->left,current,table_sym,temp1);
		current=translate_Exp(Exp->left->right->right,current,table_sym,temp2);
		temp.u.assign.left=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.left->u.var_no=temp1;
		if(table_sym[temp1].type==STARINT)
		temp.u.assign.left->kind=STAR;
		else 
		temp.u.assign.left->kind=VARIABLE;
		temp.u.assign.right=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.right->u.var_no=temp2;
		if(table_sym[temp2].type==STARINT)
		temp.u.assign.right->kind=STAR;
		else 
		temp.u.assign.right->kind=VARIABLE;
		current=addcode(&temp,current);
	}
	else if(!strcmp(Exp->left->name,"ID")&&!strcmp(Exp->left->right->name,"LP")&&!strcmp(Exp->left->right->right->name,"RP"))
	{
		char* fun=Exp->left->value.IDname;
		if(!strcmp(fun,"read"))
		{
			ic temp;
			temp.kind=READ;
			temp.u.dec_val.dec_val=(Operand)malloc(sizeof(struct Operand_));
			temp.u.dec_val.dec_val->kind=VARIABLE;
			temp.u.dec_val.dec_val->u.var_no=topid;
			current=addcode(&temp,current);
		}
		else 
		{
			int funid=get_valid(fun,table_sym);
			ic temp;
			temp.kind=CALL;
			temp.u.assign.left=(Operand)malloc(sizeof(struct Operand_));
			temp.u.assign.left->kind=VARIABLE;
			temp.u.assign.left->u.var_no=topid;
			temp.u.assign.right=(Operand)malloc(sizeof(struct Operand_));
			temp.u.assign.right->kind=FUNCNAME;
			temp.u.assign.right->u.var_no=funid;
	//		printf("function call! function name:%s varid:%d \n",table_sym[funid].ID,topid);
			current=addcode(&temp,current);
		}
	}
	else if(!strcmp(Exp->left->name,"ID")&&!strcmp(Exp->left->right->name,"LP")&&!strcmp(Exp->left->right->right->name,"Args")&&!strcmp(Exp->left->right->right->right->name,"RP"))
	{
		char* fun=Exp->left->value.IDname;
		//printf("in Args!\n");
		if(!strcmp(fun,"write"))
		{
			current=translate_Args(Exp->left->right->right,current,table_sym,-2);
		}
		else {
			int funid=get_valid(fun,table_sym);
			current=translate_Args(Exp->left->right->right,current,table_sym,topid);
			ic temp;
			temp.kind=CALL;
			temp.u.assign.left=(Operand)malloc(sizeof(struct Operand_));
			temp.u.assign.left->kind=VARIABLE;
			temp.u.assign.left->u.var_no=topid;
			temp.u.assign.right=(Operand)malloc(sizeof(struct Operand_));
			temp.u.assign.right->kind=FUNCNAME;
			temp.u.assign.right->u.var_no=funid;
	//		printf("function call! function name:%s varid:%d \n",table_sym[funid].ID,topid);
			current=addcode(&temp,current);
		}
	}
	else if(!strcmp(Exp->left->name,"LP")&&!strcmp(Exp->left->right->name,"Exp")&&!strcmp(Exp->left->right->right->name,"RP"))
	{
		//printf("get here!\n");
		current=translate_Exp(Exp->left->right,current,table_sym,topid);
	}
	else if(!strcmp(Exp->left->name,"Exp")&&!strcmp(Exp->left->right->name,"PLUS")&&!strcmp(Exp->left->right->right->name,"Exp"))
	{
	//	printf("in plus!\n");
		int temp1=new_id(table_sym);
		ic temp;
		temp.kind=ADD;
		current=translate_Exp(Exp->left,current,table_sym,temp1);
		int temp2=new_id(table_sym);
		current=translate_Exp(Exp->left->right->right,current,table_sym,temp2);
		temp.u.binop.op1=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.op1->kind=VARIABLE;
		temp.u.binop.op1->u.var_no=temp1;
		temp.u.binop.op2=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.op2->kind=VARIABLE;
		temp.u.binop.op2->u.var_no=temp2;
		temp.u.binop.result=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.result->kind=VARIABLE;
		temp.u.binop.result->u.var_no=topid;
		if(table_sym[temp1].type!=ARRAY&&table_sym[temp2].type!=ARRAY)
		table_sym[topid].type=INT;
		else table_sym[topid].type=ARRAY;
	//	printf("recognize PLUS: v%d + v%d = v%d\n",temp1,temp2,topid);

		current=addcode(&temp,current);
	}
	else if(!strcmp(Exp->left->name,"Exp")&&!strcmp(Exp->left->right->name,"MINUS")&&!strcmp(Exp->left->right->right->name,"Exp"))
	{
	//	printf("in plus!\n");
		int temp1=new_id(table_sym);
		ic temp;
		temp.kind=SUB;
		current=translate_Exp(Exp->left,current,table_sym,temp1);
		int temp2=new_id(table_sym);
		current=translate_Exp(Exp->left->right->right,current,table_sym,temp2);
		temp.u.binop.op1=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.op1->kind=VARIABLE;
		temp.u.binop.op1->u.var_no=temp1;
		temp.u.binop.op2=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.op2->kind=VARIABLE;
		temp.u.binop.op2->u.var_no=temp2;
		temp.u.binop.result=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.result->kind=VARIABLE;
		temp.u.binop.result->u.var_no=topid;
		if(table_sym[temp1].type!=ARRAY&&table_sym[temp2].type!=ARRAY)
		table_sym[topid].type=INT;
		else table_sym[topid].type=ARRAY;
	//	printf("recognize MINUS: v%d - v%d = v%d\n",temp1,temp2,topid);
		current=addcode(&temp,current);
	}
	else if(!strcmp(Exp->left->name,"Exp")&&!strcmp(Exp->left->right->name,"STAR")&&!strcmp(Exp->left->right->right->name,"Exp"))
	{
	//	printf("in plus!\n");
		int temp1=new_id(table_sym);
		ic temp;
		temp.kind=MUL;
		current=translate_Exp(Exp->left,current,table_sym,temp1);
		int temp2=new_id(table_sym);
		current=translate_Exp(Exp->left->right->right,current,table_sym,temp2);
		temp.u.binop.op1=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.op1->kind=VARIABLE;
		temp.u.binop.op1->u.var_no=temp1;
		temp.u.binop.op2=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.op2->kind=VARIABLE;
		temp.u.binop.op2->u.var_no=temp2;
		temp.u.binop.result=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.result->kind=VARIABLE;
		temp.u.binop.result->u.var_no=topid;
		if(table_sym[temp1].type!=ARRAY&&table_sym[temp2].type!=ARRAY)
		table_sym[topid].type=INT;
		else table_sym[topid].type=ARRAY;
	//	printf("recognize STAR: v%d * v%d = v%d\n",temp1,temp2,topid);
		current=addcode(&temp,current);
	}
	else if(!strcmp(Exp->left->name,"Exp")&&!strcmp(Exp->left->right->name,"DIV")&&!strcmp(Exp->left->right->right->name,"Exp"))
	{
	//	printf("in plus!\n");
		int temp1=new_id(table_sym);
		ic temp;
		temp.kind=DIV;
		current=translate_Exp(Exp->left,current,table_sym,temp1);
		int temp2=new_id(table_sym);
		current=translate_Exp(Exp->left->right->right,current,table_sym,temp2);
		temp.u.binop.op1=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.op1->kind=VARIABLE;
		temp.u.binop.op1->u.var_no=temp1;
		temp.u.binop.op2=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.op2->kind=VARIABLE;
		temp.u.binop.op2->u.var_no=temp2;
		temp.u.binop.result=(Operand)malloc(sizeof(struct Operand_));
		temp.u.binop.result->kind=VARIABLE;
		temp.u.binop.result->u.var_no=topid;
		if(table_sym[temp1].type!=ARRAY&&table_sym[temp2].type!=ARRAY)
		table_sym[topid].type=INT;
		else table_sym[topid].type=ARRAY;
	//	printf("recognize DIV: v%d / v%d = v%d\n",temp1,temp2,topid);
		current=addcode(&temp,current);
	}
	else if(!strcmp(Exp->left->name,"NOT")||!strcmp(Exp->left->right->name,"AND")||!strcmp(Exp->left->right->name,"RELOP")||!strcmp(Exp->left->right->name,"OR"))
	{
	//	printf("in COND!\n");
		int label1= label++;	
		int label2= label++;
		ic temp;
		temp.kind=ASSIGN;
		temp.u.assign.left=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.left->kind=VARIABLE;
		temp.u.assign.left->u.var_no=topid;
		temp.u.assign.right=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.right->kind=CONSTANT;
		temp.u.assign.right->u.value_i=0;
		current=addcode(&temp,current);
		current=translate_Cond(Exp,current,label1,label2,table_sym);
		temp.kind=LABEL;
		temp.u.label.label_num=label1;
		current=addcode(&temp,current);
		temp.kind=ASSIGN;
		temp.u.assign.left=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.left->kind=VARIABLE;
		temp.u.assign.left->u.var_no=topid;
		temp.u.assign.right=(Operand)malloc(sizeof(struct Operand_));
		temp.u.assign.right->kind=CONSTANT;
		temp.u.assign.right->u.value_i=1;
		current=addcode(&temp,current);
		temp.kind=LABEL;
		temp.u.label.label_num=label2;
		current=addcode(&temp,current);
	}
	else if(!strcmp(Exp->left->name,"MINUS")&&!strcmp(Exp->left->right->name,"Exp"))
	{
		current=translate_Exp(Exp->left->right,current,table_sym,topid);
		ic temp;
		temp.kind=NEG;
		temp.u.neg.to_neg=(Operand)malloc(sizeof(struct Operand_));
		temp.u.neg.to_neg->kind=VARIABLE;
		temp.u.neg.to_neg->u.var_no=topid;
		current=addcode(&temp,current);
	}
	else if(!strcmp(Exp->left->name,"Exp")&&!strcmp(Exp->left->right->name,"LB"))
	{
		translate_Array(Exp,current,table_sym,topid);
	}
	return current;
}

ics* translate_Def(node* Def,ics*current,sym* table_sym)
{
	//printf("in Def!\n");
	node* DecList=Def->left->right;
	while(1)
	{
		if(DecList->left->left->left->right&&!strcmp(DecList->left->left->left->right->name,"LB"))
		{
			//ARRAY
			int dec_id=new_id(table_sym);
			node* start=DecList->left->left;
			//VarDec LB INT RB
			int size=1;
			table_sym[dec_id].node=(an*)malloc(sizeof(an));
			table_sym[dec_id].node->next=NULL;
			an* cur=table_sym[dec_id].node;
			while(start->left->right)
			{
				int val=start->left->right->right->value.ivalue;
				size*=val;
				start=start->left;
				cur->size=size;
				cur->next=(an*)malloc(sizeof(current));
				cur=cur->next;
			}
			table_sym[dec_id].ID=start->left->value.IDname;
			table_sym[dec_id].type=ARRAY;
			ic temp;
			temp.kind=DECADDR;
			temp.u.dec_addr.size=size;
			temp.u.dec_addr.valid=dec_id;
	//		printf("find array v%d size %d",dec_id,size);
	//		table_sym[dec_id].offset=offsetnow;
			offsetnow+=size*4;
			current=addcode(&temp,current);
		}
		else if(!DecList->left->left->right)//no assign
		{
			int dec_id=new_id(table_sym);
			//printf("get node!\n");
			table_sym[dec_id].ID=DecList->left->left->left->value.IDname;
			//printf("get id\n");
			if(!strcmp(Def->left->left->value.IDname,"int"))
			{
				table_sym[dec_id].type=INT;
			}
			else
			{
				table_sym[dec_id].type=FLOAT;
			}
		}
		else 
		{
			//to be continue
			//Exp assigned Exp
			int dec_id=new_id(table_sym);
			//printf("get node!\n");
			table_sym[dec_id].ID=DecList->left->left->left->value.IDname;
			//printf("get id\n");
			if(!strcmp(Def->left->left->value.IDname,"int"))
			{
				table_sym[dec_id].type=INT;
			}
			else
			{
				table_sym[dec_id].type=FLOAT;
			}
			ic temp;
			temp.kind=ASSIGN;
			int temp1=new_id(table_sym);
			current=translate_Exp(DecList->left->left->right->right,current,table_sym,temp1);
			temp.u.assign.right=(Operand)malloc(sizeof(struct Operand_));
			temp.u.assign.right->kind=VARIABLE;
			temp.u.assign.right->u.var_no=temp1;
			temp.u.assign.left=(Operand)malloc(sizeof(struct Operand_));
			temp.u.assign.left->kind=VARIABLE;
			temp.u.assign.left->u.var_no=dec_id;
			table_sym[dec_id].type=table_sym[temp1].type;
			printf("recognize IDname %s, id: %d\n",DecList->left->left->value.IDname,dec_id);
			current=addcode(&temp,current);
		}
		if(DecList->left->right)
		{
			DecList=DecList->left->right->right;
		}
		else break;
	}
	return current;
}
ics* translate_DefList(node*DefList,ics*current,sym*table_sym)
{
//	printf("in DefList!\n");
	current=translate_Def(DefList->left,current,table_sym);
	if(DefList->left->right)
	{
		current=translate_DefList(DefList->left->right,current,table_sym);
	}
	return current;
}

ics* translate_StmtList(node* StmtList,ics* current,sym* table_sym)
{
//	printf("in StmtList!\n");
	current=translate_Stmt(StmtList->left,current,table_sym);
	if(StmtList->left->right)
	{
		current=translate_StmtList(StmtList->left->right,current,table_sym);
	}
	//printf("normal return !\n");
	return current;
}

ics* translate_CompSt(node*CompSt,ics*current,sym* table_sym)
{
	//printf("in CompSt! name: %s\n",CompSt->name);
	if(!strcmp(CompSt->left->right->name,"DefList"))
	{
		//printf("in CompSt!\n");
		current=translate_DefList(CompSt->left->right,current,table_sym);
		if(!strcmp(CompSt->left->right->right->name,"StmtList"))
		current=translate_StmtList(CompSt->left->right->right,current,table_sym);
	}
	else {
		if(!strcmp(CompSt->left->right->name,"StmtList"))
		{
			current=translate_StmtList(CompSt->left->right,current,table_sym);
		}
	}
//	printf("exit CompSt!\n");
	return current;
}
ics* translate_Stmt(node* Stmt,ics* current,sym* table_sym)
{
//	printf("in Stmt!\n");
	if(!strcmp(Stmt->left->name,"CompSt"))
	{
		current=translate_CompSt(Stmt->left,current,table_sym);
	}
	else if(!strcmp(Stmt->left->name,"Exp")&&!strcmp(Stmt->left->right->name,"SEMI"))
	{
		current=translate_Exp(Stmt->left,current,table_sym,-1);//use -1 to mark NULL
	}
	else if(!strcmp(Stmt->left->name,"RETURN")&&!strcmp(Stmt->left->right->name,"Exp")&&!strcmp(Stmt->left->right->right->name,"SEMI"))
	{
		//printf("in return!\n");
		int temp=new_id(table_sym);
		current=translate_Exp(Stmt->left->right,current,table_sym,temp);
		ic retcode;
		retcode.kind=RETURN;
		retcode.u.dec_val.dec_val=(Operand)malloc(sizeof(struct Operand_));
		retcode.u.dec_val.dec_val->kind=VARIABLE;
		retcode.u.dec_val.dec_val->u.var_no=temp;
		//printf("recognize return!\n");
		current=addcode(&retcode,current);
	}
	else if(!strcmp(Stmt->left->name,"IF")&&!Stmt->left->right->right->right->right->right)
	{
//		printf("IF\n");
		int label1=label++;
		int label2=label++;
		current=translate_Cond(Stmt->left->right->right,current,label1,label2,table_sym);
		ic temp1;
		temp1.kind=LABEL;
		temp1.u.label.label_num=label1;
		current=addcode(&temp1,current);
		current=translate_Stmt(Stmt->left->right->right->right->right,current,table_sym);
		ic temp2;
		temp2.kind=LABEL;
		temp2.u.label.label_num=label2;
		current=addcode(&temp2,current);
	}
	else if(!strcmp(Stmt->left->name,"IF"))
	{
//		printf("IF_ELSE!\n");
		int label1=label++;
		int label2=label++;
		int label3=label++;
		current=translate_Cond(Stmt->left->right->right,current,label1,label2,table_sym);
		ic temp1;
		temp1.kind=LABEL;
		temp1.u.label.label_num=label1;
		current=addcode(&temp1,current);
		current=translate_Stmt(Stmt->left->right->right->right->right,current,table_sym);
		ic temp2;
		temp2.kind=GOTO;
		temp2.u.label.label_num=label3;
		current=addcode(&temp2,current);
		ic temp3;
		temp3.kind=LABEL;
		temp3.u.label.label_num=label2;
		current=addcode(&temp3,current);
		current=translate_Stmt(Stmt->left->right->right->right->right->right->right,current,table_sym);
		ic temp4;
		temp4.kind=LABEL;
		temp4.u.label.label_num=label3;
		current=addcode(&temp4,current);	
	}
	else if(!strcmp(Stmt->left->name,"WHILE"))
	{
		int label1=label++;
		int label2=label++;
		int label3=label++;
		ic temp1;
		temp1.kind=LABEL;
		temp1.u.label.label_num=label1;
		current=addcode(&temp1,current);
		current=translate_Cond(Stmt->left->right->right,current,label2,label3,table_sym);
		ic temp2;
		temp2.kind=LABEL;
		temp2.u.label.label_num=label2;
		current=addcode(&temp2,current);
		current=translate_Stmt(Stmt->left->right->right->right->right,current,table_sym);
		ic temp3;
		temp3.kind=GOTO;
		temp3.u.label.label_num=label1;
		current=addcode(&temp3,current);
		ic temp4;
		temp4.kind=LABEL;
		temp4.u.label.label_num=label3;
		current=addcode(&temp4,current);	
	}
	//else : if-else while
	return current;
}
void printcode(ics* head,sym* table_sym)
{
	//printf("now start to print code!\n");
	ics* current=head;
//	printf("writing address: %s",addrout);
	FILE * out =fopen(addrout,"w");
	fprintf(out,".data\n");
	fprintf(out,"_prompt: .asciiz \"Enter an integer:\"\n_ret: .asciiz \"\\n\"\n");
	fprintf(out,".globl main\n.text\n");
	fprintf(out,"read:\nli $v0, 4\nla $a0, _prompt\nsyscall\nli $v0, 5\nsyscall\njr $ra\n\n");
	fprintf(out,"write:\nli $v0, 1\n,syscall\nli $v0, 4\nla $a0, _ret\nsyscall\nmove $v0, $0\njr $ra\n\n");
	while(current)
	{
		//printf("lineno:");
		if(current->code.kind==DEC)
		{
		//	printf("FUNCTION %s\n",table_sym[current->code.u.dec_val.dec_val->u.var_no].ID);
			fprintf(out,"%s:\n",table_sym[current->code.u.dec_val.dec_val->u.var_no].ID);
			if(!strcmp(table_sym[current->code.u.dec_val.dec_val->u.var_no].ID,"main"))
			{
				fprintf(out,"li $t1, 4\n");
				fprintf(out,"sw $t1,0($sp)\n");
			}
		}
		else if(current->code.kind==PARAM)
		{
		//	printf("PARAM v%d\n",current->code.u.dec_val.dec_val->u.var_no);
		//	fprintf(out,"PARAM v%d\n",current->code.u.dec_val.dec_val->u.var_no);
		}
		else if(current->code.kind==RETURN)
		{
			if(current->code.u.dec_val.dec_val->kind==VARIABLE)
			{
		//		printf("RETURN v%d\n",current->code.u.dec_val.dec_val->u.var_no);			
				fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.dec_val.dec_val->u.var_no].offset);
				fprintf(out,"move $v0, $t1\n");
				fprintf(out,"jr $ra\n");
				//fprintf(out,"RETURN v%d\n",current->code.u.dec_val.dec_val->u.var_no);
        }
			else if(current->code.u.dec_val.dec_val->kind==CONSTANT)
			{
				//now will not be used 
			}
		}
		else if(current->code.kind==NEG)
		{
			fprintf(out,"li $t1, 0\n");
			fprintf(out,"lw $t2, %d($sp)\n",table_sym[current->code.u.neg.to_neg->u.var_no].offset);
			fprintf(out,"sub $t2, $t1, $t2\n");
			fprintf(out,"sw $t2, %d($sp)\n",table_sym[current->code.u.neg.to_neg->u.var_no].offset);
		//	fprintf(out,"v%d := #0 - v%d\n",current->code.u.neg.to_neg->u.var_no,current->code.u.neg.to_neg->u.var_no);
		}
		else if(current->code.kind==ASSIGN)
		{
			if(current->code.u.assign.right->kind==CONSTANT)
			{
				if(table_sym[current->code.u.assign.left->u.var_no].type==INT)
				{
					//printf("v%d := #%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.value_i);
					fprintf(out,"li $t1, %d\n",current->code.u.assign.right->u.value_i);
					fprintf(out,"sw $t1, %d($sp)\n",table_sym[current->code.u.assign.left->u.var_no].offset);
					//fprintf(out,"v%d := #%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.value_i);
				}
				else
				{
					//printf("*v%d := #%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.value_i);
					fprintf(out,"li $t1, %d\n",current->code.u.assign.right->u.value_i);
					fprintf(out,"lw $t2, %d($sp)\n",table_sym[current->code.u.assign.left->u.var_no].offset);
					fprintf(out,"sw $t1, 0($t2)\n");
					//fprintf(out,"*v%d := #%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.value_i);			
				}
			}
			else if(current->code.u.assign.right->kind==VARIABLE)
			{
				if(table_sym[current->code.u.assign.left->u.var_no].type==INT)
				{
					//printf("v%d := v%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.var_no);
					fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.assign.right->u.var_no].offset);
					fprintf(out,"sw $t1, %d($sp)\n",table_sym[current->code.u.assign.left->u.var_no].offset);

					//fprintf(out,"v%d := v%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.var_no);
				
				}
				else
				{
					//printf("*v%d := v%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.var_no);
					fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.assign.right->u.var_no].offset);
					fprintf(out,"lw $t2, %d($sp)\n",table_sym[current->code.u.assign.left->u.var_no].offset);
					fprintf(out,"sw $t1, 0($t2)\n");
					//fprintf(out,"*v%d := v%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.var_no);			
				}
			}
			else
			{
				if(table_sym[current->code.u.assign.left->u.var_no].type==INT)
				{
					//printf("v%d := *v%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.var_no);
					fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.assign.right->u.var_no].offset);
					fprintf(out,"lw $t1, 0($t1)\n");
					fprintf(out,"sw $t1, %d($sp)\n",table_sym[current->code.u.assign.left->u.var_no].offset);
					//fprintf(out,"sw $t2, 0($t1)\n");
					//fprintf(out,"v%d := *v%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.var_no);
				
				}
				else
				{
					fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.assign.right->u.var_no].offset);
					fprintf(out,"lw $t2, %d($sp)\n",table_sym[current->code.u.assign.left->u.var_no].offset);
					fprintf(out,"lw $t3, 0($t1)\n");
					fprintf(out,"sw $t3, 0($t2)\n");
					//printf("*v%d := *v%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.var_no);
					//fprintf(out,"*v%d := *v%d\n",current->code.u.assign.left->u.var_no,current->code.u.assign.right->u.var_no);			
				}
			}
			
		}
		else if(current->code.kind==ADD)
		{
			fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.binop.op1->u.var_no].offset);
			if(table_sym[current->code.u.binop.op1->u.var_no].type==STARINT)
			fprintf(out,"lw $t1, 0($t1)\n");
			//fprintf(out,"v%d := ",current->code.u.binop.result->u.var_no);
			fprintf(out,"lw $t2, %d($sp)\n",table_sym[current->code.u.binop.op2->u.var_no].offset);
			if(table_sym[current->code.u.binop.op2->u.var_no].type==STARINT)
			fprintf(out,"lw $t2, 0($t2)\n");
			fprintf(out,"add $t3, $t1, $t2\n");
			if(table_sym[current->code.u.binop.result->u.var_no].type==STARINT)
			{
				fprintf(out,"lw $t4, %d($sp)\n",table_sym[current->code.u.binop.result->u.var_no].offset);
				fprintf(out,"sw $t3, 0($t4)\n");
			}
			else
			{
				fprintf(out,"sw $t3, %d($sp)\n",table_sym[current->code.u.binop.result->u.var_no].offset);
			}
			
		}
		else if(current->code.kind==SUB)
		{
			fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.binop.op1->u.var_no].offset);
			if(table_sym[current->code.u.binop.op1->u.var_no].type==STARINT)
			fprintf(out,"lw $t1, 0($t1)\n");
			//fprintf(out,"v%d := ",current->code.u.binop.result->u.var_no);
			fprintf(out,"lw $t2, %d($sp)\n",table_sym[current->code.u.binop.op2->u.var_no].offset);
			if(table_sym[current->code.u.binop.op2->u.var_no].type==STARINT)
			fprintf(out,"lw $t2, 0($t2)\n");
			fprintf(out,"sub $t3, $t1, $t2\n");
			if(table_sym[current->code.u.binop.result->u.var_no].type==STARINT)
			{
				fprintf(out,"lw $t4, %d($sp)\n",table_sym[current->code.u.binop.result->u.var_no].offset);
				fprintf(out,"sw $t3, 0($t4)\n");
			}
			else
			{
				fprintf(out,"sw $t3, %d($sp)\n",table_sym[current->code.u.binop.result->u.var_no].offset);
			}
		}	
		else if(current->code.kind==MUL)
		{
			fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.binop.op1->u.var_no].offset);
			if(table_sym[current->code.u.binop.op1->u.var_no].type==STARINT)
			fprintf(out,"lw $t1, 0($t1)\n");
			//fprintf(out,"v%d := ",current->code.u.binop.result->u.var_no);
			fprintf(out,"lw $t2, %d($sp)\n",table_sym[current->code.u.binop.op2->u.var_no].offset);
			if(table_sym[current->code.u.binop.op2->u.var_no].type==STARINT)
			fprintf(out,"lw $t2, 0($t2)\n");
			fprintf(out,"mul $t3, $t1, $t2\n");
			if(table_sym[current->code.u.binop.result->u.var_no].type==STARINT)
			{
				fprintf(out,"lw $t4, %d($sp)\n",table_sym[current->code.u.binop.result->u.var_no].offset);
				fprintf(out,"sw $t3, 0($t4)\n");
			}
			else
			{
				fprintf(out,"sw $t3, %d($sp)\n",table_sym[current->code.u.binop.result->u.var_no].offset);
			}
		}
		else if(current->code.kind==DIV)
		{
			fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.binop.op1->u.var_no].offset);
			if(table_sym[current->code.u.binop.op1->u.var_no].type==STARINT)
			fprintf(out,"lw $t1, 0($t1)\n");
			//fprintf(out,"v%d := ",current->code.u.binop.result->u.var_no);
			fprintf(out,"lw $t2, %d($sp)\n",table_sym[current->code.u.binop.op2->u.var_no].offset);
			if(table_sym[current->code.u.binop.op2->u.var_no].type==STARINT)
			fprintf(out,"lw $t2, 0($t2)\n");
			fprintf(out,"div $t1, $t2\n");
			fprintf(out,"mflo $t3\n");
			if(table_sym[current->code.u.binop.result->u.var_no].type==STARINT)
			{
				fprintf(out,"lw $t4, %d($sp)\n",table_sym[current->code.u.binop.result->u.var_no].offset);
				fprintf(out,"sw $t3, 0($t4)\n");
			}
			else
			{
				fprintf(out,"sw $t3, %d($sp)\n",table_sym[current->code.u.binop.result->u.var_no].offset);
			}
		}
		else if(current->code.kind==READ)
		{
			fprintf(out,"addi $sp, $sp, -%d\n",offsetnow);
			fprintf(out,"sw $ra, 0($sp)\n");
			fprintf(out,"jal read\n");
			fprintf(out,"lw $ra,0($sp)\n");
			fprintf(out,"addi $sp, $sp, %d\n",offsetnow);
			fprintf(out,"sw $v0, %d($sp)\n",table_sym[current->code.u.dec_val.dec_val->u.var_no].offset);
		}
		else if(current->code.kind==WRITE)
		{
			if(table_sym[current->code.u.args.args_num].type==STARINT)
			{
				fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.args.args_num].offset);
				fprintf(out,"lw $a0, 0($t1)\n");
				fprintf(out,"addi $sp, $sp, -%d\n",offsetnow);
				fprintf(out,"sw $ra, 0($sp)\n");
				fprintf(out,"jal write\n");
				fprintf(out,"lw $ra,0($sp)\n");
				fprintf(out,"addi $sp, $sp, %d\n",offsetnow);
				//fprintf(out,"WRITE *v%d\n",current->code.u.args.args_num);
			}
			else{
				fprintf(out,"lw $a0, %d($sp)\n",table_sym[current->code.u.args.args_num].offset);
		//		fprintf(out,"lw $a0, 0($t1)\n");
				fprintf(out,"addi $sp, $sp, -%d\n",offsetnow);
				fprintf(out,"sw $ra, 0($sp)\n");
				fprintf(out,"jal write\n");
				fprintf(out,"lw $ra,0($sp)\n");
				fprintf(out,"addi $sp, $sp, %d\n",offsetnow);
			}
		}
		else if(current->code.kind==ARGS)
		{
		//	printf("ARG v%d\n",current->code.u.args.args_num);
			int valnum=0;
			ics * temp=current;
			//fprintf(out,"ARG v%d\n",current->code.u.args.args_num);
			while(temp->code.kind==ARGS)
			{
				temp=temp->next;
				valnum++;
			}
		//	printf("%d\n",temp->code.kind);
			char* funcname=table_sym[temp->code.u.assign.right->u.var_no].ID;
			temp=head;
		//	printf("%s\n",funcname);
			while(temp->code.kind!=DEC ||strcmp(table_sym[temp->code.u.dec_val.dec_val->u.var_no].ID,funcname))
			{
				temp=temp->next;
			}
			//printf("find the temp!\n");
			//printf("%d\n",valnum);
			while(valnum--)
			{
				temp=temp->next;
			}
			while(current->code.kind==ARGS)
			{
				fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.args.args_num].offset);
				fprintf(out,"sw $t1, %d($sp)\n",table_sym[temp->code.u.dec_val.dec_val->u.var_no].offset);
//				printf("arg success!\n");
				current=current->next;
				temp=temp->prev;
			}
			current=current->prev;
	}
		else if(current->code.kind==GOTO)
		{
			//printf("GOTO label%d\n",current->code.u.label.label_num);
			fprintf(out,"j label%d\n",current->code.u.label.label_num);
		}
		else if(current->code.kind==LABEL)
		{
			//printf("LABEL label%d :\n",current->code.u.label.label_num);
			fprintf(out,"label%d:\n",current->code.u.label.label_num);
		}
		else if(current->code.kind==CALL)
		{
			//printf("v%d := CALL %s\n",current->code.u.assign.left->u.var_no,table_sym[current->code.u.assign.right->u.var_no].ID);
			fprintf(out,"lw $t1, 0($sp)\n");
			fprintf(out,"li $t2, 4\n");
			fprintf(out,"mul $t2, $t2, $t1\n");
			//fprintf(out,"addi $t1, %d",offsetnow);
			fprintf(out,"la $t3, %d($sp)\n",offsetnow);
			fprintf(out,"add $t3, $t2, $t3\n");
			fprintf(out,"sw $ra, ($t3)\n");
			fprintf(out,"addi $t1, $t1, 1\n");
			fprintf(out,"sw $t1, 0($sp)\n");
			fprintf(out,"jal %s\n",table_sym[current->code.u.assign.right->u.var_no].ID);
			//printf("v%d := CALL %s\n",current->code.u.assign.left->u.var_no,table_sym[current->code.u.assign.right->u.var_no].ID);
			fprintf(out,"lw $t1, 0($sp)\n");
			fprintf(out,"addi $t1, $t1, -1\n");
			fprintf(out,"sw $t1, 0($sp)\n");
			fprintf(out,"li $t2, 4\n");
			fprintf(out,"mul $t2, $t2, $t1\n");
			//fprintf(out,"addi $t1, %d",offsetnow);
			fprintf(out,"la $t3, %d($sp)\n",offsetnow);
			fprintf(out,"add $t3, $t2, $t3\n");
			fprintf(out,"lw $ra, ($t3)\n");
			if(table_sym[current->code.u.assign.left->u.var_no].type==STARINT)
			{
				fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.assign.left->u.var_no].offset);
				fprintf(out,"sw $v0, 0($t1)\n");
			//	fprintf(out,"v%d := CALL %s\n",current->code.u.assign.left->u.var_no,table_sym[current->code.u.assign.right->u.var_no].ID);
			}
			else
			{
				fprintf(out,"sw $v0, %d($sp)\n",table_sym[current->code.u.assign.left->u.var_no].offset);
			}
			
		}
		else if(current->code.kind==COND_N)
		{
			if(table_sym[current->code.u.cond_nval.t1].type==STARINT)
			{
				fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.cond_nval.t1].offset);
				fprintf(out,"lw $t1, 0($t1)\n");
			}
			else
			{
				fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.cond_nval.t1].offset);
			}
			if(table_sym[current->code.u.cond_nval.t2].type==STARINT)
			{
				fprintf(out,"lw $t2, %d($sp)\n",table_sym[current->code.u.cond_nval.t2].offset);
				fprintf(out,"lw $t2, 0($t2)\n");
			}
			else
			{
				fprintf(out,"lw $t2, %d($sp)\n",table_sym[current->code.u.cond_nval.t2].offset);
			}
			if(!strcmp(current->code.u.cond_nval.op,"=="))
			{
				fprintf(out,"beq");
			}
			else if(!strcmp(current->code.u.cond_nval.op,"!="))
			{
				fprintf(out,"bne");
			}
			else if(!strcmp(current->code.u.cond_nval.op,">"))
			{
				fprintf(out,"bgt");
			}
			else if(!strcmp(current->code.u.cond_nval.op,"<"))
			{
				fprintf(out,"blt");
			}
			else if(!strcmp(current->code.u.cond_nval.op,">="))
			{
				fprintf(out,"bge");
			}
			else if(!strcmp(current->code.u.cond_nval.op,"<="))
			{
				fprintf(out,"ble");
			}
			//fprintf(out,"%s ",current->code.u.cond_nval.op);
			fprintf(out," $t1, $t2, ");
//			fprintf(out,"v%d",current->code.u.cond_nval.t2);
			//printf("IF v%d %s v%d GOTO label%d\nGOTO label%d\n",current->code.u.cond_nval.t1,current->code.u.cond_nval.op,current->code.u.cond_nval.t2,current->code.u.cond_nval.lt,current->code.u.cond_nval.lf);
			fprintf(out,"label%d\nj label%d\n",current->code.u.cond_nval.lt,current->code.u.cond_nval.lf);
		}
		else if(current->code.kind==COND_O)
		{
			//printf("IF v%d != #0 GOTO label%d\nGOTO label%d\n",current->code.u.cond_oval.t1,current->code.u.cond_oval.lt,current->code.u.cond_oval.lf);
			if(table_sym[current->code.u.cond_nval.t1].type==STARINT)
			{
				fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.cond_oval.t1].offset);
				fprintf(out,"lw $t1, 0($t1)\n");
			}
			else
			{
				fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.cond_oval.t1].offset);
			}
			fprintf(out,"li $t2, 0\n");
			fprintf(out,"bne $t1, $t2 ");
			fprintf(out,"label%d\nj label%d\n",current->code.u.cond_oval.lt,current->code.u.cond_oval.lf);
		}
		else if(current->code.kind==DECADDR)
		{
			//printf("DEC v%d %d\n",current->code.u.dec_addr.valid,current->code.u.dec_addr.size*4);
			//fprintf(out,"DEC v%d %d\n",current->code.u.dec_addr.valid,current->code.u.dec_addr.size*4);
			
		}
		else if(current->code.kind==ADDR)
		{
			if(table_sym[current->code.u.addr.right->u.var_no].type==FUNARRAY)
			{
				//printf("v%d := v%d * #4\n",current->code.u.addr.bias->u.var_no,current->code.u.addr.bias->u.var_no);
				//printf("v%d := v%d + v%d\n",current->code.u.addr.left->u.var_no,current->code.u.addr.right->u.var_no,current->code.u.addr.bias->u.var_no);
				fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.addr.bias->u.var_no].offset);
				fprintf(out,"li $t2, 4\n");
				fprintf(out,"mul $t1, $t1, $t2\n");
				//fprintf(out,"v%d := v%d * #4\n",current->code.u.addr.bias->u.var_no,current->code.u.addr.bias->u.var_no);
				fprintf(out,"lw $t3, %d($sp)\n",table_sym[current->code.u.addr.right->u.var_no].offset);
				fprintf(out,"add $t4, $t3, $t1\n");
				fprintf(out,"sw $t4, %d($sp)\n",table_sym[current->code.u.addr.left->u.var_no].offset);
				//fprintf(out,"v%d := v%d + v%d\n",current->code.u.addr.left->u.var_no,current->code.u.addr.right->u.var_no,current->code.u.addr.bias->u.var_no);
			
			}
			else{
					//printf("v%d := v%d * #4\n",current->code.u.addr.bias->u.var_no,current->code.u.addr.bias->u.var_no);
					//printf("v%d := &v%d + v%d\n",current->code.u.addr.left->u.var_no,current->code.u.addr.right->u.var_no,current->code.u.addr.bias->u.var_no);
					fprintf(out,"lw $t1, %d($sp)\n",table_sym[current->code.u.addr.bias->u.var_no].offset);
					fprintf(out,"li $t2, 4\n");
					fprintf(out,"mul $t1, $t1, $t2\n");
					//fprintf(out,"v%d := v%d * #4\n",current->code.u.addr.bias->u.var_no,current->code.u.addr.bias->u.var_no);
					fprintf(out,"la $t3, %d($sp)\n",table_sym[current->code.u.addr.right->u.var_no].offset);
					fprintf(out,"add $t4, $t3, $t1\n");
					fprintf(out,"sw $t4, %d($sp)\n",table_sym[current->code.u.addr.left->u.var_no].offset);
				}
		}
		current=current->next;
	}
	fclose(out);
}
ics* translate(node*head,ics* current,sym* table_sym)
{
	if(head==NULL)
		return current;
	else if(!strcmp(head->name,"FunDec"))
	{
		current=translate_func(head,table_sym,current);
//		printf("translate end!\n");
	}
	else{
		//printf("now is %s\n",head->name);
		current=translate(head->left,current,table_sym);
		current=translate(head->right,current,table_sym);
	}
	if(!strcmp(head->name,"Program"))
	{	
		printcode(current,table_sym);
		exit(0);
	}
	return current;
}

int gencode(node* head)
{
	//preorder to find if struct exist
	sym table_sym[MAX_SYM];
	for(int i=0;i<MAX_SYM;i++)
	{
		table_sym[i].type=UNUSED;
		table_sym[i].ID=NULL;
	}
	int strnum=checkstr(head);
	if(strnum>0)
	{	printf("Cannot translate: Code contains variables or parameters of structure type.\n");
		//printf("struct number:%d\n",strnum);
	}
	else //printf("this code can be translate!\n");
	{
		//start to translate
		ics* first=NULL;
		translate(head,first,table_sym);
		//printf("translate complete!\n");
		//printf("%d\n",first->code.kind);
		//printcode(first,table_sym);
	}
	return 0;
}
