#include<stdio.h>
/*extern FILE* yyin;
int main(int argc, char** argv) {
	if (argc > 1) {
		if (!(yyin = fopen(argv[1], "r"))) {
		perror(argv[1]);
		return 1;
		}
	}
	while (yylex() != 0);
	return 0;
*/
extern int yydebug;
char* addrout;
int main(int argc, char** argv)
{
	if (argc <= 1) return 1;
	FILE* f = fopen(argv[1], "r");
	addrout = argv[2];
	if (!f)
	{
		perror(argv[1]);
		return 1;
	}
	yyrestart(f);
//	yydebug=1;
	yyparse();
	return 0;
}
