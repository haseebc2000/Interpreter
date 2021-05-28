#include "parserInt.h"
using namespace std;
map <string,bool> defVar;
map <string,Token> SymTable;
map <string,Value> TempsResults;
queue <Value> * ValQue; 
namespace Parser{
	bool pushed_back=false;
    LexItem	pushed_token;
	static LexItem GetNextToken(istream& in,int& line){
		if(pushed_back){
            pushed_back=false;
            return pushed_token;
            }
            return getNextToken(in,line);
            }
	static void PushBackToken(LexItem &t){
        if(pushed_back) 
        abort();
        pushed_back=true;
        pushed_token=t;}
        }
static int error_count=0;
int ErrCount(){
    return error_count;
    }
void ParseError(int line,string msg){
    ++error_count;
    cout<<line<<": "<<msg<<"\n";
    }
bool Prog(istream& in,int& line){
	bool dl=false,sl=false;
    LexItem tok=Parser::GetNextToken(in,line);
    string progname;
	if (tok.GetToken()==PROGRAM){
        tok=Parser::GetNextToken(in,line);
		if (tok.GetToken()==IDENT){
            progname=tok.GetLexeme();
            dl=Decl(in,line);
			if(!dl){
                ParseError(line,"Incorrect Declaration in Program");
                return false;
                }
            sl=Stmt(in,line);
			if(!sl){
                ParseError(line,"Incorrect Statement in program");
                return false;
                }
            tok=Parser::GetNextToken(in,line);
			if (tok.GetToken()==END){
                tok=Parser::GetNextToken(in,line);
				if (tok.GetToken()==PROGRAM){
                    tok=Parser::GetNextToken(in,line);
                    if (tok.GetToken()==IDENT){
						if (tok.GetLexeme() !=progname){
                            ParseError(line,"Incorrect Program Name");
                            return false;
                            }
                            return true;
                            }
					else{
                        ParseError(line,"Missing Program Name");
                        }
                        }
				else{
                    ParseError(line,"Missing PROGRAM at the End");
                    }
                    }
			else{
                ParseError(line,"Missing END of Program");
                }	
                }
                }
	    else if(tok.GetToken()==ERR)
        {
		ParseError(line,"Unrecognized Input Pattern");
		cout<<"("<<tok.GetLexeme()<<")\n";
        } 
        return false;
        }
bool Decl(istream& in,int& line){
	bool status=false; LexItem tok; LexItem t=Parser::GetNextToken(in,line);
	if(t==INTEGER||t==REAL||t==CHAR){
		tok=Parser::GetNextToken(in,line);
		if (tok.GetToken()==COLON){status=IdList(in,line,t);
			if (status){status=Decl(in,line);return status;}}
		else{ParseError(line,"Missing Colon"); return false;}}
	Parser::PushBackToken(t); return true;}
bool Stmt(istream& in,int& line){
	bool status; LexItem t=Parser::GetNextToken(in,line); switch(t.GetToken()){
	case PRINT: status=PrintStmt(in,line); if(status) status=Stmt(in,line); break;
	case IF: status=IfStmt(in,line); if(status) status=Stmt(in,line); break;
	case IDENT: Parser::PushBackToken(t); status=AssignStmt(in,line); if(status) status=Stmt(in,line); break;
	case READ: status=ReadStmt(in,line); if(status) status=Stmt(in,line); break;
	default: Parser::PushBackToken(t);return true;} return status;}
bool PrintStmt(istream& in,int& line){ 
	LexItem t; ValQue=new queue<Value>;
	if((t=Parser::GetNextToken(in,line)) !=COMA){ ParseError(line,"Missing a Comma"); return false;}
	bool ex=ExprList(in,line);
	if(!ex){ ParseError(line,"Missing expression after print");
		while(!(*ValQue).empty())ValQue -> pop(); delete ValQue; return false;}
	while(!(*ValQue).empty()){ Value nextVal=(*ValQue).front(); cout<<nextVal; ValQue -> pop();}
	cout<<"\n"; return ex;}
bool IfStmt(istream& in,int& line){
	Value val; bool ex=false ;LexItem t; val.SetType(VBOOL);
	if((t=Parser::GetNextToken(in,line)) !=LPAREN){
		ParseError(line,"Missing Left Parenthesis"); return false;}
	ex=LogicExpr(in,line,val);
	if(!ex){ParseError(line,"Missing if statement Logic Expression"); return false;}
	if((t=Parser::GetNextToken(in,line)) !=RPAREN){
		ParseError(line,"Missing Right Parenthesis"); return false;}
	if((t=Parser::GetNextToken(in,line)) !=THEN){ParseError(line,"Missing THEN"); return false;}
	if (val.GetBool()==true){ bool st=Stmt(in,line);
		if(!st){ ParseError(line,"Missing statement for IF"); return false;}}
	else{ while (t !=END) t=Parser::GetNextToken(in,line); Parser::PushBackToken(t);}
	if((t=Parser::GetNextToken(in,line)) !=END){
		ParseError(line,"Missing END of IF"); return false;}
	if((t=Parser::GetNextToken(in,line)) !=IF){
		ParseError(line,"Missing IF at End of IF statement"); return false;} return true;}
bool ReadStmt(istream& in,int& line){
	LexItem t; if((t=Parser::GetNextToken(in,line))!=COMA){ParseError(line,"Missing a Comma"); return false;}
	bool ex=VarList(in,line); if(!ex){ParseError(line,"Missing Variable after Read Statement"); return false;} return ex;}
bool IdList(istream& in,int& line,LexItem& tok){
	bool status=false; string identstr; LexItem t=Parser::GetNextToken(in,line);
	if(t==IDENT){ identstr=t.GetLexeme(); if (!(defVar.find(identstr)->second)){
			defVar[identstr]=true; SymTable[identstr]=tok.GetToken();}	
		else{ParseError(line,"Variable Redefinition");return false;}}
	else{ParseError(line,"Missing Variable");return false;}
	t=Parser::GetNextToken(in,line); if (t==COMA){status=IdList(in,line,tok);}
	else if(t.GetToken()==ERR){
		ParseError(line,"Unrecognized Input Pattern");
		cout<<"("<<t.GetLexeme()<<")\n"; return false;}
	else{Parser::PushBackToken(t);return true;} return status;}
bool VarList(istream& in,int& line){
	LexItem t; bool status=false; status=Var(in,line,t); string identstr;
	if(!status){ParseError(line,"Missing Variable");return false;}
	LexItem tok=Parser::GetNextToken(in,line); if (tok==COMA)status=VarList(in,line);
	else if(tok.GetToken()==ERR){ ParseError(line,"Unrecognized Input Pattern");
		cout<<"("<<tok.GetLexeme()<<")\n"; return false;}
	else{Parser::PushBackToken(tok);return true;} return status;}
bool Var(istream& in,int& line,LexItem& tok){
	string identstr; LexItem t=Parser::GetNextToken(in,line); tok=t;
	if (t==IDENT){ identstr=t.GetLexeme(); if (!(defVar.find(identstr)->second)){
			ParseError(line,"Undeclared Variable"); return false;}return true;}
	else if(t.GetToken()==ERR){ ParseError(line,"Unrecognized Input Pattern");
		cout<<"("<<t.GetLexeme()<<")\n"; return false;} return false;}
bool AssignStmt(istream& in,int& line){
	bool varstatus=false,status=false; LexItem t; Value val; string identstr;
	varstatus=Var(in,line,t); identstr=t.GetLexeme();
	if (varstatus){ t=Parser::GetNextToken(in,line);
		if (t==ASSOP){ status=Expr(in,line,val);
			if(!status){ParseError(line,"Missing Expression in Assignment Statment");return status;}
			float tempfloat; if (SymTable[identstr]==INTEGER && val.GetType()==VREAL){
				tempfloat=val.GetReal(); val.SetType(VINT); val.SetInt((int)tempfloat); TempsResults[identstr]=val;}
			else TempsResults[identstr]=val;}
		else if(t.GetToken()==ERR){ ParseError(line,"Unrecognized Input Pattern");
			cout<<"("<<t.GetLexeme()<<")\n"; return false;} else{ParseError(line,"Missing Assignment Operator=");return false;}}
	else {ParseError(line,"Missing Left-Hand Side Variable in Assignment statement");return false;} return status;}
bool ExprList(istream& in,int& line){
	Value val; bool status=false; status=Expr(in,line,val);
	if(!status){ParseError(line,"Missing Expression");return false;} ValQue->push(val);
	LexItem tok=Parser::GetNextToken(in,line);
	if (tok==COMA) status=ExprList(in,line); else if(tok.GetToken()==ERR){
		ParseError(line,"Unrecognized Input Pattern"); cout<<"("<<tok.GetLexeme()<<")\n"; return false;}
	else{Parser::PushBackToken(tok);return true;} return status;}
bool Expr(istream& in,int& line,Value & retVal){
    Value val1,val2; bool t1=Term(in,line,val1); LexItem tok;
    if(!t1)return false; retVal=val1; tok=Parser::GetNextToken(in,line);
    if(tok.GetToken()==ERR){ ParseError(line,"Unrecognized Input Pattern");
        cout<<"("<<tok.GetLexeme()<<")\n"; return false;} while (tok==PLUS || tok==MINUS){
        t1=Term(in,line,val2); if(!t1){ParseError(line,"Missing operand after operator"); return false;}
		if(retVal.GetType()==VCHAR || val2.GetType()==VCHAR){
            ParseError(line,"Run-Time Error-Illegal Mixed Type Operands"); return false;}
        else{if (val1.GetType()==VERR || val2.GetType()==VERR){
            	ParseError(line,"Variable Redefinition");return false;}
            if(tok==PLUS) retVal=retVal+val2; else if(tok==MINUS)retVal=retVal-val2;}
        tok=Parser::GetNextToken(in,line);
        if(tok.GetToken()==ERR){ ParseError(line,"Unrecognized Input Pattern");
            cout<< "("<<tok.GetLexeme()<<")\n"; return false;}}
    Parser::PushBackToken(tok); return true;}
bool Term(istream& in,int& line,Value& retVal){
	Value val1,val2; bool t1=SFactor(in,line,val1); LexItem tok;
	if(!t1)return false; retVal=val1; tok=Parser::GetNextToken(in,line);
	if(tok.GetToken()==ERR){ ParseError(line,"Unrecognized Input Pattern");
			cout<<"("<<tok.GetLexeme()<<")\n"; return false;}
	while (tok==MULT || tok==DIV){ t1=SFactor(in,line,val2);
		if(!t1){ParseError(line,"Missing operand after operator");return false;}
		if(retVal.GetType()==VCHAR || val2.GetType()==VCHAR){
			ParseError(line,"Run-Time Error-Illegal Mixed Type Operands"); return false;}
		else{ if (val1.GetType()==VERR || val2.GetType()==VERR){
            	ParseError(line,"Variable Redefinition"); return false;} if (tok==MULT) retVal=retVal*val2;
			else if (tok==DIV){ if (val2.GetInt()==0){
					ParseError(line,"Run-Time Error-Illegal Division by Zero"); return false;}
				else retVal=retVal/val2;}} tok=Parser::GetNextToken(in,line);
		if(tok.GetToken()==ERR){ ParseError(line,"Unrecognized Input Pattern");
			cout<<"("<<tok.GetLexeme()<<")\n"; return false;}}
	Parser::PushBackToken(tok); return true;}
bool SFactor(istream& in,int& line,Value& retVal){
	LexItem t=Parser::GetNextToken(in,line); int sign=0;
	if(t==MINUS)sign=-1; else if(t==PLUS) sign=1;
	else Parser::PushBackToken(t); return Factor(in,line,sign,retVal);}
bool LogicExpr(istream& in,int& line,Value& retVal){
	Value val1,val2; bool t1=Expr(in,line,val1); LexItem tok;
	if(!t1) return false; tok=Parser::GetNextToken(in,line);
	if(tok.GetToken()==ERR){ ParseError(line,"Unrecognized Input Pattern");
		cout<<"("<<tok.GetLexeme()<<")\n"; return false;}
	if (tok==LTHAN  || tok==EQUAL){ t1=Expr(in,line,val2);
		if(!t1){ParseError(line,"Missing expression after relational operator");return false;}
		if(val1.GetType()==VCHAR || val2.GetType()==VCHAR){
            ParseError(line,"Run-Time Error-Illegal Mixed Type Operands"); return false;}
		if (tok==EQUAL){ if ((val1==val2).GetBool()) retVal.SetBool(true); else retVal.SetBool(false);}
		else if (tok==LTHAN){ if ((val1 < val2).GetBool()) retVal.SetBool(true); else retVal.SetBool(false); }return true;}
	Parser::PushBackToken(tok); return true;}
bool Factor(istream& in,int& line,int sign,Value& retVal){
	LexItem tok=Parser::GetNextToken(in,line); if(tok==IDENT){ string lexeme=tok.GetLexeme();
		if (!(defVar.find(lexeme)->second)){ParseError(line,"Undefined Variable");return false;}
		retVal=TempsResults[lexeme]; if (sign==-1){if (retVal.GetType()==VINT) retVal.SetInt(-(retVal.GetInt()));
			else if (retVal.GetType()==VREAL) retVal.SetReal(-(retVal.GetReal()));} return true;}
	else if(tok==ICONST){ retVal.SetType(VINT);
		if (sign==-1)retVal.SetInt(-(stoi(tok.GetLexeme())));
		else retVal.SetInt(stoi(tok.GetLexeme()));return true;}
	else if(tok==SCONST){retVal.SetType(VCHAR);retVal.SetChar(tok.GetLexeme());return true;}
	else if(tok==RCONST){retVal.SetType(VREAL);if (sign==-1)retVal.SetReal(-(stof(tok.GetLexeme())));
		else retVal.SetReal(stof(tok.GetLexeme()));return true;}
	else if(tok==LPAREN){ bool ex=Expr(in,line,retVal);
		if(!ex){ParseError(line,"Missing expression after (");return false;}
		if(Parser::GetNextToken(in,line)==RPAREN)return ex;
		ParseError(line,"Missing) after expression"); return false;}
	else if(tok.GetToken()==ERR){ ParseError(line,"Unrecognized Input Pattern");
		cout<<"("<<tok.GetLexeme()<<")\n";return false;}
	ParseError(line,"Unrecognized input"); return 0;}