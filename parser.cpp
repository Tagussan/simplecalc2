#include <string>
#include <iostream>
#include <cmath>
#include <stack>
#include <sstream>
#define rep(i,n) for(long i = 0;i<(n);i++)
#define debug(i) cout << #i << "=" << i << endl;
using namespace std;

enum class rule_t {EXPRESSION = 0, EXPRESSION_REM, TERM, TERM_REM, CHUNK, CHUNK_REM, FACTOR, POW, PRIM_EXPRESSION};
string rule_str[] = {"EXPRESSION", "EXPRESSION_REM", "TERM", "TERM_REM", "CHUNK", "CHUNK_REM", "FACTOR", "POW", "PRIM_EXPRESSION"};
enum class op_t {ADD = 0, bin_MIN, MUL, DIV, uni_MIN, POW, LB, RB, NUMBER};
char op_char[] = {'+', '-', '*', '/', '#', '^', '(', ')', 'N'};

class op_token
{
    public:
    op_t op;
    op_token(op_t op){
        this->op = op;
    }
    double apply_op(stack<double>& S)
    {
        if(op == op_t::uni_MIN){
            double a = S.top();S.pop();
            return -1.0*a;
        }else{
            double a = S.top();S.pop();
            double b = S.top();S.pop();
            swap(a,b);
            switch(op){
                case op_t::ADD:
                    return a+b;
                case op_t::bin_MIN:
                    return a-b;
                case op_t::MUL:
                    return a*b;
                case op_t::DIV:
                    return a/b;
                case op_t::POW:
                    return pow(a,b);
            }
        }
    }
    void print()
    {
        cout << op_char[static_cast<int>(op)];
    }
};

class lex_token
{
    public:
    bool is_rule;
    union 
    {
        op_t op;
        rule_t rule;
    } token;
    lex_token(op_t op)
    {
        is_rule = false;
        token.op = op;
    }
    lex_token(rule_t r)
    {
        is_rule = true;
        token.rule = r;
    }
    bool match(char c)
    {
        return !is_rule && op_char[static_cast<int>(token.op)] == c;
    }
    void print(){
        if(!is_rule){
            cout << op_char[static_cast<int>(token.op)];
        }else{
            cout << rule_str[static_cast<int>(token.rule)];
        }
    }
};

void printLexS(stack<lex_token> S){
    cout << "lexS:";
    while(!S.empty()){
        lex_token top = S.top();S.pop();
        top.print();
        cout << " ";
    }
    cout << endl;
}

void printValueS(stack<double> S){
    cout << "valueS:";
    while(!S.empty()){
        double top = S.top();S.pop();
        cout << top << " ";
    }
    cout << endl;
}

void printOpS(stack<op_token> S){
    cout << "opS:";
    while(!S.empty()){
        op_token top = S.top();S.pop();
        top.print();
        cout << " ";
    }
    cout << endl;
}

class parser
{
    public:
    string input;
    int pt;
    stack<lex_token> lexS;
    stack<op_token> opS;
    stack<double> valueS;
    bool inputEnd = false;
    parser(string s){
        this->input = s;
        pt = 0;
        lexS.push(lex_token(rule_t::EXPRESSION));
    }
    double evaluate(){
        while(lexS.size() != 0 && pt <= input.size()){
            printLexS(lexS);
            printValueS(valueS);
            if(pt == input.size()){
                inputEnd = true;
            }
            if(!inputEnd){
                cout << input.substr(pt) << endl;
            }
            lex_token top = lexS.top();lexS.pop();
            if(top.is_rule){
                char c = input[pt];
                switch(top.token.rule){
                    case rule_t::EXPRESSION:
                        lexS.push(lex_token(rule_t::EXPRESSION_REM));lexS.push(lex_token(rule_t::TERM));
                        break;
                    case rule_t::EXPRESSION_REM:
                        if(inputEnd) break;
                        c = input[pt];
                        if(c == '+'){
                            lexS.push(lex_token(rule_t::EXPRESSION_REM));
                            lexS.push(lex_token(rule_t::TERM));
                            lexS.push(lex_token(op_t::ADD));
                        }else if(c == '-'){
                            lexS.push(lex_token(rule_t::EXPRESSION_REM));
                            lexS.push(lex_token(rule_t::TERM));
                            lexS.push(lex_token(op_t::bin_MIN));
                        }
                        break;
                    case rule_t::TERM:
                        lexS.push(lex_token(rule_t::TERM_REM));lexS.push(lex_token(rule_t::CHUNK));
                        break;
                    case rule_t::TERM_REM:
                        if(inputEnd) break;
                        c = input[pt];
                        if(c == '*'){
                            lexS.push(lex_token(rule_t::TERM_REM));
                            lexS.push(lex_token(rule_t::CHUNK));
                            lexS.push(lex_token(op_t::MUL));
                        }else if(c == '/'){
                            lexS.push(lex_token(rule_t::TERM_REM));
                            lexS.push(lex_token(rule_t::CHUNK));
                            lexS.push(lex_token(op_t::DIV));
                        }
                        break;
                    case rule_t::CHUNK:
                        lexS.push(lex_token(rule_t::CHUNK_REM));
                        lexS.push(lex_token(rule_t::FACTOR));
                        if(inputEnd) break;
                        c = input[pt];
                        if(c == '-'){
                            lexS.push(lex_token(op_t::uni_MIN));
                        }
                        break;
                    case rule_t::CHUNK_REM:
                        if(inputEnd) break;
                        c = input[pt];
                        //factor should begin with '(' or number
                        if(('0' <= c && c <= '9') || c == '('){
                            lexS.push(lex_token(rule_t::CHUNK_REM));lexS.push(lex_token(rule_t::FACTOR));
                        }
                        break;
                    case rule_t::FACTOR:
                        lexS.push(lex_token(rule_t::POW));lexS.push(lex_token(rule_t::PRIM_EXPRESSION));
                        break;
                    case rule_t::POW:
                        if(inputEnd) break;
                        c = input[pt];
                        if(c == '^'){
                            lexS.push(lex_token(rule_t::PRIM_EXPRESSION));
                            lexS.push(lex_token(op_t::POW));
                        }
                        break;
                    case rule_t::PRIM_EXPRESSION:
                        if(inputEnd) break;
                        c = input[pt];
                        if(c == '('){
                            lexS.push(lex_token(op_t::RB));lexS.push(lex_token(rule_t::EXPRESSION));lexS.push(lex_token(op_t::LB));
                        }else if('0' <= c && c <= '9'){
                            lexS.push(lex_token(op_t::NUMBER));
                        }
                        break;
                    default:
                        break;
                }
            }else{
                int length = 0;
                int val;
                stringstream ss;
                switch(top.token.op){
                    case op_t::NUMBER:
                    {
                        while(pt + length < input.size() && '0' <= input[pt + length] && input[pt + length] <= '9'){
                            length++;
                        }
                        ss << input.substr(pt, length);
                        cout << "number:" << input.substr(pt,length) << endl;
                        ss >> val;
                        valueS.push(1.0*val);
                        pt += length;
                        break;
                    }
                    default:
                    {    
                        if(top.match(input[pt])){
                            pt++;
                            if(top.token.op != op_t::LB && top.token.op != op_t::RB){
                                opS.push(op_token(top.token.op));
                            }
                        }else{
                            //usually not reachable
                            cout << "parser error" << endl;
                        }
                        break;
                    }
                }
            }
        }
        if(!(lexS.size() == 0 && pt == input.size())){
            debug(lexS.size());
            debug(pt);
            cout << "parse error" << endl;
        }
        while(opS.size() != 0 && valueS.size() != 0){
            printOpS(opS);
            printValueS(valueS);
            op_token top = opS.top();opS.pop();
            valueS.push(top.apply_op(valueS));
        }
        return valueS.top();
    }
};

int main(){
    parser psr("1-2-3");
    cout << psr.evaluate() << endl;
    return 0;
}
