#include <iostream>
#include <stack>
#include <string>
#include <cmath>
#include <sstream>
using namespace std;

enum class rule_t { EXPRESSION = 0, EXPRESSION_REM, TERM, TERM_REM, CHUNK, CHUNK_REM, FACTOR, POW, PRIM_EXPRESSION};
string rule_str[] = { "EXPRESSION", "EXPRESSION_REM", "TERM", "TERM_REM", "CHUNK", "CHUNK_REM", "FACTOR", "POW", "PRIM_EXPRESSION" };
enum class op_t {ADD = 0, SUB, MUL, DIV, MIN, POW, LB, RB, NUMBER, END};
string op_str[] = { "+", "-(SUB)", "*", "/", "-(MIN)", "^", "(", ")", "NUMBER", "END"};

class parser
{
public:
    string input;
    int pt;
    stack<rule_t> ruleS;
    stack<op_t> opS;
    stack<double> valS;
    bool inputEnd;
    parser(){
        ruleS.push(rule_t::EXPRESSION);
        opS.push(op_t::END);
        pt = 0;
        inputEnd = false;
    }
    void debug(stack<rule_t> rules, stack<op_t> ops, stack<double> vals){
        cout << endl;
        cout << input.substr(pt) << endl;
        cout << "ruleS:";
        while(!rules.empty()){
            cout << rule_str[static_cast<int>(rules.top())] << " ";
            rules.pop();
        }
        cout << endl;
        cout << "opS:";
        while(!ops.empty()){
            cout << op_str[static_cast<int>(ops.top())] << " ";
            ops.pop();
        }
        cout << endl;
        cout << "vals:";
        while(!vals.empty()){
            cout << vals.top() << " ";
            vals.pop();
        }
        cout << endl;
    }
    double number(string& s, int& p){
        int length = 0;
        double val;
        while (p + length < s.size() && '0' <= input[p + length] && input[p + length] <= '9'){
            length++;
        }
        val = (double)stoi(input.substr(p, length));
        p += length;
        return val;
    }
    double evaluate(string s){
        this->input = s;
        while(!ruleS.empty()){
            ruleS.pop();
        }
        while(!opS.empty()){
            opS.pop();
        }
        while(!valS.empty()){
            valS.pop();
        }
        ruleS.push(rule_t::EXPRESSION);
        opS.push(op_t::END);
        pt = 0;
        inputEnd = false;
        return evaluate();
    }
    double evaluate(){
        while (!ruleS.empty()){
            if(pt == input.size()){
                inputEnd = true;
            }
            debug(ruleS, opS, valS);
            op_t topOp;
            double val1, val2;
            switch (ruleS.top())
            {
            case rule_t::EXPRESSION:
                ruleS.pop();
                ruleS.push(rule_t::EXPRESSION_REM);
                ruleS.push(rule_t::TERM);
                break;
            case rule_t::EXPRESSION_REM:
                topOp = opS.top();
                if (topOp == op_t::ADD || topOp == op_t::SUB){
                    opS.pop();
                    val1 = valS.top(); valS.pop();
                    val2 = valS.top(); valS.pop();
                    valS.push(topOp == op_t::ADD ? val2 + val1 : val2 - val1);
                }
                if (!inputEnd && (input[pt] == '+' || input[pt] == '-')){
                    opS.push(input[pt] == '+' ? op_t::ADD : op_t::SUB);
                    //ruleS.pop(); no use
                    //ruleS.push(rule_t::EXPRESSION_REM);
                    ruleS.push(rule_t::TERM);
                    pt++;
                }
                else{
                    ruleS.pop();
                }
                break;
            case rule_t::TERM:
                ruleS.pop();
                ruleS.push(rule_t::TERM_REM);
                ruleS.push(rule_t::CHUNK);
                break;
            case rule_t::TERM_REM:
                topOp = opS.top();
                if (topOp == op_t::MUL || topOp == op_t::DIV){
                    opS.pop();
                    val1 = valS.top(); valS.pop();
                    val2 = valS.top(); valS.pop();
                    valS.push(topOp == op_t::MUL ? val2 * val1 : val2 / val1);
                }
                if (!inputEnd && (input[pt] == '*' || input[pt] == '/')){
                    opS.push(input[pt] == '*' ? op_t::MUL : op_t::DIV);
                    //ruleS.pop();
                    //ruleS.push(rule_t::TERM_REM);
                    ruleS.push(rule_t::CHUNK);
                    pt++;
                }
                else{
                    ruleS.pop();
                }
                break;
            case rule_t::CHUNK:
                topOp = opS.top();
                if (topOp == op_t::MIN){
                    opS.pop();
                    val1 = valS.top(); valS.pop();
                    valS.push(-1.0 * val1);
                }
                if (!inputEnd && input[pt] == '-'){
                    opS.push(op_t::MIN);
                    pt++;
                }
                ruleS.pop();
                ruleS.push(rule_t::CHUNK_REM);
                ruleS.push(rule_t::FACTOR);
                break;
            case rule_t::CHUNK_REM:
                topOp = opS.top();
                if(topOp == op_t::MUL){
                    opS.pop();
                    val1 = valS.top(); valS.pop();
                    val2 = valS.top(); valS.pop();
                    valS.push(val2 * val1);
                }
                if(!inputEnd && (('0' <= input[pt] && input[pt] <= '9') || input[pt] == '(')){
                    opS.push(op_t::MUL);
                    //ruleS.pop();
                    //ruleS.push(rule_t::CHUNK_REM);
                    ruleS.push(rule_t::FACTOR);
                }else{
                    ruleS.pop();
                }
                break;
            case rule_t::FACTOR:
                ruleS.pop();
                ruleS.push(rule_t::POW);
                ruleS.push(rule_t::PRIM_EXPRESSION);
                break;
            case rule_t::POW:
                topOp = opS.top();
                if (topOp == op_t::POW){
                    opS.pop();
                    val1 = valS.top(); valS.pop();
                    val2 = valS.top(); valS.pop();
                    valS.push(pow(val2, val1));
                }
                if (!inputEnd && input[pt] == '^'){
                    opS.push(op_t::POW);
                    //ruleS.pop();
                    //ruleS.push(rule_t::POW);
                    ruleS.push(rule_t::PRIM_EXPRESSION);
                    pt++;
                }else{
                    ruleS.pop();
                }
                break;
            case rule_t::PRIM_EXPRESSION:
                if (!inputEnd && '0' <= input[pt] && input[pt] <= '9'){
                    valS.push(number(input, pt));
                    ruleS.pop();
                }
                else if (!inputEnd && input[pt] == '('){
                    //ruleS.pop();
                    //ruleS.push(rule_t::PRIM_EXPRESSION);
                    ruleS.push(rule_t::EXPRESSION);
                    opS.push(op_t::LB);
                    pt++;
                }
                else if (!inputEnd && input[pt] == ')'){
                    ruleS.pop();
                    opS.pop();
                    pt++;
                }
                if(inputEnd){
                    cout << "parse error" << endl;
                    return 0;
                }
                break;
            default:
                break;
            }
        }
        return valS.top();
    }

};

int main(){
    parser psr = parser();
    string input;
    while(cin >> input){
        cout << psr.evaluate(input) << endl;
    }
    //cout << psr.evaluate("(1+2") << endl;
    return 0;
}
