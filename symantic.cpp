#include "symantic.hpp"
using std::string;
std::vector<SymbolTable*> sym_table_stack;
std::vector<int> offsets_stack;
string my_func;
int num_of_args;
int inloop;
bool MAIN_Found=0;
int reg_counter = 0;
CodeBuffer &buffer = CodeBuffer::instance();
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////DONE

void up_f(Node* N, Formals* F)
{
    // cout << "up_f - start" << endl;
    my_func=N->_id;
    num_of_args = F->vec.size();
    // cout << "up_f - end" << endl;
}


string UPPERSTR(string input)
{
    // cout << "UPPERSTR - start" << endl;
    std::string result;
    result.reserve(input.size()); // Reserve memory for efficiency

    for (char c : input) {
        result.push_back(std::toupper(c));
    }

    return result;
    // cout << "UPPERSTR - end" << endl;
};

void is_it_bool(Exp *e)
{
    // cout << "is_it_bool - start" << endl;
    if (e->_type != "bool")
    {
        output::errorMismatch(yylineno);
        exit(0);
    }
    // cout << "is_it_bool - end" << endl;
}
void decl_func(){

    buffer.emitGlobal("@.int_specifier = constant [4 x i8] c\"%d\\0A\\00\"");
    buffer.emitGlobal("@.str_specifier = constant [4 x i8] c\"%s\\0A\\00\"");
    buffer.emitGlobal("declare i32 @printf(i8*, ...)");
    buffer.emitGlobal("declare void @exit(i32)");


    buffer.emitGlobal("@.DividedByZero = constant [23 x i8] c\"Error division by zero\\00\"");


    buffer.emitGlobal("define void @print(i8*) {");
    buffer.emitGlobal("call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.str_specifier, i32 0, i32 0), i8* %0)");
    buffer.emitGlobal("ret void");
    buffer.emitGlobal("}");

    buffer.emitGlobal("define void @printi(i32) {");
    buffer.emitGlobal("call i32 (i8*, ...) @printf(i8* getelementptr ([4 x i8], [4 x i8]* @.int_specifier, i32 0, i32 0), i32 %0)");
    buffer.emitGlobal("ret void");
    buffer.emitGlobal("}");

}

void StartProg()
{
    // cout << "StartProg - start" << endl;
    SymbolTable* MY_TABLE = new SymbolTable();
    sym_table_stack.push_back(MY_TABLE);

    vector<string> x = { "string", "void" };
    vector<string> y = { "int", "void" };

    Scope* print(new Scope("print", x, 0, true));
    Scope* printi(new Scope("printi", y, 0, true));

    MY_TABLE->table.push_back(print);
    MY_TABLE->table.push_back(printi);
    offsets_stack.push_back(0);
    decl_func();
    // cout << "StartProg - end" << endl;
}

void endProg(int yy_eof, int yy_char)
{
    // cout << "endProg - start" << endl;
    if (yy_char != yy_eof)
    {
        output::errorSyn(yylineno);
        exit(0);
    }


    if (!MAIN_Found) {
        output::errorMainMissing();
        exit(0);
    }
    removeTable();
    buffer.printGlobalBuffer();
    buffer.printCodeBuffer();
    // cout << "endProg - end" << endl;
}

Scope* FindID(const string& name)
{
    // cout << "FindID - start" << endl;
    for (int i = sym_table_stack.size() - 1; i >= 0; i--)
    {

        SymbolTable *current = sym_table_stack[i];

        for (int j = 0; j < current->table.size(); j++)
        {
            //////////////cout<<sym_table_stack[i]->table[j]->Val<< " "<<name<<i<<endl;
            if (current->table[j]->Val == name)
            {

                return current->table[j];
            }
        }
    }
    // cout << "FindID - end" << endl;
    return nullptr;
}

void addTable()
{
    // cout << "addTable - start" << endl;
    for (auto i : sym_table_stack)
    {
        for (auto j : i->table)
        {
            ////////////// //// // cout <<j->Val << " " << j->is_func << " " << j->Type.size() << endl;
        }
        ////////////// //// // cout <<"---------new TBL----------" << endl;
    }


    SymbolTable *new_table = new SymbolTable();
    sym_table_stack.push_back(new_table);
    offsets_stack.push_back(offsets_stack.back());
    // cout << "addTable - end" << endl;
}

void removeTable()
{
    // cout << "removeTable - start" << endl;
    //output::endScope();

    for (int i = 0; i < sym_table_stack.back()->table.size(); i++)
    {
        //////////////cout<<sym_table_stack.back()->table[i]->Val<<" "<<sym_table_stack.back()->table[i]->is_func;
        if (sym_table_stack.back()->table[i]->is_func)
        {

            string retType = sym_table_stack.back()->table[i]->Type.back();
            sym_table_stack.back()->table[i]->Type.pop_back();

            auto x = sym_table_stack.back()->table[i]->Type;
            auto y = convertToUppercase(x);
            //output::printID(sym_table_stack.back()->table[i]->Val, 0, output::makeFunctionType(UPPERSTR(retType), y));
            continue;
        }

        string id = sym_table_stack.back()->table[i]->Val;
        int offset = sym_table_stack.back()->table[i]->Offset;
        string type = sym_table_stack.back()->table[i]->Type[0];
       // output::printID((id), offset, UPPERSTR(type));
    }

    sym_table_stack.pop_back();
    offsets_stack.pop_back();
    // cout << "removeTable - end" << endl;
}

void is_legale_byte(Node *z_number)
{
    // cout << "is_legale_byte - start" << endl;
    int my_b_value = std::stoi(z_number->_id);
    if (my_b_value < 0 || my_b_value > 255)
    {
        output::errorByteTooLarge(yylineno, z_number->_id);
        exit(0);
    }
    // cout << "is_legale_byte - end" << endl;
}

Program::Program()
{
    // cout << "Program - start" << endl;
    inloop = 0;
    num_of_args=0;
    StartProg();
    // cout << "Program - end" << endl;
}

////LPAREN Exp RPAREN
Exp::Exp(Exp *my_exp): Node(my_exp->_id,my_exp->_reg,my_exp->_label), _type(my_exp->_type),true_list(my_exp->true_list),false_list(my_exp->false_list)
{
    // cout << "Exp::Exp(Exp *my_exp) - start" << endl;
    // cout << "Exp::Exp(Exp *my_exp) - end" << endl;
}

/////NUM, NUM B, STRING  ,TRUE, FALSE
Exp::Exp(string Mtype, Node *N): Node(N->_id), _type(Mtype) {
    // cout << "Exp::Exp(string Mtype, Node *N) - start" << endl;
    vector<pair<int, BranchLabelIndex>> T;
    vector<pair<int, BranchLabelIndex>> F;
    this->true_list= T;
    this->false_list= F;

    this->_reg=get_var();
    //השמה
    if (Mtype=="num"){
        buffer.emit(this->_reg + " = add i32 0," +N->_id);
    }
    if(Mtype == "byte"){
        buffer.emit(this->_reg + " = add i8 0," +N->_id);
        }
    if(Mtype=="string"){
        int  str_size=N->_id.size()-1;
        string m_r= this->_reg;
        m_r.erase(0,1);//remove the % from the reg
        buffer.emitGlobal("@" + m_r + " = constant [ " + to_string(str_size) + " x i8 ] c" + N->_id + "\\00\"");
        buffer.emit(this->_reg + " = getelementptr [ " + to_string(str_size) + " x i8 ], [ " + to_string(str_size) + " x i8 ]* @" + m_r + ", i8 0, i8 0");
    }


    // cout << "Exp::Exp(string Mtype, Node *N) - end" << endl;
}

///NOT Exp
Exp::Exp(Exp *my_exp, bool x): Node(my_exp->_id,my_exp->_reg,my_exp->_label), _type(my_exp->_type),true_list(my_exp->false_list),false_list(my_exp->true_list){
    // cout << "Exp::Exp(Exp *my_exp, bool x) - start" << endl;
   //this->_reg = get_var();
   // buffer.emit(this->_reg + " = add i1 0, " + my_exp->_reg);///////////////////////////////////////*************************************
    if (x)
    {
        if (my_exp->_id == "true")
        {
            this->_id = "false";
        }
        else
        {
            this->_id = "true";
        }
    }

    // cout << "Exp::Exp(Exp *my_exp, bool x) - end" << endl;
}

///ID
Exp::Exp(Node *id){
    // cout << "Exp::Exp(Node *id) - start" << endl;
    vector<pair<int, BranchLabelIndex>> F;
    vector<pair<int, BranchLabelIndex>> T;
    this->true_list = T;
    this->false_list = F;

    Scope *S = FindID(id->_id);
    if (S == nullptr || S->is_func) {
        output::errorUndef(yylineno, id->_id);
        exit(0);
    }
    this->_type = S->Type[0];
    this->_id = id->_id;

    this->_reg = get_var();
    string my_ptr = get_var();
    if (S->Offset >= 0) {//LOCAL VARIABLE

        buffer.emit( my_ptr + " = getelementptr [ 50 x i32], [ 50 x i32]*%stack, i32 0, i32 " + to_string(S->Offset));

    } else {//FUNCTION PARAMETER

        buffer.emit( my_ptr + "= getelementptr [ " + to_string(num_of_args) + " x i32], [ " + to_string(num_of_args) + " x i32]* %params, i32 0, i32 " + to_string( num_of_args+ S->Offset));

    }
    buffer.emit(this->_reg + " = load i32, i32* "+ my_ptr);


    string llvm_type= get_llvm_type(this->_type);
    if(llvm_type!="i32"){
        string tmp_reg=this->_reg;
        this->_reg=get_var();
        buffer.emit( this->_reg + " = trunc i32 " + tmp_reg + " to " + llvm_type);

    }
    cout <<_reg<<"*************************************"<<endl;
    // cout << "Exp::Exp(Node *id) - end" << endl;
}
///| LPAREN Type RPAREN Exp
Exp::Exp(Type *type, Exp *E): Node(E->_id,E->_reg,E->_label), _type(type->_type),true_list(E->true_list),false_list(E->false_list){
    // cout << "Exp::Exp(Type *type, Exp *E) - start" << endl;
    if ((type->_type == "byte" || type->_type == "int") && (E->_type == "byte" || E->_type == "int")){
        return;
    }
    // cout << "Exp::Exp(Type *type, Exp *E) - end" << endl;
    output::errorMismatch(yylineno);
    exit(0);
}	// LPAREN Type RPAREN Exp



Exp::Exp(string Mtype, string bool_val) : Node(bool_val), _type(Mtype) {
    vector<pair<int, BranchLabelIndex>> F;
    vector<pair<int, BranchLabelIndex>> T;
    this->_reg = get_var();
    false_list = F;
    true_list = T;
    if (bool_val== "true"){
        buffer.emit(this->_reg + " = add i1 0,1");
    }
    else if (bool_val== "false"){
        buffer.emit( this->_reg + " = add i1 0,0");
    }
}


string  check_zero_dizision(Exp *Beta){
    // cout << "check_zero_dizision(Exp *Beta) - start" << endl;
    string zero = get_var();
    buffer.emit(zero + " = icmp eq i32 %" + Beta->_reg + ", 0");
    int loc = buffer.emit("br i1 " + zero + ", label @, label @");

    string label1 = buffer.genLabel();
    string ptr_exp = get_var(); // TODO: DO FUNC

    buffer.emit(ptr_exp + " = getelementptr [23 x i8], [23 x i8]*@DivideByZero, i32 0, i32 0");
    buffer.emit("call i32 (i8*, ...) @printf(i8* " + ptr_exp + ")");
    buffer.emit("call void @exit(i32 0)");
    int loc2 = buffer.emit("br label @");

    string label2 = buffer.genLabel();

    buffer.bpatch(buffer.makelist({loc, FIRST}), label1);
    buffer.bpatch(buffer.makelist({loc, SECOND}), label2);
    buffer.bpatch(buffer.makelist({loc2, FIRST}), label2);
    // cout << "check_zero_dizision(Exp *Beta) - end" << endl;
return label2 ;
}

string relop_helper( Exp *A, Exp *Beta, Node* op_sign, string my_reg){
    // cout << "relop_helper( Exp *A, Exp *Beta, Node* op_sign, string my_reg) - start" << endl;
    string llvm_size = (A->_type == "int" || Beta->_type == "int") ? "i32" : "i8";
    string llvm_op;
    string FINISH_ = "";
    if (llvm_size == "i32") {
        llvm_op = (op_sign->_id == "eq") ? "eq" : (op_sign->_id == "!=") ? "ne" : (op_sign->_id == ">")
                                                                                  ? "sgt"
                                                                                  : (op_sign->_id == "<")
                                                                                    ? "slt"
                                                                                    : (op_sign->_id ==
                                                                                       "<=") ? "sle" : "sge";
    }
    else {
        llvm_op = (op_sign->_id == "==") ? "eq" : (op_sign->_id == "!=") ? "ne" : (op_sign->_id == ">")
                                                                                  ? "ugt"
                                                                                  : (op_sign->_id == "<")
                                                                                    ? "ult"
                                                                                    : (op_sign->_id ==
                                                                                       "<=") ? "ule" : "uge";
    }

    if (A->_type == "byte"&&llvm_size== "i32") {
        buffer.emit( A->_reg + " = zext i8  " + A->_reg + " to i32");
    }
    if (Beta->_type == "byte"&&llvm_size== "i32"){
        buffer.emit( Beta->_reg + " = zext i8 " + Beta->_reg + " to i32");
    }
    buffer.emit( my_reg + " = icmp "  +llvm_op + " "  + llvm_size + " " + A->_reg + ", " + Beta->_reg);
    ///why?

    if (A->_label != ""){
        FINISH_ = A->_label;
    }
    else {
        FINISH_ = Beta->_label;
    }
    // cout << "relop_helper( Exp *A, Exp *Beta, Node* op_sign, string my_reg) - end" << endl;
    return FINISH_;
}

string binop_helper( Exp *A, Exp *Beta, Node* op_sign, string my_reg){
    // cout << "binop_helper( Exp *A, Exp *Beta, Node* op_sign, string my_reg) - start" << endl;
    string FINISHED_="";
    string llvm_size = (A->_type == "int" || Beta->_type == "int") ? "i32" : "i8";
    string llvm_op="";
    string tmp="";
    if (A->_type== "byte"&&llvm_size=="i32"){
        tmp = get_var();
        buffer.emit( tmp + " = zext i8 " + A->_reg + " to i32");
        A->_reg = tmp;
    }
    tmp="";
    if (Beta->_type == "byte"){
        tmp= get_var();
        buffer.emit( tmp + " = zext i8 " + Beta->_reg + " to i32");
        Beta->_reg = tmp;
    }

    if (op_sign->_id == "+"){
        llvm_op = (A->_type == "int" || Beta->_type == "int") ? "add" : "add";
    }
    else if (op_sign->_id == "-")
    {
        llvm_op = (A->_type == "int" || Beta->_type == "int") ? "sub" : "sub";
    }
    else if (op_sign->_id == "*")
    {
        llvm_op = (A->_type == "int" || Beta->_type == "int") ? "mul" : "mul";
    }

    else if (op_sign->_id == "/"){
        llvm_op = (A->_type == "int" || Beta->_type == "int") ? "sdiv" : "sdiv";
        FINISHED_ = check_zero_dizision(Beta);
    }
    buffer.emit(my_reg + " = " + llvm_op + " " + llvm_size + " " + A->_reg + ", " + Beta->_reg);
    if ( A->_type == "byte" && Beta->_type == "byte" &&  op_sign->_id == "/"){
        string my_reg2 = get_var();
        buffer.emit(my_reg2 + " = trunc i32 " + my_reg + " to i8");
        return FINISHED_+" "+my_reg2;
    }
    // cout << "binop_helper( Exp *A, Exp *Beta, Node* op_sign, string my_reg) - end" << endl;
    return FINISHED_+" "+my_reg;
}

////// Exp BINOP_MULT Exp , Exp BINOP_PLUS Exp  , Exp AND Exp , Exp OR Exp , Exp RELOP Exp
Exp::Exp(string op, Exp *A, Exp *Beta, Node* op_sign, Prev* prev){
    // cout << "Exp::Exp(string op, Exp *A, Exp *Beta, Node* op_sign, Prev* prev) - start" << endl;
    vector<pair<int, BranchLabelIndex>> F;
    vector<pair<int, BranchLabelIndex>> T;
    this->_reg = get_var();
    this->true_list = T;
    this->false_list = F;
    if (A->_type==Beta->_type|| (A->_type == "int"  &&Beta->_type == "byte") || (Beta->_type == "int" && A->_type == "byte")){
        if (op == "binop_mult" || op == "binop_plus"){
            this->_type = (A->_type == "int" || Beta->_type == "int") ? "int" : "byte";

            string tmp = binop_helper(A, Beta, op_sign, this->_reg);

            std::istringstream iss(tmp);
            std::vector<std::string> tokens;
            std::string token;
            while (std::getline(iss, token, ' ')) {
                tokens.push_back(token);
            }
            this->_reg = token[1];
            if(this->_label!=""){
                this->_label=token[0];
           }

            return;
        }

        if (op == "relop")
        {
            this->_type = "bool";

            string tmp=relop_helper(A, Beta, op_sign, this->_reg);

            if(this->_label!=""){
                this->_label=tmp;
            }
            return;
        }
    }

    if (A->_type == "bool" && Beta->_type == "bool"){
        this->_type = "bool";
        if (Beta->_label != "") {
            this->_label = Beta->_label;
        }
        else {
            this->_label = prev->_lable; //TODO - what is P?
        }
        if (op == "or")///////////////////////////////////////////////////////////////HAVENT CHECKED
        {
            this->_id = (A->_id == "true" || Beta->_id == "true") ? "true" : "false";
            int loc_befor = buffer.emit("br label @");
            string x = buffer.genLabel();
            int loc_after = buffer.emit("br label @");
            string endLabel = buffer.genLabel();
            buffer.emit( this->_reg + " = phi i1 [" + Beta->_reg + ", " + this->_label + "],[ 1 , " + x + "]");
            buffer.bpatch(buffer.makelist({prev->_location, FIRST}), x);
            buffer.bpatch(buffer.makelist({prev->_location, SECOND}), prev->_lable);
            buffer.bpatch(buffer.makelist({loc_befor, FIRST}), endLabel);
            buffer.bpatch(buffer.makelist({loc_after, FIRST}), endLabel);
            return;
        }

        if (op == "and")
        {
            this->_id = (A->_id == "true" && Beta->_id == "true") ? "true" : "false";
            int loc_befor = buffer.emit("br label @");
            string x = buffer.genLabel();
            int loc_after = buffer.emit("br label @");
            string endLabel = buffer.genLabel();
            buffer.emit( this->_reg + " = phi i1 [" +Beta->_reg + ", " + A->_label + "],[" + Beta->_reg + ", " + x + "]");
            buffer.bpatch(buffer.makelist({prev->_location, FIRST}), prev->_lable);
            buffer.bpatch(buffer.makelist({prev->_location, SECOND}), x);
            buffer.bpatch(buffer.makelist({loc_befor, FIRST}), endLabel);
            buffer.bpatch(buffer.makelist({loc_after, FIRST}), endLabel);
            return;
        }
    }

    // cout << "Exp::Exp(string op, Exp *A, Exp *Beta, Node* op_sign, Prev* prev) - end" << endl;
    output::errorMismatch(yylineno);
    exit(0);
}//////////////////////////////////////////////////////////////////////////////////////////////

Exp::Exp(Call* call) : Node( call->_id, call->_reg, call->_label), _type(call->my_type){
    // cout << "Exp::Exp(Call* call) - start" << endl;
    vector<pair<int, BranchLabelIndex>> _False;
    vector<pair<int, BranchLabelIndex>> _True;
    this->true_list = _True;
    this->false_list = _False;
    // cout << "Exp::Exp(Call* call) - end" << endl;
}

Statement::Statement(string Mtype): Node(), my_type(Mtype){
    // cout << "Statement::Statement(string Mtype): Node(), my_type(Mtype) - start" << endl;
    // TODO: check if need to change Mtype to "ret type"
    vector<pair<int, BranchLabelIndex>> b;
    vector<pair<int, BranchLabelIndex>> c;
    this->b_list = b;
     this->c_list = c;

    if ((FindID(my_func)->Type.back()) != "void")
    {
        output::errorMismatch(yylineno);
        exit(0);
    }
    buffer.emit("ret void");
    // cout << "Statement::Statement(string Mtype): Node(), my_type(Mtype) - end" << endl;
}

////Call SC
Statement::Statement(Call* call): Node(call->_id, call->_reg, call->_label), my_type(call->my_type){
    // cout << "Statement::Statement(Call* call) - start" << endl;
    vector<pair<int, BranchLabelIndex>> list_break;
    vector<pair<int, BranchLabelIndex>> list_continue;
    this->b_list = list_break;
    this->c_list = list_continue;
    // cout << "Statement::Statement(Call* call) - end" << endl;
}

//////// BREAK SC  , CONTINUE SC /////////////////////////////////
Statement::Statement(Node *node){
    // cout << "Statement::Statement(Node *node) - start" << endl;
    vector<pair<int, BranchLabelIndex>> b;
    vector<pair<int, BranchLabelIndex>> c;
    this->b_list = b;
   this->c_list = c;

    if (inloop ==0  && node->_id == "break"){
        output::errorUnexpectedBreak(yylineno);
        exit(0);
    }
    else if (inloop == 0 && node->_id == "continue"){
        output::errorUnexpectedContinue(yylineno);
        exit(0);
    }
    my_type = node->_id;
    int go_to_4_bp=buffer.emit("br label @");
    if ( node->_id == "break")
    {
        this->b_list=buffer.makelist({go_to_4_bp,FIRST});
    }
    else if (node->_id == "continue")
    {
        this->c_list =buffer.makelist({go_to_4_bp, FIRST});

    }
    /////////////////////////////////////////////////////////////// data
    // cout << "Statement::Statement(Node *node) - end" << endl;
}

////RETURN Exp SC
Statement::Statement(Exp* exp)
{

    // cout << "Statement::Statement(Exp* exp) - start" << endl;
    if (exp->_type == "void")
    {
        output::errorMismatch(yylineno);
        exit(0);
    }

        for (int i = sym_table_stack.size() - 1; i >= 0; i--)
        {

            SymbolTable *current = sym_table_stack[i];

            for (int j = 0; j < current->table.size(); j++)
            {

                if (current->table[j]->Val == my_func)
                {

                    string return_type = current->table[j]->Type.back();
                    string exp_type = exp->_type;

                    if (return_type == exp_type || (return_type == "int" && exp_type == "byte")){
                        if ((return_type == "int" && exp_type == "byte")) {
                            string new_reg = get_var();
                            buffer.emit(new_reg + " = zext i8 " + exp->_reg + " to i32");
                            buffer.emit("ret " + get_llvm_type(return_type) + " " + new_reg);
                        }

                        else{
                            this->my_type = return_type;    // TODO: check if need to change to exp_type like in the reference
                            buffer.emit("ret " + get_llvm_type(return_type) + " " + exp->_reg);
                        }
                        return;
                    }
                    else
                    {
                        if(!current->table[j]->is_ovewride){
                            output::errorMismatch(yylineno);
                            exit(0);
                        }

                    }
                }
            }
        }


    // cout << "Statement::Statement(Exp* exp) - end" << endl;
        output::errorUndef(yylineno, "");
        exit(0);
}
////Type ID ASSIGN Exp SC
Statement::Statement(Type* type, Node* node, Exp* exp){
    // cout << "Statement::Statement(Type* type, Node* node, Exp* exp) - start" << endl;
     //// // cout <<"type->_type: " << type->_type << endl;
     //// // cout <<"node->_id: " << node->_id << endl;
     //// // cout <<"exp->_type: " << exp->_type << endl;
     vector<pair<int, BranchLabelIndex>> b;
     vector<pair<int, BranchLabelIndex>> c;
     this->b_list = b;
     this->c_list = c;

    if (type->_type != exp->_type)
    {
        if (!(type->_type == "int" && exp->_type == "byte"))
        {
            ////////////// //// // cout <<"not should be here" << endl;
            output::errorMismatch(yylineno);
            exit(0);
        }
    }

    Scope *tmp = FindID(node->_id);
    if (tmp != nullptr)
    {
        // //// // cout <<0 << endl;
        output::errorDef(yylineno, node->_id);
        exit(0);
    }
    int z = offsets_stack.back()++;
    vector<string> tmppp;
    tmppp.push_back(type->_type);
    // //////////////cout<<" node->_id"<<endl;
    Scope* new_scope(new Scope(node->_id, tmppp, z));
    sym_table_stack.back()->table.push_back(new_scope);

    this->_reg = get_var();
    string  exp_reg = exp->_reg;
    string llvm_type = get_llvm_type(type->_type);

    // cout << this->_reg << " ********* " << llvm_type << endl;

    if (type->_type == "int" && exp->_type == "byte")
    {
        exp_reg= get_var();
        buffer.emit(exp_reg+ " = zext i8 " + exp->_reg + " to i32");
    }
    
    buffer.emit(this->_reg + " = add " + llvm_type + "  0, " + exp_reg);
    string my_ptr = get_var();
    buffer.emit(my_ptr + " = getelementptr [50 x i32], [50 x i32]* @stack, i32 0, i32 " + to_string(z));
    // cout << "C" << endl;
    string m_reg= this->_reg;


    if (llvm_type != "i32") {
        m_reg = get_var();
        buffer.emit(m_reg+ " = zext " + llvm_type + " " + this->_reg + " to i32");
    }

    // cout << "Liad3 ***************" << endl;

    buffer.emit("store i32 " + m_reg + ", i32* " + my_ptr);
    // cout << "Statement::Statement(Type* type, Node* node, Exp* exp) - end" << endl;
}

Statement::Statement(Node* node, Exp* exp){
    // cout << "Statement::Statement(Node* node, Exp* exp) - start" << endl;
    vector<pair<int, BranchLabelIndex>> b;
    vector<pair<int, BranchLabelIndex>> c;
    this->b_list = b;
    this->c_list = c;

    //// // cout <<"Statement::Statement(Node* node, Exp* exp)" << endl;
    Scope *tmp = FindID(node->_id);
    if (tmp == nullptr|| tmp->is_func)
    {
        output::errorUndef(yylineno, node->_id);
        exit(0);
    }

    if (tmp->Type.back() != exp->_type)
    {
        if (!(strcmp(tmp->Type.end()->c_str(), "int") == 0 && exp->_type == "byte"))
        {
            output::errorMismatch(yylineno);
            exit(0);
        }
    }

    if (tmp->is_func)
    {
        output::errorUndef(yylineno, node->_id);
        exit(0);
    }

    my_type = exp->_type;
    this->_label = exp->_label;
    // emmiting the store
    string reg = get_var();
    string llvm_type = get_llvm_type(exp->_type);
    string my_exp_reg= exp->_reg;
    if (llvm_type != "i32") {
        my_exp_reg = get_var();
        buffer.emit(my_exp_reg + " = zext " + llvm_type + " " + exp->_reg + " to i32");
        buffer.emit(reg + " = add i32 0," +my_exp_reg);
        string ptr = get_var();
        if (tmp->Offset >= 0)
        {
            buffer.emit(
                    ptr +
                    " = getelementptr [ 50 x i32], [ 50 x i32]* %stack, i32 0, i32 " +
                    to_string(tmp->Offset));
        }
        else if (tmp->Offset < 0)
        {
            buffer.emit(
                    ptr +
                    " = getelementptr [ " + to_string(num_of_args) + " x i32], [ " + to_string(num_of_args) + " x i32]* %params, i32 0, i32 " +
                    to_string(tmp->Offset + num_of_args));
        }
        buffer.emit("store i32 " + exp->_reg + ", i32* " + ptr);
    }

    this->_reg = reg;
        // cout << "Statement::Statement(Node* node, Exp* exp) - end" << endl;
}
//Type ID SC
Statement::Statement(Type *type, Node *node)
{
    // cout << "Statement::Statement(Type *type, Node *node) - start" << endl;
    vector<pair<int, BranchLabelIndex>> B;
    vector<pair<int, BranchLabelIndex>> C;
    this->b_list = B;
    this->c_list = C;

    // ////////////// //// // cout <<"Statement::Statement(Type *type, Node *node)" << endl;
    this->_id = node->_id;
    my_type = type->_type;

    Scope *tmp = FindID(node->_id);
    if (tmp != nullptr)
    {
        // //// // cout <<1 << endl;
        output::errorDef(yylineno, node->_id);
        exit(0);
    }

    int z = offsets_stack.back()++;
    vector<string> tmp123;
    tmp123.push_back(type->_type);
    // //////////////cout<<node->_id<<endl;
    Scope *new_scope = new Scope(node->_id, tmp123, z);
    sym_table_stack.back()->table.push_back(new_scope);

    this->_reg = get_var();
    string llvm_type = get_llvm_type(type->_type);
    buffer.emit(this->_reg + " = add " + llvm_type + " 0, 0");

    string reg_ptr = get_var();
    buffer.emit(reg_ptr + " = getelementptr [50 x i32], [50 x i32]* %stack, i32 0, i32 " + to_string(z));

    string KEEPER = this->_reg;
    if (llvm_type != "i32")
    {
        KEEPER = get_var();
        buffer.emit(KEEPER + " = zext " + llvm_type + " " + this->_reg + " to i32");
    }
    buffer.emit("store i32 " + KEEPER + " , i32* " + reg_ptr);
    // cout << "Statement::Statement(Type *type, Node *node) - end" << endl;
}

FormalsList::FormalsList(FormalsDecl *formalsDecl)
{
    // cout << "FormalsList::FormalsList(FormalsDecl *formalsDecl) - start" << endl;
    if(formalsDecl->_id==my_func){
        // //// // cout <<2 << endl;
        output::errorDef(yylineno,my_func);
        exit(0);
    }
    //////////////// //// // cout <<"FormalsList::FormalsList(FormalsDecl *formalsDecl)" << endl;
   for (int i = 0; i < vec.size(); i++)
    {
        auto my_arg=vec[i]->_id;
        for (int j = 0; j < vec.size(); j++)
        {
            if(my_arg==vec[j]->_id){
                // //// // cout <<3 << endl;
                output::errorDef(yylineno,my_arg);
                exit(0);
            }
        }
    }
    vec.push_back(formalsDecl);	// TODO: if need to insert to the begin of the vector
    // cout << "FormalsList::FormalsList(FormalsDecl *formalsDecl) - end" << endl;
}

FormalsList::FormalsList(FormalsDecl *f_d, FormalsList *f_l)
{
    // cout << "FormalsList::FormalsList(FormalsDecl *f_d, FormalsList *f_l) - start" << endl;
    if(f_d->_id==my_func){
        // //// // cout <<4 << endl;
        output::errorDef(yylineno,my_func);
        exit(0);
    }
    this->vec = vector<FormalsDecl*> (f_l->vec);
    for (int i = 0; i < vec.size(); i++)
    {
         ////cout<<vec[i]->_id<< " "<<f_d->_id<<yylineno<<endl;
            if(vec[i]->_id==f_d->_id){
                // //// // cout <<5 << endl;
                output::errorDef(yylineno,f_d->_id);
                exit(0);
            }

    }
    vec.push_back(f_d); // TODO: if need to insert to the begin of the vector

    // cout << "FormalsList::FormalsList(FormalsDecl *f_d, FormalsList *f_l) - end" << endl;
}

Formals::Formals(FormalsList *f_l)
{
    // cout << "Formals::Formals(FormalsList *f_l) - start" << endl;
    for(int i=0;i<f_l->vec.size();i++)
    {
        if(f_l->vec[i]->_id==my_func)
        {
            // //// // cout <<6 << endl;
            output::errorDef(yylineno,my_func);
            exit(0);
        }
    }
    this->vec = vector<FormalsDecl*> (f_l->vec);
    // cout << "Formals::Formals(FormalsList *f_l) - end" << endl;
}

FormalsDecl::FormalsDecl(Type* t, Node* id) : Node(id->_id), _type(t->_type)
{
    // cout << "FormalsDecl::FormalsDecl(Type* t, Node* id) - start" << endl;
    if(id->_id==my_func)
    {
        // //// // cout <<7 << endl;
        output::errorDef(yylineno,my_func);
        exit(0);
    }
    // cout << "FormalsDecl::FormalsDecl(Type* t, Node* id) - end" << endl;
};

Call::Call(Node* id) : Node(id->_id)
{
    // cout << "Call::Call(Node* id) - start" << endl;
    int ctr=0;
    auto first_tbl = sym_table_stack.front();
    for (auto i = first_tbl->table.begin() ; i < first_tbl->table.end(); i++){
      //  FindID("foo");
   // ////////////cout<<(*i)->Val<<this->my_type<<(*i)->Type.size()<<endl;
        if ((*i)->Val == id->_id && (*i)->is_func && (*i)->Type.size() == 1)
        {
            this->my_type = (*i)->Type.back();

            this->_reg = get_var();
            string llvm_type = get_llvm_type(this->my_type);
            if (llvm_type == "void")
            {
                buffer.emit("call void @" + id->_id + "()");
            }
            else
            {
                buffer.emit(_reg + " = call " + llvm_type + " @" + id->_id + "()");
            }

            return;
        }else if ((*i)->Val == id->_id && !(*i)->is_func)
        {
            output::errorUndefFunc(yylineno, id->_id);
            exit(0);
        }else if ((*i)->Val == id->_id && (*i)->is_func && (*i)->Type.size() > 1&&!(*i)->is_ovewride){
            ////////////cout<<(*i)->Type[0]<<" "<<(*i)->Type[1]<<endl;
            output::errorPrototypeMismatch(yylineno,  id->_id);
            exit(0);
        }else if ((*i)->Val == id->_id && (*i)->is_func && (*i)->Type.size() > 1&&(*i)->is_ovewride){
           ctr++;
        }

    }if(ctr>0){
        output::errorPrototypeMismatch(yylineno,  id->_id);
        exit(0);
    }

        output::errorUndefFunc(yylineno, id->_id);
        exit(0);
        // cout << "Call::Call(Node* id) - end" << endl;
}

Call::Call(Node *id, ExpList *exp_list) : Node(id->_id)
{
    // cout << "Call::Call(Node *id, ExpList *exp_list) - start" << endl;
    auto first_tbl = sym_table_stack.front();
    int counter = 0;
    int counter_func=0;

    for (auto i = first_tbl->table.begin() ; i < first_tbl->table.end(); i++) {
       ////////cout<<(*i)->Val<<" loop call"<<endl;

        if ((*i)->Val == id->_id && (*i)->is_func && (*i)->Type.size() == exp_list->vec.size() + 1) {
            if(!(*i)->is_ovewride){
                counter_func++;
                continue;
            }
            int yy=exp_list->vec.size()-1;
            for (int j = 0; j < exp_list->vec.size(); j++) {
                if ((*i)->is_ovewride&&(*i)->Type[j] == exp_list->vec[yy]->_type ||
                    ((*i)->Type[j] == "int" && exp_list->vec[yy]->_type == "byte")) {
                    counter++;
                }
                if(counter== exp_list->vec.size()){
                    counter_func++;
                }
                yy--;
            }
            counter=0;
        }

    }

    if (counter_func > 1) {
        output::errorAmbiguousCall(yylineno, id->_id);
        exit(0);
    }
 //   auto first_tbl = sym_table_stack.front();
    first_tbl = sym_table_stack.front();
    ////////cout<<"here";
    int ARE_THERE_FEW_FUNC=0;


    for (auto i = first_tbl->table.begin() ; i < first_tbl->table.end(); i++)
    {
        string args_types = "( ";

        if ((*i)->Val != id->_id){
            continue;
        }
        if((*i)->Val == id->_id){
            ARE_THERE_FEW_FUNC++;
        }

        if ((*i)->Val == id->_id && (*i)->is_func && (*i)->Type.size() == exp_list->vec.size() + 1) {

           int y=exp_list->vec.size()-1;
            int ctr1=0;
            for (int j = 0; j < exp_list->vec.size(); j++,y--){
                if ((*i)->Type[j] != exp_list->vec[y]->_type) // if the type is not equal
                {
                    buffer.emit("*******************************************");
                    cout <<"*******************************************"<<exp_list->vec[y]->_reg<<endl;
                    if (!((*i)->Type[j] == "int" && exp_list->vec[y]->_type == "byte")) // if iiligle cast
                    {
                        if ((*i)->is_ovewride == false)
                        {
                            output::errorPrototypeMismatch(yylineno, id->_id);
                            exit(0);
                        }
                        else
                        {
                            break;
                        }
                    }
                    else // good cast
                    {
                        // HW5 zero extend
                        string new_reg = get_var();
                        buffer.emit(new_reg + " = zext i8 " + exp_list->vec[y]->_reg + " to i32");
                        args_types += "i32 " + new_reg + " ,";
                        ctr1++; // count the good casts (byte to int)
                    }
                }
                else // if the type is equal
                {
                    buffer.emit("*******************************************");
                    cout<<"*******************************************"<<exp_list->vec[y]->_reg<<endl;
                    args_types += get_llvm_type((*i)->Type[j]) + " " + exp_list->vec[y]->_reg + " ,";
                    ctr1++; // count the good casts or equal types
                }
                if (ctr1 == exp_list->vec.size() ) { //
                    args_types.pop_back();
                    args_types += ")";
                    this->my_type = (*i)->Type.back();
                    this->_reg = get_var();
                    if (this->my_type == "void"){
                        buffer.emit("call void @" + id->_id + args_types);
                    }
                    else{
                        buffer.emit(this->_reg + " = call " + get_llvm_type(this->my_type) + " @" + id->_id +
                                    args_types);
                    }

                    int loccation = buffer.emit("br label @");
                    this->_label = buffer.genLabel(); // TODO: check what is does ?
                    buffer.bpatch(buffer.makelist({loccation, FIRST}), this->_label);
                    return;
                }
            }
        }
    }

    if(ARE_THERE_FEW_FUNC)
    {
        output::errorPrototypeMismatch(yylineno,  id->_id);
        exit(0);
    }

    else
        {
            output::errorUndefFunc(yylineno, id->_id);
            exit(0);
        }
    // cout << "Call::Call(Node *id, ExpList *exp_list) - end" << endl;
}

ExpList::ExpList(Exp *exp)
{
    // cout << "ExpList::ExpList(Exp *exp) - start" << endl;
   // this->vec.emplace(vec.begin(), exp);
    //this->vec.insert(vec.begin(),exp);
    cout<<exp->_id<<"*************************************************"<<endl;

    cout<<exp->_reg<<"*************************************************"<<endl;
    this->vec.push_back(exp);
    // cout << "ExpList::ExpList(Exp *exp) - end" << endl;
}

ExpList::ExpList(Exp *exp, ExpList *exp_list)
{
    // cout << "ExpList::ExpList(Exp *exp, ExpList *exp_list) - start" << endl;
    this->vec = vector<Exp*> (exp_list->vec);
    cout<<exp->_reg<<"************************************************"<<endl;
    //vec.emplace(vec.begin(), exp);
  //  vec.insert(vec.begin(),exp);
    this->vec.push_back(exp);
    // cout << "ExpList::ExpList(Exp *exp, ExpList *exp_list) - end" << endl;
}

bool check_if_doubl(string name,Formals* formal,RetType* retType, OverRide* overRide)
{
    // cout << "check_if_doubl - start" << endl;
    int ctr=0;
    for (int i = sym_table_stack.size() - 1; i >= 0; i--){ // go over the stack
        SymbolTable *current = sym_table_stack[i];
        for (int j = 0; j < current->table.size(); j++) // go over the table
        {
            //////cout<<sym_table_stack[i]->table[j]->Val<< " "<<name<<endl;
            if (current->table[j]->Val == name && current->table[j]->is_func&&!(current->table[j]->is_ovewride)){ // if the name is already in the table and it is a function and it is not override
                ctr++;
            }
            else if (current->table[j]->Val == name && current->table[j]->is_func && (current->table[j]->is_ovewride) && overRide->is_override == false) { // if the name is already in the table and it is a function and it is not override
                output::errorOverrideWithoutDeclaration(yylineno, name);
                exit(0);
            }
            else  if (current->table[j]->Val == name &&!(current->table[j]->is_func)){ // if the name is already in the table and it is not a function
                ctr++;
            }
            else if (current->table[j]->Val == name && current->table[j]->is_func&&(current->table[j]->is_ovewride)) // if the name is already in the table and it is a function and it is override
            {
                //// //// // cout <<current->table[j]->Val << " befor type" << endl;
                if( current->table[j]->Type.size() ==formal->vec.size()+1)  // if the size of the type is equal to the size of the formal +1
                {
                    int size = formal->vec.size();
                    for (int x = 0; x < current->table[j]->Type.size() - 1; x++)  // go over the types of the function in the table
                    {
                        if (!(current->table[j]->Type[x] == formal->vec[size - x - 1]->_type)) { // if the type is not equal
                            // //// // cout <<current->table[j]->Type[x] << " " << formal->vec[size - x - 1]->_type << endl;
                            break;
                        }
                        if (x == formal->vec.size() - 1) // if all the types are equal
                        {
                            if (current->table[j]->Type[x + 1] == retType->my_type)  // if the return type is equal
                            {
                                // //// // cout << 8 << endl;

                                // //// // cout << "is overwirde in table " << current->table[j]->is_ovewride << endl;
                                // //// // cout << "is overwirde in node " << overRide->is_override << endl;

                                // //// // cout <<current->table[j]->Val << " " << name << endl;
                                // //// // cout <<current->table[j]->Type[x + 1] << " " << retType->my_type << endl;

                                for (auto &k : current->table[j]->Type)
                                {
                                    // //// // cout <<k << ", ";
                                }

                                // //// // cout <<endl;

                                for (auto &l : formal->vec)
                                {
                                    // //// // cout <<l->_type << ", ";
                                }

                                // //// // cout <<endl;


                                output::errorDef(yylineno, name);
                                exit(0);
                            }
                        }
                    }
                    //// //// // cout <<"next func" << endl;
                }
            }
        }
    }
    // cout << "check_if_doubl - end" << endl;
    return ctr>1;
}

void check_if_doubl_param(std::string name)
{
    // cout << "check_if_doubl_param - start" << endl;
    for (int i = sym_table_stack.size() - 1; i >= 0; i--){ // go over the stack
        SymbolTable *current = sym_table_stack[i];
        for (int j = 0; j < current->table.size(); j++) // go over the table
        {
            //////cout<<sym_table_stack[i]->table[j]->Val<< " "<<name<<endl;
            if (current->table[j]->Val == name){ // if the name is already in the table and it is a function and it is not override
                output::errorDef(yylineno, name);
                exit(0);
            }

        }
    }
    // cout << "check_if_doubl_param - end" << endl;
    return ;
}

FuncDecl::FuncDecl(OverRide* overRide, RetType* retType, Node* id, Formals* formal)
{
    // cout << "FuncDecl::FuncDecl(OverRide* overRide, RetType* retType, Node* id, Formals* formal) - start" << endl;
  my_func = id->_id;
    num_of_args= formal->vec.size();
    this->my_type = retType->my_type;
    if(check_if_doubl(id->_id,formal,retType, overRide)){
        output::errorDef(yylineno, id->_id);
        exit(0);
    }
    auto x = FindID(id->_id);
    if (x != nullptr && overRide->is_override && !x->is_ovewride) // if the function is override and the function in the table is not override
    {
        output::errorFuncNoOverride(yylineno, id->_id);
        exit(0);
    }
    if (x != nullptr && x->is_ovewride && !overRide->is_override) // if the function is not override and the function in the table is override
    {
        output::errorOverrideWithoutDeclaration(yylineno, id->_id);
        exit(0);
    }if (x != nullptr && !x->is_ovewride && !overRide->is_override){
        output::errorDef(yylineno, id->_id);
        exit(0);
    }
    if (overRide->is_override) {
        if (id->_id == "main") {
            output::errorMainOverride(yylineno);
            exit(0);
        }
    }
    if (x != nullptr && id->_id == "main") {
        // //// // cout <<11 << endl;
        output::errorDef(yylineno, id->_id);
        exit(0);
    }
    if (id->_id == "main"&& retType->my_type != "void"){
        if(MAIN_Found){
            // //// // cout <<12 << endl;
            output::errorDef(yylineno, id->_id);
            exit(0);
        }
    }
    if ( (id->_id == "main"&& formal->vec.size() == 0 && retType->my_type == "void") ) {
        MAIN_Found= true;
    }

    vector<string> params = vector<string> (formal->vec.size());
    int j=0;
    for (int i = formal->vec.size()-1; j< formal->vec.size(); i--)
    {
         if(my_func==formal->vec[i]->_id){
            // //// // cout <<13 << endl;
            output::errorDef(yylineno,my_func);
            exit(0);
         }
       ////////////////cout<<formal->vec[i]->_type<<"decl       ";
        params[j] = formal->vec[i]->_type;
        j++;
    }
   ////////////// //// // cout <<2 << endl;
    params.push_back(retType->my_type);
    // //////////////cout<<id->_id<<endl;
    auto tmp = new Scope(id->_id, params, 0, true, overRide->is_override);
    sym_table_stack.back()->table.push_back(tmp);



    addTable();
    //// ////////////// //// // cout <<3 << endl;
     j=0;
    string my_params_for_buff= "( ";
    for (int i = formal->vec.size()-1; j< formal->vec.size(); i--)
    {
        my_params_for_buff += get_llvm_type(formal->vec[i]->_type) + ",";//hw5

        check_if_doubl_param(formal->vec[i]->_id);
        vector<string> tmpp123;

        tmpp123.push_back(formal->vec[i]->_type);
        auto tmp = new Scope(formal->vec[i]->_id, tmpp123, -j - 1);
        sym_table_stack.back()->table.push_back(tmp);
        j++;

    }
    my_params_for_buff.pop_back();
    my_params_for_buff += " )";

    //now from here : take care of hw5
    int arg_size = formal->vec.size();
    buffer.emit("define " + get_llvm_type(retType->my_type) +" @" +id->_id +my_params_for_buff+ " {");
    buffer.emit("%stack = alloca [50 x i32] ");
    buffer.emit("%params = alloca [" + to_string(arg_size) +" x i32]");
    for (int i = 0; i < arg_size; i++){
        string ptr = get_var();
        buffer.emit(ptr+ " = getelementptr [" + to_string(arg_size) +" x i32], [" + to_string(arg_size) +" x i32]* %params, i32 0, i32 " +to_string(arg_size - i - 1));
        string tmp_register = "%"+to_string(i);
        if ( get_llvm_type(params[i]) != "i32") {
            tmp_register = get_var();
            buffer.emit( tmp_register + " = zext " +  get_llvm_type(params[i]) + " %" + to_string(i) + " to i32");
        }
        buffer.emit("store i32 " + tmp_register + ", i32* " + ptr);
    }

    // cout << "FuncDecl::FuncDecl(OverRide* overRide, RetType* retType, Node* id, Formals* formal) - end" << endl;
}

Statement* InParserCHANGEinWhile(bool flag, N* n, Prev* prev, Statement* statement)
{
    // cout << "InParserCHANGEinWhile(bool flag, N* n, Prev* prev, Statement* statement) - start" << endl;
   if(flag)
   {
       inloop ++;
       return statement;
   }

   int label_place = buffer.emit("br label @");
    string my_label = buffer.genLabel();
    buffer.bpatch(buffer.makelist({n->to_backpatch, FIRST}), n->_label);
    buffer.bpatch(buffer.makelist({prev->_location, FIRST}), prev->_lable);
    buffer.bpatch(buffer.makelist({prev->_location, SECOND}), my_label);
    buffer.bpatch(buffer.makelist({label_place, FIRST}), n->_label);

   if (statement->b_list.size() != 0){
        buffer.bpatch(statement->b_list, my_label);
    }
   if (statement->c_list.size() != 0){
        buffer.bpatch(statement->c_list, prev->_lable);
   }

   inloop--;
    // cout << "InParserCHANGEinWhile(bool flag, N* n, Prev* prev, Statement* statement) - end" << endl;
    return statement;
}


void check_bool(Node *node)
{
    // cout << "check_bool(Node *node) - start" << endl;
    Exp *exp = dynamic_cast<Exp *>(node);
    //////////////cout<<"check_bool"<<endl;
    if (exp->_type != "bool") {
        output::errorMismatch(yylineno);
        exit(0);
    }
    // cout << "check_bool(Node *node) - end" << endl;
}

std::vector<std::string> convertToUppercase(std::vector<std::string> input)
{
    // cout << "convertToUppercase(std::vector<std::string> input) - start" << endl;
    std::vector<std::string> result;
    result.reserve(input.size()); // Reserve memory for efficiency

    for (const std::string& str : input) {
        std::string convertedString;
        convertedString.reserve(str.size()); // Reserve memory for efficiency

        for (char c : str) {
            convertedString.push_back(std::toupper(c));
        }

        result.push_back(convertedString);
    }
    // cout << "convertToUppercase(std::vector<std::string> input) - end" << endl;
    return result;
}

void exitFunc(RetType* ret)
{
    // cout << "exitFunc() - start" << endl;
    my_func = "";
    num_of_args=0;
    // cout << "exitFunc() - end" << endl;
    if(ret->my_type=="void"){
        buffer.emit("ret void");
    }
    else{
        string this_type= get_llvm_type(ret->my_type);
        buffer.emit("ret "+this_type+" 0");

    }
    buffer.emit("}");
}

M::M()
{
    // cout << "M() - start" << endl;
   this->next_quad = buffer.genLabel();
   // cout << "M() - end" << endl;
}

N::N()
{
    // cout << "N() - start" << endl;
    this->to_backpatch = buffer.emit("br label @");
    this->_label = buffer.genLabel();
    // cout << "N() - end" << endl;
}

string get_var()
{
    return "%my_var" + to_string(reg_counter++);
}


string get_llvm_type(string type) {
  // cout << "get_llvm_type(string type) - start" << endl;
    if (type == "int") {
        return "i32";
    } else if (type == "bool") {
        return "i1";
    } else if (type == "string") {
        return "i8*";
    } else if (type == "void") {
        return "void";
    } else if (type == "byte") {
        return "i8";
    }
    // cout << "get_llvm_type(string type) - end" << endl;
}

Statements::Statements (Statement* s)
{
    // cout << "Statements (Statement* s) - start" << endl;
    this->c_list = s->c_list;
    this->b_list = s->b_list;
    // cout << "Statements (Statement* s) - end" << endl;
}

Statements::Statements(Statements* ss,Statement* s)
{
    // cout << "Statements(Statements* ss,Statement* s) - start" << endl;

    if (ss != nullptr&&s != nullptr){
        // cout << "ss is not null" << endl;

        this->c_list= buffer.merge(ss->c_list,s->c_list);
        this->b_list=buffer.merge(ss->b_list,s->b_list);

        // cout << "ss is null" << endl;
      } else if(ss == nullptr&&s != nullptr){
        cout<<"ss is null"<<endl;
        this->c_list= s->c_list;
        cout<<"ss is null"<<endl;
        this->b_list= s->b_list;
    } else if(ss != nullptr&&s == nullptr){
        cout<<"s is null"<<endl;
        this->c_list= ss->c_list;
        cout<<"s is null"<<endl;
        this->b_list= ss->b_list;
    }

    // cout << "Statements(Statements* ss,Statement* s) - end" << endl;
}

////LBRACE OS Statments CS RBRACE //////////////////
Statement::Statement(Statements* statements){
    if (statements == nullptr){
      cout<<"Statement(Statements* statements) - null"<<endl;
    }
    // cout << "Statement(Statements* statements) - start" << endl;
    vector<pair<int, BranchLabelIndex>> list_break;
    vector<pair<int, BranchLabelIndex>> list_continue;
    // cout << "Statement(Statements* statements) - a" << endl;
    this->b_list = list_break;
    this->c_list = list_continue;
    // cout << "Statement(Statements* statements) - b" << endl;
    this->c_list = statements->c_list;
    // cout << "Statement(Statements* statements) - c" << endl;
    this->b_list = statements->b_list;
    // cout << "Statement(Statements* statements) - end" << endl;
    ////////////////////////////////////data
};

// IF LPAREN ISBOOL RPAREN OS M Statement  {$$ = new Statement(dynamic_cast<Statement*>($7)); removeTable(); backpatching_for_if(dynamic_cast<M*>($6),dynamic_cast<IsBool*>($3));} //V
//            | IF LPAREN ISBOOL RPAREN OS M Statement  ELSE OS  N Statement   {$$= backpatching_for_if_else(dynamic_cast<M*>($6),dynamic_cast<N*>($10),dynamic_cast<IsBool*>($3),dynamic_cast<Statement*>($7),dynamic_cast<Statement*>($10));}// V
//
Statement::Statement(Statement* statement){
    // cout << "Statement(Statement* statement) - start" << endl;
    this->c_list = statement->c_list;
    this->b_list = statement->b_list;
    // cout << "Statement(Statement* statement) - end" << endl;
}

void  backpatching_for_if(M* my_label, IsBool *isBool){

    // cout << "backpatching_for_if(M* my_label, IsBool *isBool) - start" << endl;
    int loc = buffer.emit("br label @");
    string a = buffer.genLabel();
    buffer.bpatch(isBool->is_bool_exp->true_list, my_label->next_quad);
    buffer.bpatch(isBool->is_bool_exp->false_list, a);
    buffer.bpatch(buffer.makelist({loc, FIRST}), a);
    // cout << "backpatching_for_if(M* my_label, IsBool *isBool) - end" << endl;
}

IsBool::IsBool(Exp *exp) : is_bool_exp(exp)
{
    // cout << "IsBool(Exp *exp) - start" << endl;

    int res = buffer.emit("br i1 " + exp->_reg + ", label @, label @");
    this->is_bool_exp->true_list = buffer.makelist({res, FIRST});
    this->is_bool_exp->false_list = buffer.makelist({res, SECOND});
    // cout << "IsBool(Exp *exp) - end" << endl;
}


/////////////////////////////////////////////////////////////////////////////////////////////////////////////////TO DO
Statement* backpatching_for_if_else(M* my_label, N* my_label2, IsBool* isBool, Statement* if_s, Statement* else_s){
    // cout << "backpatching_for_if_else(M* my_label, N* my_label2, IsBool* isBool, Statement* if_s, Statement* else_s) - start" << endl;
    cout<<"********************************"<<isBool->is_bool_exp->_reg<<endl;
    int loc = buffer.emit("br label @");
    string a = buffer.genLabel();
    if (isBool== nullptr){
        // cout << "isBool is null" << endl;
    }
    // cout << "isBool is not null" << endl;
    buffer.bpatch(isBool->is_bool_exp->true_list, my_label->next_quad);
    // cout << "isBool->is_bool_exp->true_list is not null" << endl;
    buffer.bpatch(isBool->is_bool_exp->false_list, my_label2->_label);
        // cout << " a" << endl;

    buffer.bpatch(buffer.makelist({my_label2->to_backpatch, FIRST}), a);
    // cout << " b" << endl;
    buffer.bpatch(buffer.makelist({loc, FIRST}), a);
    // cout << " c" << endl;
    if_s->b_list = buffer.merge(if_s->b_list, else_s->b_list);
    // cout << " d" << endl;
    if_s->c_list = buffer.merge(if_s->c_list, else_s->c_list);
    // cout << "backpatching_for_if_else(M* my_label, N* my_label2, IsBool* isBool, Statement* if_s, Statement* else_s) - end" << endl;
    return if_s;
}



Prev::Prev(Exp* E)
{
    // cout << "Prev::Prev(Exp* left) - start" << endl;
    this->_location = buffer.emit("br i1 %" + E->_reg + ", label @, label @");
    this->_lable = buffer.genLabel();
    // cout << "Prev::Prev(Exp* left) - end" << endl;
}

Node* createPrev(Exp* E){
// cout << "createPrev(Exp* E) - start" << endl;
   return new Prev(E);
}

Node* if_discover(Exp *A){
    // cout << "if_discover(Exp *A) - start" << endl;
    Node *david = new Prev(A);
    // cout << "if_discover(Exp *A) - end" << endl;
    return david;
}