%{
%}

%token IDENTIFIER INTEGER_LITERAL NULL_LITERAL
%token EQ_OP

%token INT
%token MAIN CLASS EXTENDS
%token THIS SUPER
%token IF ELSE WHILE NEW DELETE RETURN
%token OUT
%token BREAK CONTINUE

%start CompilationUnit

%%

CompilationUnit
	: MainFunctionDeclaration
	| MainFunctionDeclaration ClassDeclarations
	| ClassDeclarations MainFunctionDeclaration
	| ClassDeclarations MainFunctionDeclaration ClassDeclarations
	;

MainFunctionDeclaration
	: INT MAIN '(' ')' MainFunctionBody
	;

MainFunctionBody
	: MainBlock
	;

ClassDeclarations
	: ClassDeclarations ClassDeclaration
	| ClassDeclaration
	;

ClassDeclaration
	: CLASS Identifier ClassBody
	| CLASS Identifier EXTENDS ClassType ClassBody
	;

ClassBody
	: '{' ClassBodyDeclarations '}'
	| '{' '}'
	;

ClassBodyDeclarations
	: ClassBodyDeclarations ClassBodyDeclaration
	| ClassBodyDeclaration
	;

ClassBodyDeclaration
	: ClassMemberDeclaration
	| ConstructorDeclaration
	| DestructorDeclaration
	| ';'
	;

ClassMemberDeclaration
	: FieldDeclaration
	| MethodDeclaration
	;

FieldDeclaration
	: Type VariableDeclarators ';'
	;

MethodDeclaration
	: Type MethodDeclarator MethodBody
	;

MethodDeclarator
	: Identifier FormalParameters
	| MethodDeclarator Dimension
	;

MethodBody
	: Block
	;

/* done */
ConstructorDeclaration
	: ConstructorDeclarator ConstructorBody
	;
/* done */
ConstructorDeclarator
	: Identifier FormalParameters
	;
/* done */
ConstructorBody
	: '{' ConstructorInvocation BlockStatements '}'
	| '{' ConstructorInvocation '}'
	| Block
	;
/* done */
ConstructorInvocation
	: THIS Arguments ';'
	| SUPER Arguments ';'
	;

DestructorDeclaration
	: DestructorDeclarator DestructorBody
	;

DestructorDeclarator
	: '~' Identifier '(' ')'
	;

DestructorBody
	: Block
	;
/* done */
FormalParameters
	: '(' FormalParameterList ')'
	| '(' ')'
	;
/* done */
FormalParameterList
	: FormalParameterList ',' FormalParameter
	| FormalParameter
	;
/* done */
FormalParameter
	: Type VariableDeclaratorID
	;

VariableDeclaratorID
	: VariableDeclaratorID Dimension
	| Identifier
	;
/* done */
Block
	: '{' BlockStatements '}'
	| '{' '}'
	;
/* done */
BlockStatements
	: BlockStatements BlockStatement
	| BlockStatement
	;

BlockStatement
	: Statement
	;

MainBlock
	: '{' MainBlockStatements '}'
	| '{' '}'
	;

MainBlockStatements
	: MainBlockStatements MainBlockStatement
	| MainBlockStatement
	;

MainBlockStatement
	: MainVariableDeclarationStatement
	| BlockStatement
	;

MainVariableDeclarationStatement
	: MainVariableDeclaration ';'
	;

VariableDeclarators
	: VariableDeclarators ',' VariableDeclarator
	| VariableDeclarator
	;

VariableDeclarator
	: VariableDeclarator Dimension
	| Identifier
	;

MainVariableDeclaration
	: Type VariableDeclarators
	;

Statement
	: Block
	| EmptyStatement
	| ExpressionStatement
	| IfThenElseStatement
	| WhileStatement
	| ReturnStatement
	| DeleteStatement
	| OutputStatement
	| BreakStatement
	| ContinueStatement
	;

IfThenElseStatement
	: IF ParenExpression Statement ELSE Statement
	;

WhileStatement
	: WHILE ParenExpression Statement
	;

ReturnStatement
	: RETURN ';'
	| RETURN Expression ';'
	;

DeleteStatement
	: DELETE Expression ';'
	;

OutputStatement
	: OUT Expression ';'
	;

BreakStatement
	: BREAK ';'
	;

ContinueStatement
	: CONTINUE ';'
	;

EmptyStatement
	: ';'
	;

ParenExpression
	: '(' Expression ')'
	;

ExpressionStatement
	: StatementExpression ';'
	;

StatementExpression
	: Assignment
	| MethodInvocation
	;

Expression
	: AssignmentExpression
	;

AssignmentExpression
	: Assignment
	| EqualityExpression
	;

Assignment
	: LeftexpHandSide AssignmentOperator AssignmentExpression
	;

LeftHandSide
	: Identifier
	| FieldAccess
	| ArrayAccess
	;

EqualityExpression
	: EqualityExpression EQ_OP RelationalExpression
	| RelationalExpression
	;

RelationalExpression
	: RelationalExpression '<' AdditiveExpression
	| RelationalExpression '>' AdditiveExpression
	| AdditiveExpression
	;

AdditiveExpression
	: AdditiveExpression '+' MultiplicativeExpression
	| AdditiveExpression '-' MultiplicativeExpression
	| MultiplicativeExpression
	;

MultiplicativeExpression
	: MultiplicativeExpression '*' UnaryExpression
	| MultiplicativeExpression '/' UnaryExpression
	| UnaryExpression
	;

UnaryExpression
	: '-' UnaryExpression
	| '!' UnaryExpression
	| CastExpression
	;

CastExpression
	: ParenExpression CastExpression
	| '(' ArrayType ')' CastExpression
	| Primary
        ;

Primary
	: ArrayCreationExpression
	| Identifier
	| PrimaryNoNewArray
	;

ArrayAccess
	: Identifier DimensionExpression
	| PrimaryNoNewArray DimensionExpression
	;

PrimaryNoNewArray
	: ParenExpression
	| THIS
	| FieldAccess
	| MethodInvocation
	| ArrayAccess
	| ClassInstanceCreationExpression
	| Literal
	;

ClassInstanceCreationExpression
	: NEW ClassType Arguments
	;
	
ArrayCreationExpression
	: NEW ClassType DimensionExpressions Dimensions
	| NEW ClassType DimensionExpressions
	| NEW PrimitiveType DimensionExpressions Dimensions
	| NEW PrimitiveType DimensionExpressions
	;

DimensionExpressions
	: DimensionExpressions DimensionExpression
	| DimensionExpression
	;

DimensionExpression
	: '[' Expression ']'
	;

Dimensions
	: Dimensions Dimension
	| Dimension
	;

Dimension
	: '[' ']'
	;

FieldAccess
	: Primary '.' Identifier
	| SUPER '.' Identifier
	;

MethodInvocation
	: Identifier Arguments
	| Primary '.' Identifier Arguments
	| SUPER '.' Identifier Arguments
	;

Arguments
	: '(' ArgumentList ')'
	| '(' ')'
	;

ArgumentList
	: ArgumentList ',' Expression
	| Expression
	;

AssignmentOperator
	: '='
	;

Type
	: ReferenceType
	| PrimitiveType
	;

PrimitiveType
	: NumericType
	;

NumericType
	: IntegralType
	;

IntegralType
	: INT
	;

ReferenceType
	: ClassType
	| ArrayType
	;

ClassType
	: Identifier
	;

ArrayType
	: PrimitiveType Dimension
	| Identifier Dimension
	| ArrayType Dimension
	;

Identifier
	: IDENTIFIER
	;

Literal
	: INTEGER_LITERAL	
	| NULL_LITERAL
	;

%%
