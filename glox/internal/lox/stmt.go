package lox

type Stmt interface {
	Accept(visitor StmtVisitor) (interface{}, error)
}
type StmtVisitor interface {
	VisitBlockStmt(stmt *BlockStmt) (interface{}, error)
	VisitExprStmt(stmt *ExprStmt) (interface{}, error)
	VisitFunctionStmt(stmt *FunctionStmt) (interface{}, error)
	VisitIfStmt(stmt *IfStmt) (interface{}, error)
	VisitPrintStmt(stmt *PrintStmt) (interface{}, error)
	VisitReturnStmt(stmt *ReturnStmt) (interface{}, error)
	VisitVarStmt(stmt *VarStmt) (interface{}, error)
	VisitWhileStmt(stmt *WhileStmt) (interface{}, error)
}
type BlockStmt struct {
	Stmts []Stmt
}

func NewBlockStmt(Stmts []Stmt) *BlockStmt {
	return &BlockStmt{Stmts}
}
func (stmt *BlockStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitBlockStmt(stmt)
}

type ExprStmt struct {
	Expr Expr
}

func NewExprStmt(Expr Expr) *ExprStmt {
	return &ExprStmt{Expr}
}
func (stmt *ExprStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitExprStmt(stmt)
}

type FunctionStmt struct {
	Name   *loxToken
	Params []*loxToken
	Body   []Stmt
}

func NewFunctionStmt(Name *loxToken, Params []*loxToken, Body []Stmt) *FunctionStmt {
	return &FunctionStmt{Name, Params, Body}
}
func (stmt *FunctionStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitFunctionStmt(stmt)
}

type IfStmt struct {
	Cond       Expr
	ThenBranch Stmt
	ElseBranch Stmt
}

func NewIfStmt(Cond Expr, ThenBranch Stmt, ElseBranch Stmt) *IfStmt {
	return &IfStmt{Cond, ThenBranch, ElseBranch}
}
func (stmt *IfStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitIfStmt(stmt)
}

type PrintStmt struct {
	Expr Expr
}

func NewPrintStmt(Expr Expr) *PrintStmt {
	return &PrintStmt{Expr}
}
func (stmt *PrintStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitPrintStmt(stmt)
}

type ReturnStmt struct {
	Keyword *loxToken
	Val     Expr
}

func NewReturnStmt(Keyword *loxToken, Val Expr) *ReturnStmt {
	return &ReturnStmt{Keyword, Val}
}
func (stmt *ReturnStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitReturnStmt(stmt)
}

type VarStmt struct {
	Name *loxToken
	Init Expr
}

func NewVarStmt(Name *loxToken, Init Expr) *VarStmt {
	return &VarStmt{Name, Init}
}
func (stmt *VarStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitVarStmt(stmt)
}

type WhileStmt struct {
	Cond Expr
	Body Stmt
}

func NewWhileStmt(Cond Expr, Body Stmt) *WhileStmt {
	return &WhileStmt{Cond, Body}
}
func (stmt *WhileStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitWhileStmt(stmt)
}
