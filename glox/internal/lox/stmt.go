package lox

type Stmt interface {
	Accept(visitor StmtVisitor) (interface{}, error)
}
type StmtVisitor interface {
	VisitBlockStmt(stmt *BlockStmt) (interface{}, error)
	VisitExpressionStmt(stmt *ExpressionStmt) (interface{}, error)
	VisitPrintStmt(stmt *PrintStmt) (interface{}, error)
	VisitVarStmt(stmt *VarStmt) (interface{}, error)
}
type BlockStmt struct {
	Statements []Stmt
}

func NewBlockStmt(Statements []Stmt) *BlockStmt {
	return &BlockStmt{Statements}
}
func (stmt *BlockStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitBlockStmt(stmt)
}

type ExpressionStmt struct {
	Expression Expr
}

func NewExpressionStmt(Expression Expr) *ExpressionStmt {
	return &ExpressionStmt{Expression}
}
func (stmt *ExpressionStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitExpressionStmt(stmt)
}

type PrintStmt struct {
	Expression Expr
}

func NewPrintStmt(Expression Expr) *PrintStmt {
	return &PrintStmt{Expression}
}
func (stmt *PrintStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitPrintStmt(stmt)
}

type VarStmt struct {
	Name        *Token
	Initializer Expr
}

func NewVarStmt(Name *Token, Initializer Expr) *VarStmt {
	return &VarStmt{Name, Initializer}
}
func (stmt *VarStmt) Accept(visitor StmtVisitor) (interface{}, error) {
	return visitor.VisitVarStmt(stmt)
}
