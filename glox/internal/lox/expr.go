package lox

type Expr interface {
	Accept(visitor ExprVisitor) (interface{}, error)
}
type ExprVisitor interface {
	VisitAssignExpr(expr *AssignExpr) (interface{}, error)
	VisitBinaryExpr(expr *BinaryExpr) (interface{}, error)
	VisitCallExpr(expr *CallExpr) (interface{}, error)
	VisitGroupExpr(expr *GroupExpr) (interface{}, error)
	VisitLiteralExpr(expr *LiteralExpr) (interface{}, error)
	VisitLogicalExpr(expr *LogicalExpr) (interface{}, error)
	VisitUnaryExpr(expr *UnaryExpr) (interface{}, error)
	VisitVarExpr(expr *VarExpr) (interface{}, error)
}
type AssignExpr struct {
	Name *Token
	Val  Expr
}

func NewAssignExpr(Name *Token, Val Expr) *AssignExpr {
	return &AssignExpr{Name, Val}
}
func (expr *AssignExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitAssignExpr(expr)
}

type BinaryExpr struct {
	Op  *Token
	Lhs Expr
	Rhs Expr
}

func NewBinaryExpr(Op *Token, Lhs Expr, Rhs Expr) *BinaryExpr {
	return &BinaryExpr{Op, Lhs, Rhs}
}
func (expr *BinaryExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitBinaryExpr(expr)
}

type CallExpr struct {
	Callee Expr
	Paren  *Token
	Args   []Expr
}

func NewCallExpr(Callee Expr, Paren *Token, Args []Expr) *CallExpr {
	return &CallExpr{Callee, Paren, Args}
}
func (expr *CallExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitCallExpr(expr)
}

type GroupExpr struct {
	Expr Expr
}

func NewGroupExpr(Expr Expr) *GroupExpr {
	return &GroupExpr{Expr}
}
func (expr *GroupExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitGroupExpr(expr)
}

type LiteralExpr struct {
	Val interface{}
}

func NewLiteralExpr(Val interface{}) *LiteralExpr {
	return &LiteralExpr{Val}
}
func (expr *LiteralExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitLiteralExpr(expr)
}

type LogicalExpr struct {
	Op  *Token
	Lhs Expr
	Rhs Expr
}

func NewLogicalExpr(Op *Token, Lhs Expr, Rhs Expr) *LogicalExpr {
	return &LogicalExpr{Op, Lhs, Rhs}
}
func (expr *LogicalExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitLogicalExpr(expr)
}

type UnaryExpr struct {
	Op   *Token
	Expr Expr
}

func NewUnaryExpr(Op *Token, Expr Expr) *UnaryExpr {
	return &UnaryExpr{Op, Expr}
}
func (expr *UnaryExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitUnaryExpr(expr)
}

type VarExpr struct {
	Name *Token
}

func NewVarExpr(Name *Token) *VarExpr {
	return &VarExpr{Name}
}
func (expr *VarExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitVarExpr(expr)
}
