package lox

type Expr interface {
	Accept(visitor ExprVisitor) (interface{}, error)
}
type ExprVisitor interface {
	VisitBinaryExpr(expr *BinaryExpr) (interface{}, error)
	VisitGroupingExpr(expr *GroupingExpr) (interface{}, error)
	VisitLiteralExpr(expr *LiteralExpr) (interface{}, error)
	VisitUnaryExpr(expr *UnaryExpr) (interface{}, error)
}
type BinaryExpr struct {
	Op    *Token
	Left  Expr
	Right Expr
}

func NewBinaryExpr(Op *Token, Left Expr, Right Expr) *BinaryExpr {
	return &BinaryExpr{Op, Left, Right}
}
func (expr *BinaryExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitBinaryExpr(expr)
}

type GroupingExpr struct {
	Expression Expr
}

func NewGroupingExpr(Expression Expr) *GroupingExpr {
	return &GroupingExpr{Expression}
}
func (expr *GroupingExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitGroupingExpr(expr)
}

type LiteralExpr struct {
	Value interface{}
}

func NewLiteralExpr(Value interface{}) *LiteralExpr {
	return &LiteralExpr{Value}
}
func (expr *LiteralExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitLiteralExpr(expr)
}

type UnaryExpr struct {
	Op         *Token
	Expression Expr
}

func NewUnaryExpr(Op *Token, Expression Expr) *UnaryExpr {
	return &UnaryExpr{Op, Expression}
}
func (expr *UnaryExpr) Accept(visitor ExprVisitor) (interface{}, error) {
	return visitor.VisitUnaryExpr(expr)
}
