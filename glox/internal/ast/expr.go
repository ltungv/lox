package ast
type Expr interface {
	Accept(ExprVisitor visitor) interface{}
}
type ExprVisitor interface {
	VisitBinaryExpr(expr *BinaryExpr) interface{}
	VisitGroupingExpr(expr *GroupingExpr) interface{}
	VisitLiteralExpr(expr *LiteralExpr) interface{}
	VisitUnaryExpr(expr *UnaryExpr) interface{}
}
type BinaryExpr struct {
	Op *Token
	Left Expr
	Right Expr
}
func NewBinaryExpr(Op *Token, Left Expr, Right Expr) {
	return BinaryExpr{Op,Left,Right}
}
func (expr *BinaryExpr) Accept(visitor *ExprVisitor) interface{} {
	return visitor.VisitBinaryExpr(expr)
}
type GroupingExpr struct {
	Expression Expr
}
func NewGroupingExpr(Expression Expr) {
	return GroupingExpr{Expression}
}
func (expr *GroupingExpr) Accept(visitor *ExprVisitor) interface{} {
	return visitor.VisitGroupingExpr(expr)
}
type LiteralExpr struct {
	Value interface{}
}
func NewLiteralExpr(Value interface{}) {
	return LiteralExpr{Value}
}
func (expr *LiteralExpr) Accept(visitor *ExprVisitor) interface{} {
	return visitor.VisitLiteralExpr(expr)
}
type UnaryExpr struct {
	Op *Token
	Expression Expr
}
func NewUnaryExpr(Op *Token, Expression Expr) {
	return UnaryExpr{Op,Expression}
}
func (expr *UnaryExpr) Accept(visitor *ExprVisitor) interface{} {
	return visitor.VisitUnaryExpr(expr)
}
