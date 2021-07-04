package lox

import "container/list"

// Each map reprents a single block scope, variables at the global scope are not
// tracked by the resolver. If it cannot resolve a variable in the local
// scopes, it assumes the variable to be in the global scope.
type scopeMap = map[string]bool

type loxFnType = int

const (
	fnTypeNone loxFnType = iota
	fnTypeFunction
)

// Resolver performs semantics analysis on the syntax tree.
type Resolver struct {
	scopes      *list.List
	interpreter *Interpreter
	reporter    Reporter
	currentFn   loxFnType
}

func NewResolver(interpreter *Interpreter, reporter Reporter) *Resolver {
	r := new(Resolver)
	r.scopes = list.New()
	r.interpreter = interpreter
	r.reporter = reporter
	r.currentFn = fnTypeNone
	return r
}

func (r *Resolver) Resolve(statements []Stmt) {
	for _, stmt := range statements {
		r.resolveStmt(stmt)
	}
}

func (r *Resolver) VisitBlockStmt(stmt *BlockStmt) (interface{}, error) {
	r.beginScope()
	for _, stmt := range stmt.Stmts {
		r.resolveStmt(stmt)
	}
	r.endScope()
	return nil, nil
}

func (r *Resolver) VisitExprStmt(stmt *ExprStmt) (interface{}, error) {
	r.resolveExpr(stmt.Expr)
	return nil, nil
}

func (r *Resolver) VisitClassStmt(stmt *ClassStmt) (interface{}, error) {
	r.declare(stmt.Name)
	r.define(stmt.Name)
	return nil, nil
}

func (r *Resolver) VisitFunctionStmt(stmt *FunctionStmt) (interface{}, error) {
	r.declare(stmt.Name)
	r.define(stmt.Name)
	r.resolveFunction(stmt, fnTypeFunction)
	return nil, nil
}

func (r *Resolver) VisitIfStmt(stmt *IfStmt) (interface{}, error) {
	r.resolveExpr(stmt.Cond)
	r.resolveStmt(stmt.ThenBranch)
	if stmt.ElseBranch != nil {
		r.resolveStmt(stmt.ElseBranch)
	}
	return nil, nil
}

func (r *Resolver) VisitPrintStmt(stmt *PrintStmt) (interface{}, error) {
	r.resolveExpr(stmt.Expr)
	return nil, nil
}

func (r *Resolver) VisitReturnStmt(stmt *ReturnStmt) (interface{}, error) {
	if r.currentFn == fnTypeNone {
		r.reporter.Report(newResolveError(stmt.Keyword,
			"Can't return from top-level code."))
	}
	if stmt.Val != nil {
		r.resolveExpr(stmt.Val)
	}
	return nil, nil
}

func (r *Resolver) VisitVarStmt(stmt *VarStmt) (interface{}, error) {
	r.declare(stmt.Name)
	if stmt.Init != nil {
		r.resolveExpr(stmt.Init)
	}
	r.define(stmt.Name)
	return nil, nil
}

func (r *Resolver) VisitWhileStmt(stmt *WhileStmt) (interface{}, error) {
	r.resolveExpr(stmt.Cond)
	r.resolveStmt(stmt.Body)
	return nil, nil
}

func (r *Resolver) VisitAssignExpr(expr *AssignExpr) (interface{}, error) {
	r.resolveExpr(expr.Val)
	r.resolveLocal(expr, expr.Name)
	return nil, nil
}

func (r *Resolver) VisitBinaryExpr(expr *BinaryExpr) (interface{}, error) {
	r.resolveExpr(expr.Lhs)
	r.resolveExpr(expr.Rhs)
	return nil, nil
}

func (r *Resolver) VisitCallExpr(expr *CallExpr) (interface{}, error) {
	r.resolveExpr(expr.Callee)
	for _, arg := range expr.Args {
		r.resolveExpr(arg)
	}
	return nil, nil
}

func (r *Resolver) VisitGroupExpr(expr *GroupExpr) (interface{}, error) {
	r.resolveExpr(expr.Expr)
	return nil, nil
}

func (r *Resolver) VisitLiteralExpr(expr *LiteralExpr) (interface{}, error) {
	return nil, nil
}

func (r *Resolver) VisitLogicalExpr(expr *LogicalExpr) (interface{}, error) {
	r.resolveExpr(expr.Lhs)
	r.resolveExpr(expr.Rhs)
	return nil, nil
}

func (r *Resolver) VisitUnaryExpr(expr *UnaryExpr) (interface{}, error) {
	r.resolveExpr(expr.Expr)
	return nil, nil
}

func (r *Resolver) VisitVarExpr(expr *VarExpr) (interface{}, error) {
	if r.scopes.Front() != nil {
		scopeMap := r.scopes.Front().Value.(scopeMap)
		if defined, exist := scopeMap[expr.Name.Lexeme]; exist && !defined {
			r.reporter.Report(newResolveError(expr.Name,
				"Can't read local variable in its own initializer."))
		}
	}

	r.resolveLocal(expr, expr.Name)
	return nil, nil
}

func (r *Resolver) resolveFunction(fn *FunctionStmt, fnType loxFnType) {
	enclosingFn := r.currentFn
	r.currentFn = fnType

	r.beginScope()
	for _, p := range fn.Params {
		r.declare(p)
		r.define(p)
	}
	for _, stmt := range fn.Body {
		r.resolveStmt(stmt)
	}
	r.endScope()

	r.currentFn = enclosingFn
}

func (r *Resolver) resolveLocal(expr Expr, name *Token) {
	steps := 0
	for scope := r.scopes.Front(); scope != nil; scope = scope.Next() {
		scopeMap := scope.Value.(scopeMap)
		if _, ok := scopeMap[name.Lexeme]; ok {
			r.interpreter.resolve(expr, steps)
			return
		}
		steps++
	}
}

// Similar to Interpreter.exec
func (r *Resolver) resolveStmt(stmt Stmt) {
	stmt.Accept(r)
}

// Similar to Interpreter.eval
func (r *Resolver) resolveExpr(expr Expr) {
	expr.Accept(r)
}

// called when resolver enters a new scope
func (r *Resolver) beginScope() {
	r.scopes.PushFront(make(scopeMap))
}

// called when resolver exits a new scope
func (r *Resolver) endScope() {
	r.scopes.Remove(r.scopes.Front())
}

func (r *Resolver) declare(name *Token) {
	if r.scopes.Front() != nil {
		scope := r.scopes.Front().Value.(scopeMap)
		if _, hasName := scope[name.Lexeme]; hasName {
			r.reporter.Report(newParseError(name,
				"Already has a variable with this name in this scope"))
		}
		scope[name.Lexeme] = false
	}
}

func (r *Resolver) define(name *Token) {
	if r.scopes.Front() != nil {
		scope := r.scopes.Front().Value.(scopeMap)
		scope[name.Lexeme] = true
	}
}
