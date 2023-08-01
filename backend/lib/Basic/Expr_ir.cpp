#include "tlang/Deserializer/Expr_ir.h"
#include "frontend.pb.h"
#include "tlang/Deserializer/Ir_visitor.h"
#include "tlang/Deserializer/Type_ir.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>
#include <string>

enum Unop deserializeUnop(const Frontend_ir::Un_op &op) {
  switch (op.value_case()) {
  case Frontend_ir::Un_op::kNeg:
    return Unop::UnopNeg;
  case Frontend_ir::Un_op::kNot:
    return Unop::UnopNot;
  default:
    llvm::outs() << "Unmatched unary op\n";
    return Unop::UnopNot;
  }
}

enum BinOp deserializeBinOp(const Frontend_ir::Bin_op &op) {
  switch (op.value_case()) {
  case Frontend_ir::Bin_op::kPlus:
    return BinOp::BinOpPlus;
  case Frontend_ir::Bin_op::kEquals:
    return BinOp::BinOpEquals;
  case Frontend_ir::Bin_op::kLessThan:
    return BinOp::BinOpLessThan;
  case Frontend_ir::Bin_op::kGreaterThan:
    return BinOp::BinOpGreaterThan;
  case Frontend_ir::Bin_op::kMultOp:
    return BinOp::BinOpMult;
  case Frontend_ir::Bin_op::kDivideOp:
    return BinOp::BinOpDivide;
  case Frontend_ir::Bin_op::kSubtractOp:
    return BinOp::BinOpSubtract;
  default:
    llvm::outs() << "Unmatched unary op\n";
    return BinOp::BinOpPlus;
  }
}

ExprUnopIR::ExprUnopIR(const Frontend_ir::Expr::_Unop &unopExpr) {
  op = deserializeUnop(unopExpr.op());
  expr = deserializeExpr(unopExpr.uexpr());
};

ExprBinOpIR::ExprBinOpIR(const Frontend_ir::Expr::_Binop &binopExpr) {
  op = deserializeBinOp(binopExpr.bin_op());
  lexpr = deserializeExpr(binopExpr.lexpr());
  rexpr = deserializeExpr(binopExpr.rexpr());
  opLineNo = binopExpr.op_line_no();
};

ExprFunctionAppIR::ExprFunctionAppIR(
    const Frontend_ir::Expr::_FunctionApp &expr) {
  functionName = expr.name();
  for (int i = 0; i < expr.args_size(); i++) {
    arguments.push_back(deserializeExpr(expr.args()[i]));
  }
};

ExprArrayMakeIR::ExprArrayMakeIR(
    const Frontend_ir::Expr::_ArrayCreation &expr) {
  varType = deserializeType(expr.texpr());
  lineNo = expr.make_line_no();
  arrConsIdx = expr.arr_cons_idx();
  for (auto e : expr.creation_exprs()) {
    creationExprs.push_back(deserializeExpr(e));
  }
};

ExprMethodCallIR::ExprMethodCallIR(const Frontend_ir::Expr::_MethodCall &expr) {
  objExpr = deserializeExpr(expr.obj_expr());
  for (auto a : expr.method_args()) {
    methodArgs.push_back(deserializeExpr(a));
  }
  methodIdx = expr.method_idx();
};

std::unique_ptr<ExprIR> deserializeExpr(const Frontend_ir::Expr &expr) {
  switch (expr.value_case()) {
  case Frontend_ir::Expr::kInteger:
    return std::unique_ptr<ExprIR>(new ExprIntegerIR(expr.integer()));
  case Frontend_ir::Expr::kFunctionApp:
    return std::unique_ptr<ExprIR>(new ExprFunctionAppIR(expr.functionapp()));
  case Frontend_ir::Expr::kUnop:
    return std::unique_ptr<ExprIR>(new ExprUnopIR(expr.unop()));
  case Frontend_ir::Expr::kBinop:
    return std::unique_ptr<ExprIR>(new ExprBinOpIR(expr.binop()));
  case Frontend_ir::Expr::kAssign:
    return std::unique_ptr<ExprIR>(new ExprAssignIR(expr.assign()));
  case Frontend_ir::Expr::kExprId:
    return std::unique_ptr<ExprIR>(new ExprIdentifierIR(expr.exprid()));
  case Frontend_ir::Expr::kEmpty:
    return std::unique_ptr<ExprIR>(new ExprEmptyIR());
  case Frontend_ir::Expr::kArrayCreation:
    return std::unique_ptr<ExprIR>(new ExprArrayMakeIR(expr.arraycreation()));
  case Frontend_ir::Expr::kVarExp:
    return std::unique_ptr<ExprIR>(new ExprVarIR(expr.varexp()));
  case Frontend_ir::Expr::kNullLit:
    return std::unique_ptr<ExprIR>(new ExprNullIR());
  case Frontend_ir::Expr::kCastExpr:
    return std::unique_ptr<ExprIR>(new ExprCastIR(expr.castexpr()));
  case Frontend_ir::Expr::kClassCreation:
    return std::unique_ptr<ExprIR>(new ExprClassMakeIR(expr.classcreation()));
  case Frontend_ir::Expr::kMethodCall:
    return std::unique_ptr<ExprIR>(new ExprMethodCallIR(expr.methodcall()));
  default:
    // FIXME
    return std::unique_ptr<ExprIR>(new ExprIntegerIR(-1));
  }
}

ExprClassMakeIR::ExprClassMakeIR(
    const Frontend_ir::Expr::_ClassCreation &expr) {
  classType = deserializeType(expr.con_texpr());
  vtableIdx = expr.vtable_index();
  for (auto a : expr.con_args()) {
    conArgs.push_back(deserializeExpr(a));
  }
};

CastType deserializeCastType(const Frontend_ir::Expr::_Cast c) {
  switch (c.value_case()) {
  case Frontend_ir::Expr::_Cast::kNoCast:
    return CastType::Identity;
  case Frontend_ir::Expr::_Cast::kWideCast:
    return CastType::Wide;
  case Frontend_ir::Expr::_Cast::kNarrowCast:
    return CastType::Narrow;
  default:
    llvm::outs() << "Cast type not set";
    return CastType::Identity;
  }
}

ExprCastIR::ExprCastIR(const Frontend_ir::Expr::_CastExpr castExpr) {
  castType = deserializeCastType(castExpr.cast_type());
  castTo = deserializeType(castExpr.cast_to());
  expr = deserializeExpr(castExpr.expr());
  castLineNo = castExpr.cast_line_no();
};

ExprVarIR::ExprVarIR(const Frontend_ir::Var &v) { var = deserializeVar(v); };
SimpleVarIR::SimpleVarIR(const std::string &name) { varName = name; };
LoadVarIR::LoadVarIR(const Frontend_ir::Var::_Load &v) {
  baseVar = deserializeVar(v.var());
};

SubscriptVarIR::SubscriptVarIR(const Frontend_ir::Var::_Subscript &var) {
  baseVar = deserializeVar(var.base_var());
  lenVar = deserializeVar(var.len_var());
  expr = deserializeExpr(var.var_exp());
  lineNo = var.line_no();
};

FieldVarIR::FieldVarIR(const Frontend_ir::Var::_Field &var) {
  baseExpr = deserializeExpr(var.base_expr());
  field_index = var.field_index();
  fieldLineNo = var.field_line_no();
};

std::unique_ptr<VarIR> deserializeVar(const Frontend_ir::Var &var) {
  switch (var.value_case()) {
  case Frontend_ir::Var::kSimple:
    return std::unique_ptr<VarIR>(new SimpleVarIR(var.simple().var_name()));
  case Frontend_ir::Var::kSubscript:
    return std::unique_ptr<VarIR>(new SubscriptVarIR(var.subscript()));
  case Frontend_ir::Var::kField:
    return std::unique_ptr<VarIR>(new FieldVarIR(var.field()));
  case Frontend_ir::Var::kLoadVar:
    return std::unique_ptr<VarIR>(new LoadVarIR(var.loadvar()));
  default:
    return nullptr;
  }
}

ExprAssignIR::ExprAssignIR(const Frontend_ir::Expr::_Assign &expr) {
  identifier = deserializeVar(expr.lhs());
  assignedExpr = deserializeExpr(expr.rhs());
}

ExprIdentifierIR::ExprIdentifierIR(const Frontend_ir::Identifier &expr) {
  var = deserializeVar(expr.id());
}

// Codegen impl

llvm::Value *ExprIntegerIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *ExprFunctionAppIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *ExprUnopIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *ExprBinOpIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *ExprAssignIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *ExprEmptyIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *ExprArrayMakeIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *SimpleVarIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *ExprIdentifierIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}

llvm::Value *SubscriptVarIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
llvm::Value *FieldVarIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
llvm::Value *ExprVarIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
llvm::Value *LoadVarIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
llvm::Value *ExprNullIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
llvm::Value *ExprCastIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
llvm::Value *ExprClassMakeIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
llvm::Value *ExprMethodCallIR::codegen(IRVisitor &visitor) {
  return visitor.codegen(*this);
}
