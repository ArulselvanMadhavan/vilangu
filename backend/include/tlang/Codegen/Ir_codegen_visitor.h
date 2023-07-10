#ifndef TLANG_IR_CODEGEN_VISITOR_H
#define TLANG_IR_CODEGEN_VISITOR_H
#include "tlang/Deserializer/Class_ir.h"
#include "tlang/Deserializer/Expr_ir.h"
#include "tlang/Deserializer/Function_ir.h"
#include "tlang/Deserializer/Ir_visitor.h"
#include "tlang/Deserializer/Program_ir.h"
#include "tlang/Deserializer/Stmt_ir.h"
#include "tlang/Deserializer/Type_ir.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <memory>
#include <stack>
#include <stdlib.h>
#include <vector>

struct LoopInfo {
  llvm::BasicBlock *loopCond;
  llvm::BasicBlock *loopEnd;
  bool hasBreak;
  bool hasContinue;
  LoopInfo(llvm::BasicBlock *loopCondBB, llvm::BasicBlock *loopEndBB)
      : loopCond(loopCondBB), loopEnd(loopEndBB), hasBreak(false),
        hasContinue(false){};
  virtual ~LoopInfo() = default;
};

class IRCodegenVisitor : public IRVisitor {
protected:
  std::unique_ptr<llvm::LLVMContext> context;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;

  std::stack<LoopInfo *> *loops;
  std::map<std::string, llvm::AllocaInst *> varEnv;
  std::string getTypeNameAsString(llvm::Type *t);
  std::string getVtableName(std::string className);
  std::string getVtableTypeName(std::string className);
  std::string getPrintIntFormatVar();
  std::string getCastErrFormatVar();
  std::string getOutOfBoundsFormatVar();
  std::string getNegativeLenFormatVar();
  std::string getNullDerefFormatVar();
  std::string getDivByZeroFormatVar();
  void addGlobalVarStr(std::string varName, llvm::StringRef content);
  void runtimeError(std::string formatStr, llvm::ArrayRef<llvm::Value *> args);

public:
  IRCodegenVisitor();
  ~IRCodegenVisitor();
  void codegenExternFunctionDeclarations();
  void codegenClasses(const std::vector<std::unique_ptr<ClassIR>> &classes);
  void codegenVTables(const std::vector<std::unique_ptr<ClassIR>> &classes);
  llvm::FunctionType *codegenFunctionType(const FunctionIR &function);
  void codegenFunctionProtos(
      const std::vector<std::unique_ptr<FunctionIR>> &functions);
  void codegenFunctionDefn(const FunctionIR &function);
  void codegenFunctionDefns(
      const std::vector<std::unique_ptr<FunctionIR>> &functions);

  void codegenProgram(const ProgramIR &program);
  void codegenMainExpr(const std::vector<std::unique_ptr<StmtIR>> &mainExpr);
  void configureTarget();
  void dumpLLVMIR();
  virtual llvm::Value *codegen(const ExprIntegerIR &expr) override;
  virtual llvm::Value *codegen(const ExprNullIR &expr) override;
  virtual llvm::Value *codegen(const ExprFunctionAppIR &expr) override;
  virtual llvm::Value *codegen(const StmtPrintfIR &expr) override;
  virtual llvm::Value *codegen(const ExprUnopIR &expr) override;
  virtual llvm::Value *codegen(const ExprBinOpIR &expr) override;
  virtual llvm::Value *codegen(const ExprCastIR &expr) override;
  virtual llvm::Value *codegen(const StmtVarDeclIR &expr) override;
  virtual llvm::Value *codegen(const ExprIdentifierIR &expr) override;
  virtual llvm::Value *codegen(const ExprAssignIR &expr) override;
  virtual llvm::Value *codegen(const SimpleVarIR &expr) override;
  virtual llvm::Value *codegen(const SubscriptVarIR &expr) override;
  virtual llvm::Value *codegen(const FieldVarIR &expr) override;
  virtual llvm::Value *codegen(const StmtBlockIR &expr) override;
  virtual llvm::Value *codegen(const StmtIfElseIR &expr) override;
  virtual llvm::Value *codegen(const StmtWhileIR &expr) override;
  virtual llvm::Value *codegen(const StmtBreakIR &expr) override;
  virtual llvm::Value *codegen(const StmtContinueIR &expr) override;
  virtual llvm::Value *codegen(const ExprEmptyIR &expr) override;
  virtual llvm::Value *codegen(const StmtExprIR &expr) override;
  virtual llvm::Value *codegen(const ExprArrayMakeIR &expr) override;
  virtual llvm::Type *codegen(const TypeIntIR &texpr) override;
  virtual llvm::Type *codegen(const TypeClassIR &texpr) override;
  virtual llvm::Type *codegen(const TypePointerIR &texpr) override;
  virtual llvm::Type *codegen(const TypeVoidIR &texpr) override;
  virtual llvm::Type *codegen(const TypeBoolIR &texpr) override;
  virtual llvm::Type *codegen(const TypeInt8IR &texpr) override;
  virtual llvm::Value *codegen(const ExprVarIR &expr) override;
  virtual llvm::Value *codegen(const LoadVarIR &expr) override;
  void
  runOptimizingPasses(const std::vector<std::unique_ptr<StmtIR>> &mainExpr);
};
#endif
