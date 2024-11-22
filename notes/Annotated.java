// Programming Language Technology (Chalmers DAT151 / GU DIT231)
// (C) 2022-24 Andreas Abel
// All rights reserved.

import java.util.List;

// The annotated version of abstract syntax of language CMM.

interface Annotated {

    // The following is yet a representation of the CMM syntax
    // _before_ type checking.
    //
    // TODO: Change this to type-annotated syntax trees.
    //
    // In essence this amounts to:
    //
    //   * Adding "Type" fields to some of the constructors.
    //   * Adding a "type()" method to the "Exp" interface to give
    //     uniform constant-time access to the type of the expression.
    //   * Possibly adding an expression constructor to convert from 'int' to 'double'.
    //
    // However, you might want to also restructure expressions (and statements etc.)
    // to better fit the need of the interpreter.
    // (E.g., 'EMul' and 'EAdd' could be fused to 'EArithOp' etc.)

    // Programs

    record Program(List<Def> listdef_) {}

    // Function definitions

    record Def(Type type_, String id_, List<Arg> listarg_, List<Stm> liststm_) {}

    // Function parameters

    record Arg(Type type_, String id_) {}

    // Statements

    sealed interface Stm permits
        SDecls, SInit, SBlock,
        SExp, SReturn, SWhile, SIfElse                  {}

    record SDecls  (Type type_, List<String> listid_)   implements Stm {}
    record SInit   (Type type_, String id_, Exp exp_)   implements Stm {}
    record SBlock  (List<Stm> liststm_)                 implements Stm {}
    record SExp    (Exp exp_)                           implements Stm {}
    record SReturn (Exp exp_)                           implements Stm {}
    record SWhile  (Exp exp_, Stm stm_)                 implements Stm {}
    record SIfElse (Exp exp_, Stm stm_1, Stm stm_2)     implements Stm {}

    // Expressions

    sealed interface Exp permits
        EBool, EInt, EDouble, EId, EApp, EPost, EPre,
        EMul, EAdd, ECmp, EAnd, EOr, EAss               {}

    record EBool   (Boolean boolean_)                   implements Exp {}
    record EInt    (Integer integer_)                   implements Exp {}
    record EDouble (Double double_)                     implements Exp {}

    record EId     (String id_)                         implements Exp {}
    record EApp    (String id_, List<Exp> listexp_)     implements Exp {}

    record EPost   (String id_, IncDecOp incdecop_)     implements Exp {}
    record EPre    (IncDecOp incdecop_, String id_)     implements Exp {}

    record EMul    (Exp exp_1, MulOp mulop_, Exp exp_2) implements Exp {}
    record EAdd    (Exp exp_1, AddOp addop_, Exp exp_2) implements Exp {}
    record ECmp    (Exp exp_1, CmpOp cmpop_, Exp exp_2) implements Exp {}

    record EAnd    (Exp exp_1, Exp exp_2)               implements Exp {}
    record EOr     (Exp exp_1, Exp exp_2)               implements Exp {}

    record EAss    (String id_, Exp exp_)               implements Exp {}

    // Types and operators can be concisely represented as enums.
    // However, you could also (partially) reuse their definition in cmm.Absyn.

    // Operators

    enum IncDecOp { OInc, ODec };
    enum MulOp    { OTimes, ODiv };
    enum AddOp    { OPlus, OMinus };
    enum CmpOp    { OLt, OGt, OLtEq, OGtEq, OEq, ONEq };

    // Types

    enum Type     { Type_bool, Type_int, Type_double, Type_void };

}
