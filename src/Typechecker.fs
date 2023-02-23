// hyggec - The didactic compiler for the Hygge programming language.
// Copyright (C) 2023 Technical University of Denmark
// Author: Alceste Scalas <alcsc@dtu.dk>
// Released under the MIT license (see LICENSE.md for details)

/// Type definitions and functions for type-checking an untyped Hygge AST, and
/// translating it into a typed AST.
module Typechecker

open AST
open Type


/// Representation of typing errors
type TypeErrors = list<Position * string>


/// A typing environment, with information used for typing a program expression.
type TypingEnv = {
    /// Mapping from the variables names in the current scope to their type.
    Vars: Map<string, Type>
    /// Mapping from type aliases in the current scope to their definition.
    TypeVars: Map<string, Type>
} with
    /// Return a compact and readable representation of the typing environment.
    override this.ToString(): string =
                 "{" + $"vars: %s{Util.formatMap this.Vars}; "
                     + $"types: %s{Util.formatMap this.TypeVars}" + "}"


/// A type alias for a typed AST, where there is a typing environment and typing
/// information in each node.
type TypedAST = AST.Node<TypingEnv, Type>


/// A type alias for a typed expression within a typed AST, where there is a
/// typing environment and typing information in each node.
type TypedExpr = AST.Expr<TypingEnv, Type>


/// Result of a typing computation: a typed AST, or a list of errors with
/// positions.
type TypingResult = Result<TypedAST, TypeErrors>


/// Auxiliary function that takes 2 Results, combines their Error contents into
/// a single Error instance, and returns it.  This function expects that at
/// least one of the two Results is an Error.
let internal mergeErrors (r1: Result<'A, TypeErrors>, r2: Result<'A, TypeErrors>): Result<'B, TypeErrors> =
    match (r1, r2) with
    | (Ok(_), Error(es)) -> Error(es)
    | (Error(es), Ok(_)) -> Error(es)
    | (Error(es1), Error(es2)) -> Error(es1 @ es2)
    | (ok1, ok2) -> failwith $"BUG: expecting at least one Error, got %O{ok1}, %O{ok2}"


/// Retrieve a list of all errors from a list of results.
let internal collectErrors (rs: List<Result<'R, List<'E>>>): List<'E> =
    let getError (x: Result<'R, List<'E>>): List<'E> = match x with
                                                       | Ok(_) -> []
                                                       | Error(es) -> es
    List.collect id (List.map getError rs)


/// Get an Ok value from a Result, and fail immediately if it is an Error.
let internal getOkValue (x: Result<'R,'E>): 'R =
    match x with
    | Ok(t) -> t
    | Error(es) -> failwith $"BUG: unexpected error: %O{es}"


/// Transform the given pretype into a full-fledget type, if possible, using the
/// given environment.  Return the resulting Type, or errors.
let rec internal resolvePretype (env: TypingEnv) (pt: AST.PretypeNode): Result<Type, TypeErrors> =
    match pt.Pretype with
    | Pretype.TId(name) ->
        match (lookupTypeVar env name) with
        | Some(t) -> Ok(t)
        | None -> Error([(pt.Pos, $"reference to undefined type: %s{name}")])


/// Resolve a type variable using the given typing environment: optionally
/// return the Type corresponding to variable 'name', or None if 'name' is not
/// defined in the given environment.
and internal lookupTypeVar (env: TypingEnv) (name: string): Option<Type> =
    // Mapping between type names and known basic types
    let btmap = Map (List.map (fun t -> (t.ToString(), t)) Type.basicTypes)
    match (btmap.TryFind name) with
    | Some(t) -> Some(t)
    | None ->
        // Let's check whether we are dealing with a type alias.  Note that we
        // do *not* recursively resolve the type alias with its definition
        match (env.TypeVars.TryFind(name)) with
        | Some(_) -> Some(TVar(name))
        | None -> None


/// Expand the given type 't' according to the given typing 'env'ironment.  If
/// the given type is a type variable, perform a recursive look-up in the
/// environment, until its actual type definition (i.e. a type that is not just
/// a type variable) is reached and returned.  If the given type is not a type
/// variable, it is just returned immediately.
let rec expandType (env: TypingEnv) (t: Type): Type =
    match t with
    | TVar(name) ->
        // Recursive look-up. Crash immediately if 'name' is not in 'env'
        expandType  env (env.TypeVars.[name])
    | other -> other


/// Check whether 't1' is subtype of 't2' in the typing environment 'env'.
let rec isSubtypeOf (env: TypingEnv) (t1: Type) (t2: Type): bool =
    match (t1, t2) with
    | (t1, t2) when t1 = t2
        -> true // Straightforward equality between types
    | (TVar(name), t2) ->
        // Expand the type variable; crash immediately if 'name' is not in 'env'
        isSubtypeOf env (env.TypeVars.[name]) t2
    | (t1, TVar(name)) ->
        // Expand the type variable; crash immediately if 'name' is not in 'env'
        isSubtypeOf env t1 (env.TypeVars.[name])
    | (_, _) -> false


/// Perform type checking on an untyped AST, using the given typing environment.
/// Return a well-typed AST in case of success, or a sequence of error messages
/// in case of failure.
let rec internal typer (env: TypingEnv) (node: UntypedAST): TypingResult =
    match node.Expr with
    | UnitVal ->
        Ok { Pos = node.Pos; Env = env; Type = TUnit; Expr = UnitVal }
    | BoolVal(v) ->
        Ok { Pos = node.Pos; Env = env; Type = TBool; Expr = BoolVal(v) }
    | IntVal(v) ->
        Ok { Pos = node.Pos; Env = env; Type = TInt; Expr = IntVal(v) }
    | FloatVal(v) ->
        Ok { Pos = node.Pos; Env = env; Type = TFloat; Expr = FloatVal(v) }
    | StringVal(v) ->
        Ok { Pos = node.Pos; Env = env; Type = TString; Expr = StringVal(v) }

    | Var(name) ->
        match (env.Vars.TryFind name) with
        | Some(tpe) ->
            Ok { Pos = node.Pos; Env = env; Type = tpe; Expr = Var(name) }
        | None ->
            Error([(node.Pos, $"undefined variable: %s{name}")])

    | Add(lhs, rhs) ->
        match (binaryNumericalOpTyper "addition" node.Pos env lhs rhs) with
        | Ok(tpe, tlhs, trhs) ->
            Ok { Pos = node.Pos; Env = env; Type = tpe; Expr = Add(tlhs, trhs) }
        | Error(es) -> Error(es)

    | Sub(lhs, rhs) ->
        match (binaryNumericalOpTyper "subtraction" node.Pos env lhs rhs) with
        | Ok(tpe, tlhs, trhs) ->
            Ok { Pos = node.Pos; Env = env; Type = tpe; Expr = Sub(tlhs, trhs) }
        | Error(es) -> Error(es)

    | Mult(lhs, rhs) ->
        match (binaryNumericalOpTyper "multiplication" node.Pos env lhs rhs) with
        | Ok(tpe, tlhs, trhs) ->
            Ok { Pos = node.Pos; Env = env; Type = tpe; Expr = Mult(tlhs, trhs) }
        | Error(es) -> Error(es)

    | And(lhs, rhs) ->
        match (binaryBooleanOpTyper "and" node.Pos env lhs rhs) with
        | Ok(tlhs, trhs) ->
            Ok { Pos = node.Pos; Env = env; Type = TBool; Expr = And(tlhs, trhs) }
        | Error(es) -> Error(es)

    | Or(lhs, rhs) ->
        match (binaryBooleanOpTyper "or" node.Pos env lhs rhs) with
        | Ok(tlhs, trhs) ->
            Ok { Pos = node.Pos; Env = env; Type = TBool; Expr = Or(tlhs, trhs) }
        | Error(es) -> Error(es)

    | Not(arg) ->
        match (typer env arg) with
        | Ok(targ) when (isSubtypeOf env targ.Type TBool) ->
            Ok { Pos = node.Pos; Env = env; Type = TBool; Expr = Not(targ) }
        | Ok(arg) ->
            Error([(node.Pos, $"logical 'not': expected argument of type %O{TBool}, "
                              + $"found %O{arg.Type}")])
        | Error(es) -> Error(es)

    | Eq(lhs, rhs) ->
        match (numericalRelationTyper "equal to" node.Pos env lhs rhs) with
        | Ok(tlhs, trhs) ->
            Ok { Pos = node.Pos; Env = env; Type = TBool; Expr = Eq(tlhs, trhs) }
        | Error(es) -> Error(es)

    | Less(lhs, rhs) ->
        match (numericalRelationTyper "less than" node.Pos env lhs rhs) with
        | Ok(tlhs, trhs) ->
            Ok { Pos = node.Pos; Env = env; Type = TBool; Expr = Less(tlhs, trhs) }
        | Error(es) -> Error(es)

    | ReadInt ->
        Ok {Pos = node.Pos; Env = env; Type = TInt; Expr = ReadInt}

    | ReadFloat ->
        Ok {Pos = node.Pos; Env = env; Type = TFloat; Expr = ReadFloat}

    | Print(arg) ->
        match (printArgTyper "print" node.Pos env arg) with
        | Ok(targ) -> Ok {Pos = node.Pos; Env = env; Type = TUnit; Expr = Print(targ)}
        | Error(es) -> Error(es)

    | PrintLn(arg) ->
        match (printArgTyper "println" node.Pos env arg) with
        | Ok(targ) -> Ok {Pos = node.Pos; Env = env; Type = TUnit; Expr = PrintLn(targ)}
        | Error(es) -> Error(es)

    | If(cond, ifT, ifF) ->
        match (typer env cond) with
        | Ok(tcond) when (isSubtypeOf env tcond.Type TBool) ->
            match ((typer env ifT), (typer env ifF)) with
            | (Ok(tifT), Ok(tifF)) when (isSubtypeOf env tifT.Type tifF.Type) ->
                Ok { Pos = node.Pos; Env = env; Type = tifT.Type;
                     Expr = If(tcond, tifT, tifF) }
            | (Ok(tifT), Ok(tifF)) when (isSubtypeOf env tifF.Type tifT.Type) ->
                Ok { Pos = node.Pos; Env = env; Type = tifF.Type;
                     Expr = If(tcond, tifT, tifF) }
            | (Ok(tifT), Ok(tifF)) ->
                Error([(node.Pos, $"mismatching 'then' and 'else' types: "
                               + $"%O{tifT.Type} and %O{tifF.Type}")])
            | otherwise -> mergeErrors otherwise
        | Ok(tcond) ->
            Error([(cond.Pos, $"'if' condition: expected type %O{TBool}, "
                              + $"found %O{tcond.Type}")])
        | Error(es) -> Error(es)

    | Seq(nodes) ->
        // We type-check all nodes, then see whether there is any error
        let typingResults = List.map (fun n -> typer env n) nodes
        let errors = collectErrors typingResults
        if errors.IsEmpty then
            let typedNodes = List.map getOkValue typingResults
            let typing = match (List.tryLast typedNodes) with
                         | Some(n) -> n.Type // Take the typing of last node
                         | None -> TUnit // Empty sequence
            Ok {Pos = node.Pos; Env = env; Type = typing; Expr = Seq(typedNodes)}
        else Error(errors)
    
    | Ascription(ascr, expr) ->
        let tascr = resolvePretype env ascr
        let texpr = typer env expr
        match (tascr, texpr) with
        | (Ok(tascr), Ok(texpr)) when (isSubtypeOf env (texpr.Type) tascr) ->
            Ok { Pos = node.Pos; Env = env; Type = tascr; Expr = Ascription(ascr, texpr) }
        | (Ok(tascr), Ok(texpr)) ->
            Error([(node.Pos, $"expression type %O{texpr.Type} does not match "
                              + $"ascription type %O{tascr}")])
        | (Ok(_), Error(es)) -> Error(es)
        | (Error(es), tn) ->
            let terrs = match tn with
                        | Ok(_) -> []
                        | Error(es2) -> es @ es2
            Error(terrs @ [(node.Pos, $"ascription with unknown type %O{ascr}")])

    | Let(name, tpe, init, scope) ->
        match (resolvePretype env tpe) with
        | Ok(letVariableType) ->
            /// Variables and types to type-check the 'let...' scope: we add the
            /// newly-declared variable and its type to the typing environment
            let envVars2 = env.Vars.Add(name, letVariableType)
            /// Environment to type-check the 'let...' scope
            let env2 = {env with Vars = envVars2}
            /// Restult of typing the 'let...' scope
            let tscope = typer env2 scope
            /// Environment for type-checking the 'let...' initialisation
            let initEnv = env // Equal to 'env'... for now ;-)
            match (typer initEnv init) with
            | Ok(tinit) ->
                /// Errors (if any) due to 'let...' initialisation type mismatch
                let terrs =
                    if not (isSubtypeOf env tinit.Type letVariableType)
                        then [(node.Pos, $"variable '%s{name}' of type %O{letVariableType} "
                                         + $"initialized with expression of type %O{tinit.Type}")]
                        else []
                match tscope with
                | Ok(tscope) ->
                    if (List.isEmpty terrs)
                        then Ok { Pos = node.Pos; Env = env; Type = tscope.Type;
                                  Expr = Let(name, tpe, tinit, tscope) }
                        else Error(terrs)
                | Error(es) -> Error(terrs @ es)
            | Error(esd) ->
                match tscope with
                | Ok(_) -> Error(esd)
                | Error(esb) -> Error(esd @ esb)
        | Error(es) -> Error(es)

    | Assertion(arg) -> 
        match (typer env arg) with
        | Ok(targ) when (isSubtypeOf env targ.Type TBool) ->
            Ok { Pos = node.Pos; Env = env; Type = TUnit; Expr = Assertion(targ) }
        | Ok(targ) ->
            Error([(node.Pos, $"assertion: expected argument of type %O{TBool}, "
                              + $"found %O{targ.Type}")])
        | Error(es) -> Error(es)

    | Type(name, def, scope) ->
        // List of known basic type identifiers
        let basicTypeNames = List.map (fun t -> t.ToString()) Type.basicTypes
        if List.contains name basicTypeNames then
            Error([(node.Pos, $"cannot redefine basic type '%s{name}'")])
        else
            match def.Pretype with
            | Pretype.TId(tname) when tname = name ->
                // The type definition is something like:  type T = T
                Error([(node.Pos, $"invalid recursive definition for type %s{name}")])
            | _ ->
                // We disallow the redefinition of type aliases.  This avoids
                // tricky corner cases and simplifies the handling of typing
                // environments.
                match (lookupTypeVar env name) with
                | Some(_) ->
                    Error([(node.Pos, $"type '%s{name}' is already defined")])
                | None ->
                    match (resolvePretype env def) with
                    | Ok(resDef) ->
                        /// Environment to type-check the 'scope' of the type
                        /// variable.  We add the new type variable to this
                        /// environment, mapped to the resolved type definition.
                        let scopeEnv =
                            {env with TypeVars = env.TypeVars.Add(name, resDef)}
                        match (typer scopeEnv scope) with
                        | Ok(tscope) ->
                            // We now need to check that the return type of the
                            // 'scope' of this type definition is also valid
                            // _outside_ the type definition, i.e. the return
                            // type does not capture the type variable being
                            // defined.  To this end, we expand the return type,
                            // and check whether the type variable being defined
                            // still occurs in it.

                            /// Expanded return type of the 'scope' expression.
                            let scopeType = expandType scopeEnv tscope.Type
                            /// Set of free type variables in the 'scope' type.
                            let scopeTypeFV = freeTypeVars scopeType
                            if (scopeTypeFV.Contains name) then
                                Error([(scope.Pos,
                                        $"type variable '%s{name} exits its scope")])
                            else
                               Ok {Pos = node.Pos; Env = env; Type = scopeType;
                                   Expr = Type(name, def, tscope)}
                        | Error(es) -> Error(es)
                    | Error(es) -> Error(es)

/// Compute the typing of a binary numerical operation, by computing and
/// combining the typings of the 'lhs' and 'rhs'.  The argument 'descr' (used in
/// error messages) specifies which expression is being typed, while 'pos'
/// specifies its position.  In case the 'lhs' and 'rhs' have the same
/// (numerical) type, return a tuple containing the type of the resulting
/// numerical expression, and the typed ASTs of the 'lhs' and 'rhs'.  Otherwise,
/// return type errors.
and internal binaryNumericalOpTyper descr pos (env: TypingEnv)
                                    (lhs: UntypedAST)
                                    (rhs: UntypedAST): Result<Type * TypedAST * TypedAST, TypeErrors> =
    let tlhs = typer env lhs
    let trhs = typer env rhs
    match (tlhs, trhs) with
    | (Ok(ln), Ok(rn)) when (isSubtypeOf env ln.Type TInt)
                            && (isSubtypeOf env rn.Type TInt) ->
        Ok(TInt, ln, rn)
    | (Ok(ln), Ok(rn)) when (isSubtypeOf env ln.Type TFloat)
                            && (isSubtypeOf env rn.Type TFloat) ->
        Ok(TFloat, ln, rn)
    | (Ok(t1), Ok(t2)) ->
        Error([(pos, $"%s{descr}: expected arguments of a same type "
                     + $"between %O{TInt} or %O{TFloat}, "
                     + $"found %O{t1.Type} and %O{t2.Type}")])
    | otherwise -> mergeErrors otherwise

/// Perform the typing of a binary logical operation, by computing the typings
/// of the 'lhs' and 'rhs'.  The argument 'descr' (used in error messages)
/// specifies which expression is being typed, while 'pos' specifies its
/// position.  In case the 'lhs' and 'rhs' have type Bool, return a tuple
/// containing the typed ASTs of the 'lhs' and 'rhs'. Otherwise, return type
/// errors.
and internal binaryBooleanOpTyper descr pos (env: TypingEnv)
                                  (lhs: UntypedAST)
                                  (rhs: UntypedAST): Result<TypedAST * TypedAST, TypeErrors> =
    let tlhs = typer env lhs
    let trhs = typer env rhs
    match (tlhs, trhs) with
    | (Ok(ln), Ok(rn)) when (isSubtypeOf env ln.Type TBool)
                            && (isSubtypeOf env rn.Type TBool) ->
        Ok(ln, rn)
    | (Ok(t1), Ok(t2)) ->
        Error([(pos, $"logical '%s{descr}': expected arguments of type %O{TBool}, "
                     + $"found %O{t1.Type} and %O{t2.Type}")])
    | otherwise -> mergeErrors otherwise

/// Perform the typing of a relation between numerical values, by computing the
/// typings of the 'lhs' and 'rhs'.  The argument 'descr' (used in error
/// messages) specifies which expression is being typed, while 'pos' specifies
/// its position.  In case the 'lhs' and 'rhs' have the same (numerical) type,
/// return a tuple containing the typed ASTs of the 'lhs' and 'rhs'. Otherwise,
/// return type errors.
and internal numericalRelationTyper descr pos (env: TypingEnv)
                                    (lhs: UntypedAST)
                                    (rhs: UntypedAST): Result<TypedAST * TypedAST, TypeErrors> =
    let tlhs = typer env lhs
    let trhs = typer env rhs
    match (tlhs, trhs) with
    | (Ok(ln), Ok(rn)) when (isSubtypeOf env ln.Type TInt)
                            && (isSubtypeOf env rn.Type TInt) ->
        Ok(ln, rn)
    | (Ok(ln), Ok(rn)) when (isSubtypeOf env ln.Type TFloat)
                            && (isSubtypeOf env rn.Type TFloat) ->
        Ok(ln, rn)
    | (Ok(t1), Ok(t2)) ->
        Error([(pos, $"relation '%s{descr}': expected arguments of a same type "
                     + $"between %O{TInt} or %O{TFloat}, "
                     + $"found %O{t1.Type} and %O{t2.Type}")])
    | otherwise -> mergeErrors otherwise

/// Perform the typing of the argument of a 'print' or 'println' expression at
/// the given 'pos'ition, using the given 'env'ironment.  The argument 'descr'
/// (used in error messages) specifies which expression is being typed, while
/// 'pos' specifies its position.  Return a typed argument in case of success.
/// Otherwise, return type errors.
and internal printArgTyper descr pos (env: TypingEnv) (arg: UntypedAST): Result<TypedAST, TypeErrors> =
    /// Types of values that can be printed.
    let printables = [TBool; TInt; TFloat; TString]
    match (typer env arg) with
    | Ok(targ) when List.exists (isSubtypeOf env targ.Type) printables ->
        Ok(targ)
    | Ok(targ)->
        Error([(pos, $"%s{descr}: expected argument of a type among "
                        + $"%s{Util.formatAsSet printables}, found %O{targ}")])
    | Error(es) -> Error(es)


/// Perform type checking of the given untyped AST.  Return a well-typed AST in
/// case of success, or a sequence of error messages in case of failure.
let typecheck (node: UntypedAST): TypingResult =
    typer {Vars = Map[]; TypeVars = Map[]} node
