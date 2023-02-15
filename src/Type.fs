// hyggec - The didactic compiler for the Hygge programming language.
// Copyright (C) 2023 Technical University of Denmark
// Author: Alceste Scalas <alcsc@dtu.dk>
// Released under the MIT license (see LICENSE.md for details)

/// Definition of a type in the hyggec compiler.
module Type


/// Representation of a type.  This is essentially the abstract syntax tree of a
/// type as it appears in a Hygge program (not to be confused with the
/// 'TypedAST' of a whole Hygge program).  The type argument I determines how
/// type identifiers are represented inside the type tree.
type Type =
    /// Boolean type.
    | TBool
    /// Integer type.
    | TInt
    /// Floating-point type (single-precision).
    | TFloat
    /// String type.
    | TString
    /// Unit type.
    | TUnit
    /// Type variable.
    | TVar of name: string

    /// Returns a human-readable string describing the type.
    override this.ToString(): string =
        match this with
        | TBool -> "bool"
        | TInt -> "int"
        | TFloat -> "float"
        | TString -> "string"
        | TUnit -> "unit"
        | TVar(name) -> name


/// List of basic types known by the compiler.  NOTE: this list must be kept in
/// sync with the definition of 'Type'.
let basicTypes = [TBool; TInt; TFloat; TString; TUnit]


/// Set of free type variables in a type.
let rec freeTypeVars (t: Type): Set<string> =
    match t with
    | TBool
    | TInt
    | TFloat
    | TString
    | TUnit -> Set[]
    | TVar(name) -> Set[name]
