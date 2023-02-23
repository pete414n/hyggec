// hyggec - The didactic compiler for the Hygge programming language.
// Copyright (C) 2023 Technical University of Denmark
// Author: Alceste Scalas <alcsc@dtu.dk>
// Released under the MIT license (see LICENSE.md for details)

/// Type definitions for the Abstract Syntax Tree of Hygge.
module AST


/// Position of an AST element, with line/column numbers starting with 1.
[<RequireQualifiedAccess>]
type Position =
    { /// The name of the file being parsed.
      FileName: string
      /// "Main" line of the AST elements, used e.g. to report typing errors.
      Line: int
      /// "Main" column of the AST elements, used e.g. to report typing errors.
      Col: int
      /// Line where the AST element starts.
      LineStart: int
      /// Column where the AST element starts.
      ColStart: int
      /// Line where the AST element ends.
      LineEnd: int
      /// Column where the AST element ends.
      ColEnd: int
    }
    with
        /// Return a comoact string representation of a position in the input
        /// source file.
        member this.Format =
           $"(%d{this.LineStart}:%d{this.ColStart}-%d{this.LineEnd}:%d{this.ColEnd})"


/// Node of the Abstract Syntex Tree of a 'pretype', i.e. something that
/// syntactically looks like a Hygge type (found e.g. in type ascriptions).
[<RequireQualifiedAccess>]
type PretypeNode =
    { /// Position of the pretype Abstract Syntax Tree node in the source file.
      Pos: Position
      /// Pretype contained in this Abstract Syntax Tree node.
      Pretype: Pretype
    }

/// Hygge pretype represented in an Abstract Syntax Tree.
and Pretype =
    /// A type identifier.
    | TId of id: string


/// Node of the Abstract Syntax Tree of a Hygge expression.  The meaning of the
/// two type arguments is the following: 'E specifies what typing environment
/// information is associated to each expression in the AST; 'T specifies what
/// type information is assigned to each expression in the AST.
[<RequireQualifiedAccess>]
type Node<'E,'T> = 
    { /// Hygge expression contained in the AST node.
      Expr: Expr<'E,'T>
      /// Position in the source file of the expression in this AST node.
      Pos: Position
      /// Typing environment used to type-check the expression in this AST node.
      Env: 'E
      /// Type assigned to the expression in this AST node.
      Type: 'T
    }


/// Hygge expression represented in an AST. The two type arguments have the same
/// meaning described in 'Node' above.
and Expr<'E,'T> =
    /// Unit value.
    | UnitVal

    /// Integer value.
    | BoolVal of value: bool

    /// Integer value.
    | IntVal of value: int

    /// Floating-point constant (single-precision, a.k.a. float32).
    | FloatVal of value: single

    /// String value.
    | StringVal of value: string

    /// Variable name.
    | Var of name: string

    /// Addition between lhs and rhs.
    | Add of lhs: Node<'E,'T>
           * rhs: Node<'E,'T>

    /// Subtraction between lhs and rhs.
    | Sub of lhs: Node<'E,'T>
           * rhs: Node<'E,'T>

    /// Multiplication between lhs and rhs.
    | Mult of lhs: Node<'E,'T>
            * rhs: Node<'E,'T>

    /// Logical and between lhs and rhs.
    | And of lhs: Node<'E,'T>
           * rhs: Node<'E,'T>

    /// Logical or between lhs and rhs.
    | Or of lhs: Node<'E,'T>
          * rhs: Node<'E,'T>

    /// Logical not
    | Not of arg: Node<'E,'T>

    /// Comparison: is the lhs equal to the rhs?
    | Eq of lhs: Node<'E,'T>
          * rhs: Node<'E,'T>

    /// Comparison: is the lhs less than the rhs?
    | Less of lhs: Node<'E,'T>
            * rhs: Node<'E,'T>

    /// Read an integer value from the console.
    | ReadInt

    /// Read a floating-point value from the console.
    | ReadFloat

    /// Print the result of the 'Arg' expression on the console.
    | Print of arg: Node<'E,'T>

    /// Print the result of the 'Arg' expression on the console, with a final
    /// newline.
    | PrintLn of arg: Node<'E,'T>

    /// Conditional expression (if ... then ... else ...).
    | If of condition: Node<'E,'T>
          * ifTrue: Node<'E,'T>
          * ifFalse: Node<'E,'T>

    /// Sequence of expressions.
    | Seq of nodes: List<Node<'E,'T>>

    /// Type ascription: an expression with an explicit type annotation.
    | Ascription of tpe: PretypeNode
                  * node: Node<'E,'T>

    /// Let-binder, used to introduce a variable with the given 'name' and type
    /// ('tpe') in a 'scope'.  The variable is initialised with the result of
    /// the expression in 'init', and can be mutable.
    | Let of name: string
           * tpe: PretypeNode
           * init: Node<'E,'T>
           * scope: Node<'E,'T>

    /// Assertion: fail at runtime if the argument does not evaluate to true.
    | Assertion of arg: Node<'E,'T>

    /// Type alias: the type called 'name' is defined as 'def' and is usable in
    /// the given 'scope'.
    | Type of name: string
            * def: PretypeNode
            * scope: Node<'E,'T>


/// A type alias for an untyped AST, where there is no typing environment nor
/// typing information (unit).
type UntypedAST = Node<unit, unit>


/// A type alias for an untyped expression within an untyped AST, where there is
/// no typing environment nor typing information (unit).
type UntypedExpr = Expr<unit, unit>
