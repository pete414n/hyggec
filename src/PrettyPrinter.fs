// hyggec - The didactic compiler for the Hygge programming language.
// Copyright (C) 2023 Technical University of Denmark
// Author: Alceste Scalas <alcsc@dtu.dk>
// Released under the MIT license (see LICENSE.md for details)

/// Functions for pretty-printing compiler data structures (e.g. ASTs).
module PrettyPrinter

open AST


/// Newline symbol for the current operating system.
let internal nl = System.Environment.NewLine


/// Generic hierarchical representation of a tree for pretty-printing.
type internal Tree =
    /// Tree node with a description and a (possibly empty) list of subtrees.
    | Node of descr: string * subtrees: List<string * Tree>

    with
        /// Return a nice, indenter representation of the tree.  The argument
        /// 'indent' is a string (expected to only contain spaces) providing the
        /// visual indentation from the left.
        member this.Format (indent: string): string =
            match this with
            // | Node(descr, []) -> descr // No indentation
            | Node(descr, subtrees) ->
                let rec formatChildren (children: List<string * Tree>) (indent: string): string =
                    match children with
                    | [] -> ""
                    | [(descr, tree)] -> // Last child
                        let nameStr = if descr <> "" then (descr + ": ") else ""
                        let childIndent = indent + " " + (String.replicate (nameStr.Length + 1) " ")
                        indent + "┗╾" + nameStr + (tree.Format childIndent)
                    | (name, tree) :: rest ->
                        let nameStr = if name <> "" then (name + ": ") else ""
                        let childIndent = indent + "┃" + (String.replicate (nameStr.Length + 1) " ")
                        indent + "┣╾" + nameStr + (tree.Format childIndent)
                            + (formatChildren rest indent)
                descr + nl + (formatChildren subtrees indent)

        /// Return a nice, indented representation of the tree.
        override this.ToString(): string =
            this.Format ""


/// Traverse a Hygge Type and return its hierarchical representation.
let rec internal formatType (t: Type.Type): Tree =
    match t with
    | Type.TBool -> Node("bool", [])
    | Type.TInt -> Node("int", [])
    | Type.TFloat -> Node("float", [])
    | Type.TString -> Node("string", [])
    | Type.TUnit -> Node("unit", [])
    | Type.TVar(name) -> Node(name, [])


/// Traverse a Hygge typing environment and return its hierarchical
/// representation.
let rec internal formatTypingEnv (env: Typechecker.TypingEnv): List<string * Tree> =
    let formatMap (m: Map<string, Type.Type>): List<string * Tree> =
        List.map (fun (name, tpe) -> (name, formatType tpe)) (Map.toList m)
    let vars = formatMap env.Vars
    let typeVars = formatMap env.TypeVars
    let varsNode = Node((if vars.IsEmpty then "∅" else "Map"), vars)
    let typeVarsNode = Node((if typeVars.IsEmpty then "∅" else "Map"), typeVars)
    [("Env.Vars", varsNode); ("Env.TypeVars", typeVarsNode)]


/// Traverse an Hygge program AST from the given 'node' and return a
/// hierarchical representation of the AST contents.
let rec internal formatASTRec (node: AST.Node<'E,'T>): Tree =
    /// Build a pretty-printer tree with the given description, AST node,
    /// and list of descendent trees with descriptions.
    let mkTree (descr: string) (node: AST.Node<'E,'T>)
               (children: List<string * Tree>): Tree =
        Node($"%s{descr} %s{node.Pos.Format}",
             (formatNodeTypingInfo node) @ children)
    match node.Expr with
    | UnitVal -> mkTree "UnitVal ()" node []
    | IntVal(value) -> mkTree $"IntVal %d{value}" node []
    | BoolVal(value) -> mkTree $"BoolVal %b{value}" node []
    | FloatVal(value) -> mkTree $"FloatVal %f{value}" node []
    | StringVal(value) -> mkTree $"StringVal \"%s{value}\"" node []
    | Var(name) -> mkTree $"Var %s{name}" node []
    | Mult(lhs, rhs) ->
        mkTree "Mult" node [("lhs", formatASTRec lhs)
                            ("rhs", formatASTRec rhs)]
    | Add(lhs, rhs) ->
        mkTree "Add" node [("lhs", formatASTRec lhs)
                           ("rhs", formatASTRec rhs)]                           
    | Sub(lhs, rhs) ->
        mkTree "Sub" node [("lhs", formatASTRec lhs)
                           ("rhs", formatASTRec rhs)]
    | And(lhs, rhs) ->
        mkTree "And" node [("lhs", formatASTRec lhs)
                           ("rhs", formatASTRec rhs)]
    | Or(lhs, rhs) ->
        mkTree "Or" node [("lhs", formatASTRec lhs)
                          ("rhs", formatASTRec rhs)]
    | Not(arg) ->
        mkTree "Not" node [("arg", formatASTRec arg)]
    | Eq(lhs, rhs) ->
        mkTree "Eq" node [("lhs", formatASTRec lhs)
                          ("rhs", formatASTRec rhs)]
    | Less(lhs, rhs) ->
        mkTree "Less" node [("lhs", formatASTRec lhs)
                            ("rhs", formatASTRec rhs)]
    | ReadInt -> mkTree "ReadInt" node []
    | ReadFloat -> mkTree "ReadFloat" node []
    | Print(arg) ->
        mkTree "Print" node [("arg", formatASTRec arg)]
    | PrintLn(arg) ->
        mkTree "PrintLn" node [("arg", formatASTRec arg)]
    | If(condition, ifTrue, ifFalse) ->
        mkTree "Conditional" node [("condition", formatASTRec condition);
                                   ("ifTrue", formatASTRec ifTrue)
                                   ("ifFalse", formatASTRec ifFalse)]
    | Seq(nodes) ->
        let children = List.map (fun n -> ("", formatASTRec n)) nodes
        mkTree "Seq" node children
    | Ascription(tpe, node) ->
        mkTree $"Ascription" node [("Ascription", formatPretypeNode tpe)
                                   ("node", formatASTRec node)]
    | Let(name, tpe, init, scope) ->
        mkTree $"Let %s{name}" node [("Ascription", formatPretypeNode tpe)
                                     ("init", formatASTRec init)
                                     ("scope", formatASTRec scope)]
    | Assertion(arg) ->
        mkTree "Assertion" node [("arg", formatASTRec arg)]
    | Type(name, def, scope) ->
        mkTree $"Type %s{name}" node [("def", formatPretypeNode def)
                                      ("scope", formatASTRec scope)]

/// Return a description of an AST node, and possibly some subtrees (that are
/// added to the overall tree structure).
and internal formatNodeTypingInfo (node: Node<'E,'T>): List<string * Tree> =
    let envChildren =
        match typeof<'E> with
        | t when t = typeof<unit> -> [] // Nothing to show
        | t when t = typeof<Typechecker.TypingEnv> ->
            formatTypingEnv ((node.Env :> obj) :?> Typechecker.TypingEnv)
        | t -> failwith $"BUG: unsupported AST environment type for pretty-printing: %O{t}"
    let typeChildren =
        match typeof<'T> with
        | t when t = typeof<unit> -> [] // Nothing to show
        | t when t = typeof<Type.Type> ->
            [("Type", formatType ((node.Type :> obj) :?> Type.Type))]
        | t -> failwith $"BUG: unsupported AST type argument for pretty-printing: %O{t}"
    envChildren @ typeChildren

/// Format a list of children of an AST node.  Each child is a pair with a
/// descriptive name (which may be empty) and an AST node.
and internal formatChildren<'E,'T> (children: list<string * Node<'E,'T>>): List<string * Tree> =
    List.map (fun (descr, node) -> (descr, formatASTRec node)) children

/// Traverse the Abstract Syntax Tree of a Hygge pretype from the given node,
/// and return a string containing a readable representation of the pretype AST
/// contents. 'indent' is a string (expected to contain only spaces) providing
/// the visual indentation from the left.
and internal formatPretypeNode (node: PretypeNode): Tree =
    match node.Pretype with
    | Pretype.TId(id) ->
        Node((formatPretypeDescr node $"Pretype Id \"%s{id}\""), [])

/// Format the description of a pretype AST node (without printing its
/// children).
and internal formatPretypeDescr (node: PretypeNode) (descr: string) : string =
    $"%s{descr}; pos: %s{node.Pos.Format}"

 /// Format the an AST node with a list of children.
and internal formatPretypeNodeWithChildren (node: PretypeNode) (descr: string)
                                           (children: list<string * PretypeNode>): Tree =
    Node((formatPretypeDescr node descr), (formatPretypeChildren children))

/// Format a list of children of an AST node.  Each child is a pair with a
/// descriptive name (which may be empty) and an AST node.  'indent' is a string
/// (expected to only contain spaces) providing the visual indentation from the
/// left.
and internal formatPretypeChildren (children: list<string * PretypeNode>): List<string * Tree> =
    List.map (fun (descr, node) -> (descr, formatPretypeNode node)) children


/// Return a compact but readable representation of the AST.
let prettyPrint<'E,'T> (node: Node<'E,'T>): string =
    (formatASTRec node).ToString()
