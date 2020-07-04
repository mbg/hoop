# hoop

A Haskell library for object-oriented programming which allows programmers to use objects in ordinary Haskell programs. In particular, the library achieves the following design objectives (to avoid ambiguity with Haskell's type classes, we refer to classes in the object-oriented sense as _object classes_):

- No extensions to the Haskell language are required beyond what is already implemented in GHC. Object classes are generated from Template Haskell quasi quotations using an OO-like syntax where the methods are defined as ordinary Haskell expressions. 
- Object classes can be instantiated from ordinary Haskell code (with an overloaded function named `new`). The resulting objects are ordinary Haskell values and can be used as such.
- Calling methods on objects can be done from within ordinary Haskell code. 
- The objects do not rely on IO. Instantiating objects and calling methods on the resulting objects is pure. 
- Object classes can inherit from other object classes, which also established subtyping relations between them. There is no limit to how deep these inheritance trees may grow.
- Class hierarchies are open for extension. I.e. the library does not need to know about all subclasses of a given class in order to generate the code for that class, allowing modular compilation.
- Casting from subtype objects to their supertypes is supported and the types are correctly reflected in Haskell's type system (e.g. assuming that we have `Duck <: Bird` and that `obj :: Duck` then `upcast obj :: Bird`) and pure.
- Type annotations are generally not required except where something would logically be ambiguous otherwise (e.g. instantiating an object with the `new` function).

## Examples

The [test](https://github.com/mbg/hoop/tree/master/test) folder contains a number of examples of the library in action, illustrating the various features. 

As a quick tutorial, a simple expression language can be implemented using the library as shown below. Note that the bodies of the two implementations of the `eval` method are ordinary Haskell expressions. The `.!` operator is an ordinary Haskell operator used to call methods on objects and `this` is just an ordinary Haskell definition, too.

```haskell
[state|
abstract state Expr where
    eval :: Int

state Val : Expr where
    data val = 0 :: Int

    eval = do
        r <- this.!val
        return r

state Add : Expr where 
    data left :: Expr 
    data right :: Expr 

    eval = do 
        x <- this.!left.!eval 
        y <- this.!right.!eval 
        return (x+y)
|]

someExpr :: Expr 
someExpr = new @Add (upcast $ new @Val 4, upcast $ new @Val 7)

result :: Int 
result = result (someExpr.!eval)
```

We can note two points of interest here that differ from popular object-oriented programming languages:

- Casts must be explicit: in the example, the objects of type `Val` must be explicitly cast to `Expr` values to instantiate the `Add` object.
- Since everything is pure, calling a method on an object produces two results: the result of the method call and a (potentially) updated object. The `result` function returns the result of calling `eval` on the `someExpr` object, discarding the resulting object.
