It's a type system!

Rationale
=========

CL fails to reify its type system, which I think is a damn shame. Types are dealt with only through "specifiers", which is a problem about the same way as only being able to refer to functions by their names is. The only actual operations you can carry out on them are typep and subtypep, the rest being done indirectly through further naming, e.g. conjunction through (list* 'and ...). It would be nice to have them as first-class citizens - objects.

It's natural to deal with the type-objects thus required with CLOS, and this invites subtyping. Gives you further possibilities. Some people, for instance, don't like the way array types essentially correspond to low-level storage considerations rather than the types of their elements; but defining a new type to mean "all arrays, regardless of storage type, containing only integers less than twelve" is impossible to define in CL without SATISFIES, and SATISFIES does not provide enough information to the type system (namely, subtyping) to be useful.

With CLOS type-objects, though, it's possible to define a new kind of type, with custom typep and subtypep relations.

CL also doesn't expose what facilities it has relating types to environments well. There is a macro mechanism for types (deftype), but no macroexpansion function, and no separation between symbol-macros and functional macros (e.g., CL defines (AND) but not AND, but this is not possible for a user). And no lexically available types analogous to flet/etc. Minor stuff, to be sure, but a weird omission.

So basically this library provides a new form of system introspection in CL.

API
===

Right now there are two sections of the library: the type object part, and type parsing/unparsing part.

Type operations
---------------

The type object part deals with types as types. The principal functions are

**typep** _object type_ => _generalized boolean_

As CL typep, but takes a type object rather than a specifier (and thus loses the &environment), and is a generic function.

You will probably want to define a typep for all your type-classes, except in the rare case that you want to subclass an existing type-class without changing its membership, and by "rare" I mean "I don't know why you'd ever do that".

**subtypep** _type1 type2_ => _subtype-p, valid_p_

As CL subtypep, but again without parsing/an environment, and a generic function.

There's a default method on subtypep that returns NIL NIL, i.e. uncertainty. Therefore, you can just define subtypep for your type-classes as relate to the few other type-classes you care about for your application, while leaving the rest undefined. It's incremental! Grow as you go, man.

Importantly, there are no other assumptions involved with this function. You could make a type-class that is a subtype of both CONS and INTEGER. That doesn't make any sense to me, but maybe you need that somehow. Subtyping is not even defined between instances of the same class - you can have (let ((a (make-my-type))) (subtypep a a)) return false if you want.

The major exceptions are the "universe" types, T and NIL, and the "logical" types, AND OR NOT. (subtypep type t) is assumed true, etc., and if (subtypep type type), (subtypep type (or type integer)), and so on. This will bite you with the hypothetical CONS INTEGER subtype mentioned, because (and cons integer) will collapse to NIL. You can override this by defining more specific methods, but again, weird thing to do.

As a generic function, it uses a special method combination called tri/combine. Tri/combine is a macro that takes several forms that return two values each which are to be interpreted as subtypep results are - that is, T T means true, NIL T means false, NIL NIL means unknown - and returns the first that returns true or false, or NIL NIL if there are none.

Use of method combinations in CL is pretty rare, so I'll explain this in some detail.

This is a "short form" method combination, as described in [clhs define-method-combination](http://www.lispworks.com/documentation/HyperSpec/Body/m_defi_4.htm#define-method-combination), and more or less in [clhs 7.6.6.4](http://www.lispworks.com/documentation/HyperSpec/Body/07_ffd.htm). To sum up, there are no :before and :after methods - every valid method is called until something is acceptable to tri/combine, i.e. certain under subtypep.

If you don't want to think about it, here's the really short version: write methods with "tri/combine", like so:

(defmethod subtypep tri/combine ((t1 my-type) (t2 my-other-type)) ...)

and don't use call-next-method and next-method-p. If you don't have a result, or want to punt to other methods, just return NIL NIL.

(You can use :around, by (defmethod subtypep :around (...) ...), and then call-next-method and next-method-p are valid.)

God that was long.

There is a small convenience function, type=. It is not presently generic.

**type=** _type1 type2_ => _generalized-boolean_

Determines if two types are equal based on their subtyping: a ≤ b and b ≥ a implies a = b.

There are also functions for dealing with types as sets - essentially, AND, OR, and NOT types. Default behavior is provided, so that (subtypep (and foo baz) foo) works as you'd expect if (subtypep foo foo), but it is also possible to collapse combinations into less generic type-objects. For example, if you were writing your own interval arithmetic, you would probably want (or (integer 3 7) (integer 5 9)) to become (integer 3 9). These functions are

**conjoin/2** _type1 type2_ => _conjoined-type_
**disjoin/2** _type1 type2_ => _disjoined-type_

These carry out AND and OR, respectively. The default method makes a generic conjunction or disjunction type-object.

**negate** _type_ => _negated-type_

Carries out NOT. The default method makes a generic negation-of-type-object type-object.

Type specifier parsing
----------------------

This part of the library converts type specifiers into type-objects and back. As such, unlike the pure type operations, it deals with environments.

### Main entry points

**parse-type** _type-specifier <tt>&optional</tt> environment_ => _type-object_

Parses a type specifier into a reified type, according to the names in _environment_. As with all CL functions, a NIL environment means the global environment. The "parsing" works analogously to CL evaluation:

1. The type-specifier is macroexpanded in environment, as by **typexpand**.
2. If the result is a symbol, it is looked up as a variable. The value of the binding is returned. If there is no binding, an error is signaled.
3. If the result is a cons, its car, which must be a symbol, is looked up first as a special operator and then as a function. If it is a special operator, the bound type-specifier-special-operator function receives the whole specifier and the environment as arguments, and should return a type object. If it is a function, the cdr of the specifier must be a proper list; **parse-type** is applied to each element, and then the type-specifier-function receives this list as arguments (i.e. as by **apply**).

Methods of defining all these things are below.

**typexpand** _type-specifier <tt>&optional</tt> environment_ => _type-specifier, expanded-p_

Analogous to **macroexpand**. Expands both symbol and functional macros. Second value is true if an expansion was carried out.

**typexpand-1** _type-specifier <tt>&optional</tt> environment_ => _type-specifier, expanded-p_

Analogous to **macroexpand-1**.

**unparse** _type-object_ => _type-specifier_

Generic function. Returns a _type-specifier_ that could be parsed by **parse-type** to get something equivalent in whatever sense to the _type-object_. This is used in printing type-objects, so tread carefully with errors.

### Defining type specifiers in the global environment

There are four functions to define type parses. Return values of all four are undefined, as they're intended for toplevel. I haven't decided how to expose special operators through a like interface yet.

If these don't work out for you, you can set things directly: check "generic access" below.

**deftype-function** _name lambda-list <tt>&body</tt> body_
**deftype-macro** _name lambda-list <tt>&body</tt> body_

Define type specifier functions and macros, respectively, as defined above.

WARNING: At the moment deftype-macro sucks. No default-default *, no <tt>&environment</tt>, no <tt>&whole</tt>. Just terrible.

Note that neither of these include deftype's behavior to make bare symbols work. That is, if you have for example (deftype-function foo () ...), <tt>(foo)</tt> will be a valid type specifier, but <tt>foo</tt> will not. You need to explicitly use the latter two macros.

**deftype-variable** _name value_
**deftype-symbol-macro** _name expansion_

Define type specifier variables and symbol-macros.

As an example use of these four, here's the definition of CONS type parsing:

(deftype-function cons (&optional car cdr)
  (make-instance 'cons-type :car (or car _<t>_) :cdr (or cdr _<t>_)))
(deftype-symbol-macro cons (cons t t))

### Generic access (and alternate environments)

If you're using your own environments, you sly dog you, there are generic functions available to define your own access:

**specifier-symbol-macro** _spec env_ => _macro-function__
(<tt>setf</tt> (**specifier-symbol-macro** _spec env_) _macro-function_)
**specifier-macro** _spec env_ => _macro-function_
(<tt>setf</tt> (**specifier-macro** _spec env_) _macro-function_)
**specifier-variable** _spec env_ => _type-object_
(<tt>setf</tt> (**specifier-variable* _spec env_) _type-object_)
**specifier-function** _spec env_ => _function_
(<tt>setf</tt> (**specifier-function* _spec env_) _function_)
**specifier-special** _spec env_ => _special-function_
(<tt>setf</tt> (**specifier-special** _spec env_) _special-function_)

where

* _macro-function_ is a function of two arguments, the specifier and the environment, that returns a _type-specifier_.
* _function_ is a function of however many arguments you like, all of which are _type-object_s, which returns a _type-object_.
* _special-function_ is a function of a specifier and an environment that returns a _type-object_.

Or they can all be NIL, which for a reader means there is no binding (as NIL isn't a valid return value for any of these), and for a writer means to remove the binding.

Note that the environment is not an optional argument. All of these have an :argument-precedence-order that prioritizes the environment.

CL types
========

The above discussion is intentionally generic: it's basically intensional set arithmetic with uncertainty, and un/parsing set definitions. No CL involved, except as the implementation language.

Also included, however, are definitions of CL types. These are in the global environment.

At the moment the following are defined:

* T, NIL
* AND, OR, NOT
* ARRAY, SIMPLE-ARRAY, all the fiddly little special array types
* CONS
* REAL and subtypes, including sensible interval arithmetic
* EQL, MEMBER

But it needs work.

TODO
====

Work never stops, baby.
