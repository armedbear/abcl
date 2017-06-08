<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# Named Readtables Manual

## Table of Contents

- [1 named-readtables ASDF System Details][9b5b]
- [2 Introduction][6faf]
    - [2.1 Links][8688]
    - [2.2 Acknowledgements][059d]
- [3 Overview][0bc2]
    - [3.1 Notes on the API][e4cd]
    - [3.2 Important API idiosyncrasies][62b8]
    - [3.3 Preregistered Readtables][58c6]
    - [3.4 Examples][cf94]
- [4 Reference][373d]

###### \[in package EDITOR-HINTS.NAMED-READTABLES\]
<a name='x-28-22named-readtables-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 named-readtables ASDF System Details

- Version: 0.9
- Description: Library that creates a namespace for named readtable
  akin to the namespace of packages.
- Licence: BSD, see LICENSE
- Author: Tobias C. Rittweiler <trittweiler@common-lisp.net>
- Maintainer: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-INTRODUCTION-20MGL-PAX-3ASECTION-29'></a>

## 2 Introduction

Named-Readtables is a library that provides a namespace for
readtables akin to the already-existing namespace of packages. In
particular:

- you can associate readtables with names, and retrieve
  readtables by names;

- you can associate source files with readtable names, and be
  sure that the right readtable is active when compiling/loading
  the file;

- similiarly, your development environment now has a chance to
  automatically determine what readtable should be active while
  processing source forms on interactive commands. (E.g. think of
  `C-c C-c` in Slime (yet to be done))

It follows that Named-Readtables is a facility for using readtables in
a localized way.

Additionally, it also attempts to become a facility for using
readtables in a *modular* way. In particular:

- it provides a macro to specify the content of a readtable at a
 glance;

- it makes it possible to use multiple inheritance between readtables.


<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-LINKS-20MGL-PAX-3ASECTION-29'></a>

### 2.1 Links

Here is the [official repository][named-readtables-repo] and the
[HTML documentation][named-readtables-doc] for the latest version.

[named-readtables-repo]: https://github.com/melisgl/named-readtables 

[named-readtables-doc]: http://melisgl.github.io/mgl-pax-world/named-readtables-manual.html 


<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-ACKNOWLEDGEMENTS-20MGL-PAX-3ASECTION-29'></a>

### 2.2 Acknowledgements

Thanks to Robert Goldman for making me want to write this library.

Thanks to Stephen Compall, Ariel Badichi, David Lichteblau, Bart
Botta, David Crawford, and Pascal Costanza for being early adopters,
providing comments and bugfixes.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-OVERVIEW-20MGL-PAX-3ASECTION-29'></a>

## 3 Overview

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-API-NOTES-20MGL-PAX-3ASECTION-29'></a>

### 3.1 Notes on the API

The API heavily imitates the API of packages. This has the nice
property that any experienced Common Lisper will take it up without
effort.

    DEFREADTABLE              -   DEFPACKAGE
    
    IN-READTABLE              -   IN-PACKAGE
    
    MERGE-READTABLES-INTO     -   USE-PACKAGE
    
    MAKE-READTABLE            -   MAKE-PACKAGE
    
    UNREGISTER-READTABLE      -   DELETE-PACKAGE
    
    RENAME-READTABLE          -   RENAME-PACKAGE
    
    FIND-READTABLE            -   FIND-PACKAGE
    
    READTABLE-NAME            -   PACKAGE-NAME
    
    LIST-ALL-NAMED-READTABLES -   LIST-ALL-PACKAGES


<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-API-IDIOSYNCRASIES-20MGL-PAX-3ASECTION-29'></a>

### 3.2 Important API idiosyncrasies

There are three major differences between the API of Named-Readtables,
and the API of packages.

1. Readtable names are symbols not strings.

    Time has shown that the fact that packages are named by strings
    causes severe headache because of the potential of package names
    colliding with each other.

    Hence, readtables are named by symbols lest to make the
    situation worse than it already is. Consequently, readtables
    named `CL-ORACLE:SQL-SYNTAX` and `CL-MYSQL:SQL-SYNTAX` can
    happily coexist next to each other. Or, taken to an extreme,
    `SCHEME:SYNTAX` and `ELISP:SYNTAX`.

    If, for example to duly signify the importance of your cool
    readtable hack, you really think it deserves a global name, you
    can always resort to keywords.

2. The inheritance is resolved statically, not dynamically.

    A package that uses another package will have access to all the
    other package's exported symbols, even to those that will be
    added after its definition. I.e. the inheritance is resolved at
    run-time, that is dynamically.

    Unfortunately, we cannot do the same for readtables in a
    portable manner.

    Therefore, we do not talk about "using" another readtable but
    about "merging" the other readtable's definition into the
    readtable we are going to define. I.e. the inheritance is
    resolved once at definition time, that is statically.

    (Such merging can more or less be implemented portably albeit at
    a certain cost. Most of the time, this cost manifests itself at
    the time a readtable is defined, i.e. once at compile-time, so
    it may not bother you. Nonetheless, we provide extra support for
    Sbcl, ClozureCL, and AllegroCL at the moment. Patches for your
    implementation of choice are welcome, of course.)

3. [`DEFREADTABLE`][8b94] does not have compile-time effects.

    If you define a package via `DEFPACKAGE`, you can make that
    package the currently active package for the subsequent
    compilation of the same file via `IN-PACKAGE`. The same is,
    however, not true for [`DEFREADTABLE`][8b94] and [`IN-READTABLE`][de3b] for the
    following reason:

    It's unlikely that the need for special reader-macros arises for
    a problem which can be solved in just one file. Most often,
    you're going to define the reader macro functions, and set up
    the corresponding readtable in an extra file.

    If [`DEFREADTABLE`][8b94] had compile-time effects, you'd have to wrap
    each definition of a reader-macro function in an `EVAL-WHEN` to
    make its definition available at compile-time. Because that's
    simply not the common case, [`DEFREADTABLE`][8b94] does not have a
    compile-time effect.

    If you want to use a readtable within the same file as its
    definition, wrap the [`DEFREADTABLE`][8b94] and the reader-macro function
    definitions in an explicit `EVAL-WHEN`.


<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-PREREGISTERED-20MGL-PAX-3ASECTION-29'></a>

### 3.3 Preregistered Readtables

- `NIL`, `:STANDARD`, and `:COMMON-LISP` designate the
*standard readtable*.

- `:MODERN` designates a *case-preserving* *standard-readtable*.

- `:CURRENT` designates the *current readtable*.


<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-EXAMPLES-20MGL-PAX-3ASECTION-29'></a>

### 3.4 Examples

```commonlisp
(defreadtable elisp:syntax
   (:merge :standard)
   (:macro-char #\? #'elisp::read-character-literal t)
   (:macro-char #\[ #'elisp::read-vector-literal t)
   ...
   (:case :preserve))

(defreadtable scheme:syntax
   (:merge :standard)
   (:macro-char #\[ #'(lambda (stream char)
                         (read-delimited-list #\] stream)))
   (:macro-char #\# :dispatch)
   (:dispatch-macro-char #\# #\t #'scheme::read-#t)
   (:dispatch-macro-char #\# #\f #'scheme::read-#f)
   ...
   (:case :preserve))

(in-readtable elisp:syntax)

...

(in-readtable scheme:syntax)

...
```


<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-REFERENCE-20MGL-PAX-3ASECTION-29'></a>

## 4 Reference

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3ADEFREADTABLE-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **DEFREADTABLE** *NAME &BODY OPTIONS*

    Define a new named readtable, whose name is given by the symbol `NAME`.
    Or, if a readtable is already registered under that name, redefine
    that one.
    
    The readtable can be populated using the following `OPTIONS`:
    
    - `(:MERGE READTABLE-DESIGNATORS+)`
    
        Merge the readtables designated into the new readtable being
        defined as per [`MERGE-READTABLES-INTO`][77fa].
    
        If no `:MERGE` clause is given, an empty readtable is used. See
        [`MAKE-READTABLE`][958e].
    
    - `(:FUSE READTABLE-DESIGNATORS+)`
    
        Like `:MERGE` except:
    
        Error conditions of type [`READER-MACRO-CONFLICT`][acb7] that are signaled
        during the merge operation will be silently *continued*. It
        follows that reader macros in earlier entries will be
        overwritten by later ones. For backward compatibility, `:FUZE` is
        accepted as an alias of `:FUSE`.
    
    - `(:DISPATCH-MACRO-CHAR MACRO-CHAR SUB-CHAR FUNCTION)`
    
        Define a new sub character `SUB-CHAR` for the dispatching macro
        character `MACRO-CHAR`, per `SET-DISPATCH-MACRO-CHARACTER`. You
        probably have to define `MACRO-CHAR` as a dispatching macro
        character by the following option first.
    
    - `(:MACRO-CHAR MACRO-CHAR FUNCTION [NON-TERMINATING-P])`
    
        Define a new macro character in the readtable, per
        `SET-MACRO-CHARACTER`. If `FUNCTION` is the keyword `:DISPATCH`,
        `MACRO-CHAR` is made a dispatching macro character, per
        `MAKE-DISPATCH-MACRO-CHARACTER`.
    
    - `(:SYNTAX-FROM FROM-READTABLE-DESIGNATOR FROM-CHAR TO-CHAR)`
    
        Set the character syntax of `TO-CHAR` in the readtable being
        defined to the same syntax as `FROM-CHAR` as per
        `SET-SYNTAX-FROM-CHAR`.
    
    - `(:CASE CASE-MODE)`
    
        Defines the *case sensitivity mode* of the resulting readtable.
    
    Any number of option clauses may appear. The options are grouped by
    their type, but in each group the order the options appeared
    textually is preserved. The following groups exist and are executed
    in the following order: `:MERGE` and `:FUSE` (one
    group), `:CASE`, `:MACRO-CHAR` and `:DISPATCH-MACRO-CHAR` (one group),
    finally `:SYNTAX-FROM`.
    
    Notes:
    
    The readtable is defined at load-time. If you want to have it
    available at compilation time -- say to use its reader-macros in the
    same file as its definition -- you have to wrap the [`DEFREADTABLE`][8b94]
    form in an explicit `EVAL-WHEN`.
    
    On redefinition, the target readtable is made empty first before
    it's refilled according to the clauses.
    
    `NIL`, `:STANDARD`, `:COMMON-LISP`, `:MODERN`, and `:CURRENT` are
    preregistered readtable names.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3AIN-READTABLE-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **IN-READTABLE** *NAME*

    Set `*READTABLE*` to the readtable referred to by the symbol `NAME`.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3AMAKE-READTABLE-20FUNCTION-29'></a>

- [function] **MAKE-READTABLE** *&OPTIONAL (NAME NIL NAME-SUPPLIED-P) &KEY MERGE*

    Creates and returns a new readtable under the specified
    `NAME`.
    
    `MERGE` takes a list of NAMED-READTABLE-DESIGNATORS and specifies the
    readtables the new readtable is created from. (See the `:MERGE` clause
    of [`DEFREADTABLE`][8b94] for details.)
    
    If `MERGE` is `NIL`, an empty readtable is used instead.
    
    If `NAME` is not given, an anonymous empty readtable is returned.
    
    Notes:
    
    An empty readtable is a readtable where each character's syntax is
    the same as in the *standard readtable* except that each macro
    character has been made a constituent. Basically: whitespace stays
    whitespace, everything else is constituent.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3AMERGE-READTABLES-INTO-20FUNCTION-29'></a>

- [function] **MERGE-READTABLES-INTO** *RESULT-READTABLE &REST NAMED-READTABLES*

    Copy the contents of each readtable in `NAMED-READTABLES`([`0`][] [`1`][9b5b]) into
    `RESULT-READTABLE`.
    
    If a macro character appears in more than one of the readtables,
    i.e. if a conflict is discovered during the merge, an error of type
    [`READER-MACRO-CONFLICT`][acb7] is signaled.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3AFIND-READTABLE-20FUNCTION-29'></a>

- [function] **FIND-READTABLE** *NAME*

    Looks for the readtable specified by `NAME` and returns it if it is
    found. Returns `NIL` otherwise.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3AENSURE-READTABLE-20FUNCTION-29'></a>

- [function] **ENSURE-READTABLE** *NAME &OPTIONAL (DEFAULT NIL DEFAULT-P)*

    Looks up the readtable specified by `NAME` and returns it if it's found.
    If it is not found, it registers the readtable designated by `DEFAULT`
    under the name represented by NAME; or if no default argument is
    given, it signals an error of type [`READTABLE-DOES-NOT-EXIST`][437a]
    instead.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3ARENAME-READTABLE-20FUNCTION-29'></a>

- [function] **RENAME-READTABLE** *OLD-NAME NEW-NAME*

    Replaces the associated name of the readtable designated by
    `OLD-NAME` with `NEW-NAME`. If a readtable is already registered under
    `NEW-NAME`, an error of type [`READTABLE-DOES-ALREADY-EXIST`][4b51] is
    signaled.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-NAME-20FUNCTION-29'></a>

- [function] **READTABLE-NAME** *NAMED-READTABLE*

    Returns the name of the readtable designated by `NAMED-READTABLE`,
    or `NIL`.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREGISTER-READTABLE-20FUNCTION-29'></a>

- [function] **REGISTER-READTABLE** *NAME READTABLE*

    Associate `READTABLE` with `NAME`. Returns the readtable.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3AUNREGISTER-READTABLE-20FUNCTION-29'></a>

- [function] **UNREGISTER-READTABLE** *NAMED-READTABLE*

    Remove the association of `NAMED-READTABLE`. Returns `T` if successfull,
    `NIL` otherwise.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3ACOPY-NAMED-READTABLE-20FUNCTION-29'></a>

- [function] **COPY-NAMED-READTABLE** *NAMED-READTABLE*

    Like `COPY-READTABLE` but takes a [`NAMED-READTABLE-DESIGNATOR`][fa0c] as argument.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3ALIST-ALL-NAMED-READTABLES-20FUNCTION-29'></a>

- [function] **LIST-ALL-NAMED-READTABLES** 

    Returns a list of all registered readtables. The returned list is
    guaranteed to be fresh, but may contain duplicates.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3ANAMED-READTABLE-DESIGNATOR-20-28TYPE-29-29'></a>

- [type] **NAMED-READTABLE-DESIGNATOR**

    Either a symbol or a readtable itself.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADER-MACRO-CONFLICT-20CONDITION-29'></a>

- [condition] **READER-MACRO-CONFLICT** *READTABLE-ERROR*

    Continuable.
    
    This condition is signaled during the merge process if a reader
    macro (be it a macro character or the sub character of a dispatch
    macro character) is present in the both source and the target
    readtable and the two respective reader macro functions differ.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-DOES-ALREADY-EXIST-20CONDITION-29'></a>

- [condition] **READTABLE-DOES-ALREADY-EXIST** *READTABLE-ERROR*

    Continuable.

<a name='x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-DOES-NOT-EXIST-20CONDITION-29'></a>

- [condition] **READTABLE-DOES-NOT-EXIST** *READTABLE-ERROR*

  [059d]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-ACKNOWLEDGEMENTS-20MGL-PAX-3ASECTION-29 "(EDITOR-HINTS.NAMED-READTABLES:@NAMED-READTABLES-ACKNOWLEDGEMENTS MGL-PAX:SECTION)"
  [0bc2]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-OVERVIEW-20MGL-PAX-3ASECTION-29 "(EDITOR-HINTS.NAMED-READTABLES:@NAMED-READTABLES-OVERVIEW MGL-PAX:SECTION)"
  [373d]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-REFERENCE-20MGL-PAX-3ASECTION-29 "(EDITOR-HINTS.NAMED-READTABLES:@NAMED-READTABLES-REFERENCE MGL-PAX:SECTION)"
  [437a]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-DOES-NOT-EXIST-20CONDITION-29 "(EDITOR-HINTS.NAMED-READTABLES:READTABLE-DOES-NOT-EXIST CONDITION)"
  [4b51]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADTABLE-DOES-ALREADY-EXIST-20CONDITION-29 "(EDITOR-HINTS.NAMED-READTABLES:READTABLE-DOES-ALREADY-EXIST CONDITION)"
  [58c6]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-PREREGISTERED-20MGL-PAX-3ASECTION-29 "(EDITOR-HINTS.NAMED-READTABLES:@NAMED-READTABLES-PREREGISTERED MGL-PAX:SECTION)"
  [62b8]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-API-IDIOSYNCRASIES-20MGL-PAX-3ASECTION-29 "(EDITOR-HINTS.NAMED-READTABLES:@NAMED-READTABLES-API-IDIOSYNCRASIES MGL-PAX:SECTION)"
  [6faf]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-INTRODUCTION-20MGL-PAX-3ASECTION-29 "(EDITOR-HINTS.NAMED-READTABLES:@NAMED-READTABLES-INTRODUCTION MGL-PAX:SECTION)"
  [77fa]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AMERGE-READTABLES-INTO-20FUNCTION-29 "(EDITOR-HINTS.NAMED-READTABLES:MERGE-READTABLES-INTO FUNCTION)"
  [8688]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-LINKS-20MGL-PAX-3ASECTION-29 "(EDITOR-HINTS.NAMED-READTABLES:@NAMED-READTABLES-LINKS MGL-PAX:SECTION)"
  [8b94]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3ADEFREADTABLE-20-28MGL-PAX-3AMACRO-29-29 "(EDITOR-HINTS.NAMED-READTABLES:DEFREADTABLE (MGL-PAX:MACRO))"
  [958e]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AMAKE-READTABLE-20FUNCTION-29 "(EDITOR-HINTS.NAMED-READTABLES:MAKE-READTABLE FUNCTION)"
  [9b5b]: #x-28-22named-readtables-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"named-readtables\" ASDF/SYSTEM:SYSTEM)"
  [acb7]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AREADER-MACRO-CONFLICT-20CONDITION-29 "(EDITOR-HINTS.NAMED-READTABLES:READER-MACRO-CONFLICT CONDITION)"
  [cf94]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-EXAMPLES-20MGL-PAX-3ASECTION-29 "(EDITOR-HINTS.NAMED-READTABLES:@NAMED-READTABLES-EXAMPLES MGL-PAX:SECTION)"
  [de3b]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3AIN-READTABLE-20-28MGL-PAX-3AMACRO-29-29 "(EDITOR-HINTS.NAMED-READTABLES:IN-READTABLE (MGL-PAX:MACRO))"
  [e4cd]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3A-40NAMED-READTABLES-API-NOTES-20MGL-PAX-3ASECTION-29 "(EDITOR-HINTS.NAMED-READTABLES:@NAMED-READTABLES-API-NOTES MGL-PAX:SECTION)"
  [fa0c]: #x-28EDITOR-HINTS-2ENAMED-READTABLES-3ANAMED-READTABLE-DESIGNATOR-20-28TYPE-29-29 "(EDITOR-HINTS.NAMED-READTABLES:NAMED-READTABLE-DESIGNATOR (TYPE))"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
