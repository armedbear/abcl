From http://www.lisp.org/mop/dictionary.html

# Generic Functions

add-dependent metaobject dependent
add-direct-method specializer method
add-direct-subclass superclass subclass
add-direct-method specializer method
add-direct-subclass superclass subclass
add-method generic-function method
allocate-instance class &rest initargs

compute-applicable-methods generic-function arguments
compute-applicable-methods-using-classes generic-function classes
compute-applicable-methods-using-classes generic-function classes
compute-class-precedence-list class
compute-default-initargs class
compute-discriminating-function generic-function
compute-effective-method generic-function method-combination methods
compute-effective-slot-definition class name direct-slot-definitions
compute-slots class
direct-slot-definition-class class &rest initargs
effective-slot-definition-class class &rest initargs

ensure-class-using-class class name &key direct-default-initargs direct-slots direct-superclasses
name metaclass &allow-other-keys
ensure-generic-function-using-class generic-function function-name &key argument-precedence-order
declarations documentation generic-function-class lambda-list method-class method-combination
name &allow-other-keys
find-method-combination generic-function method-combination-type-name method-combination-options

make-method-lambda generic-function method lambda-expression environment
map-dependents metaobject function

reader-method-class class direct-slot &rest initargs
remove-dependent metaobject dependent
remove-direct-method specializer method
remove-direct-subclass superclass subclass
remove-method generic-function method
set-funcallable-instance-function funcallable-instance function
slot-boundp-using-class class object slot

slot-makunbound-using-class class object slot
slot-value-using-class class object slot
specializer-direct-generic-functions specializer
specializer-direct-methods specializer
standard-instance-access instance location
update-dependent metaobject dependent &rest initargs
validate-superclass class superclass
writer-method-class class direct-slot &rest initargs

## Readers for Class Metaobjects

class-default-initargs class
class-direct-default-initargs class
class-direct-slots class
class-direct-subclasses class
class-direct-superclasses class
class-finalized-p class
class-name class
class-precedence-list class
class-prototype class
class-slots class

## Readers for Generic Function Metaobjects

generic-function-argument-precedence-order generic-function
generic-function-declarations generic-function
generic-function-lambda-list generic-function
generic-function-method-class generic-function
generic-function-method-combination generic-function
generic-function-methods and generic-function-name generic-function

## Readers for Method Metaobjects

method-function method
method-generic-function method
method-lambda-list method
method-specializers method
method-qualifiers method
accessor-method-slot-definition method

## Direct Slot Definition Metaobjects

slot-definition-readers direct-slot
slot-definition-writers direct-slot

## Readers for Slot Definition Metaobjects

slot-definition-allocation slot
slot-definition-initargs slot
slot-definition-initform slot
slot-definition-initfunction slot
slot-definition-name slot
slot-definition-type slot


# Functions

ensure-class name &key &allow-other-keys
ensure-generic-function function-name &key &allow-other-keys
eql-specializer-object eql-specializer
extract-lambda-list specialized-lambda-list
extract-specializer-names specialized-lambda-list
funcallable-standard-instance-access instance location
intern-eql-specializer object
(setf class-name) new-name class
(setf generic-function-name) new-name generic-function
(setf slot-value-using-class) new-value class object slot
