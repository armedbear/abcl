/*
 * Closure.java
 *
 * Copyright (C) 2002-2008 Peter Graves
 * Copyright (C) 2008 Ville Voutilainen
 * $Id$
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

import java.util.ArrayList;

public class Closure extends Function
{
  // Parameter types.
  private static final int REQUIRED = 0;
  private static final int OPTIONAL = 1;
  private static final int KEYWORD  = 2;
  private static final int REST     = 3;
  private static final int AUX      = 4;

  // States.
  private static final int STATE_REQUIRED = 0;
  private static final int STATE_OPTIONAL = 1;
  private static final int STATE_KEYWORD  = 2;
  private static final int STATE_REST     = 3;
  private static final int STATE_AUX      = 4;

  private static final Parameter[] emptyParameterArray;
  static 
    {
        emptyParameterArray = new Parameter[0];
    }
  private Parameter[] requiredParameters = emptyParameterArray;
  private Parameter[] optionalParameters = emptyParameterArray;
  private Parameter[] keywordParameters = emptyParameterArray;
  private Parameter[] auxVars = emptyParameterArray;
  private final LispObject body;
  private final Environment environment;
  private final boolean andKey;
  private final boolean allowOtherKeys;
  private Symbol restVar;
  private Symbol envVar;
  private int arity;

  private int minArgs;
  private int maxArgs;

  private static final Symbol[] emptySymbolArray;
  static 
    {
        emptySymbolArray = new Symbol[0];
    }
  private Symbol[] variables = emptySymbolArray;
  private Symbol[] specials = emptySymbolArray;

  private boolean bindInitForms;

  public Closure(LispObject lambdaExpression, Environment env)
    throws ConditionThrowable
  {
    this(null, lambdaExpression, env);
  }

  public Closure(final LispObject name, final LispObject lambdaExpression,
                 final Environment env)
    throws ConditionThrowable
  {
    super(name, lambdaExpression.cadr());
    final LispObject lambdaList = lambdaExpression.cadr();
    setLambdaList(lambdaList);
    if (!(lambdaList == NIL || lambdaList instanceof Cons))
      error(new LispError("The lambda list " + lambdaList.writeToString() +
                           " is invalid."));
    boolean _andKey = false;
    boolean _allowOtherKeys = false;
    if (lambdaList instanceof Cons)
      {
        final int length = lambdaList.length();
        ArrayList<Parameter> required = null;
        ArrayList<Parameter> optional = null;
        ArrayList<Parameter> keywords = null;
        ArrayList<Parameter> aux = null;
        int state = STATE_REQUIRED;
        LispObject remaining = lambdaList;
        while (remaining != NIL)
          {
            LispObject obj = remaining.car();
            if (obj instanceof Symbol)
              {
                if (state == STATE_AUX)
                  {
                    if (aux == null)
                      aux = new ArrayList<Parameter>();
                    aux.add(new Parameter((Symbol)obj, NIL, AUX));
                  }
                else if (obj == Symbol.AND_OPTIONAL)
                  {
                    state = STATE_OPTIONAL;
                    arity = -1;
                  }
                else if (obj == Symbol.AND_REST || obj == Symbol.AND_BODY)
                  {
                    state = STATE_REST;
                    arity = -1;
                    maxArgs = -1;
                    remaining = remaining.cdr();
                    if (remaining == NIL)
                      {
                        error(new LispError(
                          "&REST/&BODY must be followed by a variable."));
                      }
                    Debug.assertTrue(restVar == null);
                    try
                      {
                        restVar = (Symbol) remaining.car();
                      }
                    catch (ClassCastException e)
                      {
                        error(new LispError(
                          "&REST/&BODY must be followed by a variable."));
                      }
                  }
                else if (obj == Symbol.AND_ENVIRONMENT)
                  {
                    remaining = remaining.cdr();
                    envVar = (Symbol) remaining.car();
                    arity = -1; // FIXME
                  }
                else if (obj == Symbol.AND_KEY)
                  {
                    state = STATE_KEYWORD;
                    _andKey = true;
                    arity = -1;
                  }
                else if (obj == Symbol.AND_ALLOW_OTHER_KEYS)
                  {
                    _allowOtherKeys = true;
                    maxArgs = -1;
                  }
                else if (obj == Symbol.AND_AUX)
                  {
                    // All remaining specifiers are aux variable specifiers.
                    state = STATE_AUX;
                    arity = -1; // FIXME
                  }
                else
                  {
                    if (state == STATE_OPTIONAL)
                      {
                        if (optional == null)
                          optional = new ArrayList<Parameter>();
                        optional.add(new Parameter((Symbol)obj, NIL, OPTIONAL));
                        if (maxArgs >= 0)
                          ++maxArgs;
                      }
                    else if (state == STATE_KEYWORD)
                      {
                        if (keywords == null)
                          keywords = new ArrayList<Parameter>();
                        keywords.add(new Parameter((Symbol)obj, NIL, KEYWORD));
                        if (maxArgs >= 0)
                          maxArgs += 2;
                      }
                    else
                      {
                        Debug.assertTrue(state == STATE_REQUIRED);
                        if (required == null)
                          required = new ArrayList<Parameter>();
                        required.add(new Parameter((Symbol)obj));
                        if (maxArgs >= 0)
                          ++maxArgs;
                      }
                  }
              }
            else if (obj instanceof Cons)
              {
                if (state == STATE_AUX)
                  {
                    Symbol sym = checkSymbol(obj.car());
                    LispObject initForm = obj.cadr();
                    Debug.assertTrue(initForm != null);
                    if (aux == null)
                      aux = new ArrayList<Parameter>();
                    aux.add(new Parameter(sym, initForm, AUX));
                  }
                else if (state == STATE_OPTIONAL)
                  {
                    Symbol sym = checkSymbol(obj.car());
                    LispObject initForm = obj.cadr();
                    LispObject svar = obj.cdr().cdr().car();
                    if (optional == null)
                      optional = new ArrayList<Parameter>();
                    optional.add(new Parameter(sym, initForm, svar, OPTIONAL));
                    if (maxArgs >= 0)
                      ++maxArgs;
                  }
                else if (state == STATE_KEYWORD)
                  {
                    Symbol keyword;
                    Symbol var;
                    LispObject initForm = NIL;
                    LispObject svar = NIL;
                    LispObject first = obj.car();
                    if (first instanceof Cons)
                      {
                        keyword = checkSymbol(first.car());
                        var = checkSymbol(first.cadr());
                      }
                    else
                      {
                        var = checkSymbol(first);
                        keyword =
                          PACKAGE_KEYWORD.intern(var.name);
                      }
                    obj = obj.cdr();
                    if (obj != NIL)
                      {
                        initForm = obj.car();
                        obj = obj.cdr();
                        if (obj != NIL)
                          svar = obj.car();
                      }
                    if (keywords == null)
                      keywords = new ArrayList<Parameter>();
                    keywords.add(new Parameter(keyword, var, initForm, svar));
                    if (maxArgs >= 0)
                      maxArgs += 2;
                  }
                else
                  invalidParameter(obj);
              }
            else
              invalidParameter(obj);
            remaining = remaining.cdr();
          }
        if (arity == 0)
          arity = length;
        if (required != null)
          {
            requiredParameters = new Parameter[required.size()];
            required.toArray(requiredParameters);
          }
        if (optional != null)
          {
            optionalParameters = new Parameter[optional.size()];
            optional.toArray(optionalParameters);
          }
        if (keywords != null)
          {
            keywordParameters = new Parameter[keywords.size()];
            keywords.toArray(keywordParameters);
          }
        if (aux != null)
          {
            auxVars = new Parameter[aux.size()];
            aux.toArray(auxVars);
          }
      }
    else
      {
        // Lambda list is empty.
        Debug.assertTrue(lambdaList == NIL);
        arity = 0;
        maxArgs = 0;
      }
    this.body = lambdaExpression.cddr();
    this.environment = env;
    this.andKey = _andKey;
    this.allowOtherKeys = _allowOtherKeys;
    minArgs = requiredParameters.length;
    if (arity >= 0)
      Debug.assertTrue(arity == minArgs);
    variables = processVariables();
    specials = processDeclarations();
  }

  private final void processParameters(ArrayList<Symbol> vars,
                                       final Parameter[] parameters)
  {
    for (Parameter parameter : parameters)
      {
        vars.add(parameter.var);
        if (parameter.svar != NIL)
          vars.add((Symbol)parameter.svar);
        if (!bindInitForms)
          if (!parameter.initForm.constantp())
            bindInitForms = true;
      }
  }

  // Also sets bindInitForms.
  private final Symbol[] processVariables()
  {
    ArrayList<Symbol> vars = new ArrayList<Symbol>();
    for (Parameter parameter : requiredParameters)
      vars.add(parameter.var);
    processParameters(vars, optionalParameters);
    if (restVar != null)
      {
        vars.add(restVar);
      }
    processParameters(vars, keywordParameters);
    Symbol[] array = new Symbol[vars.size()];
    vars.toArray(array);
    return array;
  }

  private final Symbol[] processDeclarations() throws ConditionThrowable
  {
    ArrayList<Symbol> arrayList = null;
    LispObject forms = body;
    while (forms != NIL)
      {
        LispObject obj = forms.car();
        if (obj instanceof Cons && obj.car() == Symbol.DECLARE)
          {
            LispObject decls = obj.cdr();
            while (decls != NIL)
              {
                LispObject decl = decls.car();
                if (decl instanceof Cons && decl.car() == Symbol.SPECIAL)
                  {
                    LispObject vars = decl.cdr();
                    while (vars != NIL)
                      {
                        Symbol var = checkSymbol(vars.car());
                        if (arrayList == null)
                          arrayList = new ArrayList<Symbol>();
                        arrayList.add(var);
                        vars = vars.cdr();
                      }
                  }
                decls = decls.cdr();
              }
            forms = forms.cdr();
          }
        else
          break;
      }
    if (arrayList == null)
      return emptySymbolArray;
    Symbol[] array = new Symbol[arrayList.size()];
    arrayList.toArray(array);
    return array;
  }

  private static final void invalidParameter(LispObject obj)
    throws ConditionThrowable
  {
    error(new LispError(obj.writeToString() +
                         " may not be used as a variable in a lambda list."));
  }

  @Override
  public LispObject typep(LispObject typeSpecifier) throws ConditionThrowable
  {
    if (typeSpecifier == Symbol.COMPILED_FUNCTION)
      return NIL;
    return super.typep(typeSpecifier);
  }

  public final LispObject getVariableList()
  {
    LispObject result = NIL;
    for (int i = variables.length; i-- > 0;)
      result = new Cons(variables[i], result);
    return result;
  }

  // Returns body as a list.
  public final LispObject getBody()
  {
    return body;
  }

  public final Environment getEnvironment()
  {
    return environment;
  }

  @Override
  public LispObject execute() throws ConditionThrowable
  {
    if (arity == 0)
      {
        return progn(body, environment, 
                     LispThread.currentThread());
      }
    else
      return execute(new LispObject[0]);
  }
    
  private final LispObject bindParametersAndExecute(
                                              Environment ext,
                                              LispThread thread,
                                              SpecialBinding lastSpecialBinding)
  throws ConditionThrowable
  {
    if (arity != minArgs)
      {
        bindParameterDefaults(optionalParameters, ext, thread);
        if (restVar != null)
          bindArg(restVar, NIL, ext, thread);
        bindParameterDefaults(keywordParameters, ext, thread);
      }
    bindAuxVars(ext, thread);
    try
      {
        return progn(body, ext, thread);
      }
    finally
      {
        thread.lastSpecialBinding = lastSpecialBinding;
      }
  }

  private final void bindRequiredParameters(Environment ext,
                                            LispThread thread,
                                            LispObject... objects)
  throws ConditionThrowable
  {
    // &whole and &environment before anything
    if (envVar != null)
      bindArg(envVar, environment, ext, thread);
    for (int i = 0; i < objects.length; ++i)
      {
        bindArg(requiredParameters[i].var, objects[i], ext, thread);
      }
  }

  public final LispObject invokeArrayExecute(LispObject... objects)
  throws ConditionThrowable
  {
    return execute(objects);
  }

  @Override
  public LispObject execute(LispObject arg) throws ConditionThrowable
  {
    if (minArgs == 1)
      {
        final LispThread thread = LispThread.currentThread();
        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
        Environment ext = new Environment(environment);
        bindRequiredParameters(ext, thread, arg);
        return bindParametersAndExecute(ext, thread, 
                                        lastSpecialBinding);
      }
    else
      {
        return invokeArrayExecute(arg);
      }
  }

  @Override
  public LispObject execute(LispObject first, LispObject second)
    throws ConditionThrowable
  {
    if (minArgs == 2)
      {
        final LispThread thread = LispThread.currentThread();
        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
        Environment ext = new Environment(environment);
        bindRequiredParameters(ext, thread, first, second);
        return bindParametersAndExecute(ext, thread, 
                                        lastSpecialBinding);
      }
    else
      {
        return invokeArrayExecute(first, second);
      }
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third)
    throws ConditionThrowable
  {
    if (minArgs == 3)
      {
        final LispThread thread = LispThread.currentThread();
        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
        Environment ext = new Environment(environment);
        bindRequiredParameters(ext, thread, first, second, third);
        return bindParametersAndExecute(ext, thread, 
                                        lastSpecialBinding);
      }
    else
      {
        return invokeArrayExecute(first, second, third);
      }
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth)
    throws ConditionThrowable
  {
    if (minArgs == 4)
      {
        final LispThread thread = LispThread.currentThread();
        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
        Environment ext = new Environment(environment);
        bindRequiredParameters(ext, thread, first, second, third, fourth);
        return bindParametersAndExecute(ext, thread, 
                                        lastSpecialBinding);
      }
    else
      {
        return invokeArrayExecute(first, second, third, fourth);
      }
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth)
    throws ConditionThrowable
  {
    if (minArgs == 5)
      {
        final LispThread thread = LispThread.currentThread();
        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
        Environment ext = new Environment(environment);
        bindRequiredParameters(ext, thread, first, second, third, fourth,
                               fifth);
        return bindParametersAndExecute(ext, thread, 
                                        lastSpecialBinding);
      }
    else
      {
        return invokeArrayExecute(first, second, third, fourth, fifth);
      }
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth)
    throws ConditionThrowable
  {
    if (minArgs == 6)
      {
        final LispThread thread = LispThread.currentThread();
        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
        Environment ext = new Environment(environment);
        bindRequiredParameters(ext, thread, first, second, third, fourth,
                               fifth, sixth);
        return bindParametersAndExecute(ext, thread, 
                                        lastSpecialBinding);
      }
    else
      {
        return invokeArrayExecute(first, second, third, fourth, fifth,
                                  sixth);
      }
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh)
    throws ConditionThrowable
  {
    if (minArgs == 7)
      {
        final LispThread thread = LispThread.currentThread();
        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
        Environment ext = new Environment(environment);
	for (Symbol special: specials)
	  ext.declareSpecial(special);
        bindRequiredParameters(ext, thread, first, second, third, fourth,
                               fifth, sixth, seventh);
        return bindParametersAndExecute(ext, thread, 
                                        lastSpecialBinding);
      }
    else
      {
        return invokeArrayExecute(first, second, third, fourth, fifth,
                                  sixth, seventh);
      }
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh, LispObject eighth)
    throws ConditionThrowable
  {
    if (minArgs == 8)
      {
        final LispThread thread = LispThread.currentThread();
        SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
        Environment ext = new Environment(environment);
        bindRequiredParameters(ext, thread, first, second, third, fourth,
                               fifth, sixth, seventh, eighth);
        return bindParametersAndExecute(ext, thread, 
                                        lastSpecialBinding);
      }
    else
      {
        return invokeArrayExecute(first, second, third, fourth, fifth,
                                  sixth, seventh, eighth);
      }
  }

  @Override
  public LispObject execute(LispObject[] args) throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
    Environment ext = new Environment(environment);
    if (optionalParameters.length == 0 && keywordParameters.length == 0)
      args = fastProcessArgs(args);
    else
      args = processArgs(args, thread);
    Debug.assertTrue(args.length == variables.length);
    if (envVar != null)
      {
        bindArg(envVar, environment, ext, thread);
      }
    for (int i = 0; i < variables.length; i++)
      {
        Symbol sym = variables[i];
        bindArg(sym, args[i], ext, thread);
      }
    bindAuxVars(ext, thread);
    special:
    for (Symbol special : specials) {
      for (Symbol var : variables)
        if (special == var)
          continue special;
      for (Parameter parameter : auxVars)
        if (special == parameter.var)
          continue special;
      ext.declareSpecial(special);
    }
    try
      {
        return progn(body, ext, thread);
      }
    finally
      {
        thread.lastSpecialBinding = lastSpecialBinding;
      }
  }

  private final boolean isSpecial(Symbol sym)
  {
    if (sym.isSpecialVariable())
      return true;
    for (Symbol special : specials)
      {
        if (sym == special)
            return true;
      }
    return false;
  }

  protected final LispObject[] processArgs(LispObject[] args, LispThread thread)
    throws ConditionThrowable
  {
    if (optionalParameters.length == 0 && keywordParameters.length == 0)
      return fastProcessArgs(args);
    final int argsLength = args.length;
    if (arity >= 0)
      {
        // Fixed arity.
        if (argsLength != arity)
          error(new WrongNumberOfArgumentsException(this));
        return args;
      }
    // Not fixed arity.
    if (argsLength < minArgs)
      error(new WrongNumberOfArgumentsException(this));
    final LispObject[] array = new LispObject[variables.length];
    int index = 0;
    // The bindings established here (if any) are lost when this function
    // returns. They are used only in the evaluation of initforms for
    // optional and keyword arguments.
    SpecialBinding lastSpecialBinding = thread.lastSpecialBinding;
    Environment ext = new Environment(environment);
    // Section 3.4.4: "...the &environment parameter is bound along with
    // &whole before any other variables in the lambda list..."
    if (bindInitForms)
      if (envVar != null)
          bindArg(envVar, environment, ext, thread);
    // Required parameters.
    for (int i = 0; i < minArgs; i++)
      {
        if (bindInitForms)
          bindArg(requiredParameters[i].var, args[i], ext, thread);
        array[index++] = args[i];
      }
    int i = minArgs;
    int argsUsed = minArgs;
    // Optional parameters.
    for (Parameter parameter : optionalParameters)
      {
        if (i < argsLength)
          {
            if (bindInitForms)
              bindArg(parameter.var, args[i], ext, thread);
            array[index++] = args[i];
            ++argsUsed;
            if (parameter.svar != NIL)
              {
                if (bindInitForms)
                  bindArg((Symbol)parameter.svar, T, ext, thread);
                array[index++] = T;
              }
          }
        else
          {
            // We've run out of arguments.
            LispObject value;
            if (parameter.initVal != null)
              value = parameter.initVal;
            else
              value = eval(parameter.initForm, ext, thread);
            if (bindInitForms)
              bindArg(parameter.var, value, ext, thread);
            array[index++] = value;
            if (parameter.svar != NIL)
              {
                if (bindInitForms)
                  bindArg((Symbol)parameter.svar, NIL, ext, thread);
                array[index++] = NIL;
              }
          }
        ++i;
      }
    // &rest parameter.
    if (restVar != null)
      {
        LispObject rest = NIL;
        for (int j = argsLength; j-- > argsUsed;)
          rest = new Cons(args[j], rest);
        if (bindInitForms)
            bindArg(restVar, rest, ext, thread);
        array[index++] = rest;
      }
    // Keyword parameters.
    if (keywordParameters.length > 0)
      {
        int argsLeft = argsLength - argsUsed;
        if (argsLeft == 0)
          {
            // No keyword arguments were supplied.
            // Bind all keyword parameters to their defaults.
            for (int k = 0; k < keywordParameters.length; k++)
              {
                Parameter parameter = keywordParameters[k];
                LispObject value;
                if (parameter.initVal != null)
                  value = parameter.initVal;
                else
                  value = eval(parameter.initForm, ext, thread);
                if (bindInitForms)
                    bindArg(parameter.var, value, ext, thread);
                array[index++] = value;
                if (parameter.svar != NIL)
                  {
                    if (bindInitForms)
                        bindArg((Symbol)parameter.svar, NIL, ext, thread);
                    array[index++] = NIL;
                  }
              }
          }
        else
          {
            if ((argsLeft % 2) != 0)
              error(new ProgramError("Odd number of keyword arguments."));
            LispObject allowOtherKeysValue = null;
            for (Parameter parameter : keywordParameters)
              {
                Symbol keyword = parameter.keyword;
                LispObject value = null;
                boolean unbound = true;
                for (int j = argsUsed; j < argsLength; j += 2)
                  {
                    if (args[j] == keyword)
                      {
                        if (bindInitForms)
                            bindArg(parameter.var, args[j+1], ext, thread);
                        value = array[index++] = args[j+1];
                        if (parameter.svar != NIL)
                          {
                            if (bindInitForms)
                                bindArg((Symbol)parameter.svar, T, ext, thread);
                            array[index++] = T;
                          }
                        args[j] = null;
                        args[j+1] = null;
                        unbound = false;
                        break;
                      }
                  }
                if (unbound)
                  {
                    if (parameter.initVal != null)
                      value = parameter.initVal;
                    else
                      value = eval(parameter.initForm, ext, thread);
                    if (bindInitForms)
                        bindArg(parameter.var, value, ext, thread);
                    array[index++] = value;
                    if (parameter.svar != NIL)
                      {
                        if (bindInitForms)
                            bindArg((Symbol)parameter.svar, NIL, ext, thread);
                        array[index++] = NIL;
                      }
                  }
                if (keyword == Keyword.ALLOW_OTHER_KEYS)
                  {
                    if (allowOtherKeysValue == null)
                      allowOtherKeysValue = value;
                  }
              }
            if (!allowOtherKeys)
              {
                if (allowOtherKeysValue == null || allowOtherKeysValue == NIL)
                  {
                    LispObject unrecognizedKeyword = null;
                    for (int j = argsUsed; j < argsLength; j += 2)
                      {
                        LispObject keyword = args[j];
                        if (keyword == null)
                          continue;
                        if (keyword == Keyword.ALLOW_OTHER_KEYS)
                          {
                            if (allowOtherKeysValue == null)
                              {
                                allowOtherKeysValue = args[j+1];
                                if (allowOtherKeysValue != NIL)
                                  break;
                              }
                            continue;
                          }
                        // Unused keyword argument.
                        boolean ok = false;
                        for (Parameter parameter : keywordParameters)
                          {
                            if (parameter.keyword == keyword)
                              {
                                // Found it!
                                ok = true;
                                break;
                              }
                          }
                        if (ok)
                          continue;
                        // Unrecognized keyword argument.
                        if (unrecognizedKeyword == null)
                          unrecognizedKeyword = keyword;
                      }
                    if (unrecognizedKeyword != null)
                      {
                        if (!allowOtherKeys &&
                            (allowOtherKeysValue == null || allowOtherKeysValue == NIL))
                          error(new ProgramError("Unrecognized keyword argument " +
                                                  unrecognizedKeyword.writeToString()));
                      }
                  }
              }
          }
      }
    else if (argsUsed < argsLength)
      {
        // No keyword parameters.
        if (argsUsed + 2 <= argsLength)
          {
            // Check for :ALLOW-OTHER-KEYS.
            LispObject allowOtherKeysValue = NIL;
            int n = argsUsed;
            while (n < argsLength)
              {
                LispObject keyword = args[n];
                if (keyword == Keyword.ALLOW_OTHER_KEYS)
                  {
                    allowOtherKeysValue = args[n+1];
                    break;
                  }
                n += 2;
              }
            if (allowOtherKeys || allowOtherKeysValue != NIL)
              {
                // Skip keyword/value pairs.
                while (argsUsed + 2 <= argsLength)
                  argsUsed += 2;
              }
            else if (andKey)
              {
                LispObject keyword = args[argsUsed];
                if (keyword == Keyword.ALLOW_OTHER_KEYS)
                  {
                    // Section 3.4.1.4: "Note that if &KEY is present, a
                    // keyword argument of :ALLOW-OTHER-KEYS is always
                    // permitted---regardless of whether the associated
                    // value is true or false."
                    argsUsed += 2;
                  }
              }
          }
        if (argsUsed < argsLength)
          {
            if (restVar == null)
              error(new WrongNumberOfArgumentsException(this));
          }
      }
    thread.lastSpecialBinding = lastSpecialBinding;
    return array;
  }

  // No optional or keyword parameters.
  protected final LispObject[] fastProcessArgs(LispObject[] args)
    throws ConditionThrowable
  {
    final int argsLength = args.length;
    if (arity >= 0)
      {
        // Fixed arity.
        if (argsLength != arity)
          error(new WrongNumberOfArgumentsException(this));
        return args;
      }
    // Not fixed arity.
    if (argsLength < minArgs)
      error(new WrongNumberOfArgumentsException(this));
    final LispObject[] array = new LispObject[variables.length];
    int index = 0;
    // Required parameters.
    for (int i = 0; i < minArgs; i++)
      {
        array[index++] = args[i];
      }
    int argsUsed = minArgs;
    // &rest parameter.
    if (restVar != null)
      {
        LispObject rest = NIL;
        for (int j = argsLength; j-- > argsUsed;)
          rest = new Cons(args[j], rest);
        array[index++] = rest;
      }
    else if (argsUsed < argsLength)
      {
        // No keyword parameters.
        if (argsUsed + 2 <= argsLength)
          {
            // Check for :ALLOW-OTHER-KEYS.
            LispObject allowOtherKeysValue = NIL;
            int n = argsUsed;
            while (n < argsLength)
              {
                LispObject keyword = args[n];
                if (keyword == Keyword.ALLOW_OTHER_KEYS)
                  {
                    allowOtherKeysValue = args[n+1];
                    break;
                  }
                n += 2;
              }
            if (allowOtherKeys || allowOtherKeysValue != NIL)
              {
                // Skip keyword/value pairs.
                while (argsUsed + 2 <= argsLength)
                  argsUsed += 2;
              }
            else if (andKey)
              {
                LispObject keyword = args[argsUsed];
                if (keyword == Keyword.ALLOW_OTHER_KEYS)
                  {
                    // Section 3.4.1.4: "Note that if &key is present, a
                    // keyword argument of :allow-other-keys is always
                    // permitted---regardless of whether the associated
                    // value is true or false."
                    argsUsed += 2;
                  }
              }
          }
        if (argsUsed < argsLength)
          {
            if (restVar == null)
              error(new WrongNumberOfArgumentsException(this));
          }
      }
    return array;
  }

  private final void bindParameterDefaults(Parameter[] parameters,
                                           Environment env,
                                           LispThread thread)
    throws ConditionThrowable
  {
    for (Parameter parameter : parameters)
      {
        LispObject value;
        if (parameter.initVal != null)
          value = parameter.initVal;
        else
          value = eval(parameter.initForm, env, thread);
        bindArg(parameter.var, value, env, thread);
        if (parameter.svar != NIL)
            bindArg((Symbol)parameter.svar, NIL, env, thread);
      }
  }

    private final void bindArg(Symbol sym, LispObject value,
                               Environment env, LispThread thread)
        throws ConditionThrowable
    {
        if (isSpecial(sym)) {
            env.declareSpecial(sym);
            thread.bindSpecial(sym, value);
        }
        else
            env.bind(sym, value);
    }

  private final void bindAuxVars(Environment env, LispThread thread)
    throws ConditionThrowable
  {
    // Aux variable processing is analogous to LET* processing.
    for (Parameter parameter : auxVars)
      {
        Symbol sym = parameter.var;
        LispObject value;

        if (parameter.initVal != null)
          value = parameter.initVal;
        else
          value = eval(parameter.initForm, env, thread);

        bindArg(sym, value, env, thread);
      }
  }

  private static class Parameter
  {
    private final Symbol var;
    private final LispObject initForm;
    private final LispObject initVal;
    private final LispObject svar;
    private final int type;
    private final Symbol keyword;

    public Parameter(Symbol var)
    {
      this.var = var;
      this.initForm = null;
      this.initVal = null;
      this.svar = NIL;
      this.type = REQUIRED;
      this.keyword = null;
    }

    public Parameter(Symbol var, LispObject initForm, int type)
      throws ConditionThrowable
    {
      this.var = var;
      this.initForm = initForm;
      this.initVal = processInitForm(initForm);
      this.svar = NIL;
      this.type = type;
      keyword =
        type == KEYWORD ? PACKAGE_KEYWORD.intern(var.name) : null;
    }

    public Parameter(Symbol var, LispObject initForm, LispObject svar,
                     int type)
      throws ConditionThrowable
    {
      this.var = var;
      this.initForm = initForm;
      this.initVal = processInitForm(initForm);
      this.svar = (svar != NIL) ? checkSymbol(svar) : NIL;
      this.type = type;
      keyword =
        type == KEYWORD ? PACKAGE_KEYWORD.intern(var.name) : null;
    }

    public Parameter(Symbol keyword, Symbol var, LispObject initForm,
                     LispObject svar)
      throws ConditionThrowable
    {
      this.var = var;
      this.initForm = initForm;
      this.initVal = processInitForm(initForm);
      this.svar = (svar != NIL) ? checkSymbol(svar) : NIL;
      type = KEYWORD;
      this.keyword = keyword;
    }

    @Override
    public String toString()
    {
      if (type == REQUIRED)
        return var.toString();
      StringBuffer sb = new StringBuffer();
      if (keyword != null)
        {
          sb.append(keyword);
          sb.append(' ');
        }
      sb.append(var.toString());
      sb.append(' ');
      sb.append(initForm);
      sb.append(' ');
      sb.append(type);
      return sb.toString();
    }

    private static final LispObject processInitForm(LispObject initForm)
      throws ConditionThrowable
    {
      if (initForm.constantp())
        {
          if (initForm instanceof Symbol)
            return initForm.getSymbolValue();
          if (initForm instanceof Cons)
            {
              Debug.assertTrue(initForm.car() == Symbol.QUOTE);
              return initForm.cadr();
            }
          return initForm;
        }
      return null;
    }
  }

  // ### lambda-list-names
  private static final Primitive LAMBDA_LIST_NAMES =
      new Primitive("lambda-list-names", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        Closure closure = new Closure(list3(Symbol.LAMBDA, arg, NIL), new Environment());
        return closure.getVariableList();
      }
    };
}
