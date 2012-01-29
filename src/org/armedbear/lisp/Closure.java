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

import static org.armedbear.lisp.Lisp.*;

import java.util.ArrayList;

public class Closure extends Function
{
  // Parameter types.
  public static final int REQUIRED = 0;
  public static final int OPTIONAL = 1;
  public static final int KEYWORD  = 2;
  public static final int REST     = 3;
  public static final int AUX      = 4;

  // States.
  private static final int STATE_REQUIRED = 0;
  private static final int STATE_OPTIONAL = 1;
  private static final int STATE_KEYWORD  = 2;
  private static final int STATE_REST     = 3;
  private static final int STATE_AUX      = 4;

  private Parameter[] requiredParameters = new Parameter[0];
  private Parameter[] optionalParameters = requiredParameters;
  private Parameter[] keywordParameters = requiredParameters;
  private Parameter[] auxVars = requiredParameters;
  private final LispObject body;
  private final LispObject executionBody;
  private final Environment environment;
  private final boolean andKey;
  private final boolean allowOtherKeys;
  private Symbol restVar;
  private Symbol envVar;
  private int arity;

  private int minArgs;
  private int maxArgs;

  private Symbol[] variables = new Symbol[0];
  private LispObject specials = NIL;

  private boolean bindInitForms;


  private ArgumentListProcessor arglist;

    /** Construct a closure object with a lambda-list described
     * by these parameters.
     *
     *
     * @param required Required parameters or an empty array for none
     * @param optional Optional parameters or an empty array for none
     * @param keyword Keyword parameters or an empty array for none
     * @param keys NIL if the lambda-list doesn't contain &amp;key, T otherwise
     * @param rest the &amp;rest parameter, or NIL if none
     * @param moreKeys NIL if &amp;allow-other-keys not present, T otherwise
     */
  public Closure(Parameter[] required,
                 Parameter[] optional,
                 Parameter[] keyword,
                 Symbol keys, Symbol rest, Symbol moreKeys) {
      minArgs = required.length;
      maxArgs = (rest == NIL && moreKeys == NIL)
          ? minArgs + optional.length + 2*keyword.length : -1;

      arity = (rest == NIL && moreKeys == NIL && keys == NIL
               && optional.length == 0)
          ? maxArgs : -1;

      requiredParameters = required;
      optionalParameters = optional;
      keywordParameters = keyword;

      if (rest != NIL)
        restVar = rest;

      andKey = keys != NIL;
      allowOtherKeys = moreKeys != NIL;
      variables = processVariables();
      bindInitForms = false;

      // stuff we don't need: we're a compiled function
      body = null;
      executionBody = null;
      environment = null;

      ArrayList<ArgumentListProcessor.RequiredParam> reqParams =
              new ArrayList<ArgumentListProcessor.RequiredParam>();
      for (Parameter req : requiredParameters)
          reqParams.add(new ArgumentListProcessor.RequiredParam(req.var, false));

      ArrayList<ArgumentListProcessor.OptionalParam> optParams =
              new ArrayList<ArgumentListProcessor.OptionalParam>();
      for (Parameter opt : optionalParameters)
          optParams.add(new ArgumentListProcessor.OptionalParam(opt.var, false,
                  (opt.svar == NIL) ? null : (Symbol)opt.svar, false,
                  opt.initForm));

      ArrayList<ArgumentListProcessor.KeywordParam> keyParams =
              new ArrayList<ArgumentListProcessor.KeywordParam>();
      for (Parameter key : keywordParameters)
          keyParams.add(new ArgumentListProcessor.KeywordParam(key.var, false,
                  (key.svar == NIL) ? null : (Symbol)key.svar, false, key.initForm,
                  key.keyword));
      arglist = new ArgumentListProcessor(this, reqParams, optParams, keyParams, andKey, allowOtherKeys, restVar);
  }


  public Closure(LispObject lambdaExpression, Environment env)
  {
    this(null, lambdaExpression, env);
  }

  public Closure(final LispObject name, final LispObject lambdaExpression,
                 final Environment env)

  {
    super(name, lambdaExpression.cadr());
    final LispObject lambdaList = lambdaExpression.cadr();
    setLambdaList(lambdaList);
    if (!(lambdaList == NIL || lambdaList instanceof Cons))
      error(new ProgramError("The lambda list " + lambdaList.princToString() +
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
                    if (_andKey)
                      {
                        error(new ProgramError(
                          "&REST/&BODY must precede &KEY."));
                      }
                    state = STATE_REST;
                    arity = -1;
                    maxArgs = -1;
                    remaining = remaining.cdr();
                    if (remaining == NIL)
                      {
                        error(new ProgramError(
                          "&REST/&BODY must be followed by a variable."));
                      }
                    if (restVar != null) 
                      {
                        error(new ProgramError(
                          "&REST/&BODY may occur only once."));
                      }
                    final LispObject remainingcar =  remaining.car();
                    if (remainingcar instanceof Symbol)
                      {
                        restVar = (Symbol) remainingcar;
                      }
                    else
                      {
                        error(new ProgramError(
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
                        if (state != STATE_REQUIRED)
                          {
                            error(new ProgramError(
                              "required parameters cannot appear after &REST/&BODY."));
                          }
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
    LispObject bodyAndDecls = parseBody(this.body, false);
    this.executionBody = bodyAndDecls.car();
    this.specials = parseSpecials(bodyAndDecls.NTH(1));

    this.environment = env;
    this.andKey = _andKey;
    this.allowOtherKeys = _allowOtherKeys;
    minArgs = requiredParameters.length;
    if (arity >= 0)
      Debug.assertTrue(arity == minArgs);
    variables = processVariables();

    arglist = new ArgumentListProcessor(this, lambdaList, specials);
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

  private static final void invalidParameter(LispObject obj)

  {
    error(new ProgramError(obj.princToString() +
                         " may not be used as a variable in a lambda list."));
  }

  @Override
  public LispObject typep(LispObject typeSpecifier)
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
  public LispObject execute()
  {
    if (arity == 0)
      {
        return progn(executionBody, environment, 
                     LispThread.currentThread());
      }
    else
      return execute(new LispObject[0]);
  }
    
  private final LispObject bindParametersAndExecute(LispObject... objects)

  {
    final LispThread thread = LispThread.currentThread();
    final SpecialBindingsMark mark = thread.markSpecialBindings();
    Environment ext = new Environment(environment);
    bindRequiredParameters(ext, thread, objects);
    if (arity != minArgs)
      {
        bindParameterDefaults(optionalParameters, ext, thread);
        if (restVar != null)
          bindArg(specials, restVar, NIL, ext, thread);
        bindParameterDefaults(keywordParameters, ext, thread);
      }
    bindAuxVars(ext, thread);
    declareFreeSpecials(ext);
    try
      {
        return progn(executionBody, ext, thread);
      }
    finally
      {
        thread.resetSpecialBindings(mark);
      }
  }

  private final void bindRequiredParameters(Environment ext,
                                            LispThread thread,
                                            LispObject[] objects)

  {
    // &whole and &environment before anything
    if (envVar != null)
      bindArg(specials, envVar, environment, ext, thread);
    for (int i = 0; i < objects.length; ++i)
      {
        bindArg(specials, requiredParameters[i].var, objects[i], ext, thread);
      }
  }

  public final LispObject invokeArrayExecute(LispObject... objects)

  {
    return execute(objects);
  }

  @Override
  public LispObject execute(LispObject arg)
  {
    if (minArgs == 1)
      {
        return bindParametersAndExecute(arg);
      }
    else
      {
        return invokeArrayExecute(arg);
      }
  }

  @Override
  public LispObject execute(LispObject first, LispObject second)

  {
    if (minArgs == 2)
      {
        return bindParametersAndExecute(first, second);
      }
    else
      {
        return invokeArrayExecute(first, second);
      }
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third)

  {
    if (minArgs == 3)
      {
        return bindParametersAndExecute(first, second, third);
      }
    else
      {
        return invokeArrayExecute(first, second, third);
      }
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth)

  {
    if (minArgs == 4)
      {
        return bindParametersAndExecute(first, second, third, fourth);
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

  {
    if (minArgs == 5)
      {
        return bindParametersAndExecute(first, second, third, fourth,
                                        fifth);
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

  {
    if (minArgs == 6)
      {
        return bindParametersAndExecute(first, second, third, fourth,
                                        fifth, sixth);
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

  {
    if (minArgs == 7)
      {
        return bindParametersAndExecute(first, second, third, fourth,
                               fifth, sixth, seventh);
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

  {
    if (minArgs == 8)
      {
        return bindParametersAndExecute(first, second, third, fourth,
                               fifth, sixth, seventh, eighth);
      }
    else
      {
        return invokeArrayExecute(first, second, third, fourth, fifth,
                                  sixth, seventh, eighth);
      }
  }

  private final void declareFreeSpecials(Environment ext)

  {
    LispObject s = specials;
    special:
    while (s != NIL) {
      Symbol special = (Symbol)s.car();
      s = s.cdr();
      for (Symbol var : variables)
	if (special == var)
          continue special;
      for (Parameter parameter : auxVars)
        if (special == parameter.var)
          continue special;
      ext.declareSpecial(special);
    }
  }

  @Override
  public LispObject execute(LispObject[] args)
  {
    final LispThread thread = LispThread.currentThread();
    final SpecialBindingsMark mark = thread.markSpecialBindings();
    Environment ext = new Environment(environment);
    if (optionalParameters.length == 0 && keywordParameters.length == 0)
      args = fastProcessArgs(args);
    else
      args = processArgs(args, thread);
    Debug.assertTrue(args.length == variables.length);
    if (envVar != null)
      {
        bindArg(specials, envVar, environment, ext, thread);
      }
    for (int i = 0; i < variables.length; i++)
      {
        Symbol sym = variables[i];
        bindArg(specials, sym, args[i], ext, thread);
      }
    bindAuxVars(ext, thread);
    declareFreeSpecials(ext);
    try
      {
        return progn(executionBody, ext, thread);
      }
    finally
      {
        thread.resetSpecialBindings(mark);
      }
  }

  
  private LispObject[] _processArgs(LispObject[] args, LispThread thread,
          Environment ext) {
        final LispObject[] array = new LispObject[variables.length];
        int index = 0;

        int argsLength = args.length;
        
        if (bindInitForms)
          if (envVar != null)
            bindArg(specials, envVar, environment, ext, thread);
        // Required parameters.
        for (int i = 0; i < minArgs; i++)
          {
            if (bindInitForms)
              bindArg(specials, requiredParameters[i].var, args[i], ext, thread);
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
                  bindArg(specials, parameter.var, args[i], ext, thread);
                array[index++] = args[i];
                ++argsUsed;
                if (parameter.svar != NIL)
                  {
                    if (bindInitForms)
                      bindArg(specials, (Symbol)parameter.svar, T, ext, thread);
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
                  bindArg(specials, parameter.var, value, ext, thread);
                array[index++] = value;
                if (parameter.svar != NIL)
                  {
                    if (bindInitForms)
                      bindArg(specials, (Symbol)parameter.svar, NIL, ext, thread);
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
                bindArg(specials, restVar, rest, ext, thread);
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
                        bindArg(specials, parameter.var, value, ext, thread);
                    array[index++] = value;
                    if (parameter.svar != NIL)
                      {
                        if (bindInitForms)
                            bindArg(specials, (Symbol)parameter.svar, NIL, ext, thread);
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
                                bindArg(specials, parameter.var, args[j+1], ext, thread);
                            value = array[index++] = args[j+1];
                            if (parameter.svar != NIL)
                              {
                                if (bindInitForms)
                                    bindArg(specials,(Symbol)parameter.svar, T, ext, thread);
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
                            bindArg(specials, parameter.var, value, ext, thread);
                        array[index++] = value;
                        if (parameter.svar != NIL)
                          {
                            if (bindInitForms)
                                bindArg(specials, (Symbol)parameter.svar, NIL, ext, thread);
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
                                                      unrecognizedKeyword.printObject()));
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
        return array;
  }
  
  protected final LispObject[] processArgs(LispObject[] args, LispThread thread)

  {
    if (optionalParameters.length == 0 && keywordParameters.length == 0)
      return fastProcessArgs(args);
    if (arity >= 0)
      {
        // Fixed arity.
        if (args.length != arity)
          error(new WrongNumberOfArgumentsException(this, arity));
        return args;
      }
    // Not fixed arity.
    if (args.length < minArgs)
      error(new WrongNumberOfArgumentsException(this, minArgs, -1));
    
    if (!bindInitForms)
        return _processArgs(args, thread, environment);
    
    // The bindings established here (if any) are lost when this function
    // returns. They are used only in the evaluation of initforms for
    // optional and keyword arguments.
    final SpecialBindingsMark mark = thread.markSpecialBindings();
    Environment ext = new Environment(environment);
    // Section 3.4.4: "...the &environment parameter is bound along with
    // &whole before any other variables in the lambda list..."
    try {
        return _processArgs(args, thread, ext);
    }
    finally {
        thread.resetSpecialBindings(mark);
    }
  }

  // No optional or keyword parameters.
  protected final LispObject[] fastProcessArgs(LispObject[] args)

  {
    final int argsLength = args.length;
    if (arity >= 0)
      {
        // Fixed arity.
        if (argsLength != arity)
          error(new WrongNumberOfArgumentsException(this, arity));
        return args;
      }
    // Not fixed arity.
    if (argsLength < minArgs)
      error(new WrongNumberOfArgumentsException(this, minArgs, -1));
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

  {
    for (Parameter parameter : parameters)
      {
        LispObject value;
        if (parameter.initVal != null)
          value = parameter.initVal;
        else
          value = eval(parameter.initForm, env, thread);
        bindArg(specials, parameter.var, value, env, thread);
        if (parameter.svar != NIL)
	  bindArg(specials, (Symbol)parameter.svar, NIL, env, thread);
      }
  }

  private final void bindAuxVars(Environment env, LispThread thread)

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

        bindArg(specials, sym, value, env, thread);
      }
  }

  public static class Parameter
  {
    final Symbol var;
    final LispObject initForm;
    final LispObject initVal;
    final LispObject svar;
    private final int type;
    final Symbol keyword;

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
      public LispObject execute(LispObject arg)
      {
        Closure closure = new Closure(list(Symbol.LAMBDA, arg, NIL), new Environment());
        return closure.getVariableList();
      }
    };
}
