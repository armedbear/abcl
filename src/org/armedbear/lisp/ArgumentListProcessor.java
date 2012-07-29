/*
 * ArgumentListProcessor.java
 *
 * Copyright (C) 2012 Erik Huelsmann
 * Copyright (C) 2002-2008 Peter Graves
 * Copyright (C) 2008 Ville Voutilainen
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
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

import java.util.List;
import java.util.ArrayList;
import static org.armedbear.lisp.Lisp.*;

/** A class to parse a lambda list and match function call arguments with it
 */
public class ArgumentListProcessor {

  // States.
  private static final int STATE_REQUIRED = 0;
  private static final int STATE_OPTIONAL = 1;
  private static final int STATE_KEYWORD  = 2;
  private static final int STATE_REST     = 3;
  private static final int STATE_AUX      = 4;

  private Param[] requiredParameters = new Param[0];
  private Param[] optionalParameters = requiredParameters;
  private KeywordParam[] keywordParameters = new KeywordParam[0];
  private Param[] auxVars = requiredParameters;
  private Param[] positionalParameters = requiredParameters;
  
  private Symbol restVar;
  private Param restParam;
  private Symbol envVar;
  private Param envParam;
  private int arity;

  private int minArgs;
  private int maxArgs;
  
  /** The variables in the lambda list, including &aux and 'supplied-p' */
  private Symbol[] variables = new Symbol[0];
  
  /** Array of booleans of value 'true' if the associated variable in the
   * variables array is a special variable */
  private boolean[] specials = new boolean[0];
  
  private boolean andKey;
  private boolean allowOtherKeys;
  
  /** The parser to be used to match function call arguments with the lambda list */
  final private ArgumentMatcher matcher;
  
  /** Holds the value 'true' if the matcher needs an evaluation environment to
   * evaluate the initforms of variales in the &optional, &key or &aux categories */
  private boolean matcherNeedsEnv;
  
  /** Used when generating errors during function call argument matching */
  private Operator function;
  
  /** Constructor to be used from compiled code
   * 
   * The compiler hands in pre-parsed lambda lists. The process of matching
   * function call arguments with lambda lists which are constructed this
   * way don't support non-constant initforms for &optional, &key and &aux
   * parameters. As a result, there's no need to create an evaluation
   * environment which in turn eliminates the need to know which variables
   * are special.
   * 
   * @param fun The function to report function call argument matching errors on
   * @param required The list of required arguments
   * @param optional The list of optional arguments
   * @param keyword The list of keyword parameters
   * @param key Indicates whether &key was specified (optionally without naming keys)
   * @param moreKeys Indicates whether &allow-other-keys was specified
   * @param rest Specifies the &rest variable name, if one was specified, or 'null' if none
   */
  public ArgumentListProcessor(Operator fun, int requiredCount,
          OptionalParam[] optional, KeywordParam[] keyword,
          boolean key, boolean moreKeys, Symbol rest) {

      function = fun;
      
      requiredParameters = new RequiredParam[requiredCount];
      positionalParameters = new Param[requiredCount + optional.length 
              + ((rest != null) ? 1 : 0)];
      
      // the same anonymous required parameter can be used any number of times
      RequiredParam r = new RequiredParam();
      for (int i = 0; i < requiredCount; i++) {
          requiredParameters[i] = r;
          positionalParameters[i] = r;
      }
          
      optionalParameters = optional;
      System.arraycopy(optional, 0,
              positionalParameters, requiredCount, optional.length);

      restVar = rest;
      if (restVar != null)
        positionalParameters[requiredCount + optional.length] =
                restParam = new RestParam(rest, false);

      andKey = key;
      allowOtherKeys = moreKeys;
      keywordParameters = keyword;


      auxVars = new Param[0];

      
      variables = extractVariables();
      specials = new boolean[variables.length]; // default values 'false' -- leave that way

      minArgs = requiredParameters.length;
      maxArgs = (rest == null && ! allowOtherKeys)
              ? minArgs + optionalParameters.length + 2*keywordParameters.length : -1;
      arity = (rest == null && ! allowOtherKeys && ! andKey && optionalParameters.length == 0)
              ? maxArgs : -1;
      
      if (keyword.length == 0)
          matcher = new FastMatcher();
      else
          matcher = new SlowMatcher();
  }
  
  
  /** Instantiates an ArgumentListProcessor by parsing the lambda list specified
   * in 'lambdaList'.
   * 
   * This constructor sets up the object to support evaluation of non-constant
   * initforms.
   * 
   * @param fun Function to use when reporting errors
   * @param lambdaList Lambda list to parse and use for function call 
   * @param specials A list of symbols specifying which variables to
   *    bind as specials during initform evaluation
   */
  public ArgumentListProcessor(Operator fun, LispObject lambdaList, LispObject specials) {
    function = fun;
    
    boolean _andKey = false;
    boolean _allowOtherKeys = false;
    if (lambdaList instanceof Cons)
      {
        final int length = lambdaList.length();
        ArrayList<Param> required = null;
        ArrayList<Param> optional = null;
        ArrayList<Param> keywords = null;
        ArrayList<Param> aux = null;
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
                      aux = new ArrayList<Param>();
                    aux.add(new AuxParam((Symbol)obj,
                            isSpecial((Symbol)obj, specials), NIL));
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
                        restParam = new RestParam(restVar, isSpecial(restVar, specials));
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
                    envParam = new EnvironmentParam(envVar, isSpecial(envVar, specials));
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
                          optional = new ArrayList<Param>();
                        optional.add(new OptionalParam((Symbol)obj,
                                isSpecial((Symbol)obj, specials), null, false, NIL));
                        if (maxArgs >= 0)
                          ++maxArgs;
                      }
                    else if (state == STATE_KEYWORD)
                      {
                        if (keywords == null)
                          keywords = new ArrayList<Param>();
                        keywords.add(new KeywordParam((Symbol)obj,
                                isSpecial((Symbol)obj, specials), null, false, NIL, null));
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
                          required = new ArrayList<Param>();
                        required.add(new RequiredParam((Symbol)obj,
                                isSpecial((Symbol)obj, specials)));
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
                      aux = new ArrayList<Param>();
                    aux.add(new AuxParam(sym, isSpecial(sym, specials), initForm));
                  }
                else if (state == STATE_OPTIONAL)
                  {
                    Symbol sym = checkSymbol(obj.car());
                    LispObject initForm = obj.cadr();
                    Symbol svar = checkSymbol(obj.cdr().cdr().car());
                    if (optional == null)
                      optional = new ArrayList<Param>();
                    optional.add(new OptionalParam(sym, isSpecial(sym, specials),
                            svar == NIL ? null : svar, isSpecial(svar, specials), initForm));
                    if (maxArgs >= 0)
                      ++maxArgs;
                  }
                else if (state == STATE_KEYWORD)
                  {
                    Symbol keyword;
                    Symbol var;
                    LispObject initForm = NIL;
                    Symbol svar = NIL;
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
                          svar = checkSymbol(obj.car());
                      }
                    if (keywords == null)
                      keywords = new ArrayList<Param>();
                    keywords.add(new KeywordParam(var, isSpecial(var, specials),
                            svar == NIL ? null : svar, isSpecial(svar, specials),
                            initForm, keyword));
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
        ArrayList<Param> positional = new ArrayList<Param>();
        
        if (envParam != null)
            positional.add(envParam);
        if (required != null)
          {
            requiredParameters = new Param[required.size()];
            required.toArray(requiredParameters);
            positional.addAll(required);
          }
        if (optional != null)
          {
            optionalParameters = new Param[optional.size()];
            optional.toArray(optionalParameters);
            positional.addAll(optional);
          }
        if (restParam != null)
            positional.add(restParam);
        if (keywords != null)
          {
            keywordParameters = new KeywordParam[keywords.size()];
            keywords.toArray(keywordParameters);
          }
        if (aux != null)
          {
            auxVars = new Param[aux.size()];
            auxVars = aux.toArray(auxVars);
          }
        
        positionalParameters = positional.toArray(positionalParameters);
      }
    else
      {
        // Lambda list is empty.
        Debug.assertTrue(lambdaList == NIL);
        arity = 0;
        maxArgs = 0;
      }

    this.andKey = _andKey;
    this.allowOtherKeys = _allowOtherKeys;
    minArgs = requiredParameters.length;
    if (arity >= 0)
      Debug.assertTrue(arity == minArgs);
    variables = extractVariables();
    this.specials = new boolean[variables.length];
    for (int i = 0; i < variables.length; i++)
        this.specials[i] = isSpecial(variables[i], specials);
    
    
    for (Param p : positionalParameters)
        if (p.needsEnvironment()) {
            matcherNeedsEnv = true;
            break;
        }
    if (! matcherNeedsEnv)
        for (Param p : keywordParameters)
            if (p.needsEnvironment()) {
                matcherNeedsEnv = true;
                break;
            }
    if (! matcherNeedsEnv)
        for (Param p : auxVars)
            if (p.needsEnvironment()) {
                matcherNeedsEnv = true;
                break;
            }
    
    
    if (keywordParameters.length == 0) {
      matcher = new FastMatcher();
    } else {
      matcher = new SlowMatcher();
    }
    

    
  }
  
  public void setFunction(Operator fun) {
      function = fun;
  }
  
  /** Matches the function call arguments 'args' with the lambda list,
   * returning an array with variable values to be used. The array is sorted
   * the same way as the variables returned by the 'extractVariables' function.
   * 
   * @param args Funcion call arguments to be matched
   * @param _environment Environment to be used for the &environment variable
   * @param env Environment to evaluate initforms in
   * @param thread Thread to be used for binding special variables
   *    -- must be LispThread.currentThread()
   * @return An array of LispObjects corresponding to the values to be bound
   *   to the variables in the lambda list
   */
  public LispObject[] match(LispObject[] args, Environment _environment,
           Environment env, LispThread thread) {
      if (matcherNeedsEnv) {
          if (thread == null)
              thread = LispThread.currentThread();
          
          env = new Environment((env == null) ? _environment : env);
      }
      LispObject[] rv = matcher.match(args, _environment, env, thread);
      for (int i = 0; i < rv.length; i++)
          Debug.assertTrue(rv[i] != null);
      return rv;
  }

  /** Binds the variable values returned from 'match' to their corresponding
   * variables in the environment 'env', with specials bound in thread 'thread'.
   * 
   * @param values Values to be bound
   * @param env
   * @param thread 
   */
  public void bindVars(LispObject[] values, Environment env, LispThread thread) {
      for (int i = 0; i < variables.length; i++) {
          Symbol var = variables[i];
          // If a symbol is declared special after a function is defined,
          // the interpreter binds a lexical variable instead of a dynamic
          // one if we don't check isSpecialVariable()
          bindArg(specials[i] || var.isSpecialVariable(),
                  var, values[i], env, thread);
      }
  }
  
  public Symbol[] freeSpecials(LispObject specials) {
      ArrayList<Symbol> list = new ArrayList<Symbol>();
      
      next_special:
          while (specials != NIL) {
              Symbol special = (Symbol)specials.car();
              specials = specials.cdr();

              for (Symbol v : variables)
                  if (v == special)
                      continue next_special;

              list.add(special);
          }

      Symbol[] rv = new Symbol[list.size()];
      return list.toArray(rv);
  }
  
  public int getArity() {
      return arity;
  }

  public int getMinArgs() {
      return minArgs;
  }
  
  public int getMaxArgs() {
      return maxArgs;
  }
  
  public Symbol[] getVariables() {
      return variables;
  }
  
  private static void invalidParameter(LispObject obj) {
    error(new ProgramError(obj.princToString() +
                         " may not be used as a variable in a lambda list."));
  }

  private Symbol[] extractVariables()
  {
    ArrayList<Symbol> vars = new ArrayList<Symbol>();
    for (Param parameter : positionalParameters)
      parameter.addVars(vars);
    for (Param parameter : keywordParameters)
        parameter.addVars(vars);
    for (Param parameter : auxVars)
        parameter.addVars(vars);
    Symbol[] array = new Symbol[vars.size()];
    vars.toArray(array);
    return array;
  }

  /** Internal class implementing the argument list to lambda list matcher.
   * Because we have two implementations - a fast one and a slower one - we
   * need this abstract super class */
  private static abstract class ArgumentMatcher {
      abstract LispObject[] match(LispObject[] args, Environment _environment,
              Environment env, LispThread thread);
  }
  
  /** ArgumentMatcher class which implements full-blown argument matching,
   * including validation of the keywords passed. */
  private class SlowMatcher extends ArgumentMatcher {
      private LispObject[] _match(LispObject[] args, Environment _environment,
                Environment env, LispThread thread) {
        final ArgList argslist = new ArgList(_environment, args);
        final LispObject[] array = new LispObject[variables.length];
        int index = 0;

        
        for (Param p : positionalParameters)
            index = p.assign(index, array, argslist, env, thread);

        if (andKey) {
            argslist.assertRemainderKeywords();

            for (Param p : keywordParameters)
                index = p.assign(index, array, argslist, env, thread);
        }
        for (Param p : auxVars)
            index = p.assign(index, array, argslist, env, thread);

        if (andKey) {
            if (allowOtherKeys)
                return array;

            if (!argslist.consumed()) // verify keywords
              {
                LispObject allowOtherKeysValue =
                        argslist.findKeywordArg(Keyword.ALLOW_OTHER_KEYS, NIL);

                if (allowOtherKeysValue != NIL)
                    return array;

                // verify keywords
                next_key:
                  while (! argslist.consumed()) {
                      LispObject key = argslist.consume();
                      argslist.consume(); // consume value

                      if (key == Keyword.ALLOW_OTHER_KEYS)
                          continue next_key;

                      for (KeywordParam k : keywordParameters)
                          if (k.keyword == key)
                              continue next_key;

                      error(new ProgramError("Unrecognized keyword argument " +
                                              key.printObject()));
                  }
              }
        } 

        if (restVar == null && !argslist.consumed())
            error(new WrongNumberOfArgumentsException(function));

        return array;
      }
      
      @Override
      LispObject[] match(LispObject[] args, Environment _environment,
                Environment env, LispThread thread) {

        if (arity >= 0)
          {
            // Fixed arity.
            if (args.length != arity)
              error(new WrongNumberOfArgumentsException(function, list(args), arity));
            return args;
          }
        // Not fixed arity.
        if (args.length < minArgs)
          error(new WrongNumberOfArgumentsException(function, minArgs, -1));

        if (thread == null)
            return _match(args, _environment, env, thread);
          
        final SpecialBindingsMark mark = thread.markSpecialBindings();
        try {
            return _match(args, _environment, env, thread);
        }
        finally {
            thread.resetSpecialBindings(mark);
        }
      }
  }
  
  /** Slimmed down ArgumentMatcher which doesn't implement keyword verification. */
  private class FastMatcher extends ArgumentMatcher {
      @Override
      LispObject[] match(LispObject[]  args, Environment _environment,
                Environment env, LispThread thread) {
        final int argsLength = args.length;
        if (arity >= 0)
          {
            // Fixed arity.
            if (argsLength != arity)
              error(new WrongNumberOfArgumentsException(function, list(args), arity));
            return args;
          }
        // Not fixed arity.
        if (argsLength < minArgs)
          error(new WrongNumberOfArgumentsException(function, minArgs, -1));
        
        final ArgList arglist = new ArgList(_environment, args);
        final LispObject[] array = new LispObject[variables.length];
        int index = 0;

        // Required parameters.
        for (Param p : positionalParameters)
            index = p.assign(index, array, arglist, env, thread);
        for (Param p : auxVars)
            index = p.assign(index, array, arglist, env, thread);

        if (andKey && !arglist.consumed())
          {
            // remaining arguments must be keyword/value pairs
            arglist.assertRemainderKeywords();
            
            if (allowOtherKeys)
                return array;
            
            LispObject allowOtherKeysValue =
                    arglist.findKeywordArg(Keyword.ALLOW_OTHER_KEYS, null);
            
            if (allowOtherKeysValue == NIL) {
                // the argument is there.
                LispObject key = arglist.consume();
                arglist.consume();
                
                if (key != Keyword.ALLOW_OTHER_KEYS)
                    error(new ProgramError("Invalid keyword argument " + key.printObject()));
                
                allowOtherKeysValue = null;
            }
            
            if (allowOtherKeysValue != null)
                return array;
            
          }
        if (!arglist.consumed())
          {
            if (restVar == null)
              error(new WrongNumberOfArgumentsException(function));
          }
        return array;
      }
  }
  
  /** Function which creates initform instances.
   * 
   * @param form
   * @return Either a ConstantInitform or NonConstantInitForm instance
   */
  private static InitForm createInitForm(LispObject form) {
      if (form.constantp())
        {
          if (form instanceof Symbol)
            return new ConstantInitForm(form.getSymbolValue());
          if (form instanceof Cons)
            {
              Debug.assertTrue(form.car() == Symbol.QUOTE);
              return new ConstantInitForm(form.cadr());
            }
          return new ConstantInitForm(form);
        }
      return new NonConstantInitForm(form);
  }
  
  /** Class to be passed around, allowing arguments to be 'consumed' from it. */
  final private static class ArgList {
      final LispObject[] args;
      int argsConsumed = 0;
      final int len;
      final Environment env;
      
      ArgList(Environment environment, LispObject[] args) {
          this.args = args;
          len = args.length;
          env = environment;
      }

      /** Asserts the number of remaining arguments is even. */
      void assertRemainderKeywords() {
          if (((len - argsConsumed) & 1) == 1)
              error(new ProgramError("Odd number of keyword arguments."));
      }
      
      /** Returns the next unconsumed value from the argument set, or 'null'
       * if all arguments have been consumed. */
      LispObject consume() {
          return (argsConsumed < len) ? args[argsConsumed++] : null;
      }
      
      /** Returns 'true' if all arguments have been consumed, false otherwise. */
      boolean consumed() {
          return (len == argsConsumed);
      }

      /** Returns the value associated with 'keyword', or 'def' if the keyword
       * isn't in the remaining arguments. Assumes the remainder is a valid property list. */
      LispObject findKeywordArg(Symbol keyword, LispObject def) {
        int i = argsConsumed;
        while (i < len)
          {
            if (args[i] == keyword)
                return args[i+1];
            i += 2;
          }
        return def;
      }

      Environment getEnvironment() {
          // ### here to satisfy the need of the EnvironmentParam, but this
          // is a slight abuse of the abstraction. Don't want to solve more complex,
          // but don't really like it this way...
          return env;
      }
      
      /** Returns a list of all values not consumed so far. */
      LispObject rest() {
        LispObject rest = NIL;
        for (int j = len; j-- > argsConsumed;)
            rest = new Cons(args[j], rest);
        
        return rest;
      }
  }
  
  /** Abstract parent of the classes used to represent the different argument types:
   *
   * - EnvironmentParam
   * - RequiredParam
   * - OptionalParam
   * - RestParam
   * - KeywordParam
   * - AuxParam
   * */
  public static abstract class Param {
      
      /** Assigns values to be bound to the correcsponding variables to the
       * array, using 'index' as the next free slot, consuming any required
       * values from 'args'. Uses 'ext' both as the evaluation environment
       * for initforms.
       * 
       * The environment 'ext' is prepared for evaluating any initforms of
       * further arguments by binding the variables to their values in it.
       * 
       * The environment 'ext' may be null, indicating none of the arguments
       * need an evaluation environment. No attempt should be made to bind
       * any variables in this case.
       * 
       * Returns the index of the next-unused slot in the 'array'.
       */
      abstract int assign(int index, LispObject[] array, ArgList args,
              Environment ext, LispThread thread);
      
      /** Returns 'true' if the parameter requires an evaluation environment
       * in order to be able to determine the value of its initform. */
      boolean needsEnvironment() { return false; }
      
      /** Adds the variables to be bound to 'vars' in the same order as they
       * will be assigned to the output array by the 'assign' method. */
      abstract void addVars(List vars);
  }

  
  /** Abstract super class representing initforms. */
  private static abstract class InitForm {
      abstract LispObject getValue(Environment ext, LispThread thread);
      boolean needsEnvironment() { return false; }
  }
  
  /** Constant init forms will be represented using this class. */
  private static class ConstantInitForm extends InitForm {
      LispObject value;
      
      ConstantInitForm(LispObject value) {
          this.value = value;
      }
      
      LispObject getValue(Environment ext, LispThread thread) {
          return value;
      }
  }
  
  
  /** Non-constant initforms will be represented using this class.
   * Callers need to know these need an evaluation environment. */
  private static class NonConstantInitForm extends InitForm {
      LispObject form;
      
      NonConstantInitForm(LispObject form) {
          this.form = form;
      }
      
      LispObject getValue(Environment ext, LispThread thread) {
          return eval(form, ext, thread);
      }
      
      @Override
      boolean needsEnvironment() { return true; }
  }
  
  /** Class used to match &environment arguments */
  private static class EnvironmentParam extends Param {
      Symbol var;
      boolean special;
      
      EnvironmentParam(Symbol var, boolean special) {
          this.var = var;
          this.special = special;
      }

        @Override
        void addVars(List vars) {
            vars.add(var);
        }

        @Override
        int assign(int index, LispObject[] array, ArgList args, Environment ext, LispThread thread) {
            array[index++] = args.getEnvironment();
            if (ext != null)
                bindArg(special, var, args.getEnvironment(), ext, thread);
            
            return index;
        }
  }
  
  
  /** Class used to match required parameters */
  public static class RequiredParam extends Param {
      Symbol var;
      boolean special;
      
      // Used above to create anonymous required parameters
      public RequiredParam() {
          this(T, false);
      }
      
      public RequiredParam(Symbol var, boolean special) {
          this.var = var;
          this.special = special;
      }
      
      @Override
      int assign(int index, LispObject[] array, ArgList args,
              Environment ext, LispThread thread) {
          LispObject value = args.consume();
          if (ext != null)
            bindArg(special, var, value, ext, thread);
          array[index++] = value;
          return index;
      }
      
      void addVars(List vars) {
          vars.add(var);
      }
  }
    
  /** Class used to match optional parameters, or, if not provided,
   * evaluate the initform. Also assigns the 'supplied-p' parameter if requested. */
  public static class OptionalParam extends Param {
      Symbol var;
      boolean special;
      Symbol suppliedVar;
      boolean suppliedSpecial;
      InitForm initForm;
      
      public OptionalParam(boolean suppliedVar, LispObject form) {
          this(T, false, suppliedVar ? T : null, false, form);
      }
      
      public OptionalParam(Symbol var, boolean special,
                    Symbol suppliedVar, boolean suppliedSpecial,
                    LispObject form) {
          this.var = var;
          this.special = special;
          
          this.suppliedVar = suppliedVar;
          this.suppliedSpecial = suppliedSpecial;
          
          initForm = createInitForm(form);
      }
      
      @Override
      int assign(int index, LispObject[] array, ArgList args,
              Environment ext, LispThread thread) {
          LispObject value = args.consume();
          
          return assign(index, array, value, ext, thread);
      }
      
      int assign(int index, LispObject[] array, LispObject value,
              Environment ext, LispThread thread) {
          if (value == null) {
              value = array[index++] = initForm.getValue(ext, thread);
              if (suppliedVar != null)
                array[index++] = NIL;
          } else {
              array[index++] = value;
              if (suppliedVar != null)
                array[index++] = T;
          }
          
          if (ext != null) {
              bindArg(special, var, value, ext, thread);
              if (suppliedVar != null)
                  bindArg(suppliedSpecial, suppliedVar, array[index-1], ext, thread);
          }
          
          return index;
      }
      
      
      @Override
      boolean needsEnvironment() {
          return initForm.needsEnvironment();
      }

      void addVars(List vars) {
          vars.add(var);
          if (suppliedVar != null)
              vars.add(suppliedVar);
      }
  }

  
  /** Class used to model the &rest parameter */
  private static class RestParam extends Param {
      Symbol var;
      boolean special;
      
      RestParam(Symbol var, boolean special) {
          this.var = var;
          this.special = special;
      }
      
      @Override
      int assign(int index, LispObject[] array, ArgList args,
                Environment ext, LispThread thread) {
          array[index++] = args.rest();

          if (ext != null)
              bindArg(special, var, array[index-1], ext, thread);

          return index;
      }
      
      @Override
      void addVars(List vars) {
          vars.add(var);
      }
  }
  
  /** Class used to represent optional parameters and their initforms */
  public static class KeywordParam extends OptionalParam {
      public Symbol keyword;
      
      public KeywordParam(boolean suppliedVar, LispObject form, Symbol keyword) {
          this(T, false, suppliedVar ? T : null, false, form, keyword);
      }
      
      public KeywordParam(Symbol var, boolean special,
                   Symbol suppliedVar, boolean suppliedSpecial,
                   LispObject form, Symbol keyword) {
          super(var, special, suppliedVar, suppliedSpecial, form);
          
          this.keyword = (keyword == null)
                  ? PACKAGE_KEYWORD.intern(var.getName()) : keyword;
      }
      
      @Override
      int assign(int index, LispObject[] array, ArgList args,
              Environment ext, LispThread thread) {
          return super.assign(index, array, args.findKeywordArg(keyword, null),
                  ext, thread);
      }
  }
  
  
  /** Class used to represent &aux parameters and their initforms */
  private static class AuxParam extends Param {
    Symbol var;
    boolean special;
    InitForm initform;

    AuxParam(Symbol var, boolean special, LispObject form) {
        this.var = var;
        this.special = special;
        initform = createInitForm(form);
    }

    @Override
    void addVars(List vars) {
        vars.add(var);
    }

    @Override
    int assign(int index, LispObject[] array, ArgList args, Environment ext, LispThread thread) {
        array[index++] = initform.getValue(ext, thread);
        
        if (ext != null)
            bindArg(special, var, array[index-1], ext, thread);
        
        return index;
    }

    @Override
    boolean needsEnvironment() {
        return initform.needsEnvironment();
    }
      
  }
}
