/*
 * native.c
 *
 * Copyright (C) 2004-2007 Peter Graves
 * $Id: native.c,v 1.2 2007-03-28 12:18:01 piso Exp $
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
 */

#include "native.h"
#include <signal.h>
#include <unistd.h>
#include <sys/times.h>

static int pipefds[2];

static void ctrl_c_handler(int sig)
{
  char c;

  /* Posix mandates SYSV semantics... could use sigaction () */
  signal(SIGINT, ctrl_c_handler);
  write(pipefds[1], &c, 1);
}

void
JNU_ThrowByName(JNIEnv *env, const char *name, const char *msg)
{
  jclass cls = (*env)->FindClass(env, name);
  /* if cls is NULL, an exception has already been thrown */
  if (cls != NULL)
    (*env)->ThrowNew(env, cls, msg);
  /* free the local ref */
  (*env)->DeleteLocalRef(env, cls);
}

JNIEXPORT void JNICALL
Java_org_armedbear_lisp_Native_installControlCHandler(JNIEnv *env, jclass cls)
{
  jmethodID mid = (*env)->GetStaticMethodID(env, cls, "callback", "()V");

  if (pipe(pipefds) != 0)
    {
      JNU_ThrowByName (env, "RuntimeException",
                       "pipe() failed in installControlCHandler()");
      return;
    }

  if (signal(SIGINT, ctrl_c_handler) == SIG_ERR)
    {
      JNU_ThrowByName (env, "RuntimeException",
                       "signal() failed in installControlCHandler()");
      return;
    }

  while (1)
    {
      char c;
      read (pipefds[0], &c, 1);
      (*env)->CallStaticVoidMethod(env, cls, mid);
    }
}

JNIEXPORT jlong JNICALL
Java_org_armedbear_lisp_Native_getCurrentThreadUserTime(JNIEnv *env, jclass cls)
{
    struct tms buf;
    times(&buf);
    return buf.tms_utime;
}

JNIEXPORT jlong JNICALL
Java_org_armedbear_lisp_Native_getCurrentThreadSystemTime(JNIEnv *env, jclass cls)
{
    struct tms buf;
    times(&buf);
    return buf.tms_stime;
}
