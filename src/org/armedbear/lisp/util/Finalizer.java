/*
 * Finalizer.java
 *
 * Copyright (C) 2011 Erik Huelsmann
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
package org.armedbear.lisp.util;

import java.lang.ref.ReferenceQueue;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.WeakHashMap;

/** Framework to monitor arbitrary objects to see if they have been
 * garbage collected, running one or more runnables when they have.
 */
public class Finalizer {

    /** Internal weak reference class which keeps a list of Runnables
     * with finalizing actions to be executed.
     */
    private static class FinalizingWeakReference
            extends java.lang.ref.WeakReference<Object> {

        /** Queue of Runnables to be executed after the object is GC-ed. */
        private LinkedList<Runnable> finalizers = new LinkedList<Runnable>();

        FinalizingWeakReference(Object o, ReferenceQueue q, Runnable finalizer) {
            super(o, q);
            finalizers.add(finalizer);
        }

        /** Adds a finalizer.
         *
         * Finalizers will be run in reverse-registration order.
         *
         * @param finalizer The finalizer to be added.
         */
        void addFinalizer(Runnable finalizer) {
            finalizers.add(finalizer);
        }

        /** Removes all registered finalizers. */
        void cancelFinalizers() {
            finalizers.clear();
        }

        /** Runs all finalizers registered. */
        void run() {
            Iterator<Runnable> iterator = finalizers.iterator();
            while (iterator.hasNext()) {
                iterator.next().run();
            }
        }
    }

    /** Queue for FinalizingWeakReference objects which need
     * to have their references run because the associated
     * object has been garbage collected
     */
    private static ReferenceQueue queue = null;

    /** A map from objects to their associated FinalizingWeakReferences
     * which is used by the routine which cancels finalization.
     */
    private static Map<Object, FinalizingWeakReference> references = null;

    /** A map which maps the finalizing references onto themselves. This map
     * makes sure that hard (as opposed to weak) references stay around to
     * prevent garbage collection of the FinalizingWeakReferences before the
     * referred objects are.
     */
    private static Map<FinalizingWeakReference, FinalizingWeakReference> anchor = null;

    /** Checks that the internal administration variables and thread have been
     * correctly set up.  This solution allows the GC monitoring thread to be
     * started as late as its first use.
     */
    synchronized private static void checkQueue() {
        if (queue == null) {
            queue = new ReferenceQueue();
            references = Collections.synchronizedMap(new WeakHashMap<Object, FinalizingWeakReference>());
            anchor = Collections.synchronizedMap(new HashMap<FinalizingWeakReference, FinalizingWeakReference>());

            Thread handler =
                    new Thread(new Runnable() {

                public void run() {
                    while (true) {
                        try {
                            FinalizingWeakReference ref =
                                    (FinalizingWeakReference) queue.remove();
                            anchor.remove(ref);
                            ref.run();
                        } catch (InterruptedException i) {
                        }
                    }
                }
            }, "ABCL finalizer");

            handler.setPriority(Thread.MAX_PRIORITY);
            handler.setDaemon(true);
            handler.start();
        }
    }

    /** Schedules a Runnable to be run after garbage collection of the object.
     *
     * Note that the Runnable can't contain references to the object to be
     * collected: it will disable garbage collection of the object.
     *
     * @param o The object to monitor for garbage collection
     * @param r The routine to be executed after GC-ing the object
     */
    public static void addFinalizer(Object o, Runnable r) {
        if (queue == null) {
            checkQueue();
        }

        FinalizingWeakReference ref = references.get(o);
        if (ref != null) {
            ref.addFinalizer(r);
        } else {
            ref = new FinalizingWeakReference(o, queue, r);
            references.put(o, ref);
            anchor.put(ref, ref);
        }
    }

    /** Cancels any references scheduled to be run after garbage
     * collection of the argument 'o'.
     *
     * @param o Object to cancel references for
     */
    public static void clearFinalizers(Object o) {
        FinalizingWeakReference ref = references.get(o);

        if (ref != null) {
            ref.cancelFinalizers();
            anchor.remove(ref);
        }
    }
}
