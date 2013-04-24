/*
 * SlotDefinitionClass.java
 *
 * Copyright (C) 2005 Peter Graves
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

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

public final class SlotDefinitionClass extends StandardClass
{
    public SlotDefinitionClass(Symbol symbol, LispObject cpl) {
        super(symbol, cpl);
        LispObject[] instanceSlotNames = {
            Symbol.NAME,
            Symbol.INITFUNCTION,
            Symbol.INITFORM,
            Symbol.INITARGS,
            Symbol.READERS,
            Symbol.WRITERS,
            Symbol.ALLOCATION,
            Symbol.ALLOCATION_CLASS,
            Symbol.LOCATION,
            Symbol._TYPE,
            Symbol._DOCUMENTATION
        };
        setClassLayout(new Layout(this, instanceSlotNames, NIL));
        //Set up slot definitions so that this class can be extended by users
        LispObject slotDefinitions = NIL;
        for(int i = instanceSlotNames.length - 1; i >= 0; i--) {
            slotDefinitions = slotDefinitions.push(new SlotDefinition(this, instanceSlotNames[i]));
        }
        // The Java class SlotDefinition sets the location slot to NIL
        // in its constructor; here we make Lisp-side subclasses of
        // standard-*-slot-definition do the same.
        LispObject locationSlot = slotDefinitions.nthcdr(8).car();
        SlotDefinition.SET_SLOT_DEFINITION_INITFORM.execute(locationSlot, NIL);
        SlotDefinition.SET_SLOT_DEFINITION_INITFUNCTION.execute(locationSlot, StandardClass.constantlyNil);
        // Fix initargs of TYPE, DOCUMENTATION slots.
        LispObject typeSlot = slotDefinitions.nthcdr(9).car();
        SlotDefinition.SET_SLOT_DEFINITION_INITARGS.execute(typeSlot, list(internKeyword("TYPE")));
        LispObject documentationSlot = slotDefinitions.nthcdr(10).car();
        SlotDefinition.SET_SLOT_DEFINITION_INITARGS.execute(documentationSlot, list(internKeyword("DOCUMENTATION")));
        setDirectSlotDefinitions(slotDefinitions);
        setSlotDefinitions(slotDefinitions);
        setFinalized(true);
    }
}
