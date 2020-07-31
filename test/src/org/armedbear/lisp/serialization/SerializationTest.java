package org.armedbear.lisp.serialization;

import org.armedbear.lisp.*;

import java.io.*;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.lang.reflect.Field;

import org.junit.Test;

import static org.armedbear.lisp.Lisp.NIL;
import static org.armedbear.lisp.Lisp.list;
import static org.junit.Assert.*;

public class SerializationTest {

    @Test
    public void testSerializationOfBuiltInFunction() throws Exception {
        Field declaredField = Primitives.class.getDeclaredField("CONS");
        declaredField.setAccessible(true);
        Object consFunction = declaredField.get(null);

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(baos);
        oos.writeObject(consFunction);

        ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
        Object readObject = ois.readObject();
        assertEquals(readObject, consFunction);
    }

    @Test
    public void testSerializationOfLocalFunction() throws Exception {
        LispObject lambda_expression =
                new Cons(Symbol.LAMBDA, new Cons(NIL, NIL));
        LispObject lambda_name =
                list(Symbol.FLET, new Symbol("test"));
        Closure closure =
                new Closure(lambda_name, lambda_expression, new Environment());

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ObjectOutputStream oos = new ObjectOutputStream(baos);
        oos.writeObject(closure);

        ObjectInputStream ois = new ObjectInputStream(new ByteArrayInputStream(baos.toByteArray()));
        Object readObject = ois.readObject();
        assertEquals(readObject, closure);
    }

}
