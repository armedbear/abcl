package org.armedbear.lisp;

import static java.lang.Math.abs;
import java.util.Random;

import org.junit.After;
import static org.junit.Assert.*;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.JUnitCore;
import org.junit.Assert;
import java.util.Date;

/**
 * Unit tests for {@link FastStringBuffer}.
 */
public class FastStringBufferTest
{
    /** Class under test. */
    private static final Class CLASS = FastStringBuffer.class;
    private static final Random random = new Random();
    private static final String CTOR_ARG = "abcde";

    public static void main(final String args[]) {
        JUnitCore.main("org.armedbear.lisp.FastStringBufferTest");
    }

    /** Test instance. */
    private FastStringBuffer buffer = null;

    @Before
    public void setUp()
    {
        buffer = new FastStringBuffer(CTOR_ARG);
    }

    @After
    public void tearDown()
    {
        buffer = null;
    }

    @Test
    public void defaultConstructor()
    {
        assertNotNull("Default constructor failed", new FastStringBuffer());
    }

    @Test
    public void constructorString()
    {
        assertNotNull("String constructor failed",
                      new FastStringBuffer(CTOR_ARG));
    }

    @Test
    public void constructorchar()
    {
        assertNotNull("char constructor failed", new FastStringBuffer('c'));
    }

    @Test
    public void constructorint()
    {
        assertNotNull("int constructor failed", new FastStringBuffer(12));
    }

    @Test(expected=OutOfMemoryError.class)
    public void constructorMaxint()
    {
        new FastStringBuffer(Integer.MAX_VALUE);
    }

    @Test(expected=NegativeArraySizeException.class)
    public void constructorMinint()
    {
        new FastStringBuffer(Integer.MIN_VALUE);
    }

    @Test
    public void lengthAfterConstructorint()
    {
        final FastStringBuffer foo = new FastStringBuffer(234);
        assertEquals("Length from int constructor not 0", 0, foo.length());
    }

    @Test
    public void lengthAfterDefaultConstructor()
    {
        assertEquals("Length from default constructor not 0", 0,
                     new FastStringBuffer().length());
    }

    @Test
    public void lengthAfterConstructorString()
    {
        final int len = CTOR_ARG.length();
        assertEquals("Length from String constructor not " + len, len,
                     new FastStringBuffer(CTOR_ARG).length());
    }

    @Test
    public void lengthAfterConstructorchar()
    {
        final char w = 'w';
        final FastStringBuffer newBuffer = new FastStringBuffer(w);
        final int len = newBuffer.length();
        assertEquals("Length from char constructor: " + len, 1, len);
    }

    // Target method to be made private during refactoring
    // @Test(expect=NoSuchMethodException.class)
    @Test
    public void capacity() throws NoSuchMethodException
    {
        CLASS.getMethod("capacity", (Class[]) null);
    }

    @Test
    public void charAt()
    {
        assertEquals("Indexed char unexpected", 'c', buffer.charAt(2));
    }

    @Test
    public void getChars()
    {
        final char[] dst = {0, 0};
        final char[] cmp = {'b', 'c'};
        buffer.getChars(1, 3, dst, 0);
        assertArrayEquals("Subarray unexpected; cmp: " + new String(cmp) +
                          ", dst: " + new String(dst), cmp, dst);
    }

    @Test(expected=StringIndexOutOfBoundsException.class)
    public void getCharsBadStartIndex()
    {
        buffer.getChars(-1, -1, null, 0);
    }

    @Test(expected=StringIndexOutOfBoundsException.class)
    public void getCharsInvertedStartEnd()
    {
        buffer.getChars(3, 1, null, 0);
    }

    @Test(expected=StringIndexOutOfBoundsException.class)
    public void getCharsExcessiveEnd()
    {
        buffer.getChars(1, 7, null, 0);
    }

    @Test
    public void setCharAt()
    {
        buffer.setCharAt(2, 'z');
        assertEquals("Incorrect setCharAt", "abzde", buffer.toString());
    }

    @Test(expected=IndexOutOfBoundsException.class)
    public void setCharAtExcess()
    {
        buffer.setCharAt(8, 'x');
    }

    @Test(expected=StringIndexOutOfBoundsException.class)
    public void setCharAtNeg()
    {
        buffer.setCharAt(-2, 'x');
    }

    @Test
    public void ensureCapacity()
    {
        buffer.ensureCapacity(200);
        assertTrue("Unexpected capacity", buffer.capacity() >= 200);
    }

    @Test(expected=NoSuchMethodException.class)
    public void setText() throws NoSuchMethodException
    {
        FastStringBuffer.class.getMethod("setText");
    }

    @Test
    public void append()
    {
        buffer.append("fgh");
        assertEquals("abcdefgh", buffer.toString());
    }

    @Test
    public void appendNullString()
    {
        buffer.append((String)null);
        assertEquals("abcdenull", buffer.toString());
    }

    @Test
    public void appendcharArray()
    {
        buffer.append(new char[]{'x', 'y', 'z'});
        assertEquals("abcdexyz", buffer.toString());
    }

    @Test(expected=NullPointerException.class)
    public void appendNullcharArray()
    {
        buffer.append((char []) null);
    }

    @Test(expected=IndexOutOfBoundsException.class)
    public void appendWithin()
    {
        buffer.append(new char[]{'x', 'y', 'z'}, 1, 3);
        assertEquals("abcdexyz", buffer.toString());
    }

    @Test
    public void appendObject()
    {
        buffer.append(new Date());
        assertTrue(buffer.length() > 5);
    }

    @Test
    public void appendchar()
    {
        buffer.append('f');
        assertEquals("abcdef", buffer.toString());
    }

    @Test
    public void appendint()
    {
        buffer.append(1);
        assertEquals("abcde1", buffer.toString());
    }

    @Test
    public void appendlong()
    {
        buffer.append(1L);
        assertEquals("abcde1", buffer.toString());
    }

    @Test
    public void setLength()
    {
        buffer.setLength(3);
        assertEquals("abc", buffer.toString());
    }

    @Test(expected=IndexOutOfBoundsException.class)
    public void setLengthNeg()
    {
        buffer.setLength(-1);
    }

    @Test
    public void setLengthExcess()
    {
        buffer.setLength(12);
        assertEquals(12, buffer.length());
        // This just seems weird to me
        assertFalse(CTOR_ARG.equals(buffer.toString()));
    }

    @Test
    public void reverse()
    {
        buffer.reverse();
        assertEquals("edcba", buffer.toString());
    }

    @Test
    public void testToString()
    {
        assertEquals(CTOR_ARG, buffer.toString());
    }

    @Test
    public void toCharArray()
    {
        assertArrayEquals(new char[] {'a', 'b', 'c', 'd', 'e'},
                          buffer.toCharArray());
    }

}
