/*
 * HtmlElement.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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
 */

package org.armedbear.j;

import java.util.List;
import java.util.Vector;

public final class HtmlElement
{
    private String  name;
    private boolean wantsEndTag;

    public HtmlElement(String name, boolean wantsEndTag)
    {
        this.name = name;
        this.wantsEndTag = wantsEndTag;
    }

    public final String getName()
    {
        return name;
    }

    public final boolean wantsEndTag()
    {
        return wantsEndTag;
    }

    public final void setWantsEndTag(boolean b)
    {
        wantsEndTag = b;
    }

    public static List getDefaultElements()
    {
        Vector v = new Vector();
        v.add(new HtmlElement("a", true));
        v.add(new HtmlElement("address", true));
        v.add(new HtmlElement("applet", true));
        v.add(new HtmlElement("b", true));
        v.add(new HtmlElement("base", false));
        v.add(new HtmlElement("basefont", false));
        v.add(new HtmlElement("big", true));
        v.add(new HtmlElement("blockquote", true));
        v.add(new HtmlElement("body", true));
        v.add(new HtmlElement("br", false));
        v.add(new HtmlElement("caption", true));
        v.add(new HtmlElement("center", true));
        v.add(new HtmlElement("cite", true));
        v.add(new HtmlElement("code", true));
        v.add(new HtmlElement("dd", true));
        v.add(new HtmlElement("dfn", true));
        v.add(new HtmlElement("div", true));
        v.add(new HtmlElement("dl", true));
        v.add(new HtmlElement("dt", true));
        v.add(new HtmlElement("em", true));
        v.add(new HtmlElement("font", true));
        v.add(new HtmlElement("form", true));
        v.add(new HtmlElement("frame", false));
        v.add(new HtmlElement("frameset", true));
        v.add(new HtmlElement("h1", true));
        v.add(new HtmlElement("h2", true));
        v.add(new HtmlElement("h3", true));
        v.add(new HtmlElement("h4", true));
        v.add(new HtmlElement("h5", true));
        v.add(new HtmlElement("h6", true));
        v.add(new HtmlElement("head", true));
        v.add(new HtmlElement("hr", false));
        v.add(new HtmlElement("html", true));
        v.add(new HtmlElement("i", true));
        v.add(new HtmlElement("img", false));
        v.add(new HtmlElement("input", false));
        v.add(new HtmlElement("isindex", false));
        v.add(new HtmlElement("kbd", true));
        v.add(new HtmlElement("li", true));
        v.add(new HtmlElement("link", false));
        v.add(new HtmlElement("map", true));
        v.add(new HtmlElement("meta", false));
        v.add(new HtmlElement("object", true));
        v.add(new HtmlElement("ol", true));
        v.add(new HtmlElement("option", true));
        v.add(new HtmlElement("p", false));
        v.add(new HtmlElement("pre", true));
        v.add(new HtmlElement("q", true));
        v.add(new HtmlElement("samp", true));
        v.add(new HtmlElement("script", true));
        v.add(new HtmlElement("select", true));
        v.add(new HtmlElement("small", true));
        v.add(new HtmlElement("strike", true));
        v.add(new HtmlElement("strong", true));
        v.add(new HtmlElement("style", true));
        v.add(new HtmlElement("sub", true));
        v.add(new HtmlElement("sup", true));
        v.add(new HtmlElement("table", true));
        v.add(new HtmlElement("td", true));
        v.add(new HtmlElement("textarea", true));
        v.add(new HtmlElement("th", true));
        v.add(new HtmlElement("title", true));
        v.add(new HtmlElement("tr", true));
        v.add(new HtmlElement("tt", true));
        v.add(new HtmlElement("u", true));
        v.add(new HtmlElement("ul", true));
        v.add(new HtmlElement("var", true));
        return v;
    }
}
