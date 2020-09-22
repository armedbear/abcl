package org.armedbear.lisp;

import java.util.List;
import java.text.MessageFormat;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class PathnameURLTest {
  

  @Test
  public void roundTrips() {
    String namestrings[] = {
      "https://www.youtube.com/user/BlackHatOfficialYT"
    };

    for (String namestring  : namestrings) {
      Pathname result = (Pathname) Pathname.create(namestring);
      String resultingNamestring = result.getNamestring();
      String message = MessageFormat.format("Namestring \"{0}\" failed to roundtrip", namestring);
      assertTrue(message,
                 namestring.equals(resultingNamestring));
    }
  }
}
