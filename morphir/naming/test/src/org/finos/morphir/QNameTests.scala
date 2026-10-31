package org.finos.morphir

import kyo.test.*
import org.finos.morphir.naming.*

class QNameTests extends Test[Any]:

  "Creating a tuple from QName" - {
    "toTuple should provide the Path and Name as a tuple" in {
      val path     = Path.fromString("ice.cream")
      val name     = Name.fromString("float")
      val expected = (path, name)
      assert(QName(path, name).toTuple == expected)
    }
  }

  "Creating a QName" - {
    "Creating a QName with a tuple" in {
      val path = Path.fromString("friday")
      val name = Name.fromString("night")
      assert(QName.fromTuple((path, name)) == QName(path, name))
    }
    "Creating a QName from a name" in {
      val path = Path.fromString("blog.Author")
      val name = Name.fromString("book")
      assert(QName.fromName(path, name) == QName(path, name))
    }
  }

  "Fetching values from QName" - {
    "localName and path" in {
      val path = Path.fromString("path")
      val name = Name.fromString("name")
      assert(QName.getLocalName(QName(path, name)) == name)
      assert(QName.getModulePath(QName(path, name)) == path)
    }
  }

  "QName and Strings" - {
    "Create String from QName" in {
      val path = Path.fromString("front.page")
      val name = Name.fromString("dictionary words")
      assert(QName(path, name).toString == "Front.Page:dictionaryWords")
    }
    "Create QName from String" in {
      val str = "Proper.Path:name"
      assert(QName.fromString(str) == Some(QName(Path.fromString("Proper.Path"), Name.fromString("name"))))
    }
    "Provide an invalid String" in {
      val str2 = "invalidpathname"
      assert(QName.fromString(str2) == None)
    }
  }
end QNameTests
