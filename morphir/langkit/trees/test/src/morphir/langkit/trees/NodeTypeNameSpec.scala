package morphir.langkit.trees

import kyo.test.*

class NodeTypeNameSpec extends Test[Any]:

  "NodeTypeName" - {
    "validation" - {
      "accepts a non-blank identifier-like string" in
        assert(NodeTypeName.make("CstIntLiteral").isRight)
      "accepts strings with internal whitespace" in
        assert(NodeTypeName.make("foo bar").isRight)
      "rejects the empty string" in
        assert(NodeTypeName.make("").isLeft)
      "rejects a whitespace-only string" in {
        assert(NodeTypeName.make(" ").isLeft)
        assert(NodeTypeName.make("\t\n ").isLeft)
      }
    }
    "equality and unwrap" - {
      "two NodeTypeNames with the same underlying string are equal" in {
        val a = NodeTypeName.make("CstIntLiteral").toOption.get
        val b = NodeTypeName.make("CstIntLiteral").toOption.get
        assert(a == b)
      }
      "unwrap returns the original string" in {
        val n = NodeTypeName.make("CstIntLiteral").toOption.get
        assert(NodeTypeName.unwrap(n) == "CstIntLiteral")
      }
    }
  }
