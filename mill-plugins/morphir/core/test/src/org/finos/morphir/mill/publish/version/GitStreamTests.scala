package org.finos.morphir.mill.publish.version

import utest.*

object GitStreamTests extends TestSuite {
  private def git(cwd: os.Path, args: String*): Unit =
    os.proc("git" +: args).call(cwd = cwd, stdout = os.Pipe, stderr = os.Pipe)

  private def repository(): os.Path = {
    val root = os.temp.dir()
    git(root, "init", "--initial-branch=main")
    git(root, "config", "user.email", "test@example.com")
    git(root, "config", "user.name", "Test")
    os.write(root / "file.txt", "one")
    git(root, "add", ".")
    git(root, "commit", "-m", "one")
    root
  }

  private def commit(root: os.Path, content: String): Unit = {
    os.write.over(root / "file.txt", content)
    git(root, "add", ".")
    git(root, "commit", "-m", content)
  }

  val tests = Tests {
    test("finds the stream's own tag and the distance from it") {
      val root = repository()
      git(root, "tag", "v0.5.0")
      commit(root, "two")
      commit(root, "three")

      val state = GitStream.resolve(root, TagStream(None)).toOption.get
      assert(state.lastTag == Some("v0.5.0"))
      assert(state.distance == 2)
      assert(state.revision.length >= 7)
      assert(!state.dirty)
    }

    test("ignores another stream's tag entirely") {
      // The property the whole design rests on: a desktop release must not become the
      // libraries' nearest tag.
      val root = repository()
      git(root, "tag", "v0.5.0")
      commit(root, "two")
      git(root, "tag", "desktop/v0.3.0")
      commit(root, "three")

      val libraries = GitStream.resolve(root, TagStream(None)).toOption.get
      assert(libraries.lastTag == Some("v0.5.0"))
      assert(libraries.distance == 2)

      val desktop = GitStream.resolve(root, TagStream(Some("desktop"))).toOption.get
      assert(desktop.lastTag == Some("desktop/v0.3.0"))
      assert(desktop.distance == 1)
    }

    test("an untagged stream reports no tag and counts every commit") {
      val root = repository()
      commit(root, "two")

      val state = GitStream.resolve(root, TagStream(Some("desktop"))).toOption.get
      assert(state.lastTag == None)
      assert(state.distance == 2)
    }

    test("a dirty working tree is reported") {
      val root = repository()
      git(root, "tag", "v0.5.0")
      os.write.over(root / "file.txt", "uncommitted")

      val state = GitStream.resolve(root, TagStream(None)).toOption.get
      assert(state.dirty)
    }

    test("a directory that is not a repository is an error, not an exception") {
      val result = GitStream.resolve(os.temp.dir(), TagStream(None))
      assert(result.isLeft)
    }
  }
}
