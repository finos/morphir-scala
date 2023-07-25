# Customizing the Build

## Using build.user.conf

* Add a build.user.conf to the root directory. 
* This file can be used to set BuildSettings -> ScalaSettings. 
* The path to each setting is based on the path in the BuildSettings case class

For example, to disable the JavaScript and Native builds, as well as
to only build with Scala 3.3.0 do the following:

```
# build.user.conf
js.enable=false
native.enable=false
scala.defaultCrossScalaVersions=3.3.0
```

It is very useful to generate the IntelliJ settings file in this mode
as it will radically increase IntelliJ's build performance.

```bash
./mill mill.scalalib.GenIdea/idea
```

## Command Environment Variables

Another way to specify certain properties on the command line is
through environment variables.
```
MORPHIR_BUILD_JVM_ENABLE=false ./mill -i showBuildSettings
```
Make sure to use `./mill -i` with this feature in order to
for these settings to take effect. Otherwise mill will use the background
mill server which is on a separate JVM and these settings will not take
effect (however the build.user.conf approach above will still work).

## Modifying JVM Properties

Use `.mill-jvm-opts` to set Java properties for the build.

## Dev Mode

In order to easily disable Native/JS builds and set the Scala
version to 3.3.0 you can also use a global environment variable.
Add the following to your `.zprofile` (on OSX) or `.bashrc` (on Linux)
etc...

```
export MORPHIR_SCALA_DEV_MODE='true'
```
