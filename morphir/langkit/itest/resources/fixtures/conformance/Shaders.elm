effect module Conformance.Shaders where { command = ShaderCmd, subscription = ShaderSub } exposing (..)

{-| An effect module, its `where` clause, and a GLSL block — the structural
forms that have no counterpart in an ordinary module.
-}


type ShaderCmd
    = NoCommand


type ShaderSub
    = NoSubscription


vertexShader : String
vertexShader =
    "placeholder"


fragment =
    [glsl|
precision mediump float;
uniform vec3 colour;

void main () {
    gl_FragColor = vec4(colour, 1.0);
}
|]
