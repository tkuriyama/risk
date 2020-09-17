module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite = test "hello world" (\_ -> Expect.equal 4 (2 + 2))
    -- todo "Test basic Raitional creation"
