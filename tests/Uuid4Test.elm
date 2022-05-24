module Uuid4Test exposing (..)

import Expect
import Test exposing (Test, describe, test)
import Uuid4


suite : Test
suite =
    describe "The Uuid4 module"
        [ describe "toHex"
            [ test "0" (\_ -> Expect.equal '0' (Uuid4.toHex 0))
            , test "5" (\_ -> Expect.equal '5' (Uuid4.toHex 5))
            , test "10" (\_ -> Expect.equal 'a' (Uuid4.toHex 10))
            , test "15" (\_ -> Expect.equal 'f' (Uuid4.toHex 15))
            , test "16" (\_ -> Expect.equal 'g' (Uuid4.toHex 16))
            ]
        , test "join"
            (\_ ->
                Expect.equal "123e4567-e89b-42d3-8456-426614174000"
                    (Uuid4.join
                        [ '1'
                        , '2'
                        , '3'
                        , 'e'
                        , '4'
                        , '5'
                        , '6'
                        , '7'
                        , 'e'
                        , '8'
                        , '9'
                        , 'b'
                        , '2'
                        , 'd'
                        , '3'
                        , '4'
                        , '5'
                        , '6'
                        , '4'
                        , '2'
                        , '6'
                        , '6'
                        , '1'
                        , '4'
                        , '1'
                        , '7'
                        , '4'
                        , '0'
                        , '0'
                        , '0'
                        ]
                    )
            )
        ]
