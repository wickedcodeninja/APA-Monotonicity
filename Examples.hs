-- |(C) Wout Elsinghorst 2013

module Examples (
  cp1,
  cp2,
  ae1,
  random,
  fac,
  fib
  ) where

import Framework.While
import Framework.While.Parser

formatDocument = unlines . fmap (++ "\n")

ae1 = parseProgram . formatDocument $
  [ "r := 7;        "       
  , "t := r + 13;   "
  , "k := 17;       "
  , "p := k + t;    "
  , "r := 3;        "
  ]
  
cp1 = parseProgram . formatDocument $
  [ "proc testA ( var a, ret t ) {"
  , "  l := 666;                  " -- l will not be constant anymore during and after the execution of the
  , "  u := 30;                   " -- while loop. u on the otherhand will always be 30.
  , "                             "
  , "  i := 0;                    "
  , "  while (i < 3) do {         "
  , "    u := 123;                "
  , "    l := l + a;              "
  , "    u := 28 + a;             " -- The function is always called with a = 2, so u will always be 30 in
  , "  };                         " -- the body of the loop. This information will propagate via the return 
  , "                             " -- value.
  , "  t := u;                    "
  , "}                            "
  , "                             "
  , "z := 2;                      "
  , "call testA ( z , h );        " -- Give z as an argument, store the return value in h
  , "k := h;                      " -- Propagate the return value further 
  ]

cp2 = parseProgram . formatDocument $
  [ "proc testA ( var k, ret r ) {" -- Without call stacks, the value of k won't be constant when testA
  , "  v := 7;                    " -- is called twice with two different arguments. With call stacks,
  , "  r := k + v;                " -- the value of k is constant --given the context of the call--.
  , "}                            "
  , "proc testB ( var h, ret s ) {"
  , "  w := 15;                   "
  , "  s := h + w;                "
  , "}                            "
  , "                             "
  , "x := 3;                      "
  , "y := 5;                      "
  , "call testA ( x , f );        "
  , "call testB ( y , g );        " -- Change from testB to testA to create a poisonous 
  , "p := f;                      " -- relationship with the previous call to testA. Increase 
  , "q := g;                      " -- call stack to something > 1 to cure the situation.
  ]


random = parseProgram $ formatDocument $
  [ "proc randomMeuk ( var x, ret p ) {"
  , "  var x;    "
  , "  while (3 < x) do {           "
  , "    x := 3 + 5;                "
  , "    b := a + 1;                "
  , "  };                           "
  , "  skip;                        "
  , "  while false do {             "
  , "    skip;                      "
  , "  };                           "
  , "  skip;                        "
  , "  p := x;                       "
  , "}                              "
  
  , "proc factorial ( var y, ret q ) {"
  , "  z := 1;               "
  , "  while (1 < y) do {    "
  , "    z := z*y;           "
  , "    y := y-1;           "
  , "  };                    "
  , "  y := 0;               "
  , "  r := y;               "  
  , "}                       "
  
  , "proc entry ( ) {                "
  , "  m := 3;                       "
  , "  call factorial ( m , n );     "
  , "  skip;                         "
  , "}                               "
  
  , "call entry ( );                          "
  ]
  
fac = parseProgram $ formatDocument $
  [ "proc factorial ( var a, ret r ) {"
  , "  z := 1;                "
  , "  while (1 < a) do {     "
  , "    z := z*a;            "
  , "    a := a-1;            "
  , "  };                     "
  , "  a := 0;                "
  , "  r := y;                "  
  , "}                        "
  , "x := 3;                  "
  , "call factorial ( x , z ); "
  ]

fib = parseProgram $ formatDocument $
  [ "proc fib ( var z, var u, ret v ) { "
  , "  if ( z < 3 ) then {              "
  , "    v := u + 1;                    " 
  , "  } else {                         " 
  , "    call fib ( z - 1 , u , b );    "
  , "    call fib ( z - 2 , v , v );    "
  , "  };                               "
  , "}                                  "
  , "call fib ( x , 0 , y );            "
  ]
  
