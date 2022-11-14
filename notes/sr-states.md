Grammar
-------

    S  → E $
    E  → E + E₁
    E  → E₁
    E₁ → E₁ * E₂
    E₁ → E₂
    E₂ → Int


Parse table
-----------

    |       |      Shift      |     Goto    | Reduce       |
    | State |  +   *  Int  $  |  E   E₁  E₂ |              |
    |-------|-----------------|-------------|--------------|
    |   0   |          7      |  1   4   6  |              |
    |   1   |  3           2  |             |              |
    |   2   |                 |             | S → E$       |
    |   3   |          7      |      9   6  |              |
    |   4   |      5          |             | E → E₁       |
    |   5   |          7      |          8  |              |
    |   6   |                 |             | E₁ → E₂      |
    |   7   |                 |             | E₂ → Int     |
    |   8   |                 |             | E₁ → E₁ * E₂ |
    |   9   |      5          |             | E → E + E₁   |


Example parse
-------------

                  Stack                       Input          Action
     !  .  !  .  !  .  !  .  !  .  !    Int + Int * Int $
    ----------------------------------------------------------------------
     0                                  Int                  S 7
       Int 7                                +                R
     0                                   E₂                  G 6
       E₂  6                                +                R
     0                                   E₁                  G 4
       E₁  4                                +                R
     0                                   E                   G 1
       E   1                                +                S 3
              +  3                            Int            S 7
                   Int 7                          *          R
                 3                            E₂             G 6
                   E₂  6                          *          R
                 3                            E₁             G 9
                   E₁  9                          *          S 5
                         *  5                       Int      S 7
                              Int 7                     $    R
                            5                       E₂       G 8
                              E₂  8                     $    R
                 3                                  E₁       G 9
                 3 E₁ 9                                 $    R
     0                                              E        G 1
     0  E  1                                            $    S 2
              $  2                                           R, accept
