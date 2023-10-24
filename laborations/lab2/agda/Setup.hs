-- Programming Language Technology (Chalmers DAT151 / GU DIT231).
-- (C) Andreas Abel 2023
-- All rights reserved.

-- | Hook BNFC into the cabal build process to generate AST, lexer, parser, and printer definitions.

import Distribution.Simple            (defaultMainWithHooks, simpleUserHooks, buildHook)
import Distribution.Simple.BuildPaths (autogenPackageModulesDir)
import System.Process                 (callProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \ packageDescription localBuildInfo userHooks buildFlags -> do
      -- For simplicity, generate files in build/global-autogen;
      -- there they are available to all components of the package.
      let dir = autogenPackageModulesDir localBuildInfo
      callProcess "bnfc"
        [ "-o", dir
        , "-d"
        , "CMM.cf"
        ]
      callProcess "agda"
        [ "--ghc", "--ghc-dont-call-ghc"
        , "src/lab2.agda"
        ]
      callProcess "rm"
        [ "-rf"
        , dir ++ "/MAlonzo"
        ]
      callProcess "mv"
        [ "src/MAlonzo"
        , dir ++ "/"
        ]
      -- Run the build process.
      buildHook simpleUserHooks packageDescription localBuildInfo userHooks buildFlags
  }
