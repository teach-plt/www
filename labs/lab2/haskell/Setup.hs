-- Programming Language Technology (Chalmers DAT151 / GU DIT231).
-- (C) Andreas Abel 2023-25
-- All rights reserved.

{-# LANGUAGE CPP #-}

-- | Hook BNFC into the cabal build process to generate AST, lexer, parser, and printer definitions.

import Distribution.Simple            (defaultMainWithHooks, simpleUserHooks, buildHook, replHook)
import Distribution.Simple.BuildPaths (autogenPackageModulesDir)
#if MIN_VERSION_Cabal(3,14,0)
import Distribution.Utils.Path        (interpretSymbolicPath)
#endif
import System.Process                 (callProcess)

main :: IO ()
main = do
  defaultMainWithHooks simpleUserHooks
    { buildHook = \ packageDescription localBuildInfo userHooks buildFlags -> do
        -- Call BNFC.
        callBNFC localBuildInfo
        -- Run the build process.
        buildHook simpleUserHooks packageDescription localBuildInfo userHooks buildFlags
    , replHook = \ packageDescription localBuildInfo userHooks buildFlags replArgs -> do
        -- Call BNFC.
        callBNFC localBuildInfo
        -- Run the repl.
        replHook simpleUserHooks packageDescription localBuildInfo userHooks buildFlags replArgs
    }
  where
    callBNFC localBuildInfo = do
      -- For simplicity, generate files in build/global-autogen;
      -- there they are available to all components of the package.
      callProcess "bnfc"
        [ "-o",
#if MIN_VERSION_Cabal(3,14,0)
          interpretSymbolicPath Nothing $
#endif
            autogenPackageModulesDir localBuildInfo
        , "-d"
        , "CMM.cf"
        ]
