/*
Copyright (c) 2020, Shayne Fletcher. All rights
reserved. SPDX-License-Identifier: BSD-3-Clause.
 */

#if !defined(GHCLIB_API_H)
#  define GHCLIB_API_H

#if defined(MIN_VERSION_ghc_lib_parser)
# if !MIN_VERSION_ghc_lib_parser(1, 0, 0)
#   define GHCLIB_API_811
# elif MIN_VERSION_ghc_lib_parser(8, 10, 1)
#   define GHCLIBAPI_810
# elif MIN_VERSION_ghc_lib_parser(8, 08, 1)
#   define GHCLIB_API_808
# else
#   error Unsupported GHC API version
# endif
#else
#  if __GLASGOW_HASKELL__ > 811
#   define GHCLIB_API_811
#  elif __GLASGOW_HASKELL__ > 810
#   define GHCLIB_API_810
# elif __GLASGOW_HASKELL__ > 808
#   define GHCLIB_API_808
# else
#   error Unsupported GHC API version
# endif
#endif

#endif
