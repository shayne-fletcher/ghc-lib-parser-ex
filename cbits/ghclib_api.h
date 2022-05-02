/*
Copyright (c) 2020, 2021 Shayne Fletcher. All rights reserved.
SPDX-License-Identifier: BSD-3-Clause.
 */

#if !defined(GHCLIB_API_H)
#  define GHCLIB_API_H

#  if !(defined (GHCLIB_API_HEAD)   \
     || defined (GHCLIB_API_904)    \
     || defined (GHCLIB_API_902)    \
     || defined (GHCLIB_API_900)    \
     || defined (GHCLIB_API_810)    \
     || defined (GHCLIB_API_808))
#    if defined(MIN_VERSION_ghc_lib_parser)
#       if !MIN_VERSION_ghc_lib_parser( 1,  0,  0)
#         define GHCLIB_API_HEAD
#       elif MIN_VERSION_ghc_lib_parser(9,  4,  0)
#         define GHCLIB_API_904
#       elif MIN_VERSION_ghc_lib_parser(9,  2,  0)
#         define GHCLIB_API_902
#       elif MIN_VERSION_ghc_lib_parser(9,  0,  0)
#         define GHCLIB_API_900
#       elif MIN_VERSION_ghc_lib_parser(8, 10,  0)
#         define GHCLIB_API_810
#       elif MIN_VERSION_ghc_lib_parser(8,  8,  0)
#         define GHCLIB_API_808
#       else
#         error Unsupported GHC API version
#      endif
#    else
#      if __GLASGOW_HASKELL__   == 905
#        define GHCLIB_API_HEAD
#      elif __GLASGOW_HASKELL__ == 904
#        define GHCLIB_API_904
#      elif __GLASGOW_HASKELL__ == 902
#        define GHCLIB_API_902
#      elif __GLASGOW_HASKELL__ == 900
#        define GHCLIB_API_900
#      elif __GLASGOW_HASKELL__ == 810
#        define GHCLIB_API_810
#      elif __GLASGOW_HASKELL__ == 808
#        define GHCLIB_API_808
#      else
#        error Unsupported GHC API version
#      endif
#    endif
#  endif
#endif
