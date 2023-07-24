/*
Copyright (c) 2020, 2021 Shayne Fletcher. All rights reserved.
SPDX-License-Identifier: BSD-3-Clause.
 */

#if !defined (GHCLIB_API_H)
#  define GHCLIB_API_H

#  if !(defined (GHC_9_10)   \
     || defined (GHC_9_8)    \
     || defined (GHC_9_6)    \
     || defined (GHC_9_4)    \
     || defined (GHC_9_2)    \
     || defined (GHC_9_0)    \
     || defined (GHC_8_10)    \
     || defined (GHC_8_8))
#    if defined(MIN_VERSION_ghc_lib_parser)
#       if !MIN_VERSION_ghc_lib_parser( 1,  0,  0)
#         define GHC_9_10
#       elif MIN_VERSION_ghc_lib_parser(9,  8,  0)
#         define GHC_9_8
#       elif MIN_VERSION_ghc_lib_parser(9,  6,  0)
#         define GHC_9_6
#       elif MIN_VERSION_ghc_lib_parser(9,  4,  0)
#         define GHC_9_4
#       elif MIN_VERSION_ghc_lib_parser(9,  2,  0)
#         define GHC_9_2
#       elif MIN_VERSION_ghc_lib_parser(9,  0,  0)
#         define GHC_9_0
#       elif MIN_VERSION_ghc_lib_parser(8, 10,  0)
#         define GHC_8_10
#       elif MIN_VERSION_ghc_lib_parser(8,  8,  0)
#         define GHC_8_8
#       else
#         error Unsupported GHC API version
#      endif
#    else
#      if __GLASGOW_HASKELL__   == 909
#        define GHC_9_9
#      elif __GLASGOW_HASKELL__   == 908
#        define GHC_9_8
#      elif __GLASGOW_HASKELL__ == 906
#        define GHC_9_6
#      elif __GLASGOW_HASKELL__ == 904
#        define GHC_9_4
#      elif __GLASGOW_HASKELL__ == 902
#        define GHC_9_2
#      elif __GLASGOW_HASKELL__ == 900
#        define GHC_9_0
#      elif __GLASGOW_HASKELL__ == 810
#        define GHC_8_10
#      elif __GLASGOW_HASKELL__ == 808
#        define GHC_8_8
#      else
#        error Unsupported GHC API version
#      endif

#    endif /* defined (MIN_VERSION_ghc_lib_parser)  */

#  endif /* !defined ( (GHC_9_10) || defined (GHC_9_8) || ... ) */

#endif /* !defined (GHCLIB_API_H) */
