/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/*
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "MPL"); you may not use this file except in
 * compliance with the MPL.  You may obtain a copy of the MPL at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the MPL is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the MPL
 * for the specific language governing rights and limitations under the
 * MPL.
 *
 * The Initial Developer of this code under the MPL is Christopher
 * R. Waterson. Portions created by Christopher R. Waterson are
 * Copyright (C) 2000 Christopher R. Waterson. All Rights Reserved.
 *
 * Contributor(s):
 *   Christopher R. Waterson <waterson@maubi.net>
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or 
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 */

/*
 * Configuration options and other general-purpose stuff.
 */

#ifndef config_h__
#define config_h__

#include "defs.h"

/* How many bits per word are there? */
#if SIZEOF_INT == 8
#  define BITS_PER_WORD 64
#elif SIZEOF_INT == 4
#  define BITS_PER_WORD 32
#elif SIZEOF_INT == 2
#  define BITS_PER_WORD 16
#endif

/* Wrappers for macros */
#define BEGIN_MACRO do {
#define END_MACRO   } while (0)

/* A type for booleans */
typedef unsigned char bool_t;

/* More verbose equivalents of `assert()' */
#ifdef DEBUG
   extern void
   runtime_assert(const char *fmtstr, ...);

   extern void
   runtime_warn(const char *fmtstr, ...);

#  define WARN(args)          runtime_warn args
#  define WARN_IF(cond, args) ((cond) ? (void)0 : runtime_warn args)
#  define ASSERT(cond, args)  ((cond) ? (void)0 : runtime_assert args)
#  define ERROR(args)         ASSERT(0, args)
#  define UNIMPLEMENTED()     ASSERT(0, ("unimplemented"))
#  define UNREACHABLE()       ASSERT(0, ("unreachable"))

#  ifdef _WIN32
#    include <crtdbg.h>
#    define VERIFY_HEAP() ASSERT(_CrtCheckMemory(), ("heap corruption"))
#  endif

#else
#  define WARN(args)          ((void)0)
#  define WARN_IF(cond, args) ((void)0)
#  define ASSERT(cond, args)  ((void)0)
#  define ERROR(args)         ((void)0)
#  define UNIMPLEMENTED()     ((void)0)
#  define UNREACHABLE()       ((void)0)
#endif

#ifndef VERIFY_HEAP
#define VERIFY_HEAP()
#endif

#endif /* config_h__ */
