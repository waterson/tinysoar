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
 * Copyright (C) 2000, 2001, 2002 Christopher R. Waterson. All Rights
 * Reserved.
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

#ifndef alloc_h__
#define alloc_h__

#include "config.h"

/* Just use the libc allocator if we've got it. Otherwise, we'll
   assume built-in implementations. */
#if defined(STDC_HEADERS)
#  include <stdlib.h>
#elif defined(HAVE_MALLOC_H)
#  include <malloc.h>
#else
extern void
heap_init(char *addrs[], int naddrs);

extern void *
malloc(unsigned size);

extern void
free(void *ptr);

#  if defined(DEBUG) && defined(HAVE_PRINTF)
extern void
heap_walk();
#  endif
#endif

/* Just use the libc memcpy, etc. if we've got it. Otherwise, we'll
   assume built-in implementations. */
#ifdef HAVE_STRING_H
#  include <string.h>
#else
extern void *
memset(void *s, int c, unsigned n);

extern void *
memcpy(void *dest, const void *src, unsigned n);
#endif

#endif /* alloc_h__ */
