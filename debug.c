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
 * Debugging support.
 */
#ifdef DEBUG

#include <stdarg.h>
#include <string.h>
#include "config.h"

#include <stdio.h>

/*
 * Alternative to assert() that traps in the debugger with a pretty
 * message.
 */
void
runtime_assert(const char *fmtstr, ...)
{
    va_list ap;

    va_start(ap, fmtstr);
    vfprintf(stderr, fmtstr, ap);
    va_end(ap);

    if (fmtstr[strlen(fmtstr) - 1] != '\n')
        fprintf(stderr, "\n");

#if defined(_MSC_VER) && _M_IX86 >= 300
    __asm { int 3 };
#elif defined(__GNUC__) && defined(__i386)
    asm("int $3");
#endif
}

void
runtime_warn(const char *fmtstr, ...)
{
    va_list ap;

    va_start(ap, fmtstr);
    vfprintf(stdout, fmtstr, ap);
    va_end(ap);

    if (fmtstr[strlen(fmtstr) - 1] != '\n')
        fprintf(stdout, "\n");
}

#endif /* DEBUG */

