//===-- Definition of VISIT type ------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIBC_TYPES_VISIT_H
#define LLVM_LIBC_TYPES_VISIT_H

typedef enum { preorder, postorder, endorder, leaf } VISIT;

#endif // LLVM_LIBC_TYPES_VISIT_H
