/*===---- cheerpintrin.h - Cheerp intrinsics -------------------------------===
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 *===-----------------------------------------------------------------------===
 */

#ifndef __CHEERPINTRIN_H
#define __CHEERPINTRIN_H

template<class R>
R* __builtin_cheerp_pointer_base(const void* ptr);
size_t __builtin_cheerp_pointer_offset(const void* ptr);
/* This method returns a closure. When it is invoked it will execute func with obj as the first argument
   and its own argument as the second one
*/
template<class R,class T,class O>
R* __builtin_cheerp_create_closure(T* func, O* obj);

template<class R,class T>
R* __builtin_cheerp_make_complete_object(T*);

template<class R,class T>
R* __builtin_cheerp_make_regular(T*, int);

#endif /* __CHEERPINTRIN_H */
