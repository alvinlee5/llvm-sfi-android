# llvm-sfi-android
Effectively this is the same as llvm-sfi, but the version of the LLVM source code used here is the same version used by the Android (Studio) Clang compiler. All the changes in llvm-sfi should appear in this repository also, but there may be some changes made to this repository that won't appear in llvm-sfi (since some changes will be specific to the Android NDK).  

For reference, the version of llvm source code used is:
Android (4691093 based on r316199) clang version 6.0.2 (https://android.googlesource.com/toolchain/clang 183abd29fc496f55536e7d904e0abae47888fc7f) (https://android.googlesource.com/toolchain/llvm 34361f192e41ed6e4e8f9aca80a4ea7e9856f327) (based on LLVM 6.0.2svn)
