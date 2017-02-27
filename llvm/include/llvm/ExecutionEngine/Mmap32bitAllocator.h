#ifndef LLVM_EXECUTIONENGINE_MMAP32BITALLOCATOR_H
#define LLVM_EXECUTIONENGINE_MMAP32BITALLOCATOR_H

#include "llvm/Support/Allocator.h"
#include "llvm/Support/raw_ostream.h"
#include <sys/mman.h>

namespace llvm {

#define MMAP_SLAB_SIZE 4096

class Mmap32bitAllocator : public AllocatorBase<Mmap32bitAllocator> {
public:
    LLVM_ATTRIBUTE_RETURNS_NONNULL void *Allocate(size_t Size,
            size_t /*Alignment*/) {
        Size = Size + ((MMAP_SLAB_SIZE-1) & (~(MMAP_SLAB_SIZE-1)));
        void *addr = mmap(NULL, Size, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_32BIT | MAP_ANONYMOUS, -1, 0);
        if (addr == MAP_FAILED) {
            llvm::errs() << "Allocate(): size: " << Size << " failed!"
                << " error: " << strerror(errno) << "\n";
            llvm_unreachable("allocation failed");
        }
        return addr;
    }

    // Pull in base class overloads.
    using AllocatorBase<Mmap32bitAllocator>::Allocate;

    void Deallocate(const void *Ptr, size_t Size) {
        Size = Size + ((MMAP_SLAB_SIZE-1) & (~(MMAP_SLAB_SIZE-1)));
        munmap(const_cast<void *>(Ptr), Size);
    }

    // Pull in base class overloads.
    using AllocatorBase<Mmap32bitAllocator>::Deallocate;

    void PrintStats() const {}
};

typedef BumpPtrAllocatorImpl<Mmap32bitAllocator> BumpPtrMmap32bitAllocator;

} // End llvm namespace

#endif
