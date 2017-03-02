// Developed as part of the emscripten project. License: MIT&LLVM

#include "Relooper.h"

#include <string.h>
#include <stdlib.h>
#include <list>
#include <stack>

template <class T, class U> static bool contains(const T& container, const U& contained) {
  return container.count(contained);
}

#if DEBUG
static void PrintDebug(const char *Format, ...);
#define DebugDump(x, ...) Debugging::Dump(x, __VA_ARGS__)
#else
#define PrintDebug(x, ...)
#define DebugDump(x, ...)
#endif

// Branch

Branch::Branch(int bId) : Ancestor(NULL), Labeled(true), branchId(bId) {
}

Branch::~Branch() {
}

void Branch::Render(Block *Target, bool SetLabel, RenderInterface* renderInterface) {
  if (SetLabel) renderInterface->renderLabel(Target->Id);
  if (Ancestor) {
    if (Type == Break || Type == Continue) {
      if (Labeled) {
        if(Type == Break)
          renderInterface->renderBreak(Ancestor->Id);
        else
          renderInterface->renderContinue(Ancestor->Id);
      } else {
        if(Type == Break)
          renderInterface->renderBreak();
        else
          renderInterface->renderContinue();
      }
    }
  }
}

// Block

Block::Block(const void* b, bool s, int Id, const void* bv) : Parent(NULL), Id(Id), privateBlock(b),
  privateBranchVar(bv), DefaultTarget(NULL), IsCheckedMultipleEntry(false), IsSplittable(s) {
}

Block::~Block() {
  for (BlockSet::iterator iter = ProcessedBranchesIn.begin(); iter != ProcessedBranchesIn.end(); iter++) {
    delete *iter;
  }
  for (BlockBranchMap::iterator iter = ProcessedBranchesOut.begin(); iter != ProcessedBranchesOut.end(); iter++) {
    delete iter->second;
  }
}

bool Block::AddBranchTo(Block *Target, int branchId) {
  if(contains(BranchesOut, Target)) // cannot add more than one branch to the same target
    return false;
  BranchesOut[Target] = new Branch(branchId);
  return true;
}

void Block::Render(bool InLoop, RenderInterface* renderInterface) {
  if (IsCheckedMultipleEntry && InLoop) {
    renderInterface->renderLabel(0);
  }

  renderInterface->renderBlock(privateBlock);

  if (!ProcessedBranchesOut.size()) return;

  bool SetLabel = true; // in some cases it is clear we can avoid setting label, see later

  // A setting of the label variable (label = x) is necessary if it can
  // cause an impact. The main case is where we set label to x, then elsewhere
  // we check if label is equal to that value, i.e., that label is an entry
  // in a multiple block. We also need to reset the label when we enter
  // that block, so that each setting is a one-time action: consider
  //
  //    while (1) {
  //      if (check) label = 1;
  //      if (label == 1) { label = 0 }
  //    }
  //
  // (Note that this case is impossible due to fusing, but that is not
  // material here.) So setting to 0 is important just to clear the 1 for
  // future iterations.
  // TODO: When inside a loop, if necessary clear the label variable
  //       once on the top, and never do settings that are in effect clears

  // Fusing: If the next is a Multiple, we can fuse it with this block. Note
  // that we must be the Inner of a Simple, so fusing means joining a Simple
  // to a Multiple. What happens there is that all options in the Multiple
  // *must* appear in the Simple (the Simple is the only one reaching the
  // Multiple), so we can remove the Multiple and add its independent groups
  // into the Simple's branches.
  MultipleShape *Fused = Shape::IsMultiple(Parent->Next);
  if (Fused) {
    PrintDebug("Fusing Multiple to Simple\n",);
    Parent->Next = Parent->Next->Next;
    Fused->UseSwitch = false; // TODO: emit switches here
    Fused->RenderLoopPrefix(renderInterface);

    // When the Multiple has the same number of groups as we have branches,
    // they will all be fused, so it is safe to not set the label at all
    if (SetLabel && Fused->InnerMap.size() == ProcessedBranchesOut.size()) {
      SetLabel = false;
    }
  }

  // Find the default target, the one without a condition
  for (BlockBranchMap::iterator iter = ProcessedBranchesOut.begin(); iter != ProcessedBranchesOut.end(); iter++) {
    if (iter->second->branchId == -1) {
      assert(!DefaultTarget); // Must be exactly one default
      DefaultTarget = iter->first;
    }
  }
  assert(DefaultTarget); // Since each block *must* branch somewhere, this must be set

  bool useSwitch = privateBranchVar != NULL;

  if (useSwitch) {
    renderInterface->renderSwitchBlockBegin(privateBranchVar);
  }

  std::vector<int> emptyBranchesIds;
  bool First = !useSwitch; // when using a switch, there is no special first
  for (BlockBranchMap::iterator iter = ProcessedBranchesOut.begin();; iter++) {
    Block *Target;
    Branch *Details;
    if (iter != ProcessedBranchesOut.end()) {
      Target = iter->first;
      if (Target == DefaultTarget) continue; // done at the end
      Details = iter->second;
      assert(Details->branchId != -1); // must have a condition if this is not the default target
    } else {
      Target = DefaultTarget;
      Details = ProcessedBranchesOut[DefaultTarget];
    }
    bool SetCurrLabel = SetLabel && Target->IsCheckedMultipleEntry;
    bool HasFusedContent = Fused && contains(Fused->InnerMap, Target->Id);
    //Cheerp: We assume that the block has content, otherwise why it's even here?
    bool HasContent = SetCurrLabel || Details->Type != Branch::Direct ||
                      HasFusedContent || renderInterface->hasBlockPrologue(Target->privateBlock, privateBlock);
    if (iter != ProcessedBranchesOut.end()) {
      // If there is nothing to show in this branch, omit the condition
      if (useSwitch) {
        renderInterface->renderCaseBlockBegin(privateBlock, Details->branchId);
      } else {
        if (HasContent) {
          renderInterface->renderIfBlockBegin(privateBlock, Details->branchId, First);
          First = false;
        } else {
          emptyBranchesIds.push_back(Details->branchId);
        }
      }
    } else {
      // this is the default
      if (useSwitch) {
        renderInterface->renderDefaultBlockBegin();
      } else {
        if (HasContent) {
          if (!emptyBranchesIds.empty()) {
            renderInterface->renderIfBlockBegin(privateBlock, emptyBranchesIds, First);
            First = false;
          } else if (!First) {
            renderInterface->renderElseBlockBegin();
          }
        }
  }
    }
    renderInterface->renderBlockPrologue(Target->privateBlock, privateBlock);
    Details->Render(Target, SetCurrLabel, renderInterface);
    if (HasFusedContent) {
      Fused->InnerMap.find(Target->Id)->second->Render(InLoop, renderInterface);
    } else if (Details->Type == Branch::Nested) {
      // Nest the parent content here, and remove it from showing up afterwards as Next
      assert(Parent->Next);
      Parent->Next->Render(InLoop, renderInterface);
      Parent->Next = NULL;
    }
    if (useSwitch && iter != ProcessedBranchesOut.end()) {
      renderInterface->renderBreak();
    }
    if (useSwitch) {
      renderInterface->renderBlockEnd();
    }
    if (iter == ProcessedBranchesOut.end()) break;
  }
  if (!First) renderInterface->renderBlockEnd();

  if (Fused) {
    Fused->RenderLoopPostfix(renderInterface);
  }
}

// MultipleShape

void MultipleShape::RenderLoopPrefix(RenderInterface* renderInterface) {
  if (Breaks) {
    if (UseSwitch) {
      if (Labeled) {
        renderInterface->renderLabelForSwitch(Id);
      }
    } else {
      if (Labeled) {
        renderInterface->renderDoBlockBegin(Id);
      } else {
        renderInterface->renderDoBlockBegin();
      }
    }
  }
}

void MultipleShape::RenderLoopPostfix(RenderInterface* renderInterface) {
  if (Breaks && !UseSwitch) {
    renderInterface->renderDoBlockEnd();
  }
}

void MultipleShape::Render(bool InLoop, RenderInterface* renderInterface) {
  RenderLoopPrefix(renderInterface);

  if (!UseSwitch) {
    // emit an if-else chain
    bool First = true;
    for (IdShapeMap::iterator iter = InnerMap.begin(); iter != InnerMap.end(); iter++) {
      renderInterface->renderIfOnLabel(iter->first, First);
      First = false;
      iter->second->Render(InLoop, renderInterface);
      renderInterface->renderBlockEnd();
    }
  } else {
    // emit a switch
    renderInterface->renderSwitchOnLabel();
    for (IdShapeMap::iterator iter = InnerMap.begin(); iter != InnerMap.end(); iter++) {
      renderInterface->renderCaseOnLabel(iter->first);
      iter->second->Render(InLoop, renderInterface);
      renderInterface->renderBreak();
      renderInterface->renderBlockEnd();
    }
    renderInterface->renderBlockEnd();
  }

  RenderLoopPostfix(renderInterface);
  if (Next) Next->Render(InLoop, renderInterface);
}

// LoopShape

void LoopShape::Render(bool InLoop, RenderInterface* renderInterface) {
  if (Labeled) {
    renderInterface->renderWhileBlockBegin(Id);
  } else {
    renderInterface->renderWhileBlockBegin();
  }
  Inner->Render(true, renderInterface);
  renderInterface->renderBlockEnd();
  if (Next) Next->Render(InLoop, renderInterface);
}


// Relooper

Relooper::Relooper(int BlockCount) : Root(NULL), MinSize(false), NeedsLabel(false), IdCounter(BlockCount) {
}

Relooper::~Relooper() {
  for (unsigned i = 0; i < Blocks.size(); i++) delete Blocks[i];
  for (unsigned i = 0; i < Shapes.size(); i++) delete Shapes[i];
}

void Relooper::AddBlock(Block *New) {
  Blocks.push_back(New);
}

struct RelooperRecursor {
  Relooper *Parent;
  RelooperRecursor(Relooper *ParentInit) : Parent(ParentInit) {}
};

typedef std::list<Block*> BlockList;

void Relooper::Calculate(Block *Entry) {
  // Scan and optimize the input
  struct PreOptimizer : public RelooperRecursor {
    PreOptimizer(Relooper *Parent) : RelooperRecursor(Parent) {}
    BlockSet Live;

    void FindLive(Block *Root) {
      BlockList ToInvestigate;
      ToInvestigate.push_back(Root);
      while (ToInvestigate.size() > 0) {
        Block *Curr = ToInvestigate.front();
        ToInvestigate.pop_front();
        if (contains(Live, Curr)) continue;
        Live.insert(Curr);
        for (BlockBranchMap::iterator iter = Curr->BranchesOut.begin(); iter != Curr->BranchesOut.end(); iter++) {
          ToInvestigate.push_back(iter->first);
        }
      }
    }

    // If a block has multiple entries but no exits, and it is small enough, it is useful to split it.
    // A common example is a C++ function where everything ends up at a final exit block and does some
    // RAII cleanup. Without splitting, we will be forced to introduce labelled loops to allow
    // reaching the final block
    void SplitDeadEnds() {
      BlockSet Splits;
      BlockSet Removed;
      for (BlockSet::iterator iter = Live.begin(); iter != Live.end(); iter++) {
        Block *Original = *iter;
        if (Original->BranchesIn.size() <= 1 || Original->BranchesOut.size() > 0) continue; // only dead ends, for now
        if (contains(Original->BranchesOut, Original)) continue; // cannot split a looping node // TODO: is this necessary?
        if (!Original->IsSplittable) continue;
        // Split the node (for simplicity, we replace all the blocks, even though we could have reused the original)
        for (BlockSet::iterator iter = Original->BranchesIn.begin(); iter != Original->BranchesIn.end(); iter++) {
          Block *Prior = *iter;
          Block *Split = new Block(Original->privateBlock, Original->IsSplittable, Parent->IdCounter++, Original->privateBranchVar);
          Parent->AddBlock(Split);
          Split->BranchesIn.insert(Prior);
          Branch *Details = Prior->BranchesOut[Original];
          Prior->BranchesOut[Split] = new Branch(Details->branchId);
          delete Details; // TODO: is this ok?
          Prior->BranchesOut.erase(Original);
          for (BlockBranchMap::iterator iter = Original->BranchesOut.begin(); iter != Original->BranchesOut.end(); iter++) {
            Block *Post = iter->first;
            Branch *Details = iter->second;
            Split->BranchesOut[Post] = new Branch(Details->branchId);
            Post->BranchesIn.insert(Split);
          }
          Splits.insert(Split);
          Removed.insert(Original);
        }
        for (BlockBranchMap::iterator iter = Original->BranchesOut.begin(); iter != Original->BranchesOut.end(); iter++) {
          Block *Post = iter->first;
          Post->BranchesIn.erase(Original);
        }
      }
      for (BlockSet::iterator iter = Splits.begin(); iter != Splits.end(); iter++) {
        Live.insert(*iter);
      }
      for (BlockSet::iterator iter = Removed.begin(); iter != Removed.end(); iter++) {
        Live.erase(*iter);
      }
    }
  };
  PreOptimizer Pre(this);
  Pre.FindLive(Entry);

  // Add incoming branches from live blocks, ignoring dead code
  for (unsigned i = 0; i < Blocks.size(); i++) {
    Block *Curr = Blocks[i];
    if (!contains(Pre.Live, Curr)) continue;
    for (BlockBranchMap::iterator iter = Curr->BranchesOut.begin(); iter != Curr->BranchesOut.end(); iter++) {
      iter->first->BranchesIn.insert(Curr);
    }
  }

  if (!MinSize) Pre.SplitDeadEnds();

  // Recursively process the graph

  struct Analyzer : public RelooperRecursor {
    Analyzer(Relooper *Parent) : RelooperRecursor(Parent) {}

    // Add a shape to the list of shapes in this Relooper calculation
    void Notice(Shape *New) {
      Parent->Shapes.push_back(New);
    }

    // Create a list of entries from a block. If LimitTo is provided, only results in that set
    // will appear
    void GetBlocksOut(Block *Source, BlockSet& Entries, BlockSet *LimitTo=NULL) {
      for (BlockBranchMap::iterator iter = Source->BranchesOut.begin(); iter != Source->BranchesOut.end(); iter++) {
        if (!LimitTo || contains(*LimitTo, iter->first)) {
          Entries.insert(iter->first);
        }
      }
    }

    // Converts/processes all branchings to a specific target
    void Solipsize(Block *Target, Branch::FlowType Type, Shape *Ancestor, BlockSet &From) {
      PrintDebug("Solipsizing branches into %d\n", Target->Id);
      DebugDump(From, "  relevant to solipsize: ");
      for (BlockSet::iterator iter = Target->BranchesIn.begin(); iter != Target->BranchesIn.end();) {
        Block *Prior = *iter;
        if (!contains(From, Prior)) {
          iter++;
          continue;
        }
        Branch *PriorOut = Prior->BranchesOut[Target];
        PriorOut->Ancestor = Ancestor;
        PriorOut->Type = Type;
        if (MultipleShape *Multiple = Shape::IsMultiple(Ancestor)) {
          Multiple->Breaks++; // We are breaking out of this Multiple, so need a loop
        }
        iter++; // carefully increment iter before erasing
        Target->BranchesIn.erase(Prior);
        Target->ProcessedBranchesIn.insert(Prior);
        Prior->BranchesOut.erase(Target);
        Prior->ProcessedBranchesOut[Target] = PriorOut;
        PrintDebug("  eliminated branch from %d\n", Prior->Id);
      }
    }

    Shape *MakeSimple(BlockSet &Blocks, Block *Inner, BlockSet &NextEntries) {
      PrintDebug("creating simple block with block #%d\n", Inner->Id);
      SimpleShape *Simple = new SimpleShape(Parent->IdCounter++);
      Notice(Simple);
      Simple->Inner = Inner;
      Inner->Parent = Simple;
      if (Blocks.size() > 1) {
        Blocks.erase(Inner);
        GetBlocksOut(Inner, NextEntries, &Blocks);
        BlockSet JustInner;
        JustInner.insert(Inner);
        for (BlockSet::iterator iter = NextEntries.begin(); iter != NextEntries.end(); iter++) {
          Solipsize(*iter, Branch::Direct, Simple, JustInner);
        }
      }
      return Simple;
    }

    Shape *MakeLoop(BlockSet &Blocks, BlockSet& Entries, BlockSet &NextEntries) {
      // Find the inner blocks in this loop. Proceed backwards from the entries until
      // you reach a seen block, collecting as you go.
      BlockSet InnerBlocks;
      BlockSet Queue = Entries;
      while (Queue.size() > 0) {
        Block *Curr = *(Queue.begin());
        Queue.erase(Queue.begin());
        if (!contains(InnerBlocks, Curr)) {
          // This element is new, mark it as inner and remove from outer
          InnerBlocks.insert(Curr);
          Blocks.erase(Curr);
          // Add the elements prior to it
          for (BlockSet::iterator iter = Curr->BranchesIn.begin(); iter != Curr->BranchesIn.end(); iter++) {
            Queue.insert(*iter);
          }
#if 0
          // Add elements it leads to, if they are dead ends. There is no reason not to hoist dead ends
          // into loops, as it can avoid multiple entries after the loop
          for (BlockBranchMap::iterator iter = Curr->BranchesOut.begin(); iter != Curr->BranchesOut.end(); iter++) {
            Block *Target = iter->first;
            if (Target->BranchesIn.size() <= 1 && Target->BranchesOut.size() == 0) {
              Queue.insert(Target);
            }
          }
#endif
        }
      }
      assert(InnerBlocks.size() > 0);

      for (BlockSet::iterator iter = InnerBlocks.begin(); iter != InnerBlocks.end(); iter++) {
        Block *Curr = *iter;
        for (BlockBranchMap::iterator iter = Curr->BranchesOut.begin(); iter != Curr->BranchesOut.end(); iter++) {
          Block *Possible = iter->first;
          if (!contains(InnerBlocks, Possible)) {
            NextEntries.insert(Possible);
          }
        }
      }

      PrintDebug("creating loop block:\n",);
      DebugDump(InnerBlocks, "  inner blocks:");
      DebugDump(Entries, "  inner entries:");
      DebugDump(Blocks, "  outer blocks:");
      DebugDump(NextEntries, "  outer entries:");

      // TODO: Optionally hoist additional blocks into the loop

      LoopShape *Loop = new LoopShape(Parent->IdCounter++);
      Notice(Loop);

      // Solipsize the loop, replacing with break/continue and marking branches as Processed (will not affect later calculations)
      // A. Branches to the loop entries become a continue to this shape
      for (BlockSet::iterator iter = Entries.begin(); iter != Entries.end(); iter++) {
        Solipsize(*iter, Branch::Continue, Loop, InnerBlocks);
      }
      // B. Branches to outside the loop (a next entry) become breaks on this shape
      for (BlockSet::iterator iter = NextEntries.begin(); iter != NextEntries.end(); iter++) {
        Solipsize(*iter, Branch::Break, Loop, InnerBlocks);
      }
      // Finish up
      Shape *Inner = Process(InnerBlocks, Entries, NULL);
      Loop->Inner = Inner;
      return Loop;
    }

    // For each entry, find the independent group reachable by it. The independent group is
    // the entry itself, plus all the blocks it can reach that cannot be directly reached by another entry. Note that we
    // ignore directly reaching the entry itself by another entry.
    //   @param Ignore - previous blocks that are irrelevant
    void FindIndependentGroups(BlockSet &Entries, BlockBlockSetMap& IndependentGroups, BlockSet *Ignore=NULL) {
      typedef std::map<Block*, Block*> BlockBlockMap;

      struct HelperClass {
        BlockBlockSetMap& IndependentGroups;
        BlockBlockMap Ownership; // For each block, which entry it belongs to. We have reached it from there.

        HelperClass(BlockBlockSetMap& IndependentGroupsInit) : IndependentGroups(IndependentGroupsInit) {}
        void InvalidateWithChildren(Block *New) { // TODO: rename New
          BlockList ToInvalidate; // Being in the list means you need to be invalidated
          ToInvalidate.push_back(New);
          while (ToInvalidate.size() > 0) {
            Block *Invalidatee = ToInvalidate.front();
            ToInvalidate.pop_front();
            Block *Owner = Ownership[Invalidatee];
            if (contains(IndependentGroups, Owner)) { // Owner may have been invalidated, do not add to IndependentGroups!
              IndependentGroups[Owner].erase(Invalidatee);
            }
            if (Ownership[Invalidatee]) { // may have been seen before and invalidated already
              Ownership[Invalidatee] = NULL;
              for (BlockBranchMap::iterator iter = Invalidatee->BranchesOut.begin(); iter != Invalidatee->BranchesOut.end(); iter++) {
                Block *Target = iter->first;
                BlockBlockMap::iterator Known = Ownership.find(Target);
                if (Known != Ownership.end()) {
                  Block *TargetOwner = Known->second;
                  if (TargetOwner) {
                    ToInvalidate.push_back(Target);
                  }
                }
              }
            }
          }
        }
      };
      HelperClass Helper(IndependentGroups);

      // We flow out from each of the entries, simultaneously.
      // When we reach a new block, we add it as belonging to the one we got to it from.
      // If we reach a new block that is already marked as belonging to someone, it is reachable by
      // two entries and is not valid for any of them. Remove it and all it can reach that have been
      // visited.

      BlockList Queue; // Being in the queue means we just added this item, and we need to add its children
      for (BlockSet::iterator iter = Entries.begin(); iter != Entries.end(); iter++) {
        Block *Entry = *iter;
        Helper.Ownership[Entry] = Entry;
        IndependentGroups[Entry].insert(Entry);
        Queue.push_back(Entry);
      }
      while (Queue.size() > 0) {
        Block *Curr = Queue.front();
        Queue.pop_front();
        Block *Owner = Helper.Ownership[Curr]; // Curr must be in the ownership map if we are in the queue
        if (!Owner) continue; // we have been invalidated meanwhile after being reached from two entries
        // Add all children
        for (BlockBranchMap::iterator iter = Curr->BranchesOut.begin(); iter != Curr->BranchesOut.end(); iter++) {
          Block *New = iter->first;
          BlockBlockMap::iterator Known = Helper.Ownership.find(New);
          if (Known == Helper.Ownership.end()) {
            // New node. Add it, and put it in the queue
            Helper.Ownership[New] = Owner;
            IndependentGroups[Owner].insert(New);
            Queue.push_back(New);
            continue;
          }
          Block *NewOwner = Known->second;
          if (!NewOwner) continue; // We reached an invalidated node
          if (NewOwner != Owner) {
            // Invalidate this and all reachable that we have seen - we reached this from two locations
            Helper.InvalidateWithChildren(New);
          }
          // otherwise, we have the same owner, so do nothing
        }
      }

      // Having processed all the interesting blocks, we remain with just one potential issue:
      // If a->b, and a was invalidated, but then b was later reached by someone else, we must
      // invalidate b. To check for this, we go over all elements in the independent groups,
      // if an element has a parent which does *not* have the same owner, we must remove it
      // and all its children.

      for (BlockSet::iterator iter = Entries.begin(); iter != Entries.end(); iter++) {
        BlockSet &CurrGroup = IndependentGroups[*iter];
        BlockList ToInvalidate;
        for (BlockSet::iterator iter = CurrGroup.begin(); iter != CurrGroup.end(); iter++) {
          Block *Child = *iter;
          for (BlockSet::iterator iter = Child->BranchesIn.begin(); iter != Child->BranchesIn.end(); iter++) {
            Block *Parent = *iter;
            if (Ignore && contains(*Ignore, Parent)) continue;
            if (Helper.Ownership[Parent] != Helper.Ownership[Child]) {
              ToInvalidate.push_back(Child);
            }
          }
        }
        while (ToInvalidate.size() > 0) {
          Block *Invalidatee = ToInvalidate.front();
          ToInvalidate.pop_front();
          Helper.InvalidateWithChildren(Invalidatee);
        }
      }

      // Remove empty groups
      for (BlockSet::iterator iter = Entries.begin(); iter != Entries.end(); iter++) {
        if (IndependentGroups[*iter].size() == 0) {
          IndependentGroups.erase(*iter);
        }
      }

#if DEBUG
      PrintDebug("Investigated independent groups:\n",);
      for (BlockBlockSetMap::iterator iter = IndependentGroups.begin(); iter != IndependentGroups.end(); iter++) {
        DebugDump(iter->second, " group: ");
      }
#endif
    }

    Shape *MakeMultiple(BlockSet &Blocks, BlockSet& Entries, BlockBlockSetMap& IndependentGroups, Shape *Prev, BlockSet &NextEntries) {
      PrintDebug("creating multiple block with %d inner groups\n", IndependentGroups.size());
      bool Fused = !!(Shape::IsSimple(Prev));
      MultipleShape *Multiple = new MultipleShape(Parent->IdCounter++);
      Notice(Multiple);
      BlockSet CurrEntries;
      for (BlockBlockSetMap::iterator iter = IndependentGroups.begin(); iter != IndependentGroups.end(); iter++) {
        Block *CurrEntry = iter->first;
        BlockSet &CurrBlocks = iter->second;
        PrintDebug("  multiple group with entry %d:\n", CurrEntry->Id);
        DebugDump(CurrBlocks, "    ");
        // Create inner block
        CurrEntries.clear();
        CurrEntries.insert(CurrEntry);
        for (BlockSet::iterator iter = CurrBlocks.begin(); iter != CurrBlocks.end(); iter++) {
          Block *CurrInner = *iter;
          // Remove the block from the remaining blocks
          Blocks.erase(CurrInner);
          // Find new next entries and fix branches to them
          for (BlockBranchMap::iterator iter = CurrInner->BranchesOut.begin(); iter != CurrInner->BranchesOut.end();) {
            Block *CurrTarget = iter->first;
            BlockBranchMap::iterator Next = iter;
            Next++;
            if (!contains(CurrBlocks, CurrTarget)) {
              NextEntries.insert(CurrTarget);
              Solipsize(CurrTarget, Branch::Break, Multiple, CurrBlocks); 
            }
            iter = Next; // increment carefully because Solipsize can remove us
          }
        }
        Multiple->InnerMap[CurrEntry->Id] = Process(CurrBlocks, CurrEntries, NULL);
        // If we are not fused, then our entries will actually be checked
        if (!Fused) {
          Parent->NeedsLabel = true;
          CurrEntry->IsCheckedMultipleEntry = true;
        }
      }
      DebugDump(Blocks, "  remaining blocks after multiple:");
      // Add entries not handled as next entries, they are deferred
      for (BlockSet::iterator iter = Entries.begin(); iter != Entries.end(); iter++) {
        Block *Entry = *iter;
        if (!contains(IndependentGroups, Entry)) {
          NextEntries.insert(Entry);
        }
      }
      // The multiple has been created, we can decide how to implement it
      if (Multiple->InnerMap.size() >= 10) {
        Multiple->UseSwitch = true;
        Multiple->Breaks++; // switch captures breaks
      }
      return Multiple;
    }

    // Main function.
    // Process a set of blocks with specified entries, returns a shape
    // The Make* functions receive a NextEntries. If they fill it with data, those are the entries for the
    //   ->Next block on them, and the blocks are what remains in Blocks (which Make* modify). In this way
    //   we avoid recursing on Next (imagine a long chain of Simples, if we recursed we could blow the stack).
    Shape *Process(BlockSet &Blocks, BlockSet& InitialEntries, Shape *Prev) {
      PrintDebug("Process() called\n",);
      BlockSet *Entries = &InitialEntries;
      BlockSet TempEntries[2];
      int CurrTempIndex = 0;
      BlockSet *NextEntries;
      Shape *Ret = NULL;
      #define Make(call) \
        Shape *Temp = call; \
        if (Prev) Prev->Next = Temp; \
        if (!Ret) Ret = Temp; \
        if (!NextEntries->size()) { PrintDebug("Process() returning\n",); return Ret; } \
        Prev = Temp; \
        Entries = NextEntries; \
        continue;
      while (1) {
        PrintDebug("Process() running\n",);
        DebugDump(Blocks, "  blocks : ");
        DebugDump(*Entries, "  entries: ");

        CurrTempIndex = 1-CurrTempIndex;
        NextEntries = &TempEntries[CurrTempIndex];
        NextEntries->clear();

        if (Entries->size() == 0) return Ret;
        if (Entries->size() == 1) {
          Block *Curr = *(Entries->begin());
          if (Curr->BranchesIn.size() == 0) {
            // One entry, no looping ==> Simple
            Make(MakeSimple(Blocks, Curr, *NextEntries));
          }
          // One entry, looping ==> Loop
          Make(MakeLoop(Blocks, *Entries, *NextEntries));
        }

        // More than one entry, try to eliminate through a Multiple groups of
        // independent blocks from an entry/ies. It is important to remove through
        // multiples as opposed to looping since the former is more performant.
        BlockBlockSetMap IndependentGroups;
        FindIndependentGroups(*Entries, IndependentGroups);

        PrintDebug("Independent groups: %d\n", IndependentGroups.size());

        if (IndependentGroups.size() > 0) {
          // We can handle a group in a multiple if its entry cannot be reached by another group.
          // Note that it might be reachable by itself - a loop. But that is fine, we will create
          // a loop inside the multiple block (which is the performant order to do it).
          for (BlockBlockSetMap::iterator iter = IndependentGroups.begin(); iter != IndependentGroups.end();) {
            Block *Entry = iter->first;
            BlockSet &Group = iter->second;
            BlockBlockSetMap::iterator curr = iter++; // iterate carefully, we may delete
            for (BlockSet::iterator iterBranch = Entry->BranchesIn.begin(); iterBranch != Entry->BranchesIn.end(); iterBranch++) {
              Block *Origin = *iterBranch;
              if (!contains(Group, Origin)) {
                // Reached from outside the group, so we cannot handle this
                PrintDebug("Cannot handle group with entry %d because of incoming branch from %d\n", Entry->Id, Origin->Id);
                IndependentGroups.erase(curr);
                break;
              }
            }
          }

          // As an optimization, if we have 2 independent groups, and one is a small dead end, we can handle only that dead end.
          // The other then becomes a Next - without nesting in the code and recursion in the analysis.
          // TODO: if the larger is the only dead end, handle that too
          // TODO: handle >2 groups
          // TODO: handle not just dead ends, but also that do not branch to the NextEntries. However, must be careful
          //       there since we create a Next, and that Next can prevent eliminating a break (since we no longer
          //       naturally reach the same place), which may necessitate a one-time loop, which makes the unnesting
          //       pointless.
          if (IndependentGroups.size() == 2) {
            // Find the smaller one
            BlockBlockSetMap::iterator iter = IndependentGroups.begin();
            Block *SmallEntry = iter->first;
            int SmallSize = iter->second.size();
            iter++;
            Block *LargeEntry = iter->first;
            int LargeSize = iter->second.size();
            if (SmallSize != LargeSize) { // ignore the case where they are identical - keep things symmetrical there
              if (SmallSize > LargeSize) {
                Block *Temp = SmallEntry;
                SmallEntry = LargeEntry;
                LargeEntry = Temp; // Note: we did not flip the Sizes too, they are now invalid. TODO: use the smaller size as a limit?
              }
              // Check if dead end
              bool DeadEnd = true;
              BlockSet &SmallGroup = IndependentGroups[SmallEntry];
              for (BlockSet::iterator iter = SmallGroup.begin(); iter != SmallGroup.end(); iter++) {
                Block *Curr = *iter;
                for (BlockBranchMap::iterator iter = Curr->BranchesOut.begin(); iter != Curr->BranchesOut.end(); iter++) {
                  Block *Target = iter->first;
                  if (!contains(SmallGroup, Target)) {
                    DeadEnd = false;
                    break;
                  }
                }
                if (!DeadEnd) break;
              }
              if (DeadEnd) {
                PrintDebug("Removing nesting by not handling large group because small group is dead end\n",);
                IndependentGroups.erase(LargeEntry);
              }
            }
          }

          PrintDebug("Handleable independent groups: %d\n", IndependentGroups.size());

          if (IndependentGroups.size() > 0) {
            // Some groups removable ==> Multiple
            Make(MakeMultiple(Blocks, *Entries, IndependentGroups, Prev, *NextEntries));
          }
        }
        // No independent groups, must be loopable ==> Loop
        Make(MakeLoop(Blocks, *Entries, *NextEntries));
      }
    }
  };

  // Main

  BlockSet AllBlocks;
  for (BlockSet::iterator iter = Pre.Live.begin(); iter != Pre.Live.end(); iter++) {
    Block *Curr = *iter;
    AllBlocks.insert(Curr);
#if DEBUG
    PrintDebug("Adding block %d (%s)\n", Curr->Id, Curr->Code);
#endif
  }

  BlockSet Entries;
  Entries.insert(Entry);
  Root = Analyzer(this).Process(AllBlocks, Entries, NULL);
  assert(Root);

  // Post optimizations

  struct PostOptimizer {
    Relooper *Parent;
    std::stack<Shape*> *Closure;

    PostOptimizer(Relooper *ParentInit) : Parent(ParentInit), Closure(NULL) {}

    #define RECURSE_Multiple(shape, func) \
      for (IdShapeMap::iterator iter = shape->InnerMap.begin(); iter != shape->InnerMap.end(); iter++) { \
        func(iter->second); \
      }
    #define RECURSE_Loop(shape, func) \
      func(shape->Inner);
    #define RECURSE(shape, func) RECURSE_##shape(shape, func);

    #define SHAPE_SWITCH(var, simple, multiple, loop) \
      if (SimpleShape *Simple = Shape::IsSimple(var)) { \
        (void)Simple; \
        simple; \
      } else if (MultipleShape *Multiple = Shape::IsMultiple(var)) { \
        (void)Multiple; \
        multiple; \
      } else if (LoopShape *Loop = Shape::IsLoop(var)) { \
        (void)Loop; \
        loop; \
      }

    // Find the blocks that natural control flow can get us directly to, or through a multiple that we ignore
    void FollowNaturalFlow(Shape *S, BlockSet &Out) {
      SHAPE_SWITCH(S, {
        Out.insert(Simple->Inner);
      }, {
        for (IdShapeMap::iterator iter = Multiple->InnerMap.begin(); iter != Multiple->InnerMap.end(); iter++) {
          FollowNaturalFlow(iter->second, Out);
        }
        FollowNaturalFlow(Multiple->Next, Out);
      }, {
        FollowNaturalFlow(Loop->Inner, Out);
      });
    }

    void FindNaturals(Shape *Root, Shape *Otherwise=NULL) {
      if (Root->Next) {
        Root->Natural = Root->Next;
        FindNaturals(Root->Next, Otherwise);
      } else {
        Root->Natural = Otherwise;
      }

      SHAPE_SWITCH(Root, {
      }, {
        for (IdShapeMap::iterator iter = Multiple->InnerMap.begin(); iter != Multiple->InnerMap.end(); iter++) {
          FindNaturals(iter->second, Root->Natural);
        }
      }, {
        FindNaturals(Loop->Inner, Loop->Inner);
      });
    }

    // Remove unneeded breaks and continues.
    // A flow operation is trivially unneeded if the shape we naturally get to by normal code
    // execution is the same as the flow forces us to.
    void RemoveUnneededFlows(Shape *Root, Shape *Natural=NULL, LoopShape *LastLoop=NULL, unsigned Depth=0) {
      BlockSet NaturalBlocks;
      FollowNaturalFlow(Natural, NaturalBlocks);
      Shape *Next = Root;
      while (Next) {
        Root = Next;
        Next = NULL;
        SHAPE_SWITCH(Root, {
          if (Simple->Inner->privateBranchVar) LastLoop = NULL; // a switch clears out the loop (TODO: only for breaks, not continue)

          if (Simple->Next) {
            if (!Simple->Inner->privateBranchVar && Simple->Inner->ProcessedBranchesOut.size() == 2 && Depth < 20) {
              // If there is a next block, we already know at Simple creation time to make direct branches,
              // and we can do nothing more in general. But, we try to optimize the case of a break and
              // a direct: This would normally be  if (break?) { break; } ..  but if we
              // make sure to nest the else, we can save the break,  if (!break?) { .. }  . This is also
              // better because the more canonical nested form is easier to further optimize later. The
              // downside is more nesting, which adds to size in builds with whitespace.
              // Note that we avoid switches, as it complicates control flow and is not relevant
              // for the common case we optimize here.
              bool Found = false;
              bool Abort = false;
              for (BlockBranchMap::iterator iter = Simple->Inner->ProcessedBranchesOut.begin(); iter != Simple->Inner->ProcessedBranchesOut.end(); iter++) {
                Block *Target = iter->first;
                Branch *Details = iter->second;
                if (Details->Type == Branch::Break) {
                  Found = true;
                  if (!contains(NaturalBlocks, Target)) Abort = true;
                } else if (Details->Type != Branch::Direct) {
                  Abort = true;
                }
              }
              if (Found && !Abort) {
                for (BlockBranchMap::iterator iter = Simple->Inner->ProcessedBranchesOut.begin(); iter != Simple->Inner->ProcessedBranchesOut.end(); iter++) {
                  Branch *Details = iter->second;
                  if (Details->Type == Branch::Break) {
                    Details->Type = Branch::Direct;
                    if (MultipleShape *Multiple = Shape::IsMultiple(Details->Ancestor)) {
                      Multiple->Breaks--;
                    }
                  } else {
                    assert(Details->Type == Branch::Direct);
                    Details->Type = Branch::Nested;
                  }
                }
              }
              Depth++; // this optimization increases depth, for us and all our next chain (i.e., until this call returns)
            }
            Next = Simple->Next;
          } else {
            // If there is no next then Natural is where we will
            // go to by doing nothing, so we can potentially optimize some branches to direct.
            for (BlockBranchMap::iterator iter = Simple->Inner->ProcessedBranchesOut.begin(); iter != Simple->Inner->ProcessedBranchesOut.end(); iter++) {
              Block *Target = iter->first;
              Branch *Details = iter->second;
              if (Details->Type != Branch::Direct && contains(NaturalBlocks, Target)) { // note: cannot handle split blocks
                Details->Type = Branch::Direct;
                if (MultipleShape *Multiple = Shape::IsMultiple(Details->Ancestor)) {
                  Multiple->Breaks--;
                }
              } else if (Details->Type == Branch::Break && LastLoop && LastLoop->Natural == Details->Ancestor->Natural) {
                // it is important to simplify breaks, as simpler breaks enable other optimizations
                Details->Labeled = false;
                if (MultipleShape *Multiple = Shape::IsMultiple(Details->Ancestor)) {
                  Multiple->Breaks--;
                }
              }
            }
          }
        }, {
          for (IdShapeMap::iterator iter = Multiple->InnerMap.begin(); iter != Multiple->InnerMap.end(); iter++) {
            RemoveUnneededFlows(iter->second, Multiple->Next, Multiple->Breaks ? NULL : LastLoop, Depth+1);
          }
          Next = Multiple->Next;
        }, {
          RemoveUnneededFlows(Loop->Inner, Loop->Inner, Loop, Depth+1);
          Next = Loop->Next;
        });
      }
    }

    // After we know which loops exist, we can calculate which need to be labeled
    void FindLabeledLoops(Shape *Root) {
      bool First = Closure == NULL;
      if (First) {
        Closure = new std::stack<Shape*>;
      }
      std::stack<Shape*> &LoopStack = *Closure;

      Shape *Next = Root;
      while (Next) {
        Root = Next;
        Next = NULL;

        SHAPE_SWITCH(Root, {
          MultipleShape *Fused = Shape::IsMultiple(Root->Next);
          // If we are fusing a Multiple with a loop into this Simple, then visit it now
          if (Fused && Fused->Breaks) {
            LoopStack.push(Fused);
          }
          if (Simple->Inner->privateBranchVar) {
            LoopStack.push(NULL); // a switch means breaks are now useless, push a dummy
          }
          if (Fused) {
            if (Fused->UseSwitch) {
              LoopStack.push(NULL); // a switch means breaks are now useless, push a dummy
            }
            RECURSE_Multiple(Fused, FindLabeledLoops);
          }
          for (BlockBranchMap::iterator iter = Simple->Inner->ProcessedBranchesOut.begin(); iter != Simple->Inner->ProcessedBranchesOut.end(); iter++) {
            Branch *Details = iter->second;
            if (Details->Type == Branch::Break || Details->Type == Branch::Continue) {
              assert(LoopStack.size() > 0);
              if (Details->Ancestor != LoopStack.top() && Details->Labeled) {
                LabeledShape *Labeled = Shape::IsLabeled(Details->Ancestor);
                Labeled->Labeled = true;
              } else {
                Details->Labeled = false;
              }
            }
          }
          if (Fused && Fused->UseSwitch) {
            LoopStack.pop();
          }
          if (Simple->Inner->privateBranchVar) {
            LoopStack.pop();
          }
          if (Fused && Fused->Breaks) {
            LoopStack.pop();
          }
          if (Fused) {
            Next = Fused->Next;
          } else {
            Next = Root->Next;
          }
        }, {
          if (Multiple->Breaks) {
            LoopStack.push(Multiple);
          }
          RECURSE(Multiple, FindLabeledLoops);
          if (Multiple->Breaks) {
            LoopStack.pop();
          }
          Next = Root->Next;
        }, {
          LoopStack.push(Loop);
          RECURSE(Loop, FindLabeledLoops);
          LoopStack.pop();
          Next = Root->Next;
        });
      }

      if (First) {
        delete Closure;
      }
    }

    void Process(Shape *Root) {
      FindNaturals(Root);
      RemoveUnneededFlows(Root);
      FindLabeledLoops(Root);
    }
  };

  PrintDebug("=== Optimizing shapes ===\n",);

  PostOptimizer(this).Process(Root);
}

void Relooper::Render(RenderInterface* renderInterface) {
  assert(Root);
  Root->Render(false, renderInterface);
}


#if DEBUG
// Debugging

void Debugging::Dump(BlockSet &Blocks, const char *prefix) {
  if (prefix) printf("%s ", prefix);
  for (BlockSet::iterator iter = Blocks.begin(); iter != Blocks.end(); iter++) {
    Block *Curr = *iter;
    printf("%d:\n", Curr->Id);
    for (BlockBranchMap::iterator iter2 = Curr->BranchesOut.begin(); iter2 != Curr->BranchesOut.end(); iter2++) {
      Block *Other = iter2->first;
      printf("  -> %d\n", Other->Id);
      assert(contains(Other->BranchesIn, Curr));
    }
  }
}

void Debugging::Dump(Shape *S, const char *prefix) {
  if (prefix) printf("%s ", prefix);
  if (!S) {
    printf(" (null)\n");
    return;
  }
  printf(" %d ", S->Id);
  SHAPE_SWITCH(S, {
    printf("<< Simple with block %d\n", Simple->Inner->Id);
  }, {
    printf("<< Multiple\n");
    for (IdShapeMap::iterator iter = Multiple->InnerMap.begin(); iter != Multiple->InnerMap.end(); iter++) {
      printf("     with entry %d\n", iter->first);
    }
  }, {
    printf("<< Loop\n");
  });
}

static void PrintDebug(const char *Format, ...) {
  printf("// ");
  va_list Args;
  va_start(Args, Format);
  vprintf(Format, Args);
  va_end(Args);
}
#endif
