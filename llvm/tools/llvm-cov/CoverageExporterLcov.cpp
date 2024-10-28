//===- CoverageExporterLcov.cpp - Code coverage export --------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file implements export of code coverage data to lcov trace file format.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//
// The trace file code coverage export follows the following format (see also
// https://linux.die.net/man/1/geninfo). Each quoted string appears on its own
// line; the indentation shown here is only for documentation purposes.
//
// - for each source file:
//   - "SF:<absolute path to source file>"
//   - for each function:
//     - "FN:<line number of function start>,<function name>"
//   - for each function:
//     - "FNDA:<execution count>,<function name>"
//   - "FNF:<number of functions found>"
//   - "FNH:<number of functions hit>"
//   - for each instrumented line:
//     - "DA:<line number>,<execution count>[,<checksum>]
//   - for each branch:
//     - "BRDA:<line number>,<branch pair id>,<branch id>,<count>"
//   - "BRF:<number of branches found>"
//   - "BRH:<number of branches hit>"
//   - "LH:<number of lines with non-zero execution count>"
//   - "LF:<number of instrumented lines>"
//   - "end_of_record"
//
// If the user is exporting summary information only, then the FN, FNDA, and DA
// lines will not be present.
//
//===----------------------------------------------------------------------===//

#include "CoverageExporterLcov.h"
#include "CoverageReport.h"
#include "CoverageViewOptions.h"

#define DEBUG_TYPE "coverage-mapping2"
#define LLVM_OUT(x) x
#define LLVM_OUT(x)

using namespace llvm;
using namespace coverage;
using BranchCoverageCollisionMode =
    llvm::CoverageViewOptions::BranchCoverageCollisionMode;

namespace {

struct NestedCountedRegion {
  std::vector<LineColPair> NestedPath;
  llvm::coverage::CountedRegion Region;

  NestedCountedRegion(std::vector<LineColPair> NestedPath,
                      llvm::coverage::CountedRegion Region)
      : NestedPath(std::move(NestedPath)), Region(std::move(Region)) {}
};

struct GroupedCountedRegion {
  uint64_t GroupId;
  uint64_t InstanceId;
  llvm::coverage::CountedRegion Region;
  std::vector<LineColPair> NestedPath;

  GroupedCountedRegion(uint64_t GroupId, uint64_t InstanceId,
                       llvm::coverage::CountedRegion Region,
                       std::vector<LineColPair> NestedPath)
      : GroupId(GroupId), InstanceId(InstanceId), Region(std::move(Region)),
        NestedPath(std::move(NestedPath)) {}
};

void renderFunctionSummary(raw_ostream &OS,
                           const FileCoverageSummary &Summary) {
  OS << "FNF:" << Summary.FunctionCoverage.getNumFunctions() << '\n'
     << "FNH:" << Summary.FunctionCoverage.getExecuted() << '\n';
}

void renderFunctions(
    raw_ostream &OS,
    const iterator_range<coverage::FunctionRecordIterator> &Functions) {
  for (const auto &F : Functions) {
    auto StartLine = F.CountedRegions.front().LineStart;
    OS << "FN:" << StartLine << ',' << F.Name << '\n';
  }
  for (const auto &F : Functions)
    OS << "FNDA:" << F.ExecutionCount << ',' << F.Name << '\n';
}

void renderLineExecutionCounts(raw_ostream &OS,
                               const coverage::CoverageData &FileCoverage) {
  coverage::LineCoverageIterator LCI{FileCoverage, 1};
  coverage::LineCoverageIterator LCIEnd = LCI.getEnd();
  for (; LCI != LCIEnd; ++LCI) {
    const coverage::LineCoverageStats &LCS = *LCI;
    if (LCS.isMapped()) {
      OS << "DA:" << LCS.getLine() << ',' << LCS.getExecutionCount() << '\n';
    }
  }
}

std::vector<NestedCountedRegion>
collectNestedBranches(const coverage::CoverageMapping &Coverage,
                      ArrayRef<llvm::coverage::ExpansionRecord> Expansions,
                      std::vector<LineColPair> &NestedPath, int ViewDepth = 0,
                      int SrcLine = 0) {
  std::vector<NestedCountedRegion> Branches;
  for (const auto &Expansion : Expansions) {
    auto ExpansionCoverage = Coverage.getCoverageForExpansion(Expansion);

    // If we're at the top level, set the corresponding source line.
    if (ViewDepth == 0)
      SrcLine = Expansion.Region.LineStart;

    NestedPath.push_back(Expansion.Region.startLoc());
    // Recursively collect branches from nested expansions.
    auto NestedExpansions = ExpansionCoverage.getExpansions();
    auto NestedExBranches = collectNestedBranches(
        Coverage, NestedExpansions, NestedPath, ViewDepth + 1, SrcLine);
    append_range(Branches, NestedExBranches);

    // Add branches from this level of expansion.
    auto ExBranches = ExpansionCoverage.getBranches();
    for (auto B : ExBranches)
      if (B.FileID == Expansion.FileID) {
        B.LineStart = SrcLine;
        Branches.push_back(NestedCountedRegion(NestedPath, B));
      }

    NestedPath.pop_back();
  }

  return Branches;
}

bool sortGroup(const GroupedCountedRegion &I, const GroupedCountedRegion &J) {
  if (I.Region.LineStart != J.Region.LineStart)
    return I.Region.LineStart < J.Region.LineStart;
#if 1
  // LineEnd and ColumnEnd is used by macros
  if (I.NestedPath != J.NestedPath)
    return I.NestedPath < J.NestedPath;
#else
  if (I.Region.LineEnd != J.Region.LineEnd)
    return I.Region.LineEnd < J.Region.LineEnd;
  if (I.Region.ColumnEnd != J.Region.ColumnEnd)
    return I.Region.ColumnEnd < J.Region.ColumnEnd;
#endif
  if (I.Region.ColumnStart != J.Region.ColumnStart)
    return I.Region.ColumnStart < J.Region.ColumnStart;

  if (I.GroupId != J.GroupId)
    return I.GroupId < J.GroupId;
#if 1
  return I.InstanceId < J.InstanceId;
#else
  if (I.InstanceId != J.InstanceId)
    return I.InstanceId < J.InstanceId;
  return I.Region.ColumnStart < J.Region.ColumnStart;
#endif
}

void appendGroupedCountedRegions(const std::vector<CountedRegion> &Src,
                                 uint64_t GroupId, uint64_t InstanceId,
                                 std::vector<GroupedCountedRegion> &Dst) {
  auto Unfolded = make_filter_range(Src, [](auto &Region) {
    return !Region.TrueFolded || !Region.FalseFolded;
  });
  Dst.reserve(Dst.size() + Src.size());
  std::transform(Unfolded.begin(), Unfolded.end(), std::back_inserter(Dst),
                 [=](auto &Region) {
                   return GroupedCountedRegion(GroupId, InstanceId, Region,
                                               {Region.startLoc()});
                 });
}

void appendGroupedCountedRegions(const std::vector<NestedCountedRegion> &Src,
                                 uint64_t GroupId, uint64_t InstanceId,
                                 std::vector<GroupedCountedRegion> &Dst) {
  auto Unfolded = make_filter_range(Src, [](auto &NestedRegion) {
    return !NestedRegion.Region.TrueFolded || !NestedRegion.Region.FalseFolded;
  });
  Dst.reserve(Dst.size() + Src.size());
  std::transform(Unfolded.begin(), Unfolded.end(), std::back_inserter(Dst),
                 [=](auto &NestedRegion) {
                   return GroupedCountedRegion(GroupId, InstanceId,
                                               NestedRegion.Region,
                                               NestedRegion.NestedPath);
                 });
}

template <typename Key>
size_t makeUniqueId(std::map<Key, size_t> &Map, Key key) {
  auto Itr = Map.find(key);
  if (Itr == Map.end()) {
    Itr = Map.insert(std::make_pair(key, Map.size())).first;
  }
  return Itr->second;
}

void combineInstanceCounts(std::vector<GroupedCountedRegion> &Branches) {
  auto NextBranch = Branches.begin();
  auto EndBranch = Branches.end();

  while (NextBranch != EndBranch) {
    auto SumBranch = NextBranch++;

    while (NextBranch != EndBranch &&
           SumBranch->Region.LineStart == NextBranch->Region.LineStart &&
           SumBranch->Region.ColumnStart == NextBranch->Region.ColumnStart &&
           SumBranch->Region.LineEnd == NextBranch->Region.LineEnd &&
           SumBranch->Region.ColumnEnd == NextBranch->Region.ColumnEnd &&
           SumBranch->NestedPath == NextBranch->NestedPath) {
      if (!NextBranch->Region.TrueFolded || !NextBranch->Region.FalseFolded) {
        SumBranch->Region.ExecutionCount += NextBranch->Region.ExecutionCount;
        SumBranch->Region.FalseExecutionCount +=
            NextBranch->Region.FalseExecutionCount;
        NextBranch->Region.TrueFolded = true;
        NextBranch->Region.FalseFolded = true;
      }
      NextBranch++;
    }
  }
}

raw_ostream &operator<<(raw_ostream &OS, const LineColPair &Location) {
  return OS << "(" << Location.first << "," << Location.second << ")";
}

raw_ostream &operator<<(raw_ostream &OS, const std::vector<LineColPair> &Path) {
  OS << "[";

  bool first = true;
  for (auto &Element : Path) {
    if (!first)
      OS << ", ";
    OS << Element;
    first = false;
  }
  OS << "]";
  return OS;
}

void renderBranchExecutionCounts(raw_ostream &OS,
                                 const coverage::CoverageMapping &Coverage,
                                 const std::string &SourceFile,
                                 const coverage::CoverageData &FileCoverage,
                                 BranchCoverageCollisionMode CollisionMode) {
  using BlockKey = std::pair<uint64_t, uint64_t>;

  std::vector<GroupedCountedRegion> Branches;
  std::map<uint64_t, size_t> GroupInstances;

  DemangleCache DC;
  uint64_t GroupId = 0;

  for (const auto &Group : Coverage.getInstantiationGroups(SourceFile)) {
    LLVM_DEBUG(dbgs() << "renderBranchExecutionCounts: Group size="
                      << Group.size() << " line=" << Group.getLine()
                      << " column=" << Group.getColumn() << "\n");

    uint64_t InstanceId = 0;
    for (const FunctionRecord *Function : Group.getInstantiations()) {
      StringRef Funcname = DC.demangle(Function->Name);
      LLVM_DEBUG(dbgs() << "renderBranchExecutionCounts: Funcname=" << Funcname
                        << " ExecutionCount=" << Function->ExecutionCount
                        << "\n");

      auto SubViewCoverage = Coverage.getCoverageForFunction(*Function);
      auto SubViewExpansions = SubViewCoverage.getExpansions();
      auto SubViewBranches = SubViewCoverage.getBranches();
      for (auto &Branch : SubViewExpansions) {
        LLVM_DEBUG(dbgs() << "renderBranchExecutionCounts: SubViewExpansion "
                          << " LineStart=" << Branch.Region.LineStart
                          << " ColumnStart=" << Branch.Region.ColumnStart
                          << " LineEnd=" << Branch.Region.LineEnd
                          << " ColumnEnd=" << Branch.Region.ColumnEnd
                          << " ExecutionCount=" << Branch.Region.ExecutionCount
                          << " FalseExecutionCount="
                          << Branch.Region.FalseExecutionCount << "\n");
      }

      // Append Branches to Groups container.
      appendGroupedCountedRegions(SubViewBranches, GroupId, InstanceId,
                                  Branches);

      // Recursively collect branches for all file expansions.
      std::vector<LineColPair> NestedPath;
      std::vector<NestedCountedRegion> ExBranches =
          collectNestedBranches(Coverage, SubViewExpansions, NestedPath);
      for (auto &ExBranch : ExBranches) {
        LLVM_DEBUG(dbgs() << "renderBranchExecutionCounts: ExBranch "
                          << " LineStart=" << ExBranch.Region.LineStart
                          << " ColumnStart=" << ExBranch.Region.ColumnStart
                          << " LineEnd=" << ExBranch.Region.LineEnd
                          << " ColumnEnd=" << ExBranch.Region.ColumnEnd
                          << " ExecutionCount="
                          << ExBranch.Region.ExecutionCount
                          << " FalseExecutionCount="
                          << ExBranch.Region.FalseExecutionCount
                          << " NestedPath=" << ExBranch.NestedPath << "\n");
      }
      // Append Expansion Branches to Groups container.
      appendGroupedCountedRegions(ExBranches, GroupId, InstanceId, Branches);

      InstanceId++;
    }
    GroupInstances[GroupId] = InstanceId;

    GroupId++;
  }

  // Sort branches based on line number to ensure branches corresponding to the
  // same source line are counted together.
  llvm::sort(Branches, sortGroup);

  if (CollisionMode == BranchCoverageCollisionMode::SumUp) {
    combineInstanceCounts(Branches);
  }

  auto NextBranch = Branches.begin();
  auto EndBranch = Branches.end();

  // Branches with the same source line are enumerated individually
  // (BranchIndex) as well as based on True/False pairs (PairIndex).
  while (NextBranch != EndBranch) {
    unsigned CurrentLine = NextBranch->Region.LineStart;
    std::map<BlockKey, size_t> Blocks;
    std::map<LineColPair, size_t> RootBranches;
    std::map<std::vector<LineColPair>, size_t> Branches;

    while (NextBranch != EndBranch &&
           CurrentLine == NextBranch->Region.LineStart) {

      if (NextBranch->Region.TrueFolded && NextBranch->Region.FalseFolded) {
        NextBranch++;
        continue;
      }

      uint64_t EffectiveInstanceId =
          CollisionMode == BranchCoverageCollisionMode::Unique
              ? NextBranch->InstanceId
              : 0;
      auto BlockId = makeUniqueId(
          Blocks, std::make_pair(NextBranch->GroupId, EffectiveInstanceId));
      auto SourceBranchIndex =
          makeUniqueId(RootBranches, NextBranch->NestedPath.front());
      auto BranchIndex = makeUniqueId(Branches, NextBranch->NestedPath);
      // TODO: Check BranchIndex for limit
      auto BlockNumber = (BlockId * 1000) + SourceBranchIndex;
      auto InstanceCount = GroupInstances[NextBranch->GroupId];

#if 1
      LLVM_DEBUG(
          dbgs() << "renderBranchExecutionCounts: NextBranch"
                 << " NestedPath=" << NextBranch->NestedPath
                 << " GroupId=" << NextBranch->GroupId
                 << " InstanceId=" << NextBranch->InstanceId
                 << " LineStart=" << NextBranch->Region.LineStart
                 << " ColumnStart=" << NextBranch->Region.ColumnStart
                 << " LineEnd=" << NextBranch->Region.LineEnd
                 << " ColumnEnd=" << NextBranch->Region.ColumnEnd
                 << " ExecutionCount=" << NextBranch->Region.ExecutionCount
                 << " FalseExecutionCount="
                 << NextBranch->Region.FalseExecutionCount << " BranchIndex="
                 << BranchIndex << " BlockNumber=" << BlockNumber
                 << " CurrentLine=" << CurrentLine << "\n");

      LLVM_OUT(OS << "# renderBranchExecutionCounts: NextBranch"
                  << " NestedPath=" << NextBranch->NestedPath
                  << " GroupId=" << NextBranch->GroupId
                  << " InstanceId=" << NextBranch->InstanceId
                  << " LineStart=" << NextBranch->Region.LineStart
                  << " ColumnStart=" << NextBranch->Region.ColumnStart
                  << " ExecutionCount=" << NextBranch->Region.ExecutionCount
                  << " FalseExecutionCount="
                  << NextBranch->Region.FalseExecutionCount << " BranchIndex="
                  << BranchIndex << " BlockNumber=" << BlockNumber
                  << " CurrentLine=" << CurrentLine << "\n");
#endif

      unsigned BC1 = NextBranch->Region.ExecutionCount;
      unsigned BC2 = NextBranch->Region.FalseExecutionCount;
      bool BranchNotExecuted = (BC1 == 0 && BC2 == 0);

      for (int I = 0; I < 2; I++) {
        auto PairIndex = (BranchIndex * 2) + I;
        auto BranchId =
            (InstanceCount >= 2 &&
             CollisionMode == BranchCoverageCollisionMode::ExpressInstance)
                ? "I" + std::to_string(NextBranch->InstanceId) + "[" +
                      std::to_string(PairIndex) + "]"
                : std::to_string(PairIndex);

        OS << "BRDA:" << CurrentLine << ',' << BlockNumber << ',' << BranchId;
        if (BranchNotExecuted)
          OS << ',' << '-' << '\n';
        else
          OS << ',' << (I == 0 ? BC1 : BC2) << '\n';
      }

      NextBranch++;
    }
  }
}

void renderLineSummary(raw_ostream &OS, const FileCoverageSummary &Summary) {
  OS << "LF:" << Summary.LineCoverage.getNumLines() << '\n'
     << "LH:" << Summary.LineCoverage.getCovered() << '\n';
}

void renderBranchSummary(raw_ostream &OS, const FileCoverageSummary &Summary) {
  OS << "BRF:" << Summary.BranchCoverage.getNumBranches() << '\n'
     << "BRH:" << Summary.BranchCoverage.getCovered() << '\n';
}

void renderFile(raw_ostream &OS, const coverage::CoverageMapping &Coverage,
                const std::string &Filename,
                const FileCoverageSummary &FileReport, bool ExportSummaryOnly,
                bool SkipFunctions, bool SkipBranches,
                BranchCoverageCollisionMode CollisionMode) {
  OS << "SF:" << Filename << '\n';

  if (!ExportSummaryOnly && !SkipFunctions) {
    renderFunctions(OS, Coverage.getCoveredFunctions(Filename));
  }
  renderFunctionSummary(OS, FileReport);

  if (!ExportSummaryOnly) {
    // Calculate and render detailed coverage information for given file.
    auto FileCoverage = Coverage.getCoverageForFile(Filename);
    renderLineExecutionCounts(OS, FileCoverage);
    if (!SkipBranches)
      renderBranchExecutionCounts(OS, Coverage, Filename, FileCoverage,
                                  CollisionMode);
  }
  if (!SkipBranches)
    renderBranchSummary(OS, FileReport);
  renderLineSummary(OS, FileReport);

  OS << "end_of_record\n";
}

void renderFiles(raw_ostream &OS, const coverage::CoverageMapping &Coverage,
                 ArrayRef<std::string> SourceFiles,
                 ArrayRef<FileCoverageSummary> FileReports,
                 bool ExportSummaryOnly, bool SkipFunctions, bool SkipBranches,
                 BranchCoverageCollisionMode CollisionMode) {
  for (unsigned I = 0, E = SourceFiles.size(); I < E; ++I)
    renderFile(OS, Coverage, SourceFiles[I], FileReports[I], ExportSummaryOnly,
               SkipFunctions, SkipBranches, CollisionMode);
}

} // end anonymous namespace

void CoverageExporterLcov::renderRoot(const CoverageFilters &IgnoreFilters) {
  std::vector<std::string> SourceFiles;
  for (StringRef SF : Coverage.getUniqueSourceFiles()) {
    if (!IgnoreFilters.matchesFilename(SF))
      SourceFiles.emplace_back(SF);
  }
  renderRoot(SourceFiles);
}

void CoverageExporterLcov::renderRoot(ArrayRef<std::string> SourceFiles) {
  FileCoverageSummary Totals = FileCoverageSummary("Totals");
  auto FileReports = CoverageReport::prepareFileReports(Coverage, Totals,
                                                        SourceFiles, Options);
  renderFiles(OS, Coverage, SourceFiles, FileReports, Options.ExportSummaryOnly,
              Options.SkipFunctions, Options.SkipBranches,
              Options.ExportBranchCoverageCollisionMode);
}
