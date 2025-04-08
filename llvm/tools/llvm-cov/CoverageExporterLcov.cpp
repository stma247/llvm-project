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
#include <iostream>

using namespace llvm;
using namespace coverage;

namespace {

struct NestedCountedRegion : public coverage::CountedRegion {
  // Contains the path to default and expanded branches.
  // Size is 1 for default branches and greater 1 for expanded branches.
  std::vector<LineColPair> NestedPath;
  // Indicates whether this item should be ignored at rendering.
  bool Ignore = false;
  //
  unsigned Position;

  NestedCountedRegion(llvm::coverage::CountedRegion Region,
                      std::vector<LineColPair> NestedPath, unsigned Position)
      : llvm::coverage::CountedRegion(std::move(Region)),
        NestedPath(std::move(NestedPath)), Position(Position) {}

  // Returns the root line of the branch.
  unsigned getEffectiveLine() const { return NestedPath.front().first; }
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              const coverage::Counter &v) {
  return os << "Counter[Kind=" << v.getKind() << ",Id=" << v.getCounterID()
            << "]";
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              const coverage::mcdc::Parameters &v) {
  return os << "TODO";
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &os, const LineColPair &v) {
  return os << "LineColPair[Line=" << v.first << ",Column=" << v.second << "]";
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              const std::vector<LineColPair> &v) {
  os << "[";
  bool first = true;
  for (auto &e : v) {
    if (!first)
      os << ", ";
    os << e;
    first = false;
  }
  return os << "]";
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              const coverage::CounterMappingRegion &v) {
  return os << "CounterMappingRegion[Count=" << v.Count
            << ", FalseCount=" << v.FalseCount
            << ", MCDCParams=" << v.MCDCParams << ", FileID=" << v.FileID
            << ", ExpandedFileID=" << v.ExpandedFileID
            << ", LineStart=" << v.LineStart
            << ", ColumnStart=" << v.ColumnStart << ", LineEnd=" << v.LineEnd
            << ", LineEnd=" << v.ColumnEnd << ", Kind=" << v.Kind << "]";
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              const coverage::CountedRegion &v) {
  const coverage::CounterMappingRegion &Base = v;
  return os << "CountedRegion[" << Base
            << ", ExecutionCount=" << v.ExecutionCount
            << ", FalseExecutionCount=" << v.FalseExecutionCount
            << ", TrueFolded=" << v.TrueFolded
            << ", FalseFolded=" << v.FalseFolded
            << ", HasSingleByteCoverage=" << v.HasSingleByteCoverage << "]";
}

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              const NestedCountedRegion &v) {
  const coverage::CountedRegion &Base = v;
  return os << "NestedCountedRegion[" << Base << ", NestedPath=" << v.NestedPath
            << ", Ignore=" << v.Ignore << "]";
}

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
                      std::vector<LineColPair> &NestedPath,
                      unsigned &Position) {
  std::vector<NestedCountedRegion> Branches;
  for (const auto &Expansion : Expansions) {
    auto ExpansionCoverage = Coverage.getCoverageForExpansion(Expansion);

    // Track the path to the nested expansions.
    NestedPath.push_back(Expansion.Region.startLoc());

    // Recursively collect branches from nested expansions.
    auto NestedExpansions = ExpansionCoverage.getExpansions();
    auto NestedExBranches =
        collectNestedBranches(Coverage, NestedExpansions, NestedPath, Position);
    append_range(Branches, NestedExBranches);

    // Add branches from this level of expansion.
    auto ExBranches = ExpansionCoverage.getBranches();
    for (auto &B : ExBranches)
      if (B.FileID == Expansion.FileID) {
        Branches.push_back(NestedCountedRegion(B, NestedPath, Position++));
      }

    NestedPath.pop_back();
  }

  return Branches;
}

void appendNestedCountedRegions(const std::vector<CountedRegion> &Src,
                                std::vector<NestedCountedRegion> &Dst) {
  auto Unfolded = make_filter_range(Src, [](auto &Region) {
    return !Region.TrueFolded || !Region.FalseFolded;
  });
  Dst.reserve(Dst.size() + Src.size());
  unsigned PositionCounter = Dst.size();
  std::transform(Unfolded.begin(), Unfolded.end(), std::back_inserter(Dst),
                 [=, &PositionCounter](auto &Region) {
                   return NestedCountedRegion(Region, {Region.startLoc()},
                                              PositionCounter++);
                 });
}

void appendNestedCountedRegions(const std::vector<NestedCountedRegion> &Src,
                                std::vector<NestedCountedRegion> &Dst) {
  auto Unfolded = make_filter_range(Src, [](auto &NestedRegion) {
    return !NestedRegion.TrueFolded || !NestedRegion.FalseFolded;
  });
  Dst.reserve(Dst.size() + Src.size());
  std::copy(Unfolded.begin(), Unfolded.end(), std::back_inserter(Dst));
}

bool sortLine(const llvm::coverage::CountedRegion &I,
              const llvm::coverage::CountedRegion &J) {
  return (I.LineStart < J.LineStart) ||
         ((I.LineStart == J.LineStart) && (I.ColumnStart < J.ColumnStart));
}

bool sortNested(const NestedCountedRegion &I, const NestedCountedRegion &J) {
  // This sorts each element by line and column.
  // Implies that all elements are first sorted by getEffectiveLine().
#if 0
  return sortLine(I, J);
#elif 1
  return std::tie(I.NestedPath, I.Position) <
         std::tie(J.NestedPath, J.Position);
#else
  auto CountI = std::make_pair(I.ExecutionCount, I.FalseExecutionCount);
  auto CountJ = std::make_pair(J.ExecutionCount, J.FalseExecutionCount);
  return std::tie(I.NestedPath, CountJ) < std::tie(J.NestedPath, CountI);
#endif
}

void combineInstanceCounts(std::vector<NestedCountedRegion> &Branches) {
  auto NextBranch = Branches.begin();
  auto EndBranch = Branches.end();

  while (NextBranch != EndBranch) {
    auto SumBranch = NextBranch++;

    // Ensure that only branches with the same NestedPath are summed up.
    while (NextBranch != EndBranch &&
           SumBranch->NestedPath == NextBranch->NestedPath) {
      SumBranch->ExecutionCount += NextBranch->ExecutionCount;
      SumBranch->FalseExecutionCount += NextBranch->FalseExecutionCount;
      // Mark this branch as ignored.
      NextBranch->Ignore = true;

      NextBranch++;
    }
  }
}

void renderBranchExecutionCounts(raw_ostream &OS,
                                 const coverage::CoverageMapping &Coverage,
                                 const coverage::CoverageData &FileCoverage,
                                 bool UnifyInstances) {

  std::vector<NestedCountedRegion> Branches;

  appendNestedCountedRegions(FileCoverage.getBranches(), Branches);
#if 0
	errs() << "renderBranchExecutionCounts Branches: size=" << Branches.size() << '\n';
#endif

  // Recursively collect branches for all file expansions.
  std::vector<LineColPair> NestedPath;
  unsigned PositionCounter = 0;
  std::vector<NestedCountedRegion> ExBranches = collectNestedBranches(
      Coverage, FileCoverage.getExpansions(), NestedPath, PositionCounter);
#if 0
  errs() << "renderBranchExecutionCounts ExBranches: size=" << ExBranches.size() << '\n';
#endif

  // Append Expansion Branches to Source Branches.
  appendNestedCountedRegions(ExBranches, Branches);
#if 0
  int index = 0;
  for(auto& B : Branches) {
	errs() << "  Branch[" << index << "]: " << B << '\n'; 
	index++;
  }
#endif

  // Sort branches based on line number to ensure branches corresponding to the
  // same source line are counted together.
  llvm::sort(Branches, sortNested);

#if 0
  errs() << "renderBranchExecutionCounts AfterSort\n"; 
  index = 0;
  for(auto& B : Branches) {
	errs() << "  Branch[" << index << "]: " << B << '\n'; 
	index++;
  }
#endif

  if (UnifyInstances) {
    combineInstanceCounts(Branches);
  }

  auto NextBranch = Branches.begin();
  auto EndBranch = Branches.end();

  // Branches with the same source line are enumerated individually
  // (BranchIndex) as well as based on True/False pairs (PairIndex).
  while (NextBranch != EndBranch) {
    unsigned CurrentLine = NextBranch->getEffectiveLine();
    unsigned PairIndex = 0;
    unsigned BranchIndex = 0;

    while (NextBranch != EndBranch &&
           CurrentLine == NextBranch->getEffectiveLine()) {
      if (!NextBranch->Ignore) {
        unsigned BC1 = NextBranch->ExecutionCount;
        unsigned BC2 = NextBranch->FalseExecutionCount;
        bool BranchNotExecuted = (BC1 == 0 && BC2 == 0);

        for (int I = 0; I < 2; I++, BranchIndex++) {
          OS << "BRDA:" << CurrentLine << ',' << PairIndex << ','
             << BranchIndex;
          if (BranchNotExecuted)
            OS << ',' << '-' << '\n';
          else
            OS << ',' << (I == 0 ? BC1 : BC2) << '\n';
        }

        PairIndex++;
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
                bool SkipFunctions, bool SkipBranches, bool UnifyInstances) {
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
      renderBranchExecutionCounts(OS, Coverage, FileCoverage, UnifyInstances);
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
                 bool UnifyInstances) {
  for (unsigned I = 0, E = SourceFiles.size(); I < E; ++I)
    renderFile(OS, Coverage, SourceFiles[I], FileReports[I], ExportSummaryOnly,
               SkipFunctions, SkipBranches, UnifyInstances);
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
              Options.UnifyFunctionInstantiations);
}
