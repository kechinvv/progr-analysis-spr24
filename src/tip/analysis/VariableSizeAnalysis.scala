package tip.analysis

import tip.ast.ANumber
import tip.ast.AstNodeData.DeclarationData
import tip.cfg._
import tip.lattices.IntervalLattice.{IntNum, MInf, PInf}
import tip.lattices._
import tip.solvers._

import scala.util.Random

trait VariableSizeAnalysisWidening extends IntervalAnalysisWidening {
  override val B: Set[IntervalLattice.Num] = Set[IntervalLattice.Num](MInf, 0, PInf) ++ List.fill(20)(IntNum(Random.nextInt(50))).toSet[IntervalLattice.Num]
}

object VariableSizeAnalysis {
  object Intraprocedural {
    class WorklistSolverWithWidening(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, IntervalLattice)
        with WorklistFixpointSolverWithReachabilityAndWidening[CfgNode]
        with VariableSizeAnalysisWidening

    class WorklistSolverWithWideningAndNarrowing(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolverWithReachability(cfg, IntervalLattice)
        with WorklistFixpointSolverWithReachabilityAndWideningAndNarrowing[CfgNode]
        with VariableSizeAnalysisWidening {

      val narrowingSteps = 5
    }
  }
}
