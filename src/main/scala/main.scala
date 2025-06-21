import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import model.Instruction.*
import model.Value.{ClosureValue, IntValue, NullValue}
import model.io.ContextWriter.ContextWriterConfig
import model.io.*
import model.parser.ml.{MlLexer, MlParser}
import model.{CAMExecutor, Instruction}
import tofu.syntax.feither.EitherFOps

import java.io.ByteArrayInputStream
import java.nio.file.Paths

val camContextConfig: ContextWriterConfig = ContextWriterConfig(
  file = Paths.get("out/result.md"),
  termCol = 50,
  codeCol = 50,
  stackCol = 100,
)

implicit val rowFormater: RowFormater[IO]     = RowFormater.make[IO]
implicit val fileWriter: FileWriter[IO]       = FileWriter.make[IO]
implicit val contextWriter: ContextWriter[IO] = model.io.ContextWriter.make[IO](camContextConfig)

val executor: CAMExecutor[IO] = CAMExecutor.make[IO]

@main
def main(): Unit = {
  // TODO: UnaryOp.negation to parser grammar
  val input = "let rec fact = fun n -> if n == 0 then 1 else n*(fact (n - 1)) in (fact 5)"
  val is    = new ByteArrayInputStream(input.getBytes())

  val mlParser = MlParser[IO](is)
  val expr = mlParser
    .parse()
    .run(mlParser.lex)
    .value
    .foldF(_.raiseError[IO, (MlLexer, MlParser.ParseContext)], _.pure[IO])
    .map(_._2.res)
    .unsafeRunSync()
  println(expr)

  val executorRes = executor.executeExpression(expr).unsafeRunSync()
  println(executorRes)

  val instr =
    InstructionsRaw.factorial._1 :: InstructionsRaw.factorial._2 // expr.toInstructions[IO](List.empty).map(_.appended(Instruction.STOP)).unsafeRunSync()
  println("\n\n")
  println(expr.toInstructions[IO](List.empty).map(_.appended(Instruction.STOP)).unsafeRunSync())
  println(instr)
  println(instr.head.execute(List.empty)(instr.tail, List(NullValue)))

}

object InstructionsRaw {
  val onePlusOne: (Instruction, List[Instruction]) = (
    PUSH,
    List(
      PUSH,
      QUOTE(IntValue(1)),
      CONS,
      CAR,
      SWAP,
      QUOTE(IntValue(1)),
      ADD,
      STOP,
    ),
  )

  // let x = 3 in if 1==1 then if 2==3 then 4 else fun y -> y + x 10 else 6
  val letIfFun: (Instruction, List[Instruction]) = (
    PUSH,
    List(
      QUOTE(IntValue(3)),
      CONS,
      PUSH,
      PUSH,
      QUOTE(IntValue(1)),
      SWAP,
      QUOTE(IntValue(1)),
      EQ,
      BRANCH(
        ClosureValue(
          List(
            PUSH,
            PUSH,
            QUOTE(IntValue(3)),
            SWAP,
            QUOTE(IntValue(2)),
            EQ,
            BRANCH(
              ClosureValue(List(QUOTE(IntValue(4)), RET)),
              ClosureValue(
                List(
                  PUSH,
                  QUOTE(
                    ClosureValue(
                      List(
                        PUSH,
                        PUSH,
                        QUOTE(IntValue(10)),
                        SWAP,
                        CDR,
                        CAR,
                        SPLIT,
                        SHR3,
                        CONS,
                        SWAP,
                        CALL,
                        SWAP,
                        CAR,
                        ADD,
                        RET,
                      ),
                    ),
                  ),
                  SWAP,
                  CONS,
                  RET,
                ),
              ),
            ),
            CALL,
            RET,
          ),
        ),
        ClosureValue(List(QUOTE(IntValue(6)), RET)),
      ),
      CALL,
      STOP,
    ),
  )

  val xSquareSquare: (Instruction, List[Instruction]) = (
    PUSH,
    List(
      PUSH,
      QUOTE(
        ClosureValue(
          List(
            PUSH,
            QUOTE(
              ClosureValue(
                List(
                  PUSH,
                  PUSH,
                  CAR,
                  SWAP,
                  CDR,
                  CAR,
                  SPLIT,
                  SHR3,
                  CONS,
                  SWAP,
                  CALL,
                  SWAP,
                  CDR,
                  CAR,
                  SPLIT,
                  SHR3,
                  CONS,
                  SWAP,
                  CALL,
                  RET,
                ),
              ),
            ),
            SWAP,
            CONS,
            RET,
          ),
        ),
      ),
      SWAP,
      CONS,
      CONS,
      PUSH,
      PUSH,
      QUOTE(
        ClosureValue(
          List(
            PUSH,
            CAR,
            SWAP,
            CAR,
            MUL,
            RET,
          ),
        ),
      ),
      SWAP,
      CONS,
      CONS,
      PUSH,
      QUOTE(IntValue(5)),
      SWAP,
      PUSH,
      CAR,
      SWAP,
      CDR,
      CAR,
      SPLIT,
      SHR3,
      CONS,
      SWAP,
      CALL,
      SPLIT,
      SHR3,
      CONS,
      SWAP,
      CALL,
      STOP,
    ),
  )

  val factorial: (Instruction, List[Instruction]) = (
    PUSH,
    List(
      QUOTE(NullValue),
      PUSH,
      QUOTE(
        ClosureValue(
          List(
            PUSH,
            PUSH,
            QUOTE(IntValue(0)),
            SWAP,
            CAR,
            EQ,
            BRANCH(
              ClosureValue(List(QUOTE(IntValue(1)), RET)),
              ClosureValue(
                List(
                  PUSH,
                  PUSH,
                  PUSH,
                  QUOTE(IntValue(1)),
                  SWAP,
                  CAR,
                  SUB,
                  SWAP,
                  CDR,
                  CAR,
                  SPLIT,
                  SHR3,
                  CONS,
                  SWAP,
                  CALL,
                  SWAP,
                  CAR,
                  MUL,
                  RET,
                ),
              ),
            ),
            CALL,
            RET,
          ),
        ),
      ),
      SWAP,
      CONS,
      PUSH,
      SHL3,
      CONS,
      SETFST,
      CAR,
      PUSH,
      QUOTE(IntValue(5)),
      SWAP,
      CAR,
      SPLIT,
      SHR3,
      CONS,
      SWAP,
      CALL,
      STOP,
    ),
  )

  val nestedconditional: (Instruction, List[Instruction]) = (
    PUSH,
    List(
      PUSH,
      QUOTE(IntValue(1)),
      SWAP,
      QUOTE(IntValue(1)),
      EQ,
      BRANCH(
        ClosureValue(
          List(
            PUSH,
            PUSH,
            QUOTE(IntValue(3)),
            SWAP,
            QUOTE(IntValue(2)),
            EQ,
            BRANCH(
              ClosureValue(List(QUOTE(IntValue(4)), RET)),
              ClosureValue(List(QUOTE(IntValue(5)), RET)),
            ),
            CALL,
            RET,
          ),
        ),
        ClosureValue(List(QUOTE(IntValue(6)), RET)),
      ),
      CALL,
      STOP,
    ),
  )
}
