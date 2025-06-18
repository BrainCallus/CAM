import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import model.Instruction.*
import model.Value.{ClosureValue, IntValue, NullValue}
import model.parser.ml.{MlLexer, MlParser}
import model.{Instruction, Value}
import tofu.syntax.feither.EitherFOps

import java.io.ByteArrayInputStream

@main
def main(): Unit = {
  val instructions = List(
    QUOTE(IntValue(1)),
    SWAP,
    QUOTE(IntValue(1)),
    ADD,
    STOP,
  )
  val input                               = "let rec fact = fun n -> if n == 0 then 1 else n*(fact (n-1)) in (fact 5)"
  val is                                  = new ByteArrayInputStream(input.getBytes())

  val mlParser = MlParser[IO](is)
  val expr = mlParser
    .parse()
    .run(mlParser.lex)
    .value
    .foldF(_.raiseError[IO, (MlLexer, MlParser.ParseContext)], _.pure[IO])
    .map(_._2.res)
    .unsafeRunSync()
  println(expr)

  val instr = expr.toInstructions[IO](List.empty).map(_.appended(Instruction.STOP)).unsafeRunSync()
   println("\n\n")
   println(instr)
  println(instr.head.execute(List.empty)(instr.tail, List(NullValue)))
}

object InstructionsRaw {
  val onePlusOne: (Instruction, List[Instruction]) = (
    DUPLICATE,
    List(
      DUPLICATE,
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
    DUPLICATE,
    List(
      QUOTE(IntValue(3)),
      CONS,
      DUPLICATE,
      DUPLICATE,
      QUOTE(IntValue(1)),
      SWAP,
      QUOTE(IntValue(1)),
      EQ,
      BRANCH(
        ClosureValue(
          List(
            DUPLICATE,
            DUPLICATE,
            QUOTE(IntValue(3)),
            SWAP,
            QUOTE(IntValue(2)),
            EQ,
            BRANCH(
              ClosureValue(List(QUOTE(IntValue(4)), RET)),
              ClosureValue(
                List(
                  PUSH(
                    ClosureValue(
                      List(
                        DUPLICATE,
                        DUPLICATE,
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
    DUPLICATE,
    List(
      PUSH(
        ClosureValue(
          List(
            PUSH(
              ClosureValue(
                List(
                  DUPLICATE,
                  DUPLICATE,
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
      DUPLICATE,
      PUSH(
        ClosureValue(
          List(
            DUPLICATE,
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
      DUPLICATE,
      QUOTE(IntValue(5)),
      SWAP,
      DUPLICATE,
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
    PUSH(NullValue),
    List(
      PUSH(
        ClosureValue(
          List(
            DUPLICATE,
            DUPLICATE,
            QUOTE(IntValue(0)),
            SWAP,
            CAR,
            EQ,
            BRANCH(
              ClosureValue(List(QUOTE(IntValue(1)), RET)),
              ClosureValue(
                List(
                  DUPLICATE,
                  DUPLICATE,
                  DUPLICATE,
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
      DUPLICATE,
      SHL3,
      CONS,
      SETFST,
      CAR,
      DUPLICATE,
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
    DUPLICATE,
    List(
      DUPLICATE,
      QUOTE(IntValue(1)),
      SWAP,
      QUOTE(IntValue(1)),
      EQ,
      BRANCH(
        ClosureValue(
          List(
            DUPLICATE,
            DUPLICATE,
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
