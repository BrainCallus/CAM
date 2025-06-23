| Term | Code | Stack |
|:-:|:-:|:-:|
| () | [PUSH, PUSH, PUSH, QUOTE(2), SWAP, QUOTE(1), EQ, BRANCH(Closure([QUOTE(2),RET]), Closure([QUOTE(4),RET])), CALL, SWAP, QUOTE(5), ADD, STOP] | [] |
| () | [PUSH, PUSH, QUOTE(2), SWAP, QUOTE(1), EQ, BRANCH(Closure([QUOTE(2),RET]), Closure([QUOTE(4),RET])), CALL, SWAP, QUOTE(5), ADD, STOP] | [()] |
| () | [PUSH, QUOTE(2), SWAP, QUOTE(1), EQ, BRANCH(Closure([QUOTE(2),RET]), Closure([QUOTE(4),RET])), CALL, SWAP, QUOTE(5), ADD, STOP] | [(), ()] |
| () | [QUOTE(2), SWAP, QUOTE(1), EQ, BRANCH(Closure([QUOTE(2),RET]), Closure([QUOTE(4),RET])), CALL, SWAP, QUOTE(5), ADD, STOP] | [(), (), ()] |
| 2 | [SWAP, QUOTE(1), EQ, BRANCH(Closure([QUOTE(2),RET]), Closure([QUOTE(4),RET])), CALL, SWAP, QUOTE(5), ADD, STOP] | [(), (), ()] |
| () | [QUOTE(1), EQ, BRANCH(Closure([QUOTE(2),RET]), Closure([QUOTE(4),RET])), CALL, SWAP, QUOTE(5), ADD, STOP] | [2, (), ()] |
| 1 | [EQ, BRANCH(Closure([QUOTE(2),RET]), Closure([QUOTE(4),RET])), CALL, SWAP, QUOTE(5), ADD, STOP] | [2, (), ()] |
| false | [BRANCH(Closure([QUOTE(2),RET]), Closure([QUOTE(4),RET])), CALL, SWAP, QUOTE(5), ADD, STOP] | [(), ()] |
| Closure([QUOTE(4),RET]) | [CALL, SWAP, QUOTE(5), ADD, STOP] | [(), ()] |
| () | [QUOTE(4), RET] | [Closure([SWAP,QUOTE(5),ADD,STOP]), ()] |
| 4 | [RET] | [Closure([SWAP,QUOTE(5),ADD,STOP]), ()] |
| 4 | [SWAP, QUOTE(5), ADD, STOP] | [()] |
| () | [QUOTE(5), ADD, STOP] | [4] |
| 5 | [ADD, STOP] | [4] |
| 9 | [STOP] | [] |
