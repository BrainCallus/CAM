| Term | Code | Stack |
|:-:|:-:|:-:|
| () | [PUSH, QUOTE(3), CONS, PUSH, PUSH, QUOTE(1), SWAP, QUOTE(1), EQ, BRANCH(Closure([PUSH,PUSH,QUOTE(3),SWAP,QUOTE(2),EQ,BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])),CALL,RET]), Closure([QUOTE(6),RET])), CALL, STOP] | [] |
| () | [QUOTE(3), CONS, PUSH, PUSH, QUOTE(1), SWAP, QUOTE(1), EQ, BRANCH(Closure([PUSH,PUSH,QUOTE(3),SWAP,QUOTE(2),EQ,BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])),CALL,RET]), Closure([QUOTE(6),RET])), CALL, STOP] | [()] |
| 3 | [CONS, PUSH, PUSH, QUOTE(1), SWAP, QUOTE(1), EQ, BRANCH(Closure([PUSH,PUSH,QUOTE(3),SWAP,QUOTE(2),EQ,BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])),CALL,RET]), Closure([QUOTE(6),RET])), CALL, STOP] | [()] |
| (3, ()) | [PUSH, PUSH, QUOTE(1), SWAP, QUOTE(1), EQ, BRANCH(Closure([PUSH,PUSH,QUOTE(3),SWAP,QUOTE(2),EQ,BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])),CALL,RET]), Closure([QUOTE(6),RET])), CALL, STOP] | [] |
| (3, ()) | [PUSH, QUOTE(1), SWAP, QUOTE(1), EQ, BRANCH(Closure([PUSH,PUSH,QUOTE(3),SWAP,QUOTE(2),EQ,BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])),CALL,RET]), Closure([QUOTE(6),RET])), CALL, STOP] | [(3, ())] |
| (3, ()) | [QUOTE(1), SWAP, QUOTE(1), EQ, BRANCH(Closure([PUSH,PUSH,QUOTE(3),SWAP,QUOTE(2),EQ,BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])),CALL,RET]), Closure([QUOTE(6),RET])), CALL, STOP] | [(3, ()), (3, ())] |
| 1 | [SWAP, QUOTE(1), EQ, BRANCH(Closure([PUSH,PUSH,QUOTE(3),SWAP,QUOTE(2),EQ,BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])),CALL,RET]), Closure([QUOTE(6),RET])), CALL, STOP] | [(3, ()), (3, ())] |
| (3, ()) | [QUOTE(1), EQ, BRANCH(Closure([PUSH,PUSH,QUOTE(3),SWAP,QUOTE(2),EQ,BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])),CALL,RET]), Closure([QUOTE(6),RET])), CALL, STOP] | [1, (3, ())] |
| 1 | [EQ, BRANCH(Closure([PUSH,PUSH,QUOTE(3),SWAP,QUOTE(2),EQ,BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])),CALL,RET]), Closure([QUOTE(6),RET])), CALL, STOP] | [1, (3, ())] |
| true | [BRANCH(Closure([PUSH,PUSH,QUOTE(3),SWAP,QUOTE(2),EQ,BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])),CALL,RET]), Closure([QUOTE(6),RET])), CALL, STOP] | [(3, ())] |
| Closure([PUSH,PUSH,QUOTE(3),SWAP,QUOTE(2),EQ,BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])),CALL,RET]) | [CALL, STOP] | [(3, ())] |
| (3, ()) | [PUSH, PUSH, QUOTE(3), SWAP, QUOTE(2), EQ, BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])), CALL, RET] | [Closure([STOP])] |
| (3, ()) | [PUSH, QUOTE(3), SWAP, QUOTE(2), EQ, BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])), CALL, RET] | [(3, ()), Closure([STOP])] |
| (3, ()) | [QUOTE(3), SWAP, QUOTE(2), EQ, BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])), CALL, RET] | [(3, ()), (3, ()), Closure([STOP])] |
| 3 | [SWAP, QUOTE(2), EQ, BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])), CALL, RET] | [(3, ()), (3, ()), Closure([STOP])] |
| (3, ()) | [QUOTE(2), EQ, BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])), CALL, RET] | [3, (3, ()), Closure([STOP])] |
| 2 | [EQ, BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])), CALL, RET] | [3, (3, ()), Closure([STOP])] |
| false | [BRANCH(Closure([QUOTE(4),RET]), Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET])), CALL, RET] | [(3, ()), Closure([STOP])] |
| Closure([PUSH,QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])),SWAP,CONS,RET]) | [CALL, RET] | [(3, ()), Closure([STOP])] |
| (3, ()) | [PUSH, QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])), SWAP, CONS, RET] | [Closure([RET]), Closure([STOP])] |
| (3, ()) | [QUOTE(Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])), SWAP, CONS, RET] | [(3, ()), Closure([RET]), Closure([STOP])] |
| Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET]) | [SWAP, CONS, RET] | [(3, ()), Closure([RET]), Closure([STOP])] |
| (3, ()) | [CONS, RET] | [Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET]), Closure([RET]), Closure([STOP])] |
| ((&0, &1), Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])) | [RET] | [Closure([RET]), Closure([STOP])] |
| ((&0, &1), Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])) | [RET] | [Closure([STOP])] |
| ((&0, &1), Closure([PUSH,CDR,CAR,SWAP,CAR,ADD,RET])) | [STOP] | [] |
