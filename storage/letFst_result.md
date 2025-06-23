| Term | Code | Stack |
|:-:|:-:|:-:|
| () | [PUSH, PUSH, QUOTE(2), SWAP, QUOTE(10), CONS, CONS, CAR, CAR, STOP] | [] |
| () | [PUSH, QUOTE(2), SWAP, QUOTE(10), CONS, CONS, CAR, CAR, STOP] | [()] |
| () | [QUOTE(2), SWAP, QUOTE(10), CONS, CONS, CAR, CAR, STOP] | [(), ()] |
| 2 | [SWAP, QUOTE(10), CONS, CONS, CAR, CAR, STOP] | [(), ()] |
| () | [QUOTE(10), CONS, CONS, CAR, CAR, STOP] | [2, ()] |
| 10 | [CONS, CONS, CAR, CAR, STOP] | [2, ()] |
| (10, 2) | [CONS, CAR, CAR, STOP] | [()] |
| ((&0, &1), ()) | [CAR, CAR, STOP] | [] |
| (10, 2) | [CAR, STOP] | [] |
| 10 | [STOP] | [] |
