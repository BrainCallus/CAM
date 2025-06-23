| Term | Code | Stack |
|:-:|:-:|:-:|
| () | [PUSH, QUOTE(1), SWAP, QUOTE(1), ADD, STOP] | [] |
| () | [QUOTE(1), SWAP, QUOTE(1), ADD, STOP] | [()] |
| 1 | [SWAP, QUOTE(1), ADD, STOP] | [()] |
| () | [QUOTE(1), ADD, STOP] | [1] |
| 1 | [ADD, STOP] | [1] |
| 2 | [STOP] | [] |
