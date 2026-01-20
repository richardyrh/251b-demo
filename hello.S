  .section .text
  .globl _start
_start:
  la t0, msg
1:
  lbu t1, 0(t0)
  beqz t1, 2f
  li t2, 0x10000000
  sb t1, 0(t2)
  addi t0, t0, 1
  j 1b
2:
  j 2b

  .section .rodata
msg:
  .asciz "Hello, RISC-V!\n"
