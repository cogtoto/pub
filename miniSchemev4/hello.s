  .text # des instructions suivent
  .globl main # rend main visible pour ld

main:
  movq $message, %rdi # argument de puts
  call puts
  movq $0, %rax # code de retour 0
  ret

.data # des donnees suivent
message:
.string "hello, world!" # termin´ee par 0