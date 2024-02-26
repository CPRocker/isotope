section .text
    global main

; program starts
main:
    ; return
    push dword 10
    pop rax
    ret

    ; return
    push dword 5
    push dword 2
    pop rbx
    pop rax
    xor rdx, rdx
    idiv rbx
    push rax
    pop rax
    ret

    ; return
    push dword 0
    pop rax
    ret

