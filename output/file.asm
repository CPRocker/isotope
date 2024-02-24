section .text
    global main

; program starts
main:
    ; return
    push dword 10
    push dword 20
    push dword 3
    pop rbx
    pop rax
    sub rax, rbx
    push rax
    pop rbx
    pop rax
    imul rax, rbx
    push rax
    push dword 2
    push dword 5
    pop rbx
    pop rax
    imul rax, rbx
    push rax
    pop rbx
    pop rax
    xor rdx, rdx
    idiv rbx
    push rax
    push dword 25
    push dword 50
    pop rbx
    pop rax
    sub rax, rbx
    push rax
    pop rbx
    pop rax
    add rax, rbx
    push rax
    push dword 135
    pop rbx
    pop rax
    sub rax, rbx
    push rax
    pop rax
    ret

    ; return
    push dword 0
    pop rax
    ret

