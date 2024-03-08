section .text
    global main

; program starts
main:
    ; declare var x
    push qword 10

    ; declare var y
    push qword 11

    ; declare var z
    push qword 21

    ; declare var a
    push qword 32

    ; return
    ; get y
    mov rax, [rsp + 16]
    push rax
    ; get z
    mov rax, [rsp + 16]
    push rax
    pop rbx
    pop rax
    add rax, rbx
    push rax
    ; get x
    mov rax, [rsp + 32]
    push rax
    pop rbx
    pop rax
    xor rdx, rdx
    idiv rbx
    push rax
    pop rax
    add rsp, 32
    ret

    ; return
    push qword 0
    pop rax
    add rsp, 0
    ret

