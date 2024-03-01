section .text
    global main

; program starts
main:
    ; declare var x
    push qword 10

    ; declare var y
    push qword 11

    ; return
    ; get y
    mov rax, [rsp + 0]
    push rax
    pop rax
    add rsp, 16
    ret

    ; return
    push qword 0
    pop rax
    add rsp, 0
    ret

