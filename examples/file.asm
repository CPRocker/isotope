global _start

; program starts
_start:
    ; return
    mov eax, 1
    push dword, 2098
    push eax
    int 0x80

    ; return
    mov eax, 1
    push dword, 303213
    push eax
    int 0x80

