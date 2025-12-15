extern strtoull
extern printf

%include "utils.inc"
default rel

%define MAX_BUTTONS 10
%define MAX_GROUPS 32
%define BUTTON_WIDTH 1

section .data
    msg db "Data: light length %02u; button list length %02u; Mask: 0x%04X;", 10, 0

section .text
global entry
entry:
    push            rsi
    ; Endptr for strtoull (discard)
    push            rax

    ; Get input length
    mov             arg(1), [rsi + 8]
    lea             arg(2), [rsp]
    mov             arg(3), 10
    call            strtoull

    pop             rcx
    pop             rsi

    ; The beginning of the input file
    mov             rdi, [rsi + 16]
    mov             rsi, rax
    call            parse

    xor             eax, eax
    ret

; RDI - pointer to beginning of text
; RSI - length of the input file
parse:
    push            rbp
    push            rbx
    push            r12
    sub             rsp, MAX_GROUPS * MAX_BUTTONS * BUTTON_WIDTH

    ; RBP = ptr to char
    mov             rbp, rdi
    ; R12 = EOF ptr
    lea             r12, [rdi + rsi]
    ; RBX = length
    mov             rbx, rsi

    xor             edi, edi
    xor             esi, esi
.lineloop:
    ; Skip '['
    inc             rbp
    xor             edx, edx
    xor             esi, esi
    xor             edi, edi
.lightloop:
    xor             eax, eax
    cmp             byte [rbp], ']'
    je              .buttongrouploop_pre
    cmp             byte [rbp], '#'
    sete            al
    mov             rcx, rsi
    shl             rax, cl
    or              rdx, rax
    inc             rsi
    inc             rbp
    jmp             .lightloop

.buttongrouploop_pre:
    ; Skip closing bracket
    inc             rbp
.buttongrouploop:
    ; Skip space 
    inc             rbp
    cmp             byte [rbp], '{'
    je              .solveline
    xor             ecx, ecx
    ; Skip parentheses
    inc             rbp
    inc             rdi

.connectionloop:
    mov             al, byte [rbp]
    sub             al, 48
    ; Store button connection
    imul            r8, rdi, MAX_BUTTONS * BUTTON_WIDTH
    add             r8, rsp
    mov             byte [r8 + rcx * BUTTON_WIDTH], al
    inc             rcx
    add             rbp, 2
    cmp             byte [rbp], ' '
    jne             .connectionloop
    jmp             .buttongrouploop

.solveline:
    ; RSI - light length
    ; RDI - button list length
    ; RDX - target mask
    ; RCX - button mask list
    mov             rcx, rsp
    call            solveline
.findnextline:
    inc             rbp
    cmp             byte [rbp], '}'
    jne             .findnextline
    inc             rbp

.nextline:
    xor             rdi, rdi
    inc             rbp
    cmp             rbp, r12
    jb              .lineloop

    add             rsp, MAX_GROUPS * MAX_BUTTONS * BUTTON_WIDTH
    pop             r12
    pop             rbx
    pop             rbp
    ret

solveline:
    push            rax

    call            debugp

    pop             rax
    ret

debugp:
    push            rax

    xor             eax, eax
%ifndef __OUTPUT_FORMAT__, win64
    mov             arg(3), rdi
%else
    mov             arg(4), rdx
    mov             arg(3), rdi
%endif
    mov             arg(2), rsi
    mov             arg(1), msg
    call            printf

    pop             rcx
    ret
