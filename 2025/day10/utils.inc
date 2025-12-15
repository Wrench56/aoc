; ================================= ;
;  > utils.inc                      ;
; --------------------------------- ;
;  NASM utility macros and helpers  ;
;                                   ;
;  Author(s) : Mark Devenyi         ;
;  Created   : 21 Feb 2025          ;
;  Updated   : 11 Dec 2025          ;
;  License   : MIT                  ;
; ================================= ;

; Arguments:
;  > 1: number - argument number
%ifidn __OUTPUT_FORMAT__, win64
    ; Windows x64 ABI
    %define __stack_arg(n) [rsp + 8 * (n + 3)]

    %define __arg1  rcx
    %define __arg2  rdx
    %define __arg3  r8
    %define __arg4  r9
    %define __arg5  __stack_arg(1)
    %define __arg6  __stack_arg(2)
    %define __arg7  __stack_arg(3)
    %define __arg8  __stack_arg(4)
    %define __arg9  __stack_arg(5)
    %define __arg10 __stack_arg(6)
    %define __arg11 __stack_arg(7)
    %define __arg12 __stack_arg(8)
    %define __arg13 __stack_arg(9)
    %define __arg14 __stack_arg(10)
    %define __arg15 __stack_arg(11)
    %define __arg16 __stack_arg(12)

%elifidn __OUTPUT_FORMAT__, elf64
    ; System V AMD64 ABI
    %define __stack_arg(n) [rsp + 8 * n]

    %define __arg1  rdi
    %define __arg2  rsi
    %define __arg3  rdx
    %define __arg4  rcx
    %define __arg5  r8
    %define __arg6  r9
    %define __arg7  __stack_arg(1)
    %define __arg8  __stack_arg(2)
    %define __arg9  __stack_arg(3)
    %define __arg10 __stack_arg(4)
    %define __arg11 __stack_arg(5)
    %define __arg12 __stack_arg(6)
    %define __arg13 __stack_arg(7)
    %define __arg14 __stack_arg(8)
    %define __arg15 __stack_arg(9)
    %define __arg16 __stack_arg(10)

%else
    %error Unsupported output format
%endif

%define arg(n) __arg %+ n
