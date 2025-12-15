extern __attribute__((sysv_abi)) int entry(int argc, char** argv);

int __attribute__((naked)) main(int argc, char** argv) {
#if defined(_WIN32) || defined(_WIN64)
    __asm__ volatile(
        "sub $32, %rsp"
    );
#endif
   __asm__ volatile(
        "call entry"
    );

#if defined(_WIN32) || defined(_WIN64)
    __asm__ volatile(
        "add $32, %rsp\n"
    );
#endif

    __asm__ volatile(
        "ret"
    );
}
