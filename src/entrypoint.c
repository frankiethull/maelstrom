// We need to forward routine registration from C to Rust
// to avoid the linker removing the static library.
//
// MAELSTROM_HAS_RUST is defined by src/Makevars when cargo built the Rust
// static library. Without it (cargo absent at install time) the package
// still installs; the reverb falls back to pure R at runtime.

#ifdef MAELSTROM_HAS_RUST
void R_init_maelstrom_extendr(void *dll);
void register_extendr_panic_hook(void);
#endif

void R_init_maelstrom(void *dll) {
#ifdef MAELSTROM_HAS_RUST
    register_extendr_panic_hook();
    R_init_maelstrom_extendr(dll);
#endif
}
