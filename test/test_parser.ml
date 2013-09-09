open Elf

let string_of_e_class cls =
  match cls with
  | ELFCLASS32 -> "ELF32"
  | ELFCLASS64 -> "ELF64"

let string_of_e_data data =
  match data with
  | ELFDATA2LSB -> "2's complement, little endian"
  | ELFDATA2MSB -> "2's complement, big endian"

let string_of_e_osabi abi =
  match abi with
  | ELFOSABI_SYSV       -> "System V"
  | ELFOSABI_HPUX       -> "HP-UX"
  | ELFOSABI_NETBSD     -> "NetBSD"
  | ELFOSABI_LINUX      -> "Linux"
  | ELFOSABI_SOLARIS    -> "Solaris"
  | ELFOSABI_AIX        -> "AIX"
  | ELFOSABI_IRIX       -> "Irix"
  | ELFOSABI_FREEBSD    -> "FreeBSD"
  | ELFOSABI_TRU64      -> "TRU64"
  | ELFOSABI_MODESTO    -> "Modesto"
  | ELFOSABI_OPENBSD    -> "OpenBSD"
  | ELFOSABI_ARM_AEABI  -> "ARM EABI"
  | ELFOSABI_ARM        -> "ARM"
  | ELFOSABI_STANDALONE -> "Standalone"
  | ELFOSABI_EXT x      -> string_of_int x

let string_of_e_type typ =
  match typ with
  | ET_NONE  -> "None"
  | ET_REL   -> "REL (Relocatable file)"
  | ET_EXEC  -> "EXEC (Executable file)"
  | ET_DYN   -> "DYN (Shared object file)"
  | ET_CORE  -> "CORE (Core file)"
  | ET_EXT x -> string_of_int x

let string_of_e_machine mach =
  match mach with
  | EM_NONE        -> "None"
  | EM_M32         -> "AT&T WE 32100"
  | EM_SPARC       -> "SUN SPARC"
  | EM_386         -> "Intel 80386"
  | EM_68K         -> "Motorola m68k"
  | EM_88K         -> "Motorola m88k"
  | EM_860         -> "Intel 80860"
  | EM_MIPS        -> "MIPS R3000 big-endian"
  | EM_S370        -> "IBM System/370"
  | EM_MIPS_RS3_LE -> "MIPS R3000 little-endian"

  | EM_PARISC      -> "HPPA"
  | EM_VPP500      -> "Fujitsu VPP500"
  | EM_SPARC32PLUS -> "Sun's \"v8plus\""
  | EM_960         -> "Intel 80960"
  | EM_PPC         -> "PowerPC"
  | EM_PPC64       -> "PowerPC 64-bit"
  | EM_S390        -> "IBM S390"

  | EM_V800        -> "NEC V800 series"
  | EM_FR20        -> "Fujitsu FR20"
  | EM_RH32        -> "TRW RH-32"
  | EM_RCE         -> "Motorola RCE"
  | EM_ARM         -> "ARM"
  | EM_ALPHA       -> "Digital Alpha"
  | EM_SH          -> "Hitachi SH"
  | EM_SPARCV9     -> "SPARC v9 64-bit"
  | EM_TRICORE     -> "Siemens Tricore"
  | EM_ARC         -> "Argonaut RISC Core"
  | EM_H8_300      -> "Hitachi H8/300"
  | EM_H8_300H     -> "Hitachi H8/300H"
  | EM_H8S         -> "Hitachi H8S"
  | EM_H8_500      -> "Hitachi H8/500"
  | EM_IA_64       -> "Intel Merced"
  | EM_MIPS_X      -> "Stanford MIPS-X"
  | EM_COLDFIRE    -> "Motorola Coldfire"
  | EM_68HC12      -> "Motorola M68HC12 "
  | EM_MMA         -> "Fujitsu MMA Multimedia Accelerator"
  | EM_PCP         -> "Siemens PCP"
  | EM_NCPU        -> "Sony nCPU embeeded RISC"
  | EM_NDR1        -> "Denso NDR1 microprocessor"
  | EM_STARCORE    -> "Motorola Start*Core processor"
  | EM_ME16        -> "Toyota ME16 processor"
  | EM_ST100       -> "STMicroelectronic ST100 processor"
  | EM_TINYJ       -> "Advanced Logic Corp. Tinyj emb.fam"
  | EM_X86_64      -> "AMD x86-64 architecture"
  | EM_PDSP        -> "Sony DSP Processor"

  | EM_FX66        -> "Siemens FX66 microcontroller"
  | EM_ST9PLUS     -> "STMicroelectronics ST9+ 8/16 mc"
  | EM_ST7         -> "STmicroelectronics ST7 8 bit mc"
  | EM_68HC16      -> "Motorola MC68HC16 microcontroller"
  | EM_68HC11      -> "Motorola MC68HC11 microcontroller"
  | EM_68HC08      -> "Motorola MC68HC08 microcontroller"
  | EM_68HC05      -> "Motorola MC68HC05 microcontroller"
  | EM_SVX         -> "Silicon Graphics SVx"
  | EM_ST19        -> "STMicroelectronics ST19 8 bit mc"
  | EM_VAX         -> "Digital VAX"
  | EM_CRIS        -> "Axis Communications 32-bit embedded processor"
  | EM_JAVELIN     -> "Infineon Technologies 32-bit embedded processor"
  | EM_FIREPATH    -> "Element 14 64-bit DSP Processor"
  | EM_ZSP         -> "LSI Logic 16-bit DSP Processor"
  | EM_MMIX        -> "Donald Knuth's educational 64-bit processor"
  | EM_HUANY       -> "Harvard University machine-independent object files"
  | EM_PRISM       -> "SiTera Prism"
  | EM_AVR         -> "Atmel AVR 8-bit microcontroller"
  | EM_FR30        -> "Fujitsu FR30"
  | EM_D10V        -> "Mitsubishi D10V"
  | EM_D30V        -> "Mitsubishi D30V"
  | EM_V850        -> "NEC v850"
  | EM_M32R        -> "Mitsubishi M32R"
  | EM_MN10300     -> "Matsushita MN10300"
  | EM_MN10200     -> "Matsushita MN10200"
  | EM_PJ          -> "picoJava"
  | EM_OPENRISC    -> "OpenRISC 32-bit embedded processor"
  | EM_ARC_A5      -> "ARC Cores Tangent-A5"
  | EM_XTENSA      -> "Tensilica Xtensa Architecture"
  | EM_AARCH64     -> "ARM AARCH64"
  | EM_TILEPRO     -> "Tilera TILEPro"
  | EM_MICROBLAZE  -> "Xilinx MicroBlaze"
  | EM_TILEGX      -> "Tilera TILE-Gx"
  | EM_EXT x       -> string_of_int x

let string_of_p_type typ =
  match typ with
  | PT_NULL    -> "NULL"
  | PT_LOAD    -> "LOAD"
  | PT_DYNAMIC -> "DYNAMIC"
  | PT_INTERP  -> "INTERP"
  | PT_NOTE    -> "NOTE"
  | PT_SHLIB   -> "SHLIB"
  | PT_PHDR    -> "PHDR"
  | PT_OTHER x -> Printf.sprintf "0x%lx" x

let string_of_p_flag flag =
  match flag with
  | PF_X     -> "X"
  | PF_W     -> "W"
  | PF_R     -> "R"
  | PF_EXT x -> string_of_int x

let string_of_sh_type typ =
  match typ with
  | SHT_NULL     -> "NULL"
  | SHT_PROGBITS -> "PROGBITS"
  | SHT_SYMTAB   -> "SYMTAB"
  | SHT_STRTAB   -> "STRTAB"
  | SHT_RELA     -> "RELA"
  | SHT_HASH     -> "HASH"
  | SHT_DYNAMIC  -> "DYNAMIC"
  | SHT_NOTE     -> "NOTE"
  | SHT_NOBITS   -> "NOBITS"
  | SHT_REL      -> "REL"
  | SHT_SHLIB    -> "SHLIB"
  | SHT_DYNSYM   -> "DYNSYM"
  | SHT_EXT x    -> Printf.sprintf "0x%lx" x

let string_of_sh_flag flag =
  match flag with
  | SHF_WRITE     -> "W"
  | SHF_ALLOC     -> "A"
  | SHF_EXECINSTR -> "X"
  | SHF_EXT x     -> string_of_int x

let print_elf e =
  let open Printf in
  let () = print_endline "ELF Header:" in
  let () = printf "  Class:                         %s\n" (string_of_e_class e.e_class) in
  let () = printf "  Data:                          %s\n" (string_of_e_data e.e_data) in
  let () = printf "  Ident Version:                 %d\n" e.e_version in
  let () = printf "  OS/ABI:                        %s\n" (string_of_e_osabi e.e_osabi) in
  let () = printf "  ABI Version:                   %d\n" e.e_abiver in
  let () = printf "  Type:                          %s\n" (string_of_e_type e.e_type) in
  let () = printf "  Machine:                       %s\n" (string_of_e_machine e.e_machine) in
  let () = printf "  Entry point address:           0x%Lx\n" e.e_entry in
  let () = print_newline () in
  let () = print_endline "Section Headers:" in
  let () = printf "[Nr] Name                   Type       Addr             Size             ES Flags Lk Inf Al\n" in
  let print_shdr n s =
      let () = printf "[%2d] %-22s %-10s %016Lx %016Lx %2Ld %-5s %2ld %3ld %2Ld\n"
               n s.sh_name (string_of_sh_type s.sh_type) s.sh_addr s.sh_size s.sh_entsize
              (String.concat "" (List.map string_of_sh_flag s.sh_flags)) s.sh_link s.sh_info s.sh_addralign in
      n + 1
  in
  let () = ignore (List.fold_left print_shdr 0 e.e_sections) in
  let () = print_newline () in
  let () = print_endline "Program Headers:" in
  let () = printf " Type             VirtAddr         PhysAddr         MemSiz           Flg Align\n" in
  let print_phdr p =
    printf " %-16s %016Lx %016Lx %016Lx %-3s %016Lx\n" (string_of_p_type p.p_type) p.p_vaddr p.p_paddr p.p_memsz
    (String.concat "" (List.map string_of_p_flag p.p_flags)) p.p_align in
  List.iter print_phdr e.e_segments

let () =
  if Array.length Sys.argv <> 2 then
    prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <file>")
  else
    let f = open_in_bin Sys.argv.(1) in
    let buf = String.create (in_channel_length f) in
    let () = really_input f buf 0 (String.length buf) in
    let () = close_in f in
    let elf = Elf.parse buf in
    match elf with
    | None -> prerr_endline "parse failed"
    | Some elf -> print_elf elf
