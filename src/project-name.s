.include "constants.inc"
.include "header.inc"

.feature force_range

; famitone2 config
FT_PAL_SUPPORT=0
FT_NTSC_SUPPORT=1
FT_PITCH_FIX=0
FT_SFX_ENABLE=1
FT_THREAD=1
FT_DPCM_ENABLE=1
FT_SFX_STREAMS=4
FT_DPCM_OFF= $c000

; music/sfx constants
; MUSIC_TRACK_1 = 0

; SFX_SOME_SFX = 0

; game config

.segment "ZEROPAGE"
FT_TEMP: .res 3
.segment "FAMITONE"
FT_BASE_ADR: .res 186

.segment "CODE"
.include "famitone2.s"

.segment "OAM"
.struct Sprite
  ycoord .byte
  tile .byte
  flag .byte
  xcoord .byte
.endstruct

all_sprites:
  .repeat 64
    .tag Sprite
  .endrepeat

.zeropage

.enum game_states
  playing = 0
.endenum

.importzp rng_seed
.importzp buttons
.importzp last_frame_buttons
.importzp released_buttons
.importzp pressed_buttons
.importzp rle_ptr

addr_ptr: .res 2 ; generic address pointer
ppu_addr_ptr: .res 2 ; temporary address for PPU_ADDR
nmis: .res 1
old_nmis: .res 1
args: .res 5
game_state: .res 1
current_level: .res 1

.segment "BSS"
; non-zp RAM goes here

.segment "CODE"

.import reset_handler
.import readjoy
.import rand
.import unrle

; .import music_data

.macro KIL ; pseudo instruction to kill the program
  .byte $12
.endmacro

.macro save_regs
  PHA
  TXA
  PHA
  TYA
  PHA
.endmacro

.macro restore_regs
  PLA
  TAY
  PLA
  TAX
  PLA
.endmacro

.macro copy_word_to_pointer word, pointer
  LDA #<word
  STA pointer
  LDA #>word
  STA pointer + 1
.endmacro

.macro add_byte_to_word byte, word
  .local skip
  CLC
  LDA byte
  ADC word
  STA word
  BCC skip
  INC word+1
skip:
.endmacro

.proc irq_handler
  RTI
.endproc

.proc nmi_handler
  save_regs

  ; Fix Scroll
  LDA PPUSTATUS
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  LDA #$00 ; horizontal scroll
  STA PPUSCROLL
  STA PPUSCROLL

  ; Refresh OAM
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
  restore_regs

  INC nmis
  RTI
.endproc

.export main
.proc main
  SEI         ; ignore IRQs
  CLD         ; disable decimal mode
  LDX #$40
  STX $4017   ; disable APU frame IRQ
  LDX #$ff
  TXS         ; Set up stack
  INX         ; now X = 0
  STX PPUCTRL ; disable NMI
  STX PPUMASK ; disable rendering
  STX $4010   ; disable DMC IRQs

  LDX #0
clear_ram:
  LDA #$00
  STA $0000,X
  STA $0100,X
  STA $0300,X
  STA $0400,X
  STA $0500,X
  STA $0600,X
  STA $0700,X
  LDA #$fe
  STA $0200,X
  INX
  BNE clear_ram

  ; load palettes
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR
load_palettes:
  LDA palettes,X
  STA PPUDATA
  INX
  CPX #$20
  BNE load_palettes

  LDA #$23
  STA rng_seed
  LDA #$C1
  STA rng_seed+1

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  ; LDX #<some_music_data
  ; LDY #>some_music_data
  ; LDA #1
  ; JSR FamiToneInit

  ; LDX #<some_sfx_data
  ; LDY #>some_sfx_data
  ; LDA #1
  ; JSR FamiToneSfxInit

  LDA #$00
  STA current_level
  JSR load_level

forever:
  LDA nmis
  CMP old_nmis
  BEQ etc
  STA old_nmis
  ; new frame code
  ; JSR rand
  JSR game_state_handler
  ; JSR FamiToneUpdate

etc:
  JMP forever
.endproc

.proc load_level
  BIT PPUSTATUS
  LDA #%00010000  ; turn of NMIs
  STA PPUCTRL
  LDA #%00000000  ; turn off screen
  STA PPUMASK

  ; clear sprite ram
  LDX #0
  LDA #$fe
:
  STA $0200,X
  INX
  BNE :-

  ; read bg rle pointer and uncompress it
  LDA current_level
  ASL
  TAX
  LDA level_pointers,X
  STA addr_ptr
  LDA level_pointers+1,X
  STA addr_ptr+1

  LDA (addr_ptr),Y
  INY
  STA rle_ptr
  LDA (addr_ptr),Y
  INY
  STA rle_ptr+1
  save_regs
:  ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL :-
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  JSR unrle
  restore_regs

vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  BIT PPUSTATUS
  LDA #%10010000  ; turn of NMIs
  STA PPUCTRL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

  RTS
.endproc

.proc player_input
  JSR readjoy
  RTS
.endproc

.proc DEBUG_start
  LDA #%00010000
  STA PPUCTRL

  BIT PPUSTATUS
  LDA #%00000000  ; turn off screen
  STA PPUMASK

  LDA PPUSTATUS
  LDA #$29
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  RTS
.endproc

.proc DEBUG_end
vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  BIT PPUSTATUS
  LDA #%00011110  ; turn on screen
  STA PPUMASK
  LDA #$20
  STA PPUADDR
  LDA #$00
  STA PPUADDR
  RTS
.endproc

.proc DEBUG_print_byte
  PHA
  AND #%11110000
  .repeat 4
  LSR
  .endrepeat
  JSR DEBUG_print_hex
  PLA
  AND #%00001111
  JSR DEBUG_print_hex
  RTS
.endproc

.proc DEBUG_print_hex
  CMP #$0A
  BCS :+
  CLC
  ADC #$10
  STA PPUDATA
  RTS
:
  ADC #$36
  STA PPUDATA
  RTS
.endproc

.proc game_state_handler
  ; Uses RTS Trick
  LDX game_state
  LDA game_state_handlers+1, X
  PHA
  LDA game_state_handlers, X
  PHA
  RTS
.endproc

.proc game_state_playing
  JSR player_input
  RTS
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"

game_state_handlers:
  .word game_state_playing-1

palettes:
.incbin "../assets/bg-palettes.pal"
.incbin "../assets/sprite-palettes.pal"

;sprites:
;.include "../assets/metasprites.s"

level_pointers:
        .word level_0_data

        ; level data format:
        ; pointer to rle bg nametable
level_0_data:
        .word nametable_level_0

nametable_level_0: .incbin "../assets/level/level-0.rle"

; music and sfx data
;.include "../assets/music/some-music.s"
;.include "../assets/music/some-sfx.s"

.segment "CHR"
.incbin "../assets/graphics.chr"
