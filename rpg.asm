!to "rpg.prg",cbm

;ver 1:loads charset and sprite data, initializes VIC, tests first sprite definitions

;=============================================================
;CONSTANTS
;=============================================================

;COLOURS
BLACK                           = $00
WHITE                           = $01
RED                             = $02
CYAN                            = $03
VIOLET                          = $04
GREEN                           = $05
BLUE                            = $06
YELLOW                          = $07
ORANGE                          = $08
BROWN                           = $09
LIGHT_RED                       = $0a
DARK_GREY                       = $0b
MIDDLE_GREY                     = $0c
LIGHT_GREEN                     = $0d
LIGHT_BLUE                      = $0e
LIGHT_GREY                      = $0f


;CPU
CIA_PROCESSOR_PORT              = $01       ;set ram/rom visibility
CIA_PRA                         = $dd00     ;set vic bank
JOYSTICK_2                      = $dc00
JOYSTICK_1                      = $dc01

;VIC CHIP

VIC_CONTROL_REGISTER_1          = $d011         ;raster top bit (#7), screen height, screen visibility
VIC_CONTROL_REGISTER_2          = $d016         ;screen width and multicolour mode
VIC_RASTER_REGISTER             = $d012
VIC_MEMORY_CONTROL              = $d018         ; bits #1-#3 pointer to char memory, #4-#7 pointer to sceen memory

VIC_IRQ_REGISTER                = $d019

VIC_SPRITE_ENABLE_REGISTER      = $d015
VIC_SPRITE_MULTICOLOR_REGISTER  = $d01c

VIC_BORDER_COLOR                = $d020
VIC_BACKGROUND_COLOR            = $d021
VIC_BGCOLOR_1                   = $d022
VIC_BGCOLOR_2                   = $d023
VIC_BGCOLOR_3                   = $d024

VIC_SPRITE_X_COORD              = $d000
VIC_SPRITE_Y_COORD              = $d001
VIC_SPRITE_X_EXTEND             = $d010

VIC_SPRITE_COLOR_BASE           = $d027
VIC_SPRITE_COLOR_1              = $d025
VIC_SPRITE_COLOR_2              = $d026

VIC_COLOR_RAM                   = $d800

;DATA
CHAR_DATA                       = $f000
SCREEN_CHAR_BUFFER              = $cc00

VARIABLE1                       = $03
VARIABLE2                       = $04
VARIABLE3                       = $05
VARIABLE4                       = $06
VARIABLE5                       = $07

MULTI1                          = $26           ;operand 1
MULTI2                          = $27           ;operand 2
MULTIL                          = $28           ;result low-byte
MULTIH                          = $29           ;result high-byte

ZEROPAGE_POINTER_1              = $17
ZEROPAGE_POINTER_2              = $19
ZEROPAGE_POINTER_3              = $1b
ZEROPAGE_POINTER_4              = $1d

NUMBER_OF_SPRITES               = $08
SPRITE_BASE                     = $40               ;64 in decimal
SPRITE_END                      = SPRITE_BASE + NUMBER_OF_SPRITES
SPRITE_POINTER_BASE             = SCREEN_CHAR_BUFFER + $3f8     ;1016 bytes after screen buffer
SPRITE_DATA_BUFFER              = $d000             ;put sprite definitions here



;=============================================================
;   INITIALIZATION
;=============================================================

        * = $0801

        ;Autostart - SYS2064
        !byte $0c, $08, $0a, $00, $9e, $20, $32, $30, $36, $34, $00, $00, $00, $00, $00

        ;initialize registers
        lda #$00
        sta VIC_SPRITE_ENABLE_REGISTER      ;disable sprites

        sta VIC_BORDER_COLOR                ;black
        sta VIC_BACKGROUND_COLOR

        ;set VIC bank
        lda CIA_PRA
        and #%11111100                      ;select VIC bank #3 - $c000 - $ffff
        sta CIA_PRA

        ;set charset and screen buffer locations
        lda #%00111100
        sta VIC_MEMORY_CONTROL              ;char data at $f000, screen buffer at $cc00


        ;===============================
        ;copy charset data to target
        ;===============================

        sei             ;turn off registers, we'll be writing under ROMs

        lda CIA_PROCESSOR_PORT      ;$01
        sta VARIABLE1               ;store default config for later

        lda #%00110000              ;turn off ROMs
        sta CIA_PROCESSOR_PORT

        lda #<CHARSET_DATA
        sta ZEROPAGE_POINTER_1
        lda #>CHARSET_DATA
        sta ZEROPAGE_POINTER_1 + 1  ;set source pointer

        jsr CopyCharsetData         ;copy the raw data to it's destination in RAM


        ;==============================
        ;set palette data for charset
        ;==============================

        ;TODO this will set up the relevent colours of each char
        ;a map for the charset will be made for colours instead of char defs
        ;these can then be read and copied to $d800 at the same time as the screen buffer is set up


        ;==============================
        ;copy sprite data to target
        ;==============================

        lda #<SPRITE_DATA
        sta ZEROPAGE_POINTER_1
        lda #>SPRITE_DATA
        sta ZEROPAGE_POINTER_1 + 1  ;set source pointer

        jsr CopySpriteData

        lda VARIABLE1
        sta CIA_PROCESSOR_PORT      ;restore default ROM visibility

        cli                         ;turn IRQs back on

        jsr ClearScreen             ;clear the screen

        jsr TEST_SpriteData

;==================================================================
;       MAIN LOOP
;==================================================================
!zone GameLoop
GameLoop
        inc VIC_BORDER_COLOR           ;flash the border

        jsr WaitFrame

        jmp GameLoop

;==================================================================
;       INITIALISATION FUNCTIONS
;==================================================================

;====================================
;Copy the Charset
;expects source at ZEROPAGE_POINTER_1
;====================================     

!zone CopyCharsetData
CopyCharsetData
        
        lda #<CHAR_DATA
        sta ZEROPAGE_POINTER_2
        lda #>CHAR_DATA
        sta ZEROPAGE_POINTER_2 + 1  ;set destination pointer

        ldx #$00                    ;counts number of bytes per char written
        ldy #$00                    ;counts number of bytes total written
        sty VARIABLE2               ;counts how many characters have been copied (VARIBLE1 is in use at this point)

.CopyChar
        lda (ZEROPAGE_POINTER_1),y
        sta (ZEROPAGE_POINTER_2),y
        inx
        iny
        cpx #$08
        bne .CopyChar

        cpy #$00                    ;if y is 0, 256 bytes have been copied and we need to adjust the pointers
        bne .PageBoundaryNotReached

        inc ZEROPAGE_POINTER_1 + 1
        inc ZEROPAGE_POINTER_2 + 1

.PageBoundaryNotReached
        inc VARIABLE2               ;another character has been copied
        beq .CopyFinished           ;if VARIABLE2 is 0, then 256 characters have been copied and we're done

        ldx #$00
        jmp .CopyChar               ;reset x for the next char

.CopyFinished
        rts

;==========================================
;CopySpriteData
;expects source data at ZEROPAGE_POINTER_1
;==========================================

!zone CopySpriteData
CopySpriteData
        ldx #$00        ;counts to 64 to check if a sprite has copied
        ldy #$00        ;counts all bytes written to check page boundary
        lda #NUMBER_OF_SPRITES
        sta VARIABLE2   ;counts number of sprites written (VARIABLE1 is in use)

        lda #<SPRITE_DATA_BUFFER
        sta ZEROPAGE_POINTER_2
        lda #>SPRITE_DATA_BUFFER
        sta ZEROPAGE_POINTER_2 + 1

.CopyLoop
        lda (ZEROPAGE_POINTER_1),y
        sta (ZEROPAGE_POINTER_2),y
        iny
        inx
        cpx #$40
        bne .CopyLoop

        dec VARIABLE2
        beq .CopyDone

        cpy #$00
        bne .PageBoundaryNotReached

        inc ZEROPAGE_POINTER_1 + 1
        inc ZEROPAGE_POINTER_2 + 1

.PageBoundaryNotReached
        ldx #$00
        jmp .CopyLoop

.CopyDone
        rts


;==========================================
;ClearScreen
;sets all chars in the screen buffer to '0'
;==========================================

!zone ClearScreen
ClearScreen
        ldx #$00
        lda #$00            ;a blank space
.CopyLoop
        sta SCREEN_CHAR_BUFFER,x
        sta SCREEN_CHAR_BUFFER + $100,x
        sta SCREEN_CHAR_BUFFER + $200,x
        sta SCREEN_CHAR_BUFFER + $2e8,x
        inx 
        bne .CopyLoop

        rts



;==================================================================
;       SYSTEM FUNCTIONS
;==================================================================

;===========================
;WaitFrame
;ensures stable timing
;===========================

!zone WaitFrame
WaitFrame
        lda $d012      ;check current raster line
        cmp #$ff        ;check for desired raster break
        beq .WaitStep2  ;don't return too soon if raster happens to be on desired line

.WaitStep2
        lda $d012
        cmp #$ff
        bne .WaitStep2

        rts



;==================================================================
;       TEST FUNCTIONS
;==================================================================
!zone TEST_SpriteData
TEST_SpriteData
  
        ldx #$ff
        stx VIC_SPRITE_ENABLE_REGISTER
        stx VIC_SPRITE_MULTICOLOR_REGISTER

        stx $d017       ;sprites double height
        stx $d01d       ;sprites double width

        ldx #$64        ;100 decimal, all sprites shown at this Y-coord
        stx VIC_SPRITE_Y_COORD
        stx VIC_SPRITE_Y_COORD + 2
        stx VIC_SPRITE_Y_COORD + 4
        stx VIC_SPRITE_Y_COORD + 6
        stx VIC_SPRITE_Y_COORD + 8
        stx VIC_SPRITE_Y_COORD + 10
        stx VIC_SPRITE_Y_COORD + 12
        stx VIC_SPRITE_Y_COORD + 14

        ldx #$20
        stx VIC_SPRITE_X_COORD
        ldx #$40
        stx VIC_SPRITE_X_COORD + 2
        ldx #$60
        stx VIC_SPRITE_X_COORD + 4
        ldx #$80
        stx VIC_SPRITE_X_COORD + 6
        ldx #$a0
        stx VIC_SPRITE_X_COORD + 8
        ldx #$c0
        stx VIC_SPRITE_X_COORD + 10
        ldx #$e0
        stx VIC_SPRITE_X_COORD + 12
        ldx #$ff
        stx VIC_SPRITE_X_COORD + 14

        ldx #$02            ;RED
        stx VIC_SPRITE_COLOR_BASE
        inx                 ;CYAN
        stx VIC_SPRITE_COLOR_BASE + 1
        inx                 ;VIOLET
        stx VIC_SPRITE_COLOR_BASE + 2
        inx                 ;GREEN
        stx VIC_SPRITE_COLOR_BASE + 3
        inx                 ;BLUE
        stx VIC_SPRITE_COLOR_BASE + 4
        ldx #$02            ;RED
        stx VIC_SPRITE_COLOR_BASE + 5
        inx                 ;CYAN
        stx VIC_SPRITE_COLOR_BASE + 6
        inx                 ;BROWN
        stx VIC_SPRITE_COLOR_BASE + 7

        ldx #ORANGE
        stx VIC_SPRITE_COLOR_1
        ldx #YELLOW
        stx VIC_SPRITE_COLOR_2

  
        ldx #SPRITE_BASE
        ldy #$00

.TestLoop
        txa
        sta SPRITE_POINTER_BASE,y
        inx
        iny
        cpy #$08        ;number of sprites so far
        bne .TestLoop

.TestSpriteDone
        rts
        


;==================================================================
;       DATA
;==================================================================

CHARSET_DATA
        !binary "multichar.chr"

SPRITE_DATA
        !binary "graphics/sprites.bin"
