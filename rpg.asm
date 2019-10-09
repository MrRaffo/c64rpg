!to "rpg.prg",cbm

;ver 1: loads charset and sprite data, initializes VIC, tests first sprite definitions
;ver 2: added simple joystick controls for player sprite and tests charset

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

NUMBER_OF_SPRITES               = $05
SPRITE_BASE                     = $40               ;64 in decimal
SPRITE_END                      = SPRITE_BASE + NUMBER_OF_SPRITES
SPRITE_POINTER_BASE             = SCREEN_CHAR_BUFFER + $3f8     ;1016 bytes after screen buffer
SPRITE_DATA_BUFFER              = $d000             ;put sprite definitions here

;LEVEL CONSTANTS
LEVEL_TILE_WIDTH                = 20                ;tiles are 2x2
LEVEL_TILE_HEIGHT               = 11

TILE_SIDE                       = 16                ;size of a tile in pixels (length of a side)
TILE_HALF_SIDE                  = 8

SPRITE_SIZE                     = 16
SPRITE_HALF_SIZE                = 8

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

;=================================
;INITIALIZE SPRITE FOR TESTING
;=================================


        lda #$40                    ;64 decimal
        sta SPRITE_X_POS
        sta SPRITE_Y_POS            

        sta SPRITE_POINTER_BASE

        lda #$01
        sta VIC_SPRITE_ENABLE_REGISTER
        sta VIC_SPRITE_MULTICOLOR_REGISTER

        lda #ORANGE
        sta VIC_SPRITE_COLOR_1

        lda #YELLOW
        sta VIC_SPRITE_COLOR_2

        lda #RED
        sta VIC_SPRITE_COLOR_BASE

;==================================
;INITIALIZE TILE DATA FOR TESTING
;==================================

        lda VIC_CONTROL_REGISTER_2
        ora #%00010000                  ;text multicolor mode
        sta VIC_CONTROL_REGISTER_2

        lda #BROWN
        sta VIC_BGCOLOR_1

        lda #ORANGE
        sta VIC_BGCOLOR_2

        lda #$0a
        tay

        ldx #$00

TestChar
        txa
        sta SCREEN_CHAR_BUFFER,x
        tya
        sta VIC_COLOR_RAM,x
        inx
        bne TestChar
        

;==================================================================
;       MAIN LOOP
;==================================================================
!zone GameLoop
GameLoop
        inc VIC_BORDER_COLOR           ;flash the border

        jsr ReadJoystick

        jsr UpdateSpritePositions      ;moves sprite software stored positions to hardware position registers, deals with extended X bit

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
;       CONTROL AND MOVEMENT FUNCTIONS
;==================================================================

;=========================
;ReadJoystick
;get js status and branch
;=========================

!zone ReadJoystick
ReadJoystick
        lda #$01
        bit JOYSTICK_2
        bne .UpNotPressed
        jsr PlayerMoveUp

.UpNotPressed
        lda #$02
        bit JOYSTICK_2
        bne .DownNotPressed
        jsr PlayerMoveDown

.DownNotPressed
        lda #$04
        bit JOYSTICK_2
        bne .LeftNotPressed
        jsr PlayerMoveLeft

.LeftNotPressed
        lda #$08
        bit JOYSTICK_2
        bne .RightNotPressed
        jsr PlayerMoveRight

.RightNotPressed
        lda #$10
        bit JOYSTICK_2
        bne .ButtonNotPressed
        jsr PlayerButton

.ButtonNotPressed
        rts

;===============================
;PlayerMoveUp
;will include code to check if move is possible later
;===============================
!zone PlayerMoveUp
PlayerMoveUp
        ldx #$00
        jsr MoveSpriteUp
        rts

!zone PlayerMoveDown
PlayerMoveDown
        ldx #$00
        jsr MoveSpriteDown
        rts

!zone PlayerMoveLeft
PlayerMoveLeft
        ldx #$00
        jsr MoveSpriteLeft
        rts

!zone PlayerMoveRight
PlayerMoveRight
        ldx #$00
        jsr MoveSpriteRight
        rts

!zone PlayerButton
PlayerButton
        ldx #$00
        jsr PlayerAction
        rts

;=====================================
;MoveSpriteUp
;expects x as sprite index
;=====================================

!zone MoveSpriteUp
MoveSpriteUp
        dec SPRITE_Y_POS,x
        rts

MoveSpriteDown
        inc SPRITE_Y_POS,x
        rts

MoveSpriteLeft
        dec SPRITE_X_POS,x
        rts

MoveSpriteRight
        inc SPRITE_X_POS,x
        rts

PlayerAction
        inc VIC_SPRITE_COLOR_BASE,x
        rts

;====================================
;UpdateSpritePositions
;moves sprite pos from ram to vic reg
;====================================

!zone UpdateSpritePositions
UpdateSpritePositions
        ldx #$00            ;index
.UpdateLoop

        txa
        asl                 ;sprite positions for x and y coords are interlaced, this gets the right index for the hw coord registers
        tay                 

        lda SPRITE_Y_POS,x
        sta VIC_SPRITE_Y_COORD,y        ;Y coord does not need to be modified

        lda SPRITE_X_POS,x
        asl                 ;effectively doubles position, high bit now in carry
        sta VIC_SPRITE_X_COORD,y            
        ror VIC_SPRITE_X_EXTEND
        inx
        cpx #$08
        bne .UpdateLoop

        rts
        
;==================================================================
;       LEVEL FUNCTIONS
;==================================================================
!zone DrawTile

;==================================================================
;       MATHS FUNCTIONS
;==================================================================

;=====================================
;Multiply
;takes operands in MULTI1 and MULTI2
;preferably put lower number in MULTI1
;result stored in MULTIL and MULTIH
;=====================================

!zone Multiply
Multiply
        ldx #$00
        stx MULTIL
        stx MULTIH
        txa             ;zero result and accumulator in prep

        ldx MULTI2
        beq .MultiplyDone
        ldx MULTI1
        beq .MultiplyDone       ;if either operand is 0, return immediately

        clc                     ;otherwise, clear the carry before beginning

.MultiplyLoop
        adc MULTI2              ;accumulator has been zeroed, continue adding MULTI2 MULTI1 times
        bcc .NoOverflow

        inc MULTIH              ;increment the high byte if accumulator overflows

.NoOverflow
        dex
        bne .MultiplyLoop       ;if x is zero, we've finished

.MultiplyDone
        sta MULTIL              ;accumulator holds low byte of the result, high byte is set during routine
        rts

;==================================================================
;       TEST FUNCTIONS
;==================================================================


;==================================================================
;       DATA
;==================================================================

SPRITE_X_POS
        !byte $00, $00, $00, $00, $00, $00, $00, $00

SPRITE_Y_POS
        !byte $00, $00, $00, $00, $00, $00, $00, $00

CHARSET_DATA
        !binary "graphics/rpgset1.chr"

SPRITE_DATA
        !binary "graphics/sprites.spr"
