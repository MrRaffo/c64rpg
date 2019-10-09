!to "rpg.prg",cbm

;ver 1: loads charset and sprite data, initializes VIC, tests first sprite definitions
;ver 2: added simple joystick controls for player sprite
;ver 3: added ability to draw a tile to any coord on screen, with colour data
;		added level drawing routine and test screen data
;ver 4: added animations and basic keyboard reading - pressing 1 to 4 will choose a different animation for player sprite
;ver 5: (b) added better animations and changed movement to 1 pixel per frame and one of four directions at a time

;ver 6: complete rewrite

;================================================
;CONSTANTS
;================================================

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

ZEROPAGE_POINTER_1              = $17
ZEROPAGE_POINTER_2              = $19
ZEROPAGE_POINTER_3              = $1b
ZEROPAGE_POINTER_4              = $1d
ZEROPAGE_POINTER_5				= $1f
ZEROPAGE_POINTER_6				= $21
ZEROPAGE_POINTER_7				= $23

MULTI1                          = $26           ;operand 1
MULTI2                          = $27           ;operand 2
MULTIL                          = $28           ;result low-byte
MULTIH                          = $29           ;result high-byte

NUMBER_OF_SPRITES               = $20
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

;DIRECTION
DIRECTION_UP					= 1
DIRECTION_DOWN					= 2
DIRECTION_LEFT					= 3
DIRECTION_RIGHT					= 4

;ACTION INDICES
ACTION_PLAYER_STAND_UP			= 1
ACTION_PLAYER_STAND_DOWN		= 2
ACTION_PLAYER_STAND_LEFT		= 3
ACTION_PLAYER_STAND_RIGHT		= 4
ACTION_PLAYER_WALK_UP			= 5
ACTION_PLAYER_WALK_DOWN			= 6
ACTION_PLAYER_WALK_LEFT			= 7
ACTION_PLAYER_WALK_RIGHT		= 8
ACTION_PLAYER_TALK				= 9
ACTION_PLAYER_DANCE				= 10


;ANIMATION INDICES
ANIMATION_PLAYER_STAND_UP		= 1
ANIMATION_PLAYER_STAND_DOWN		= 2
ANIMATION_PLAYER_STAND_LEFT		= 3
ANIMATION_PLAYER_STAND_RIGHT 	= 4
ANIMATION_PLAYER_WALK_UP		= 5
ANIMATION_PLAYER_WALK_DOWN		= 6
ANIMATION_PLAYER_WALK_LEFT		= 7
ANIMATION_PLAYER_WALK_RIGHT		= 8
ANIMATION_PLAYER_TALK			= 9
ANIMATION_PLAYER_DANCE			= 10


;=========================================================
;	INITIALIZATION
;=========================================================

		* = $0801
		
		;Autostart - SYS2064 in BASIC
		!byte $0c, $08, $0a, $00, $9e, $20, $32, $30, $36, $34, $00, $00, $00, $00, $00
		
		
		;initialize registers
		lda #$00
		sta VIC_SPRITE_ENABLE_REGISTER		;disable sprites
		
		sta VIC_BACKGROUND_COLOR			;black
		lda #GREEN
		sta VIC_BORDER_COLOR
		
		;set VIC bank
		lda CIA_PRA							;($dd00)
		and #%11111100						;set VIC bank #3 ($c000 - $ffff)
		sta CIA_PRA
		
		;set charset and screen buffer locations
		lda #%00111100
		sta VIC_MEMORY_CONTROL				;screen buffer at $cc00, char data at $f000
		
		
		;============================
		;copy charset data to target
		;============================
		
		sei									;set interrupts off - writing under ROMs
		
		lda CIA_PROCESSOR_PORT 				;($01) Controls ROM visibility
		sta VARIABLE1
		
		lda #%00110000						;disable ROMs (bottom 3 bits)
		sta CIA_PROCESSOR_PORT
		
		lda #<CHARSET_DATA
		sta ZEROPAGE_POINTER_1
		lda #>CHARSET_DATA
		sta ZEROPAGE_POINTER_1 + 1			;set source of data
		
		jsr CopyCharsetData					;copy data to RAM ($f000)
		
		;============================
		;copy sprite data to target
		;============================
		
		lda #<SPRITE_DATA
		sta ZEROPAGE_POINTER_1
		lda #>SPRITE_DATA
		sta ZEROPAGE_POINTER_1 + 1
		
		jsr CopySpriteData
		
		;=============================
		;restore ROMs
		;=============================
		
		lda VARIABLE1
		sta CIA_PROCESSOR_PORT
		
		cli									;turn IRQs back on
		
		;=============================
		;prepare screen
		;=============================
		
		jsr ClearScreen						;clear the screen
		lda VIC_CONTROL_REGISTER_2
		ora #%00010000						;enable multicolor mode
		sta VIC_CONTROL_REGISTER_2
		
		;============================
		;testing
		;============================
		
		;jsr TEST_Sprites
		;jsr TEST_Chars
		;jsr TEST_DrawTile
		
		;setup for testing purposes - should be screen specific later
		lda #BROWN
		sta VIC_BGCOLOR_1
		lda #MIDDLE_GREY
		sta VIC_BGCOLOR_2
		
		
		ldx #$00							;LEVEL index
		jsr LEVEL_LoadData
		jsr LEVEL_DrawScreen
		
;====================================================================
;	MAIN GAME LOOP
;====================================================================
		
!zone GameLoop
GameLoop
		jsr TEST_FrameTimer					;flash border

        jsr TEST_LevelSwitch                ;checks button, changes screen data on press
		
		jsr WaitFrame						;wait until frame is finished
		
		jmp GameLoop
		
;====================================================================
;	SYSTEM FUNCTIONS
;====================================================================

;==========================
;WaitFrame
;locks code to the VIC chip refresh
;waits until a frame has been drawn before moving on
;==========================

!zone WaitFrame
WaitFrame
		lda VIC_RASTER_REGISTER				;($d012)
		cmp #$ff
		beq .WaitStep2
		
.WaitStep2
		lda VIC_RASTER_REGISTER
		cmp #$ff
		bne .WaitStep2
		
		rts
		
;=========================
;CopyCharsetData
;copies charset to RAM from source in ZP_1
;=========================

!zone CopyCharsetData
CopyCharsetData
		
		lda #<CHAR_DATA
		sta ZEROPAGE_POINTER_2
		lda #>CHAR_DATA
		sta ZEROPAGE_POINTER_2 + 1			;set destination pointer
		
		ldx #$00							;counts bytes per char written
		ldy #$00							;counts total bytes written
		sty VARIABLE2						;counts completed chars written (VARIABLE1 is in use)
		
.CopyChar
		lda (ZEROPAGE_POINTER_1),y
		sta (ZEROPAGE_POINTER_2),y
		inx
		iny
		cpx #$08							;check if char is finished
		bne .CopyChar
		
		cpy #$00							;check if 256 bytes have been copied
		bne .PageBoundaryNotReached
		
		
		inc ZEROPAGE_POINTER_1 + 1			;update high bytes of pointers if need be
		inc ZEROPAGE_POINTER_2 + 1
		
.PageBoundaryNotReached
		inc VARIABLE2						;another char has been copied
		beq .CopyFinished					;if VARIABLE2 is 0, 256 chars, ie the complete charset, is copied
		
		ldx #$00
		jmp .CopyChar						;reset x for the next char
		
.CopyFinished
		rts
		
;===============================
;CopySpriteData
;copies raw sprite data to VIC bank from source at ZP_1
;===============================

!zone CopySpriteData
CopySpriteData

		lda #<SPRITE_DATA_BUFFER
		sta ZEROPAGE_POINTER_2
		lda #>SPRITE_DATA_BUFFER
		sta ZEROPAGE_POINTER_2 + 1
		
		ldx #NUMBER_OF_SPRITES				;counter
		stx VARIABLE2						;VARIABLE1 in use
		ldx #$00
		ldy #$00
		
.CopyLoop
		lda (ZEROPAGE_POINTER_1),y
		sta (ZEROPAGE_POINTER_2),y
		iny
		inx
		cpx #$40							;64 decimal, 1 sprite
		bne .CopyLoop
		
		dec VARIABLE2
		beq .CopyDone
		
		ldx #$00							;prepare counter for next sprite
		
		cpy #$00							;check if page boundary has been reached
		bne	.CopyLoop
		
		inc ZEROPAGE_POINTER_1 + 1			;if so, increment pointer high bytes
		inc ZEROPAGE_POINTER_2 + 1			
		
		jmp .CopyLoop
		
.CopyDone
		rts
		
;================================
;ClearScreen
;clears the screen, setting all chars in buffer to '0', which in my tileset will be blank
;================================

!zone ClearScreen
ClearScreen
		ldx #$00
		lda #$00							;blank space
		
.CopyLoop
		sta SCREEN_CHAR_BUFFER,x
		sta SCREEN_CHAR_BUFFER + 256, x
		sta SCREEN_CHAR_BUFFER + 512, x
		sta SCREEN_CHAR_BUFFER + 744, x
		inx
		bne .CopyLoop
		
		rts

;=========================================================================
;	LEVEL LOADING FUNCTIONS
;=========================================================================

;==========================
;LEVEL_LoadData
;load screen data from raw memory to the current screen buffers
;screen index should be stored in x (or a 'current_level' flag later)
;===========================

!zone LEVEL_LoadData						;TODO
LEVEL_LoadData
		txa
		asl
		tax									;pointers are 2 bytes, double it to get offset

		lda DATA_LEVEL_POINTERS,x
		sta ZEROPAGE_POINTER_7
		lda DATA_LEVEL_POINTERS + 1,x
		sta ZEROPAGE_POINTER_7 + 1			;pointer to start of data pointers
		
		ldy #$00
		lda (ZEROPAGE_POINTER_7),y
		sta ZEROPAGE_POINTER_1
		iny
		lda (ZEROPAGE_POINTER_7),y
		sta ZEROPAGE_POINTER_1 + 1			;start of tile data
		iny
		lda (ZEROPAGE_POINTER_7),y
		sta ZEROPAGE_POINTER_2
		iny
		lda (ZEROPAGE_POINTER_7),y
		sta ZEROPAGE_POINTER_2 + 1			;start of palette data
		iny
		lda (ZEROPAGE_POINTER_7),y
		sta ZEROPAGE_POINTER_3
		iny
		lda (ZEROPAGE_POINTER_7),y
		sta ZEROPAGE_POINTER_3 + 1			;start of tile flag data
		
		lda #<CURRENT_SCREEN_TILE_DATA
		sta ZEROPAGE_POINTER_4
		lda #>CURRENT_SCREEN_TILE_DATA
		sta ZEROPAGE_POINTER_4 + 1
		
		lda #<CURRENT_SCREEN_PALETTE_DATA
		sta ZEROPAGE_POINTER_5
		lda #>CURRENT_SCREEN_PALETTE_DATA
		sta ZEROPAGE_POINTER_5 + 1
		
		lda #<CURRENT_SCREEN_ATTRIBUTE_DATA
		sta ZEROPAGE_POINTER_6
		lda #>CURRENT_SCREEN_ATTRIBUTE_DATA
		sta ZEROPAGE_POINTER_6 + 1			;destination pointers set
		
		ldy #$00
		
.LoadData
		lda (ZEROPAGE_POINTER_1),y
		sta (ZEROPAGE_POINTER_4),y			;copy tile data
		
		lda (ZEROPAGE_POINTER_2),y
		sta (ZEROPAGE_POINTER_5),y			;copy palette data
		
		;lda (ZEROPAGE_POINTER_3),y
		;sta (ZEROPAGE_POINTER_6),y			;copy attribute data
		
		iny
		cpy #$dc							;220 in decimal, number of tiles
		bne .LoadData
		
		rts									;use linux program convert.c to set a default palette

;==========================
;LEVEL_DrawScreen
;draws a full screen of data, uses LEVEL_DrawTile
;assumes data is in CURRENT_SCREEN... buffers
;==========================

!zone LEVEL_DrawScreen						;TODO
LEVEL_DrawScreen

		ldx #$00							
		stx VARIABLE1						;column counter
		stx VARIABLE2						;row counter
		stx VARIABLE5						;total counter
		
		
.DrawLoop
		ldx VARIABLE5
		lda CURRENT_SCREEN_TILE_DATA,x
		sta VARIABLE3
		lda CURRENT_SCREEN_PALETTE_DATA,x
		sta VARIABLE4
		
		jsr LEVEL_DrawTile
		
		inc VARIABLE5
		ldy VARIABLE1
		iny
		cpy #$14							;20, check if a row has been fully drawn
		bne .NoRowIncrement
		
		ldy #$00
		inc VARIABLE2
		ldx VARIABLE2
		cpx #$0b							;11 in decimal, check if all rows are drawn
		beq .DrawFinished
		
.NoRowIncrement
		sty VARIABLE1
		jmp .DrawLoop
		 
.DrawFinished
		rts

;==========================
;LEVEL_DrawTile
;draws a tile to coords given in VARIABLE1 and VARIABLE2 (x, y)
;tile type in VARIABLE3 and tile palette in VARIABLE4
;==========================

!zone LEVEL_DrawTile
LEVEL_DrawTile
		ldy VARIABLE2						;get row index
		lda SCREEN_ROW_LOW_BYTE,y			
		sta ZEROPAGE_POINTER_1
		sta ZEROPAGE_POINTER_2				;pointer 2 will write to colour ram, same offset as char buffer
		lda SCREEN_ROW_HIGH_BYTE,y			;column index
		sta ZEROPAGE_POINTER_1 + 1			;ZP1 now points to upper left hand corner of the first tile in the 
											;row given by VARIABLE2
		
		clc
		adc #( ( VIC_COLOR_RAM - SCREEN_CHAR_BUFFER ) & 0xff00 ) >> 8	;get high byte of color ram for tile
		sta ZEROPAGE_POINTER_2 + 1
		
		lda VARIABLE1
		asl 								;VARIABLE1 contains X-coord, double this to get actual char index
		tay									;(tiles are 2x2)
		
		lda VARIABLE3
		asl
		asl									;4 chars make a tile, stored linearly. Multiply index by 4 to get
											;to the start of the tile data
											
		tax
		
		;==========================
		;write top 2 chars of tile
		;==========================
		
		sta (ZEROPAGE_POINTER_1),y
		lda VARIABLE4
		sta (ZEROPAGE_POINTER_2),y
		inx
		iny
		txa
		sta (ZEROPAGE_POINTER_1),y
		lda VARIABLE4
		sta (ZEROPAGE_POINTER_2),y
		
		;==============================
		;set pointer to bottom 2 chars
		;==============================
		
		tya
		clc
		adc #$27							;39 in decimal, skip a line
		tay
		
		;==============================
		;write data for bottom 2 chars
		;==============================
		
		inx
		txa
		sta (ZEROPAGE_POINTER_1),y
		lda VARIABLE4
		sta (ZEROPAGE_POINTER_2),y
		inx
		iny
		txa
		sta (ZEROPAGE_POINTER_1),y
		lda VARIABLE4
		sta (ZEROPAGE_POINTER_2),y

		rts
		
;=========================================================================
;	TEST FUNCTIONS
;=========================================================================

;==========================
;TEST_Chars
;==========================

!zone TEST_Chars
TEST_Chars
		ldx #$00
		
.TestLoop
		txa
		sta SCREEN_CHAR_BUFFER,x
		inx
		bne .TestLoop
		
		rts

;==========================
;TEST_Sprites
;check sprites have loaded
;==========================

!zone TEST_Sprites
TEST_Sprites
		lda #$01
		sta VIC_SPRITE_ENABLE_REGISTER
		sta VIC_SPRITE_MULTICOLOR_REGISTER
		
		lda #$40
		sta SPRITE_POINTER_BASE
		
		lda #GREEN
		sta VIC_SPRITE_COLOR_BASE
	
		lda #YELLOW
		sta VIC_SPRITE_COLOR_2
		
		lda #ORANGE
		sta VIC_SPRITE_COLOR_1
		
		lda #$b0			;64
		sta VIC_SPRITE_X_COORD
		sta VIC_SPRITE_Y_COORD
		
		rts

;=========================
;TEST_DrawTile
;draws a tile, type 2, palette 10 ($0a) at pos (5, 2)
;=========================

!zone TEST_DrawTile
TEST_DrawTile



		lda #$05
		sta VARIABLE1					;x coord
		lda #$02
		sta VARIABLE2					;y coord
		
		lda #$03
		sta VARIABLE3					;type
		
		lda #$0b
		sta VARIABLE4					;palette
		
		jsr LEVEL_DrawTile
		
		rts
		
;=========================
;TEST_FrameTimer
;flashes the border to show main loop is running
;=========================

!zone TEST_FrameTimer
TEST_FrameTimer
		inc VIC_BORDER_COLOR
		rts

;=========================
;TEST_LevelSwitch
;changes screen at the touch of a button
;=========================
!zone TEST_LevelSwitch
TEST_LevelSwitch
        lda JOYSTICK_2
        and #%00010000                  ;check button
        bne .NoButton                   ;a '0' means the button IS pressed

        ldx #$01                        ;index of second screen
        jsr LEVEL_LoadData
        jsr LEVEL_DrawScreen

.NoButton
        rts
		
;=========================================================================
;	CURRENT SCREEN DATA
;=========================================================================

;Each number in this table refers to the tile that should be drawn
CURRENT_SCREEN_TILE_DATA
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

;The primary colour for each tile on screen
CURRENT_SCREEN_PALETTE_DATA
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

;each tile will have 8 flags - to be finalised, things like will it block a characters movement (wall) or not (grass), is it a door etc
CURRENT_SCREEN_ATTRIBUTE_DATA
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
		!byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

CURRENT_SCREEN_BG_1
		!byte $00
		
CURRENT_SCREEN_BG_2
		!byte $00
		
;any extra data that might be needed - such as number and positions of npcs
CURRENT_SCREEN_GENERAL_DATA

;==========================
;	SCREEN DATA - A POINTER LOOKUP TABLE
;==========================

SCREEN_ROW_LOW_BYTE
        !byte ( ( SCREEN_CHAR_BUFFER +   0 ) & 0x00ff )
        !byte ( ( SCREEN_CHAR_BUFFER +  80 ) & 0x00ff )
        !byte ( ( SCREEN_CHAR_BUFFER + 160 ) & 0x00ff )
        !byte ( ( SCREEN_CHAR_BUFFER + 240 ) & 0x00ff )
        !byte ( ( SCREEN_CHAR_BUFFER + 320 ) & 0x00ff )
        !byte ( ( SCREEN_CHAR_BUFFER + 400 ) & 0x00ff )
        !byte ( ( SCREEN_CHAR_BUFFER + 480 ) & 0x00ff )
        !byte ( ( SCREEN_CHAR_BUFFER + 560 ) & 0x00ff )
        !byte ( ( SCREEN_CHAR_BUFFER + 640 ) & 0x00ff )
        !byte ( ( SCREEN_CHAR_BUFFER + 720 ) & 0x00ff )
        !byte ( ( SCREEN_CHAR_BUFFER + 800 ) & 0x00ff )

SCREEN_ROW_HIGH_BYTE
        !byte ( ( SCREEN_CHAR_BUFFER +   0 ) & 0xff00 ) >> 8
        !byte ( ( SCREEN_CHAR_BUFFER +  80 ) & 0xff00 ) >> 8
        !byte ( ( SCREEN_CHAR_BUFFER + 160 ) & 0xff00 ) >> 8
        !byte ( ( SCREEN_CHAR_BUFFER + 240 ) & 0xff00 ) >> 8
        !byte ( ( SCREEN_CHAR_BUFFER + 320 ) & 0xff00 ) >> 8
        !byte ( ( SCREEN_CHAR_BUFFER + 400 ) & 0xff00 ) >> 8
        !byte ( ( SCREEN_CHAR_BUFFER + 480 ) & 0xff00 ) >> 8
        !byte ( ( SCREEN_CHAR_BUFFER + 560 ) & 0xff00 ) >> 8
        !byte ( ( SCREEN_CHAR_BUFFER + 640 ) & 0xff00 ) >> 8
        !byte ( ( SCREEN_CHAR_BUFFER + 720 ) & 0xff00 ) >> 8
        !byte ( ( SCREEN_CHAR_BUFFER + 800 ) & 0xff00 ) >> 8
		
		
;=========================================================================
;	EXTERNAL DATA
;=========================================================================

CHARSET_DATA
		!binary "graphics/rpgset1.chr"

SPRITE_DATA
		!binary "graphics/sprites.spr"
		
		
;======================
;	LEVEL DATA
;======================

DATA_LEVEL_POINTERS
        !word DATA_LEVEL_0_POINTER
        !word DATA_LEVEL_1_POINTER
        !word 0

DATA_LEVEL_0_POINTER
        !word DATA_LEVEL_0
        !word DATA_LEVEL_0 + 220
        !word DATA_LEVEL_0 + 440

DATA_LEVEL_1_POINTER
        !word DATA_LEVEL_1
        !word DATA_LEVEL_1 + 220
        !word DATA_LEVEL_1 + 440

DATA_LEVEL_0
        !binary "levels/level0.lvl"

DATA_LEVEL_1
        !binary "levels/level1.lvl"

;TEST_TILE_DATA
		;!binary "levels/screen1.scn"
		
;TEST_PALETTE_DATA
;		!binary "levels/TESTpalette"
