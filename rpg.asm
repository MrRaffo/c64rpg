!to "rpg.prg",cbm

;ver 1: loads charset and sprite data, initializes VIC, tests first sprite definitions
;ver 2: added simple joystick controls for player sprite
;ver 3: added ability to draw a tile to any coord on screen, with colour data
;		added level drawing routine and test screen data
;ver 4: added animations and basic keyboard reading - pressing 1 to 4 will choose a different animation for player sprite
;ver 5: (b) added better animations and changed movement to 1 pixel per frame and one of four directions at a time

;ver 6: version 6 is a re-write, the level structure has been changed, so far it has
;       new level structure and level loading, per-tile coloring and per-tile attribute flags
;       basic joystick reading
;       sprite movement, but no sprite animation
;		added clipping with background for player sprite

;ver 7: re-introduced animations

;ver 8: added changing sprite priorities and extended x coords

;ver 9: tested adding multiple sprites
;       added overworld map, allow player to walk around 4x4 screens
;       did NOT implement level packing and erasing sprites from screen

;ver 10:added sprite loading from level data, and unloading sprites upon leaving a screen
;       unloading data does not put current sprite data back in level memory

;================================================
;CONSTANTS
;================================================

;COLOURS
BLACK                           = $00
WHITE                           = $01
RED                             = $02
CYAN                      	    = $03
VIOLET                          = $04
GREEN                         	= $05
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
CIA_PROCESSOR_PORT              = $01           ;set ram/rom visibility
CIA_PRA                         = $dd00         ;set vic bank
JOYSTICK_2                      = $dc00
JOYSTICK_1                      = $dc01

;VIC CHIP

VIC_CONTROL_REGISTER_1          = $d011         ;raster top bit (#8), screen height, screen visibility
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

VIC_SCREEN_OFFSET_X             = 24
VIC_SCREEN_OFFSET_Y             = 45            ;these are the coordinates of the bottom left of the first tile on screen
                                                ;use these to calculate a tile position for the sprite

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
VARIABLE6                       = $08
VARIABLE7                       = $09

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

NUMBER_OF_SPRITES               = $40
SPRITE_BASE                     = $40               ;64 in decimal
SPRITE_END                      = SPRITE_BASE + NUMBER_OF_SPRITES
SPRITE_POINTER_BASE             = SCREEN_CHAR_BUFFER + $3f8     ;1016 bytes after screen buffer
SPRITE_DATA_BUFFER              = $d000             ;put sprite definitions here

;WORLD CONSTANTS
OVERWORLD_WIDTH                 = 4
OVERWORLD_HEIGHT                = 4

;LEVEL CONSTANTS
LEVEL_TILE_WIDTH                = 20                ;tiles are 2x2
LEVEL_TILE_HEIGHT               = 11

TILE_SIDE                       = 16                ;size of a tile in pixels (length of a side)
TILE_HALF_SIDE                  = 8

SPRITE_SIZE                     = 16
SPRITE_HALF_SIZE                = 8
SPRITE_CLIPPING_DISTANCE_LEFT	= SPRITE_HALF_SIZE - 4
SPRITE_CLIPPING_DISTANCE_RIGHT  = SPRITE_HALF_SIZE + 6
SPRITE_CLIPPING_DISTANCE_UP		= SPRITE_HALF_SIZE
SPRITE_CLIPPING_DISTANCE_DOWN 	= 0

;DIRECTION
DIRECTION_UP					= 1
DIRECTION_DOWN					= 2
DIRECTION_LEFT					= 3
DIRECTION_RIGHT					= 4

;ACTION INDICES
ACTION_GENERAL_IDLE				= 0

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

ACTION_SHEEP_STAND_LEFT		    = 11
ACTION_SHEEP_STAND_RIGHT		= 12
ACTION_SHEEP_WALK_LEFT	    	= 13
ACTION_SHEEP_WALK_RIGHT	    	= 14
ACTION_SHEEP_EAT_LEFT	    	= 15
ACTION_SHEEP_EAT_RIGHT	    	= 16

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

ANIMATION_SHEEP_STAND_LEFT		= 11
ANIMATION_SHEEP_STAND_RIGHT		= 12
ANIMATION_SHEEP_WALK_LEFT		= 13
ANIMATION_SHEEP_WALK_RIGHT		= 14
ANIMATION_SHEEP_EAT_LEFT		= 15
ANIMATION_SHEEP_EAT_RIGHT		= 16


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
		
        ;==========================
        ;Prepare the background
        ;==========================

		;setup for testing purposes - should be screen specific later
		lda #BROWN
		sta VIC_BGCOLOR_1
		lda #MIDDLE_GREY
		sta VIC_BGCOLOR_2


        ;==========================
        ;Prepare the sprite
        ;==========================

        ldx #$ff
        stx VIC_SPRITE_MULTICOLOR_REGISTER

        ldx #$01
        stx VIC_SPRITE_ENABLE_REGISTER

        ldx #LIGHT_GREY
		stx SPRITE_UNIQUE_COLOR
        stx VIC_SPRITE_COLOR_BASE

        ldx #ORANGE
        stx VIC_SPRITE_COLOR_1

        ldx #YELLOW
        stx VIC_SPRITE_COLOR_2

        ldx #$40
        stx SPRITE_POINTER_BASE

        ;TEST TILE COORDS
        ldx #$06
        stx VARIABLE1
        ldx #$04
        stx VARIABLE2


        ldx #$00                    ;sprite index
        jsr LOGIC_PlaceSpriteInTile

		;=========================
		;Prepare animation for testing
		;=========================
		
		ldx #DIRECTION_DOWN
		stx SPRITE_CURRENT_DIRECTION
		
		;=========================
		;Load npc sprite for testing
		;=========================
		
		;jsr TEST_LoadSprite

		ldx #$00							;LEVEL index
        stx PLAYER_CURRENT_SCREEN
		jsr LEVEL_LoadScreenData
		jsr LEVEL_DrawScreen
	
		
;====================================================================
;	MAIN GAME LOOP
;====================================================================
		
!zone GameLoop
GameLoop
		;jsr TEST_FrameTimer				;flash border

		inc $d020
		
        ;jsr TEST_LevelSwitch               ;checks button, changes screen data on press

		jsr SYSTEM_HandleInput
		
		jsr SYSTEM_HandleLogic
				
		jsr LOGIC_UpdateSpriteAction
		
		dec $d020
		
		jsr SYSTEM_WaitFrame    			;wait until frame is finished
		
		jmp GameLoop
		
;====================================================================
;	SYSTEM FUNCTIONS
;====================================================================

;==========================
;SYSTEM_DisableScreen
;black out the screen while drawing to it
;==========================
!zone SYSTEM_DisableScreen
SYSTEM_DisableScreen
        lda $d011
        and %11101111
        sta $d011
        rts
        
;==========================
;SYSTEM_EnableScreen
;==========================
!zone SYSTEM_EnableScreen
SYSTEM_EnableScreen      
        lda $d011
        ora %00010000
        sta $d011
        rts

;==========================
;SYSTEM_HandleInput
;==========================
!zone SYSTEM_HandleInput
SYSTEM_HandleInput
		jsr INPUT_GetPlayerInput
		jsr INPUT_ParseInput
		rts

;==========================
;SYSTEM_HandleLogic
;==========================
!zone SYSTEM_HandleLogic
SYSTEM_HandleLogic
		jsr LOGIC_UpdateAnimation
		jsr LOGIC_UpdateSpritePriority
		jsr LOGIC_UpdateSpriteData
		jsr LOGIC_HandleExtendBit
		rts
		

;==========================
;SYSTEM_HandleAction

;==========================
;WaitFrame
;locks code to the VIC chip refresh
;waits until a frame has been drawn before moving on
;==========================

!zone SYSTEM_WaitFrame
SYSTEM_WaitFrame
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
;LEVEL_PlayerChangeScreen
;player has walked to the edge of the screen to trigger movement to the next area
;the direction they are travelling in is stored in VARIABLE1
;==========================
!zone LEVEL_PlayerChangeScreen
LEVEL_PlayerChangeScreen

        

        jsr LEVEL_UnloadData

        lda VARIABLE1
        
        cmp #DIRECTION_UP
        beq .GetScreenUp

        cmp #DIRECTION_DOWN
        beq .GetScreenDown

        cmp #DIRECTION_LEFT
        beq .GetScreenLeft

        cmp #DIRECTION_RIGHT
        beq .GetScreenRight

.GetScreenUp
        ldx SPRITE_TILE_X_POSITION
        stx VARIABLE1
        ldx #(LEVEL_TILE_HEIGHT-1)
        stx VARIABLE2

        ldx #$00
        jsr LOGIC_PlaceSpriteInTile
        
        dec PLAYER_WORLD_COORD + 1      ;decrease y coord
        ldx PLAYER_WORLD_COORD + 1
        stx MULTI1
        ldx #(OVERWORLD_WIDTH)
        stx MULTI2

        jsr MATHS_Multiply
        lda MULTIL
        clc
        adc PLAYER_WORLD_COORD

        sta PLAYER_CURRENT_SCREEN
        
        jmp .LoadNewScreen

.GetScreenDown
        ldx SPRITE_TILE_X_POSITION
        stx VARIABLE1
        ldx #$00
        stx VARIABLE2

        jsr LOGIC_PlaceSpriteInTile

        inc PLAYER_WORLD_COORD + 1
        ldx PLAYER_WORLD_COORD + 1
        stx MULTI1
        ldx #(OVERWORLD_WIDTH)
        stx MULTI2

        jsr MATHS_Multiply
        lda MULTIL
        clc
        adc PLAYER_WORLD_COORD

        sta PLAYER_CURRENT_SCREEN
       
        jmp .LoadNewScreen

.GetScreenLeft
        ldx SPRITE_TILE_Y_POSITION
        stx VARIABLE2
        ldx #(LEVEL_TILE_WIDTH - 1)
        stx VARIABLE1

        ldx #$00

        jsr LOGIC_PlaceSpriteInTile

        dec PLAYER_WORLD_COORD
        ldx PLAYER_WORLD_COORD + 1
        stx MULTI1
        ldx #(OVERWORLD_WIDTH)
        stx MULTI2

        jsr MATHS_Multiply

        lda MULTIL
        clc
        adc PLAYER_WORLD_COORD

        sta PLAYER_CURRENT_SCREEN
        
        jmp .LoadNewScreen

.GetScreenRight
        ldx SPRITE_TILE_Y_POSITION
        stx VARIABLE2                   ;players new x coord, as they will be entering from the left
        ldx #$00
        stx VARIABLE1
        
        jsr LOGIC_PlaceSpriteInTile
        
        inc PLAYER_WORLD_COORD          ;increase players world x coord
        ldx PLAYER_WORLD_COORD + 1      ;player Y coord
        stx MULTI1
        ldx #(OVERWORLD_WIDTH)
        stx MULTI2

        jsr MATHS_Multiply              ;get absolute reference of next screen

        lda MULTIL
        clc
        adc PLAYER_WORLD_COORD
        
        sta PLAYER_CURRENT_SCREEN
        
        jmp .LoadNewScreen

.LoadNewScreen
        jsr LEVEL_LoadScreenData
        jsr LEVEL_DrawScreen

        rts

;==========================
;LEVEL_UnloadData
;store current level config back into memory
;==========================
!zone LEVEL_UnloadData
LEVEL_UnloadData

        ;jsr SYSTEM_DisableScreen

        jsr LEVEL_UnloadSprites

        lda PLAYER_CURRENT_SCREEN
        asl
        tax
        
        lda DATA_LEVEL_POINTERS,x
        sta ZEROPAGE_POINTER_7
        lda DATA_LEVEL_POINTERS + 1,x
        sta ZEROPAGE_POINTER_7 + 1
        
        lda #<CURRENT_SCREEN_TILE_DATA                  ;get pointers to screen buffers
        sta ZEROPAGE_POINTER_1
        lda #>CURRENT_SCREEN_TILE_DATA
        sta ZEROPAGE_POINTER_1 + 1
        
        lda #<CURRENT_SCREEN_PALETTE_DATA
        sta ZEROPAGE_POINTER_2
        lda #>CURRENT_SCREEN_PALETTE_DATA
        sta ZEROPAGE_POINTER_2 + 1
        
        lda #<CURRENT_SCREEN_ATTRIBUTE_DATA
        sta ZEROPAGE_POINTER_3
        lda #>CURRENT_SCREEN_ATTRIBUTE_DATA
        sta ZEROPAGE_POINTER_3 + 1
        
        ldy #$00                                        ;get pointers to level memory
        lda (ZEROPAGE_POINTER_7),y
        sta ZEROPAGE_POINTER_4
        iny
        lda (ZEROPAGE_POINTER_7),y
        sta ZEROPAGE_POINTER_4 + 1
        iny
        lda (ZEROPAGE_POINTER_7),y
        sta ZEROPAGE_POINTER_5
        iny
        lda (ZEROPAGE_POINTER_7),y
        sta ZEROPAGE_POINTER_5 + 1
        iny
        lda (ZEROPAGE_POINTER_7),y
        sta ZEROPAGE_POINTER_6
        iny
        lda (ZEROPAGE_POINTER_7),y
        sta ZEROPAGE_POINTER_6 + 1
        
        ldy #$00
        
.DataCopyLoop
        lda (ZEROPAGE_POINTER_1),y
        sta (ZEROPAGE_POINTER_4),y
        
        lda (ZEROPAGE_POINTER_2),y
        sta (ZEROPAGE_POINTER_5),y
        
        lda (ZEROPAGE_POINTER_3),y
        sta (ZEROPAGE_POINTER_6),y
        
        iny
        cpy #$dc                                        ;220 in decimal, number of tiles per screen
        bne .DataCopyLoop
        
        rts

;==========================
;LEVEL_UnloadSprites    -   TODO
;this will handle how sprites are 'put away' when changing screen
;as I'm not sure yet how the player will interact with most sprites
;this code will simply remove them, allowing them to be reloaded
;upon returning to the screen.
;==========================
!zone LEVEL_UnloadSprites
LEVEL_UnloadSprites

        ldx #$00
        lda #$00

.UnloadBufferLoop
        sta CURRENT_SCREEN_SPRITE_DATA,x
        inx
        cpx #$38                            ;56, size of sprite data buffer
        bne .UnloadBufferLoop
        
        sta SPRITE_NPC_NUMBER_ACTIVE
        sta SPRITE_PRIORITY
        
        ldx #$01
        lda #$ff
        
.UnloadDataLoop
        sta SPRITE_NPC_NUMBER_ACTIVE,x
        inx
        cpx #$08
        bne .UnloadDataLoop
        
        lda #$01
        sta VIC_SPRITE_ENABLE_REGISTER
        
        rts


;==========================
;LEVEL_LoadScreenData
;loads all info needed for a screen
;==========================
!zone LEVEL_LoadScreenData
LEVEL_LoadScreenData
        jsr LEVEL_LoadData
        jsr LEVEL_LoadSpriteData
        jsr LEVEL_InitSprites
        ;jsr LEVEL_LoadDoorData             ;TODO
        
        ;jsr SYSTEM_EnableScreen
        rts

;==========================
;LEVEL_LoadData
;load screen data from raw memory to the current screen buffers
;screen index should be stored in x (or a 'current_level' flag later)
;===========================
!zone LEVEL_LoadData						
LEVEL_LoadData
		
        lda PLAYER_CURRENT_SCREEN
        
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
		
		lda (ZEROPAGE_POINTER_3),y
		sta (ZEROPAGE_POINTER_6),y			;copy attribute data
		
		iny
		cpy #$dc							;220 in decimal, number of tiles
		bne .LoadData
		
		rts									;use linux program convert.c to set a default palette

;==========================
;LEVEL_LoadSpriteData
;copies data from memory to buffer
;==========================
!zone LoadSpriteData
LEVEL_LoadSpriteData
        lda PLAYER_CURRENT_SCREEN
        asl
        tax

        ldy #$03                            ;sprite data is in pointer 3 (from 0) of the level data
        tya
        asl
        tay

        lda #<CURRENT_SCREEN_SPRITE_DATA
        sta ZEROPAGE_POINTER_1
        lda #>CURRENT_SCREEN_SPRITE_DATA
        sta ZEROPAGE_POINTER_1 + 1

        lda DATA_LEVEL_POINTERS,x
        sta ZEROPAGE_POINTER_2
        lda DATA_LEVEL_POINTERS + 1,x
        sta ZEROPAGE_POINTER_2 + 1      ;ZP2 now points to the start of the level data

        lda (ZEROPAGE_POINTER_2),y
        sta ZEROPAGE_POINTER_3
        iny
        lda (ZEROPAGE_POINTER_2),y
        sta ZEROPAGE_POINTER_3 + 1      ;ZP3 now points to the start of the sprite data

        ldy #$00
        ldx #$ff
        sty VARIABLE1                   ;counts sprites copied

.CopyLoop
        lda (ZEROPAGE_POINTER_3),y
        sta (ZEROPAGE_POINTER_1),y
        iny
        inx
        cpx #$00
        beq .CheckForSpriteData

        cpx #$08
        bne .CopyLoop

        inc VARIABLE1
        ldx VARIABLE1
        cpx #$07                        ;once the info for 7 sprites has been copied, we're done
        beq .CopyFinished

        ldx #$ff                        ;otherwise, reset x and go again
        jmp .CopyLoop


.CheckForSpriteData
        cmp #$00
        bne .CopyLoop

.CopyFinished
        rts

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

;==============================
;LEVEL_InitSprites
;activates sprites after they've been written to the buffer
;type, colour, action, animation, x grid pos, y grid pos
;==============================
        * = $2000
!zone LEVEL_InitSprites
LEVEL_InitSprites
        ldy #$00
        ldx #$00
        
.CheckForSprite
        lda CURRENT_SCREEN_SPRITE_DATA,y
        cmp #$00
        beq .NoMoreSpritesToLoad
        
.InitializeSprite
        inc SPRITE_NPC_NUMBER_ACTIVE
        ldx SPRITE_NPC_NUMBER_ACTIVE                ;x will now store the sprites index for memory structures
                        
        sta SPRITE_TYPE,x
        
        iny                                         ;load the sprite info from current screen buffers to sprite structures
        lda CURRENT_SCREEN_SPRITE_DATA,y
        sta SPRITE_UNIQUE_COLOR,x
        
        iny
        lda CURRENT_SCREEN_SPRITE_DATA,y
        sta SPRITE_CURRENT_ACTION,x
        sta SPRITE_NEXT_ACTION,x
        
        iny
        lda CURRENT_SCREEN_SPRITE_DATA,y
        sta VARIABLE1
        sty VARIABLE7
        jsr LOGIC_ChangeAnimation
        
        ldy VARIABLE7
        iny 
        lda CURRENT_SCREEN_SPRITE_DATA,y
        sta VARIABLE1
        
        iny 
        lda CURRENT_SCREEN_SPRITE_DATA,y
        sta VARIABLE2
        
        sty VARIABLE7
        jsr LOGIC_PlaceSpriteInTile
        ldy VARIABLE7
        
        iny                                         ;currently only using 6 bytes for sprite data, but left 2 more in case they are needed
        iny
        iny                                         ;this puts us on the next sprites data
        
        txa
        sta SPRITE_PRIORITY,x
        
        cpx #$07
        bne .CheckForSprite
        
.NoMoreSpritesToLoad
        inx                                         ;total number of sprites, now including player
        lda #$00                                    ;this will set the 'sprites enabled' register

.EnableLoop
        sec
        rol
        dex
        cpx #$00
        bne .EnableLoop
        
        sta VIC_SPRITE_ENABLE_REGISTER
        
.InitSpritesFinished
        rts
        
  
;=========================================================================
;   INPUT FUNCTIONS
;=========================================================================

;======================
;INPUT_GetInput
;reads joystick and relevent keyboard buttons
;do joystick first, reset CIA reg after keyboard read
;======================
!zone INPUT_GetInput
INPUT_GetPlayerInput

.ReadJoystick
        lda JOYSTICK_2
        cmp PLAYER_JOYSTICK_STATUS
        beq .ReadKeyboard

        sta PLAYER_JOYSTICK_STATUS


.ReadKeyboard
        ;This will set flags to be parsed later TODO

.GetInputFinished
        rts

;======================
;INPUT_ParseInput
;handles instructions from the player
;======================

!zone INPUT_ParseInput
INPUT_ParseInput

.ReadJoystick
        eor #$0f
        and #$0f                        ;check for change in direction
        bne .CheckDirections

        lda SPRITE_CURRENT_DIRECTION
        cmp #DIRECTION_UP
        bne .NotUp

        lda #ACTION_PLAYER_STAND_UP
        sta SPRITE_NEXT_ACTION
        jmp .CheckButton

.NotUp
        cmp #DIRECTION_DOWN
        bne .NotDown

        lda #ACTION_PLAYER_STAND_DOWN
        sta SPRITE_NEXT_ACTION
        jmp .CheckButton

.NotDown
        cmp #DIRECTION_LEFT
        bne .NotLeft

        lda #ACTION_PLAYER_STAND_LEFT
		sta SPRITE_NEXT_ACTION
        jmp .CheckButton

.NotLeft
        lda #ACTION_PLAYER_STAND_RIGHT
        sta SPRITE_NEXT_ACTION
        jmp .CheckButton

.CheckDirections
        lda #$01
        bit PLAYER_JOYSTICK_STATUS
        bne .UpNotPressed

        jsr LOGIC_PlayerMoveUp
        jmp .CheckButton

.UpNotPressed
        lda #$02
        bit PLAYER_JOYSTICK_STATUS
        bne .DownNotPressed

        jsr LOGIC_PlayerMoveDown
        jmp .CheckButton

.DownNotPressed
        lda #$04
        bit PLAYER_JOYSTICK_STATUS
        bne .LeftNotPressed

        jsr LOGIC_PlayerMoveLeft
        jmp .CheckButton

.LeftNotPressed
        lda #$08
        bit PLAYER_JOYSTICK_STATUS
        bne .CheckButton

        jsr LOGIC_PlayerMoveRight

.CheckButton
        lda #$10
        bit PLAYER_JOYSTICK_STATUS
        bne .JoystickStatusNotChanged

        jsr LOGIC_PlayerButton        

.JoystickStatusNotChanged
        rts

;=========================================================================
;   LOGIC FUNCTIONS
;=========================================================================

;========================
;PlayerMovement Functions
;========================

;================================================
;LOGIC_PlayerMoveUp
;================================================
!zone LOGIC_PlayerMoveUp
LOGIC_PlayerMoveUp
        ldx #DIRECTION_UP
        stx SPRITE_CURRENT_DIRECTION
		ldx #ANIMATION_PLAYER_WALK_UP
		stx SPRITE_NEXT_ACTION

        ldx #$00       ;current sprite index, will have to be priority sorted by height when more sprites added
        lda SPRITE_TILE_Y_DELTA,x
		cmp #SPRITE_HALF_SIZE
		beq .CheckCanMoveUp

.CanMoveUp
		dec SPRITE_TILE_Y_DELTA,x
		lda SPRITE_TILE_Y_DELTA,x
		cmp #$ff
		bne .NoChangeInTilePosition
		
		dec SPRITE_TILE_Y_POSITION,x
		lda #(TILE_SIDE - 1)
  		sta SPRITE_TILE_Y_DELTA,x
				
.NoChangeInTilePosition
		jsr LOGIC_MoveSpriteUp
        rts

.CheckCanMoveUp
		lda SPRITE_TILE_Y_POSITION,x
		cmp #$00
		beq .CheckScreenUp
		
		sta MULTI1
		dec MULTI1						;need data for tile above player
		lda #LEVEL_TILE_WIDTH
		sta MULTI2
		
		jsr MATHS_Multiply
		
		lda MULTIL
		clc
		adc SPRITE_TILE_X_POSITION,x	;add position along current row to get correct block
										;shouldn't be any overflow as all results are < 220
		tay
		lda CURRENT_SCREEN_ATTRIBUTE_DATA,y
		and #$01						;check if low bit is set		
		bne .NoMoveAvailable			;if so, we can't move
		
										;we now have to check for blocks to the above right and
										;above left of the sprite to avoid clipping problems
										
		lda SPRITE_TILE_X_DELTA,x
		cmp #SPRITE_HALF_SIZE
		bmi .CheckUpperLeft
		
.CheckUpperRight
		cmp #(SPRITE_CLIPPING_DISTANCE_RIGHT + 1)
		bmi .CanMoveUp					;if we haven't moved too far to the right, move up
		
		iny
		lda CURRENT_SCREEN_ATTRIBUTE_DATA,y
		and #$01
		bne .NoMoveAvailable
		
		jmp .CanMoveUp
		
.CheckUpperLeft
		cmp #SPRITE_CLIPPING_DISTANCE_LEFT
		bpl .CanMoveUp
		
		dey
		lda CURRENT_SCREEN_ATTRIBUTE_DATA,y
		and #$01
		bne .NoMoveAvailable
		
		jmp .CanMoveUp
		

.NoMoveAvailable		
		rts								;if we can't move up, don't, and return to program

.CheckScreenUp
        lda PLAYER_WORLD_COORD+1
        cmp #$00
        beq .NoMoveAvailable            ;if we're at the top of the overworld, we can't go any further

        lda #DIRECTION_UP
        sta VARIABLE1

        jmp LEVEL_PlayerChangeScreen
		
		
;================================================
;LOGIC_PlayerMoveDown
;================================================
!zone LOGIC_PlayerMoveDown
LOGIC_PlayerMoveDown
        ldx #DIRECTION_DOWN
        stx SPRITE_CURRENT_DIRECTION
		ldx #ANIMATION_PLAYER_WALK_DOWN
		stx SPRITE_NEXT_ACTION

        ldx #$00
		lda SPRITE_TILE_Y_DELTA,x
		cmp #(SPRITE_SIZE - 1)
		beq .CheckCanMoveDown
		
.CanMoveDown
		inc SPRITE_TILE_Y_DELTA,x
		lda SPRITE_TILE_Y_DELTA,x
		cmp #$10						;16, check if we've crossed the tile boundary
		bne .NoChangeInTilePosition
		
		inc SPRITE_TILE_Y_POSITION,x
		lda #$00
		sta SPRITE_TILE_Y_DELTA,x
		
.NoChangeInTilePosition
		jsr LOGIC_MoveSpriteDown
		rts
		
.CheckCanMoveDown
		lda SPRITE_TILE_Y_POSITION,x
		cmp #(LEVEL_TILE_HEIGHT - 1)
		beq .CheckScreenDown
		
		sta MULTI1
		inc MULTI1						;get coords of tile below sprite
		lda #LEVEL_TILE_WIDTH
		sta MULTI2
		
		jsr MATHS_Multiply
		
		lda MULTIL
		clc
		adc SPRITE_TILE_X_POSITION,x
		
		tay
		lda CURRENT_SCREEN_ATTRIBUTE_DATA,y
		and #$01
		bne .NoMoveAvailable
		
		lda SPRITE_TILE_X_DELTA,x
		cmp #SPRITE_HALF_SIZE
		bmi .CheckLowerLeft
		
.CheckLowerRight
		cmp #(SPRITE_CLIPPING_DISTANCE_RIGHT + 1)
		bmi .CanMoveDown
		
		iny
		lda CURRENT_SCREEN_ATTRIBUTE_DATA,y
		and #$01
		bne .NoMoveAvailable
		
		jsr .CanMoveDown
		
.CheckLowerLeft
		cmp #SPRITE_CLIPPING_DISTANCE_LEFT
		bpl .CanMoveDown
		
		dey
		lda CURRENT_SCREEN_ATTRIBUTE_DATA,y
		and #$01
		bne .NoMoveAvailable
		
		jsr .CanMoveDown
		
.NoMoveAvailable
		rts

.CheckScreenDown
        lda PLAYER_WORLD_COORD + 1
        cmp #(OVERWORLD_HEIGHT - 1)
        beq .NoMoveAvailable

        lda #DIRECTION_DOWN
        sta VARIABLE1

        jmp LEVEL_PlayerChangeScreen
		
;================================================
;LOGIC_PlayerMoveLeft
;================================================
!zone LOGIC_PlayerMoveLeft
LOGIC_PlayerMoveLeft
        ldx #DIRECTION_LEFT
        stx SPRITE_CURRENT_DIRECTION
		ldx #ANIMATION_PLAYER_WALK_LEFT
		stx SPRITE_NEXT_ACTION

		ldx #$00
		lda SPRITE_TILE_X_DELTA,x
		cmp #SPRITE_CLIPPING_DISTANCE_LEFT
		beq .CheckCanMoveLeft
		
.CanMoveLeft
		dec SPRITE_TILE_X_DELTA,x
		lda SPRITE_TILE_X_DELTA,x
		cmp #$ff						;check for underflow
		bne .NoChangeInTilePosition
		
		dec SPRITE_TILE_X_POSITION,x
		lda #(TILE_SIDE - 1)
		sta SPRITE_TILE_X_DELTA,x
		
.NoChangeInTilePosition
        jsr LOGIC_MoveSpriteLeft
        rts

.CheckCanMoveLeft
		lda SPRITE_TILE_X_POSITION,x
		cmp #$00
		beq .CheckScreenLeft				;at the edge of the screen

		lda SPRITE_TILE_Y_POSITION,x
		sta MULTI1
		lda #LEVEL_TILE_WIDTH
		sta MULTI2
		
		jsr MATHS_Multiply
		
		lda MULTIL
		clc
		adc SPRITE_TILE_X_POSITION,x
		
		tay
		dey									;get block to the left of the player
		
		lda CURRENT_SCREEN_ATTRIBUTE_DATA,y
		and #$01
		bne .NoMoveAvailable
		
		lda SPRITE_TILE_Y_DELTA,x
		cmp #SPRITE_CLIPPING_DISTANCE_UP
		bpl .CanMoveLeft
		
.CheckUpperLeft
		tya
		sec
		sbc #LEVEL_TILE_WIDTH				;get address of tile above player to the left
		tay
		
		lda CURRENT_SCREEN_ATTRIBUTE_DATA,y
		and #$01
		beq .CanMoveLeft
		
.NoMoveAvailable
		rts
		
.CheckScreenLeft
        lda PLAYER_WORLD_COORD
        cmp #$00
        beq .NoMoveAvailable

        lda #DIRECTION_LEFT
        sta VARIABLE1

        jmp LEVEL_PlayerChangeScreen


;================================================
;LOGIC_PlayerMoveRight
;================================================
!zone LOGIC_PlayerMoveRight
LOGIC_PlayerMoveRight
        ldx #DIRECTION_RIGHT
        stx SPRITE_CURRENT_DIRECTION
		ldx #ANIMATION_PLAYER_WALK_RIGHT
		stx SPRITE_NEXT_ACTION

        ldx #$00
		lda SPRITE_TILE_X_DELTA,x
		cmp #SPRITE_CLIPPING_DISTANCE_RIGHT
		beq .CheckCanMoveRight
		
.CanMoveRight
		inc SPRITE_TILE_X_DELTA,x
		lda SPRITE_TILE_X_DELTA,x
		cmp #$10
		bne .NoChangeInTilePosition
		
		inc SPRITE_TILE_X_POSITION,x
		lda #$00
		sta SPRITE_TILE_X_DELTA,x
		
.NoChangeInTilePosition
        jsr LOGIC_MoveSpriteRight
        rts

.CheckCanMoveRight
		lda SPRITE_TILE_X_POSITION,x
		cmp #(LEVEL_TILE_WIDTH - 1)
		beq .CheckNextScreen            ;check if player can transition to a new screen
		
		lda SPRITE_TILE_Y_POSITION,x
		sta MULTI1
		lda #LEVEL_TILE_WIDTH
		sta MULTI2
		
		jsr MATHS_Multiply
		
		lda MULTIL
		clc
		adc SPRITE_TILE_X_POSITION,x
		
		tay
		iny 							;get tile to right of player
		
		lda CURRENT_SCREEN_ATTRIBUTE_DATA,y
		and #$01
		bne .NoMoveAvailable
		
		lda SPRITE_TILE_Y_DELTA,x
		cmp #SPRITE_CLIPPING_DISTANCE_UP
		bpl .CanMoveRight
		
.CheckUpperRight
		tya
		sec
		sbc	#LEVEL_TILE_WIDTH
		tay
		
		lda CURRENT_SCREEN_ATTRIBUTE_DATA,y
		and #$01
		beq .CanMoveRight

.NoMoveAvailable
        rts

.CheckNextScreen
        lda PLAYER_WORLD_COORD
        cmp #(OVERWORLD_WIDTH - 1)
        beq .NoMoveAvailable

        lda #DIRECTION_RIGHT
        sta VARIABLE1

        jmp LEVEL_PlayerChangeScreen

		
;================================================
;LOGIC_PlayerButton
;================================================
!zone LOGIC_PlayerButton
LOGIC_PlayerButton
        jsr TEST_EditTile
		rts


		
;==========================
;MoveSprite Functions
;expects x to hold sprite index
;==========================
!zone LOGIC_MoveSpriteUp
LOGIC_MoveSpriteUp
        dec SPRITE_Y_POSITION,x
        rts

!zone LOGIC_MoveSpriteDown
LOGIC_MoveSpriteDown
        inc SPRITE_Y_POSITION,x
        rts

!zone LOGIC_MoveSpriteHorizontal
LOGIC_MoveSpriteLeft
        dec SPRITE_X_POSITION,x
        bpl .UpdateDone                 ;screen 'boundary' not crossed (high bit not affected)

        lda BIT_MASK,x
        eor #$ff
        and SPRITE_X_EXTEND
        sta SPRITE_X_EXTEND

        rts

LOGIC_MoveSpriteRight
        inc SPRITE_X_POSITION,x
        bne .UpdateDone

        lda BIT_MASK,x
        ora SPRITE_X_EXTEND
        sta SPRITE_X_EXTEND

.UpdateDone
        rts

;============================
;LOGIC_UpdateSpritePriority
;prepares data and calls bubblesort routine
;============================
!zone LOGIC_UpdateSpritePriority
LOGIC_UpdateSpritePriority
		ldx SPRITE_NPC_NUMBER_ACTIVE
        cpx #$00
        bne .PrepareSort
        rts
        
.PrepareSort
		stx VARIABLE5
		inc VARIABLE5
		ldx #$00
		
.CopyDataToBuffer
		ldy SPRITE_PRIORITY,x
		lda SPRITE_Y_POSITION,y
		sta BUBBLE_DATA_VALUES,x
		tya
		sta BUBBLE_DATA_INDICES,x
		inx
		cpx VARIABLE5
		bne .CopyDataToBuffer
		
		dec VARIABLE5
		
		jsr MATHS_BubbleSort
		
.CopyDataToSpriteMemory
		ldx #$00
		inc VARIABLE5
.CopyLoop
		lda BUBBLE_DATA_INDICES,x
		sta SPRITE_PRIORITY,x
		inx
		cpx VARIABLE5
		bne .CopyLoop
		
		rts
		
;============================
;LOGIC_UpdateSpriteData
;moves sprite coord data from memory to their registers
;============================
!zone LOGIC_UpdateSpriteData
LOGIC_UpdateSpriteData
        ldx SPRITE_NPC_NUMBER_ACTIVE
		inx
		stx VARIABLE2
		
		ldx #$00                		;index
		stx VARIABLE1
		
.UpdateLoop
		
		ldx VARIABLE1
		ldy SPRITE_PRIORITY,x
		
		lda SPRITE_UNIQUE_COLOR,y
		sta VIC_SPRITE_COLOR_BASE,x
		
		lda SPRITE_CURRENT_ANIMATION_FRAME,y
		sta SPRITE_POINTER_BASE,x
		
		txa
		asl
		tax
		
		lda SPRITE_X_POSITION,y
		sta VIC_SPRITE_X_COORD,x
		
		lda SPRITE_Y_POSITION,y
		sta VIC_SPRITE_Y_COORD,x
		
		inc VARIABLE1
		ldx VARIABLE1
		cpx VARIABLE2
		bne .UpdateLoop

		rts
		
;=================================
;LOGIC_HandleExtendBit
;rearranges the extend bit based on priorities
;=================================
!zone LOGIC_HandleExtendBit
LOGIC_HandleExtendBit
		
		ldx SPRITE_NPC_NUMBER_ACTIVE
		inx
		stx VARIABLE1
		
		ldx #$00
		stx VARIABLE2
		stx VARIABLE3
		
.UpdateLoop
		ldy SPRITE_PRIORITY,x
		lda BIT_MASK,y
		
		bit SPRITE_X_EXTEND
		beq .UnsetBit
		
.SetBit
		lda BIT_MASK,x
		ora VARIABLE2
		sta VARIABLE2
		jmp .CheckIfDone
		
.UnsetBit
		lda BIT_MASK,x
		eor #$ff
		and VARIABLE2
		sta VARIABLE2
		
.CheckIfDone
		inx
		cpx VARIABLE1
		bne .UpdateLoop

.UpdateVicRegister		
		lda VARIABLE2
        sta VIC_SPRITE_X_EXTEND

        rts

;===============================
;LOGIC_PlaceSpriteInTile
;puts a sprite in a tile on the screen
;expects sprite index in the x register, tile coords in VARIABLE1 and VARIABLE2
;===============================
!zone LOGIC_PlaceSpriteInTile
LOGIC_PlaceSpriteInTile

        stx VARIABLE6                   ;for later, sprite index
		lda VARIABLE1
		sta SPRITE_TILE_X_POSITION,x
		lda VARIABLE2
		sta SPRITE_TILE_Y_POSITION,x

        jsr LOGIC_GetTileCoords

        ldx VARIABLE6

        lda VARIABLE3
        sta SPRITE_X_POSITION,x
        lda VARIABLE4
        sta SPRITE_Y_POSITION,x

		lda #SPRITE_HALF_SIZE			;													 _____
		sta SPRITE_TILE_X_DELTA,x		;put sprite in the centre of the tile				|     |
		lda #(SPRITE_SIZE - 1)			;												    |     |
		sta SPRITE_TILE_Y_DELTA,x		;sprite position centre of bottom row of pixels ->  |__.__| 
		
        lda VARIABLE5
        cmp #$00
        beq .NoSpriteExtend

        lda BIT_MASK,x					;set sprite extend bit
        ora SPRITE_X_EXTEND
        sta SPRITE_X_EXTEND
		jmp .PositionDone

.NoSpriteExtend							;or else make sure extend bit is not set
		lda BIT_MASK,x
		eor #$ff
		and SPRITE_X_EXTEND
		sta SPRITE_X_EXTEND
		
.PositionDone
        rts

;===============================
;LOGIC_GetTileCoords
;returns tile coords, x,y of tile in VARIABLE1 and VARIABLE2
;answer returned in VARIABLE3 and VARIABLE4, overflow set in VARIABLE5
;===============================
!zone LOGIC_GetTileCoords
LOGIC_GetTileCoords
        ldx #$00
        stx VARIABLE3
        stx VARIABLE4
        stx VARIABLE5

.GetTileYCoord
        ldx VARIABLE2
        stx MULTI1
        ldx #TILE_SIDE              ;16, size of tile
        stx MULTI2
        
        jsr MATHS_Multiply
        
        clc
        lda MULTIL
        adc #VIC_SCREEN_OFFSET_Y     ;re-adjust coords to take into account the border
        sta VARIABLE4               ;y coord set

.GetTileXCoord
        ldx VARIABLE1
        stx MULTI1
        ldx #TILE_SIDE
        stx MULTI2

        jsr MATHS_Multiply

        ldx MULTIH
        bne .HighByteSet

        clc
        lda MULTIL
        adc #VIC_SCREEN_OFFSET_X
        bcc .TileCoordsFound

        lda #$01
        sta VARIABLE5
        jmp .TileCoordsFound

.HighByteSet
        stx VARIABLE5
        clc
        lda MULTIL
        adc #VIC_SCREEN_OFFSET_X

.TileCoordsFound
        sta VARIABLE3
        rts

;=================================
;	ANIMATION AND ACTIONS
;=================================

;==========================
;LOGIC_UpdateAnimation
;loops through sprites, making necessary updates
;==========================
!zone LOGIC_UpdateAnimation
LOGIC_UpdateAnimation
		ldx #$00
		
.CheckForChange
		lda SPRITE_NEXT_ACTION,x
		cmp SPRITE_CURRENT_ACTION,x
		beq .UpdateLoop
		
		sta VARIABLE1
		jsr LOGIC_ChangeAnimation
		
.UpdateLoop
		jsr LOGIC_UpdateCurrentFrame
		inx
		cpx #$08
		bne .CheckForChange
		rts

;==========================
;LOGIC_UpdateCurrentFrame
;x is the sprite index
;==========================
!zone LOGIC_UpdateCurrentFrame
LOGIC_UpdateCurrentFrame
		dec SPRITE_ANIMATION_TIMER,x
		bne .NoUpdateNeededYet				;check if enough time has passed to update frame
		
		lda SPRITE_ANIMATION_CYCLE,x
		asl
		tay									;double to get correct address of animation pointer
		
		lda ANIMATION_LIST,y
		sta ZEROPAGE_POINTER_1
		lda ANIMATION_LIST + 1,y
		sta ZEROPAGE_POINTER_1 + 1			;get pointer to the current animation
		
		ldy #$00
		
		lda (ZEROPAGE_POINTER_1),y			;this is the timer for the animation
		sta SPRITE_ANIMATION_TIMER,x
		inc SPRITE_FRAME_INDEX,x
		ldy SPRITE_FRAME_INDEX,x
		
		lda (ZEROPAGE_POINTER_1),y			;get index of next frame in sequence
		bne .NoRestartNeeded				;a value of 0 indicates end of animation
		
		ldy #$01							;frame 1 has offset of 1, as timer is at 0
		lda (ZEROPAGE_POINTER_1),y
		tay
		lda #$01
		sta SPRITE_FRAME_INDEX,x
		tya
		
.NoRestartNeeded
		sta SPRITE_CURRENT_ANIMATION_FRAME,x
		
.NoUpdateNeededYet
		rts
		
;===========================
;LOGIC_ChangeAnimation
;expects x as sprite index, VARIABLE1 as new animation
;===========================
!zone LOGIC_ChangeAnimation
LOGIC_ChangeAnimation
		lda VARIABLE1
		sta SPRITE_ANIMATION_CYCLE,x
		
		asl
		tay
		lda ANIMATION_LIST,y
		sta ZEROPAGE_POINTER_1
		lda ANIMATION_LIST + 1,y
		sta ZEROPAGE_POINTER_1 + 1
		
		ldy #$00
		lda (ZEROPAGE_POINTER_1),y
		sta SPRITE_ANIMATION_TIMER,x
		
		iny
		lda (ZEROPAGE_POINTER_1),y
		sta SPRITE_CURRENT_ANIMATION_FRAME,x
		
		lda #$01
		sta SPRITE_FRAME_INDEX,x
		
		rts
		
		
;============================
;LOGIC_UpdateSpriteAction
;============================
!zone LOGIC_UpdateSpriteAction
LOGIC_UpdateSpriteAction
		ldx #$00
.UpdateLoop
		lda SPRITE_NEXT_ACTION,x
		sta SPRITE_CURRENT_ACTION,x
		inx
		cpx #$08
		bne .UpdateLoop
		
		rts
		
;=========================================================================
;   MATHS FUNCTIONS
;=========================================================================

;======================
;MATHS_Multiply
;takes 2 operands and multiplies them together
;operands given in MULTI1 and MULTI2, result (16-bit) stored in MULTIH and MULTIL
;preferred to put smaller number in MULTI1
;======================
!zone MATHS_Multiply
MATHS_Multiply
        ldx #$00
        stx MULTIL
        stx MULTIH
        txa                     ;zero result bytes and accumulator

        ldx MULTI2
        beq .MultiplyDone
        ldx MULTI1
        beq .MultiplyDone       ;if either operand is zero, we are done

        clc                     ;prepare to multiply non-zero operands

.MultiplyLoop
        adc MULTI2              ;add MULTI2 to itself MULTI1 times
        bcc .NoOverFlow

        inc MULTIH              ;increment result high byte in case of overflow
        clc

.NoOverFlow
        dex
        bne .MultiplyLoop       ;when x is zero, we are finished

.MultiplyDone
        sta MULTIL              ;accumulator holds low byte of result, high byte set during operation
        rts

		
;===========================
;MATHS_BubbleSort
;sorts sprites indices from lowest to highest ie, highest y positions first
;expects number of elements to sort in VARIABLE5 and data to be in specific buffer
;============================

!zone MATHS_BubbleSort
MATHS_BubbleSort

		lda VARIABLE5
		cmp #$00
        beq .SortFinished       ;return if there's nothing to sort
        
        cmp #$08
		bcc .StartSort
		
		lda #$07
		sta VARIABLE5			;make sure we never check too many items
	
.StartSort
		ldx #$00
		stx VARIABLE4			;counts number of passes of sorting routine
		
.BubbleSort
		ldx #$00
		ldy #$00
		stx VARIABLE1			;temp storage for swapping data
		stx VARIABLE2			;temp storage for swapping indices
		stx VARIABLE3			;counts number of swaps, when this is zero, we're done
		inc VARIABLE4			;sets to zero first run through, then counts
		
.SortLoop
		lda BUBBLE_DATA_VALUES,y
		sta VARIABLE1
		iny
		inx
		lda BUBBLE_DATA_VALUES,y
		cmp VARIABLE1
		bcc .NoSwapNeeded
		beq .NoSwapNeeded
		
.SwapValues
		dey
		sta BUBBLE_DATA_VALUES,y
		lda BUBBLE_DATA_INDICES,y
		sta VARIABLE2
		lda BUBBLE_DATA_INDICES,x
		dex
		sta BUBBLE_DATA_INDICES,x
		iny
		inx
		lda VARIABLE1
		sta BUBBLE_DATA_VALUES,y
		lda VARIABLE2
		sta BUBBLE_DATA_INDICES,x
		
		inc VARIABLE3			;we've swapped, so set the flag, we're not done yet
		
.NoSwapNeeded
		cpy VARIABLE5			;V5 = total number of sprites - 1, no point checking for more
		bne .SortLoop
		
.CheckIfFinished
		lda VARIABLE3
		cmp #$00
		bne .BubbleSort
		
.SortFinished
		rts
	
BUBBLE_DATA_VALUES
		!byte $00, $00, $00, $00, $00, $00, $00, $00
		
BUBBLE_DATA_INDICES
		!byte $00, $00, $00, $00, $00, $00, $00, $00

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
;TEST_LoadSprite
;initializes a sprite to test various routines - sprite data:
;type, colour, action, animation, x grid pos, y grid pos
;=========================
!zone TEST_LoadSprite
TEST_LoadSprite

        ldy #$00

.LoadSprite
		inc SPRITE_NPC_NUMBER_ACTIVE
		ldx SPRITE_NPC_NUMBER_ACTIVE
        
        cpx $08
        bne .SpriteSlotFree
        rts
        
.SpriteSlotFree        
        txa
        sta SPRITE_PRIORITY,x
        
        lda CURRENT_SCREEN_SPRITE_DATA,y
        sta SPRITE_TYPE,x
        
        iny
        lda CURRENT_SCREEN_SPRITE_DATA,y
        sta SPRITE_UNIQUE_COLOR,x
        
        iny
        lda CURRENT_SCREEN_SPRITE_DATA,y
        sta SPRITE_CURRENT_ACTION,x
        sta SPRITE_NEXT_ACTION,x
        
        iny
        lda CURRENT_SCREEN_SPRITE_DATA,y
        sta VARIABLE1
        sty VARIABLE7
        jsr LOGIC_ChangeAnimation
        
        ldy VARIABLE7
        iny
        lda CURRENT_SCREEN_SPRITE_DATA,y
        sta VARIABLE1
        iny
        lda CURRENT_SCREEN_SPRITE_DATA,y
        sta VARIABLE2
        
        sty VARIABLE7
        jsr LOGIC_PlaceSpriteInTile
        
        ldy VARIABLE7
        iny

        iny
        iny                             ;sprite data takes 8 bytes
        
        cpx #$03
        bne .LoadSprite
        
		lda #$00
		inx                             ;add 1 to include player sprite
.EnableLoop
		sec
		rol								;add 'bits' for each sprite
		dex
		bne .EnableLoop
		
		sta VIC_SPRITE_ENABLE_REGISTER

.Finished
		rts
        
;========================
;TEST_EditTile
;make a tile 'flowers' for testing
;========================
!zone TEST_EditTile
TEST_EditTile
        lda SPRITE_TILE_X_POSITION
        sta VARIABLE1                           ;VAR1 for DrawTile
        lda SPRITE_TILE_Y_POSITION
        sta VARIABLE2
        sta MULTI1
        
        lda #LEVEL_TILE_WIDTH
        sta MULTI2
        
        jsr MATHS_Multiply
        lda MULTIL
        clc
        adc SPRITE_TILE_X_POSITION
        
        tax
        
        lda #$14                                ;FLOWERS tile
        sta VARIABLE3
        sta CURRENT_SCREEN_TILE_DATA,x          ;update level buffers
        
        lda #$0b                                ;CYAN multicolour
        sta VARIABLE4
        sta CURRENT_SCREEN_PALETTE_DATA,x
        
        jsr LEVEL_DrawTile                      ;LEVEL_DrawTile needs coords in V1, V2 and Tile and Palette in V3 and V4
        
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
        * = $3000
;type, colour, action, animation, x grid pos, y grid pos - max of 7
CURRENT_SCREEN_SPRITE_DATA
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00

;includes stairs and secrets - any way of transition to another screen other than walking off the edge - max of 4 per screen
CURRENT_SCREEN_DOOR_DATA
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
        !byte $00, $00, $00, $00, $00, $00, $00, $00
;=================================================================
;   SPRITE PLACEMENT DATA
;=================================================================

        * = $4000

SPRITE_NPC_NUMBER_ACTIVE
		!byte $00												;number of non-player sprites

SPRITE_X_EXTEND
        !byte $00

SPRITE_X_POSITION                                               ;sprite position on screen
        !byte $00, $00, $00, $00, $00, $00, $00, $00

SPRITE_Y_POSITION
        !byte $00, $00, $00, $00, $00, $00, $00, $00

SPRITE_TILE_X_POSITION                                          ;tile sprite currently occupies
        !byte $00, $00, $00, $00, $00, $00, $00, $00

SPRITE_TILE_Y_POSITION
        !byte $00, $00, $00, $00, $00, $00, $00, $00

SPRITE_TILE_X_DELTA                                             ;how far through a tile the sprite has moved
        !byte $00, $00, $00, $00, $00, $00, $00, $00

SPRITE_TILE_Y_DELTA
        !byte $00, $00, $00, $00, $00, $00, $00, $00

SPRITE_PRIORITY
		!byte $00, $ff, $ff, $ff, $ff, $ff, $ff, $ff			;lower sprites should show over higher
		
SPRITE_UNIQUE_COLOR
		!byte $00, $00, $00, $00, $00, $00, $00, $00			
		
;=================================================================
;   SPRITE ACTION DATA
;=================================================================

SPRITE_TYPE
		!byte $00, $00, $00, $00, $00, $00, $00, $00			;for AI routines

SPRITE_CURRENT_ACTION											
		!byte $00, $00, $00, $00, $00, $00, $00, $00            ;what the sprite is currently up to
		
SPRITE_CURRENT_DIRECTION
		!byte $00, $00, $00, $00, $00, $00, $00, $00
		
SPRITE_NEXT_ACTION												
		!byte $00, $00, $00, $00, $00, $00, $00, $00            ;next action for sprite
		
SPRITE_DECISION_TIMER
		!byte $00, $00, $00, $00, $00, $00, $00, $00			;time between npc actions

;=================================================================
;   SPRITE ANIMATION DATA
;=================================================================
		
SPRITE_CURRENT_ANIMATION_FRAME									;what the sprite should display
		!byte $00, $00, $00, $00, $00, $00, $00, $00
		
SPRITE_FRAME_INDEX												;how far into an animation sprite is
		!byte $00, $00, $00, $00, $00, $00, $00, $00
		
SPRITE_ANIMATION_TIMER											;how quickly an animation should play
		!byte $00, $00, $00, $00, $00, $00, $00, $00
			
SPRITE_ANIMATION_CYCLE											;current animation that should be playing
		!byte $00, $00, $00, $00, $00, $00, $00, $00

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
;   PLAYER DATA
;=========================================================================

PLAYER_JOYSTICK_STATUS
        !byte $00

PLAYER_WORLD_COORD
        !byte $00, $00

PLAYER_CURRENT_SCREEN
        !byte $00

;=========================================================================
;   GAME FLAGS
;=========================================================================



;=========================================================================
;   GENERAL DATA
;=========================================================================

BIT_MASK
        !byte $01, $02, $04, $08, $10, $20, $40, $80	

;==================================================================
;	ANIMATION DATA
;==================================================================

;=============================
;	ANIMATIONS
;=============================

ANIMATION_LIST
		!word 0x0000						;null pointer
		!word ANIM_PLAYER_STAND_UP
		!word ANIM_PLAYER_STAND_DOWN
		!word ANIM_PLAYER_STAND_LEFT
		!word ANIM_PLAYER_STAND_RIGHT
		!word ANIM_PLAYER_WALK_UP
		!word ANIM_PLAYER_WALK_DOWN
		!word ANIM_PLAYER_WALK_LEFT
		!word ANIM_PLAYER_WALK_RIGHT
		!word ANIM_PLAYER_TALK
		!word ANIM_PLAYER_DANCE
		
		!word ANIM_SHEEP_STAND_LEFT
		!word ANIM_SHEEP_STAND_RIGHT
		!word ANIM_SHEEP_WALK_LEFT
		!word ANIM_SHEEP_WALK_RIGHT
		!word ANIM_SHEEP_EAT_LEFT
		!word ANIM_SHEEP_EAT_RIGHT

		
;==========================================
;	PLAYER ANIMATION DATA
;==========================================
		
ANIM_PLAYER_STAND_UP
		!byte $08
		!byte $4b, $00						;one frame for standing

ANIM_PLAYER_STAND_DOWN
		!byte $08
		!byte $40, $00
		
ANIM_PLAYER_STAND_LEFT
		!byte $08
		!byte $49, $00
		
ANIM_PLAYER_STAND_RIGHT
		!byte $08
		!byte $46, $00
		
ANIM_PLAYER_WALK_UP
		!byte $06
		!byte $4c, $4b, $4d, $4b, $00
		
ANIM_PLAYER_WALK_DOWN
		!byte $03
		!byte $41, $42, $41, $40, $43, $44, $43, $40, $00
		
ANIM_PLAYER_WALK_LEFT
		!byte $08
		!byte $48, $49, $4a, $49, $00
		
ANIM_PLAYER_WALK_RIGHT
		!byte $08
		!byte $45, $46, $47, $46, $00
		
ANIM_PLAYER_TALK
		!byte $06
		!byte $4e, $4f, $00
		
ANIM_PLAYER_DANCE
		!byte $0a							;Timer
		!byte $50, $51, $52, $51, $00 		;Frame List
		

;================================================
;	SHEEP ANIMATION DATA
;================================================

ANIM_SHEEP_STAND_LEFT
		!byte $01
		!byte $6c, $00
		
ANIM_SHEEP_STAND_RIGHT
		!byte $01
		!byte $68, $00
		
ANIM_SHEEP_WALK_LEFT
		!byte $06
		!byte $72, $6c, $73, $6c, $00
		
ANIM_SHEEP_WALK_RIGHT
		!byte $06
		!byte $70, $68, $71, $68, $00
		
ANIM_SHEEP_EAT_LEFT
		!byte $08
		!byte $6c, $6d, $6e, $6f, $6e, $6d, $00

ANIM_SHEEP_EAT_RIGHT
		!byte $08
		!byte $68, $69, $6a, $6b, $6a, $69, $00
		
;====================================================
;	LEVEL DATA
;====================================================

DATA_OVERWORLD_MAP
        !byte $00, $01, $02, $03
        !byte $04, $05, $06, $07
        !byte $08, $09, $0a, $0b
        !byte $0c, $0d, $0e, $0f

DATA_LEVEL_POINTERS
        !word DATA_LEVEL_0_POINTER
        !word DATA_LEVEL_1_POINTER
        !word DATA_LEVEL_2_POINTER
        !word DATA_LEVEL_3_POINTER
        !word DATA_LEVEL_4_POINTER
        !word DATA_LEVEL_5_POINTER
        !word DATA_LEVEL_6_POINTER
        !word DATA_LEVEL_7_POINTER
        !word DATA_LEVEL_8_POINTER
        !word DATA_LEVEL_9_POINTER
        !word DATA_LEVEL_10_POINTER
        !word DATA_LEVEL_11_POINTER
        !word DATA_LEVEL_12_POINTER
        !word DATA_LEVEL_13_POINTER
        !word DATA_LEVEL_14_POINTER
        !word DATA_LEVEL_15_POINTER
        !word 0

DATA_LEVEL_0_POINTER
        !word DATA_LEVEL_0                  ;Tile Data
        !word DATA_LEVEL_0 + 220            ;Palette Data
        !word DATA_LEVEL_0 + 440            ;Attribute Flag Data
        !word DATA_LEVEL_0 + 660            ;Sprite Data
        !word DATA_LEVEL_0 + 716            ;Door Data
DATA_LEVEL_1_POINTER
        !word DATA_LEVEL_1
        !word DATA_LEVEL_1 + 220
        !word DATA_LEVEL_1 + 440
        !word DATA_LEVEL_1 + 660
        !word DATA_LEVEL_1 + 716
DATA_LEVEL_2_POINTER
        !word DATA_LEVEL_2
        !word DATA_LEVEL_2 + 220
        !word DATA_LEVEL_2 + 440 
        !word DATA_LEVEL_2 + 660
        !word DATA_LEVEL_2 + 716
DATA_LEVEL_3_POINTER
        !word DATA_LEVEL_3
        !word DATA_LEVEL_3 + 220
        !word DATA_LEVEL_3 + 440 
        !word DATA_LEVEL_3 + 660
        !word DATA_LEVEL_3 + 716

DATA_LEVEL_4_POINTER
        !word DATA_LEVEL_4
        !word DATA_LEVEL_4 + 220
        !word DATA_LEVEL_4 + 440 
        !word DATA_LEVEL_4 + 660
        !word DATA_LEVEL_4 + 716

DATA_LEVEL_5_POINTER
        !word DATA_LEVEL_5
        !word DATA_LEVEL_5 + 220
        !word DATA_LEVEL_5 + 440 
        !word DATA_LEVEL_5 + 660
        !word DATA_LEVEL_5 + 716

DATA_LEVEL_6_POINTER
        !word DATA_LEVEL_6
        !word DATA_LEVEL_6 + 220
        !word DATA_LEVEL_6 + 440 
        !word DATA_LEVEL_6 + 660
        !word DATA_LEVEL_6 + 716

DATA_LEVEL_7_POINTER
        !word DATA_LEVEL_7
        !word DATA_LEVEL_7 + 220
        !word DATA_LEVEL_7 + 440 
        !word DATA_LEVEL_7 + 660
        !word DATA_LEVEL_7 + 716

DATA_LEVEL_8_POINTER
        !word DATA_LEVEL_8
        !word DATA_LEVEL_8 + 220
        !word DATA_LEVEL_8 + 440 
        !word DATA_LEVEL_8 + 660
        !word DATA_LEVEL_8 + 716

DATA_LEVEL_9_POINTER
        !word DATA_LEVEL_9
        !word DATA_LEVEL_9 + 220
        !word DATA_LEVEL_9 + 440 
        !word DATA_LEVEL_9 + 660
        !word DATA_LEVEL_9 + 716

DATA_LEVEL_10_POINTER
        !word DATA_LEVEL_10
        !word DATA_LEVEL_10 + 220
        !word DATA_LEVEL_10 + 440 
        !word DATA_LEVEL_10 + 660
        !word DATA_LEVEL_10 + 716

DATA_LEVEL_11_POINTER
        !word DATA_LEVEL_11
        !word DATA_LEVEL_11 + 220
        !word DATA_LEVEL_11 + 440 
        !word DATA_LEVEL_11 + 660
        !word DATA_LEVEL_11 + 716

DATA_LEVEL_12_POINTER
        !word DATA_LEVEL_12
        !word DATA_LEVEL_12 + 220
        !word DATA_LEVEL_12 + 440 
        !word DATA_LEVEL_12 + 660
        !word DATA_LEVEL_12 + 716

DATA_LEVEL_13_POINTER
        !word DATA_LEVEL_13
        !word DATA_LEVEL_13 + 220
        !word DATA_LEVEL_13 + 440 
        !word DATA_LEVEL_13 + 660
        !word DATA_LEVEL_13 + 716

DATA_LEVEL_14_POINTER
        !word DATA_LEVEL_14
        !word DATA_LEVEL_14 + 220
        !word DATA_LEVEL_14 + 440 
        !word DATA_LEVEL_14 + 660
        !word DATA_LEVEL_14 + 716

DATA_LEVEL_15_POINTER
        !word DATA_LEVEL_15
        !word DATA_LEVEL_15 + 220
        !word DATA_LEVEL_15 + 440 
        !word DATA_LEVEL_15 + 660
        !word DATA_LEVEL_15 + 716



DATA_LEVEL_0
        !source "testlvls/level0test.txt"
DATA_LEVEL_1
        !source "testlvls/level1test.txt"
DATA_LEVEL_2
        !binary "testlvls/levelc.lvl"
DATA_LEVEL_3
        !binary "testlvls/leveld.lvl"
DATA_LEVEL_4
        !binary "testlvls/levele.lvl"
DATA_LEVEL_5
        !binary "testlvls/levelf.lvl"
DATA_LEVEL_6
        !binary "testlvls/levelg.lvl"
DATA_LEVEL_7
        !binary "testlvls/levelh.lvl"
DATA_LEVEL_8
        !binary "testlvls/leveli.lvl"
DATA_LEVEL_9
        !binary "testlvls/levelj.lvl"
DATA_LEVEL_10
        !binary "testlvls/levelk.lvl"
DATA_LEVEL_11
        !binary "testlvls/levell.lvl"
DATA_LEVEL_12
        !binary "testlvls/levelm.lvl"
DATA_LEVEL_13
        !binary "testlvls/leveln.lvl"
DATA_LEVEL_14
        !binary "testlvls/levelo.lvl"
DATA_LEVEL_15
        !binary "testlvls/levelp.lvl"

;=========================================================================
;	EXTERNAL DATA
;=========================================================================

CHARSET_DATA
		!binary "graphics/rpgset1.chr"

SPRITE_DATA
		!binary "graphics/sprites.spr"

!sl "test.txt"
