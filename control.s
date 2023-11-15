.segment "HEADER"
  .byte "NES"		;identification string
	.byte $1A
	.byte $02		;amount of PRG ROM in 16K units
	.byte $01		;amount of CHR ROM in 8K units
	.byte $00		;mapper and mirroing
	.byte $00, $00, $00, $00
	.byte $00, $00, $00, $00, $00

.segment "STARTUP"

.segment "ZEROPAGE"
playerX: .res 1 ; Player 1 X coordinate
playerY: .res 1 ; Player 1 Y coordinate
playerPunching: .res 1 ; In the hex editor 1 represents punching state, 0 is not punching
movingPlayer: .res 1 ; In the hex editor 1 represents that the player is moving, 0 represents idle state
playerOrientation: .res 1 ; In the hex editor, 0 indicates left, 1 indicates right
playerAerial: .res 1 ; In the hex editor, grounded is 0, flying is 1
playerAscending: .res 1 ; 0 indicates that is descending and 1 indicates that is ascending
jumpCounter: .res 1
frameCounter: .res 1
timer: .res 1
pad1: .res 1 ; Button input states for Player 1

; PPU registers with their memory address
PPUCONTROL  = $2000
PPUMASK     = $2001
PPUSTATUS   = $2002
PPUADDR     = $2006
PPUDATA     = $2007
OAMADDR     = $2003
OAMDMA      = $4014

CONTROLLER1 = $4016 ; Memory address that will handle controller1 button states
btnRIGHT   = %00000001
btnLEFT    = %00000010
btnDOWN    = %00000100 ; not used
btnUP      = %00001000 ; not used
btnSTART   = %00010000 ; not used
btnSELECT  = %00100000 ; not used
btnB       = %01000000
btnA       = %10000000

.segment "CODE"

; Interrupt request handler
; Handles interrupts from external devices
.proc IRQ
  RTI
.endproc

; NMI handler is responsible for updating the player's information and drawing the player on the screen
.proc NMI
  LDA #$00
  STA OAMADDR
  LDA #$02
  STA OAMDMA
  LDA #$00

  ; read controller
  JSR readController1
  ; update character
  JSR updatePlayer
  ; draw character
  JSR draw

  STA $2005
  STA $2005
  RTI
.endproc

; Initializes the program when the reset button is pressed
.proc RESET
  SEI ; disables interrupts during reset
  CLD
  LDX #$00
  STX PPUCONTROL
  STX PPUMASK

vblankwait:
  BIT PPUSTATUS
  BPL vblankwait

	LDX #$00
	LDA #$ff
clearOam:
	STA $0200,X ; set sprite y-positions off the screen
	INX
	INX
	INX
	INX
	BNE clearOam

vblankwait2:
	BIT PPUSTATUS
	BPL vblankwait2

	; initialize zero-page values
	LDA #$68 ; X starting player position
	STA playerX
	LDA #$af ; Y starting player position
	STA playerY
    LDA #$00
    STA playerPunching
    STA movingPlayer
    STA playerOrientation
    STA playerAerial
    STA jumpCounter
    STA frameCounter
    STA timer
    JMP main
.endproc

.proc main
  ; write a palette
  LDX PPUSTATUS
  LDX #$3f
  STX PPUADDR
  LDX #$00
  STX PPUADDR

load_palettes:
  lda $2002
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
  ldx #$00
@loop:
  lda palettes, x
  sta $2007
  inx
  cpx #$20
  bne @loop

LoadBackground:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$20
  STA $2006             ; write the high byte ($20) of $2000 address
  LDA #$00
  STA $2006             ; write the low byte ($00) of $2000 address
  LDX #$00              ; start out at 0
LoadBackgroundLoop:
  LDA background, x       ; load data from address (background + the value in x)
  STA $2007               ; write to PPU
  INX                     ; X++
  BNE LoadBackgroundLoop  ; Branch to LoadBackgroundLoop if compare was Not Equal to zero
                          ; if compare was equal to 00, keep going down
  LDX #$00                ; start out at 0
LoadBackgroundLoop2:      ; Do this one more time for the next 256 bytes
  LDA background+256, x   
  STA $2007               
  INX
  BNE LoadBackgroundLoop2 
                          
  LDX #$00                
LoadBackgroundLoop3:      ; Do this one more time for the next 256 bytes
  LDA background+512, x   
  STA $2007               
  INX
  BNE LoadBackgroundLoop3
                          
  LDX #$00            
LoadBackgroundLoop4:      ; Do this one more time for the next 256 bytes
  LDA background+768, x   
  STA $2007               
  INX
  BNE LoadBackgroundLoop4


LoadAttribute:
  LDA $2002             ; read PPU status to reset the high/low latch
  LDA #$23
  STA $2006             ; write the high byte of $23C0 address
  LDA #$C0
  STA $2006             ; write the low byte of $23C0 address
  LDX #$00              ; start out at 0
LoadAttributeLoop:
  LDA attributes, x      ; load data from address (attribute + the value in x)
  STA $2007             ; write to PPU
  INX                   ; X = X + 1
  CPX #$08              ; Compare X to hex $08, decimal 8 - copying 8 bytes
  BNE LoadAttributeLoop  ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                        ; if compare was equal to 128, keep going down


vblankwait:       ; wait for another vblank before continuing
  BIT PPUSTATUS
  BPL vblankwait

  LDA #%10010000  ; turn on NMIs, sprites use first pattern table
  STA PPUCONTROL
  LDA #%00011110  ; turn on screen
  STA PPUMASK

running: ; keeps program running indefinitely
  JMP running
.endproc


.proc readController1
  PHA ; Pushes the accumulator register (A) onto the stack.
  TXA ; Transfers the value from the X register to the accumulator.
  PHA ; Pushes the value from the X register onto the stack.
  PHP ; Pushes the processor status register onto the stack.
  ; This is a common practice at the beginning of a subroutine to preserve the state of registers.

  ; write a 1, then a 0, to CONTROLLER1
  ; to latch button states
  ; When you press a button on the controller, the code writes 1 to the 
  ; memory address associated with the controller (CONTROLLER1).
  ; The act of writing 1 causes the controller to capture the current 
  ; state of all buttons. It's like taking a snapshot of which buttons 
  ; are pressed and which ones are not at that specific moment.
  ; After capturing the button states, the code writes 0 to the same 
  ; memory address (CONTROLLER1). This resets the latch in preparation 
  ; for the next read. Now the controller is ready to capture the button 
  ; states again.
  LDA #$01
  STA CONTROLLER1
  LDA #$00
  STA CONTROLLER1

  LDA #%00000001
  STA pad1

getButtons:
  LDA CONTROLLER1 ; Read next button's state
  LSR A           ; Shift button state right, into carry flag
  ROL pad1        ; Rotate button state from carry flag
                  ; onto right side of pad1
                  ; and leftmost 0 of pad1 into carry flag
  BCC getButtons ; Continue until original "1" is in carry flag

  PLP
  PLA
  TAX
  PLA
  RTS
.endproc

.proc draw
  PHP 
  PHA 
  TXA
  PHA
  TYA
  PHA

  ; Drawing the character.
  
  LDX playerOrientation
  LDY playerAerial
  CPY #$00
  BEQ walkingAnim

  jumpingAnim:
    JSR verticalStatus
    CPX #$01
    BEQ jumpingRight

    jumpingLeft:
      JSR strayLeft
      JMP updating


    jumpingRight:
      JSR strayRight
      JMP updating


  walkingAnim:
    CPX #$01
    BEQ generalStatusRight


    generalStatusLeft:
      LDY movingPlayer
      CPY #$01
      BEQ walkingLeft
      LDY playerPunching
      CPY #$00
      BEQ standingLeft

    punchingLeft:
      JSR leftPunch
      JMP updating


    walkingLeft:
      JSR walkLeft
      JMP updating

    standingLeft:
      JSR idle_left
      JMP updating

    generalStatusRight:
      LDY movingPlayer
      CPY #$01
      BEQ walkingRight
      LDY playerPunching
      CPY #$00
      BEQ standingRight


    punchingRight:
      JSR rightPunch
      JMP updating

    walkingRight:
      JSR walkRight
      JMP updating

    standingRight:
      JSR idle_right
    

  updating:
    JSR updateLocation


  PLA ; Done with updates, restore registers
  TAY ; and return to where we called this
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc updateLocation
  ; store tile locations
  ; top left tile:
  LDA playerY
  STA $0200
  LDA playerX
  STA $0203

  ; top right tile (x + 8):
  LDA playerY
  STA $0204
  LDA playerX
  CLC
  ADC #$08 ; 08 og
  STA $0207

  ; bottom left tile (y + 8):
  LDA playerY
  CLC
  ADC #$08 ; 08 og
  STA $0208
  LDA playerX
  STA $020b

  ; bottom right tile (x + 8, y + 8)
  LDA playerY
  CLC
  ADC #$08 ; 08 og
  STA $020c
  LDA playerX
  CLC
  ADC #$08 ; 08 og
  STA $020f
  RTS
.endproc


.proc verticalStatus
  PHP  ; Start by saving registers,
  PHA  ; as usual.
  TXA
  PHA
  TYA
  PHA

  LDA playerAscending
  CMP #$00
  BEQ inc_y

  dec_y:
    LDA playerY
    CMP #$03 ; jump height
    BEQ go_down
    LDA jumpCounter
    CMP #$30 ; jump height
    BEQ go_down

    DEC playerY
    INC jumpCounter
    JMP end

    go_down:
      LDA #$00
      STA playerAscending
      JMP end

  inc_y:
    JSR check_falling_collision
    LDA jumpCounter
    CMP #$00
    BEQ end_jump_anim

    INC playerY
    JMP end

    end_jump_anim:
      LDA #$00
      STA jumpCounter
      STA playerAerial

  end:

  PLA ; Done with updates, restore registers
  TAY ; and return to where we called this
  PLA
  TAX
  PLA
  PLP

  RTS
.endproc


.proc check_falling_collision
  PHP  ; Start by saving registers,
  PHA  ; as usual.
  TXA
  PHA
  TYA
  PHA

  LDX playerY
  CPX #$af ; AF og player positions
  BEQ reset

  LDX playerY
  CPX #$97 ; 58 og platform height
  BNE end

  LDY playerX
  CPY #$9c ; BD og right platform corner
  BCS end

  CPY #$56 ; 37 og left platform corner
  BCC end

  reset:
    LDY #$00
    STY playerAerial
    STY jumpCounter

  end:

  PLA ; Done with updates, restore registers
  TAY ; and return to where we called this
  PLA
  TAX
  PLA
  PLP

  RTS
.endproc


.proc strayLeft
  ; write player ship tile numbers
  LDA #$0a
  STA $0201
  LDA #$09
  STA $0205
  LDA #$1a
  STA $0209
  LDA #$19
  STA $020d
  ; write player ship tile attributes
  ; use palette 0
  LDA #$42
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  RTS
.endproc

.proc strayRight
  ; write player ship tile numbers
  LDA #$09
  STA $0201
  LDA #$0a
  STA $0205
  LDA #$19
  STA $0209
  LDA #$1a
  STA $020d
  ; write player ship tile attributes
  ; use palette 0
  LDA #$02
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  RTS
.endproc

.proc leftPunch
  ; write player ship tile numbers
  LDA #$0e
  STA $0201
  LDA #$0d
  STA $0205
  LDA #$1e
  STA $0209
  LDA #$1d
  STA $020d

  ; write player ship tile attributes
  ; use palette 0
  LDA #$42
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  
  RTS
.endproc

.proc rightPunch
  ; write player ship tile numbers
  LDA #$0d
  STA $0201
  LDA #$0e
  STA $0205
  LDA #$1d
  STA $0209
  LDA #$1e
  STA $020d

  ; write player ship tile attributes
  ; use palette 0
  LDA #$02
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  RTS
.endproc

.proc walkLeft

  ; timer delay for frames
  LDA timer
  CMP #$03
  BEQ left_cycle
  INC timer
  JMP updateLocation
  

  left_cycle:
  ; reset timer
  LDA #$00
  STA timer

  ;checking which animation should be displayed
  LDX frameCounter
  cpx #$00
  beq left_cycle1

  LDX frameCounter
  cpx #$01
  beq left_cycle2

  LDX frameCounter
  cpx #$02
  beq left_cycle3

  left_cycle1:
    LDA #$04
    STA $0201
    LDA #$03
    STA $0205
    LDA #$14
    STA $0209
    LDA #$13
    STA $020d

    LDA #$42
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$01
    STX frameCounter
    JMP updateLocation
    
  left_cycle2:
    LDA #$06
    STA $0201
    LDA #$05
    STA $0205
    LDA #$16
    STA $0209
    LDA #$15
    STA $020d
    
    LDA #$42
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$02
    STX frameCounter
    JMP updateLocation
    
  left_cycle3:
    LDA #$08
    STA $0201
    LDA #$07
    STA $0205
    LDA #$18
    STA $0209
    LDA #$17
    STA $020d

    LDA #$42
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$00
    STX frameCounter
  RTS
.endproc

.proc walkRight

  ; timer delay for frames
  LDA timer
  CMP #$03
  BEQ right_cycle
  INC timer
  JMP updateLocation
  

  right_cycle:
  ; reset timer
  LDA #$00
  STA timer

  ;checking which animation should be displayed
  LDX frameCounter
  cpx #$00
  beq right_cycle1

  LDX frameCounter
  cpx #$01
  beq right_cycle2

  LDX frameCounter
  cpx #$02
  beq right_cycle3

  right_cycle1:
    LDA #$03
    STA $0201
    LDA #$04
    STA $0205
    LDA #$13
    STA $0209
    LDA #$14
    STA $020d

    LDA #$02
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$01
    STX frameCounter
    JMP updateLocation
    
  right_cycle2:
    LDA #$05
    STA $0201
    LDA #$06
    STA $0205
    LDA #$15
    STA $0209
    LDA #$16
    STA $020d
    
    LDA #$02
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$02
    STX frameCounter
    JMP updateLocation
    
  right_cycle3:
    LDA #$07
    STA $0201
    LDA #$08
    STA $0205
    LDA #$17
    STA $0209
    LDA #$18
    STA $020d

    LDA #$02
    STA $0202
    STA $0206
    STA $020a
    STA $020e

    LDX #$00
    STX frameCounter
  RTS
.endproc


.proc idle_left
  ; write player ship tile numbers
  LDA #$02
  STA $0201
  LDA #$01
  STA $0205
  LDA #$12
  STA $0209
  LDA #$11
  STA $020d

  ; write player ship tile attributes
  ; use palette 0
  LDA #$42
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  RTS
.endproc


.proc idle_right
  ; write player ship tile numbers
  LDA #$01
  STA $0201
  LDA #$02
  STA $0205
  LDA #$11
  STA $0209
  LDA #$12
  STA $020d

  ; write player ship tile attributes
  ; use palette 0
  LDA #$02
  STA $0202
  STA $0206
  STA $020a
  STA $020e
  RTS
.endproc



.proc updatePlayer
  PHP  ; Start by saving registers,
  PHA  ; as usual.
  TXA
  PHA
  TYA
  PHA

  ;JSR move_down       ; cant put it cuz it messes with the timer delay, need to find another way for the default sprite to be stand

check_a:
  LDA pad1
  AND #btnA
  BEQ check_b

  up_collision:
    LDX playerY
    CPX #$07     ;coord is (0,7)
    BEQ check_b

  on_air:
    LDX playerAerial
    CPX #$01
    BEQ check_b

  JSR move_up

check_b:
  LDA pad1
  AND #btnB
  BEQ check_left
  JSR punch

check_left:
  LDA pad1        ; Load button presses
  AND #btnLEFT   ; Filter out all but Left
  BEQ check_right ; If result is zero, left not pressed

  left_collision:
    LDX playerX
    CPX #$00         ;coord is (0,af)
    BEQ check_right

  ; If the branch is not taken, move player left
  JSR move_left

check_right:
  LDA pad1
  AND #btnRIGHT
  BEQ check_movement

    right_collision:
    LDX playerX
    CPX #$F0         ; coord is (f0,af)
    BEQ check_movement

  JSR move_right
  
check_movement:
  LDA pad1
  AND #$43
  BNE done_checking
  LDA #$00
  STA movingPlayer
  LDA pad1
  AND #$40
  BNE done_checking
  LDA #$00
  STA playerPunching

done_checking:
  PLA ; Done with updates, restore registers
  TAY ; and return to where we called this
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc move_up
 ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDX playerAerial
  CPX #$01
  BEQ end
  LDX #$01
  STX playerAerial
  STX playerAscending
  LDX #$00
  STX jumpCounter
  DEC playerY


  end:
  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc


.proc move_down
  ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDY playerOrientation
  CPY #$00
  BNE down_right

  down_left:
  ; write player ship tile numbers
  LDA #$02
  STA $0201
  LDA #$01
  STA $0205
  LDA #$12
  STA $0209
  LDA #$11
  STA $020d

  ; write player ship tile attributes
  ; use palette 0
  LDA #$42
  STA $0202
  STA $0206
  STA $020a
  STA $020e

  JMP other_code

  down_right:
  ; write player ship tile numbers
  LDA #$01
  STA $0201
  LDA #$02
  STA $0205
  LDA #$11
  STA $0209
  LDA #$12
  STA $020d

  ; write player ship tile attributes
  ; use palette 0
  LDA #$02
  STA $0202
  STA $0206
  STA $020a
  STA $020e

other_code:
  ; store tile locations
  ; top left tile:
  LDA playerY
  STA $0200
  LDA playerX
  STA $0203

  ; top right tile (x + 8):
  LDA playerY
  STA $0204
  LDA playerX
  CLC
  ADC #$08
  STA $0207

  ; bottom left tile (y + 8):
  LDA playerY
  CLC
  ADC #$08
  STA $0208
  LDA playerX
  STA $020b

  ; bottom right tile (x + 8, y + 8)
  LDA playerY
  CLC
  ADC #$08
  STA $020c
  LDA playerX
  CLC
  ADC #$08
  STA $020f

  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc move_left
  ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA playerAerial
  CMP #$00
  BNE end

  check_edge:
    LDA playerY
    CMP #$97 ; 58 og
    BNE end 

    ; Storing player direction

    platform_fall:
      LDA playerX
      CMP #$56 ; 36 og
      BCS end

      set_fall:
        LDY #$01
        STY playerAerial
        STY jumpCounter
        LDY #$00
        STY playerAscending

  end:
    DEC playerX
    LDY #$00
    STY playerOrientation
    LDY #$01
    STY movingPlayer
  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS

.endproc


.proc move_right
   ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA

  LDA playerAerial
  CMP #$00
  BNE end

  check_edge:
    LDA playerY
    CMP #$97 ; 58 og
    BNE end 

    platform_fall:
      LDA playerX
      CMP #$9c ; BD og
      BCC end

      set_fall:
        LDY #$01
        STY playerAerial
        STY jumpCounter
        LDY #$00
        STY playerAscending

  end:
    INC playerX
    LDY #$01
    STY playerOrientation
    STY movingPlayer
  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.proc punch
  ; save registers
  PHP
  PHA
  TXA
  PHA
  TYA
  PHA


  LDY #$01
  STY playerPunching


  ; restore registers and return
  PLA
  TAY
  PLA
  TAX
  PLA
  PLP
  RTS
.endproc

.segment "RODATA"
palettes:
  ; Background Palette
  .byte $0f, $00, $10, $30 ;P0
  .byte $0f, $00, $27, $27 ;P1
  .byte $0f, $17, $12, $30 ;P2
  .byte $0f, $00, $00, $00 ;P3

  ; Load your Sprite Palette data from the .chr file here
  .byte $0f, $00, $10, $30 ;P0
  .byte $0f, $17, $12, $30 ;P1
  .byte $0f, $1c, $2b, $39 ;P2
  .byte $0f, $00, $27, $30 ;P3  

background:

	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$22,$23,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$20,$21,$32,$33,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$30,$31,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$20,$21,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$30,$31,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$26,$27,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$36,$37,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$26,$27,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$36,$37,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$06,$07,$00,$00,$00,$00,$60,$61,$62,$63,$00,$00,$00,$00,$00,$00,$00,$00,$6c,$6d,$6e,$6f,$00,$2a,$2b,$00,$00,$28,$29,$00
	.byte $00,$00,$16,$17,$1e,$1f,$00,$00,$70,$71,$72,$73,$00,$00,$00,$00,$00,$00,$00,$00,$7c,$7d,$7e,$7f,$00,$3a,$3b,$00,$00,$38,$39,$00
	.byte $04,$05,$0c,$0d,$2e,$2f,$08,$09,$04,$05,$04,$05,$02,$03,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$04,$05,$09,$04,$05
	.byte $14,$15,$1c,$1d,$3e,$3f,$18,$19,$14,$15,$14,$15,$12,$13,$14,$15,$14,$15,$14,$15,$14,$15,$14,$15,$14,$15,$1c,$04,$05,$19,$14,$15
	.byte $04,$05,$02,$03,$04,$02,$03,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$04,$05,$02,$03,$04,$05,$04,$05,$04,$05,$04,$05
	.byte $14,$15,$12,$13,$14,$12,$13,$15,$14,$15,$14,$15,$14,$15,$14,$15,$14,$15,$14,$15,$14,$15,$12,$13,$14,$15,$14,$15,$14,$15,$14,$15
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


attributes:
  .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$55,$55,$55,$55,$00,$a0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.segment "VECTORS"
	.word NMI ; addr nmi_handler og
	.word RESET ; addr reset_handler og
  .word IRQ ; addr irq_handler og
	; specialized hardware interurpts

; Character memory
.segment "CHARS"
.incbin "rom.chr"