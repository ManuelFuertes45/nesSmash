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

; PPU registers with their memory address
PPUCONTROL  = $2000
PPUMASK     = $2001
PPUSTATUS   = $2002
PPUADDR     = $2006
PPUDATA     = $2007
OAMADDR     = $2003
OAMDMA      = $4014

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
  LDA #$00

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




.segment "RODATA"
palettes:
  ; Background Palette
  .byte $0f, $00, $10, $30 ;P0
  .byte $0f, $00, $27, $27 ;P1
  .byte $0f, $17, $12, $30 ;P2
  .byte $0f, $1c, $2b, $39 ;P3

  ; Load your Sprite Palette data from the .chr file here
  .byte $0f, $00, $10, $30 ;P0
  .byte $0f, $17, $12, $30 ;P1
  .byte $0f, $1c, $2b, $39 ;P2
  .byte $0f, $00, $27, $30 ;P3

background:

	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$22,$23,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$81,$82,$83,$00
	.byte $00,$20,$21,$32,$33,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$20,$21,$00,$00,$00,$90,$01,$92,$93,$00
    .byte $00,$30,$31,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$30,$31,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
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
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$af,$af,$af,$af,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$55,$55,$55,$55,$00,$a0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.segment "VECTORS"
	.word NMI ; addr nmi_handler og
	.word RESET ; addr reset_handler og
  .word IRQ ; addr irq_handler og
	; specialized hardware interurpts

; Character memory
.segment "CHARS"
.incbin "rom.chr"