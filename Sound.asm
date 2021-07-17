; =========================================================
; Module: Sound
; =========================================================
; Handles making sound
; =========================================================
.module Sound

PSG = $7F

; Each channel needs to keep track of:
; 0 -> state
;      bits 0 and 1 = section/pitch envelope
;          00 = idle (channel free)
;          01 = section 1
;          10 = section 2
;          11 = section 3
;      bits 2 and 3 = ADSR state
;          00 = releasing
;          01 = attacking
;          10 = decaying
;          11 = sustaining
; 1 -> amplitude
;      from 0 (quietest) to 127 (loudest)
; 2 -> pitch
;      from 0 to 255
; 3 -> duration (byte value) in 1/20 of a second
;      decremented every 5 centiseconds
; 4 -> pitch envelope step counter
; 5 -> amplitude envelope step counter
; 6 -> current envelope state
;      (13 bytes)
; = 20 bytes per channel

ChannelCount = 4
ChannelSize = 20
Channels = allocVar(ChannelSize * ChannelCount)

Channel.State = 0
Channel.Amplitude = 1
Channel.Pitch = 2
Channel.Duration = 3
Channel.PitchStep = 4
Channel.AmplitudeStep = 5
Channel.Envelope = 6

Envelope.T = 0 ; 0 to 127 Length of each step in hundredths of a second
Envelope.PI1 = 1 ; -128 to 127 Change of pitch per step in section 1
Envelope.PI2 = 2 ; -128 to 127 Change of pitch per step in section 2
Envelope.PI3 = 3 ; -128 to 127 Change of pitch per step in section 3
Envelope.PN1 = 4 ; 0 to 255 Number of steps in section 1
Envelope.PN2 = 5 ; 0 to 255 Number of steps in section 2
Envelope.PN3 = 6 ; 0 to 255 Number of step in section 3
Envelope.AA = 7 ; -127 to 127 Change of amplitude per step during attack phase
Envelope.AD = 8 ; -127 to 127 Change of amplitude per step during decay phase
Envelope.AS = 9 ; -127 to 0 Change of amplitude per step during sustain phase
Envelope.AR = 10 ; -127 to 0 Change of amplitude per step during release phase
Envelope.ALA = 11 ; 0 to 126 Target level at end of attack phase
Envelope.ALD = 12 ; 0 to 126 Target level at end of decay phase

EnvelopeCount = 2
EnvelopeSize = 13
Envelopes = allocVar(EnvelopeSize * EnvelopeCount)

ChannelUpdateTimer = allocVar(1)
ChannelUpdatePeriod = 5

Reset:
	ld a,ChannelUpdatePeriod
	ld (ChannelUpdateTimer),a
	
	; Clear the channels.
	xor a
	ld hl,Channels
	ld de,Channels + 1
	ld bc,(ChannelSize * ChannelCount) - 1
	ld (hl),a
	ldir
	
	; ...and the envelopes.
	ld hl,Envelopes
	ld de,Envelopes + 1
	ld bc,(EnvelopeSize * EnvelopeCount) - 1
	ld (hl),a
	ldir
	
	ret

Tick:
	push ix
	push hl
	push de
	push bc
	
	; Is it time to update the channel note queue?
	ld a,(ChannelUpdateTimer)
	dec a
	jr z,TickUpdateChannelNotes
	ld (ChannelUpdateTimer),a
	
	; We still need to update the envelopes.
	jp TickEnvelopes

TickUpdateChannelNotes:
	ld a,ChannelUpdatePeriod
	ld (ChannelUpdateTimer),a
		
	ld ix,Channels
	ld b,ChannelCount

TickNextChannelNote:
	
	; What's the current channel state?
	ld a,(ix+Channel.State)
	or a
	jr z,ChannelNoteInactive
	
	;Have we reached the end of its duration?
	ld a,(ix+Channel.Duration)
	or a
	jr nz,TickNotEndedNoteDuration
	
	; The note has finished, so change channel state to "release".
	ld a,(ix+Channel.State)
	and %11110000
	ld (ix+Channel.State),a
	
	xor a
	jr TickEndedNoteDuration
	
TickNotEndedNoteDuration:
	dec a
TickEndedNoteDuration:
	ld (ix+Channel.Duration),a
	
ChannelNoteInactive:

	; Advance to the next channel.
	ld de,ChannelSize	
	add ix,de
	djnz TickNextChannelNote

	; Now we need to update the envelopes.

TickEnvelopes:
	
	ld ix,Channels
	ld b,ChannelCount
	ld c,0 ; channel 0
	
TickNextEnvelope:

	push bc
	
	; Is it time to advance to the next step?
	ld a,(ix+Channel.AmplitudeStep)
	ld d,a
	and %01111111
	jr nz,+
	
	; Step the envelope
	call StepEnvelope
	jr EnvelopeStepped
	
+:
	; It's not time, so decrement the counter
	; and skip the envelope tick.
	dec a
	ld e,a
	ld a,d
	and %10000000
	or e
	
	ld (ix+Channel.AmplitudeStep),a

EnvelopeStepped:

	; Send the current channel state to the sound chip.
	ld a,c
	ld e,(ix+Channel.Pitch)
	ld l,(ix+Channel.Amplitude)
		
	call UpdateChannel
	
	pop bc
	
	; Advance to the next channel.
	ld de,ChannelSize
	add ix,de
	inc c
	djnz TickNextEnvelope
	
	pop bc
	pop de
	pop hl
	pop ix
	ret


StepEnvelope:
	
	; We're ticking now, so set the step count to the envelope's
	; T value to schedule the next update.
	ld a,(ix+Channel.Envelope+Envelope.T)
	ld (ix+Channel.AmplitudeStep),a

	; Amplitude envelope.
	ld a,(ix+Channel.State)
	and %00001100
	jr z,StepEnvelopeReleasing
	
	srl a
	srl a
	
	dec a
	jr z,StepEnvelopeAttacking
	
	dec a
	jr z,StepEnvelopeDecaying

StepEnvelopeSustaining:

	; Sustain envelope.
	ld a,(ix+Channel.Envelope+Envelope.AS)
	ld e,0
	
	call AdvanceChannelEnvelope
	or $FF
	jr nz,StepEnvelopeDoneAmplitude
	
	; Move to the releasing state.
	ld a,(ix+Channel.State)
	and  %11110011
	ld (ix+Channel.State),a

	jr StepEnvelopeDoneAmplitude

StepEnvelopeDecaying:

	; Decay envelope.
	ld a,(ix+Channel.Envelope+Envelope.AD)
	ld e,(ix+Channel.Envelope+Envelope.ALD)
	
	call AdvanceChannelEnvelope
	
	jr nz,StepEnvelopeDoneAmplitude
	
	; Move to the sustaining state.
	ld a,(ix+Channel.State)
	or  %00001100
	ld (ix+Channel.State),a
	
	jr StepEnvelopeDoneAmplitude
	
StepEnvelopeAttacking:
	
	; Attack envelope.
	ld a,(ix+Channel.Envelope+Envelope.AA)
	ld e,(ix+Channel.Envelope+Envelope.ALA)
	
	call AdvanceChannelEnvelope
	
	jr nz,StepEnvelopeDoneAmplitude
	
	; Move to the decaying state.
	ld a,(ix+Channel.State)
	and %11110011
	or  %00001000
	ld (ix+Channel.State),a
	
	jr StepEnvelopeDoneAmplitude
	
StepEnvelopeReleasing:

	; Release envelope.
	ld a,(ix+Channel.Envelope+Envelope.AR)
	ld e,0
	call AdvanceChannelEnvelope

StepEnvelopeDoneAmplitude:

	; Advance pitch.
	ret

; ---------------------------------------------------------
; AdvanceChannelEnvelope -> Advances a channel envelope
; ---------------------------------------------------------
; Inputs:   ix = pointer to channel
; Outputs:  z set if threshold reached/passed, nz if not.
; Destroys: af, de.
; ---------------------------------------------------------
AdvanceChannelEnvelope:
	ld d,(ix+Channel.Amplitude)
	call AdvanceEnvelopeValue
	ld (ix+Channel.Amplitude),a
	ret

; ---------------------------------------------------------
; AdvanceEnvelope -> Advances a value by a delta.
; ---------------------------------------------------------
; Inputs:   a = delta (-128..127)
;           d = value
;           e = threshold
; Outputs:  a = amended value.
;           z set if threshold reached/passed, nz if not.
; Destroys: af.
; ---------------------------------------------------------
AdvanceEnvelopeValue:
	or a
	jr z,MaintainEnvelopeValue
	jp p,IncrementEnvelopeValue
	jp DecrementEnvelopeValue

MaintainEnvelopeValue: ; d = 0
	ld a,d
	cp e
	ret
	
IncrementEnvelopeValue: ; d > 0
	add a,d
	
	; Clamp from 0..255
	jr nc,+
	ld a,255
	jp p,+
	xor a
+:
	; Check if we've passed the threshold.
	cp e
	ret c
	
	; Set to the treshold and set z.
	ld a,e
	cp a
	ret

DecrementEnvelopeValue: ; d < 0
	add a,d
	
	; Clamp from 0..255
	jr c,+
	ld a,255
	jp p,+
	xor a
+:	
	; Check if we've passed the threshold.
	cp e
	ret z
	ret nc
	
	; Set to the threshold and set z.
	ld a,e
	cp a
	ret

; ---------------------------------------------------------
; WriteCommand -> Writes a sound command.
; ---------------------------------------------------------
; Inputs:   bc = channel number
;           e = pitch (0..255)
;           l = amplitude/envelope
;           a = duration (1..255)
; Outputs:  None.
; Destroys: af, c, d, hl.
; ---------------------------------------------------------
WriteCommand:
	di
	
	ld ix,Channels
	
	.if ChannelSize != 20
	.fail "Sound.WriteCommand expects Sound.ChannelSize = 20"
	.endif
	push af
	push de
	ld a,c
	and %11
	; 20 = 16+4
	add a,a
	add a,a
	ld e,a
	add a,a
	add a,a
	add a,e
	ld d,0
	ld e,a
	add ix,de
	pop de
	pop af
	
	ld (ix+Channel.Duration),a
	ld (ix+Channel.Pitch),e
	ld (ix+Channel.Amplitude),0
	
	
	ld a,%00000101 ; section 1, attacking
	ld (ix+Channel.State),a
	
	; Initialise the envelope.
	ld a,l
	
	; Set HL to point to the target envelope.
	push ix
	pop hl
	ld de,Channel.Envelope
	add hl,de

	or a
	jr z,PresetEnvelope
	jp p,LoadEnvelope

PresetEnvelope:
	
	ex de,hl
	ld hl,FixedAmplitudeEnvelope
	ld bc,EnvelopeSize-2
	ldir
	
	; Convert 0..-15 to the range 0..127.
	neg
	and %00001111
	ld c,a
	sla c
	sla c
	sla c
	srl a
	or c
	
	; Set ALA, ALD to amplitude.
	ld (de),a
	inc de
	ld (de),a
	
	jr LoadedEnvelope
	
LoadEnvelope:

	; a = envelope number.
	; *13 = 1+4+8
	dec a
	call GetEnvelopeAddressOffset
	
	push hl
	ld hl,Envelopes
	add hl,de
	pop de
	
	ld bc,EnvelopeSize
	
	ldir
	
LoadedEnvelope:
	ei
	ret

FixedAmplitudeEnvelope:
; T, PI1, PI2, PI3, PN1, PN2, PN3, AA, AD, AS, AR [, ALA, ALD]
.db $80, 0, 0, 0, 0, 0, 0, 127, 0, 0, -127
	
GetEnvelopeAddressOffset:
.if EnvelopeSize != 13
.fail "Sound.GetEnvelopeAddressOffset expects Sound.EnvelopeSize = 13"
.endif
	ld e,a
	add a,a
	add a,a
	ld d,a
	add a,a
	add a,d
	add a,e
	ld d,0
	ret

; ---------------------------------------------------------
; UpdateChannel -> Immediately updates an output channel.
; ---------------------------------------------------------
; Inputs:   a = channel number (0..3)
;           e = pitch (0..255)
;           l = amplitude (0..127)
; Outputs:  None.
; Destroys: af, c, d, hl.
; ---------------------------------------------------------
UpdateChannel:
	
	; First, we need to translate the channel number.
	; On the BBC Micro, channel 0 (%00) is noise and can take its frequency from channel 1 (%01).
	; On the SN76489, channel 3 (%11) is noise and can take its frequency from channel 2 (%10).
	; The other two channels aren't hardware-specific, so just invert the bits to translate:
	
	cpl
	and %11
	
	; Move channel number from bits %......cc to bits %.cc.....
	; (>>>3 is same as <<<5)
	
	rrca
	rrca
	rrca
	
	or %10000000
	ld c,a
	
	; c is now the "latch channel" command byte.
	
	; Amplitude is from 0..127, but we need to scale from %1111..%0000
	ld a,l
	srl a
	srl a
	srl a
	and %00001111
	neg
	add a,15 + %00010000 ; We always want bit 4 set to latch the volume.
	
	; Combine with the "latch channel" byte.
	or c
	
	; Look up the period from our precomputed period table.
	ld d,0
	sla e
	rl d
	
	ld hl,PeriodTable
	add hl,de
	
	; Write the previously-computed latch control byte.
	out (PSG),a
	
	; Write the low four bits of the pitch.
	ld a,(hl)
	inc hl
	or c
	out (PSG),a
	
	; Write the period high six bits.
	ld a,(hl)
	out (PSG),a
		
	ret
	
	
PeriodTable:
.for pitch = 0 to 255
	; Middle C = 52, so A440 = 52+9*4= 88
	freq = 440 * 2 ** ((pitch - 88) / 12 / 4)
	period = round(3563217 / 32 / freq)
	.db period & %00001111
	.db (period >> 4) & %00111111
.loop


.endmodule