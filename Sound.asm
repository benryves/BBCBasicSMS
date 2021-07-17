; =========================================================
; Module: Sound
; =========================================================
; Handles making sound
; =========================================================
.module Sound

PSG = $7F

; Each channel needs to keep track of:
; 0 -> state
;      bits 0 and 1 = ADSR state
;          00 = releasing
;          01 = attacking
;          10 = decaying
;          11 = sustaining
;      bits 2 and 3 = queue length
;      bits 4 and 5 = section/pitch envelope
;          00 = idle (channel free)
;          01 = section 1
;          10 = section 2
;          11 = section 3
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
; 19 -> queue
;       (12 bytes)
; = 32 bytes per channel

ChannelCount = 4
ChannelSize = 32
Channels = allocVar(ChannelSize * ChannelCount)

Channel.State = 0
Channel.Amplitude = 1
Channel.Pitch = 2
Channel.Duration = 3
Channel.PitchStep = 4
Channel.AmplitudeStep = 5
Channel.Envelope = 6
Channel.Queue = 19

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

EnvelopeCount = 4
EnvelopeSize = 13
Envelopes = allocVar(EnvelopeSize * EnvelopeCount)

ChannelUpdateTimer = allocVar(1)
ChannelUpdatePeriod = 5

Status = allocVar(1)
Status.Active = 0
Status.CanPlaySynchronisedNotes = 1

Reset:
	
	call Silence
	
	; Clear the envelopes, too!
	ld hl,Envelopes
	ld de,Envelopes + 1
	ld bc,(EnvelopeSize * EnvelopeCount) - 1
	ld (hl),a
	ldir
	
	ret

Silence:
	ld a,ChannelUpdatePeriod
	ld (ChannelUpdateTimer),a

	xor a
	ld (Status),a
	
	; Clear the channels.
	ld hl,Channels
	ld de,Channels + 1
	ld bc,(ChannelSize * ChannelCount) - 1
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
	
	; First, we'll count up how many channels are ready to play back in a synchronised fashion.
	
	ld ix,Channels
	ld b,ChannelCount
	
	ld a,(Status)
	res Status.CanPlaySynchronisedNotes,a
	ld (Status),a
	
	ld de,0

CountChannelsToSync:
	ld a,(ix+Channel.State)
	ld c,a
	
	and %00000011 ; Is it released?
	jr nz,NotReadyForSync
	
	ld a,c
	and %00001100 ; Is there any data in its queue?
	jr z,NotReadyForSync
	
	; Now we know there's data in the queue, is it a synchronised note?
	ld a,(ix+Channel.Queue+Channel.State)
	and $0F
	jr z,NotReadyForSync
	
	; Hooray!
	inc d ; Count up the number of channels that are ready and waiting.
	
	cp e ; Is it a larger sync amount?
	jr c,+
	ld e,a
+:

NotReadyForSync:
	push de
	ld de,ChannelSize
	add ix,de
	pop de
	djnz CountChannelsToSync
	
	; We've counted how many channels have synced notes (D) and the lowest sync number (E).
	
	ld a,d
	or a
	jr z,+ ; No synchronised notes in any queues.
	
	cp e
	jr z,+ ; If the requirement =1 and we count 1, that's because we're counting ourselves.
	jr c,+ ; Not enough synchronised notes in the queues.
	
	ld a,(Status)
	set Status.CanPlaySynchronisedNotes,a
	ld (Status),a
	
+:

	ld ix,Channels
	ld b,ChannelCount

TickNextChannelNote:
	
	; What's the current channel state?
	ld a,(ix+Channel.State)
	or a
	jr z,ChannelNoteInactive
	
	; Have we reached the end of its duration?
	ld a,(ix+Channel.Duration)
	or a
	jr nz,TickNotEndedNoteDuration
	
	; The note has finished, so change channel state to "release".
	ld a,(ix+Channel.State)
	and %11001100
	ld (ix+Channel.State),a
	
	jr ChannelNoteInactive
	
TickNotEndedNoteDuration:
	dec a
TickEndedNoteDuration:
	ld (ix+Channel.Duration),a
	jr ChannelNoteWasActive
	
ChannelNoteInactive:

	; If we get here, the note is currently inactive.
	; Is there a note in the queue to fetch?
	ld a,(ix+Channel.State)
	and %00001100
	jr z,ChannelNoteNoDequeue
	
	; There's something to (possibly) dequeue!
	push hl
	push bc
	push ix
	
.if Channel.Queue != 0
	ld de,Channel.Queue ; The pointer is to the next free space on the queue.
	add ix,de
.endif
	
	; a = duration
	; l = amplitude
	; e = pitch
	; bc = channel
	
	ld a,(ix+Channel.State)
	
	; If S of the &HSFN channel number is non-zero, we need to wait to syncronise.
	ld h,a
	and $0F
	ld a,h
	jr z,NotSynchronisationControl
	
	; We can now only play this note if we counted enough channels earlier.
	ld a,(Status)
	bit Status.CanPlaySynchronisedNotes,a
	jr nz,PlayNoteInQueue
	
	pop ix
	pop bc
	pop hl
	jr ChannelNoteNoDequeue

NotSynchronisationControl:	
	and $F0
	jr z,PlayNoteInQueue
	
	; We get here if H in &HSFN of the channel number is non-zero.
	; This indicates it's a continuation note, we process it through
	; the queue but don't actually change the state of the note
	; that's playing...
	
	pop ix
	jr SkipWritingCommand

PlayNoteInQueue:
	ld b,a
	ld l,(ix+Channel.Amplitude)
	ld e,(ix+Channel.Pitch)
	ld a,(ix+Channel.Duration)
	
	pop ix
	
	; Write the data
	call WriteCommandGotChannelAddress

SkipWritingCommand:
		
	; Move the queue pointer backwards.
	ld a,(ix+Channel.State)
	sub 4
	ld (ix+Channel.State),a
	
	; Shuffle the data inside the queue backwards
	push ix
	pop hl
	
	ld de,Channel.Queue
	add hl,de
	push hl

	ld de,4
	add hl,de
	pop de
	
	ld bc,8
	ldir
	
	pop bc
	pop hl
	
ChannelNoteNoDequeue:

ChannelNoteWasActive:

	; Advance to the next channel.
	ld de,ChannelSize	
	add ix,de
	djnz TickNextChannelNote

	; Move any 

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
	jr z,StepEnvelopeZero
	dec a
	jr nz,NoStepEnvelope

StepEnvelopeZero:	
	; Step the envelope
	call StepEnvelope
	jr EnvelopeStepped
	
NoStepEnvelope:
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
	and %00000011
	jr z,StepEnvelopeReleasing
	
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
	and  %11111100
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
	or  %00000011
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
	and %11111100
	or  %00000010
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
	
	; Set to the threshold and set z.
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
; QueueCommand -> Queues a sound command.
; ---------------------------------------------------------
; Inputs:   bc = channel number
;           e = pitch (0..255)
;           l = amplitude/envelope
;           a = duration (1..255)
; Outputs:  z is set if the value was stored in the queue,
;           nz if the queue is full.
; Destroys: ix, f, h
; ---------------------------------------------------------
QueueCommand:
	di
	call GetChannelAddress
	
	ld h,a
	
	ld a,c
	and $F0
	jr z,NoFlushQueue
	
	; We need to flush the queue.
	xor a
	ld (ix+Channel.State),a
	ld (ix+Channel.Duration),a

NoFlushQueue:
	
	ld a,(ix+Channel.State)
	cpl
	and %00001100
	jr nz,QueueNotFull
	
	; The queue is full, sorry.
	
	or $FF
	ld a,h
	ret

QueueNotFull:

	ld a,(ix+Channel.State)
	and %00001100
	
	push de
	
	; Make de = queue offset
	ld e,a
	ld d,0
	
	; Advance queue pointer
	ld a,(ix+Channel.State)
	add a,4
	ld (ix+Channel.State),a
	
	add ix,de
	
.if Channel.Queue != 0
	ld de,Channel.Queue
	add ix,de
.endif

	pop de
	
	; ix -> queue
	ld (ix+Channel.State),b     ; The queue position is implied, so B contains control byte
	ld (ix+Channel.Amplitude),l ; amplitude/envelope
	ld (ix+Channel.Pitch),e     ; pitch
	ld (ix+Channel.Duration),h  ; duration
	
	xor a
	ld a,h
	ei
	ret

; ---------------------------------------------------------
; WriteCommand -> Writes a sound command.
; ---------------------------------------------------------
; Inputs:   bc = channel number
;           e = pitch (0..255)
;           l = amplitude/envelope
;           a = duration (1..255)
; Outputs:  None.
; Destroys: af, c, d, hl, ix.
; ---------------------------------------------------------
WriteCommand:
	di
	call GetChannelAddress

WriteCommandGotChannelAddress:

	ld (ix+Channel.Duration),a
	ld (ix+Channel.Pitch),e
	ld (ix+Channel.Amplitude),0
	
	
	ld a,(ix+Channel.State)
	and %11001100
	or  %00010001 ; section 1, attacking
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
	xor a
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
	ld e,a
	ld d,0
	ret

GetChannelAddress:
	ld ix,Channels
	
	.if ChannelSize != 32
	.fail "Sound.GetChannelAddress expects Sound.ChannelSize = 32"
	.endif

	push af
	push de
	ld a,c
	and %11
	; 36 = 32+4
	add a,a
	add a,a
	;ld e,a
	add a,a
	add a,a
	add a,a
	;add a,e
	ld d,0
	ld e,a
	add ix,de
	pop de
	pop af
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