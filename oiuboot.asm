; ------------------------------------------------------------------------
; oiu boot pager code (oiuboot.asm) by Charles Perkins.
;
; This code may be freely included in any project which is distributed 
; under the Gnu Programming License (GPL.)
;
; For inclusion any more or less restrictive project please contact me
; (chuck@memetech.com) for permission. I will give it--I'm just curious
; about who might like to use this boot pager.
;
; ------------------------------------------------------------------------
;
; The boot sector and primitive pager (LBA IDE x86) for oiu
;
; This 512 byte boot sector performs the following operations:
;
; Create an IDT
; Load the GDTR and IDTR
; Switch to protected mode
; Jump to a 32-bit ring 0 code segment
; Load the other segment registers with 32-bit ring 0 data descriptors
; Establish the top of stack
; Create the page directory and a page table 
; Enable Paging
; Enable the A20 line
; Reprogram the first PIC
; Jump to a not-present page
;
; When a page fault occurs, the code does the following:
;
; Intercept the page fault
; Determine the faulting page
; Calculate the correct sectors to load from disk
; Load eight sectors from the disk
; Resume execution
;
; This primitive pager assumes a linear identity mapped address space.
; The first megabyte is presumed to be scratch-pad or device memory and is
; not paged--the first page (8 sectors) on the hard drive correspond to
; the first actual page of memory above the 1M mark. The maximum addressable
; virtual memory equals the actual amount of physical memory--the pager is
; simply used as a mechanism for getting a working system image from hard
; drive storage into actual RAM. The primitive pager does not write pages
; back to disk.
;
; No faults other than a page fault are handled. At all. There are no error
; messages or faults presented to the user--the code either works (in which
; case the VM location jumped to may include a routine to present the user 
; with a welcome message) or the system loops forever, or the system triple-
; faults. Interrupts are left disabled.
;
; This NASM source file is padded to create a boot sector and further padded
; to create either a 1.44M floppy image or an 8M hard disk image (17 cylinders,
; 16 heads, 63 sectors per track) that both Bochs and VMWare can use and that
; (in the case of the 1.44M image) can be rawritten for testing on a real Pentium.
;
; Assemble with NASM: nasm -fbin oiuboot.asm
;
; ------------------------------------------------------------------------

[BITS 16]       ; the bios starts out in 16-bit real mode

segment .text
ORG 0x7c00

; ------------------------------------------------------------------------
; Build the Interrupt Descriptor Table
; ------------------------------------------------------------------------
; Write 256 entries starting at 0x3000 (absolute) with all of them 
; except for two (the general protection fault and page fault entries)  
; pointing to the generic INTHNDLR routine
; ------------------------------------------------------------------------

		mov	cx, 0x0300	
		mov	es, cx		
		mov	bx, 256*8	
iloop:		sub	bx, 8
		mov	word [es:bx], INTHNDLR
		mov	dword [es:bx+2], 0xEE000008
		mov	word [es:bx+6], 0
		cmp	bx, 0		
		jne	iloop 		


		mov	bx, 14*8
		mov	cx, PFHNDLR	; low 16 bits of page fault handler loc
		mov	[es:bx], cx


; ------------------------------------------------------------------------
; Load the GDT and IDT Registers and disable iterrupts
; ------------------------------------------------------------------------

		lgdt [cs:GDTPTR]
		lidt [cs:IDTPTR]

		cli

; ------------------------------------------------------------------------
; Switch to Protected Mode, flush the instruction queue, jump to a 32-bit segment
; ------------------------------------------------------------------------

		mov	eax,cr0		
		inc	ax		; Set PE bit 
		mov	cr0,eax		

		jmp	PMNOW		; Flush the instruction queue
PMNOW:

		DB	0x66		; 32-bit operand override
		DB	0xEA		; far jump
		DD	IS32BITNOW	; 32-bit offset to following code
		DW	0x08		; 16-bit selector (1st entry in GDT)

; ------------------------------------------------------------------------
; Load the segment registers with a 0-based flat (4gb) data descriptor
; ------------------------------------------------------------------------

[BITS 32]
IS32BITNOW:


		xor	eax,eax		
		mov	al,10h		; 2nd entry in GDT
 		mov	ds,ax		
 		mov	es,ax		
		mov	fs,ax		
 		mov	gs,ax		
 		mov	ss,ax		

; ------------------------------------------------------------------------
; Put the top of stack somewhere
; ------------------------------------------------------------------------
 
 		mov	esp,08000h

; ------------------------------------------------------------------------
; Create a page directory and a page table and then enable paging
; ------------------------------------------------------------------------
; Linear direct map only the first Megabyte of memory,
; leaving the rest of memory marked in the MMU as not present
; so that any access will cause a page fault.
; ------------------------------------------------------------------------

 		mov	edi,4000h	; edi = &PT
		mov	eax,3		; Address 0, bit p & r/w set
		mov	ecx,256		; 256*4K = 1M linear direct mapped
InitPt:
		stosd			; Write one entry
		add	eax,1000h	; Next page address
		loop	InitPt		

 		mov	edi,4000h + (256*4)	; edi = &PT
		mov	eax,0		; Not present
		mov	ecx,768		; 768*4K = 3M  
InitPt2:
		stosd			; Write one entry
		loop	InitPt2		

 		mov	edi,5000h	; edi = &PD
		mov	eax,0		; Not present
		mov	ecx,1024	; All of the PD Entries  
InitPt3:
		stosd			; Write one entry
		loop	InitPt3


		mov	eax,5000h	; eax  = &PD
		mov	ebx,4003h      	; ebx  = &PT | 3
		mov	[eax],ebx	; PD[0] = &PT


	        MOV     EDI, CR3
	        AND     EDI, 0FFFH
	        OR      EDI, 5000H
	        MOV     CR3, EDI	; CR3 = ^PAGE DIRECTORY

		mov eax,cr0
		or eax,80000000h	; enable paging
		mov cr0,eax

; ------------------------------------------------------------------------
; Enable the A20 Line so that we can actually use odd-numbered megabytes
; ------------------------------------------------------------------------

enable_A20:

	call    a20wait
	mov     al,0xAD
	out     0x64,al

	call    a20wait
	mov     al,0xD0
	out     0x64,al

	call    a20wait2
	in      al,0x60
	push    eax

	call    a20wait
	mov     al,0xD1
	out     0x64,al

	call    a20wait
	pop     eax
	or      al,2
	out     0x60,al

        call    a20wait
	mov     al,0xAE
	out     0x64,al

	call    a20wait


; ------------------------------------------------------------------------
; Reprogram the 1st PIC
; ------------------------------------------------------------------------

	 mov al, 0x20
	 out 0x20, al

	 mov al,0x11
	 out 0x20,al

	 mov al,0x20 
	 out 0x21,al

	 mov al,4
	 out 0x21,al

	 mov al,1
	 out 0x21,al

; ------------------------------------------------------------------------
; Cause a page fault by jumping to a mapped out location
; ------------------------------------------------------------------------

	jmp 0x00101000

; ------------------------------------------------------------------------
; A20 Line helper routines
; ------------------------------------------------------------------------

a20wait:
.l0:	mov     ecx,65536
.l1:	in      al,0x64
	test    al,2
	jz      .l2
	loop    .l1
	jmp     .l0
.l2:	ret


a20wait2:
.l0:	mov     ecx,65536
.l1:	in      al,0x64
	test    al,1
	jnz     .l2
        loop    .l1
	jmp     .l0
.l2:	ret

; ------------------------------------------------------------------------
; Catch-all interrupt handler
; ------------------------------------------------------------------------

INTHNDLR:
;	mov  word [0xb800E], 0xAAAA
	jmp $	; don't even try to do anything.


; ------------------------------------------------------------------------
; Page Fault interrupt handler
; ------------------------------------------------------------------------
; Determine the faulting address and page                    
; Put some memory under that location                        
; (this pager assumes that any RAM it uses is identity mapped)
; Load the corresponding four sectors to that location
; Resume the faulting instruction                            
; ------------------------------------------------------------------------

PFHNDLR:

	pushad
	mov	ebx, cr2	; get the fault location
	and	bx, 0xF000	; mask off the location within the page
	mov	eax, ebx
	shr	ebx, 10		; make it a 4 byte offset in the page table
	or	eax, 3		; set the present and R/W bits
	mov	[ebx+0x4000], eax
	shl	ebx, 1		; now it is the sector offset from start of disk

	sub	ebx, 0x800	; adjust for the 1M already assumed to be present

	mov	eax, ebx
	shr	eax, 24		; get the top 4 bits of the 28-bit LBA address
	or	al, 0xE0	; set the top three bits including the LBA bit  

				; (patch 0xE0 to be 0xF0 in order to make the 
				; boot pager load from the IDE primary slave.)

	mov     dx,1f6h         ; Drive and head port (top 4 bits of LBA)
	out     dx,al

	mov     dx,1f2h         ; Sector count port
	mov     al,8            ; Read eight sectors (one page)
	out     dx,al

	
	mov     dx,1f3h         ; Sector number port
	mov     eax,ebx         ; 
	out     dx,al		; low byte of 28-bit LBA sector number

	shr	eax, 8
	mov     dx,1f4h         ; Cylinder low port
	out     dx,al		; bits 8-15 of 28-bit LBA sector number

	shr	eax, 8
	mov     dx,1f5h         ; Cylinder high port
	out     dx,al		; bits 16-24 of 28-bit LBA sector number

	mov     dx,1f7h         ; Command port
	mov     al,20h          ; Read with retry.
	out     dx,al

	mov ecx, cr2		; faulting address
	and ecx, 0xFFFFF000	; page start
	mov ebx, ecx
	add ebx, 4096

moredata:
	mov     dx,1f7h         ; Command port
	in      al,dx
	test    al,8            ; Check to see if the data is there yet
	jz      moredata     

	mov     dx,1f0h         ; Data Register
	in      ax,dx
	mov	[ecx], ax
	add	ecx,2
	cmp	ecx,ebx

	jne	moredata

	popad
	pop eax
	iret

; ------------------------------------------------------------------------
; Sector padding and data (IDTPTR, GDT, GDTPTR, AA55 bootable flag)
; ------------------------------------------------------------------------
;
; The GDT and IDT pointers, the GDT, and the bootable flag are all
; packed in pretty tightly. The format is as follows:
;
; 480 [....][IDTPTR (6 bytes)]
; 488 [Code Segment GDT Entry]
; 496 [Data Segment GDT Entry]
; 504 [GDTPTR (6 bytes)][AA55]
; 
; Note that because the first selector in the GDT is never accessed
; by the processor it is being re-used to hold the IDT pointer (the
; GDT actually starts at 480.)
; ------------------------------------------------------------------------

	times 482-($-$$) db 0 ; pad with zeros to align our structure
                              ; at the end of the sector
; IDT Pointer

IDTPTR	dw 8*256-1	; idt_limit
	dd 0x00003000	; idt location


; code descriptor 4 GB flat segment starting 0000:0000h 

	dw 0ffffh, 0h 
	db 0h, 09ah, 0cfh, 0h

; data descriptor 4 GB flat segment starting 0000:0000h

	dw 0ffffh, 0h			; Base 0000:0000h
	db 0h, 092h, 0cfh, 0h

; IDT Pointer

GDTPTR	dw $-GDTPTR-24-1; gdt limit (size -1)
       	dd GDTPTR - 24	; (point 8 bytes short of the code segment
			; to adjust for the never-referenced null
			; GDT entry)
; Boot Signature

	dw 0xAA55	; boot sector signature


; ------------------------------------------------------------------------
; The following is not a part of the boot sector but provides some test
; code for the boot sector to jump to, showing that it works.
;
; The hard disk image generated fits the format of an 8 megabyte drive
; with 17 cylinders, 16 heads, and 63 sectors per track. Bochs will
; access such a drive directly when given the right command line parameters, 
; and VMWare can use it as a plain disk when the proper .pln file is 
; created to describe it.
;
; for Bochs: bochs "boot: c" "diskc: file=oiuboot, cyl=17, heads=16, spt=63"
; (if bochs starts with the mouse captured and it is compiled with debug
; mode then you may have to press f-12 then select the debugger window and
; type c <enter> to continue execution.)
;
; VMWare .pln file:
;	DRIVETYPE      ide
;	CYLINDERS     17
;	HEADS         16
;	SECTORS       63
;	ACCESS "C:\oiuboot" 0 17136
;
; ------------------------------------------------------------------------

; fill out the rest of the hard disk image

	times 4096-512 db 0 ; pad to the end of the first page 

	mov	ebx,0
notdone:
	mov	al,[ebx+0x00102000]
	cmp	al, 0
	je	doneprint
	mov	ah, 0x0F
	shl	ebx,1
	mov	[ebx+0xb8000],ax
	shr	ebx,1
	inc	ebx
	jmp	notdone

doneprint:
;	mov  dword [0xb8000], 0xAAAACCCC

	jmp $

	times 8192-($-$$) db 0 ; pad to the end of the second page 
	
	db '                                                                                '
	db ' If you can see this message then the boot pager has successfully paged in      '
	db ' code from the hard drive and executed it.                                      '
	db '                                                                                '
	db ' That code also accesses paged-out data (this text screen.)                     '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                        Cool, eh?                                               '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db '                                                                                '
	db 0

	times 8773632-($-$$) db 'X'	;for 8M disk image
