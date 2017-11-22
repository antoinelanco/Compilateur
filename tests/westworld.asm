.text
	move $fp, $sp
	lw $a0, 0($a1)
	jal atoi
	move $a0, $v0
	jal main
	li $v0, 10
	syscall
bass:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
#_prog_0
	li $t1, 6
	li $t0, 4
	mul $t0, $t1, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t2, $v0
#_prog_1
#_prog_2
	li $t0, 0
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 0
	sw $t1, 4($t0)
#_prog_3
	li $t0, 1
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 2
	sw $t1, 4($t0)
#_prog_4
	li $t0, 2
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 7
	sw $t1, 4($t0)
#_prog_5
	li $t0, 3
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 14
	sw $t1, 4($t0)
#_prog_6
	li $t0, 4
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 15
	sw $t1, 4($t0)
#_prog_7
	li $t0, 5
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 15
	sw $t1, 4($t0)
#_prog_8
	sw $t2, -4($fp)
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
blank:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	move $t6, $a0
	move $t2, $a1
#_prog_0
	li $t0, 4
	mul $t0, $t6, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t3, $v0
#_prog_1
	move $t7, $t3
#_prog_2
	li $t5, 0
#_prog_3
	b _label_1
#_label_2
_label_2:
#_prog_5
	li $t0, 4
	mul $t0, $t2, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t3, $v0
#_prog_6
#_prog_7
	li $t4, 0
#_prog_8
	b _label_3
#_label_4
_label_4:
#_prog_10
	li $t1, 4
	mul $t1, $t4, $t1
	add $t0, $t3, $t1
	li $t1, 0
	sw $t1, 4($t0)
#_prog_11
	li $t1, 1
	add $t4, $t4, $t1
#_prog_12
#_label_3
_label_3:
#_prog_14
	slt $t8, $t4, $t2
#_prog_15
	bnez $t8, _label_4
#_prog_16
	li $t1, 4
	mul $t1, $t5, $t1
	add $t0, $t7, $t1
	sw $t3, 4($t0)
#_prog_17
	li $t1, 1
	add $t3, $t5, $t1
#_prog_18
	move $t5, $t3
#_label_1
_label_1:
#_prog_20
	slt $t3, $t5, $t6
#_prog_21
	bnez $t3, _label_2
#_prog_22
	sw $t7, -4($fp)
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
	jr $ra
burn:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	move $t2, $a0
	move $t3, $a1
	move $t7, $a2
	move $t6, $a3
	lw $t9, 8($fp)
#_prog_0
	li $t4, 0
#_prog_1
	b _label_5
#_label_6
_label_6:
#_prog_3
	li $t8, 0
#_prog_4
	li $t1, 4
	mul $t1, $t4, $t1
	add $t0, $t6, $t1
	lw $t5, 4($t0)
#_prog_5
	add $t5, $t5, $t2
#_prog_6
#_prog_7
	b _label_7
#_label_8
_label_8:
#_prog_9
	mul $t0, $t4, $t3
	sw $t0, -12($fp)
#_prog_10
	lw $t0, -12($fp)
	add $t0, $t0, $t8
	sw $t0, -8($fp)
#_prog_11
	lw $t0, -8($fp)
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t9, $t1
	lw $t1, 4($t0)
	sw $t1, -16($fp)
#_prog_12
	li $t1, 4
	mul $t1, $t5, $t1
	lw $t0, -16($fp)
	add $t0, $t0, $t1
	li $t1, 1
	sw $t1, 4($t0)
#_prog_13
	li $t1, 1
	add $t8, $t8, $t1
#_prog_14
#_label_7
_label_7:
#_prog_16
	slt $t0, $t8, $t3
	sw $t0, -4($fp)
#_prog_17
	lw $t0, -4($fp)
	bnez $t0, _label_8
#_prog_18
	li $t1, 1
	add $t4, $t4, $t1
#_prog_19
#_label_5
_label_5:
#_prog_21
	slt $t5, $t4, $t7
#_prog_22
	bnez $t5, _label_6
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
	jr $ra
main:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -16
	move $t2, $a0
#_prog_0
	sw $t2, -4($sp)
	addi $sp, $sp, -4
	addi $sp, $sp, 0
	jal westworld
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $t2, -4($sp)
	move $t2, $v0
#_prog_1
	sw $t2, -4($sp)
	addi $sp, $sp, -4
	li $t0, 24
	move $a0, $t0
	li $t0, 40
	move $a1, $t0
	move $a2, $t2
	addi $sp, $sp, 0
	jal play
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $t2, -4($sp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 24
	jr $ra
play:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t3, $a0
	move $t2, $a1
	move $t5, $a2
#_prog_0
	li $t1, 1
	sub $t3, $t3, $t1
#_prog_1
	move $t4, $t3
#_prog_2
	b _label_9
#_label_10
_label_10:
#_prog_4
	li $t1, 1
	sub $t3, $t2, $t1
#_prog_5
#_prog_6
	b _label_11
#_label_12
_label_12:
#_prog_8
	li $t1, 4
	mul $t1, $t4, $t1
	add $t0, $t5, $t1
	lw $t6, 4($t0)
#_prog_9
	li $t1, 4
	mul $t1, $t3, $t1
	add $t0, $t6, $t1
	lw $t6, 4($t0)
#_prog_10
	bnez $t6, _label_13
#_prog_11
	li $a0, 32
	li $v0, 11
	syscall
#_prog_12
	li $a0, 32
	li $v0, 11
	syscall
#_prog_13
	b _label_14
#_label_13
_label_13:
#_prog_15
	li $a0, 35
	li $v0, 11
	syscall
#_prog_16
	li $a0, 35
	li $v0, 11
	syscall
#_label_14
_label_14:
#_prog_18
	li $t1, 1
	sub $t3, $t3, $t1
#_prog_19
#_label_11
_label_11:
#_prog_21
	li $t0, 0
	sle $t6, $t0, $t3
#_prog_22
	bnez $t6, _label_12
#_prog_23
	li $a0, 10
	li $v0, 11
	syscall
#_prog_24
	li $t1, 1
	sub $t3, $t4, $t1
#_prog_25
	move $t4, $t3
#_label_9
_label_9:
#_prog_27
	li $t0, 0
	sle $t3, $t0, $t4
#_prog_28
	bnez $t3, _label_10
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
repeat:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t5, $a0
	move $t4, $a1
#_prog_0
	li $t0, 2
	mul $t3, $t0, $t5
#_prog_1
	li $t0, 4
	mul $t0, $t3, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t2, $v0
#_prog_2
	move $t3, $t2
#_prog_3
	li $t2, 0
#_prog_4
	b _label_15
#_label_16
_label_16:
#_prog_6
	li $t1, 4
	mul $t1, $t2, $t1
	add $t0, $t4, $t1
	lw $t6, 4($t0)
#_prog_7
	li $t1, 4
	mul $t1, $t2, $t1
	add $t0, $t3, $t1
	sw $t6, 4($t0)
#_prog_8
	li $t1, 4
	mul $t1, $t2, $t1
	add $t0, $t4, $t1
	lw $t7, 4($t0)
#_prog_9
	add $t6, $t2, $t5
#_prog_10
	li $t1, 4
	mul $t1, $t6, $t1
	add $t0, $t3, $t1
	sw $t7, 4($t0)
#_prog_11
	li $t1, 1
	add $t2, $t2, $t1
#_prog_12
#_label_15
_label_15:
#_prog_14
	slt $t6, $t2, $t5
#_prog_15
	bnez $t6, _label_16
#_prog_16
	sw $t3, -4($fp)
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
theme:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
#_prog_0
	li $t1, 12
	li $t0, 4
	mul $t0, $t1, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t2, $v0
#_prog_1
#_prog_2
	li $t0, 0
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 14
	sw $t1, 4($t0)
#_prog_3
	li $t0, 1
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 14
	sw $t1, 4($t0)
#_prog_4
	li $t0, 2
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 15
	sw $t1, 4($t0)
#_prog_5
	li $t0, 3
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 14
	sw $t1, 4($t0)
#_prog_6
	li $t0, 4
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 14
	sw $t1, 4($t0)
#_prog_7
	li $t0, 5
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 15
	sw $t1, 4($t0)
#_prog_8
	li $t0, 6
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 14
	sw $t1, 4($t0)
#_prog_9
	li $t0, 7
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 12
	sw $t1, 4($t0)
#_prog_10
	li $t0, 8
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 10
	sw $t1, 4($t0)
#_prog_11
	li $t0, 9
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 10
	sw $t1, 4($t0)
#_prog_12
	li $t0, 10
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 10
	sw $t1, 4($t0)
#_prog_13
	li $t0, 11
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 10
	sw $t1, 4($t0)
#_prog_14
	sw $t2, -4($fp)
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
	jr $ra
westworld:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
#_prog_0
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	li $t0, 24
	move $a0, $t0
	li $t0, 40
	move $a1, $t0
	addi $sp, $sp, 0
	jal blank
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	move $t2, $v0
#_prog_1
#_prog_2
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	addi $sp, $sp, 0
	jal theme
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	move $t3, $v0
#_prog_3
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	li $t0, 12
	move $a0, $t0
	li $t0, 2
	move $a1, $t0
	li $t0, 12
	move $a2, $t0
	move $a3, $t3
	sw $t2, -4($sp)
	addi $sp, $sp, -4
	jal burn
	addi $sp, $sp, 4
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
#_prog_4
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	addi $sp, $sp, 0
	jal theme
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	move $t3, $v0
#_prog_5
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	li $t0, 24
	move $a0, $t0
	li $t0, 2
	move $a1, $t0
	li $t0, 12
	move $a2, $t0
	move $a3, $t3
	sw $t2, -4($sp)
	addi $sp, $sp, -4
	jal burn
	addi $sp, $sp, 4
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
#_prog_6
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	addi $sp, $sp, 0
	jal bass
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	move $t3, $v0
#_prog_7
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	li $t0, 6
	move $a0, $t0
	move $a1, $t3
	addi $sp, $sp, 0
	jal repeat
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	move $t3, $v0
#_prog_8
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	li $t0, 12
	move $a0, $t0
	move $a1, $t3
	addi $sp, $sp, 0
	jal repeat
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	move $t3, $v0
#_prog_9
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	li $t0, 0
	move $a0, $t0
	li $t0, 1
	move $a1, $t0
	li $t0, 24
	move $a2, $t0
	move $a3, $t3
	sw $t2, -4($sp)
	addi $sp, $sp, -4
	jal burn
	addi $sp, $sp, 4
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
#_prog_10
	sw $t2, -4($fp)
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
	jr $ra
atoi:
	move $t0, $a0
	li $t1, 0
	li $t2, 10
atoi_loop:
	lbu $t3, 0($t0)
	beq $t3, $zero, atoi_end
	li $t4, 48
	blt $t3, $t4, atoi_error
	li $t4, 57
	bgt $t3, $t4, atoi_error
	addi $t3, $t3, -48
	mul $t1, $t1, $t2
	add $t1, $t1, $t3
	addi $t0, $t0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	move $v0, $t1
	jr $ra
.data
