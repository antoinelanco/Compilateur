.text
	move $fp, $sp
	lw $a0, 0($a1)
	jal atoi
	move $a0, $v0
	jal main
	li $v0, 10
	syscall
main:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	sw $a0, -40($fp)
#_prog_0
	lw $t1, -40($fp)
	li $t0, 4
	mul $t0, $t1, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	sw $v0, -28($fp)
#_prog_1
	lw $t0, -28($fp)
	sw $t0, -36($fp)
#_prog_2
	li $t0, 0
	sw $t0, -32($fp)
#_prog_3
	b _label_3
#_label_4
_label_4:
#_prog_5
	lw $t0, -32($fp)
	li $t1, 49
	add $t0, $t0, $t1
	sw $t0, -24($fp)
#_prog_6
	lw $t0, -32($fp)
	li $t1, 4
	mul $t1, $t0, $t1
	lw $t0, -36($fp)
	add $t0, $t0, $t1
	lw $t1, -24($fp)
	sw $t1, 4($t0)
#_prog_7
	lw $t0, -32($fp)
	li $t1, 1
	add $t0, $t0, $t1
	sw $t0, -20($fp)
#_prog_8
	lw $t0, -20($fp)
	sw $t0, -32($fp)
#_label_3
_label_3:
#_prog_10
	lw $t0, -32($fp)
	li $t1, 100
	slt $t0, $t0, $t1
	sw $t0, -16($fp)
#_prog_11
	lw $t0, -16($fp)
	bnez $t0, _label_4
#_prog_12
	li $t0, 0
	sw $t0, -32($fp)
#_prog_13
	b _label_1
#_label_2
_label_2:
#_prog_15
	lw $t0, -32($fp)
	li $t1, 4
	mul $t1, $t0, $t1
	lw $t0, -36($fp)
	add $t0, $t0, $t1
	lw $t1, 4($t0)
	sw $t1, -12($fp)
#_prog_16
	lw $a0, -12($fp)
	li $v0, 11
	syscall
#_prog_17
	lw $t0, -32($fp)
	li $t1, 1
	add $t0, $t0, $t1
	sw $t0, -8($fp)
#_prog_18
	lw $t0, -8($fp)
	sw $t0, -32($fp)
#_label_1
_label_1:
#_prog_20
	lw $t0, -32($fp)
	li $t1, 100
	slt $t0, $t0, $t1
	sw $t0, -4($fp)
#_prog_21
	lw $t0, -4($fp)
	bnez $t0, _label_2
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
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
