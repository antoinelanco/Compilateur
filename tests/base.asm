.text
	move $fp, $sp
	addi $fp, $fp, -4
	lw $a0, 0($a1)
	jal atoi
	sw $v0, 0($fp)
	addi $sp, $sp, -32
#_main_0
	li $t2, 0
	sw $t2, -32($fp)
#_main_1
	b _label_main_3
#_label_main_4
_label_main_4:
#_main_3
	lw $a0, -32($fp)
	li $v0, 11
	syscall
#_main_4
	lw $t1, -32($fp)
	li $t2, 1
	add $t3, $t1, $t2
	sw $t3, -28($fp)
#_main_5
	lw $t2, -28($fp)
	sw $t2, -32($fp)
#_label_main_3
_label_main_3:
#_main_7
	lw $t1, -32($fp)
	lw $t2, 0($fp)
	slt $t3, $t1, $t2
	sw $t3, -24($fp)
#_main_8
	lw $t0, -24($fp)
	bnez $t0, _label_main_4
#_main_9
	li $t1, 2
	li $t2, 2
	add $t3, $t1, $t2
	sw $t3, -20($fp)
#_main_10
	lw $t1, -20($fp)
	li $t2, 4
	seq $t3, $t1, $t2
	sw $t3, -16($fp)
#_main_11
	lw $t0, -16($fp)
	bnez $t0, _label_main_1
#_main_12
	b _label_main_2
#_label_main_1
_label_main_1:
#_main_14
	li $a0, 54
	li $v0, 11
	syscall
#_label_main_2
_label_main_2:
#_main_16
	lw $t1, -32($fp)
	li $t2, 1
	add $t3, $t1, $t2
	sw $t3, -12($fp)
#_main_17
	lw $t2, -12($fp)
	sw $t2, -32($fp)
#_main_18
	lw $t1, -32($fp)
	li $t2, 1
	sub $t3, $t1, $t2
	sw $t3, -8($fp)
#_main_19
	lw $t2, -8($fp)
	sw $t2, -32($fp)
#_main_20
	lw $t1, -32($fp)
	li $t2, 2
	div $t3, $t1, $t2
	sw $t3, -4($fp)
#_main_21
	lw $t2, -4($fp)
	sw $t2, -32($fp)
	li $v0, 10
	syscall
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
