 .text
	move $fp, $sp
	addi $fp, $fp, -4
	lw $a0, 0($a1)
	jal atoi
	sw $v0, 0($fp)
	addi $sp, $sp, 0
#_main_0
	li $t5, 1
#_main_1
	li $t4, 0
#_main_2
	b _label_main_1
#_label_main_2
_label_main_2:
#_main_4
	li $t5, 0
#_main_5
	li $t3, 0
#_main_6
	b _label_main_3
#_label_main_4
_label_main_4:
#_main_8
	mul $t7, $t4, $t4
#_main_9
	mul $t6, $t3, $t3
#_main_10
	add $t7, $t7, $t6
#_main_11
	lw $t0, 0($fp)
	lw $t1, 0($fp)
	mul $t6, $t0, $t1
#_main_12
	slt $t6, $t7, $t6
#_main_13
	bnez $t6, _label_main_5
#_main_14
	li $a0, 35
	li $v0, 11
	syscall
#_main_15
	b _label_main_6
#_label_main_5
_label_main_5:
#_main_17
	li $a0, 46
	li $v0, 11
	syscall
#_main_18
	li $t5, 1
#_label_main_6
_label_main_6:
#_main_20
	li $a0, 32
	li $v0, 11
	syscall
#_main_21
	li $t1, 1
	add $t3, $t3, $t1
#_main_22
#_label_main_3
_label_main_3:
#_main_24
	lw $t0, 0($fp)
	li $t1, 1
	add $t6, $t0, $t1
#_main_25
	slt $t6, $t3, $t6
#_main_26
	bnez $t6, _label_main_4
#_main_27
	li $a0, 10
	li $v0, 11
	syscall
#_main_28
	li $t1, 1
	add $t3, $t4, $t1
#_main_29
	move $t4, $t3
#_label_main_1
_label_main_1:
#_main_31
	bnez $t5, _label_main_2
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