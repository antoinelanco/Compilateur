.text
	move $fp, $sp
	lw $a0, 0($a1)
	jal atoi
	sw $v0, 0($sp)
	jal main
	li $v0, 10
	syscall
main:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	lw $t2, 8($fp)
#_prog_0
	li $t5, 1
#_prog_1
	li $t4, 0
#_prog_2
	b _label_1
#_label_2
_label_2:
#_prog_4
	li $t5, 0
#_prog_5
	li $t3, 0
#_prog_6
	b _label_3
#_label_4
_label_4:
#_prog_8
	mul $t7, $t4, $t4
#_prog_9
	mul $t6, $t3, $t3
#_prog_10
	add $t7, $t7, $t6
#_prog_11
	mul $t6, $t2, $t2
#_prog_12
	slt $t6, $t7, $t6
#_prog_13
	bnez $t6, _label_5
#_prog_14
	li $a0, 35
	li $v0, 11
	syscall
#_prog_15
	b _label_6
#_label_5
_label_5:
#_prog_17
	li $a0, 46
	li $v0, 11
	syscall
#_prog_18
	li $t5, 1
#_label_6
_label_6:
#_prog_20
	li $a0, 32
	li $v0, 11
	syscall
#_prog_21
	li $t1, 1
	add $t3, $t3, $t1
#_prog_22
#_label_3
_label_3:
#_prog_24
	li $t1, 1
	add $t6, $t2, $t1
#_prog_25
	slt $t6, $t3, $t6
#_prog_26
	bnez $t6, _label_4
#_prog_27
	li $a0, 10
	li $v0, 11
	syscall
#_prog_28
	li $t1, 1
	add $t3, $t4, $t1
#_prog_29
	move $t4, $t3
#_label_1
_label_1:
#_prog_31
	bnez $t5, _label_2
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
