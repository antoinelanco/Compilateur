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
	move $t2, $a0
#_prog_0
	li $t3, 0
#_prog_1
	li $t1, 4
	li $t0, 4
	mul $t0, $t1, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t2, $v0
#_prog_2
#_prog_3
	li $t0, 0
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 80
	sw $t1, 4($t0)
#_prog_4
	li $t0, 1
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 85
	sw $t1, 4($t0)
#_prog_5
	li $t0, 2
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 84
	sw $t1, 4($t0)
#_prog_6
	li $t0, 3
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 69
	sw $t1, 4($t0)
#_prog_7
	b _label_1
#_label_2
_label_2:
#_prog_9
	li $t1, 4
	mul $t1, $t3, $t1
	add $t0, $t2, $t1
	lw $t4, 4($t0)
#_prog_10
	move $a0, $t4
	li $v0, 11
	syscall
#_prog_11
	li $t1, 1
	add $t3, $t3, $t1
#_prog_12
#_label_1
_label_1:
#_prog_14
	li $t1, 4
	slt $t4, $t3, $t1
#_prog_15
	bnez $t4, _label_2
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
