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
	sw $t2, -4($sp)
	addi $sp, $sp, -4
	sw $t2, -4($sp)
	sw $t2, -8($sp)
	li $t0, 9
	sw $t0, -12($sp)
	addi $sp, $sp, -12
	jal rec_mul
	addi $sp, $sp, 12
	addi $sp, $sp, 4
	lw $t2, -4($sp)
	move $t2, $a0
#_prog_1
	move $a0, $t2
	li $v0, 11
	syscall
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
rec_mul:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	lw $t2, 16($fp)
	lw $t4, 12($fp)
	lw $t3, 8($fp)
#_prog_0
	li $t1, 0
	sgt $t5, $t3, $t1
#_prog_1
	bnez $t5, _label_1
#_prog_2
	sw $t2, -4($fp)
#_prog_3
	b _label_2
#_label_1
_label_1:
#_prog_5
	add $t2, $t2, $t4
#_prog_6
	li $t1, 1
	sub $t3, $t3, $t1
#_prog_7
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	sw $t5, -16($sp)
	addi $sp, $sp, -16
	sw $t2, -4($sp)
	sw $t4, -8($sp)
	sw $t3, -12($sp)
	addi $sp, $sp, -12
	jal rec_mul
	addi $sp, $sp, 12
	addi $sp, $sp, 16
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	lw $t5, -16($sp)
	move $t2, $a0
#_prog_8
	sw $t2, -4($fp)
#_label_2
_label_2:
	lw $a0, -4($fp)
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
