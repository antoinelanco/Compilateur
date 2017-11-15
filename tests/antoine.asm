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
	li $a0, 10
	li $t0, 4
	mul $a1, $a0, $t0
	addi $a0, $a1, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t2, $v0
#_prog_1
#_prog_2
	li $a0, 65
	li $a1, 0
	li $t1, 4
	mul $t1, $a1, $t1
	add $t0, $t2, $t1
	sw $a0, 4($t0)
#_prog_3
	li $a1, 0
	li $a0, 4
	mul $a0, $a1, $a0
	add $t0, $t2, $a0
	lw $t2, 4($t0)
#_prog_4
	move $a0, $t2
	li $v0, 11
	syscall
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
