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
	sw $a0, -16($fp)
#_prog_0
	li $t1, 8
	li $t0, 4
	mul $t0, $t1, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	sw $v0, -8($fp)
#_prog_1
	li $t0, 0
	li $t1, 4
	mul $t1, $t0, $t1
	lw $t0, -8($fp)
	add $t0, $t0, $t1
	li $t1, 46
	sw $t1, 4($t0)
#_prog_2
	li $t0, 1
	li $t1, 4
	mul $t1, $t0, $t1
	lw $t0, -8($fp)
	add $t0, $t0, $t1
	li $t1, 5
	sw $t1, 4($t0)
#_prog_3
	li $t0, 2
	li $t1, 4
	mul $t1, $t0, $t1
	lw $t0, -8($fp)
	add $t0, $t0, $t1
	li $t1, 3
	sw $t1, 4($t0)
#_prog_4
	li $t0, 3
	li $t1, 4
	mul $t1, $t0, $t1
	lw $t0, -8($fp)
	add $t0, $t0, $t1
	li $t1, 65
	sw $t1, 4($t0)
#_prog_5
	li $t0, 4
	li $t1, 4
	mul $t1, $t0, $t1
	lw $t0, -8($fp)
	add $t0, $t0, $t1
	li $t1, 78
	sw $t1, 4($t0)
#_prog_6
	li $t0, 5
	li $t1, 4
	mul $t1, $t0, $t1
	lw $t0, -8($fp)
	add $t0, $t0, $t1
	li $t1, 15
	sw $t1, 4($t0)
#_prog_7
	li $t0, 6
	li $t1, 4
	mul $t1, $t0, $t1
	lw $t0, -8($fp)
	add $t0, $t0, $t1
	li $t1, 42
	sw $t1, 4($t0)
#_prog_8
	li $t0, 7
	li $t1, 4
	mul $t1, $t0, $t1
	lw $t0, -8($fp)
	add $t0, $t0, $t1
	li $t1, 32
	sw $t1, 4($t0)
#_prog_9
	lw $t0, -8($fp)
	sw $t0, -12($fp)
#_prog_10
	li $t0, 3
	li $t1, 4
	mul $t1, $t0, $t1
	lw $t0, -12($fp)
	add $t0, $t0, $t1
	lw $t1, 4($t0)
	sw $t1, -4($fp)
#_prog_11
	lw $a0, -4($fp)
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
