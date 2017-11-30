.text
	move $fp, $sp
	lw $a0, 0($a1)
	jal atoi
	move $a0, $v0
	jal main_integer
	li $v0, 10
	syscall
dummy_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t2, $a0
#_prog_0
	sw $t2, -4($fp)
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
dummy_integer_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	move $t2, $a0
	move $t3, $a1
#_prog_0
	add $t2, $t3, $t2
#_prog_1
	sw $t2, -4($fp)
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
	jr $ra
main_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	move $t2, $a0
#_prog_0
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	move $a0, $t2
	addi $sp, $sp, 0
	jal print_surch_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
#_prog_1
	li $a0, 10
	li $v0, 11
	syscall
#_prog_2
	li $t1, 1
	add $t3, $t2, $t1
#_prog_3
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	move $a0, $t3
	move $a1, $t2
	addi $sp, $sp, 0
	jal print_surch_integer_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
#_prog_4
	li $a0, 10
	li $v0, 11
	syscall
#_prog_5
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	move $a0, $t2
	addi $sp, $sp, 0
	jal dummy_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	move $t3, $v0
#_prog_6
	move $a0, $t3
	li $v0, 11
	syscall
#_prog_7
	li $a0, 10
	li $v0, 11
	syscall
#_prog_8
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	li $t0, 1
	move $a0, $t0
	move $a1, $t2
	addi $sp, $sp, 0
	jal dummy_integer_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	move $t2, $v0
#_prog_9
	move $a0, $t2
	li $v0, 11
	syscall
#_prog_10
	li $a0, 10
	li $v0, 11
	syscall
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
	jr $ra
print_surch_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t2, $a0
#_prog_0
	move $a0, $t2
	li $v0, 11
	syscall
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
print_surch_integer_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t2, $a0
	move $t3, $a1
#_prog_0
	move $a0, $t3
	li $v0, 11
	syscall
#_prog_1
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
