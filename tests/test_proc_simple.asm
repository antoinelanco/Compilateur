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
	move $t2, $a0
#_prog_0
	li $t6, 65
#_prog_1
	li $t5, 66
#_prog_2
	li $t4, 67
#_prog_3
	li $t3, 68
#_prog_4
	li $t2, 69
#_prog_5
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	sw $t5, -16($sp)
	sw $t6, -20($sp)
	addi $sp, $sp, -20
	move $a0, $t6
	move $a1, $t5
	move $a2, $t4
	move $a3, $t3
	sw $t2, -4($sp)
	addi $sp, $sp, -4
	jal print_ixe
	addi $sp, $sp, 4
	addi $sp, $sp, 20
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	lw $t5, -16($sp)
	lw $t6, -20($sp)
	move $t2, $v0
#_prog_6
#_prog_7
	move $a0, $t2
	li $v0, 11
	syscall
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
print_ixe:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t6, $a0
	move $t5, $a1
	move $t4, $a2
	move $t3, $a3
	lw $t2, 8($fp)
#_prog_0
	move $a0, $t6
	li $v0, 11
	syscall
#_prog_1
	move $a0, $t5
	li $v0, 11
	syscall
#_prog_2
	move $a0, $t4
	li $v0, 11
	syscall
#_prog_3
	move $a0, $t3
	li $v0, 11
	syscall
#_prog_4
	move $a0, $t2
	li $v0, 11
	syscall
#_prog_5
	li $t0, 70
	sw $t0, -4($fp)
	lw $v0, -4($fp)
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
