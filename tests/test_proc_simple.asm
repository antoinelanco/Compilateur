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
	lw $t2, 8($fp)
#_prog_0
	li $t3, 0
#_prog_1
	b _label_1
#_label_2
_label_2:
#_prog_3
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	addi $sp, $sp, -12
	sw $t2, -4($sp)
	li $t0, 51
	sw $t0, -8($sp)
	addi $sp, $sp, -8
	jal print_ixe
	addi $sp, $sp, 8
	addi $sp, $sp, 12
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
#_prog_4
	li $a0, 10
	li $v0, 11
	syscall
#_prog_5
	li $t1, 1
	add $t3, $t3, $t1
#_prog_6
#_label_1
_label_1:
#_prog_8
	li $t1, 10
	slt $t4, $t3, $t1
#_prog_9
	bnez $t4, _label_2
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
	lw $t3, 12($fp)
	lw $t2, 8($fp)
#_prog_0
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	li $t0, 52
	sw $t0, -12($sp)
	li $t0, 53
	sw $t0, -16($sp)
	li $t0, 54
	sw $t0, -20($sp)
	li $t0, 55
	sw $t0, -24($sp)
	li $t0, 56
	sw $t0, -28($sp)
	li $t0, 57
	sw $t0, -32($sp)
	li $t0, 55
	sw $t0, -36($sp)
	addi $sp, $sp, -36
	jal print_ixeprime
	addi $sp, $sp, 36
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
print_ixeprime:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	lw $t2, 40($fp)
	lw $t3, 36($fp)
	lw $t4, 32($fp)
	lw $t5, 28($fp)
	lw $t6, 24($fp)
	lw $t7, 20($fp)
	lw $t8, 16($fp)
	lw $t9, 12($fp)
	lw $t0, 4($fp)
	sw $t0, -4($fp)
#_prog_0
	lw $a0, -4($fp)
	li $v0, 11
	syscall
#_prog_1
	move $a0, $t9
	li $v0, 11
	syscall
#_prog_2
	move $a0, $t8
	li $v0, 11
	syscall
#_prog_3
	move $a0, $t7
	li $v0, 11
	syscall
#_prog_4
	move $a0, $t6
	li $v0, 11
	syscall
#_prog_5
	move $a0, $t5
	li $v0, 11
	syscall
#_prog_6
	move $a0, $t4
	li $v0, 11
	syscall
#_prog_7
	move $a0, $t3
	li $v0, 11
	syscall
#_prog_8
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
