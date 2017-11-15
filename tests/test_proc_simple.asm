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
	li $t4, 0
#_prog_1
	li $t3, 0
#_prog_2
	b _label_1
#_label_2
_label_2:
#_prog_4
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	sw $t5, -16($sp)
	addi $sp, $sp, -16
	sw $t2, -4($sp)
	addi $sp, $sp, -4
	jal print_ixe
	addi $sp, $sp, 4
	addi $sp, $sp, 16
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	lw $t5, -16($sp)
	move $t3, $a0
#_prog_5
#_prog_6
	li $t1, 1
	add $t4, $t4, $t1
#_prog_7
#_label_1
_label_1:
#_prog_9
	li $t1, 10
	slt $t5, $t4, $t1
#_prog_10
	bnez $t5, _label_2
#_prog_11
	move $a0, $t3
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
	lw $t2, 8($fp)
#_prog_0
	sw $t2, -4($fp)
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
