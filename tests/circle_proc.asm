.text
	move $fp, $sp
	lw $a0, 0($a1)
	jal atoi
	sw $v0, 0($sp)
	jal main
	li $v0, 10
	syscall
line:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	lw $t4, 12($fp)
	lw $t3, 8($fp)
#_prog_0
	li $t0, 0
	sw $t0, -4($fp)
#_prog_1
	li $t5, 0
#_prog_2
	b _label_1
#_label_2
_label_2:
#_prog_4
	slt $t2, $t5, $t4
#_prog_5
	bnez $t2, _label_3
#_prog_6
	li $a0, 35
	li $v0, 11
	syscall
#_prog_7
	b _label_4
#_label_3
_label_3:
#_prog_9
	li $a0, 46
	li $v0, 11
	syscall
#_prog_10
	li $t0, 1
	sw $t0, -4($fp)
#_label_4
_label_4:
#_prog_12
	li $a0, 32
	li $v0, 11
	syscall
#_prog_13
	li $t1, 1
	add $t2, $t5, $t1
#_prog_14
	move $t5, $t2
#_label_1
_label_1:
#_prog_16
	li $t1, 1
	add $t2, $t3, $t1
#_prog_17
	slt $t2, $t5, $t2
#_prog_18
	bnez $t2, _label_2
#_prog_19
	li $a0, 10
	li $v0, 11
	syscall
	lw $a0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
loop:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	lw $t3, 12($fp)
	lw $t2, 8($fp)
#_prog_0
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	addi $sp, $sp, -12
	sw $t3, -4($sp)
	sw $t2, -8($sp)
	addi $sp, $sp, -8
	jal point
	addi $sp, $sp, 8
	addi $sp, $sp, 12
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	move $t4, $a0
#_prog_1
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	addi $sp, $sp, -12
	sw $t4, -4($sp)
	sw $t2, -8($sp)
	addi $sp, $sp, -8
	jal line
	addi $sp, $sp, 8
	addi $sp, $sp, 12
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	move $t4, $a0
#_prog_2
	bnez $t4, _label_5
#_prog_3
	b _label_6
#_label_5
_label_5:
#_prog_5
	li $t1, 1
	add $t3, $t3, $t1
#_prog_6
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	addi $sp, $sp, -12
	sw $t3, -4($sp)
	sw $t2, -8($sp)
	addi $sp, $sp, -8
	jal loop
	addi $sp, $sp, 8
	addi $sp, $sp, 12
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
#_label_6
_label_6:
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
	jr $ra
main:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	lw $t2, 8($fp)
#_prog_0
	sw $t2, -4($sp)
	addi $sp, $sp, -4
	li $t0, 0
	sw $t0, -4($sp)
	sw $t2, -8($sp)
	addi $sp, $sp, -8
	jal loop
	addi $sp, $sp, 8
	addi $sp, $sp, 4
	lw $t2, -4($sp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
	jr $ra
point:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	lw $t4, 12($fp)
	lw $t2, 8($fp)
#_prog_0
	li $t3, 0
#_prog_1
	b _label_7
#_label_8
_label_8:
#_prog_3
	li $t1, 1
	add $t3, $t3, $t1
#_prog_4
#_label_7
_label_7:
#_prog_6
	mul $t6, $t4, $t4
#_prog_7
	mul $t5, $t3, $t3
#_prog_8
	add $t6, $t6, $t5
#_prog_9
	mul $t5, $t2, $t2
#_prog_10
	slt $t5, $t6, $t5
#_prog_11
	bnez $t5, _label_8
#_prog_12
	sw $t3, -8($fp)
	lw $a0, -8($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
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
