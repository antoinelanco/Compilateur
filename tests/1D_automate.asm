.text
	move $fp, $sp
	lw $a0, 0($a1)
	jal atoi
	move $a0, $v0
	jal main_integer
	li $v0, 10
	syscall
get_left_integer_integer_tableau_de_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t3, $a0
	move $t4, $a1
	move $t2, $a2
#_prog_0
	li $t1, 1
	sub $t2, $t3, $t1
#_prog_1
	seq $t2, $t4, $t2
#_prog_2
	bnez $t2, _label_1
#_prog_3
	li $t1, 1
	add $t2, $t4, $t1
#_prog_4
	sw $t2, -4($fp)
#_prog_5
	b _label_2
#_label_1
_label_1:
#_prog_7
	li $t0, 0
	sw $t0, -4($fp)
#_label_2
_label_2:
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
get_right_integer_integer_tableau_de_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	move $t3, $a0
	move $t4, $a1
	move $t2, $a2
#_prog_0
	li $t1, 0
	seq $t2, $t4, $t1
#_prog_1
	bnez $t2, _label_3
#_prog_2
	li $t1, 1
	sub $t2, $t4, $t1
#_prog_3
	sw $t2, -4($fp)
#_prog_4
	b _label_4
#_label_3
_label_3:
#_prog_6
	li $t1, 1
	sub $t2, $t3, $t1
#_prog_7
	sw $t2, -4($fp)
#_label_4
_label_4:
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
	sw $t4, -12($sp)
	sw $t5, -16($sp)
	sw $t6, -20($sp)
	addi $sp, $sp, -20
	move $a0, $t2
	addi $sp, $sp, 0
	jal make_regle_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 20
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	lw $t5, -16($sp)
	lw $t6, -20($sp)
	move $t2, $v0
#_prog_1
#_prog_2
	li $t4, 50
#_prog_3
	li $t3, 70
#_prog_4
	li $t0, 4
	mul $t0, $t3, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t5, $v0
#_prog_5
#_prog_6
	li $t1, 2
	div $t6, $t3, $t1
#_prog_7
	li $t1, 1
	sub $t6, $t6, $t1
#_prog_8
	li $t1, 4
	mul $t1, $t6, $t1
	add $t0, $t5, $t1
	li $t1, 1
	sw $t1, 4($t0)
#_prog_9
	li $t1, 2
	div $t6, $t3, $t1
#_prog_10
	li $t1, 4
	mul $t1, $t6, $t1
	add $t0, $t5, $t1
	li $t1, 1
	sw $t1, 4($t0)
#_prog_11
	li $t1, 2
	div $t6, $t3, $t1
#_prog_12
	li $t1, 1
	add $t6, $t6, $t1
#_prog_13
	li $t1, 4
	mul $t1, $t6, $t1
	add $t0, $t5, $t1
	li $t1, 1
	sw $t1, 4($t0)
#_prog_14
	b _label_5
#_label_6
_label_6:
#_prog_16
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	sw $t5, -16($sp)
	sw $t6, -20($sp)
	addi $sp, $sp, -20
	move $a0, $t3
	move $a1, $t5
	addi $sp, $sp, 0
	jal print_line_integer_tableau_de_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 20
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	lw $t5, -16($sp)
	lw $t6, -20($sp)
#_prog_17
	li $a0, 10
	li $v0, 11
	syscall
#_prog_18
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	sw $t5, -16($sp)
	sw $t6, -20($sp)
	addi $sp, $sp, -20
	move $a0, $t3
	move $a1, $t5
	move $a2, $t2
	addi $sp, $sp, 0
	jal update_integer_tableau_de_integer_tableau_de_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 20
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	lw $t5, -16($sp)
	lw $t6, -20($sp)
	move $t5, $v0
#_prog_19
#_prog_20
	li $t1, 1
	sub $t4, $t4, $t1
#_prog_21
#_label_5
_label_5:
#_prog_23
	li $t1, 0
	sgt $t6, $t4, $t1
#_prog_24
	bnez $t6, _label_6
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
	jr $ra
make_regle_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t5, $a0
#_prog_0
	li $t1, 8
	li $t0, 4
	mul $t0, $t1, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t2, $v0
#_prog_1
	move $t3, $t2
#_prog_2
	li $t4, 0
#_prog_3
	b _label_7
#_label_8
_label_8:
#_prog_5
	li $t1, 2
	div $t2, $t5, $t1
#_prog_6
#_prog_7
	li $t1, 2
	mul $t6, $t2, $t1
#_prog_8
	sub $t5, $t5, $t6
#_prog_9
	li $t1, 4
	mul $t1, $t4, $t1
	add $t0, $t3, $t1
	sw $t5, 4($t0)
#_prog_10
	move $t5, $t2
#_prog_11
	li $t1, 1
	add $t2, $t4, $t1
#_prog_12
	move $t4, $t2
#_label_7
_label_7:
#_prog_14
	li $t1, 8
	slt $t2, $t4, $t1
#_prog_15
	bnez $t2, _label_8
#_prog_16
	sw $t3, -4($fp)
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
print_line_integer_tableau_de_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	move $t3, $a0
	move $t2, $a1
#_prog_0
	li $t4, 0
#_prog_1
	b _label_9
#_label_10
_label_10:
#_prog_3
	li $t1, 4
	mul $t1, $t4, $t1
	add $t0, $t2, $t1
	lw $t5, 4($t0)
#_prog_4
	li $t1, 1
	seq $t5, $t5, $t1
#_prog_5
	bnez $t5, _label_11
#_prog_6
	li $a0, 32
	li $v0, 11
	syscall
#_prog_7
	li $a0, 32
	li $v0, 11
	syscall
#_prog_8
	b _label_12
#_label_11
_label_11:
#_prog_10
	li $a0, 35
	li $v0, 11
	syscall
#_prog_11
	li $a0, 35
	li $v0, 11
	syscall
#_label_12
_label_12:
#_prog_13
	li $t1, 1
	add $t4, $t4, $t1
#_prog_14
#_label_9
_label_9:
#_prog_16
	slt $t5, $t4, $t3
#_prog_17
	bnez $t5, _label_10
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
	jr $ra
update_integer_tableau_de_integer_tableau_de_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t5, $a0
	move $t4, $a1
	move $t3, $a2
#_prog_0
	li $t0, 4
	mul $t0, $t5, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t2, $v0
#_prog_1
#_prog_2
	li $t6, 0
#_prog_3
	b _label_13
#_label_14
_label_14:
#_prog_5
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	sw $t5, -16($sp)
	sw $t6, -20($sp)
	sw $t7, -24($sp)
	addi $sp, $sp, -24
	move $a0, $t6
	move $a1, $t5
	move $a2, $t4
	move $a3, $t3
	addi $sp, $sp, 0
	jal update_state_integer_integer_tableau_de_integer_tableau_de_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 24
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	lw $t5, -16($sp)
	lw $t6, -20($sp)
	lw $t7, -24($sp)
	move $t7, $v0
#_prog_6
	li $t1, 4
	mul $t1, $t6, $t1
	add $t0, $t2, $t1
	sw $t7, 4($t0)
#_prog_7
	li $t1, 1
	add $t6, $t6, $t1
#_prog_8
#_label_13
_label_13:
#_prog_10
	slt $t7, $t6, $t5
#_prog_11
	bnez $t7, _label_14
#_prog_12
	sw $t2, -4($fp)
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
update_state_integer_integer_tableau_de_integer_tableau_de_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	move $t5, $a0
	move $t4, $a1
	move $t3, $a2
	move $t2, $a3
#_prog_0
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	sw $t5, -16($sp)
	sw $t6, -20($sp)
	sw $t7, -24($sp)
	addi $sp, $sp, -24
	move $a0, $t4
	move $a1, $t5
	move $a2, $t3
	addi $sp, $sp, 0
	jal get_right_integer_integer_tableau_de_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 24
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	lw $t5, -16($sp)
	lw $t6, -20($sp)
	lw $t7, -24($sp)
	move $t6, $v0
#_prog_1
	li $t1, 4
	mul $t1, $t6, $t1
	add $t0, $t3, $t1
	lw $t7, 4($t0)
#_prog_2
	li $t1, 4
	mul $t1, $t5, $t1
	add $t0, $t3, $t1
	lw $t6, 4($t0)
#_prog_3
	li $t0, 2
	mul $t6, $t0, $t6
#_prog_4
	add $t7, $t7, $t6
#_prog_5
	li $t0, 2
	li $t1, 2
	mul $t6, $t0, $t1
#_prog_6
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	sw $t5, -16($sp)
	sw $t6, -20($sp)
	sw $t7, -24($sp)
	addi $sp, $sp, -24
	move $a0, $t4
	move $a1, $t5
	move $a2, $t3
	addi $sp, $sp, 0
	jal get_left_integer_integer_tableau_de_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 24
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	lw $t5, -16($sp)
	lw $t6, -20($sp)
	lw $t7, -24($sp)
	move $t4, $v0
#_prog_7
	li $t1, 4
	mul $t1, $t4, $t1
	add $t0, $t3, $t1
	lw $t3, 4($t0)
#_prog_8
	mul $t3, $t6, $t3
#_prog_9
	add $t3, $t7, $t3
#_prog_10
#_prog_11
	li $t1, 4
	mul $t1, $t3, $t1
	add $t0, $t2, $t1
	lw $t2, 4($t0)
#_prog_12
	sw $t2, -4($fp)
	lw $v0, -4($fp)
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
