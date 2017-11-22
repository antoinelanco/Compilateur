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
	sw $t2, -4($sp)
	addi $sp, $sp, -4
	li $t0, 20
	move $a0, $t0
	addi $sp, $sp, 0
	jal make_tab
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $t2, -4($sp)
	move $t2, $v0
#_prog_1
#_prog_2
	sw $t2, -4($sp)
	addi $sp, $sp, -4
	move $a0, $t2
	addi $sp, $sp, 0
	jal print_tab
	addi $sp, $sp, 0
	addi $sp, $sp, 4
	lw $t2, -4($sp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
make_tab:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t2, $a0
#_prog_0
	li $t0, 4
	mul $t0, $t2, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t3, $v0
#_prog_1
#_prog_2
	li $t4, 0
#_prog_3
	b _label_1
#_label_2
_label_2:
#_prog_5
	li $t0, 4
	mul $t0, $t2, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t5, $v0
#_prog_6
	li $t1, 4
	mul $t1, $t4, $t1
	add $t0, $t3, $t1
	sw $t5, 4($t0)
#_prog_7
	li $t5, 0
#_prog_8
	b _label_3
#_label_4
_label_4:
#_prog_10
	add $t6, $t4, $t5
#_prog_11
	li $t1, 49
	add $t7, $t6, $t1
#_prog_12
	li $t1, 4
	mul $t1, $t4, $t1
	add $t0, $t3, $t1
	lw $t6, 4($t0)
#_prog_13
	li $t1, 4
	mul $t1, $t5, $t1
	add $t0, $t6, $t1
	sw $t7, 4($t0)
#_prog_14
	li $t1, 1
	add $t5, $t5, $t1
#_prog_15
#_label_3
_label_3:
#_prog_17
	li $t1, 10
	slt $t6, $t5, $t1
#_prog_18
	bnez $t6, _label_4
#_prog_19
	li $t1, 1
	add $t4, $t4, $t1
#_prog_20
#_label_1
_label_1:
#_prog_22
	li $t1, 10
	slt $t5, $t4, $t1
#_prog_23
	bnez $t5, _label_2
#_prog_24
	sw $t3, -4($fp)
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
print_tab:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	move $t2, $a0
#_prog_0
	li $t3, 0
#_prog_1
	b _label_5
#_label_6
_label_6:
#_prog_3
	li $t4, 0
#_prog_4
	b _label_7
#_label_8
_label_8:
#_prog_6
	li $t1, 4
	mul $t1, $t3, $t1
	add $t0, $t2, $t1
	lw $t5, 4($t0)
#_prog_7
	li $t1, 4
	mul $t1, $t4, $t1
	add $t0, $t5, $t1
	lw $t5, 4($t0)
#_prog_8
	move $a0, $t5
	li $v0, 11
	syscall
#_prog_9
	li $t1, 1
	add $t4, $t4, $t1
#_prog_10
#_label_7
_label_7:
#_prog_12
	li $t1, 10
	slt $t5, $t4, $t1
#_prog_13
	bnez $t5, _label_8
#_prog_14
	li $a0, 10
	li $v0, 11
	syscall
#_prog_15
	li $t1, 1
	add $t3, $t3, $t1
#_prog_16
#_label_5
_label_5:
#_prog_18
	li $t1, 10
	slt $t4, $t3, $t1
#_prog_19
	bnez $t4, _label_6
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
