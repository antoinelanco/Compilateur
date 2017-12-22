.text
	move $fp, $sp
	lw $a0, 0($a1)
	jal atoi
	move $a0, $v0
	li $a0, 8
	li $v0, 9
	syscall
	la $t0, gestion_catch
	sw $t0, 4($v0)
	sw $v0, 0($v0)
	move $s0, $v0
	jal main_integer
gestion_catch:
	li $v0, 10
	syscall
fun_integer:
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
fun_integer_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t2, $a0
	move $t3, $a1
#_prog_0
	li $t5, 10
#_prog_1
	li $t4, 20
#_prog_2
	move $a0, $t5
	li $v0, 11
	syscall
#_prog_3
	move $a0, $t4
	li $v0, 11
	syscall
#_prog_4
	move $a0, $t3
	li $v0, 11
	syscall
#_prog_5
	move $a0, $t2
	li $v0, 11
	syscall
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
main_integer:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, 0
	move $t2, $a0
#_prog_0
	li $t1, 2
	li $t0, 4
	mul $t0, $t1, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t3, $v0
#_prog_1
#_prog_2
	li $t0, 0
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t3, $t1
	li $t1, 70
	sw $t1, 4($t0)
#_prog_3
	li $t0, 0
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t3, $t1
	lw $t3, 4($t0)
#_prog_4
	move $a0, $t3
	li $v0, 11
	syscall
#_prog_5
	lw $t0, 4($s0)
	jr $t0
#_prog_6
	li $t0, 4
	mul $t0, $t2, $t0
	addi $a0, $t0, 4
	li $v0, 9
	syscall
	sw $a0, 0($v0)
	move $t3, $v0
#_prog_7
	move $t2, $t3
#_prog_8
	li $t0, 0
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	li $t1, 65
	sw $t1, 4($t0)
#_prog_9
	li $t0, 0
	li $t1, 4
	mul $t1, $t0, $t1
	add $t0, $t2, $t1
	lw $t2, 4($t0)
#_prog_10
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	move $a0, $t2
	addi $sp, $sp, 0
	jal fun_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
#_prog_11
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	addi $sp, $sp, -8
	li $t0, 60
	move $a0, $t0
	li $t0, 50
	move $a1, $t0
	addi $sp, $sp, 0
	jal fun_integer_integer
	addi $sp, $sp, 0
	addi $sp, $sp, 8
	lw $t2, -4($sp)
	lw $t3, -8($sp)
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
