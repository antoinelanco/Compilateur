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
#_main_0
	li $a0, 10
	jal new_array
	move $t3, $v0
#_main_1
	move $t2, $t3
#_main_2
	li $a0, 10
	jal new_array
	move $t3, $v0
#_main_3
	li $a0, 2
	move $a1, $t2
	jal _check_array_bounds
	move $a0, $t2
	li $a1, 2
	move $a2, $t3
	jal _store_in_array
#_main_4
	li $a0, 2
	move $a1, $t2
	jal _check_array_bounds
	move $a0, $t2
	li $a1, 2
	jal _load_array_elt
	move $t3, $v0
#_main_5
	li $a0, 3
	move $a1, $t3
	jal _check_array_bounds
	move $a0, $t3
	li $a1, 3
	li $a2, 65
	jal _store_in_array
#_main_6
	li $a0, 2
	move $a1, $t2
	jal _check_array_bounds
	move $a0, $t2
	li $a1, 2
	jal _load_array_elt
	move $t3, $v0
#_main_7
	li $a0, 3
	move $a1, $t3
	jal _check_array_bounds
	move $a0, $t3
	li $a1, 3
	jal _load_array_elt
	move $t3, $v0
#_main_8
	move $a0, $t3
	addi $sp, $sp, 0
	jal print
	addi $sp, $sp, 0
#_main_9
	li $a0, 2
	move $a1, $t2
	jal _check_array_bounds
	move $a0, $t2
	li $a1, 2
	jal _load_array_elt
	move $t3, $v0
#_main_10
	li $a0, 3
	move $a1, $t3
	jal _check_array_bounds
	move $a0, $t3
	li $a1, 3
	jal _load_array_elt
	move $t3, $v0
#_main_11
	sw $t2, -4($sp)
	sw $t3, -8($sp)
	sw $t4, -12($sp)
	addi $sp, $sp, -12
	move $a0, $t3
	addi $sp, $sp, 0
	jal make
	addi $sp, $sp, 0
	addi $sp, $sp, 12
	lw $t2, -4($sp)
	lw $t3, -8($sp)
	lw $t4, -12($sp)
	move $t3, $v0
#_main_12
	move $t2, $t3
#_main_13
	li $a0, 1
	move $a1, $t2
	jal _check_array_bounds
	move $a0, $t2
	li $a1, 1
	jal _load_array_elt
	move $t3, $v0
#_main_14
	li $a0, 2
	move $a1, $t3
	jal _check_array_bounds
	move $a0, $t3
	li $a1, 2
	jal _load_array_elt
	move $t3, $v0
#_main_15
	move $a0, $t3
	addi $sp, $sp, 0
	jal print
	addi $sp, $sp, 0
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 8
	jr $ra
make:
	sw $fp, -4($sp)
	sw $ra, -8($sp)
	addi $sp, $sp, -8
	move $fp, $sp
	addi $sp, $sp, -4
	move $t2, $a0
#_make_0
	move $a0, $t2
	jal new_array
	move $t3, $v0
#_make_1
	move $t2, $t3
#_make_2
	li $a0, 10
	jal new_array
	move $t3, $v0
#_make_3
	li $a0, 1
	move $a1, $t2
	jal _check_array_bounds
	move $a0, $t2
	li $a1, 1
	move $a2, $t3
	jal _store_in_array
#_make_4
	li $a0, 1
	move $a1, $t2
	jal _check_array_bounds
	move $a0, $t2
	li $a1, 1
	jal _load_array_elt
	move $t3, $v0
#_make_5
	li $a0, 2
	move $a1, $t3
	jal _check_array_bounds
	move $a0, $t3
	li $a1, 2
	li $a2, 66
	jal _store_in_array
#_make_6
	sw $t2, -4($fp)
	lw $v0, -4($fp)
	lw $ra, 0($fp)
	lw $fp, 4($fp)
	addi $sp, $sp, 12
	jr $ra
new_array:
	move $t0, $a0
	li $t1, 4
	mul $t0, $t0, $t1
	addi $t0, $t0, 4
	li $v0, 9
	move $t1, $a0
	move $a0, $t0
	syscall
	sw $t1, 0($v0)
	jr $ra
_load_array_elt:
	move $t0, $a1
	li $t1, 4
	mul $t0, $t0, $t1
	addi $t0, $t0, 4
	move $t1, $a0
	add $t0, $t0, $t1
	lw $v0, 0($t0)
	jr $ra
_store_in_array:
	move $t0, $a1
	li $t1, 4
	mul $t0, $t0, $t1
	addi $t0, $t0, 4
	move $t1, $a0
	add $t0, $t0, $t1
	move $t1, $a2
	sw $t1, 0($t0)
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
_check_array_bounds:
	bgez $a0, _ckeck_bound_1
	move $t0, $a0
	li $v0, 4
	la $a0, _array_out_of_bounds_string
	syscall
	li $a0, 45
	jal print
	neg $t0, $t0
	addi $a0, $t0, 48
	jal print
	li $a0, 10
	jal print
	li $v0, 10
	syscall
_ckeck_bound_1:
	lw $a1, 0($a1)
	blt $a0, $a1, _ckeck_bound_2
	move $t0, $a0
	li $v0, 4
	la $a0, _array_out_of_bounds_string
	syscall
	addi $a0, $t0, 48
	jal print
	li $a0, 10
	jal print
	li $v0, 10
	syscall
_ckeck_bound_2:
	jr $ra
