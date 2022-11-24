.class public prime
.super java/lang/Object

;; Constructor for class prime

.method public <init>()V
.limit stack 1

	aload_0
	invokespecial java/lang/Object/<init>()V
	return

.end method

;; Default Java entrypoint: void main (String [])

.method public static main([Ljava/lang/String;)V
.limit stack 1

	invokestatic prime/main()I
	pop
	return

.end method

;; C-- entrypoint: int main()

.method public static main()I
.limit locals 0
.limit stack 1

	;; if (prime (641))

	ldc	641
	invokestatic	prime/prime(I)Z
	ifne	L0

	;; printInt (0);

	iconst_0
	invokestatic	Runtime/printInt(I)V
	goto	L1
L0:

	;; printInt (641);

	ldc	641
	invokestatic	Runtime/printInt(I)V
L1:
	iconst_0
	ireturn

.end method

.method public static divides(II)Z
.limit locals 2
.limit stack 2

	;; return p / q * q == p;

	iload_1         ;; p
	iload_0         ;; q
	idiv
	iload_0
	imul
	iload_1
	if_icmpeq       L0
	iconst_0
	goto	L1
L0:
	iconst_1
L1:
	ireturn

.end method

.method public static prime(I)Z
.limit locals 2
.limit stack 2

	;; if (p <= 1 || divides (2, p))

	iload_0         ;; p
	iconst_1
	if_icmple	L4
	iconst_2
	iload_0
	invokestatic	prime/divides(II)Z
	ifne	L4

	;; int q = 3;

	iconst_3
	istore_1

	;; while (q * q <= p)

	goto	L3
L0:

	;; if (divides (q, p))

	iload_1
	iload_0
	invokestatic	prime/divides(II)Z
	ifne	L1

	;; q = q + 2;

	iload_1
	iconst_2
	iadd
	istore_1
	goto	L2
L1:

	;; return false;

	iconst_0
	ireturn
L2:
L3:
	iload_1
	iload_1
	imul
	iload_0
	if_icmple	L0
	goto	L5
L4:

	;; return false;

	iconst_0
	ireturn
L5:

	;; return true;

	iconst_1
	ireturn

.end method
