.class public pi
.super java/lang/Object

.method public <init>()V
  .limit locals 1

  aload_0
  invokespecial java/lang/Object/<init>()V
  return

.end method

.method public static main([Ljava/lang/String;)V
  .limit locals 1000
  .limit stack  1000

  ldc 1
  ldc 2
  iadd
  ldc 2
  imul
  ldc 5
  isub
  istore 0
  iload 0
  invokestatic Runtime/print(I)V
  ldc2_w 3.14159
  iload 0
  i2d
  dmul
  dstore 1
  dload 1
  iload 0
  i2d
  dadd
  iload 0
  i2d
  dsub
  dstore 1
  dload 1
  invokestatic Runtime/print(D)V
  return

.end method
