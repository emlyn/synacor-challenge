#!/usr/bin/env python3

# 0x178b: jt   r0  0x1793 /* 6035 */
# 0x178e: add  r0  r1  0x01 /* 1 */
# 0x1792: ret
# 0x1793: jt   r1  0x17a0 /* 6048 */
# 0x1796: add  r0  r0  0x7fff /* 32767 */
# 0x179a: set  r1  r7
# 0x179d: call 0x178b /* 6027 */
# 0x179f: ret
# 0x17a0: push r0
# 0x17a2: add  r1  r1  0x7fff /* 32767 */
# 0x17a6: call 0x178b /* 6027 */
# 0x17a8: set  r1  r0
# 0x17ab: pop  r0
# 0x17ad: add  r0  r0  0x7fff /* 32767 */
# 0x17b1: call 0x178b /* 6027 */
# 0x17b3: ret

# https://rosettacode.org/wiki/Ackermann_function#Python:_Without_recursive_function_calls

r7 = 0

def ack_ix(m, n):
  "Paddy3118's iterative with optimisations on m"

  stack = [m, n]

  while  len(stack) > 1:
    global r7
    n, m = stack.pop(), stack.pop()

    if   m == 0:
      stack.append(n + 1)
    elif m == 1:
      stack.append(n + r7 + 1)
    elif m == 2:
      #r7=1: 2n+3, r7=2: 3n+5, r7=3: 4n+7
      stack.append(((r7 + 1) * n + 2 * r7 + 1) & 0x7fff)
    #elif m == 3:
    #    stack.append((2**(n + 3) - 3) & 0x7fff)
    elif n == 0:
      stack.extend([m-1, r7])
    else:
      stack.extend([m-1, m, n-1])

  return stack[0]

# r7 = 1
# m = 3
# for n in range(10):
#   print(f"r7 = {r7}: ack({m}, {n}) = {ack_ix(m, n)}")

for i in range(1, 2**15):
  r7 = i
  v = ack_ix(4, 1) & 0x7fff
  print(f"r7 = {r7}, v = {v}")
  if v == 6:
    break

# r7 = 25734
