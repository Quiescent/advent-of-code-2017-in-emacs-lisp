# for (b = 107900; b <= 124900; b += 17)
#   for (d = 2; d <= b; ++d)
#     for (e = 2; e <= b; ++e)
#       # This is checking for instances where two numbers can
#       # multiply to give b.
#       if (d * e - b == 0)
#         h++
# Result is (c - b) / 17

# Smaller program:
h = 0
for b in xrange(107900, 124901, 17):
    flag = False
    for d in xrange(2, (b / 2) - 1):
        if (b % d == 0):
            flag = True
    if flag:
        h += 1
print "Result: ", h

h = 0
b = 79
c = b
b *= 100
b -= -100000
# b = 107900
print "B: ", b
c = b
# c = 107900
c -= -17000
# c = 124900
while True:
  f = 1
  d = 2
  while True:
    e = 2
    while True:
      g = d
      g *= e
      g -= b
      if g == 0:
        f = 0
      e -= -1
      g = e
      g -= b
      if g == 0:
        break
    d -= -1
    g = d
    g -= b
    if (g == 0):
      break;
  if f == 0:
    h -= -1
  g = b
  g -= c
  if g == 0:
    break
  b -= -17

print "Done with long running: ", h
