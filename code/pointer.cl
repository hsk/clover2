
a:pointer = System.malloc(123.to_ulong);

a->int = 123;
System.assert(a->int == 123);

b:pointer = a + 4.to_ulong;

b->int = 245;

System.assert(b->int == 245);

b++;
b++;
b++
b++;

b->int = 345;

c:pointer = a + 8.to_ulong;
System.assert(c->int == 345);

System.free(a);
