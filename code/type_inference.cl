
a:lambda(int,int):int = lambda(a:int, b:int):int { return a + b };
b:lambda(int,int):int = lambda(a:int, b:int) { return a + b };

Clover.test("type inference test1", a(1,2) == 3);
Clover.test("type inference test2", b(3,4) == 7);

c:=0;

Clover.test("type inference test3", c.className().equals("Integer"));
