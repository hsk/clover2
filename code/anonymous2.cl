a:Anonymous = new Integer(3);

Clover.test("Anonymous test1", a.toString().equals("3").cast("bool"));
Clover.test("Anonymous test2", a.equals(3.toInteger).cast("bool"));
Clover.test("Anonymous test3", a.getValue().cast("int") == 3);

b:Anonymous = new Integer(7);

c:Integer = b;
Clover.test("Anonymous test4", c == 7);

fun:lambda(int,int):Anonymous = lambda(a:int, b:int):Anonymous { return a+b };

d: Integer = fun(2,5);
Clover.test("Anonymous test5", d == 7);

print("Anonymous test6...");
e:List<Integer> = d.toAnonymous().cast("List<Integer>");
println("OK");

print("Anonymous test7...");
f:int[] = d.toAnonymous().cast("int[]");
println("OK");

