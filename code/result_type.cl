
a := new ResultTypeTest();

println("a.get() is \{a.get()}");

Clover.test("result type test", a.get() == 123);

Clover.test("result type test2", a.get2() == 345);

b:EqualableList<String> = equalable_list { 1, 2, 3 }.map { |n:Integer| n.toString(); }

Clover.test("result type test3", b.equals(equalable_list { "1", "2", "3" }));

c := equalable_list { 1, 2, 3 };

c.each { it.toString().println(); }
d:EqualableList<String> = equalable_list { 1, 2, 3 }.map { it.toString() }

Clover.test("result type test4", d.equals(equalable_list { "1", "2", "3" }));

a.method();

a.method2(new Integer(1), new Integer(2));

Clover.test("result type test7", slist{1,2,3}.map { it * 3}.equals(slist{3,6,9}));


Clover.test("result type test8", a.method3 { it + it2 } == 3);

e:SortableList<String> = slist{1,2,3}.map { return it.toString(); }

Clover.test("result type test9", slist{1,2,3}.map { return it.toString() }.equals(slist{"1","2","3"}));
