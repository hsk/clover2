
a:Array<Integer> = {1, 2, 3}.toArray;

Clover.test("array3 test1", a.length() == 3);
Clover.test("array3 test2", a.items[0] == 1 && a.items[1] == 2 && a.items[2] == 3);


a.each(lambda(item:Integer)
{
    System.println(item.toString());
});

b:int[] = { 1, 2, 3 };

b.each(lambda(item:Integer)
{
    System.println(item.toString());
});


