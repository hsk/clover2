
a:=0;
3.times() {
    a++;
}

Clover.test("simple block test1", a == 3);

sum := 0;
list { 1, 2, 3 }.each() {|i:Integer|
    sum += i;
}

Clover.test("simple block test2", sum == 6);

list { 1,2,3 }.each() {|n:Integer| n.toString().println() }

str := "";
list { 1,2,3 }.map() {|n:Integer|:Anonymous return n.toString(); }.each() {|n:Anonymous| str.append(n.cast("String")); }

Clover.test("simple block test3", str.equals("123"));

str = "";
equalable_list { 1,2,3 }.map() {|n:Integer|:Anonymous return n.toString(); }.each() {|n:Anonymous| str.append(n.cast("String")); }.toString().println();
