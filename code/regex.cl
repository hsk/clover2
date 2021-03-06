
a:regex = /^A/;

pcre_ovec: PcreOVec = new PcreOVec(3);

Clover.test("regex test", System.pcre_exec(a, B"ABC", 0, 3, pcre_ovec) >= 0);

printf("ovec.start[0] %d ovec.end[0] %d\n", array { pcre_ovec.start[0].toInteger, pcre_ovec.end[0].toInteger } );
Clover.test("regex test", pcre_ovec.start[0] == 0 && pcre_ovec.end[0] == 1);

pcre_ovec2: PcreOVec = new PcreOVec(3);

Clover.test("regex test2", System.pcre_exec(/bc$/, B"abc", 0, 3, pcre_ovec2) >= 0);
Clover.test("regex test2", pcre_ovec2.start[0] == 1 && pcre_ovec2.end[0] == 3);

pcre_ovec3: PcreOVec = new PcreOVec(3);

Clover.test("regex test3", System.pcre_exec(/^[a-z]/i, B"ABC", 0, 3, pcre_ovec3) >= 0);
Clover.test("regex test3", pcre_ovec3.start[0] == 0 && pcre_ovec3.end[0] == 1);

b:regex = /^A/g;

Clover.test("regex test4", b.global && !b.ignoreCase && !b.multiline && !b.extended && !b.dotAll && !b.anchored && !b.dollarEndOnly && !b.ungreedy);

Clover.test("regex test5", /./.toString.equals("."));
Clover.test("regex test6", /./ == /./);
