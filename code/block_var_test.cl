a:int = 1;

System.assert(a == 1);

if(true) {
    a = 3;
    b := 3;

    System.assert(a == 3 && b == 3);
}

if(false) {
    a = 4;
    b := 4;
    c := 4;

    System.assert(a == 4 && b == 4 && c == 4);
}
else {
    a = 5;
    b := 5;
    c := 5;

    System.assert(a == 5 && b == 5 && c == 5);

    if(true) {
        a = 5;
        b = 6;
        c = 6;
        d := 7;

        System.assert(a == 5 && b == 6 && c == 6 && d == 7);

        a = 6;

        if(true) {
            a = 7;
            b = 7;
            c = 7;
        }
    }

    System.assert(a == 7 && b == 7 && c == 7);
}

