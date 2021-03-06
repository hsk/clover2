# clover2 computer language

[![Build Status](https://travis-ci.org/ab25cq/clover2.svg?branch=master)](https://travis-ci.org/ab25cq/clover2)


version 3.1.9

サポートしている機能

1. プリミティブクラスと普通クラスの2種類あり、プリミティブクラスはヒープを使いません。メソッド呼び出しも無く演算子のみサポートしていてプリミティブクラスの値への処理は高速です。しかしboxingやunboxingがありプリミティブクラスにメソッドコールを行ったときやコンテナライブラリに値を追加する場合などはboxingされてプリミティブクラスの使いずらさを軽減しています。逆に演算子の対象になった場合はunboxingされます。

2. 関数型言語のようにLambdaやclosureは第一級オブジェクトです。正規表現も第一級オブジェクトです。

3. 簡易なGenericsがあります。JavaのGenericsを簡素にしたようものです。コンパイル時のみGenerics情報があり、実行時には消えているので効率的です。

4. 簡略化のために継承はありません。インターフェースとクラスとモジュールのみあります。そのためインターフェースに対するメソッド呼び出し以外はすべてコンパイル時にメソッドが特定でき効率的です。

5. オープンクラスです。組み込みのクラスを含む全てのクラスに後からメソッドやフィールドを追加することができます。同名のメソッドを定義することができ、mixin-layersスタイルのような差分プログラミングをすることができます。

6. シェルのようなインタプリタもあります。外部コマンドも簡単に実行でき、clover2のメソッドとも簡単に混ぜることができます。

> ls().grep("main.c").toString().scan(/./).toCommand().less()
list {m,a,i,n,.,c}

とless外部コマンドで表示される。 メソッド名や外部コマンド名、ファイル名の補完もされます。

7. LLVMでJITします。

インストール方法については詳しくはwikiを見てください https://github.com/ab25cq/clover2/wiki

Yet another compiler and a Virtual Machine.

FEATURES

1. primitive class and none primitive class with boxing and unboxing
2. Lambda and closure, regex are first-class object
3. Generics
4. No inheritance, only interfaces and classes, modules
5. open class
6. LLVM JIT

See clover2 wiki on github (Japanese) https://github.com/ab25cq/clover2/wiki

LICENSE is GPL-2.0. see LICENSE file

