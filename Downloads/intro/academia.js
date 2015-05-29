// create your Animal class here
var Animal = function(name,numlegs){
	this.name = name;
	this.numlegs= numlegs;
};

// create the sayName method for Animal
Animal.prototype.sayName = function (){
	console.log("Hi my name is"+" "+ this.name);
};

// provided code to test above constructor and method
var penguin = new Animal("Captain Cook", 2);
penguin.sayName();

var w = {f:1};
console.log(w);

function matrixadd (a,b) {
	return a.map(function(b,c){
		return b.map(function(d,e){
			return d + b[c][e];
		});
	});
}

function adder(a) {
	return function(b) {
		return a+b;
	};
}
adder(2)(2);
// 4

function zeroFill(number, size) {
	return (new Array(size + 1).join(0) + Math.abs(~~number)).slice(-size);
}

// Checkerboard method
function checkerboard (size) {
	"use strict";
	var RED = '[r]';
	var BLACK = '[b]';
	var board = '';
	for(var row = 0; row < size; row++) {
		for(var col = 0; col < size; col++) {
			board += (row + col) % 2 === 0 ? RED : BLACK;
		}
		board += '\n';
	}
	return board;
}

(function d (a) {
	setTimeout(function(){
		console.log('hello');
		if (--a) d(a);
	},3000);
}(4));

function digital_root(n) {
	if (n < 10) return n;
	return digital_root(
		n.toString().split('').reduce(function(acc, d) { return acc + +d; }, 0));
}
digital_root(139);
// 4

Array.prototype.groupBy = function(fn) {
	return this.reduce(function(o, a){
		var v = fn ? fn(a) : a;
		return (o[v] = o[v] || []).push(a), o;
	}, {});
};

multiplicationTable = function(size) {
	var result = [];
	for (var i = 0; i < size; i++) {
		result[i] = [];
		for(var j = 0; j < size; j++) {
			result[i][j] = (i + 1) * (j + 1);
		}
	}
	return result;
};

function likes(names) {
	if(names.length === 0) return "no one likes this";
	if(names.length === 1) return names[0] + " likes this";
	if(names.length === 2) return names[0] + " and " + names[1] + " like this";
	if(names.length === 3) return names[0] + ", " + names[1] + " and " + names[2] + " like this";
	return names[0] + ", " + names[1] + " and " + (names.length - 2) + " others like this";
}

(2-2) || false;
// false

arr= Array.apply(null,[1,2]);
// [ 1, 2 ]

function fact(n) {
	return n==0?1:n*fact(n-1);
}
fact(4);
// 24

console.log(parseInt(2)%2);
// 0

var digit_name3 = (function(){
	var names = ['zero','one','two','three','four','five','six','seven','eight','nine'];
	return function(n){
		return names[n];
	};
})();
digit_name3(5);
// five

var pre = [3];
console.log( pre[0] );
// 3

var _ = {};// hole
_===_;
// true

Array.prototype.square  = function () { return this.map(function(n) { return n*n; }); };
Array.prototype.cube    = function () { return this.map(function(n) { return n*n*n; }); };
Array.prototype.average = function () { return this.sum() / this.length; };
Array.prototype.sum     = function () { return this.reduce(function(a, b) { return a + b; }, 0); };
Array.prototype.even    = function () { return this.filter(function(item) { return 0 == item % 2; }); };
Array.prototype.odd     = function () { return this.filter(function(item) { return 0 != item % 2; }); };
