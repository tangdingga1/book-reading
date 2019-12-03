// 清单 4.2 Tuple 数据类型
const Tuple = function () {
    const typeInfo = Array.prototype.slice.call(arguments, 0);
    const _T = function () {
        const value = Array.prototype.slice.call(arguments, 0);
        // 判空
        if (value.some(val => val == null)) {
            throw new ReferenceError('Touple may not have any null values');
        }
        if (value.length !== typeInfo.length) {
            throw new TypeError('Touple arity does not match its prototype');
        }
        values.map(function(val, index) {
            this['_' + (index + 1)] = checkType(typeInfo[index])(val);
        }, this);
        Object.freeze(this);
    }
    _T.prototype.values = function() {
        return Object.keys(this).map(function(k) {
            return this[k];
        }, this);
    }
    return _T;
}

// StringPair
const StringPair = Tuple(String, String);
const name = new StringPair('Barkley', 'Rosser');
[first, last] = name.values();

// 二次参数的科里化
function curry2(fn) {
    return function(firstArg) {
        return function(secondArg) {
            return fn(firstArg, secondArg);
        }
    }
}
// 使用范例
const name = curry2(function(last, first) {
    return new StringPair(last, first);
});

name('Curry')('Hasjell').values();

// 4.5.2 描述与求值分离

const explode = (str) => str.split(/\s+/);

const count = (arr) => arr.length;

const countWords = compose(count, explode);

countWords(str); // -> str 非空字符长度

function compose() {
    let args = arguments;
    let start = args.length - 1;
    return function() {
        let i = start;
        // 第一个函数调用直接传递所有的arguments
        let result = args[start].apply(this, arguments);
        while (i--) {
            result = args[i].call(this, result);
        }
        return result;
    }
}