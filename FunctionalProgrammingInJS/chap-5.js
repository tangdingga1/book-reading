// 5.1 包裹值的函数式数据
class Wrapper {
    constructor(value) {
        this._value = value;
    }
    map(f) {
        return f(this._value);
    }
    toString() {
        return 'Wrapper(' + this.value + ')';
    }
}

Wrapper.prototype.fmap = function(f) {
    return warp(f(this._value))
}

const warp = val => new Wrapper(val);

// Monad 创建一定规则的容器
const Empty = function() {};
Empty.prototype.map = function() { return this; };
const empty = () => new Empty();

const isEven = (n) => Number.isFinite(n) && (n % 2 == 0);
const half = (val) => isEven(val) ? warp(val / 2) : empty();

// wrapper monad
class Wrapper {
    constructor(value) {
        this.value = value;
    }
    // unit 指定值放入 Monadic结构中
    static of (a) {
        return new Wrapper(a);
    }
    // bind 函数
    map(f) {
        return Wrapper.of(f(this.value));
    }
    // 扁平化
    join() {
        if (!(this.value instanceof Wrapper)) {
            return this;
        }
        return this.value.join();
    }
    toString() {
        return `Wrapper (${this.value})`;
    }
}
// 使用
Wrapper.of('Hello Monads!')
    .map(R.toUpper)
    .map(R.identity);

// IO Monad
class IO {
    constructor(effect) {
        if (!_.isFunction(effect)) {
            throw 'IO Usage: function required';
        }
        this.effect = effect;
    }
    // 处理入参为非函数
    static of(a) {
        return new IO(() => a);
    }
    // 入参为函数
    static from(fn) {
        return new IO(fn);
    }
    map(fn) {
        var self = this;
        return new IO(function() {
            return fn(self.effect())
        });
    }
    chain(fn) {
        return fn(this.effect());
    }
    run() {
        return this.effect();
    }
}