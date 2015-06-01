//"no strict";
var strict_mode = false;
var NotEarlyErrorString = "NotEarlyError";
var EarlyErrorRePat = "^((?!" + NotEarlyErrorString + ").)*$";
var NotEarlyError = new Error(NotEarlyErrorString);

function Test262Error(message) {
    this.message = message || "";
}

Test262Error.prototype.toString = function () {
    return "Test262Error: " + this.message;
};

var $ERROR;
$ERROR = function $ERROR(message) {
    throw new Test262Error(message);
};

function testFailed(message) {
    $ERROR(message);
}

function testRun(id, path, description, codeString, result, error) {
  if (result!=="pass") {
      throw new Error("Test '" + path + "'failed: " + error);
  }
}

// define a default `print` function for async tests where there is no 
// global `print`
var print;

// in node use console.log
if (typeof console === "object") {
    print = function () {
        var args = Array.prototype.slice.call(arguments);
        console.log(args.join(" "));
    };
}

// in WScript, use WScript.Echo
if (typeof WScript === "object") {
    print = function () {
        var args = Array.prototype.slice.call(arguments);
        WScript.Echo(args.join(" "));
    };

    // also override $ERROR to force a nonzero exit code exit 
    // TODO? report syntax errors
    var oldError = $ERROR;
    $ERROR = function (message) {
        print("Test262 Error: " + message);
        WScript.Quit(1);
    };
}

function assert(mustBeTrue, message) {
    if (mustBeTrue === true) {
        return;
    }

    if (message === undefined) {
        message = 'Expected true but got ' + String(mustBeTrue);
    }
    $ERROR(message);
}

assert._isSameValue = function (a, b) {
    if (a === b) {
        // Handle +/-0 vs. -/+0
        return a !== 0 || 1 / a === 1 / b;
    }

    // Handle NaN vs. NaN
    return a !== a && b !== b;
};

assert.sameValue = function (actual, expected, message) {
    if (assert._isSameValue(actual, expected)) {
        return;
    }

    if (message === undefined) {
        message = 'Expected SameValue(' + String(actual) + ', ' + String(expected) + ') to be true';
    }
    $ERROR(message);
};

assert.notSameValue = function (actual, unexpected, message) {
    if (!assert._isSameValue(actual, unexpected)) {
        return;
    }

    if (message === undefined) {
        message = 'Expected SameValue(' + String(actual) + ', ' + String(unexpected) + ') to be false';
    }
    $ERROR(message);
};

assert.throws = function (expectedErrorConstructor, func) {
    if (func === undefined) {
        $ERROR('assert.throws requires two arguments: the error constructor and a function to run');
        return;
    }

    try {
        func();
    } catch (thrown) {
        if (typeof thrown !== 'object' || thrown === null) {
            $ERROR('Thrown value was not an object!');
            return;
        }
        if (thrown.constructor !== expectedErrorConstructor) {
            $ERROR('Expected a ' + expectedErrorConstructor.name + ' but got a ' + thrown.constructor.name);
        }
        return;
    }

    $ERROR('Expected a ' + expectedErrorConstructor.name + ' to be thrown but no exception was thrown at all');
};

//CHECK#1 
var OBJECT = 0;
if ((OBJECT = Object, {}) instanceof OBJECT !== true) {
  $ERROR('#1: var OBJECT = 0; (OBJECT = Object, {}) instanceof OBJECT === true');
}

//CHECK#2
var object = {}; 
if (object instanceof (object = 0, Object) !== true) {
  $ERROR('#2: var object = {};  object instanceof (object = 0, Object) === true');
}


