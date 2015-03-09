function clFormat(str, indentSize) {
    var clThis;
    var lineLength;
    var indentPosition;
    var tokenCount;
    var escaped;
    var quoted;
    var commented;
    var through;
    var collect;
    var specialIndent;
    var countToken;
    var getSpaceOfIndentation;
    var insertSpacesForToken;
    var throughToken;
    var throughString;
    var throughSingleLineComment;
    if (indentSize === undefined) {
        indentSize = 4;
    };
    var result = '';
    var tokenCharacters = [];
    var insertNSpaces = function (n) {
        for (var i = 1; i <= n; i += 1) {
            result += ' ';
        };
        return n;
    };
    var emptyTokenCharacters = function () {
        var token = '';
        var _js1 = tokenCharacters.reverse();
        var _js3 = _js1.length;
        for (var _js2 = 0; _js2 < _js3; _js2 += 1) {
            var c = _js1[_js2];
            result += c;
            token += c;
        };
        tokenCharacters = [];
        return token;
    };
    var lastChar = function () {
        return result[result.length - 1];
    };
    var fsm = (clThis = null, lineLength = 0, indentPosition = [[0]], tokenCount = [0], escaped = null, quoted = null, commented = null, (clThis = (through = function (c) {
        switch (c) {
        case ' ':
        case '\t':
            clThis = through;
            break;
        case '\n':
            if ('\n' == commented) {
                commented = null;
            };
            if (!(1 === tokenCount[0] || 0 === result.length)) {
                collect('\n');
                lineLength = 0;
                clThis = through;
            };
            break;
        case '(':
            collect('(', 'count', insertSpacesForToken() + 1);
            indentPosition.unshift([2, lineLength]);
            tokenCount.unshift(1);
            clThis = through;
            break;
        case ')':
            collect(')');
            if (1 !== tokenCount.length) {
                tokenCount.shift();
            };
            if (1 !== indentPosition.length) {
                indentPosition.shift();
            };
            countToken();
            clThis = through;
            break;
        case '\'':
        case '`':
            quoted = c;
            break;
        case '"':
            collect('"', 'count', insertSpacesForToken() + 1);
            ++tokenCount[0];
            clThis = throughString;
            break;
        case ';':
            commented = '\n';
            collect(';', 'count', insertSpacesForToken() + 1);
            clThis = throughSingleLineComment;
            break;
        default:
            throughToken(c);
            clThis = throughToken;
        };
        return c;
    }, collect = function (c) {
        var _js5 = arguments.length;
        for (var n4 = 1; n4 < _js5; n4 += 2) {
            switch (arguments[n4]) {
            case 'count':
                count = arguments[n4 + 1];
            };
        };
        var count = 'undefined' === typeof count ? 1 : count;
        lineLength += count;
        result += c;
        return c;
    }, specialIndent = function (nb) {
        return nb * indentSize + (indentPosition[0][1] - 1);
    }, countToken = function (token) {
        var formBeginning;
        if (1 === tokenCount[0]) {
            if (token) {
                if (':' === token[0]) {
                    indentPosition[0] = (formBeginning = indentPosition[0][1], [[formBeginning, formBeginning]]);
                } else if (-1 !== ['defun', 'with-open-file', 'let', 'progn', 'lambda', 'defmacro'].indexOf(token)) {
                    indentPosition[0] = [[specialIndent(1), specialIndent(1)]];
                } else if (-1 !== ['unwind-protect'].indexOf(token)) {
                    indentPosition[0] = [[specialIndent(2), specialIndent(1)]];
                } else if (-1 !== ['multiple-value-bind', 'register-groups-bind'].indexOf(token)) {
                    indentPosition[0] = [[specialIndent(2), specialIndent(2), specialIndent(1)]];
                } else {
                    indentPosition[0][0] = lineLength + 1;
                };
            } else {
                indentPosition[0][0] = lineLength + 1;
            };
        };
        return 1 !== tokenCount.length ? ++tokenCount[0] : null;
    }, getSpaceOfIndentation = function () {
        var it;
        if (0 !== lineLength) {
            return 1;
        } else {
            if (1 === indentPosition[0].length) {
                if (Object.prototype.toString.apply(indentPosition[0][0]) === '[object Array]') {
                    var val7 = indentPosition[0][0][0];
                    if (!commented) {
                        indentPosition[0][0] = (it = indentPosition[0][0].slice(1), it != null ? (1 === it.length ? it[0] : it) : indentPosition[0][0][0]);
                    };
                    return val7;
                } else {
                    return indentPosition[0][0];
                };
            } else {
                var it8 = 2 === tokenCount[0] ? indentPosition[0][1] : indentPosition[0][0];
                indentPosition[0] = [it8];
                return it8;
            };
        };
    }, insertSpacesForToken = function () {
        return insertNSpaces(0 === result.length || 1 === tokenCount[0] ? 0 : getSpaceOfIndentation()) + (quoted ? (result += quoted, quoted = null, 1) : 0);
    }, throughToken = function (c) {
        if (-1 !== [' ', ')', '\n', '\t'].indexOf(c)) {
            lineLength += insertSpacesForToken() + tokenCharacters.length;
            countToken(emptyTokenCharacters());
            through(c);
        } else {
            tokenCharacters.unshift(c);
        };
        return c;
    }, throughString = function (c) {
        switch (c) {
        case '\\':
            escaped = !escaped;
            break;
        case '"':
            if (escaped) {
                escaped = null;
            } else {
                clThis = through;
            };
            break;
        default:
            escaped = null;
        };
        ++lineLength;
        return result += c;
    }, throughSingleLineComment = function (c) {
        switch (c) {
        case '\n':
            return through(c);
        default:
            ++lineLength;
            return result += c;
        };
    }, through), function () {
        var params = [];
        for (var i9 = 0; i9 < arguments.length - 0; i9 += 1) {
            params[i9] = arguments[i9 + 0];
        };
        return clThis.apply(this, params);
    }));
    if (str) {
        var _js11 = str.length;
        for (var _js10 = 0; _js10 < _js11; _js10 += 1) {
            var c = str[_js10];
            fsm(c);
        };
    };
    return result;
};