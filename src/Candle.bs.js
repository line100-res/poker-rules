// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Nom = require("@line100/rescript-nom/src/Nom.bs.js");
var Seq = require("@line100/rescript-seq/src/Seq.bs.js");
var Curry = require("rescript/lib/js/curry.js");
var Belt_List = require("rescript/lib/js/belt_List.js");
var Caml_exceptions = require("rescript/lib/js/caml_exceptions.js");

var Err = /* @__PURE__ */Caml_exceptions.create("Candle.Err");

function mergeCandle(a, c) {
  return {
          o: a.o,
          c: c.c,
          h: Math.max(a.h, c.h),
          l: Math.min(a.l, c.l),
          v: a.v + c.v,
          t: c.t
        };
}

function toOpenSeq(s) {
  return Seq.map(s, (function (k) {
                return [
                        k.o,
                        k.t
                      ];
              }));
}

function toCloseSeq(s) {
  return Seq.map(s, (function (k) {
                return [
                        k.c,
                        k.t
                      ];
              }));
}

function toHighSeq(s) {
  return Seq.map(s, (function (k) {
                return [
                        k.h,
                        k.t
                      ];
              }));
}

function toLowSeq(s) {
  return Seq.map(s, (function (k) {
                return [
                        k.l,
                        k.t
                      ];
              }));
}

function getOpen(param, param$1) {
  return Nom.mapValue((function (param, param$1) {
                return Nom.identity(/* ReachTailErr */0, param, param$1);
              }), (function (k) {
                return [
                        k.o,
                        k.t
                      ];
              }), param, param$1);
}

function getClose(param, param$1) {
  return Nom.mapValue((function (param, param$1) {
                return Nom.identity(/* ReachTailErr */0, param, param$1);
              }), (function (k) {
                return [
                        k.c,
                        k.t
                      ];
              }), param, param$1);
}

function getHigh(param, param$1) {
  return Nom.mapValue((function (param, param$1) {
                return Nom.identity(/* ReachTailErr */0, param, param$1);
              }), (function (k) {
                return [
                        k.h,
                        k.t
                      ];
              }), param, param$1);
}

function getLow(param, param$1) {
  return Nom.mapValue((function (param, param$1) {
                return Nom.identity(/* ReachTailErr */0, param, param$1);
              }), (function (k) {
                return [
                        k.l,
                        k.t
                      ];
              }), param, param$1);
}

function reducListe(n, f, input, i) {
  if (!input) {
    return {
            TAG: /* Fail */1,
            _0: input,
            _1: /* ReachTailErr */0,
            _2: i
          };
  }
  var match = Seq.takeListCount(input, n);
  var count = match[0];
  if (count !== 0) {
    var lst = match[1];
    if (count < n) {
      return {
              TAG: /* Fail */1,
              _0: input,
              _1: {
                TAG: /* MergeCandleInsufficientFailed */0,
                _0: Curry._1(f, lst)
              },
              _2: i
            };
    } else {
      return {
              TAG: /* Pass */0,
              _0: Seq.drop(input, count),
              _1: Curry._1(f, lst),
              _2: i + count | 0
            };
    }
  }
  throw {
        RE_EXN_ID: Err,
        _1: "unreachable",
        Error: new Error()
      };
}

function scanList(n, f, input, i) {
  if (!input) {
    return {
            TAG: /* Fail */1,
            _0: input,
            _1: /* ReachTailErr */0,
            _2: i
          };
  }
  var match = Seq.takeListCount(input, n);
  var count = match[0];
  if (count !== 0) {
    var lst = match[1];
    if (count < n) {
      return {
              TAG: /* Fail */1,
              _0: input,
              _1: {
                TAG: /* ScanCandleInsufficientFailed */1,
                _0: Curry._1(f, lst)
              },
              _2: i
            };
    } else {
      return {
              TAG: /* Pass */0,
              _0: Seq.drop(input, 1),
              _1: Curry._1(f, lst),
              _2: i + 1 | 0
            };
    }
  }
  throw {
        RE_EXN_ID: Err,
        _1: "unreachable",
        Error: new Error()
      };
}

function merge(n) {
  return function (param, param$1) {
    return reducListe(n, (function (lst) {
                  if (lst) {
                    return Belt_List.reduce(lst.tl, lst.hd, mergeCandle);
                  }
                  throw {
                        RE_EXN_ID: Err,
                        _1: "unreachable",
                        Error: new Error()
                      };
                }), param, param$1);
  };
}

function sma(n, getValue) {
  if (n < 2) {
    throw {
          RE_EXN_ID: Err,
          _1: "ma(n, v) must be bigger than 1",
          Error: new Error()
        };
  }
  return function (param, param$1) {
    return scanList(n, (function (lst) {
                  var sum = function (_l, _acc, _count) {
                    while(true) {
                      var count = _count;
                      var acc = _acc;
                      var l = _l;
                      if (!l) {
                        return [
                                acc,
                                count
                              ];
                      }
                      var first = l.hd;
                      _count = count + 1 | 0;
                      _acc = [
                        acc[0] + Curry._1(getValue, first),
                        first.t
                      ];
                      _l = l.tl;
                      continue ;
                    };
                  };
                  var match = sum(lst, [
                        0.0,
                        0
                      ], 0);
                  var match$1 = match[0];
                  return [
                          match$1[0] / match$1[1],
                          match[1]
                        ];
                }), param, param$1);
  };
}

exports.Err = Err;
exports.mergeCandle = mergeCandle;
exports.toOpenSeq = toOpenSeq;
exports.toCloseSeq = toCloseSeq;
exports.toHighSeq = toHighSeq;
exports.toLowSeq = toLowSeq;
exports.getOpen = getOpen;
exports.getClose = getClose;
exports.getHigh = getHigh;
exports.getLow = getLow;
exports.reducListe = reducListe;
exports.scanList = scanList;
exports.merge = merge;
exports.sma = sma;
/* No side effect */
