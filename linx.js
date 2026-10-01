const keywords = "namespace importall export import def set fun if elif else end ret retv swap for while continue break continuen breakn len typeof nan inf nil true false".split(" ");
const ops = "~+-*/%^=,;()><![]&|:@?$".split("");

function lex(x) {
  let ts = [], i = 0, l = x.length;
  while(i<l) {
    const s = x.charCodeAt(i);
    if(s===10 || s===9 || s===32) {
      i++;
      continue;
    }
    if(s===34) {
      let j = ++i, s = "";
      while(i<l) {
        const k = x.charCodeAt(i);
        if(k===34 || k===10) break;
        if(k==92) {
          s += x.slice(j, i);
          const f = x[++i];
          if(f==="\\" || f==='"') s += f, ++i;
          if(f==="n") s += "\n", ++i;
          else throw "invalid escape";
          j = i;
          continue;
        }
        ++i;
      }
      s += x.slice(j, i);
      if(x.charCodeAt(i++)!==34)
        throw "unclosed string";
      ts.push(["<str>", s]);
      continue;
    }
    if(s===35) {
      ++i;
      if(i<l && x.charCodeAt(i)===42) {
        while(i<l && !(x[i]==="*" && x[i+1]==="#")) ++i;
        if(!(x[i]==="*" && x[i+1]==="#"))
          throw "unclosed comment";
        i += 2;
        continue;
      }
      while(i<l && x.charCodeAt(i)!==10) ++i;
      ++i;
      continue;
    }
    if((s>=97 && s<=122) || (s>=65 && s<=90) || s===95) {
      let j = i++;
      while(i<l) {
        const s = x.charCodeAt(i);
        if((s<97 || s>122) && (s<65 || s>90) && s!==95 && (s<48 || s>57)) break;
        ++i;
      }
      j = x.slice(j, i);
      if(!keywords.includes(j))
        ts.push(["<id>", j]);
      else ts.push([j]);
      continue;
    }
    if(s>=48 && s<=57) {
      let j = i++;
      while(i<l) {
        const s = x.charCodeAt(i);
        if(s<48 || s>57) break;
        ++i;
      }
      
      if(x.charCodeAt(i)===46) {
        ++i;
        while(i<l) {
          const s = x.charCodeAt(i);
          if(s<48 || s>57) break;
          ++i;
        }
      }
      
      ts.push(["<num>", parseFloat(x.slice(j, i))]);
      continue;
    }
    
    if(ops.includes(x[i])) {
      ts.push([x[i++]]);
      continue;
    }
    throw "unexpected symbol "+x[i];
  }
  return ts;
}

const node_kind = Object.create(null);
("namespace import export program for while break continue swap smem sidx if else fun ret set def literal id binop unop call idx ternary nullish typeof len array mem".split(" ").forEach((x, i) => (node_kind[x] = i)));

const valid_bop = "+-*/%^".split("");
const valid_cop = "~=&|><".split("");

function parse(x) {
  function expect(type) {
    const s = x[i++];
    if(!s)
      throw "expected "+type+" near <eof>";
    const y = s[1] ?? s[0];
    if(s[0]!==type)
      throw "expected "+type+" near "+y;
    return y;
  }
  
  function p_primary() {
    const [s0, s1] = x[i++];
    if(!s0) throw "unexpected eof";
    
    if(s0==="<num>")
      return { kind: node_kind.literal, val: s1 };
    if(s0==="inf")
      return { kind: node_kind.literal, val: Infinity };
    if(s0==="nan")
      return { kind: node_kind.literal, val: NaN };
    if(s0==="true" || s0==="false")
      return { kind: node_kind.literal, val: s0==="true" };
    if(s0==="<str>")
      return { kind: node_kind.literal, val: s1 };
    if(s0==="nil")
      return { kind: node_kind.literal, val: null };
    
    if(s0==="<id>")
      return { kind: node_kind.id, name: s1 };
    
    if(s0==="(") {
      const x = p_expr();
      expect(")");
      return x;
    }
    if(s0==="[") {
      const items = [];
      while(i<l) {
        if(x[i][0]==="]") break;
        items.push(p_expr());
        if(i<l && x[i][0]!=="]") expect(",");
      }
      expect("]");
      return { kind: node_kind.array, items };
    }
    if(s0==="!" || s0==="-") {
      const val = p_idxcall();
      return { kind: node_kind.unop, val, op: s0 };
    }
    if(s0==="len" || s0==="typeof") {
      const val = p_idxcall();
      return { kind: node_kind[s0], val };
    }
    throw "expected expression";
  }
  function p_idxcall() {
    let left = p_primary();
    while(i<l) {
      const y = x[i++][0];
      if(y==="(") {
        const args = [];
        while(i<l) {
          if(x[i][0]===")") break;
          args.push(p_expr());
          if(i<l && x[i][0]!==")") expect(",");
        }
        expect(")");
        left = { kind: node_kind.call, callee: left, args };
        continue;
      }
      if(y==="[") {
        const idx = p_expr();
        expect("]");
        left = { kind: node_kind.idx, arr: left, idx };
        continue;
      }
      if(y===":") {
        const prop = expect("<id>");
        left = { kind: node_kind.mem, obj: left, prop };
        continue;
      }
      --i;
      break;
    }
    return left;
  }
  function p_expr_1() {
    let left = p_idxcall();
    while(i<l) {
      const op = x[i][0];
      if(valid_bop.includes(op)) {
        ++i;
        const right = p_idxcall();
        left = { kind: node_kind.binop, left, op, right };
        continue;
      }
      break;
    }
    return left;
  }
  function p_expr_0() {
    let left = p_expr_1();
    while(i<l) {
      const op = x[i][0];
      if(valid_cop.includes(op)) {
        ++i;
        const right = p_expr_1();
        left = { kind: node_kind.binop, left, op, right };
        continue;
      }
      break;
    }
    return left;
  }
  function p_expr() {
    let left = p_expr_0();
    const s = x[i++]?.[0];
    if(s==="?") {
      const truth = p_expr_0();
      expect(";");
      const falseth = p_expr_0();
      return { kind: node_kind.ternary, cond: left, truth, falseth };
    }
    if(s==="$") {
      const right = p_expr_0();
      return { kind: node_kind.nullish, left, right };
    }
    --i;
    return left;
  }
  function p_if() {
    const cond = p_expr();
    const stmts = [];

    const simple = x[i]?.[0]===";";
    if(simple) {
      ++i;
      stmts.push(p_stmt());
    } else {
      while(i<l) {
        const j = x[i][0];
        if(j==="end" || j==="else" || j==="elif") break;
        stmts.push(p_stmt());
      }
    }
    
    const b = { kind: node_kind.if, cond, stmts, alt: null };
    const y = x[i]?.[0];
    if(y==="elif") {
      ++i;
      b.alt = p_if();
    } else if(y==="else") {
      ++i;
      const stmts = [];
      while(i<l) {
        if(x[i][0]==="end") break;
        stmts.push(p_stmt());
      }
      b.alt = { kind: node_kind.else, stmts };
      expect("end");
    } else if(!simple) expect("end");
    return b;
  }
  function p_stmt() {
    const s = x[i++][0];
    if(s==="@") {
      const name = expect("<id>");
      const stmt = p_stmt();
      const kind = stmt.kind;
      if(kind!==node_kind.for && kind!==node_kind.while)
        throw "only loops can have labels";
      stmt.label = name;
      return stmt;
    }
    if(s==="break" || s==="continue") {
      return { kind: node_kind[s], label: null };
    }
    if(s==="breakn" || s==="continuen") {
      const label = expect("<id>");
      return { kind: s==="breakn" ? node_kind.break : node_kind.continue, label };
    }
    if(s==="ret" || s==="retv") {
      let val = null;
      if(s==="retv") val = p_expr();
      return { kind: node_kind.ret, val };
    }
    if(s==="if") return p_if();
    if(s==="while") {
      const cond = p_expr();
      const stmts = [];
      while(i<l) {
        if(x[i][0]==="end") break;
        stmts.push(p_stmt());
      }
      expect("end");
      return { kind: node_kind.while, cond, stmts, label: null };
    }
    if(s==="for") {
      const counter = expect("<id>");
      const iota = p_expr();
      expect(",");
      const cond = p_expr();
      expect(",");
      const step = p_expr();
      
      const stmts = [];
      while(i<l) {
        if(x[i][0]==="end") break;
        stmts.push(p_stmt());
      }
      expect("end");
      return {
        kind: node_kind.for,
        counter, cond, step, iota,
        stmts, label: null
      };
    }
    
    if(s==="set") {
      const name = p_idxcall();
      const val = p_expr();
      if(name.kind===node_kind.idx) {
        return { kind: node_kind.sidx, arr: name.arr, idx: name.idx, val };
      }
      if(name.kind===node_kind.mem) {
        return { kind: node_kind.smem, obj: name.obj, prop: name.prop, val };
      }
      if(name.kind===node_kind.id) {
        return { kind: node_kind.set, name: name.name, val };
      }
      throw "invalid left operand for set";
    }
    if(s==="swap") {
      const left = expect("<id>");
      const right = expect("<id>");
      return { kind: node_kind.swap, left, right };
    }
    if(s==="def") {
      const inits = [];
      while(true) {
        const name = expect("<id>");
        const val = p_expr();
        inits.push({ name, val });
        if(x[i]?.[0]===",") {
          ++i;
          continue;
        }
        break;
      }
      return { kind: node_kind.def, inits };
    }
    
    --i;
    const e = p_idxcall();
    if(e.kind===node_kind.call) return e;
    throw "expected statement";
  }

  function p_progstmt() {
    const s = x[i++][0];
    if(s==="def") {
      --i;
      return p_stmt();
    }
    if(s==="namespace") {
      const name = expect("<id>");
      const stmts = [];
      while(i<l) {
        const s = x[i][0];
        if(s==="fun") {
          stmts.push(p_progstmt());
          continue;
        }
        if(s==="def") {
          stmts.push(p_stmt());
          continue;
        }
        break;
      }
      expect("end");
      return { kind: node_kind.namespace, name, stmts };
    }
    if(s==="importall") {
      const name = expect("<id>");
      const path = expect("<str>");
      return { kind: node_kind.import, all: true, name, path };
    }
    if(s==="import") {
      const names = [];
      while(true) {
        names.push(expect("<id>"));
        if(x[i]?.[0]===",") {
          ++i;
          continue;
        }
        break;
      }
      const path = expect("<str>");
      return { kind: node_kind.import, all: false, names, path };
    }
    if(s==="export") {
      const stmt = p_progstmt();
      if(stmt.kind!==node_kind.fun && stmt.kind!==node_kind.namespace)
        throw "invalid export";
      return { kind: node_kind.export, stmt, name: stmt.name };
    }
    if(s==="fun") {
      const name = expect("<id>");
      const params = [];
      while(i<l && x[i][0]===",") {
        ++i;
        params.push(expect("<id>"));
      }
      const stmts = [];
      while(i<l) {
        if(x[i][0]==="end") break;
        stmts.push(p_stmt());
      }
      expect("end");
      return { kind: node_kind.fun, name, params, stmts };
    }
    throw "expected statement";
  }
  
  x = lex(x);
  let i = 0, l = x.length;
  const stmts = [];
  while(i<l) {
    stmts.push(p_progstmt());
  }
  analyze(stmts);
  return { kind: node_kind.program, stmts };
}

function analyze_def(node) {
  switch(node.kind) {
    case node_kind.call:
    case node_kind.idx:
    case node_kind.mem:
      linxerr("invalid expression at top-level");
    case node_kind.binop:
      analyze_def(node.left);
      analyze_def(node.right);
      break;
    case node_kind.array:
      for(const s of node.items)
        analyze_def(s);
      break;
    case node_kind.unop:
    case node_kind.len:
    case node_kind.typeof:
      analyze_def(node.val);
      break;
  }
}

function analyze_fun(node, loop) {
  switch(node.kind) {
    case node_kind.break:
    case node_kind.continue: {
      const name = node.kind===node_kind.break ? "break" : "continue";
      if(!loop) linxerr("illegal "+name+" statement outside loop");
      break;
    }
    case node_kind.if:
    case node_kind.while:
    case node_kind.for: {
      const iloop = node.kind===node_kind.if ? loop : true;
      for(const s of node.stmts)
        analyze_fun(s, iloop);
      break;
    }
  }
}

function analyze(stmts) {
  for(const s of stmts) {
    if(s.kind===node_kind.namespace) {
      analyze(s.stmts);
    } else if(s.kind===node_kind.def) {
      for(const { val } of s.inits)
        analyze_def(val);
    } else if(s.kind===node_kind.fun) {
      for(const node of s.stmts)
        analyze_fun(node, false);
    }
  }
}

class Fun {
  constructor(params, stmts, env) {
    this.params = params;
    this.stmts = stmts;
    this.env = env;
  }
}

class Env {
  constructor(parent = null) {
    this.parent = parent;
    this.syms = Object.create(null);
    this.root = false;
  }
  resolve(name) {
    if(name in this.syms) return this;
    if(this.parent) return this.parent.resolve(name);
    throw "unknown name: "+name;
  }
  get(name) {
    const env = this.resolve(name);
    return env.syms[name];
  }
  set(name, value) {
    const env = this.resolve(name);
    return env.syms[name] = value;
  }
  define(name, value) {
    this.syms[name] = value;
  }
}

class RetSign {
  constructor(val) {
    this.val = val;
  }
}

class LoopSign {
  constructor(mode, label) {
    this.mode = mode;
    this.label = label;
  }
}

const g_g = new Env();

function _print(x, u = new Set()) {
  if(typeof x==="object") {
    if(x instanceof Map) return "[object]";
    if(u.has(x)) return "";
    u.add(x);
  }
  
  const tx = typeof x;
  if(tx==="number") {
    if(Number.isNaN(x))
      return "nan";
    if(x===Infinity)
      return "inf";
    if(x===-Infinity)
      return "-inf";
    return String(x);
  }
  if(x===null) return "nil";
  if(tx==="boolean")
    return String(x);
  
  if(tx==="string") return x;
  
  if(x instanceof Fun)
    return "[function]";
  
  if(x instanceof Array) {
    let j = "";
    for(let i = 0, l = x.length; i<l;) {
      const k = x[i++];
      if(k===undefined) j += "null";
      else j += _print(k, u);
      if(i<l) j += ",";
    }
    return j;
  }

  return "[object]";
}

g_g.define("math", new Map([
  ["floor", Math.floor],
  ["sqrt", Math.sqrt],
  ["ceil", Math.ceil],
  ["abs", Math.abs],
  ["random", Math.random]
]));
g_g.define("os", new Map([
  ["print", (x) => {
    process.stdout.write(_print(x));
  }],
  ["println", (x) => {
    process.stdout.write(_print(x)+"\n");
  }],
  ["input", () => {
    const buf = Buffer.alloc(4096);
    const n = fs.readSync(0, buf, 0, 4096, null);
    return buf.subarray(0, n).toString("utf8");
  }]
]));
g_g.define("string", new Map([
  ["to", (x) => _print(x)],
  ["char", (x) => {
    if(typeof x!=="number")
      throw "can't convert non-number to character";
    if(x<0 || !Number.isInteger(x))
      throw "not a valid codepoint";
    return String.fromCharCode(x);
  }],
]));
g_g.define("number", new Map([
  ["to", (x) => Number(x)],
  ["is_nan", (x) => Number.isNaN(x)],
  ["is_finite", (x) => Number.isFinite(x)],
]));

const cache_import = Object.create(null);

function remlastdash(x) {
  const i = x.lastIndexOf("/");
  if(i>-1) return x.slice(0, i);
  return x;
}

function evaluate(x, e) {
  if(!x) throw "bad generated code";
  switch(x.kind) {
    case node_kind.namespace: {
      const e2 = new Env(e);
      
      const o = new Map();
      for(const s of x.stmts) {
        if(s.kind===node_kind.def) {
          for(const { name, val } of s.inits) {
            o.set(name, evaluate(val, e2));
          }
        } else if(s.kind===node_kind.fun) {
          o.set(s.name, new Fun(s.params, s.stmts, e2));
        }
      }
      e.define(x.name, o);
      return o;
    }
    case node_kind.import: {
      if(!e.root)
        throw "can only import in top level";
      
      if(x.path.length<2 || !x.path.startsWith("./"))
        throw "invalid path";
      
      const fpath = path.resolve(remlastdash(e.fpath)+"/"+x.path.slice(2));
      
      let es = cache_import[fpath];
      if(es===-1)
        throw "circular dependecy not allowed";
      if(!es) {
        cache_import[fpath] = -1;
        const g = run(read_file(fpath), fpath);
        es = new Map(g.exports);
        cache_import[fpath] = es;
      }
      
      if(x.all) {
        e.define(x.name, es);
        return null;
      } else {
        for(const name of x.names)
          e.define(name, es.get(name) ?? null);
        return null;
      }
      return null;
    }
    case node_kind.export: {
      if(!e.root)
        throw "can only export in top level";
      
      if(e.exports.findIndex((y) => y[0]===x.name)!==-1)
        throw "duplicated export name";
      
      const stmt = evaluate(x.stmt, e);
      
      e.exports.push([x.name, stmt]);
      return null;
    }
    
    case node_kind.literal:
      return x.val;
    case node_kind.id:
      return e.get(x.name);
    case node_kind.idx: {
      const arr = evaluate(x.arr, e);
      if(!(arr instanceof Array))
        throw "can't index non-array";
      const idx = evaluate(x.idx, e);
      if(typeof idx!=="number")
        throw "cant index with non-number";
      if(idx<0)
        throw "index out of bound";
      return arr[idx] ?? null;
    }
    case node_kind.mem: {
      const obj = evaluate(x.obj, e);
      if(!(obj instanceof Map))
        throw "can't get property of non-object";
      return obj.get(x.prop) ?? null;
    }
    case node_kind.len: {
      const val = evaluate(x.val, e);
      if(typeof val!=="string" && !(val instanceof Array))
        throw "can't get length of non-array";
      return val.length;
    }
    case node_kind.typeof: {
      const val = evaluate(x.val, e);
      if(val instanceof Map) return "object";
      if(val instanceof Fun) return "function";
      if(val instanceof Array) return "array";
      return typeof val;
    }
    case node_kind.array: {
      const c = [];
      for(let i = 0; i<x.items.length; ++i) {
        const v = evaluate(x.items[i], e);
        c[i] = v;
      }
      return c;
    }
    case node_kind.unop: {
      const v = evaluate(x.val, e);
      if(x.op==="-") {
        if(typeof v!=="number")
          throw "can't operate with non-number";
        return -v;
      }
      return v ? false : true;
    }
    case node_kind.binop: {
      const l = evaluate(x.left, e);
      if(x.op==="&") {
        if(l) return evaluate(x.right, e);
        return l;
      }
      if(x.op==="|") {
        if(l) return l;
        return evaluate(x.right, e);
      }
      const r = evaluate(x.right, e);
      if(x.op==="~") return _print(l)+_print(r);
      if(typeof l!=="number" || typeof r!=="number") {
        if(x.op==="=") return l===r;
        throw "can't operate with non-number";
      }
      switch(x.op) {
        case "+": return l+r;
        case "-": return l-r;
        case "*": return l*r;
        case "/": return l/r;
        case "%": return l%r;
        case "^": return l**r;
        case "=": return l===r;
        case ">": return l>r;
        case "<": return l<r;
      }
      return null;
    }
    case node_kind.ternary: {
      if(evaluate(x.cond, e)) return evaluate(x.truth, e);
      return evaluate(x.falseth, e);
    }
    case node_kind.nullish: {
      const l = evaluate(x.left, e);
      if(l===null) return evaluate(x.right, e);
      return l;
    }

    case node_kind.program: {
      for(const s of x.stmts)
        evaluate(s, e);
      return null;
    }
    case node_kind.fun: {
      const fun = new Fun(x.params, x.stmts, e);
      e.define(x.name, fun);
      return fun;
    }
    case node_kind.if: {
      const cond = evaluate(x.cond, e);
      if(cond) {
        const e2 = new Env(e);
        for(const s of x.stmts)
          evaluate(s, e2);
      } else if(x.alt) {
        const e2 = new Env(e);
        if(x.alt.kind===node_kind.else) {
          for(const s of x.alt.stmts)
            evaluate(s, e2);
        } else {
          evaluate(x.alt, e2);
        }
      }
      return null;
    }
    case node_kind.ret: {
      throw new RetSign(x.val && evaluate(x.val, e));
    }
    case node_kind.set: {
      e.set(x.name, evaluate(x.val, e));
      return null;
    }
    case node_kind.swap: {
      const left = e.get(x.left);
      const right = e.get(x.right);
      e.set(x.left, right);
      e.set(x.right, left);
      return null;
    }
    case node_kind.def: {
      for(const { name, val } of x.inits)
        e.define(name, evaluate(val, e));
      return null;
    }
    case node_kind.sidx: {
      const arr = evaluate(x.arr, e);
      if(!(arr instanceof Array))
        throw "can't set index of non-array";
      
      const idx = evaluate(x.idx, e);
      if(typeof idx!=="number")
        throw "cant set index of array with non-number";
      
      if(idx<0)
        throw "index out of bound";
      
      arr[idx] = evaluate(x.val, e);
      return null;
    }
    case node_kind.smem: {
      const obj = evaluate(x.obj, e);
      if(!(obj instanceof Map))
        throw "can't set property of non-object";
      
      obj.set(x.prop, evaluate(x.val, e));
      return null;
    }
    case node_kind.for: {
      const step = evaluate(x.step, e);
      if(typeof step!=="number")
        throw "can't step with non-number";
      
      const iota = evaluate(x.iota, e);
      if(typeof iota!=="number")
        throw "can't start count on a non-number";
      
      for(let i = iota; true; i += step) {
        try {
          const e2 = new Env(e);
          e2.define(x.counter, i);
          
          if(!evaluate(x.cond, e2)) return null;
          
          for(const s of x.stmts)
            evaluate(s, e2);
        } catch(e) {
          if((e instanceof LoopSign) && e.label===x.label) {
            if(e.mode===0) return null;
            continue;
          }
          throw e;
        }
      }
      return null;
    }
    case node_kind.while: {
      while(evaluate(x.cond, e)) {
        try {
          const e2 = new Env(e);
          for(const s of x.stmts)
            evaluate(s, e2);
        } catch(e) {
          if((e instanceof LoopSign) && e.label===x.label) {
            if(e.mode===0) return null;
            continue;
          }
          throw e;
        }
      }
      return null;
    }
    case node_kind.break:
    case node_kind.continue:
      throw new LoopSign(x.kind===node_kind.break ? 0 : 1, x.label);
    case node_kind.call: {
      const func = evaluate(x.callee, e);
      const args = [];
      for(let i = 0; i<x.args.length; ++i) {
        args[i] = evaluate(x.args[i], e);
      }
      return call_func(func, args, e);
    }
  }
  throw "can't evaluate "+x.kind;
}

function call_func(func, args) {
  if(typeof func==="function") {
    if(func.length!==args.length)
      throw "incorrect number of arguments";
    return func.apply(null, args) ?? null;
  }
  
  if(!(func instanceof Fun))
    throw "can't call non-function";
  
  if(func.params.length!==args.length)
    throw "incorrect number of arguments";
  
  const e2 = new Env(func.env);
  for(let i = 0; i<args.length; ++i) {
    e2.define(func.params[i], args[i]);
  }
  
  try {
    for(const s of func.stmts)
      evaluate(s, e2);
  } catch(e) {
    if(e instanceof RetSign)
      return e.val;
    throw e;
  }
  return null;
}

function run(x, fpath, is_main = false, args = null) {
  const e = new Env(g_g);
  e.root = true;
  e.exports = [];
  e.fpath = fpath;
  try {
    evaluate(parse(x), e);
    if(is_main) {
      if(!("main" in e.syms))
        linxerr("'main' needs to be defined");
      
      const mf = e.syms.main;
      if(!(mf instanceof Fun))
        linxerr("'main' needs to be a function");

      if(mf.params.length===0)
        call_func(mf, []);
      else call_func(mf, [args]);
    }
    return e;
  } catch(e) {
    linxerr(fpath+" @ "+String(e));
  }
}

function linxerr(x) {
  fs.writeSync(2, x+"\n");
  process.exit(1);
}

import fs from "node:fs";
import path from "node:path";

function read_file(path) {
  if(!fs.existsSync(path, { isFile: true })) {
    linxerr("path don't exist or isn't a file");
  }
  
  let code;
  try {
    code = fs.readFileSync(path, "utf8");
  } catch(e) {
    linxerr("can't access file");
  }
  return code;
}

let fpath = process.argv[2];
if(fpath==="-v") {
  fs.writeSync(1, "linx v0.0.2-alpha\n");
  process.exit(0);
}
let i = 2;
if(fpath==="--") fpath = process.argv[++i];
if(!fpath) {
  linxerr("expected file path");
}
fpath = path.resolve(fpath);

run(read_file(fpath), fpath, true, process.argv.slice(i+1));
process.exit(0);