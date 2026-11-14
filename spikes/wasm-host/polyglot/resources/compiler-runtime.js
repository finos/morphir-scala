// Runtime compatibility for the upstream compiler guest, executed inside GraalJS.
// All file access crosses the explicitly bounded Scala bridge; no Node process runs.
globalThis.__compilerExit = 0;
let virtualCwd = "/workspace";
const normalize = value => {
  const parts = (value.startsWith("/") ? value : virtualCwd + "/" + value).split("/");
  const result = [];
  for (const part of parts) {
    if (part === "..") result.pop();
    else if (part && part !== ".") result.push(part);
  }
  return "/" + result.join("/");
};
const stat = path => ({
  size: __bridge.pathSize(normalize(path)), dev: 0, ino: 0, mode: 33188, nlink: 1,
  uid: 0, gid: 0, rdev: 0, atimeMs: 0, mtimeMs: 0, ctimeMs: 0,
  isFile: () => !__bridge.isDir(normalize(path)), isDirectory: () => __bridge.isDir(normalize(path)),
  isCharacterDevice: () => false, isBlockDevice: () => false, isSymbolicLink: () => false,
  isFIFO: () => false, isSocket: () => false
});
const fs = {
  constants: {O_RDONLY: 0, O_WRONLY: 1, O_RDWR: 2, O_APPEND: 1024, O_CREAT: 64,
    O_TRUNC: 512, O_EXCL: 128, O_NONBLOCK: 2048, O_NOCTTY: 256, O_DSYNC: 4096, O_SYNC: 1052672},
  openSync: (path, flags) => __bridge.open(normalize(path), flags),
  closeSync: fd => __bridge.close(fd),
  readSync: (fd, bytes, offset, length, position) => __bridge.read(fd, bytes, offset, length, position),
  writeSync: (fd, bytes, offset, length, position) => {
    if (typeof bytes === "string") { bytes = new TextEncoder().encode(bytes); offset = 0; length = bytes.length; position = null; }
    return __bridge.write(fd, bytes, offset, length, position);
  },
  statSync: path => stat(path), lstatSync: path => stat(path),
  fstatSync: fd => ({size: BigInt(__bridge.size(fd))}),
  existsSync: path => __bridge.exists(normalize(path)),
  readdirSync: path => Array.from(__bridge.list(normalize(path))),
  mkdirSync: path => __bridge.mkdir(normalize(path)),
  rmdirSync: path => __bridge.remove(normalize(path)),
  unlinkSync: path => __bridge.remove(normalize(path)),
  renameSync: (a, b) => __bridge.rename(normalize(a), normalize(b))
};
globalThis.process = {
  versions: {node: "bounded-polyglot-shim"}, platform: "darwin", env: {},
  argv: ["polyglot", "/toolchain/moonc.js", ...Array.from(__arguments)],
  cwd: () => virtualCwd,
  chdir: path => { const target = normalize(path); if (!__bridge.isDir(target)) throw Error("not a directory"); virtualCwd = target; },
  on: () => {},
  exit: code => { __compilerExit = code; throw Error("__COMPILER_EXIT__" + code); }
};
globalThis.require = name => {
  switch (name) {
    case "node:fs": return fs;
    case "node:fs/promises": return {readFile: async path => new Uint8Array(__bridge.readAll(normalize(path)))};
    case "node:path": return {join: (...parts) => normalize(parts.join("/")), dirname: path => path.slice(0, path.lastIndexOf("/")) || "/"};
    case "node:tty": return {isatty: () => false};
    default: throw Error("Node module denied: " + name);
  }
};
require.main = {filename: "/toolchain/moonc.js"};

// Optional adapter for repeated import names with distinct function signatures.
// Only import names change; function/type indices and all code/data sections are preserved.
globalThis.__importAliases = [];
if (__uniqueImports) {
  const instantiate = WebAssembly.instantiate.bind(WebAssembly);
  const encode = new TextEncoder();
  const decode = new TextDecoder();
  const leb = value => { const bytes = []; do { const next = value & 127; value >>>= 7; bytes.push(next | (value ? 128 : 0)); } while (value); return bytes; };
  const join = parts => { const output = new Uint8Array(parts.reduce((n,p)=>n+p.length,0)); let offset=0; for(const part of parts){output.set(part,offset);offset+=part.length;} return output; };
  const nameBytes = name => { const bytes = encode.encode(name); return join([leb(bytes.length),bytes]); };
  WebAssembly.instantiate = (input, imports = {}, options) => {
    if (!(input instanceof Uint8Array)) return instantiate(input, imports, options);
    let position = 8;
    const read = () => { let result=0,shift=0,next; do { if(shift>=35||position>=input.length)throw Error('invalid import LEB'); next=input[position++];result|=(next&127)<<shift;shift+=7; }while(next&128);return result>>>0; };
    const string = () => { const length=read();const bytes=input.slice(position,position+length);position+=length;return decode.decode(bytes); };
    const parts = [input.slice(0,8)];
    const mapped = Object.create(null);
    while(position<input.length) {
      const start=position,id=input[position++],length=read(),end=position+length;
      if(end>input.length)throw Error('truncated section');
      if(id!==2){parts.push(input.slice(start,end));position=end;continue;}
      const count=read(), entries=[leb(count)];
      for(let index=0;index<count;index++) {
        const module=string(),name=string(),kind=input[position++],descriptor=position;
        if(kind===0)read();else if(kind===4){position++;read();}else throw Error('import alias adapter accepts functions/tags only');
        const alias=name+'__polyglot_'+index,original=imports[module]?.[name];
        if(original===undefined)throw Error('missing original import '+module+'.'+name);
        (mapped[module]??=Object.create(null))[alias]=typeof original==='function' ? (...args)=>original(...args) : original;
        entries.push(nameBytes(module),nameBytes(alias),[kind],input.slice(descriptor,position));
        __importAliases.push({module,name,alias,kind});
      }
      if(position!==end)throw Error('invalid import section extent');
      const payload=join(entries);parts.push([2],leb(payload.length),payload);
    }
    const rewritten=join(parts);
    globalThis.__rewrittenModule=rewritten;
    return instantiate(rewritten,mapped,options);
  };
}
