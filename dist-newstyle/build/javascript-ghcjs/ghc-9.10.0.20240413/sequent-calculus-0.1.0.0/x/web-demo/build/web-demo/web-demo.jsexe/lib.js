var h$isNode_ = false;
var h$isJvm_ = false;
var h$isJsShell_ = false;
var h$isJsCore_ = false;
var h$isBrowser_ = false;
var h$isGHCJSi_ = false;
function h$isNode() {
  return h$isNode_;
}
function h$isJvm() {
  return h$isJvm_;
}
function h$isJsShell() {
  return h$isJsShell_;
}
function h$isJsCore() {
  return h$isJsCore_;
}
function h$isBrowser() {
  return h$isBrowser_;
}
function h$isGHCJSi() {
  return h$isGHCJSi_;
}
if(typeof process !== 'undefined' && (typeof h$TH !== 'undefined' || (typeof require !== 'undefined' && typeof module !== 'undefined' && module.exports))) {
    h$isNode_ = true;
    var fs = require('fs');
    var path = require('path');
    var os = require('os');
    var child_process = require('child_process');
    var h$fs = fs;
    var h$path = path;
    var h$os = os;
    var h$child = child_process;
    var h$process = process;
    function h$getProcessConstants() {
      var cs = process['binding']('constants');
      if(typeof cs.os === 'object' && typeof cs.fs === 'object') {
        return cs;
      } else {
        return { 'fs': cs
               , 'crypto': cs
               , 'os': { 'UV_UDP_REUSEADDR': cs['UV_UDP_REUSEADDR']
                           , 'errno': cs
                           , 'signals': cs
                           }
               };
      }
    }
    var h$processConstants = h$getProcessConstants();
} else if(typeof Java !== 'undefined') {
    h$isJvm_ = true;
    this.console = {
      log: function(s) {
        java.lang.System.out.print(s);
      }
    };
} else if(typeof snarf !== 'undefined' && typeof print !== 'undefined' && typeof quit !== 'undefined') {
    h$isJsShell_ = true;
    this.console = { log: this.print };
} else if(typeof numberOfDFGCompiles !== 'undefined' && typeof jscStack !== 'undefined') {
    h$isJsCore_ = true;
} else {
    h$isBrowser_ = true;
}
if(typeof global !== 'undefined' && global.h$GHCJSi) {
  h$isGHCJSi_ = true;
}
function h$getGlobal(that) {
    if(typeof global !== 'undefined') return global;
    return that;
}

function h$base_access(file, file_off, mode, c) {
    if(h$isNode()) {
        h$fs.access(h$decodeUtf8z(file, file_off), mode, function(err) {
            if (err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_chmod(file, file_off, mode, c) {
    if(h$isNode()) {
        h$fs.chmod(h$decodeUtf8z(file, file_off), mode, function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_close(fd, c) {
  return h$close(fd,c);
}
function h$close(fd,c) {
  if (c) {
    var fdo = h$base_fds[fd];
    if(fdo) {
        delete h$base_fds[fd];
        if(--fdo.refs < 1) {
          if(fdo.close) {
            fdo.close(fd, fdo, c);
          } else {
            c(0);
          }
        } else {
          c(0);
        }
    } else {
        h$errno = 28;
        c(-1);
    }
  } else {
    try {
      h$fs.closeSync(fd);
      return 0;
    } catch(err) {
      h$setErrno(err);
      return (-1);
    }
  }
}
function h$base_dup(fd, c) {
    h$base_dup2(fd, h$base_fdN--, c);
}
function h$base_dup2(fd, new_fd, c) {
    var fdo = h$base_fds[fd];
    if(!fdo) {
      h$errno = 28;
      c(-1);
    } else {
      var new_fdo = h$base_fds[new_fd];
      function f() {
        h$base_fds[new_fd] = fdo;
        fdo.refs++;
        c(new_fd);
      }
      if(new_fdo) {
        h$base_close(new_fd, f);
      } else {
        f();
      }
    }
}
function h$base_fstat(fd, stat, stat_off, c) {
    if(h$isNode()) {
        h$fs.fstat(fd, function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_isatty(fd) {
    var fdo = h$base_fds[fd];
    if(fdo && typeof fdo.isatty !== 'undefined') {
      if(typeof fdo.isatty === 'function') return fdo.isatty() ? 1 : 0;
      return fdo.isatty ? 1 : 0;
    }
    return 0;
}
function h$long_from_number(f,c) {
  if (f > 0) {
      if (f >= 0x8000000000000000) {
        return c(0x7FFFFFFF,0xFFFFFFFF);
      }
      return c(f / 0x100000000, f);
    } else if (f < 0) {
      if (f <= -0x8000000000000000) {
        return c(0x80000000,0);
      }
      var h = -f / 0x100000000;
      var l = -f;
      var nl = (~l + 1) | 0;
      var nh = (~h + !nl) | 0;
      return c(nh,nl);
    } else {
      return c(0,0);
    }
}
function h$base_lseek(fd, pos_h, pos_l, whence, c) {
    if(h$isNode()) {
        var p = (((pos_h)*0x100000000) + ((pos_l)>>>0));
        var o = h$base_fds[fd];
        if(!o) {
            h$errno = 8;
            c(-1,-1);
        } else {
            switch(whence) {
            case 0:
                o.pos = p;
                c(pos_h, pos_l);
                break;
            case 1:
                o.pos += p;
                h$long_from_number(o.pos,c);
                break;
            case 2:
                h$fs.fstat(fd, function(err, fs) {
                    if(err) {
                        h$setErrno(err);
                        c(-1,-1);
                    } else {
                        o.pos = fs.size + p;
                        h$long_from_number(o.pos,c);
                    }
                });
                break;
            default:
                h$errno = 28;
                c(-1,-1);
            }
        }
    } else {
        h$unsupported();
        c(-1, -1);
    }
}
function h$base_lstat(file, file_off, stat, stat_off, c) {
    if(h$isNode()) {
        h$fs.lstat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$lstat(file, file_off, stat, stat_off) {
  if(h$isNode()) {
    try {
      var fs = h$fs.lstatSync(h$decodeUtf8z(file, file_off));
      h$base_fillStat(fs, stat, stat_off);
      return 0;
    } catch(e) {
      h$setErrno(e);
      return -1;
    }
  } else
    h$unsupported(-1);
}
function h$rmdir(file, file_off) {
  if(h$isNode()) {
    try {
      var fs = h$fs.rmdirSync(h$decodeUtf8z(file, file_off));
      return 0;
    } catch(e) {
      h$setErrno(e);
      return -1;
    }
  } else
    h$unsupported(-1);
}
function h$rename(old_path, old_path_off, new_path, new_path_off) {
  if (h$isNode()) {
    try {
      fs.renameSync(h$decodeUtf8z(old_path, old_path_off), h$decodeUtf8z(new_path, new_path_off));
      return 0;
    } catch(e) {
      h$setErrno(e);
      return -1;
    }
  } else
    h$unsupported(-1);
}
function h$getcwd(buf, off, buf_size) {
  if (h$isNode()) {
    try {
      var cwd = h$encodeUtf8(process.cwd());
      h$copyMutableByteArray(cwd, 0, buf, off, cwd.len);
      { h$ret1 = (0); return (cwd); };
    } catch (e) {
      h$setErrno(e);
      return -1;
    }
  } else
    h$unsupported(-1);
}
function h$realpath(path,off,resolved,resolved_off) {
  if (h$isNode()) {
    try {
      var rp = h$encodeUtf8(fs.realpathSync(h$decodeUtf8z(path,off)));
      if (resolved !== null) {
        h$copyMutableByteArray(rp, 0, resolved, resolved_off, Math.min(resolved.len - resolved_off, rp.len));
        { h$ret1 = (resolved_off); return (resolved); };
      }
      { h$ret1 = (0); return (rp); };
    } catch (e) {
      h$setErrno(e);
      return -1;
    }
  } else
    h$unsupported(-1);
}
function h$base_open(file, file_off, how, mode, c) {
  return h$open(file,file_off,how,mode,c);
}
function h$openat(dirfd, file, file_off, how, mode) {
  if (dirfd != h$base_at_fdcwd) {
    return h$unsupported(-1);
  }
  else {
    return h$open(file,file_off,how,mode,undefined);
  }
}
function h$open(file, file_off, how, mode,c) {
    if(h$isNode()) {
        var flags, off;
        var fp = h$decodeUtf8z(file, file_off);
        var acc = how & h$base_o_accmode;
        if(acc === h$base_o_rdonly) {
            flags = h$processConstants['fs']['O_RDONLY'];
        } else if(acc === h$base_o_wronly) {
            flags = h$processConstants['fs']['O_WRONLY'];
        } else {
            flags = h$processConstants['fs']['O_RDWR'];
        }
        off = (how & h$base_o_append) ? -1 : 0;
        flags = flags | ((how & h$base_o_trunc) ? h$processConstants['fs']['O_TRUNC'] : 0)
                      | ((how & h$base_o_creat) ? h$processConstants['fs']['O_CREAT'] : 0)
                      | ((how & h$base_o_excl) ? h$processConstants['fs']['O_EXCL'] : 0)
                      | ((how & h$base_o_append) ? h$processConstants['fs']['O_APPEND'] : 0);
        if (c) {
          h$fs.open(fp, flags, mode, function(err, fd) {
              if(err) {
                  h$handleErrnoC(err, -1, 0, c);
              } else {
                  var f = function(p) {
                      h$base_fds[fd] = { read: h$base_readFile
                                       , write: h$base_writeFile
                                       , close: h$base_closeFile
                                       , fd: fd
                                       , pos: p
                                       , refs: 1
                                       };
                      c(fd);
                  }
                  if(off === -1) {
                      h$fs.stat(fp, function(err, fs) {
                          if(err) h$handleErrnoC(err, -1, 0, c); else f(fs.size);
                      });
                  } else {
                      f(0);
                  }
              }
          });
        }
        else {
          try {
            var fd = h$fs.openSync(fp, flags, mode);
            var f = function(p) {
                      h$base_fds[fd] = { read: h$base_readFile
                                       , write: h$base_writeFile
                                       , close: h$base_closeFile
                                       , fd: fd
                                       , pos: p
                                       , refs: 1
                                       };
                  }
            if(off === -1) {
              var fs = h$fs.statSync(fp);
              f(fs.size);
            } else {
              f(0);
            }
            return fd;
          } catch(err) {
            h$setErrno(err);
            return -1;
          }
        }
    } else
        return h$unsupported(-1,c);
}
function h$base_read(fd, buf, buf_off, n, c) {
    var fdo = h$base_fds[fd];
    if(fdo && fdo.read) {
        fdo.read(fd, fdo, buf, buf_off, n, c);
    } else {
        h$fs.read(fd, buf.u8, buf_off, n, null, function(err, bytesRead, buf0) {
            h$handleErrnoC(err, -1, bytesRead, c);
        });
    }
}
function h$base_stat(file, file_off, stat, stat_off, c) {
    if(h$isNode()) {
        h$fs.stat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handleErrnoC(err, -1, 0, c);
            } else {
                h$base_fillStat(fs, stat, stat_off);
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_umask(mode) {
    if(h$isNode()) return process.umask(mode);
    return 0;
}
function h$base_write(fd, buf, buf_off, n, c) {
  return h$write(fd,buf,buf_off,n,c);
}
function h$write(fd, buf, buf_off, n, c) {
    if (c) {
      var fdo = h$base_fds[fd];
      if(fdo && fdo.write) {
          fdo.write(fd, fdo, buf, buf_off, n, c);
      } else {
          h$fs.write(fd, buf.u8, buf_off, n, function(err, bytesWritten, buf0) {
              h$handleErrnoC(err, -1, bytesWritten, c);
          });
      }
    } else {
      try {
        return h$fs.writeSync(fd, buf.u8, buf_off, n);
      } catch(err) {
        h$setErrno(err);
        return (-1);
      }
    }
}
function h$base_ftruncate(fd, pos_h, pos_l, c) {
    if(h$isNode()) {
        h$fs.ftruncate(fd, (((pos_h)*0x100000000) + ((pos_l)>>>0)), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_unlink(file, file_off, c) {
    if(h$isNode()) {
        h$fs.unlink(h$decodeUtf8z(file, file_off), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_getpid() {
    if(h$isNode()) return process.pid;
    return 0;
}
function h$base_link(file1, file1_off, file2, file2_off, c) {
    if(h$isNode()) {
        h$fs.link(h$decodeUtf8z(file1, file1_off), h$decodeUtf8z(file2, file2_off), function(err) {
            h$handleErrnoC(err, -1, 0, c);
        });
    } else
        h$unsupported(-1, c);
}
function h$base_mkfifo(file, file_off, mode, c) {
    throw "h$base_mkfifo";
}
function h$base_sigemptyset(sigset, sigset_off) {
    return 0;
}
function h$base_sigaddset(sigset, sigset_off, sig) {
    return 0;
}
function h$base_sigprocmask(sig, sigset1, sigset1_off, sigset2, sigset2_off) {
    return 0;
}
function h$base_tcgetattr(attr, termios, termios_off) {
    return 0;
}
function h$base_tcsetattr(attr, val, termios, termios_off) {
    return 0;
}
function h$base_utime(file, file_off, timbuf, timbuf_off, c) {
    if(h$isNode()) {
        h$fs.fstat(h$decodeUtf8z(file, file_off), function(err, fs) {
            if(err) {
                h$handleErrnoC(err, 0, -1, c);
            } else {
                h$base_store_field_number(timbuf, timbuf_off, 0, 8, fs.atime.getTime());
                h$base_store_field_number(timbuf, timbuf_off, 8, 8, fs.mtime.getTime());
                c(0);
            }
        });
    } else
        h$unsupported(-1, c);
}
function h$base_waitpid(pid, stat, stat_off, options, c) {
    throw "h$base_waitpid";
}
const h$base_o_rdonly = 0x00000;
const h$base_o_wronly = 0x00001;
const h$base_o_rdwr = 0x00002;
const h$base_o_accmode = 0x00003;
const h$base_o_append = 0x00008;
const h$base_o_creat = 0x00200;
const h$base_o_trunc = 0x00400;
const h$base_o_excl = 0x00800;
const h$base_o_noctty = 0x20000;
const h$base_o_nonblock = 0x00004;
const h$base_o_binary = 0x00000;
const h$base_at_fdcwd = -100;
function h$base_stat_check_mode(mode,p) {
  var r = (mode & h$fs.constants.S_IFMT) === p;
  return r ? 1 : 0;
}
function h$base_c_s_isreg(mode) {
  return h$base_stat_check_mode(mode,h$fs.constants.S_IFREG);
}
function h$base_c_s_ischr(mode) {
  return h$base_stat_check_mode(mode,h$fs.constants.S_IFCHR);
}
function h$base_c_s_isblk(mode) {
  return h$base_stat_check_mode(mode,h$fs.constants.S_IFBLK);
}
function h$base_c_s_isdir(mode) {
  return h$base_stat_check_mode(mode,h$fs.constants.S_IFDIR);
}
function h$base_c_s_isfifo(mode) {
  return h$base_stat_check_mode(mode,h$fs.constants.S_IFIFO);
}
function h$base_c_fcntl_read(fd,cmd) {
    return -1;
}
function h$base_c_fcntl_write(fd,cmd,value) {
    return -1;
}
function h$base_c_fcntl_lock(fd,cmd,ptr,ptr_o) {
    return -1;
}
function h$base_fillStat(fs, b, off) {
    if(off%4) throw new Error("h$base_fillStat: not aligned");
    var o = off>>2;
    for(var i=0;i<(96>>2);i++) {
        b.i3[o+i] = 0;
    }
    h$base_store_field_number(b, off, 0, 4, fs.dev);
    h$base_store_field_number(b, off, 4, 4, fs.mode);
    h$base_store_field_number(b, off, 8, 4, fs.nlink);
    h$base_store_field_number(b, off, 12, 4, fs.uid);
    h$base_store_field_number(b, off, 16, 4, fs.gid);
    h$base_store_field_number(b, off, 20, 4, fs.rdev);
    h$base_store_field_number(b, off, 24, 8, fs.size);
    h$base_store_field_number(b, off, 32, 4, fs.blksize);
    h$base_store_field_number(b, off, 36, 4, fs.blocks);
    h$base_store_field_number(b, off, 88, 8, fs.ino);
    var atimeS = Math.floor(fs.atimeMs/1000);
    var atimeNs = (fs.atimeMs/1000 - atimeS) * 1000000000;
    h$base_store_field_number(b, off, 40, 8, atimeS);
    h$base_store_field_number(b, off, 48, 4, atimeNs);
    var mtimeS = Math.floor(fs.mtimeMs/1000);
    var mtimeNs = (fs.mtimeMs/1000 - mtimeS) * 1000000000;
    h$base_store_field_number(b, off, 56, 8, mtimeS);
    h$base_store_field_number(b, off, 64, 4, mtimeNs);
    var ctimeS = Math.floor(fs.ctimeMs/1000);
    var ctimeNs = (fs.ctimeMs/1000 - ctimeS) * 1000000000;
    h$base_store_field_number(b, off, 72, 8, ctimeS);
    h$base_store_field_number(b, off, 80, 4, ctimeNs);
}
function h$base_store_field_number(ptr, ptr_off, field_off, field_size, val) {
    if(ptr_off%4) throw new Error("ptr not aligned");
    if(field_off%4) throw new Error("field not aligned");
    if(typeof val !== 'number') throw new Error("not a number: " + val);
    if(field_size === 4) {
        ptr.i3[(ptr_off>>2)+(field_off>>2)] = val;
    } else if(field_size === 8) {
        h$long_from_number(val, (h,l) => {
            ptr.i3[(ptr_off>>2)+(field_off>>2)] = l;
            ptr.i3[(ptr_off>>2)+(field_off>>2)+1] = h;
        });
    } else {
        throw new Error("unsupported field size: " + field_size);
    }
}
function h$base_return_field(ptr, ptr_off, field_off, field_size) {
    if(ptr_off%4) throw new Error("ptr not aligned");
    if(field_off%4) throw new Error("field not aligned");
    if(field_size === 4) {
        return ptr.i3[(ptr_off>>2) + (field_off>>2)];
    } else if(field_size === 8) {
        { h$ret1 = (ptr.i3[(ptr_off>>2) + (field_off>>2)]); return (ptr.i3[(ptr_off>>2) + (field_off>>2)+1]); };
    } else {
        throw new Error("unsupported field size: " + field_size);
    }
}
function h$base_sizeof_stat() {
    return 96;
}
function h$base_st_mtime(stat, stat_off) {
    return h$base_return_field(stat, stat_off, 56, 8);
}
function h$base_st_size(stat, stat_off) {
    return h$base_return_field(stat, stat_off, 24, 8);
}
function h$base_st_mode(stat, stat_off) {
    return h$base_return_field(stat, stat_off, 4, 4);
}
function h$base_st_dev(stat, stat_off) {
    return h$base_return_field(stat, stat_off, 0, 4);
}
function h$base_st_ino(stat, stat_off) {
    return h$base_return_field(stat, stat_off, 88, 8);
}
              var h$base_echo = 1;
              var h$base_tcsanow = 2;
              var h$base_icanon = 4;
              var h$base_vmin = 8;
              var h$base_vtime = 16;
              var h$base_sigttou = 0;
              var h$base_sig_block = 0;
              var h$base_sig_setmask = 0;
              var h$base_f_getfl = 0;
              var h$base_f_setfl = 0;
              var h$base_f_setfd = 0;
              var h$base_fd_cloexec = 0;
              var h$base_sizeof_termios = 4;
              var h$base_sizeof_sigset_t = 4;
function h$base_lflag(termios, termios_off) {
    return 0;
}
function h$base_poke_lflag(termios, termios_off, flag) {
    return 0;
}
function h$base_ptr_c_cc(termios, termios_off) {
    { h$ret1 = (0); return (h$newByteArray(8)); };
}
              var h$base_default_buffer_size = 32768;
function h$base_c_s_issock(mode) {
    return 0;
}
              var h$base_SEEK_SET = 0;
              var h$base_SEEK_CUR = 1;
              var h$base_SEEK_END = 2;
function h$base_set_saved_termios(a, b, c) {
    { h$ret1 = (0); return (null); };
}
function h$base_get_saved_termios(r) {
    { h$ret1 = (0); return (null); };
}
function h$lockFile(fd, dev, ino, for_writing) {
    return 0;
}
function h$unlockFile(fd) {
    return 0;
}
var h$base_readStdin , h$base_writeStderr, h$base_writeStdout;
var h$base_isattyStdin = false, h$base_isattyStdout = false, h$base_isattyStderr = false;
var h$base_closeStdin = null, h$base_closeStderr = null, h$base_closeStdout = null;
var h$base_readFile, h$base_writeFile, h$base_closeFile;
var h$base_stdin_waiting = new h$Queue();
var h$base_stdin_chunk = { buf: null
                           , pos: 0
                           , processing: false
                           };
var h$base_stdin_eof = false;
var h$base_process_stdin = function() {
    var c = h$base_stdin_chunk;
    var q = h$base_stdin_waiting;
    if(!q.length() || c.processing) return;
    c.processing = true;
    if(!c.buf) { c.pos = 0; c.buf = process.stdin.read(); }
    while(c.buf && q.length()) {
        var x = q.dequeue();
        var n = Math.min(c.buf.length - c.pos, x.n);
        for(var i=0;i<n;i++) {
            x.buf.u8[i+x.off] = c.buf[c.pos+i];
        }
        c.pos += n;
        x.c(n);
        if(c.pos >= c.buf.length) c.buf = null;
        if(!c.buf && q.length()) { c.pos = 0; c.buf = process.stdin.read(); }
    }
    while(h$base_stdin_eof && q.length()) q.dequeue().c(0);
    c.processing = false;
}
if(h$isNode()) {
    h$base_closeFile = function(fd, fdo, c) {
        var real_fd = typeof fdo.fd === 'number' ? fdo.fd : fd;
        h$fs.close(real_fd, function(err) {
            delete h$base_fds[fd];
            h$handleErrnoC(err, -1, 0, c);
        });
    }
    h$base_readFile = function(fd, fdo, buf, buf_offset, n, c) {
        var pos = typeof fdo.pos === 'number' ? fdo.pos : null;
        var real_fd = typeof fdo.fd === 'number' ? fdo.fd : fd;
        h$fs.read(real_fd, Buffer.alloc(n), 0, n, pos, function(err, bytesRead, nbuf) {
            if(err) {
                h$setErrno(err);
                c(-1);
            } else {
                for(var i=bytesRead-1;i>=0;i--) buf.u8[buf_offset+i] = nbuf[i];
                if(typeof fdo.pos === 'number') fdo.pos += bytesRead;
                c(bytesRead);
            }
        });
    }
    var h$base_stdinHandlerInstalled = false;
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        if(!h$base_stdinHandlerInstalled) {
            process.stdin.on('readable', h$base_process_stdin);
            h$base_stdinHandlerInstalled = true;
        }
        h$base_stdin_waiting.enqueue({buf: buf, off: buf_offset, n: n, c: c});
        h$base_process_stdin();
    }
    h$base_closeStdin = function(fd, fdo, c) {
        c(0);
    }
    h$base_writeFile = function(fd, fdo, buf, buf_offset, n, c) {
        var pos = typeof fdo.pos === 'number' ? fdo.pos : null;
        var nbuf = Buffer.alloc(n);
        for(var i=0;i<n;i++) nbuf[i] = buf.u8[i+buf_offset];
        var real_fd = typeof fdo.fd === 'number' ? fdo.fd : fd;
        if(typeof fdo.pos === 'number') fdo.pos += n;
        h$fs.write(real_fd, nbuf, 0, n, pos, function(err, bytesWritten) {
            if(err) {
                h$setErrno(err);
                if(typeof fdo.pos === 'number') fdo.pos -= n;
                if(h$errno === 6)
                    setTimeout(function() { h$base_writeFile(fd, fdo, buf, buf_offset, n, c); }, 20);
                else c(-1);
            } else {
                if(typeof fdo.pos === 'number') fdo.pos += bytesWritten - n;
                c(bytesWritten);
            }
        });
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        h$base_writeFile(1, fdo, buf, buf_offset, n, c);
    }
    h$base_closeStdout = function(fd, fdo, c) {
        c(0);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        h$base_writeFile(2, fdo, buf, buf_offset, n, c);
    }
    h$base_closeStderr = function(fd, fdo, c) {
        c(0);
    }
    process.stdin.on('end', function() { h$base_stdin_eof = true; h$base_process_stdin(); });
    h$base_isattyStdin = function() { return process.stdin.isTTY; };
    h$base_isattyStdout = function() { return process.stdout.isTTY; };
    h$base_isattyStderr = function() { return process.stderr.isTTY; };
} else if (h$isJsShell()) {
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        c(0);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        putstr(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        printErr(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
} else if(h$isJsCore()) {
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
 c(0);
    }
    var h$base_stdoutLeftover = { f: print, val: null };
    var h$base_stderrLeftover = { f: debug, val: null };
    var h$base_writeWithLeftover = function(buf, n, buf_offset, c, lo) {
 var lines = h$decodeUtf8(buf, n, buf_offset).split(/\r?\n/);
 if(lines.length === 1) {
     if(lines[0].length) {
  if(lo.val !== null) lo.val += lines[0];
  else lo.val = lines[0];
     }
 } else {
            lo.f(((lo.val !== null) ? lo.val : '') + lines[0]);
     for(var i=1;i<lines.length-1;i++) lo.f(lines[i]);
     if(lines[lines.length-1].length) lo.val = lines[lines.length-1];
     else lo.val = null;
 }
 c(n);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
 h$base_writeWithLeftover(buf, n, buf_offset, c, h$base_stdoutLeftover);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
 h$base_writeWithLeftover(buf, n, buf_offset, c, h$base_stderrLeftover);
    }
} else {
    h$base_readStdin = function(fd, fdo, buf, buf_offset, n, c) {
        c(0);
    }
    h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
        console.log(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
    h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
        console.log(h$decodeUtf8(buf, n, buf_offset));
        c(n);
    }
}
var h$base_stdin_fd =
  { read: h$base_readStdin
  , close: h$base_closeStdin
  , isatty: h$base_isattyStdin
  , refs: 1
  };
var h$base_stdout_fd =
  { write: h$base_writeStdout
  , close: h$base_closeStdout
  , isatty: h$base_isattyStdout
  , refs: 1
  };
var h$base_stderr_fd =
  { write: h$base_writeStderr
  , close: h$base_closeStderr
  , isatty: h$base_isattyStderr
  , refs: 1
  };
var h$base_fdN = -3;
var h$base_fds = [h$base_stdin_fd, h$base_stdout_fd, h$base_stderr_fd];
function h$shutdownHaskellAndExit(code, fast) {
    h$exitProcess(code);
}
function h$rand() {
  return (32768 * Math.random()) & 32767;
}
function h$stg_sig_install(sigNo, actionCode, sigSet_d, sigSet_o) {
  return 0;
}
const h$putchar_buf = h$newByteArray(1);
function h$putchar(c) {
  h$putchar_buf.u8[0] = c;
  h$base_write(1, h$putchar_buf, 0, 1, null);
  return h$errno;
}
function h$__hscore_set_errno(n) {
  h$errno = n;
}
function h$opendir(path) {
  if(!h$isNode()) {
    throw "h$opendir unsupported";
  }
  const d = fs.opendirSync(h$decodeUtf8z(path,0));
  { h$ret1 = (0); return (d); };
}
function h$closedir(d,o) {
  if(!h$isNode()) {
    throw "h$closedir unsupported";
  }
  d.closeSync();
  return 0;
}
function h$readdir(d,o) {
  if(!h$isNode()) {
    throw "h$readdir unsupported";
  }
  const c = d.readSync();
  { h$ret1 = (0); return (c); };
}
function h$__hscore_readdir(d,o,dst_a,dst_o) {
  if(!h$isNode()) {
    throw "h$readdir unsupported";
  }
  const e = d.readSync();
  if (!(dst_a).arr) (dst_a).arr = []; (dst_a).arr[dst_o*2] = e; (dst_a).dv.setInt32(dst_o*2,0,true);;
  return 0;
}
function h$__hscore_free_dirent(a,o) {
}
function h$__hscore_d_name(a,o) {
  { h$ret1 = (0); return (h$encodeModifiedUtf8(a.name)); };
}
function h$mkdir(path, path_offset, mode) {
  if (!h$isNode()) {
    throw "h$mkdir unsupported";
  }
  const d = h$decodeUtf8z(path, path_offset);
  try {
    h$fs.mkdirSync(d, {mode: mode});
  } catch(e) {
    h$setErrno(e);
    return -1;
  }
  return 0;
}

var h$errno = 0;
function h$__hscore_get_errno() {
                                             ;
  return h$errno;
}
function h$unsupported(status, c) {
    h$errno = 12456;
    if(c) c(status);
    return status;
}
function h$base_strerror(err) {
    if(err === 12456) {
 { h$ret1 = (0); return (h$encodeUtf8("operation unsupported on this platform")); };
    }
    { h$ret1 = (0); return (h$encodeUtf8(h$errorStrs[err] || "unknown error")); };
}
function h$setErrno(e) {
                               ;
  var es = e.toString();
  var getErr = function() {
      if(es.indexOf('ENOTDIR') !== -1) return 54;
      if(es.indexOf('EISDIR') !== -1) return 31;
      if(es.indexOf('ENOENT') !== -1) return 44;
      if(es.indexOf('EEXIST') !== -1) return 20;
      if(es.indexOf('ENETUNREACH') !== -1) return 28;
      if(es.indexOf('EPERM') !== -1) return 63;
      if(es.indexOf('EMFILE') !== -1) return 33;
      if(es.indexOf('EPIPE') !== -1) return 64;
      if(es.indexOf('EAGAIN') !== -1) return 6;
      if(es.indexOf('EINVAL') !== -1) return 28;
      if(es.indexOf('ESPIPE') !== -1) return 70;
      if(es.indexOf('EBADF') !== -1) return 8;
      if(es.indexOf('ENOSPC') !== -1) return 51;
      if(es.indexOf('EACCES') !== -1) return 2;
      if(es.indexOf('Bad argument') !== -1) return 44;
      throw ("setErrno not yet implemented for: " + e);
  }
  h$errno = getErr();
}
var h$errorStrs = { 1: "Argument list too long"
                   , 2: "Permission denied"
                   , 28: "Invalid argument"
                   , 8: "Bad file descriptor"
                   , 54: "Not a directory"
                   , 31: "Illegal operation on a directory"
                   , 44: "No such file or directory"
                   , 63: "Operation not permitted"
                   , 20: "File exists"
                   , 33: "Too many open files"
                   , 64: "Broken pipe"
                   , 6: "Resource temporarily unavailable"
                   , 70: "Illegal seek"
                   }
function h$handleErrno(r_err, f) {
  try {
    return f();
  } catch(e) {
    h$setErrno(e);
    return r_err;
  }
}
function h$handleErrnoS(r_err, r_success, f) {
  try {
    f();
    return r_success;
  } catch(e) {
    h$setErrno(e);
    return r_err;
  }
}
function h$handleErrnoC(err, r_err, r_success, c) {
    if(err) {
        h$setErrno(err);
        c(r_err);
    } else {
        c(r_success);
    }
}

function h$rts_isThreaded() {
  return 0;
}
  
function h$rts_isTracing() {
  return 0;
}

function h$rts_isDynamic() {
  return 0;
}

function h$rts_isDebugged() {
  return 0;
}

function h$rts_isProfiled() {
  return 0;
}

  
function h$Set(s) {
    this._vals = [];
    this._keys = [];
    this._size = 0;
}
h$Set.prototype.size = function() {
    return this._size;
}
h$Set.prototype.add = function(o) {
    var k = this._keys, v = this._vals;
    if(k[o._key] === undefined) {
        k[o._key] = this._size;
        v[this._size++] = o;
    }
}
h$Set.prototype.remove = function(o) {
    if(this._size === 0) return;
    var k = this._keys, v = this._vals, x = k[o._key];
    if(x !== undefined) {
        delete k[o._key];
        var ls = --this._size;
        if(ls !== x) {
            var l = v[ls];
            v[x] = l;
            k[l._key] = x;
        }
        v[ls] = undefined;
        if(v.length > 10 && 2 * v.length > 3 * ls) this._vals = v.slice(0, ls);
    }
}
h$Set.prototype.has = function(o) {
    return this._keys[o._key] !== undefined;
}
h$Set.prototype.clear = function() {
    if(this._size > 0) {
 this._keys = [];
 this._vals = [];
 this._size = 0;
    }
}
h$Set.prototype.iter = function() {
    return new h$SetIter(this);
}
h$Set.prototype.values = function() {
    return this._vals;
}
function h$SetIter(s) {
    this._n = 0;
    this._s = s;
    this._r = true;
}
h$SetIter.prototype.next = function() {
    if(this._n < this._s._size) {
        this._r = false;
        return this._s._vals[this._n++];
    } else {
        this._r = true;
        return null;
    }
}
h$SetIter.prototype.peek = function() {
    if(this._n < this._s._size) {
        return this._s._vals[this._n];
    } else {
        return null;
    }
}
h$SetIter.prototype.remove = function() {
    if(!this._r) {
        this._s.remove(this._s._vals[--this._n]);
        this._r = true;
    }
}
function h$Map() {
    this._pairsKeys = [];
    this._pairsValues = [];
    this._keys = [];
    this._size = 0;
}
h$Map.prototype.size = function() {
    return this._size;
}
h$Map.prototype.put = function(k,v) {
    var ks = this._keys, pk = this._pairsKeys, pv = this._pairsValues, x = ks[k._key];
    if(x === undefined) {
        var n = this._size++;
        ks[k._key] = n;
        pk[n] = k;
        pv[n] = v;
    } else {
        pv[x] = v;
    }
}
h$Map.prototype.remove = function(k) {
    var kk = k._key, ks = this._keys, pk = this._pairsKeys, pv = this._pairsValues, x = ks[kk];
    if(x !== undefined) {
        delete ks[kk];
        var ss = --this._size;
        if(ss !== x) {
            var pks = pk[ss];
            pk[x] = pks;
            pv[x] = pv[ss];
            ks[pks._key] = x;
        }
        pv[ss] = undefined;
        pk[ss] = undefined;
        if(pk.length > 10 && 2 * pk.length > 3 * this._size) {
            this._pairsKeys = pk.slice(0,ss);
            this._pairsValues = pv.slice(0,ss);
        }
    }
}
h$Map.prototype.has = function(k) {
    return this._keys[k._key] !== undefined;
}
h$Map.prototype.get = function(k) {
    var n = this._keys[k._key];
    if(n !== undefined) {
        return this._pairsValues[n];
    } else {
        return null;
    }
}
h$Map.prototype.iter = function() {
    return new h$MapIter(this);
}
h$Map.prototype.keys = function () {
    return this._pairsKeys;
}
h$Map.prototype.values = function() {
    return this._pairsValues;
}
function h$MapIter(m) {
    this._n = 0;
    this._m = m;
}
h$MapIter.prototype.next = function() {
    return this._n < this._m._size ? this._m._pairsKeys[this._n++] : null;
}
h$MapIter.prototype.nextVal = function() {
    return this._n < this._m._size ? this._m._pairsValues[this._n++] : null;
}
h$MapIter.prototype.peek = function() {
    return this._n < this._m._size ? this._m._pairsKeys[this._n] : null;
}
h$MapIter.prototype.peekVal = function() {
    return this._n < this._m._size ? this._m._pairsValues[this._n] : null;
}
function h$Queue() {
    var b = { b: [], n: null };
    this._blocks = 1;
    this._first = b;
    this._fp = 0;
    this._last = b;
    this._lp = 0;
}
h$Queue.prototype.length = function() {
    return 1000 * (this._blocks - 1) + this._lp - this._fp;
}
h$Queue.prototype.isEmpty = function() {
    return this._blocks === 1 && this._lp >= this._fp;
}
h$Queue.prototype.enqueue = function(o) {
    if(this._lp === 1000) {
        var newBlock = { b: [o], n: null };
        this._blocks++;
        this._last.n = newBlock;
        this._last = newBlock;
        this._lp = 1;
    } else {
        this._last.b[this._lp++] = o;
    }
}
h$Queue.prototype.dequeue = function() {
    if(this._blocks === 1 && this._fp >= this._lp) {
        return null;
    } else {
        var qfb = this._first.b, r = qfb[this._fp];
        qfb[this._fp] = null;
        if(++this._fp === 1000) {
            if(this._blocks === 1) {
                this._lp = 0;
            } else {
                this._blocks--;
                this._first = this._first.n;
            }
            this._fp = 0;
        } else if(this._blocks === 1 && this._fp >= this._lp) {
            this._lp = this._fp = 0;
        }
        return r;
    }
}
h$Queue.prototype.peek = function() {
    if(this._blocks === 0 || (this._blocks === 1 && this._fp >= this._lp)) {
        return null;
    } else {
        return this._first.b[this._fp];
    }
}
h$Queue.prototype.iter = function() {
    var b = this._first, bp = this._fp, lb = this._last, lp = this._lp;
    return function() {
        if(b === null || (b === lb && bp >= lp)) {
            return null;
        } else {
            var r = b.b[bp];
            if(++bp === 1000) {
                b = b.n;
                bp = 0;
                if(b === null) lb = null;
            }
            return r;
        }
    }
}
function h$HeapSet() {
    this._keys = [];
    this._prios = [];
    this._vals = [];
    this._size = 0;
}
h$HeapSet.prototype.size = function() {
    return this._size;
}
h$HeapSet.prototype.add = function(op,o) {
    var p = this._prios, k = this._keys, v = this._vals, x = k[o._key];
    if(x !== undefined) {
        var oop = p[x];
        if(oop !== op) {
            p[x] = op;
            if(op < oop) {
                this._upHeap(x);
            } else {
                this._downHeap(x, this._size);
            }
        }
    } else {
        var s = this._size++;
        k[o._key] = s;
        p[s] = op;
        v[s] = o;
        this._upHeap(s);
    }
}
h$HeapSet.prototype.has = function(o) {
    return this._keys[o._key] !== undefined;
}
h$HeapSet.prototype.prio = function(o) {
    var x = this._keys[o._key];
    if(x !== undefined) {
        return this._prios[x];
    } else {
        return null;
    }
}
h$HeapSet.prototype.peekPrio = function() {
    return this._size > 0 ? this._prios[0] : null;
}
h$HeapSet.prototype.peek = function() {
    return this._size > 0 ? this._vals[0] : null;
}
h$HeapSet.prototype.pop = function() {
    if(this._size > 0) {
        var v = this._vals[0];
        this._removeNode(0);
        return v;
    } else {
        return null;
    }
}
h$HeapSet.prototype.remove = function(o) {
    var x = this._keys[o._key];
    if(x !== undefined) this._removeNode(x);
}
h$HeapSet.prototype.iter = function() {
    var n = 0, v = this._vals, s = this._size;
    return function() {
        return n < s ? v[n++] : null;
    }
}
h$HeapSet.prototype.values = function() {
    return this._vals;
}
h$HeapSet.prototype._removeNode = function(i) {
    var p = this._prios, v = this._vals, s = --this._size, k = this._keys;
    delete k[v[i]._key];
    if(i !== s) {
        v[i] = v[s];
        p[i] = p[s];
        k[v[i]._key] = i;
    }
    v[s] = null;
    p[s] = null;
    this._downHeap(i,s);
}
h$HeapSet.prototype._downHeap = function(i,s) {
    var p = this._prios, v = this._vals, k = this._keys;
    var j,l,r,ti,tj;
    while(true) {
        j = i, r = 2*(i+1), l = r-1;
        if(l < s && p[l] < p[i]) i = l;
        if(r < s && p[r] < p[i]) i = r;
        if(i !== j) {
            ti = v[i];
            tj = v[j];
            v[j] = ti;
            v[i] = tj;
            k[ti._key] = j;
            k[tj._key] = i;
            ti = p[i];
            p[i] = p[j];
            p[j] = ti;
        } else {
            break;
        }
    }
}
h$HeapSet.prototype._upHeap = function(i) {
    var ti, tj, j, p = this._prios, v = this._vals, k = this._keys;
    while(i !== 0) {
        j = (i-1) >> 1;
        if(p[i] < p[j]) {
            ti = v[i];
            tj = v[j];
            v[j] = ti;
            v[i] = tj;
            k[ti._key] = j;
            k[tj._key] = i;
            ti = p[i];
            p[i] = p[j];
            p[j] = ti;
            i = j;
        } else {
            break;
        }
    }
}

function h$hs_quotWord64(h1,l1,h2,l2) {
  var a = ((BigInt(h1) << BigInt(32)) | BigInt(l1>>>0));
  var b = ((BigInt(h2) << BigInt(32)) | BigInt(l2>>>0));
  var r = BigInt.asUintN(64, a / b);
  { h$ret1 = ((Number(BigInt.asUintN(32, r)) >>> 0)); return ((Number(r >> BigInt(32)) >>> 0)); };
}
function h$hs_remWord64(h1,l1,h2,l2) {
  var a = ((BigInt(h1) << BigInt(32)) | BigInt(l1>>>0));
  var b = ((BigInt(h2) << BigInt(32)) | BigInt(l2>>>0));
  var r = BigInt.asUintN(64, a % b);
  { h$ret1 = ((Number(BigInt.asUintN(32, r)) >>> 0)); return ((Number(r >> BigInt(32)) >>> 0)); };
}
function h$hs_timesWord64(h1,l1,h2,l2) {
  var rh = h$mul2Word32(l1,l2);
  var rl = h$ret1;
  rh += Math.imul(l1,h2)>>>0;
  rh += Math.imul(l2,h1)>>>0;
  rh >>>= 0;
  { h$ret1 = (rl); return (rh); };
}
function h$hs_minusWord64(h1,l1,h2,l2) {
  var l = l1-l2;
  var rl = l>>>0;
  var rh = (h1-h2-(l!=rl?1:0))>>>0;
  { h$ret1 = (rl); return (rh); };
}
function h$hs_plusWord64(h1,l1,h2,l2) {
  var l = l1+l2;
  var rl = l>>>0;
  var rh = (h1+h2+(l!=rl?1:0))>>>0;
  { h$ret1 = (rl); return (rh); };
}
function h$hs_timesInt64(h1,l1,h2,l2) {
  var rh = h$mul2Word32(l1,l2);
  var rl = h$ret1;
  rh += Math.imul(l1,h2)|0;
  rh += Math.imul(l2,h1)|0;
  rh |= 0;
  { h$ret1 = (rl); return (rh); };
}
function h$hs_quotInt64(h1,l1,h2,l2) {
  var a = ((BigInt(h1) << BigInt(32)) | BigInt(l1>>>0));
  var b = ((BigInt(h2) << BigInt(32)) | BigInt(l2>>>0));
  var r = BigInt.asIntN(64, a / b);
  { h$ret1 = ((Number(BigInt.asUintN(32,r)) >>> 0)); return ((Number(r >> BigInt(32))|0)); };
}
function h$hs_remInt64(h1,l1,h2,l2) {
  var a = ((BigInt(h1) << BigInt(32)) | BigInt(l1>>>0));
  var b = ((BigInt(h2) << BigInt(32)) | BigInt(l2>>>0));
  var r = BigInt.asIntN(64, a % b);
  { h$ret1 = ((Number(BigInt.asUintN(32,r)) >>> 0)); return ((Number(r >> BigInt(32))|0)); };
}
function h$hs_plusInt64(h1,l1,h2,l2) {
  var l = l1+l2;
  var rl = l>>>0;
  var rh = (h1+h2+(l!=rl?1:0))|0;
  { h$ret1 = (rl); return (rh); };
}
function h$hs_minusInt64(h1,l1,h2,l2) {
  var l = l1-l2;
  var rl = l>>>0;
  var rh = (h1-h2-(l!=rl?1:0))|0;
  { h$ret1 = (rl); return (rh); };
}
function h$hs_uncheckedShiftLWord64(h,l,n) {
  var rh, rl;
  n &= 63;
  if (n == 0) {
    rh = h;
    rl = l;
  } else if (n === 32) {
    rh = l;
    rl = 0;
  } else if (n < 32) {
    rh = (((h << n) | (l >>> (32 - n)))>>>0);
    rl = ((l << n)>>>0);
  } else {
    rh = ((l << (n - 32))>>>0);
    rl = 0;
  }
  { h$ret1 = (rl); return (rh); };
}
function h$hs_uncheckedShiftRWord64(h,l,n) {
  var rh, rl;
  n &= 63;
  if(n == 0) {
    rh = h;
    rl = l;
  } else if(n === 32) {
    rh = 0;
    rl = h;
  } else if(n < 32) {
    rh = h >>> n;
    rl = (((l >>> n ) | (h << (32-n)))>>>0);
  } else {
    rh = 0;
    rl = h >>> (n-32);
  }
  { h$ret1 = (rl); return (rh); };
}
function h$hs_uncheckedShiftLLInt64(h,l,n) {
  var rh,rl;
  n &= 63;
  if (n == 0) {
    rh = h;
    rl = l;
  } else if (n === 32) {
    rh = l|0;
    rl = 0;
  } else if (n < 32) {
    rh = (h << n) | (l >>> (32 - n));
    rl = ((l << n)>>>0);
  } else {
    rh = l << (n - 32);
    rl = 0;
  }
  { h$ret1 = (rl); return (rh); };
}
function h$hs_uncheckedShiftRAInt64(h,l,n) {
  var rh,rl;
  n &= 63;
  if (n == 0) {
    rh = h;
    rl = l;
  } else if (n < 32) {
    rh = h >> n;
    rl = (((l >>> n) | ((h << (32 - n))>>>0))>>>0);
  } else {
    rh = h >= 0 ? 0 : -1;
    rl = ((h >> (n - 32))>>>0);
  }
  { h$ret1 = (rl); return (rh); };
}
function h$hs_uncheckedShiftRLInt64(h,l,n) {
  var rh,rl;
  n &= 63;
  if(n == 0) {
    rh = h;
    rl = l;
  } else if(n == 32) {
    rh = 0;
    rl = ((h)>>>0);
  } else if(n < 32) {
    rh = h >>> n;
    rl = (((l >>> n) | (h << (32-n)))>>>0);
  } else {
    rh = 0;
    rl = h >>> (n-32);
  }
  { h$ret1 = (rl); return (rh); };
}
function h$hs_timesInt2(l1,l2) {
  var ah = l1 >> 16;
  var al = l1 & 0xFFFF;
  var bh = l2 >> 16;
  var bl = l2 & 0xFFFF;
  var r0 = al * bl;
  var r1 = r0 >>> 16;
  r0 &= 0xFFFF;
  r1 += al * bh;
  var r2 = r1 >> 16;
  r1 &= 0xFFFF;
  r1 += ah * bl;
  r2 += r1 >> 16;
  r1 &= 0xFFFF;
  r2 += ah * bh;
  var r3 = (r2 >> 16) & 0xFFFF;
  r2 &= 0xFFFF;
  const rh = r3 << 16 | r2;
  const rl = r1 << 16 | r0;
  var s = rl >> 31;
  if (rh === s) {
    { h$ret1 = (s); h$ret2 = (rl); return (0); };
  }
  else {
    { h$ret1 = (rh); h$ret2 = (rl); return (1); };
  }
}
function h$mul2Word32(l1,l2) {
  var ah = l1 >>> 16;
  var al = l1 & 0xFFFF;
  var bh = l2 >>> 16;
  var bl = l2 & 0xFFFF;
  var r0 = al * bl;
  var r1 = r0 >>> 16;
  r0 &= 0xFFFF;
  r1 += al * bh;
  var r2 = r1 >>> 16;
  r1 &= 0xFFFF;
  r1 += ah * bl;
  r2 += r1 >>> 16;
  r1 &= 0xFFFF;
  r2 += ah * bh;
  var r3 = (r2 >>> 16) & 0xFFFF;
  r2 &= 0xFFFF;
  const rh = (r3 << 16 | r2) >>> 0;
  const rl = (r1 << 16 | r0) >>> 0;
  { h$ret1 = (rl); return (rh); };
}
function h$quotWord32(n,d) {
  var a = (BigInt(n));
  var b = (BigInt(d));
  var r = BigInt.asUintN(32, a / b);
  return Number(r);
}
function h$remWord32(n,d) {
  var a = (BigInt(n));
  var b = (BigInt(d));
  var r = BigInt.asUintN(32, a % b);
  return Number(r);
}
function h$quotRemWord32(n,d) {
  var a = (BigInt(n));
  var b = (BigInt(d));
  var q = BigInt.asUintN(32, a / b);
  var r = BigInt.asUintN(32, a % b);
  { h$ret1 = (Number(r)); return (Number(q)); };
}
function h$quotRem2Word32(nh,nl,d) {
  var a = ((BigInt(nh) << BigInt(32)) | BigInt(nl>>>0));
  var b = (BigInt(d));
  var q = BigInt.asUintN(32, a / b);
  var r = BigInt.asUintN(32, a % b);
  { h$ret1 = (Number(r)); return (Number(q)); };
}
function h$wordAdd2(l1,l2) {
  var r = (l1 >>> 1) + (l2 >>> 1) + (1 & l1 & l2);
  var h = r >>> 31;
  var l = (l1 + l2) >>> 0;
  { h$ret1 = (l); return (h); };
}
function h$isDoubleNegativeZero(d) {
  return (d===0 && (1/d) === -Infinity) ? 1 : 0;
}
function h$isFloatNegativeZero(d) {
  return (d===0 && (1/d) === -Infinity) ? 1 : 0;
}
function h$isDoubleInfinite(d) {
  return (d === Number.NEGATIVE_INFINITY || d === Number.POSITIVE_INFINITY) ? 1 : 0;
}
function h$isFloatInfinite(d) {
  return (d === Number.NEGATIVE_INFINITY || d === Number.POSITIVE_INFINITY) ? 1 : 0;
}
function h$isFloatFinite(d) {
  return (d !== Number.NEGATIVE_INFINITY && d !== Number.POSITIVE_INFINITY && !isNaN(d)) ? 1 : 0;
}
function h$isDoubleFinite(d) {
  return (d !== Number.NEGATIVE_INFINITY && d !== Number.POSITIVE_INFINITY && !isNaN(d)) ? 1 : 0;
}
function h$isDoubleNaN(d) {
  return (isNaN(d)) ? 1 : 0;
}
function h$isFloatNaN(d) {
  return (isNaN(d)) ? 1 : 0;
}
function h$isDoubleDenormalized(d) {
  return (d !== 0 && Math.abs(d) < 2.2250738585072014e-308) ? 1 : 0;
}
function h$isFloatDenormalized(d) {
  h$convertFloat[0] = d;
  var i = h$convertInt[0];
  var exp = (i >> 23) & 0xff;
  var s = i&8388607;
  return ((s !== 0 && exp === 0) ? 1 : 0);
}
var h$convertBuffer = new ArrayBuffer(8);
var h$convertDouble = new Float64Array(h$convertBuffer);
var h$convertFloat = new Float32Array(h$convertBuffer);
var h$convertInt = new Int32Array(h$convertBuffer);
var h$convertWord = new Uint32Array(h$convertBuffer);
h$convertFloat[0] = 0.75;
function h$decodeFloatInt(d) {
    if(isNaN(d)) {
        { h$ret1 = (105); return (-12582912); };
    }
    h$convertFloat[0] = d;
    var i = h$convertInt[0];
    var exp = (i >> 23) & 0xff;
    var sgn = 2 * (i >> 31) + 1;
    var s = i&8388607;
    if(exp === 0) {
        if(s === 0) {
            { h$ret1 = (0); return (0); };
        } else {
            h$convertFloat[0] = d*8388608;
            i = h$convertInt[0];
            s = (i&8388607) | 8388608;
            exp = ((i >> 23) & 0xff) - 173;
            { h$ret1 = (exp); return (sgn*s); }
        }
    } else {
      { h$ret1 = (exp - 150); return (sgn * (s|8388608)); };
    }
}
function h$decodeDouble2Int(d) {
    if(isNaN(d)) {
 { h$ret1 = (-1572864); h$ret2 = (0); h$ret3 = (972); return (1); };
    }
    h$convertDouble[0] = d;
    var i1 = h$convertInt[1];
    var ret1, ret2 = h$convertInt[0], ret3;
    var exp = (i1&2146435072)>>>20;
  if(exp === 0) {
    if((i1&2147483647) === 0 && ret2 === 0) {
      ret1 = 0;
      ret3 = 0;
    } else {
      h$convertDouble[0] = d*9007199254740992;
      i1 = h$convertInt[1];
      ret1 = (i1&1048575)|1048576;
      ret2 = h$convertInt[0];
      ret3 = ((i1&2146435072)>>>20)-1128;
    }
  } else {
    ret3 = exp-1075;
    ret1 = (i1&1048575)|1048576;
  }
    { h$ret1 = (ret1); h$ret2 = (ret2); h$ret3 = (ret3); return (i1<0?-1:1); };
}
function h$rintDouble(a) {
  var rounda = Math.round(a);
  if(a >= 0) {
    if(a%1===0.5 && rounda%2===1) {
      return rounda-1;
    } else {
      return rounda;
    }
  } else {
    if(a%1===-0.5 && rounda%2===-1) {
      return rounda-1;
    } else {
      return rounda;
    }
  }
}
var h$rintFloat = h$rintDouble;
function h$acos(d) { return Math.acos(d); }
function h$acosf(f) { return Math.acos(f); }
function h$asin(d) { return Math.asin(d); }
function h$asinf(f) { return Math.asin(f); }
function h$atan(d) { return Math.atan(d); }
function h$atanf(f) { return Math.atan(f); }
function h$atan2(x,y) { return Math.atan2(x,y); }
function h$atan2f(x,y) { return Math.atan2(x,y); }
function h$cos(d) { return Math.cos(d); }
function h$cosf(f) { return Math.cos(f); }
function h$sin(d) { return Math.sin(d); }
function h$sinf(f) { return Math.sin(f); }
function h$tan(d) { return Math.tan(d); }
function h$tanf(f) { return Math.tan(f); }
function h$cosh(d) { return (Math.exp(d)+Math.exp(-d))/2; }
function h$coshf(f) { return h$cosh(f); }
function h$sinh(d) { return (Math.exp(d)-Math.exp(-d))/2; }
function h$sinhf(f) { return h$sinh(f); }
function h$tanh(d) { return (Math.exp(2*d)-1)/(Math.exp(2*d)+1); }
function h$tanhf(f) { return h$tanh(f); }
var h$popCntTab =
   [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8];
function h$popCnt32(x) {
   return h$popCntTab[x&0xFF] +
          h$popCntTab[(x>>>8)&0xFF] +
          h$popCntTab[(x>>>16)&0xFF] +
          h$popCntTab[(x>>>24)&0xFF];
}
function h$popCnt64(x1,x2) {
   return h$popCntTab[x1&0xFF] +
          h$popCntTab[(x1>>>8)&0xFF] +
          h$popCntTab[(x1>>>16)&0xFF] +
          h$popCntTab[(x1>>>24)&0xFF] +
          h$popCntTab[x2&0xFF] +
          h$popCntTab[(x2>>>8)&0xFF] +
          h$popCntTab[(x2>>>16)&0xFF] +
          h$popCntTab[(x2>>>24)&0xFF];
}
function h$reverseWord(w) {
  var r = w;
  r = ((r >>> 1) & 0x55555555) | ((r & 0x55555555) << 1);
  r = ((r >>> 2) & 0x33333333) | ((r & 0x33333333) << 2);
  r = ((r >>> 4) & 0x0F0F0F0F) | ((r & 0x0F0F0F0F) << 4);
  r = ((r >>> 8) & 0x00FF00FF) | ((r & 0x00FF00FF) << 8);
  r = ( r >>> 16 ) | ( r << 16);
  r = r >>> 0;
  return r;
}
function h$bswap64(x1,x2) {
  { h$ret1 = ((((x1 >>> 24) | (x1 << 24) | ((x1 & 0xFF00) << 8) | ((x1 & 0xFF0000) >> 8))>>>0)); return ((((x2 >>> 24) | (x2 << 24) | ((x2 & 0xFF00) << 8) | ((x2 & 0xFF0000) >> 8))>>>0)); };
}
var h$clz32 = Math.clz32 || function(x) {
    if (x < 0) return 0;
    if (x === 0) return 32;
    return 31 - ((Math.log(x) / Math.LN2) | 0);
}
function h$clz8(x) {
    return h$clz32(x&255)-24;
}
function h$clz16(x) {
    return h$clz32(x&65535)-16;
}
function h$clz64(x1,x2) {
    return (x1 === 0) ? 32 + h$clz32(x2) : h$clz32(x1);
}
var h$ctz32tbl = [32,0,1,26,2,23,27,0,3,16,24,30,28,11,0,13,4,7,17,0,25,22,31,15,29,10,12,6,0,21,14,9,5,20,8,19,18,0,0,0,0,0,31];
function h$ctz32(x) {
    return h$ctz32tbl[((x&-x)%37)&63];
}
function h$ctz16(x) {
    return h$ctz32(x|65536);
}
function h$ctz8(x) {
    return h$ctz32(x|256);
}
function h$ctz64(x1,x2) {
    return (x2 === 0) ? 32 + h$ctz32(x1) : h$ctz32(x2);
}
function h$decodeDoubleInt64(d) {
  if(isNaN(d)) {
    { h$ret1 = (-1572864); h$ret2 = (0); return (972); };
  }
  h$convertDouble[0] = d;
  var i0 = h$convertInt[0], i1 = h$convertInt[1];
  var exp = (i1&2146435072)>>>20;
  var ret1, ret2 = i0, ret3;
  if(exp === 0) {
    if((i1&2147483647) === 0 && ret2 === 0) {
      ret1 = 0;
      ret3 = 0;
    } else {
      h$convertDouble[0] = d*9007199254740992;
      i1 = h$convertInt[1];
      ret1 = (i1&1048575)|1048576;
      ret2 = h$convertInt[0];
      ret3 = ((i1&2146435072)>>>20)-1128;
    }
  } else {
    ret3 = exp-1075;
    ret1 = (i1&1048575)|1048576;
  }
  if(d < 0) {
    if(ret2 === 0) {
      ret1 = ((~ret1) + 1) | 0;
    } else {
      ret1 = ~ret1;
      ret2 = ((~ret2) + 1) | 0;
    }
  }
  { h$ret1 = (ret1); h$ret2 = (ret2); return (ret3); };
}
function h$__int_encodeDouble(j,e) {
  if (!j) return 0;
  return (j|0) * (2 ** (e|0));
}
function h$__word_encodeDouble(j,e) {
  if (!j) return 0;
  return (j>>>0) * (2 ** (e|0));
}
function h$__int_encodeFloat(j,e) {
  if (!j) return 0;
  return Math.fround((j|0) * (2 ** (e|0)));
}
function h$__word_encodeFloat(j,e) {
  if (!j) return 0;
  return Math.fround((j>>>0) * (2 ** (e|0)));
}
function h$castWord32ToFloat(v) {
  h$convertWord[0] = v;
  return h$convertFloat[0];
}
function h$castFloatToWord32(v) {
  h$convertFloat[0] = v;
  return h$convertWord[0];
}
function h$castWord64ToDouble(h,l) {
  h$convertWord[0] = l;
  h$convertWord[1] = h;
  return h$convertDouble[0];
}
function h$castDoubleToWord64(v) {
  h$convertDouble[0] = v;
  var l = h$convertWord[0];
  var h = h$convertWord[1];
  { h$ret1 = (l); return (h); };
}
function h$sqrt(x) {
  return Math.sqrt(x);
}
function h$sqrtf(x) {
  return Math.fround(Math.sqrt(x));
}
function h$log1p(x) {
  return Math.log1p(x);
}
function h$log1pf(x) {
  return Math.fround(Math.log1p(x));
}
function h$expm1(x) {
  return Math.expm1(x);
}
function h$expm1f(x) {
  return Math.fround(Math.expm1(x));
}

function h$compactNew(size) {
  throw new Error("not implemented");
}
function h$compactResize(compact, size) {
}
function h$compactContains(compact, obj) {
  return 0;
}
function h$compactContainsAny(obj) {
  return 0;
}
function h$compactGetFirstBlock(compact) {
  { h$ret1 = (0); return (null); };
}
function h$compactGetNextBlock(compact, blocka, blokco) {
  { h$ret1 = (0); return (null); };
}
function h$compactAllocateBlock(size, suggesta, suggesto) {
  throw new Error("not implemented");
  { h$ret1 = (0); return (null); };
}
function h$compactFixupPointers(blocka, blocko, roota, rooto) {
  throw new Error("not implemented");
  { h$ret1 = (null); h$ret2 = (0); return (null); };
}
function h$compactAdd(compact, obj) {
  throw new Error("not implemented");
}
function h$compactAddWithSharing(compact, obj) {
  throw new Error("not implemented");
}
function h$compactCompactSize(compact) {
  return 0;
}

function h$debugAlloc_verifyReachability(mark) {
}
function h$debugAlloc_notifyAlloc(obj) {
}
function h$debugAlloc_notifyUse(obj) {
}

var h$enums = [];
function h$initEnums() {
  for(var i=0;i<256;i++) {
    h$enums[i] = h$makeEnum(i);
  }
}
h$initStatic.push(h$initEnums);
function h$makeEnum(tag) {
  var f = function() {
    return h$stack[h$sp];
  }
  h$setObjInfo(f, 2, "Enum", [], tag+1, 0, [1], null);
  return h$c0(f);
}
function h$tagToEnum(tag) {
  if(tag >= h$enums.length) {
    return h$makeEnum(tag);
  } else {
    return h$enums[tag];
  }
}
function h$dataTag(e) {
  return (e===true)?1:((typeof e !== 'object')?0:(e.f.a-1));
}

var h$glbl;
function h$getGlbl() { h$glbl = this; }
h$getGlbl();
function h$log() {
  try {
    if(h$glbl) {
      if(h$glbl.console && h$glbl.console.log) {
        h$glbl.console.log.apply(h$glbl.console,arguments);
      } else {
        h$glbl.print.apply(this,arguments);
      }
    } else {
      if(typeof console !== 'undefined') {
        console.log.apply(console, arguments);
      } else if(typeof print !== 'undefined') {
        print.apply(null, arguments);
      }
    }
  } catch(ex) {
  }
}
function h$collectProps(o) {
  var props = [];
  for(var p in o) { props.push(p); }
  return("{"+props.join(",")+"}");
}
var h$programArgs_;
var h$rtsArgs_;
function h$programArgs() {
  if (!h$programArgs_) {
    h$initArgs();
  }
  return h$programArgs_;
}
function h$rtsArgs() {
  if (!h$rtsArgs_) {
    h$initArgs();
  }
  return h$rtsArgs_;
}
function h$initArgs() {
  if(h$isNode()) {
      h$programArgs_ = process.argv.slice(1);
  } else if(h$isJvm()) {
      h$programArgs_ = h$getGlobal(this).arguments.slice(0);
      h$programArgs_.unshift("a.js");
  } else if(h$isJsShell() && typeof h$getGlobal(this).scriptArgs !== 'undefined') {
      h$programArgs_ = h$getGlobal(this).scriptArgs.slice(0);
      h$programArgs_.unshift("a.js");
  } else if((h$isJsShell() || h$isJsCore()) && typeof h$getGlobal(this).arguments !== 'undefined') {
      h$programArgs_ = h$getGlobal(this).arguments.slice(0);
      h$programArgs_.unshift("a.js");
  } else {
      h$programArgs_ = [ "a.js" ];
  }
  {
      var prog_args = [];
      var rts_args = [];
      var in_rts = false;
      var i = 0;
      for(i=0;i<h$programArgs_.length;i++) {
          var a = h$programArgs_[i];
          if (a === "--RTS") {
              break;
          }
          else if (a === "--") {
              break;
          }
          else if (a === "+RTS") {
              in_rts = true;
          }
          else if (a === "-RTS") {
              in_rts = false;
          }
          else if (in_rts) {
              rts_args.push(a);
          }
          else {
              prog_args.push(a);
          }
      }
      for (;i<h$programArgs_.length;i++) {
          prog_args.push(h$programArgs_[i]);
      }
      h$programArgs_ = prog_args;
      h$rtsArgs_ = rts_args;
  }
}
function h$getProgArgv(argc_v,argc_off,argv_v,argv_off) {
  var c = h$programArgs().length;
  if(c === 0) {
    argc_v.dv.setInt32(argc_off, 0, true);
  } else {
    argc_v.dv.setInt32(argc_off, c, true);
    var argv = h$newByteArray(4*c);
    for(var i=0;i<h$programArgs().length;i++) {
      if (!(argv).arr) (argv).arr = []; (argv).arr[4*i] = h$encodeUtf8(h$programArgs()[i]); (argv).dv.setInt32(4*i,0,true);;
    }
    if (!(argv_v).arr) (argv_v).arr = []; (argv_v).arr[argv_off] = argv; (argv_v).dv.setInt32(argv_off,0,true);;
  }
}
function h$setProgArgv(n, ptr_d, ptr_o) {
  var args = [];
  for(var i=0;i<n;i++) {
    var off = ptr_o+4*i;
    var p = (((ptr_d).arr && (ptr_d).arr[off]) ? (ptr_d).arr[off] : null_); var o = (ptr_d).dv.getInt32(off,true);;
    var arg = h$decodeUtf8z(p, o);
    args.push(arg);
  }
  h$programArgs_ = args;
}
function h$getpid() {
  if(h$isNode()) return process.id;
  return 0;
}
function h$cpuTimePrecision() {
  return 1000;
}
var h$fakeCpuTime = 1.0;
function h$getCPUTime() {
if(h$isNode()) {
  var t = process.cpuUsage();
  var cput = t.user + t.system;
  return cput;
}
  return ++h$fakeCpuTime;
  return -1;
}
function h$__hscore_environ() {
    if(h$isNode()) {
        var env = [], i;
        for(i in process.env) {
          var envv = i + '=' + process.env[i];
          env.push(envv);
        }
        if(env.length === 0) return null;
        var p = h$newByteArray(4*env.length+1);
        for(i=0;i<env.length;i++) {
          if (!(p).arr) (p).arr = []; (p).arr[4*i] = h$encodeUtf8(env[i]); (p).dv.setInt32(4*i,0,true);;
        }
        if (!(p).arr) (p).arr = []; (p).arr[4*env.length] = null; (p).dv.setInt32(4*env.length,0,true);;
        { h$ret1 = (0); return (p); };
    }
    { h$ret1 = (0); return (null); };
}
function h$__hsbase_unsetenv(name, name_off) {
    return h$unsetenv(name, name_off);
}
function h$getenv(name, name_off) {
    if(h$isNode()) {
        var n = h$decodeUtf8z(name, name_off);
        if(typeof process.env[n] !== 'undefined') {
            { h$ret1 = (0); return (h$encodeUtf8(process.env[n])); };
        }
    }
    { h$ret1 = (0); return (null); };
}
function h$setenv(name, name_off, val, val_off, overwrite) {
  var n = h$decodeUtf8z(name, name_off);
  var v = h$decodeUtf8z(val, val_off);
  if(n.indexOf('=') !== -1) {
    h$setErrno("EINVAL");
    return -1;
  }
  if(h$isNode()) {
    if(overwrite || typeof process.env[n] !== 'undefined') process.env[n] = v;
  }
  return 0;
}
function h$unsetenv(name, name_off) {
  var n = h$decodeUtf8z(name, name_off);
  if(n.indexOf('=') !== -1) {
    h$setErrno("EINVAL");
    return -1;
  }
  if(h$isNode()) delete process.env[n];
  return 0;
}
function h$putenv(str, str_off) {
  var x = h$decodeUtf8z(str, str_off);
  var i = x.indexOf('=');
  if(i === -1) {
    if(h$isNode()) delete process.env[x];
  } else {
    var name = x.substring(0, i)
    var val = x.substring(i+1);
    if(h$isNode()) process.env[name] = val;
  }
  return 0;
}
function h$errorBelch() {
  h$log("### errorBelch: do we need to handle a vararg function here?");
}
function h$errorBelch2(buf1, buf_offset1, buf2, buf_offset2) {
  var pat = h$decodeUtf8z(buf1, buf_offset1);
  h$errorMsg(h$append_prog_name(pat), h$decodeUtf8z(buf2, buf_offset2));
}
function h$append_prog_name(str) {
  function basename(path) {
   return path.split('/').reverse()[0];
  }
  if(h$isNode()) {
    return basename(process.argv[1]) + ": " + str;
  }
  return str;
}
function h$debugBelch2(buf1, buf_offset1, buf2, buf_offset2) {
  h$errorMsg(h$decodeUtf8z(buf1, buf_offset1), h$decodeUtf8z(buf2, buf_offset2));
}
function h$errorMsg(pat) {
  function stripTrailingNewline(xs) {
    return xs.replace(/\r?\n$/, "");
  }
  var str = pat;
  for(var i=1;i<arguments.length;i++) {
    str = str.replace(/%s/, arguments[i]);
  }
  if(h$isGHCJSi()) {
  } else if(h$isNode()) {
    process.stderr.write(str);
  } else if (h$isJsShell() && typeof printErr !== 'undefined') {
    if(str.length) printErr(stripTrailingNewline(str));
  } else if (h$isJsShell() && typeof putstr !== 'undefined') {
    putstr(str);
  } else if (h$isJsCore()) {
    if(str.length) {
 if(h$base_stderrLeftover.val !== null) {
     debug(h$base_stderrLeftover.val + stripTrailingNewline(str));
     h$base_stderrLeftover.val = null;
 } else {
     debug(stripTrailingNewline(str));
 }
    }
  } else {
    if(typeof console !== 'undefined') {
      console.log(str);
    }
  }
}
function h$performMajorGC() {
    var t = h$currentThread, err = null;
    t.sp = h$sp;
    h$currentThread = null;
    try {
        h$gc(t);
    } catch(e) {
        err = e;
    }
    h$currentThread = t;
    h$sp = t.sp;
    h$stack = t.stack;
    if(err) throw err;
}
function h$ghczminternalZCSystemziCPUTimeZCgetrusage() {
  return 0;
}
function h$getrusage() {
  return 0;
}
function h$gettimeofday(tv_v,tv_o,tz_v,tz_o) {
  var now = Date.now();
  tv_v.dv.setInt32(tv_o, (now / 1000)|0, true);
  tv_v.dv.setInt32(tv_o + 4, ((now % 1000) * 1000)|0, true);
  if(tv_v.len >= tv_o + 12) {
    tv_v.dv.setInt32(tv_o + 8, ((now % 1000) * 1000)|0, true);
  }
  return 0;
}
var h$__hscore_gettimeofday = h$gettimeofday;
var h$myTimeZone = h$encodeUtf8("UTC");
function h$localtime_r(timep_v, timep_o, result_v, result_o) {
  var t = timep_v.i3[timep_o];
  var d = new Date(t * 1000);
  result_v.dv.setInt32(result_o , d.getSeconds(), true);
  result_v.dv.setInt32(result_o + 4 , d.getMinutes(), true);
  result_v.dv.setInt32(result_o + 8 , d.getHours(), true);
  result_v.dv.setInt32(result_o + 12, d.getDate(), true);
  result_v.dv.setInt32(result_o + 16, d.getMonth(), true);
  result_v.dv.setInt32(result_o + 20, d.getFullYear()-1900, true);
  result_v.dv.setInt32(result_o + 24, d.getDay(), true);
  result_v.dv.setInt32(result_o + 28, 0, true);
  result_v.dv.setInt32(result_o + 32, -1, true);
  result_v.dv.setInt32(result_o + 40, 0, true);
  if (!(result_v).arr) (result_v).arr = []; (result_v).arr[result_o+40] = h$myTimeZone; (result_v).dv.setInt32(result_o+40,0,true);;
  if (!(result_v).arr) (result_v).arr = []; (result_v).arr[result_o+48] = h$myTimeZone; (result_v).dv.setInt32(result_o+48,0,true);;
  { h$ret1 = (result_o); return (result_v); };
}
var h$__hscore_localtime_r = h$localtime_r;
function h$checkForeignRefs(refs) {
  function argSize(t) {
    if(t === "ghc-prim:GHC.Prim.Word64#") return 2;
    if(t === "ghc-prim:GHC.Prim.State#") return 0;
    if(t === "ghc-prim:GHC.Prim.Void#") return 0;
    if(t === "ghc-prim:GHC.Prim.Int#") return 1;
    if(t === "ghc-prim:GHC.Prim.Int64#") return 2;
    if(t === "ghc-prim:GHC.Prim.Weak#") return 1;
    if(t === "ghc-prim:GHC.Prim.Addr#") return 2;
    if(t === "ghc-prim:GHC.Prim.Word#") return 1;
    if(t === "ghc-prim:GHC.Prim.Float#") return 1;
    if(t === "ghc-prim:GHC.Prim.Double#") return 1;
    if(t === "ghc-prim:GHC.Prim.ByteArray#") return 2;
    if(t === "ghc-prim:GHC.Prim.ThreadId#") return 1;
    console.warn("unknown argument type: " + t);
    return 1;
  }
  function callStr(r) {
    return r.pattern + '(' + r.arguments.join(', ') + ') -> ' + r.result + ' ' + r.span;
  }
  function checkRef(r) {
    if(r.cconv === "ccall") {
      var f = null;
      try {
        f = eval(r.pattern);
      } catch(e) { }
      if(!f) {
        console.warn("referenced pattern does not exist: " + callStr(r));
        return;
      }
      if(typeof f !== 'function') {
        console.warn("referenced pattern is not a function: " + callStr(r));
        return;
      }
      var s = 0, ba = 0;
      for(var i = 0; i < r.arguments.length; i++) {
        var a = r.arguments[i];
        s += argSize(a);
        ba += a === "ghc-prim:GHC.Prim.ByteArray#" ? 1 : 0;
      }
      if(f.length != s) {
        console.warn("number of arguments does not seem to match: " + callStr(r));
      }
      if(ba !== 0 && f.length === (s - ba)) {
        console.warn("number of arguments matches old ByteArray calling convention: " + callStr(r));
      }
    }
  }
  for(var i=0;i<refs.length;i++) {
    checkRef(refs[i]);
  }
}
var h$GHCConcSignalSignalHandlerStore_d = null;
var h$GHCConcSignalSignalHandlerStore_o = 0;
function h$getOrSetGHCConcSignalSignalHandlerStore(d,o) {
  if(d) {
    h$GHCConcSignalSignalHandlerStore_d = d;
    h$GHCConcSignalSignalHandlerStore_o = o;
  }
  { h$ret1 = (h$GHCConcSignalSignalHandlerStore_o); return (h$GHCConcSignalSignalHandlerStore_d); };
}

// default eventlog writer: does nothing
var h$event_log_writer = (a,o) => {return;}

// redirect the eventlog to stderr
function h$eventlogToStderr() {
  h$event_log_writer = (a,o) => h$errorMsg(h$decodeUtf8z(a,o));
}

function h$traceEvent(ev_v,ev_o) {
  h$event_log_writer(ev_v,ev_o);
}

function h$traceMarker(ev_v,ev_o) {
  h$event_log_writer(ev_v,ev_o);
}

function h$flushEventlog(cap_a,cap_o) {
}

var h$gcMark = 2;
var h$retainCAFs = false;
var h$extensibleRetentionRoots = [];
var h$extensibleRetentionCallbacks = [];
function h$registerExtensibleRetentionRoot(f) {
    h$extensibleRetentionRoots.push(f);
}
function h$unregisterExtensibleRetentionRoot(f) {
    h$extensibleRetentionRoots = h$extensibleRetentionRoots.filter(function(g) { return f !== g; });
}
function h$registerExtensibleRetention(f) {
    h$extensibleRetentionCallbacks.push(f);
}
function h$unregisterExtensibleRetention(f) {
    h$extensibleRetentionCallbacks = h$extensibleRetentionCallbacks.filter(function(g) { return f !== g; });
}
function h$isMarked(obj) {
  return (typeof obj === 'object' || typeof obj === 'function') &&
        ((typeof obj.m === 'number' && (obj.m & 3) === h$gcMark) || (obj.m && typeof obj.m === 'object' && obj.m.m === h$gcMark));
}
function h$gcQuick(t) {
    if(h$currentThread !== null) throw "h$gcQuick: GC can only run when no thread is running";
    h$resetRegisters();
    h$resetResultVars();
    var i;
    if(t !== null) {
        if(t instanceof h$Thread) {
            h$resetThread(t);
        } else {
            for(var i=0;i<t.length;i++) h$resetThread(t[i]);
        }
    } else {
        var nt, runnable = h$threads.iter();
        while((nt = runnable()) !== null) h$resetThread(nt);
        var iter = h$blocked.iter();
        while((nt = iter.next()) !== null) h$resetThread(nt);
    }
}
function h$gc(t) {
    if(h$isGHCJSi()) return;
    if(h$currentThread !== null) throw "h$gc: GC can only be run when no thread is running";
    h$resetRegisters();
    h$resetResultVars();
    h$gcMark = 5-h$gcMark;
    var i;
    for(i=h$extensibleRetentionRoots.length-1;i>=0;i--) {
      var a = h$extensibleRetentionRoots[i](h$gcMark);
      if(a) h$follow(a, a.length-1);
    }
    if(t !== null) {
 h$markThread(t);
 h$resetThread(t);
    }
    var nt, runnable = h$threads.iter();
    while((nt = runnable()) !== null) {
 h$markThread(nt);
 h$resetThread(nt);
    }
    var iter = h$blocked.iter();
    while((nt = iter.next()) !== null) {
        if(nt.delayed ||
    (nt.blockedOn instanceof h$MVar && nt.stack && nt.stack[nt.sp] === h$unboxFFIResult)) {
            h$markThread(nt);
        }
 h$resetThread(nt);
    }
    iter = h$extraRoots.iter();
    while((nt = iter.next()) !== null) h$follow(nt.root);
    for(i=0;i<h$stablePtrData.length;i++) {
      if(h$stablePtrData[i]) h$follow(h$stablePtrData[i]);
    }
    h$resolveDeadlocks();
    var toFinalize = h$markRetained();
    h$finalizeWeaks(toFinalize);
    h$finalizeCAFs();
    var now = Date.now();
    h$lastGc = now;
    h$debugAlloc_verifyReachability(h$gcMark);
}
function h$markWeaks() {
  var i, w, marked, mark = h$gcMark;
  do {
    marked = false;
    for (i = 0; i < h$weakPointerList.length; ++i) {
      w = h$weakPointerList[i];
      if (((w.keym.m & 3) === mark)) {
 if (w.val !== null && !((typeof w.val.m === 'number' && (w.val.m & 3) === mark) || (typeof w.val.m === 'object' && ((w.val.m.m & 3) === mark)))) {
          h$follow(w.val);
   marked = true;
 }
 if (w.finalizer !== null && !((typeof w.finalizer.m === 'number' && (w.finalizer.m & 3) === mark) || (typeof w.finalizer.m === 'object' && ((w.finalizer.m.m & 3) === mark)))) {
          h$follow(w.finalizer);
   marked = true;
 }
      }
    }
  } while(marked);
}
function h$markRetained() {
    var iter, marked, w, i, mark = h$gcMark;
    var newList = [];
    var toFinalize = [];
    do {
        marked = false;
        for (i = 0; i < h$weakPointerList.length; ++i) {
            w = h$weakPointerList[i];
            if (w === null) {
                continue;
            }
            if (((w.keym.m & 3) === mark)) {
                if (w.val !== null && !((typeof w.val.m === 'number' && (w.val.m & 3) === mark) || (typeof w.val.m === 'object' && ((w.val.m.m & 3) === mark)))) {
                    h$follow(w.val);
                }
                if (w.finalizer !== null && !((typeof w.finalizer.m === 'number' && (w.finalizer.m & 3) === mark) || (typeof w.finalizer.m === 'object' && ((w.finalizer.m.m & 3) === mark)))) {
                    h$follow(w.finalizer);
                }
                newList.push(w);
                h$weakPointerList[i] = null;
                marked = true;
            }
        }
    } while(marked);
    for (i = 0; i < h$weakPointerList.length; ++i) {
        w = h$weakPointerList[i];
        if (w === null) {
            continue;
        }
        if(w.val !== null) {
            w.val = null;
        }
        if(w.finalizer !== null) {
            if(!((typeof w.finalizer.m === 'number' && (w.finalizer.m & 3) === mark) || (typeof w.finalizer.m === 'object' && ((w.finalizer.m.m & 3) === mark)))) {
                h$follow(w.finalizer);
            }
            toFinalize.push(w);
        }
    }
    h$weakPointerList = newList;
    return toFinalize;
}
function h$markThread(t) {
    var mark = h$gcMark;
    if(((typeof t.m === 'number' && (t.m & 3) === mark) || (typeof t.m === 'object' && ((t.m.m & 3) === mark)))) return;
    h$follow(t);
}
function h$followObjGen(c, work, w) {
   work[w++] = c.d1;;
   var d = c.d2;
   for(var x in d) {
     work[w++] = d[x];;
   }
    return w;
}
function h$follow(obj, sp) {
    var i, ii, iter, c, work, w;
    var work, mark = h$gcMark;
    if(typeof sp === 'number') {
        work = obj.slice(0, sp+1);
        w = sp + 1;
    } else {
        work = [obj];
        w = 1;
    }
    while(w > 0) {
        c = work[--w];
        if(c !== null && c !== undefined && typeof c === 'object' && ((typeof c.m === 'number' && (c.m&3) !== mark) || (typeof c.m === 'object' && c.m !== null && typeof c.m.m === 'number' && (c.m.m&3) !== mark))) {
            var doMark = false;
            var cf = c.f;
            if(typeof cf === 'function' && (typeof c.m === 'number' || typeof c.m === 'object')) {
                if(typeof c.m === 'number') c.m = (c.m&-4)|mark; else c.m.m = (c.m.m & -4)|mark;;
                var d = c.d2;
                switch(cf.size) {
                case 0: break;
                case 1: work[w++] = c.d1;; break;
                case 2: { work[w++] = c.d1; work[w++] = d; }; break;
                case 3: var d3=c.d2; { work[w++] = c.d1; work[w++] = d3.d1; work[w++] = d3.d2; }; break;
                case 4: var d4=c.d2; { work[w++] = c.d1; work[w++] = d4.d1; work[w++] = d4.d2; work[w++] = d4.d3; }; break;
                case 5: var d5=c.d2; { work[w++] = c.d1; work[w++] = d5.d1; work[w++] = d5.d2; work[w++] = d5.d3; }; work[w++] = d5.d4;; break;
                case 6: var d6=c.d2; { work[w++] = c.d1; work[w++] = d6.d1; work[w++] = d6.d2; work[w++] = d6.d3; }; { work[w++] = d6.d4; work[w++] = d6.d5; }; break;
                case 7: var d7=c.d2; { work[w++] = c.d1; work[w++] = d7.d1; work[w++] = d7.d2; work[w++] = d7.d3; }; { work[w++] = d7.d4; work[w++] = d7.d5; work[w++] = d7.d6; }; break;
                case 8: var d8=c.d2; { work[w++] = c.d1; work[w++] = d8.d1; work[w++] = d8.d2; work[w++] = d8.d3; }; { work[w++] = d8.d4; work[w++] = d8.d5; work[w++] = d8.d6; work[w++] = d8.d7; }; break;
                case 9: var d9=c.d2; { work[w++] = c.d1; work[w++] = d9.d1; work[w++] = d9.d2; work[w++] = d9.d3; }; { work[w++] = d9.d4; work[w++] = d9.d5; work[w++] = d9.d6; work[w++] = d9.d7; }; work[w++] = d9.d8;; break;
                case 10: var d10=c.d2; { work[w++] = c.d1; work[w++] = d10.d1; work[w++] = d10.d2; work[w++] = d10.d3; }; { work[w++] = d10.d4; work[w++] = d10.d5; work[w++] = d10.d6; work[w++] = d10.d7; }; { work[w++] = d10.d8; work[w++] = d10.d9; }; break;
                case 11: var d11=c.d2; { work[w++] = c.d1; work[w++] = d11.d1; work[w++] = d11.d2; work[w++] = d11.d3; }; { work[w++] = d11.d4; work[w++] = d11.d5; work[w++] = d11.d6; work[w++] = d11.d7; }; { work[w++] = d11.d8; work[w++] = d11.d9; work[w++] = d11.d10; }; break;
                case 12: var d12=c.d2; { work[w++] = c.d1; work[w++] = d12.d1; work[w++] = d12.d2; work[w++] = d12.d3; }; { work[w++] = d12.d4; work[w++] = d12.d5; work[w++] = d12.d6; work[w++] = d12.d7; }; { work[w++] = d12.d8; work[w++] = d12.d9; work[w++] = d12.d10; work[w++] = d12.d11; }; break;
                default: w = h$followObjGen(c,work,w);
                }
                var s = cf.s;
                if(s !== null) {
                    for(var i=0;i<s.length;i++) work[w++] = s[i];;
                }
            } else if(typeof c.len === 'number' && c.buf instanceof ArrayBuffer) {
                if(typeof c.m === 'number') c.m = (c.m&-4)|mark; else c.m.m = (c.m.m & -4)|mark;;
            } else if(c instanceof h$Weak) {
                if(typeof c.m === 'number') c.m = (c.m&-4)|mark; else c.m.m = (c.m.m & -4)|mark;;
            } else if(c instanceof h$MVar) {
                if(typeof c.m === 'number') c.m = (c.m&-4)|mark; else c.m.m = (c.m.m & -4)|mark;;
                iter = c.writers.iter();
                while((ii = iter()) !== null) {
      work[w++] = ii[1];;
      work[w++] = ii[0];;
  }
  iter = c.readers.iter();
  while((ii = iter()) !== null) {
      work[w++] = ii;;
  }
         if(c.waiters) {
    for(i=c.waiters.length-1;i>=0;i--) {
      work[w++] = c.waiters[i];;
    }
  }
                if(c.val !== null && !((typeof c.val.m === 'number' && (c.val.m & 3) === mark) || (typeof c.val.m === 'object' && ((c.val.m.m & 3) === mark)))) work[w++] = c.val;;
            } else if(c instanceof h$MutVar) {
                if(typeof c.m === 'number') c.m = (c.m&-4)|mark; else c.m.m = (c.m.m & -4)|mark;;
                work[w++] = c.val;;
            } else if(c instanceof h$TVar) {
                if(typeof c.m === 'number') c.m = (c.m&-4)|mark; else c.m.m = (c.m.m & -4)|mark;;
                work[w++] = c.val;;
  iter = c.blocked.iter();
  while((ii = iter.next()) !== null) {
      work[w++] = ii;;
  }
  if(c.invariants) {
      iter = c.invariants.iter();
      while((ii = iter.next()) !== null) {
   work[w++] = ii;;
      }
  }
            } else if(c instanceof h$Thread) {
                if(typeof c.m === 'number') c.m = (c.m&-4)|mark; else c.m.m = (c.m.m & -4)|mark;;
                if(c.stack) {
                    for(i=c.sp;i>=0;i--) {
   work[w++] = c.stack[i];;
      }
                }
  for(i=0;i<c.excep.length;i++) {
      work[w++] = c.excep[i][0];;
      work[w++] = c.excep[i][1];;
  }
            } else if(c instanceof h$Transaction) {
                if(typeof c.m === 'number') c.m = (c.m&-4)|mark; else c.m.m = (c.m.m & -4)|mark;;
                for(i=c.invariants.length-1;i>=0;i--) {
      work[w++] = c.invariants[i].action;;
  }
                work[w++] = c.action;;
                iter = c.tvars.iter();
                while((ii = iter.nextVal()) !== null) {
      work[w++] = ii.val;;
  }
            } else if(c instanceof Array && c.__ghcjsArray) {
                if(typeof c.m === 'number') c.m = (c.m&-4)|mark; else c.m.m = (c.m.m & -4)|mark;;
                for(i=0;i<c.length;i++) {
                    var x = c[i];
                    if(typeof x === 'object' && x !== null && !((typeof x.m === 'number' && (x.m & 3) === mark) || (typeof x.m === 'object' && ((x.m.m & 3) === mark)))) {
          work[w++] = x;;
      }
                }
            } else if(typeof c === 'object') {
                for(i=h$extensibleRetentionCallbacks.length-1;i>=0;i--) {
                    var x = h$extensibleRetentionCallbacks[i](c, mark);
                    if(x === false) continue;
                    if(x !== true) {
                      for(j=x.length-1;j>=0;j--) {
            work[w++] = x[j];;
        }
                    }
                    break;
                }
            }
        }
    }
}
function h$resetThread(t) {
    var stack = t.stack;
    if(!stack) return;
    var sp = t.sp;
    if(stack.length - sp > sp && stack.length > 100) {
        t.stack = t.stack.slice(0,sp+1);
    } else {
        for(var i=sp+1;i<stack.length;i++) {
            stack[i] = null;
        }
    }
}
function h$resolveDeadlocks() {
    var kill, t, iter, bo, mark = h$gcMark;
    do {
        h$markWeaks();
        kill = null;
        iter = h$blocked.iter();
        while((t = iter.next()) !== null) {
            if(((typeof t.m === 'number' && (t.m & 3) === mark) || (typeof t.m === 'object' && ((t.m.m & 3) === mark)))) continue;
            bo = t.blockedOn;
            if(bo instanceof h$MVar) {
                if(bo.m === mark) throw "assertion failed: thread should have been marked";
                kill = h$ghczminternalZCGHCziInternalziJSziPrimziInternalziblockedIndefinitelyOnMVar;
                break;
            } else if(t.blockedOn instanceof h$TVarsWaiting) {
                kill = h$ghczminternalZCGHCziInternalziJSziPrimziInternalziblockedIndefinitelyOnSTM;
                break;
            } else {
            }
        }
        if(kill) {
            h$killThread(t, kill);
            h$markThread(t);
        }
    } while(kill);
}
function h$addCAF(o) {
  h$CAFs.push(o);
  h$CAFsReset.push([o.f, o.d1, o.d2]);
}
function h$finalizeCAFs() {
    if(h$retainCAFs) return;
    var mark = h$gcMark;
    for(var i=0;i<h$CAFs.length;i++) {
        var c = h$CAFs[i];
        if(c.m & 3 !== mark) {
            var cr = h$CAFsReset[i];
            if(c.f !== cr[0]) {
                c.f = cr[0];
                c.d1 = cr[1];
                c.d2 = cr[2];
            }
        }
    }
}

/*
  set up the google closure library. this is a rather hacky setup
  to make it work with our shims without requiring compilation
  or pulling in the google closure library module loader
 */
var goog = {};
goog.global = h$getGlobal(this);
goog.provide = function() { };
goog.require = function() { };
goog.isDef = function(val) { return val !== undefined; };
goog.inherits = function(childCtor, parentCtor) {
  /** @constructor */
  function tempCtor() {};
  tempCtor.prototype = parentCtor.prototype;
  childCtor.superClass_ = parentCtor.prototype;
  childCtor.prototype = new tempCtor();
  /** @override */
  childCtor.prototype.constructor = childCtor;

  /**
   * Calls superclass constructor/method.
   *
   * This function is only available if you use goog.inherits to
   * express inheritance relationships between classes.
   *
   * NOTE: This is a replacement for goog.base and for superClass_
   * property defined in childCtor.
   *
   * @param {!Object} me Should always be "this".
   * @param {string} methodName The method name to call. Calling
   *     superclass constructor can be done with the special string
   *     'constructor'.
   * @param {...*} var_args The arguments to pass to superclass
   *     method/constructor.
   * @return {*} The return value of the superclass method/constructor.
   */
  childCtor.base = function(me, methodName, var_args) {
    // Copying using loop to avoid deop due to passing arguments object to
    // function. This is faster in many JS engines as of late 2014.
    var args = new Array(arguments.length - 2);
    for (var i = 2; i < arguments.length; i++) {
      args[i - 2] = arguments[i];
    }
    return parentCtor.prototype[methodName].apply(me, args);
  };
};

goog.isString = function(v) {
    return typeof v === 'string';
}

goog.math = {};
goog.crypt = {};



function h$__hscore_sizeof_termios() {
    return 4;
}
function h$tcgetattr(x, y, z) {
    return 0;
}
function h$__hscore_get_saved_termios(r) {
    { h$ret1 = (0); return (null); };
}
function h$__hscore_set_saved_termios(a, b, c) {
    { h$ret1 = (0); return (null); };
}
function h$__hscore_sizeof_sigset_t() {
    return 4;
}
function h$sigemptyset(a, b) {
    { h$ret1 = (0); return (null); };
}
function h$__hscore_sigttou() {
    return 0;
}
function h$sigaddset(a, b, c) {
    return 0;
}
function h$__hscore_sig_block() {
    return 0;
}
function h$sigprocmask(a,b,c,d,e) {
    { h$ret1 = (0); return (0); };
}
function h$__hscore_lflag(a,b) {
    return 0;
}
function h$__hscore_icanon() {
    return 0;
}
function h$__hscore_poke_lflag(a, b, c) {
    return 0;
}
function h$__hscore_ptr_c_cc(a, b) {
    { h$ret1 = (0); return (h$newByteArray(8)); };
}
function h$__hscore_vmin() {
    { h$ret1 = (0); return (h$newByteArray(8)); };
}
function h$__hscore_vtime() {
    return 0;
}
function h$__hscore_tcsanow() {
    return 0;
}
function h$tcsetattr(a,b,c,d) {
    return 0;
}
function h$__hscore_sig_setmask() {
    return 0;
}


function h$MD5Init(ctx, ctx_off) {
  if(!ctx.arr) { ctx.arr = []; }
  ctx.arr[ctx_off] = new goog.crypt.Md5();
}
var h$__hsbase_MD5Init = h$MD5Init;

function h$MD5Update(ctx, ctx_off, data, data_off, len) {
  var arr = new Uint8Array(data.buf, data_off);
  ctx.arr[ctx_off].update(arr, len);
}
var h$__hsbase_MD5Update = h$MD5Update;

function h$MD5Final(dst, dst_off, ctx, ctx_off) {
  var digest = ctx.arr[ctx_off].digest();
  for(var i=0;i<16;i++) {
    dst.u8[dst_off+i] = digest[i];
  }
}
var h$__hsbase_MD5Final = h$MD5Final;


/**************************************************
 * Temporarilyl vendored Closure Library
 **************************************************/


// Copyright 2011 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview MD5 cryptographic hash.
 * Implementation of http://tools.ietf.org/html/rfc1321 with common
 * optimizations and tweaks (see http://en.wikipedia.org/wiki/MD5).
 *
 * Usage:
 *   var md5 = new goog.crypt.Md5();
 *   md5.update(bytes);
 *   var hash = md5.digest();
 *
 * Performance:
 *   Chrome 23              ~680 Mbit/s
 *   Chrome 13 (in a VM)    ~250 Mbit/s
 *   Firefox 6.0 (in a VM)  ~100 Mbit/s
 *   IE9 (in a VM)           ~27 Mbit/s
 *   Firefox 3.6             ~15 Mbit/s
 *   IE8 (in a VM)           ~13 Mbit/s
 *
 */

/**
 * MD5 cryptographic hash constructor.
 * @constructor
 * @extends {goog.crypt.Hash}
 * @final
 * @struct
 */
goog.crypt.Md5 = function() {

  this.blockSize = 512 / 8;

  /**
   * Holds the current values of accumulated A-D variables (MD buffer).
   * @type {!Array<number>}
   * @private
   */
  this.chain_ = new Array(4);

  /**
   * A buffer holding the data until the whole block can be processed.
   * @type {!Array<number>}
   * @private
   */
  this.block_ = new Array(this.blockSize);

  /**
   * The length of yet-unprocessed data as collected in the block.
   * @type {number}
   * @private
   */
  this.blockLength_ = 0;

  /**
   * The total length of the message so far.
   * @type {number}
   * @private
   */
  this.totalLength_ = 0;

  this.reset();
};


/**
 * Integer rotation constants used by the abbreviated implementation.
 * They are hardcoded in the unrolled implementation, so it is left
 * here commented out.
 * @type {Array<number>}
 * @private
 *
goog.crypt.Md5.S_ = [
  7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
  5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
  4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
  6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21
];
 */

/**
 * Sine function constants used by the abbreviated implementation.
 * They are hardcoded in the unrolled implementation, so it is left
 * here commented out.
 * @type {Array<number>}
 * @private
 *
goog.crypt.Md5.T_ = [
  0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
  0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
  0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
  0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
  0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
  0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
  0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
  0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
  0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
  0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
  0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
  0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
  0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
  0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
  0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
  0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
];
 */


/** @override */
goog.crypt.Md5.prototype.reset = function() {
  this.chain_[0] = 0x67452301;
  this.chain_[1] = 0xefcdab89;
  this.chain_[2] = 0x98badcfe;
  this.chain_[3] = 0x10325476;

  this.blockLength_ = 0;
  this.totalLength_ = 0;
};


/**
 * Internal compress helper function. It takes a block of data (64 bytes)
 * and updates the accumulator.
 * @param {Array<number>|Uint8Array|string} buf The block to compress.
 * @param {number=} opt_offset Offset of the block in the buffer.
 * @private
 */
goog.crypt.Md5.prototype.compress_ = function(buf, opt_offset) {
  if (!opt_offset) {
    opt_offset = 0;
  }

  // We allocate the array every time, but it's cheap in practice.
  var X = new Array(16);

  // Get 16 little endian words. It is not worth unrolling this for Chrome 11.
  if (goog.isString(buf)) {
    for (var i = 0; i < 16; ++i) {
      X[i] = (buf.charCodeAt(opt_offset++)) |
             (buf.charCodeAt(opt_offset++) << 8) |
             (buf.charCodeAt(opt_offset++) << 16) |
             (buf.charCodeAt(opt_offset++) << 24);
    }
  } else {
    for (var i = 0; i < 16; ++i) {
      X[i] = (buf[opt_offset++]) |
             (buf[opt_offset++] << 8) |
             (buf[opt_offset++] << 16) |
             (buf[opt_offset++] << 24);
    }
  }

  var A = this.chain_[0];
  var B = this.chain_[1];
  var C = this.chain_[2];
  var D = this.chain_[3];
  var sum = 0;

  /*
   * This is an abbreviated implementation, it is left here commented out for
   * reference purposes. See below for an unrolled version in use.
   *
  var f, n, tmp;
  for (var i = 0; i < 64; ++i) {

    if (i < 16) {
      f = (D ^ (B & (C ^ D)));
      n = i;
    } else if (i < 32) {
      f = (C ^ (D & (B ^ C)));
      n = (5 * i + 1) % 16;
    } else if (i < 48) {
      f = (B ^ C ^ D);
      n = (3 * i + 5) % 16;
    } else {
      f = (C ^ (B | (~D)));
      n = (7 * i) % 16;
    }

    tmp = D;
    D = C;
    C = B;
    sum = (A + f + goog.crypt.Md5.T_[i] + X[n]) & 0xffffffff;
    B += ((sum << goog.crypt.Md5.S_[i]) & 0xffffffff) |
         (sum >>> (32 - goog.crypt.Md5.S_[i]));
    A = tmp;
  }
   */

  /*
   * This is an unrolled MD5 implementation, which gives ~30% speedup compared
   * to the abbreviated implementation above, as measured on Chrome 11. It is
   * important to keep 32-bit croppings to minimum and inline the integer
   * rotation.
   */
  sum = (A + (D ^ (B & (C ^ D))) + X[0] + 0xd76aa478) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[1] + 0xe8c7b756) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[2] + 0x242070db) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[3] + 0xc1bdceee) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[4] + 0xf57c0faf) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[5] + 0x4787c62a) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[6] + 0xa8304613) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[7] + 0xfd469501) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[8] + 0x698098d8) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[9] + 0x8b44f7af) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[10] + 0xffff5bb1) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[11] + 0x895cd7be) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (D ^ (B & (C ^ D))) + X[12] + 0x6b901122) & 0xffffffff;
  A = B + (((sum << 7) & 0xffffffff) | (sum >>> 25));
  sum = (D + (C ^ (A & (B ^ C))) + X[13] + 0xfd987193) & 0xffffffff;
  D = A + (((sum << 12) & 0xffffffff) | (sum >>> 20));
  sum = (C + (B ^ (D & (A ^ B))) + X[14] + 0xa679438e) & 0xffffffff;
  C = D + (((sum << 17) & 0xffffffff) | (sum >>> 15));
  sum = (B + (A ^ (C & (D ^ A))) + X[15] + 0x49b40821) & 0xffffffff;
  B = C + (((sum << 22) & 0xffffffff) | (sum >>> 10));
  sum = (A + (C ^ (D & (B ^ C))) + X[1] + 0xf61e2562) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[6] + 0xc040b340) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[11] + 0x265e5a51) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[0] + 0xe9b6c7aa) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[5] + 0xd62f105d) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[10] + 0x02441453) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[15] + 0xd8a1e681) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[4] + 0xe7d3fbc8) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[9] + 0x21e1cde6) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[14] + 0xc33707d6) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[3] + 0xf4d50d87) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[8] + 0x455a14ed) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (C ^ (D & (B ^ C))) + X[13] + 0xa9e3e905) & 0xffffffff;
  A = B + (((sum << 5) & 0xffffffff) | (sum >>> 27));
  sum = (D + (B ^ (C & (A ^ B))) + X[2] + 0xfcefa3f8) & 0xffffffff;
  D = A + (((sum << 9) & 0xffffffff) | (sum >>> 23));
  sum = (C + (A ^ (B & (D ^ A))) + X[7] + 0x676f02d9) & 0xffffffff;
  C = D + (((sum << 14) & 0xffffffff) | (sum >>> 18));
  sum = (B + (D ^ (A & (C ^ D))) + X[12] + 0x8d2a4c8a) & 0xffffffff;
  B = C + (((sum << 20) & 0xffffffff) | (sum >>> 12));
  sum = (A + (B ^ C ^ D) + X[5] + 0xfffa3942) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[8] + 0x8771f681) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[11] + 0x6d9d6122) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[14] + 0xfde5380c) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[1] + 0xa4beea44) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[4] + 0x4bdecfa9) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[7] + 0xf6bb4b60) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[10] + 0xbebfbc70) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[13] + 0x289b7ec6) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[0] + 0xeaa127fa) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[3] + 0xd4ef3085) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[6] + 0x04881d05) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (B ^ C ^ D) + X[9] + 0xd9d4d039) & 0xffffffff;
  A = B + (((sum << 4) & 0xffffffff) | (sum >>> 28));
  sum = (D + (A ^ B ^ C) + X[12] + 0xe6db99e5) & 0xffffffff;
  D = A + (((sum << 11) & 0xffffffff) | (sum >>> 21));
  sum = (C + (D ^ A ^ B) + X[15] + 0x1fa27cf8) & 0xffffffff;
  C = D + (((sum << 16) & 0xffffffff) | (sum >>> 16));
  sum = (B + (C ^ D ^ A) + X[2] + 0xc4ac5665) & 0xffffffff;
  B = C + (((sum << 23) & 0xffffffff) | (sum >>> 9));
  sum = (A + (C ^ (B | (~D))) + X[0] + 0xf4292244) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[7] + 0x432aff97) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[14] + 0xab9423a7) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[5] + 0xfc93a039) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[12] + 0x655b59c3) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[3] + 0x8f0ccc92) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[10] + 0xffeff47d) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[1] + 0x85845dd1) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[8] + 0x6fa87e4f) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[15] + 0xfe2ce6e0) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[6] + 0xa3014314) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[13] + 0x4e0811a1) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));
  sum = (A + (C ^ (B | (~D))) + X[4] + 0xf7537e82) & 0xffffffff;
  A = B + (((sum << 6) & 0xffffffff) | (sum >>> 26));
  sum = (D + (B ^ (A | (~C))) + X[11] + 0xbd3af235) & 0xffffffff;
  D = A + (((sum << 10) & 0xffffffff) | (sum >>> 22));
  sum = (C + (A ^ (D | (~B))) + X[2] + 0x2ad7d2bb) & 0xffffffff;
  C = D + (((sum << 15) & 0xffffffff) | (sum >>> 17));
  sum = (B + (D ^ (C | (~A))) + X[9] + 0xeb86d391) & 0xffffffff;
  B = C + (((sum << 21) & 0xffffffff) | (sum >>> 11));

  this.chain_[0] = (this.chain_[0] + A) & 0xffffffff;
  this.chain_[1] = (this.chain_[1] + B) & 0xffffffff;
  this.chain_[2] = (this.chain_[2] + C) & 0xffffffff;
  this.chain_[3] = (this.chain_[3] + D) & 0xffffffff;
};


/** @override */
goog.crypt.Md5.prototype.update = function(bytes, opt_length) {
  if (!goog.isDef(opt_length)) {
    opt_length = bytes.length;
  }
  var lengthMinusBlock = opt_length - this.blockSize;

  // Copy some object properties to local variables in order to save on access
  // time from inside the loop (~10% speedup was observed on Chrome 11).
  var block = this.block_;
  var blockLength = this.blockLength_;
  var i = 0;

  // The outer while loop should execute at most twice.
  while (i < opt_length) {
    // When we have no data in the block to top up, we can directly process the
    // input buffer (assuming it contains sufficient data). This gives ~30%
    // speedup on Chrome 14 and ~70% speedup on Firefox 6.0, but requires that
    // the data is provided in large chunks (or in multiples of 64 bytes).
    if (blockLength == 0) {
      while (i <= lengthMinusBlock) {
        this.compress_(bytes, i);
        i += this.blockSize;
      }
    }

    if (goog.isString(bytes)) {
      while (i < opt_length) {
        block[blockLength++] = bytes.charCodeAt(i++);
        if (blockLength == this.blockSize) {
          this.compress_(block);
          blockLength = 0;
          // Jump to the outer loop so we use the full-block optimization.
          break;
        }
      }
    } else {
      while (i < opt_length) {
        block[blockLength++] = bytes[i++];
        if (blockLength == this.blockSize) {
          this.compress_(block);
          blockLength = 0;
          // Jump to the outer loop so we use the full-block optimization.
          break;
        }
      }
    }
  }

  this.blockLength_ = blockLength;
  this.totalLength_ += opt_length;
};


/** @override */
goog.crypt.Md5.prototype.digest = function() {
  // This must accommodate at least 1 padding byte (0x80), 8 bytes of
  // total bitlength, and must end at a 64-byte boundary.
  var pad = new Array((this.blockLength_ < 56 ?
                       this.blockSize :
                       this.blockSize * 2) - this.blockLength_);

  // Add padding: 0x80 0x00*
  pad[0] = 0x80;
  for (var i = 1; i < pad.length - 8; ++i) {
    pad[i] = 0;
  }
  // Add the total number of bits, little endian 64-bit integer.
  var totalBits = this.totalLength_ * 8;
  for (var i = pad.length - 8; i < pad.length; ++i) {
    pad[i] = totalBits & 0xff;
    totalBits /= 0x100; // Don't use bit-shifting here!
  }
  this.update(pad);

  var digest = new Array(16);
  var n = 0;
  for (var i = 0; i < 4; ++i) {
    for (var j = 0; j < 32; j += 8) {
      digest[n++] = (this.chain_[i] >>> j) & 0xff;
    }
  }
  return digest;
};

// Copyright 2011 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

/**
 * @fileoverview Abstract cryptographic hash interface.
 *
 * See goog.crypt.Sha1 and goog.crypt.Md5 for sample implementations.
 *
 */




/**
 * Create a cryptographic hash instance.
 *
 * @constructor
 * @struct
 */
goog.crypt.Hash = function() {
  /**
   * The block size for the hasher.
   * @type {number}
   */
  this.blockSize = -1;
};


/**
 * Resets the internal accumulator.
 */
goog.crypt.Hash.prototype.reset = goog.abstractMethod;


/**
 * Adds a byte array (array with values in [0-255] range) or a string (might
 * only contain 8-bit, i.e., Latin1 characters) to the internal accumulator.
 *
 * Many hash functions operate on blocks of data and implement optimizations
 * when a full chunk of data is readily available. Hence it is often preferable
 * to provide large chunks of data (a kilobyte or more) than to repeatedly
 * call the update method with few tens of bytes. If this is not possible, or
 * not feasible, it might be good to provide data in multiplies of hash block
 * size (often 64 bytes). Please see the implementation and performance tests
 * of your favourite hash.
 *
 * @param {Array<number>|Uint8Array|string} bytes Data used for the update.
 * @param {number=} opt_length Number of bytes to use.
 */
goog.crypt.Hash.prototype.update = goog.abstractMethod;


/**
 * @return {!Array<number>} The finalized hash computed
 *     from the internal accumulator.
 */
goog.crypt.Hash.prototype.digest = goog.abstractMethod;

function h$sti(i,c,xs) {
    i.f = c;
    h$init_closure(i,xs);
}
function h$stc(i,c,xs) {
    i.f = c;
    h$init_closure(i,xs);
    h$addCAF(i);
}
function h$stl(o, xs, t) {
    var r = t ? t : h$ghczmprimZCGHCziTypesziZMZN;
    var x;
    if(xs.length > 0) {
        for(var i=xs.length-1;i>=0;i--) {
            x = xs[i];
            if(!x && x !== false && x !== 0) throw "h$toHsList: invalid element";
            r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (x), (r)));
        }
    }
    o.f = r.f;
    o.d1 = r.d1;
    o.d2 = r.d2;
    o.m = r.m;
}
var h$staticDelayed = [];
function h$d() {
    var c = h$c(null);
    h$staticDelayed.push(c);
    return c;
}
var h$allocN = 0;
function h$traceAlloc(x) {
    h$log("allocating: " + (++h$allocN));
    x.alloc = h$allocN;
}
function h$di(c) {
    h$staticDelayed.push(c);
}
function h$p(x) {
    h$staticDelayed.push(x);
    return x;
}
var h$entriesStack = [];
var h$staticsStack = [];
var h$labelsStack = [];
function h$scheduleInit(entries, objs, lbls, infos, statics) {
    var d = h$entriesStack.length;
    h$entriesStack.push(entries);
    h$staticsStack.push(objs);
    h$labelsStack.push(lbls);
    h$initStatic.push(function() {
        h$initInfoTables(d, entries, objs, lbls, infos, statics);
    });
}
function h$initInfoTables ( depth
                          , funcs
                          , objects
                          , lbls
                          , infoMeta
                          , infoStatic
                          ) {
  var n, i, j, o, pos = 0, info;
  function code(c) {
    if(c < 34) return c - 32;
    if(c < 92) return c - 33;
    return c - 34;
  }
  function next() {
    var c = info.charCodeAt(pos);
    if(c < 124) {
      pos++;
      return code(c);
    }
    if(c === 124) {
      pos+=3;
      var r = 90 + 90 * code(info.charCodeAt(pos-2))
                  + code(info.charCodeAt(pos-1));
      return r;
    }
    if(c === 125) {
      pos+=4;
      var r = 8190 + 8100 * code(info.charCodeAt(pos-3))
                   + 90 * code(info.charCodeAt(pos-2))
                   + code(info.charCodeAt(pos-1));
      return r;
    }
    throw ("h$initInfoTables: invalid code in info table: " + c + " at " + pos)
  }
  function nextCh() {
        return next();
  }
    function nextInt() {
        var n = next();
        var r;
        if(n === 0) {
            var n1 = next();
            var n2 = next();
            r = n1 << 16 | n2;
        } else {
            r = n - 12;
        }
        return r;
    }
    function nextSignificand() {
        var n = next();
        var n1, n2, n3, n4, n5;
        var r;
        if(n < 2) {
            n1 = next();
            n2 = next();
            n3 = next();
            n4 = next();
            n5 = n1 * 281474976710656 + n2 * 4294967296 + n3 * 65536 + n4;
            r = n === 0 ? -n5 : n5;
        } else {
            r = n - 12;
        }
        return r;
    }
    function nextEntry(o) { return nextIndexed("nextEntry", h$entriesStack, o); }
    function nextObj(o) { return nextIndexed("nextObj", h$staticsStack, o); }
    function nextLabel(o) { return nextIndexed("nextLabel", h$labelsStack, o); }
    function nextIndexed(msg, stack, o) {
        var n = (o === undefined) ? next() : o;
        var i = depth;
        while(n >= stack[i].length) {
            n -= stack[i].length;
            i--;
            if(i < 0) throw (msg + ": cannot find item " + n + ", stack length: " + stack.length + " depth: " + depth);
        }
        return stack[i][n];
    }
    function nextArg() {
        var o = next();
        var n, n1, n2, d0, d1, d2, d3;
        var isString = false;
        switch(o) {
        case 0:
            return false;
        case 1:
            return true;
        case 2:
            return 0;
        case 3:
            return 1;
        case 4:
            return nextInt();
        case 5:
            return null;
        case 6:
            n = next();
            switch(n) {
            case 0:
                return -0.0;
            case 1:
                return 0.0;
            case 2:
                return 1/0;
            case 3:
                return -1/0;
            case 4:
                return 0/0;
            case 5:
                n1 = nextInt();
                var ns = nextSignificand();
              if(n1 > 600) {
                return ns * Math.pow(2,n1-600) * Math.pow(2,600);
              } else if(n1 < -600) {
                return ns * Math.pow(2,n1+600) * Math.pow(2,-600);
              } else {
                return ns * Math.pow(2, n1);
              }
            default:
                n1 = n - 36;
                return nextSignificand() * Math.pow(2, n1);
            }
        case 7:
            isString = true;
        case 8:
            n = next();
            var ba = h$newByteArray(isString ? (n+1) : n);
            var b8 = ba.u8;
            if(isString) b8[n] = 0;
            var p = 0;
            while(n > 0) {
                switch(n) {
                case 1:
                    d0 = next();
                    d1 = next();
                    b8[p] = ((d0 << 2) | (d1 >> 4));
                    break;
                case 2:
                    d0 = next();
                    d1 = next();
                    d2 = next();
                    b8[p++] = ((d0 << 2) | (d1 >> 4));
                    b8[p] = ((d1 << 4) | (d2 >> 2));
                    break;
                default:
                    d0 = next();
                    d1 = next();
                    d2 = next();
                    d3 = next();
                    b8[p++] = ((d0 << 2) | (d1 >> 4));
                    b8[p++] = ((d1 << 4) | (d2 >> 2));
                    b8[p++] = ((d2 << 6) | d3);
                    break;
                }
                n -= 3;
            }
            return ba;
        case 9:
            var isFun = next() === 1;
            var lbl = nextLabel();
            return h$initPtrLbl(isFun, lbl);
        case 10:
            var c = { f: nextEntry(), d1: null, d2: null, m: 0 };
            var n = next();
            var args = [];
            while(n--) {
                args.push(nextArg());
            }
            return h$init_closure(c, args);
        default:
            return nextObj(o-11);
        }
    }
    info = infoMeta; pos = 0;
  for(i=0;i<funcs.length;i++) {
    o = funcs[i];
    var ot;
    var oa = 0;
    var oregs = 256;
    switch(next()) {
      case 0:
        ot = 0;
        break;
      case 1:
        ot = 1;
        var arity = next();
        var skipRegs = next()-1;
        if(skipRegs === -1) throw "h$initInfoTables: unknown register info for function";
        var skip = skipRegs & 1;
        var regs = skipRegs >>> 1;
        oregs = (regs << 8) | skip;
        oa = arity + ((regs-1+skip) << 8);
        break;
      case 2:
        ot = 2;
        oa = next();
        break;
      case 3:
        ot = -1;
        oa = 0;
        oregs = next() - 1;
        if(oregs !== -1) oregs = ((oregs >>> 1) << 8) | (oregs & 1);
        break;
      default: throw ("h$initInfoTables: invalid closure type")
    }
    var size = next() - 1;
    var nsrts = next();
    var srt = null;
    if(nsrts > 0) {
      srt = [];
      for(var j=0;j<nsrts;j++) {
          srt.push(nextObj());
      }
    }
    o.t = ot;
    o.i = [];
    o.n = "";
    o.a = oa;
    o.r = oregs;
    o.s = srt;
    o.m = 0;
    o.size = size;
  }
    info = infoStatic;
    pos = 0;
    for(i=0;i<objects.length;i++) {
      o = objects[i];
      var nx = next();
      switch(nx) {
      case 0:
          break;
      case 1:
          o.f = nextEntry();
        n = next();
        if(n === 0) {
          o.d1 = null;
          o.d2 = null;
        } else if(n === 1) {
          o.d1 = nextArg();
          o.d2 = null;
        } else if(n === 2) {
          o.d1 = nextArg();
          o.d2 = nextArg();
        } else {
          for(j=0;j<n;j++) {
            h$setField(o, j, nextArg());
          }
        }
          break;
      case 2:
        o.f = nextEntry();
        n = next();
        if(n === 0) {
          o.d1 = null;
          o.d2 = null;
        } else if(n === 1) {
          o.d1 = nextArg();
          o.d2 = null;
        } else if(n === 2) {
          o.d1 = nextArg();
          o.d2 = nextArg();
        } else {
          for(j=0;j<n;j++) {
            h$setField(o, j, nextArg());
          }
        }
          h$addCAF(o);
          break;
      case 3:
          break;
      case 4:
          break;
      case 5:
          break;
      case 6:
          break;
      case 7:
          n = next();
          var b = h$newByteArray(n);
          for(j=0;j>n;j++) {
              b.u8[j] = next();
          }
          break;
      case 8:
          o.f = h$ghczmprimZCGHCziTypesziZMZN_con_e;
          break;
      case 9:
          n = next();
          var hasTail = next();
          var c = (hasTail === 1) ? nextObj() : h$ghczmprimZCGHCziTypesziZMZN;
          while(n--) {
              c = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (nextArg()), (c)));
          }
          o.f = c.f;
          o.d1 = c.d1;
          o.d2 = c.d2;
          break;
      case 10:
          n = next();
          o.f = nextEntry();
          for(j=0;j<n;j++) {
              h$setField(o, j, nextArg());
          }
          break;
      case 11:
          o.f = nextEntry();
          break;
      case 12:
          o.f = nextEntry();
          o.d1 = nextArg();
          break;
      case 13:
          o.f = nextEntry();
          o.d1 = nextArg();
          o.d2 = nextArg();
          break;
      case 14:
          o.f = nextEntry();
          o.d1 = nextArg();
          o.d2 = { d1: nextArg(), d2: nextArg()};
          break;
      case 15:
          o.f = nextEntry();
          o.d1 = nextArg();
          o.d2 = { d1: nextArg(), d2: nextArg(), d3: nextArg() };
          break;
      case 16:
          o.f = nextEntry();
          o.d1 = nextArg();
          o.d2 = { d1: nextArg(), d2: nextArg(), d3: nextArg(), d4: nextArg() };
          break;
      case 17:
          o.f = nextEntry();
          o.d1 = nextArg();
          o.d2 = { d1: nextArg(), d2: nextArg(), d3: nextArg(), d4: nextArg(), d5: nextArg() };
          break;
      default:
          throw ("invalid static data initializer: " + nx);
      }
  }
  h$staticDelayed = null;
}
function h$initPtrLbl(isFun, lbl) {
    return lbl;
}
function h$callDynamic(f_d,f_o) {
  if (f_d !== h$stablePtrBuf) {
    throw ("callDynamic: expecting a StablePtr and got: " + f_d)
  }
  var f = h$deRefStablePtr(f_o);
  var args = Array.prototype.slice.call(arguments, 2);
  return f.apply(f, args);
}
function h$sliceArray(a, start, n) {
  var r = a.slice(start, start+n);
  r.__ghcjsArray = true;
  r.m = 0;
  return r;
}
function h$copyMutableArray(a1,o1,a2,o2,n) {
  if (n <= 0) return;
  if (o1 < o2) {
    for (var i=n-1;i>=0;i--) {
      a2[o2+i] = a1[o1+i];
    }
  } else {
    for (var i=0;i<n;i++) {
      a2[o2+i] = a1[o1+i];
    }
  }
}
function h$copyMutableByteArray(a1,o1,a2,o2,n) {
  if (n <= 0) return;
  if (o1 < o2) {
    for (var i=n-1;i>=0;i--) {
      a2.u8[o2+i] = a1.u8[o1+i];
    }
  } else {
    for (var i=0;i<n;i++) {
      a2.u8[o2+i] = a1.u8[o1+i];
    }
  }
  if (!a1.arr) return;
  if (!a2.arr) { a2.arr = [] };
  if (o1 < o2) {
    for (var i=n-1;i>=0;i--) {
      a2.arr[o2+i] = a1.arr[o1+i] || null;
    }
  } else {
    for (var i=0;i<n;i++) {
      a2.arr[o2+i] = a1.arr[o1+i] || null;
    }
  }
}
function h$memcpy() {
  if(arguments.length === 3) {
    var dst = arguments[0];
    var src = arguments[1];
    var n = arguments[2];
    for(var i=n-1;i>=0;i--) {
      dst.u8[i] = src.u8[i];
    }
    { h$ret1 = (0); return (dst); };
  } else if(arguments.length === 5) {
    var dst = arguments[0];
    var dst_off = arguments[1]
    var src = arguments[2];
    var src_off = arguments[3];
    var n = arguments[4];
    for(var i=n-1;i>=0;i--) {
      dst.u8[i+dst_off] = src.u8[i+src_off];
    }
    { h$ret1 = (dst_off); return (dst); };
  } else {
    throw "h$memcpy: unexpected argument";
  }
}
function h$setField(o,n,v) {
    if(n > 0 && !o.d2) o.d2 = {};
    switch(n) {
    case 0:
        o.d1 = v;
        return;
    case 1:
        o.d2.d1 = v;
        return;
    case 2:
        o.d2.d2 = v;
        return;
    case 3:
        o.d2.d3 = v;
        return;
    case 4:
        o.d2.d4 = v;
        return;
    case 5:
        o.d2.d5 = v;
        return;
    case 6:
        o.d2.d6 = v;
        return;
    case 7:
        o.d2.d7 = v;
        return;
    case 8:
        o.d2.d8 = v;
        return;
    case 9:
        o.d2.d9 = v;
        return;
    case 10:
        o.d2.d10 = v;
        return;
    case 11:
        o.d2.d11 = v;
        return;
    case 12:
        o.d2.d12 = v;
        return;
    case 13:
        o.d2.d13 = v;
        return;
    case 14:
        o.d2.d14 = v;
        return;
    case 15:
        o.d2.d15 = v;
        return;
    case 16:
        o.d2.d16 = v;
        return;
    case 17:
        o.d2.d17 = v;
        return;
    case 18:
        o.d2.d18 = v;
        return;
    case 19:
        o.d2.d19 = v;
        return;
    case 20:
        o.d2.d20 = v;
        return;
    case 21:
        o.d2.d21 = v;
        return;
    case 22:
        o.d2.d22 = v;
        return;
    case 23:
        o.d2.d23 = v;
        return;
    case 24:
        o.d2.d24 = v;
        return;
    case 25:
        o.d2.d25 = v;
        return;
    case 26:
        o.d2.d26 = v;
        return;
    case 27:
        o.d2.d27 = v;
        return;
    case 28:
        o.d2.d28 = v;
        return;
    case 29:
        o.d2.d29 = v;
        return;
    case 30:
        o.d2.d30 = v;
        return;
    case 31:
        o.d2.d31 = v;
        return;
    case 32:
        o.d2.d32 = v;
        return;
    case 33:
        o.d2.d33 = v;
        return;
    case 34:
        o.d2.d34 = v;
        return;
    case 35:
        o.d2.d35 = v;
        return;
    case 36:
        o.d2.d36 = v;
        return;
    case 37:
        o.d2.d37 = v;
        return;
    case 38:
        o.d2.d38 = v;
        return;
    case 39:
        o.d2.d39 = v;
        return;
    case 40:
        o.d2.d40 = v;
        return;
    case 41:
        o.d2.d41 = v;
        return;
    case 42:
        o.d2.d42 = v;
        return;
    case 43:
        o.d2.d43 = v;
        return;
    case 44:
        o.d2.d44 = v;
        return;
    case 45:
        o.d2.d45 = v;
        return;
    case 46:
        o.d2.d46 = v;
        return;
    case 47:
        o.d2.d47 = v;
        return;
    case 48:
        o.d2.d48 = v;
        return;
    case 49:
        o.d2.d49 = v;
        return;
    case 50:
        o.d2.d50 = v;
        return;
    case 51:
        o.d2.d51 = v;
        return;
    case 52:
        o.d2.d52 = v;
        return;
    case 53:
        o.d2.d53 = v;
        return;
    case 54:
        o.d2.d54 = v;
        return;
    case 55:
        o.d2.d55 = v;
        return;
    case 56:
        o.d2.d56 = v;
        return;
    case 57:
        o.d2.d57 = v;
        return;
    case 58:
        o.d2.d58 = v;
        return;
    case 59:
        o.d2.d59 = v;
        return;
    case 60:
        o.d2.d60 = v;
        return;
    case 61:
        o.d2.d61 = v;
        return;
    case 62:
        o.d2.d62 = v;
        return;
    case 63:
        o.d2.d63 = v;
        return;
    case 64:
        o.d2.d64 = v;
        return;
    case 65:
        o.d2.d65 = v;
        return;
    case 66:
        o.d2.d66 = v;
        return;
    case 67:
        o.d2.d67 = v;
        return;
    case 68:
        o.d2.d68 = v;
        return;
    case 69:
        o.d2.d69 = v;
        return;
    case 70:
        o.d2.d70 = v;
        return;
    case 71:
        o.d2.d71 = v;
        return;
    case 72:
        o.d2.d72 = v;
        return;
    case 73:
        o.d2.d73 = v;
        return;
    case 74:
        o.d2.d74 = v;
        return;
    case 75:
        o.d2.d75 = v;
        return;
    case 76:
        o.d2.d76 = v;
        return;
    case 77:
        o.d2.d77 = v;
        return;
    case 78:
        o.d2.d78 = v;
        return;
    case 79:
        o.d2.d79 = v;
        return;
    case 80:
        o.d2.d80 = v;
        return;
    case 81:
        o.d2.d81 = v;
        return;
    case 82:
        o.d2.d82 = v;
        return;
    case 83:
        o.d2.d83 = v;
        return;
    case 84:
        o.d2.d84 = v;
        return;
    case 85:
        o.d2.d85 = v;
        return;
    case 86:
        o.d2.d86 = v;
        return;
    case 87:
        o.d2.d87 = v;
        return;
    case 88:
        o.d2.d88 = v;
        return;
    case 89:
        o.d2.d89 = v;
        return;
    case 90:
        o.d2.d90 = v;
        return;
    case 91:
        o.d2.d91 = v;
        return;
    case 92:
        o.d2.d92 = v;
        return;
    case 93:
        o.d2.d93 = v;
        return;
    case 94:
        o.d2.d94 = v;
        return;
    case 95:
        o.d2.d95 = v;
        return;
    case 96:
        o.d2.d96 = v;
        return;
    case 97:
        o.d2.d97 = v;
        return;
    case 98:
        o.d2.d98 = v;
        return;
    case 99:
        o.d2.d99 = v;
        return;
    case 100:
        o.d2.d100 = v;
        return;
    case 101:
        o.d2.d101 = v;
        return;
    case 102:
        o.d2.d102 = v;
        return;
    case 103:
        o.d2.d103 = v;
        return;
    case 104:
        o.d2.d104 = v;
        return;
    case 105:
        o.d2.d105 = v;
        return;
    case 106:
        o.d2.d106 = v;
        return;
    case 107:
        o.d2.d107 = v;
        return;
    default:
        o.d2["d"+n] = v;
    }
}
function h$mkSelThunk(r, f, rf) {
  var sn = h$makeStableName(r);
  var res = h$c2(f, r, rf);
  if(sn.sel) {
    sn.sel.push(res);
  } else {
    sn.sel = [res];
  }
  return res;
}
function h$memchr(a_v, a_o, c, n) {
  for(var i=0;i<n;i++) {
    if(a_v.u8[a_o+i] === c) {
      { h$ret1 = (a_o+i); return (a_v); };
    }
  }
  { h$ret1 = (0); return (null); };
}
function h$strlen(a_v, a_o) {
  var i=0;
  while(true) {
    if(a_v.u8[a_o+i] === 0) { return i; }
    i++;
  }
}
function h$newArray(len, e) {
    var r = new Array(len);
    r.__ghcjsArray = true;
    r.m = 0;
    if(e === null) e = r;
    for(var i=0;i<len;i++) r[i] = e;
    return r;
}
function h$roundUpToMultipleOf(n,m) {
  var rem = n % m;
  return rem === 0 ? n : n - rem + m;
}
function h$newByteArray(len) {
  var len0 = Math.max(h$roundUpToMultipleOf(len, 8), 8);
  var buf = new ArrayBuffer(len0);
  return h$wrapByteArray(buf,len);
}
function h$wrapByteArray(buf,len) {
  return { buf: buf
         , len: len
         , i3: new Int32Array(buf)
         , u8: new Uint8Array(buf)
         , u1: new Uint16Array(buf)
         , f3: new Float32Array(buf)
         , f6: new Float64Array(buf)
         , dv: new DataView(buf)
         , arr: []
         , m: 0
         }
}
function h$resizeMutableByteArray(a, n) {
  var r;
  if(a.len == n) {
    r = a;
  } else {
    r = h$newByteArray(n);
    for(var i = n - 1; i >= 0; i--) {
      r.u8[i] = a.u8[i];
    }
  }
  return r
}
function h$shrinkMutableByteArray(a, n) {
  if(a.len !== n) {
    var r = h$newByteArray(n);
    for(var i = n - 1; i >= 0; i--) {
      r.u8[i] = a.u8[i];
    }
    a.buf = r.buf;
    a.len = r.len;
    a.i3 = r.i3;
    a.u8 = r.u8;
    a.u1 = r.u1;
    a.f3 = r.f3;
    a.f6 = r.f6;
    a.dv = r.dv;
  }
}
function h$shrinkMutableCharArray(a, n) {
  a.length = n;
}
function h$compareByteArrays(a1,o1,a2,o2,n) {
  for(var i = 0; i < n; i++) {
    var x = a1.u8[i + o1];
    var y = a2.u8[i + o2];
    if(x < y) return -1;
    if(x > y) return 1;
  }
  return 0;
}
function h$wrapBuffer(buf, unalignedOk, offset, length) {
  if(!unalignedOk && offset && offset % 8 !== 0) {
    throw new Error("h$wrapBuffer: offset not aligned:" + offset);
  }
  if(!buf || !(buf instanceof ArrayBuffer)) {
    throw new Error("h$wrapBuffer: not an ArrayBuffer: " + buf)
  }
  if(!offset) { offset = 0; }
  if(!length || length < 0) { length = buf.byteLength - offset; }
  return { buf: buf
         , len: length
         , i3: (offset%4) ? null : new Int32Array(buf, offset, length >> 2)
         , u8: new Uint8Array(buf, offset, length)
         , u1: (offset%2) ? null : new Uint16Array(buf, offset, length >> 1)
         , f3: (offset%4) ? null : new Float32Array(buf, offset, length >> 2)
         , f6: (offset%8) ? null : new Float64Array(buf, offset, length >> 3)
         , dv: new DataView(buf, offset, length)
         };
}
var h$arrayBufferCounter = 0;
function h$arrayBufferId(a) {
  if (a.__ghcjsArrayBufferId === undefined)
    a.__ghcjsArrayBufferId = h$arrayBufferCounter++;
  return a.__ghcjsArrayBufferId;
}
function h$comparePointer(a1,o1,a2,o2) {
  if (a1 === null) {
    return a2 === null ? 0 : -1;
  } else if (a2 === null) {
    return 1;
  }
  var i1 = h$arrayBufferId(a1.buf);
  var i2 = h$arrayBufferId(a2.buf);
  if (i1 === i2) {
    var bo1 = a1.dv.byteOffset + o1;
    var bo2 = a2.dv.byteOffset + o2;
    return bo1 === bo2 ? 0 : (bo1 < bo2 ? -1 : 1);
  }
  else
    return i1 < i2 ? -1 : 1;
}
var h$stableNameN = 1;
function h$StableName(m) {
  this.m = m;
  this.s = null;
  this.sel = null;
}
var h$stableName_false = new h$StableName(0);
var h$stableName_true = new h$StableName(0);
function h$makeStableName(x) {
  if(x === false) {
    return h$stableName_false;
  } else if(x === true) {
    return h$stableName_true;
  } else if(typeof x === 'number') {
    return x;
  } else if(((typeof(x)==='object')&&(x).f === h$unbox_e)) {
    return ((typeof(x) === 'number')?(x):(x).d1);
  } else if(typeof x === 'object') {
    if(typeof x.m !== 'object') {
      x.m = new h$StableName(x.m);
    }
    return x.m;
  } else {
    throw new Error("h$makeStableName: invalid argument");
  }
}
function h$stableNameInt(s) {
  if(typeof s === 'number') {
    if(s!=s) return 999999;
    var s0 = s|0;
    if(s0 === s) return s0;
    h$convertDouble[0] = s;
    return h$convertInt[0] ^ h$convertInt[1];
  } else {
    var x = s.s;
    if(x === null) {
      x = s.s = h$stableNameN = (h$stableNameN+1)|0;
    }
    return x;
  }
}
function h$eqStableName(s1o,s2o) {
  if(s1o!=s1o && s2o!=s2o) return 1;
  return s1o === s2o ? 1 : 0;
}
function h$malloc(n) {
  { h$ret1 = (0); return (h$newByteArray(n)); };
}
function h$calloc(n,size) {
  { h$ret1 = (0); return (h$newByteArray(n*size)); };
}
function h$free() {
}
function h$memset() {
  var buf_v, buf_off, chr, n;
  buf_v = arguments[0];
  if(arguments.length == 4) {
    buf_off = arguments[1];
    chr = arguments[2];
    n = arguments[3];
  } else if(arguments.length == 3) {
    buf_off = 0;
    chr = arguments[1];
    n = arguments[2];
  } else {
    throw("h$memset: unexpected argument")
  }
  var end = buf_off + n;
  for(var i=buf_off;i<end;i++) {
    buf_v.u8[i] = chr;
  }
  { h$ret1 = (buf_off); return (buf_v); };
}
function h$memcmp(a_v, a_o, b_v, b_o, n) {
  for(var i=0;i<n;i++) {
    var a = a_v.u8[a_o+i];
    var b = b_v.u8[b_o+i];
    var c = a-b;
    if(c !== 0) { return c; }
  }
  return 0;
}
function h$memmove(a_v, a_o, b_v, b_o, n) {
  if(n > 0) {
    var tmp = new Uint8Array(b_v.buf.slice(b_o,b_o+n));
    for(var i=0;i<n;i++) {
      a_v.u8[a_o+i] = tmp[i];
    }
  }
  { h$ret1 = (a_o); return (a_v); };
}
function h$mkPtr(v, o) {
  return (h$c2(h$ghczminternalZCGHCziInternalziPtrziPtr_con_e, (v), (o)));
};
function h$mkFunctionPtr(f) {
  var d = h$newByteArray(4);
  d.arr = [f];
  return d;
}
var h$freeHaskellFunctionPtr = function () {
}
var h$extraRootsN = 0;
var h$extraRoots = new h$Set();
function h$addExtraRoot() {
}
function h$createAdjustor(cconv, stbl_d, stbl_o, lbl_d, lbl_o, typeStr_d, typeStr_o) {
  var func = lbl_d.arr[lbl_o];
  var stbl = h$deRefStablePtr(stbl_o);
  if(typeof func !== 'function') {
    throw new Error("h$createAdjustor: not a function");
  }
  { h$ret1 = (h$makeStablePtr(func.bind(null, stbl_o))); return (h$stablePtrBuf); };
}
function h$makeCallback(f, extraArgs, action) {
    var args = extraArgs.slice(0);
    args.unshift(action);
    var c = function() {
        return f.apply(this, args);
    }
    c._key = ++h$extraRootsN;
    c.root = action;
    h$extraRoots.add(c);
    return c;
}
function h$makeCallbackApply(n, f, extraArgs, fun) {
  var c;
  if(n === 1) {
    c = function(x) {
      var args = extraArgs.slice(0);
      var action = (h$c2(h$ap1_e,(fun),((h$c1(h$ghczminternalZCGHCziInternalziJSziPrimziJSVal_con_e, (x))))));
      args.unshift(action);
      return f.apply(this, args);
    }
  } else if (n === 2) {
    c = function(x,y) {
      var args = extraArgs.slice(0);
      var action = (h$c3(h$ap2_e,(fun),((h$c1(h$ghczminternalZCGHCziInternalziJSziPrimziJSVal_con_e, (x)))),((h$c1(h$ghczminternalZCGHCziInternalziJSziPrimziJSVal_con_e, (y))))));
      args.unshift(action);
      return f.apply(this, args);
    }
  } else if (n === 3) {
    c = function(x,y,z) {
      var args = extraArgs.slice(0);
      var action = (h$c2(h$ap1_e,((h$c3(h$ap2_e,(fun),((h$c1(h$ghczminternalZCGHCziInternalziJSziPrimziJSVal_con_e, (x)))),((h$c1(h$ghczminternalZCGHCziInternalziJSziPrimziJSVal_con_e, (y))))))),((h$c1(h$ghczminternalZCGHCziInternalziJSziPrimziJSVal_con_e, (z))))));
      args.unshift(action);
      return f.apply(this, args);
    }
  } else {
    throw new Error("h$makeCallbackApply: unsupported arity");
  }
  c.root = fun;
  c._key = ++h$extraRootsN;
  h$extraRoots.add(c);
  return c;
}
function h$retain(c) {
  var k = c._key;
  if(typeof k !== 'number') throw new Error("retained object does not have a key");
  if(k === -1) c._key = ++h$extraRootsN;
  h$extraRoots.add(c);
}
function h$release(c) {
  h$extraRoots.remove(c);
}
function h$isInstanceOf(o,c) {
  return o instanceof c;
}
function h$getpagesize() {
  return 4096;
}
var h$MAP_ANONYMOUS = 0x20;
function h$mmap(addr_d, addr_o, len, prot, flags, fd, offset1, offset2) {
  if(flags & h$MAP_ANONYMOUS || fd === -1) {
    { h$ret1 = (0); return (h$newByteArray(len)); };
  } else {
    throw "h$mmap: mapping a file is not yet supported";
  }
}
function h$mprotect(addr_d, addr_o, size, prot) {
  return 0;
}
function h$munmap(addr_d, addr_o, size) {
  if(addr_d && addr_o === 0 && size >= addr_d.len) {
    addr_d.buf = null;
    addr_d.i3 = null;
    addr_d.u8 = null;
    addr_d.u1 = null;
    addr_d.f3 = null;
    addr_d.f6 = null;
    addr_d.dv = null;
  }
  return 0;
}
function h$pdep8(src, mask) {
  var bit, k = 0, dst = 0;
  for(bit=0;bit<8;bit++) {
    if((mask & (1 << bit)) !== 0) {
      dst |= ((src >>> k) & 1) << bit;
      k++;
    }
  }
  return dst;
}
function h$pdep16(src, mask) {
  var bit, k = 0, dst = 0;
  for(bit=0;bit<16;bit++) {
    if((mask & (1 << bit)) !== 0) {
      dst |= ((src >>> k) & 1) << bit;
      k++;
    }
  }
  return dst;
}
function h$pdep32(src, mask) {
  var bit, k = 0, dst = 0;
  for(bit=0;bit<32;bit++) {
    if((mask & (1 << bit)) !== 0) {
      dst |= ((src >>> k) & 1) << bit;
      k++;
    }
  }
  return (dst >>> 0);
}
function h$pdep64(src_b, src_a, mask_b, mask_a) {
 var bit, k = 0, dst_a = 0, dst_b = 0;
 for(bit=0;bit<32;bit++) {
   if((mask_a & (1 << bit)) !== 0) {
     dst_a |= ((src_a >>> k) & 1) << bit;
     k++;
   }
 }
 for(bit=0;bit<32;bit++) {
   if((mask_b & (1 << bit)) !== 0) {
     if(k >= 32) {
       dst_b |= ((src_b >>> (k - 32)) & 1) << bit;
     } else {
       dst_b |= ((src_a >>> k) & 1) << bit;
     }
     k++;
   }
 }
 { h$ret1 = ((dst_a >>> 0)); return ((dst_b >>> 0)); };
}
function h$pext8(src, mask) {
  var bit, k = 0, dst = 0;
  for(bit=0;bit<8;bit++) {
    if((mask & (1 << bit)) !== 0) {
      dst |= ((src >>> bit) & 1) << k;
      k++;
    }
  }
  return dst;
}
function h$pext16(src, mask) {
  var bit, k = 0, dst = 0;
  for(bit=0;bit<16;bit++) {
    if((mask & (1 << bit)) !== 0) {
      dst |= ((src >>> bit) & 1) << k;
      k++;
    }
  }
  return dst;
}
function h$pext32(src, mask) {
  var bit, k = 0, dst = 0;
  for(bit=0;bit<32;bit++) {
    if((mask & (1 << bit)) !== 0) {
      dst |= ((src >>> bit) & 1) << k;
      k++;
    }
  }
  return dst;
}
function h$pext64(src_b, src_a, mask_b, mask_a) {
 var bit, k = 0, dst_a = 0, dst_b = 0;
 for(bit=0;bit<32;bit++) {
   if((mask_a & (1 << bit)) !== 0) {
     dst_a |= ((src_a >>> bit) & 1) << k;
     k++;
   }
 }
 for(bit=0;bit<32;bit++) {
   if((mask_b & (1 << bit)) !== 0) {
     if(k >= 32) {
       dst_b |= ((src_b >>> bit) & 1) << (k-32);
     } else {
       dst_a |= ((src_b >>> bit) & 1) << k;
     }
     k++;
   }
 }
 { h$ret1 = (dst_a); return (dst_b); };
}
function h$checkOverlapByteArray(a1, o1, a2, o2, n) {
  if (n == 0) return true;
  if (a1 !== a2) return true;
  if (o1 === o2) return true;
  if (o1 < o2) return o2 - o1 >= n;
  if (o1 > o2) return o1 - o2 >= n;
  return true;
}
var h$HEAP = null;
function h$initEmscriptenHeap() {
  h$HEAP = h$wrapByteArray(Module.HEAP8.buffer, Module.HEAP8.buffer.byteLength);
}
function h$mkHeapPtr(offset) {
  if (!h$HEAP) {
    throw new Error("h$mkHeapPtr: Emscripten h$HEAP not initialized");
  }
  return {'array':h$HEAP, 'offset': offset};
}
function h$copyToHeap(buf_d, buf_o, tgt, len) {
    if(len === 0) return;
    var u8 = buf_d.u8;
    for(var i=0;i<len;i++) {
        Module.HEAPU8[tgt+i] = u8[buf_o+i];
    }
}
function h$copyFromHeap(src, buf_d, buf_o, len) {
    var u8 = buf_d.u8;
    for(var i=0;i<len;i++) {
        u8[buf_o+i] = Module.HEAPU8[src+i];
    }
}
function h$initHeapBufferLen(buf_d, buf_o, len) {
  var buf_ptr = _malloc(len);
  h$copyToHeap(buf_d, buf_o, buf_ptr, len);
  return buf_ptr;
}
function h$initHeapBuffer(str_d, str_o) {
  if(str_d === null) return null;
  return ptr = h$initHeapBufferLen(str_d, str_o, str_d.len);
}
function h$withOutBufferOnHeap(ptr_d, ptr_o, len, cont) {
  var ptr = _malloc(len);
  h$copyToHeap(ptr_d, ptr_o, ptr, len);
  var ret = cont(ptr);
  h$copyFromHeap(ptr, ptr_d, ptr_o, len);
  _free(ptr);
  return ret;
}
function h$withCBufferOnHeap(str_d, str_o, len, cont) {
    var str = _malloc(len);
    if(str_d !== null) h$copyToHeap(str_d, str_o, str, len);
    var ret = cont(str);
    _free(str);
    return ret;
}
function h$withCStringOnHeap(str_d, str_o, cont) {
  return h$withCBufferOnHeap(str_d, str_o, str_d === null ? 0 : h$strlen(str_d,str_o), cont);
}
function h$derefHeapPtr_addr(offset) {
  var ptr = h$newByteArray(4);
  ptr.u8.set(Module.HEAPU8.subarray(offset, offset+4));
  return ptr.i3[0];
}
function h$putHeapAddr(a,o,offset) {
  if (offset == 0) {
    if (!(a).arr) (a).arr = []; (a).arr[o] = null; (a).dv.setInt32(o,0,true);;
  } else {
    if (!(a).arr) (a).arr = []; (a).arr[o] = h$HEAP; (a).dv.setInt32(o,offset,true);;
  }
}
function h$copyCStringFromHeap(offset) {
  if(offset == 0) return null;
  var len = 0;
  while(HEAPU8[offset+len] !== 0){ len++; };
  var str = h$newByteArray(len+1);
  str.u8.set(HEAPU8.subarray(offset,offset+len+1));
  return str;
}
function h$copyPtrArrayFromHeap(offset,n) {
  var ptr = h$newByteArray(4*n);
  ptr.u8.set(HEAPU8.subarray(offset, offset+4*n));
  return ptr;
}
function h$registerFunPtrOnHeap(funptr_d, funptr_o, ask_ptr, ty, mkfn) {
  if (funptr_o == 0) return 0;
  var fun = h$deRefStablePtr(funptr_o);
  if (ask_ptr) {
    var cb_ptr = getEmptyTableSlot();
    Module.removeFunction(cb_ptr);
    var cb = mkfn(fun,cb_ptr);
    var ptr = Module.addFunction(cb,ty);
    assert(cb_ptr === ptr, "h$registerJSFunPtrOnHeap: got different pointer offsets: " + cb_ptr + " and " + ptr);
    return ptr;
  }
  else {
    var cb = mkfn(fun);
    return Module.addFunction(cb,ty);
  }
}
function h$unregisterFunPtrFromHeap(p) {
  return Module.removeFunction(p);
}

// add exported things to global again, run this after all node modules
/*
var h$glbl = this;
for(p in exports) { 
//  console.log("exporting: " + p);
//  console.log("type: " + (typeof this[p]));
  if(typeof this[p] === 'undefined') {
    h$glbl[p] = exports[p];
  }
}
*/
if(typeof exports !== 'undefined') {
  if(typeof WeakMap === 'undefined' && typeof global !== 'undefined') {
    global.WeakMap = exports.WeakMap;
  }
//  var Map     = exports.Map;
//  var Set     = exports.Set;
}


function h$isFloat (n) {
  return n===+n && n!==(n|0);
}
function h$isInteger (n) {
  return n===+n && n===(n|0);
}
function h$typeOf(o) {
    if (!(o instanceof Object)) {
        if (o == null) {
            return 0;
        } else if (typeof o == 'number') {
            if (h$isInteger(o)) {
                return 1;
            } else {
                return 2;
            }
        } else if (typeof o == 'boolean') {
            return 3;
        } else {
            return 4;
        }
    } else {
        if (Object.prototype.toString.call(o) == '[object Array]') {
            return 5;
        } else if (!o) {
            return 0;
        } else {
            return 6;
        }
    }
}
function h$flattenObj(o) {
    var l = [], i = 0;
    for (var prop in o) {
        l[i++] = [prop, o[prop]];
    }
    return l;
}
function h$buildObject() {
    var r = {}, l = arguments.length;
    for(var i = 0; i < l; i += 2) {
        var k = arguments[i], v = arguments[i+1];
        r[k] = v;
    }
    return r;
}
function h$buildObjectFromList(xs) {
    var r = {}, k, v, t;
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
        xs = ((xs).d2);
        t = ((xs).d2);
        if(((t).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
            k = ((xs).d1);
            v = ((t).d1);
            xs = ((t).d2);
            r[k] = v;
        } else {
            return r;
        }
    }
    return r;
}
function h$buildObjectFromTupList(xs) {
    var r = {};
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 var h = ((xs).d1);
 xs = ((xs).d2);
 r[((((h).d1)).d1)] = ((((h).d2)).d1);
    }
    return r;
}

var h$registerCC = null, h$registerCCS = null, h$setCCS = null;
var h$runProf = function(f) {
    f();
}
if(h$isNode()) {
    (function() {
 try {
            var p = require('ghcjs-profiling');
            if(p.isProfiling()) {
  h$registerCC = p.registerCC;
  h$registerCCS = p.registerCCS;
  h$setCCS = p.setCCS;
  h$runProf = p.runCC;
            }
 } catch(e) {}
    })();
}
var h$cachedCurrentCcs = -1;
function h$reportCurrentCcs() {
    if(h$setCCS) {
        if(h$currentThread) {
            var ccsKey = h$currentThread.ccs._key;
            if(h$cachedCurrentCcs !== ccsKey) {
                h$cachedCurrentCcs = ccsKey;
                h$setCCS(ccsKey);
            }
        } else if(h$cachedCurrentCcs !== -1) {
            h$cachedCurrentCcs = -1;
            h$setCCS(2147483647);
        }
    }
}
var h$ccList = [];
var h$ccsList = [];
var h$CCUnique = 0;
function h$CC(label, module, srcloc, isCaf) {
  this.label = label;
  this.module = module;
  this.srcloc = srcloc;
  this.isCaf = isCaf;
  this._key = h$CCUnique++;
  this.memAlloc = 0;
  this.timeTicks = 0;
  if(h$registerCC) h$registerCC(this._key, label, module + ' ' + srcloc, -1,-1);
  h$ccList.push(this);
}
var h$CCSUnique = 0;
function h$CCS(parent, cc) {
  if (parent !== null && parent.consed.has(cc)) {
    return (parent.consed.get(cc));
  }
  this.consed = new h$Map();
  this.cc = cc;
  this._key = h$CCSUnique++;
  if (parent) {
    this.root = parent.root;
    this.depth = parent.depth + 1;
    this.prevStack = parent;
    parent.consed.put(cc,this);
  } else {
    this.root = this;
    this.depth = 0;
    this.prevStack = null;
  }
  this.prevStack = parent;
  this.sccCount = 0;
  this.timeTicks = 0;
  this.memAlloc = 0;
  this.inheritedTicks = 0;
  this.inheritedAlloc = 0;
  if(h$registerCCS) {
    var x = this, stack = [];
    while(x) { stack.push(x.cc._key); x = x.prevStack; }
    h$registerCCS(this._key, stack);
  }
  h$ccsList.push(this);
}
var h$CC_MAIN = new h$CC("MAIN", "MAIN", "<built-in>", false);
var h$CC_SYSTEM = new h$CC("SYSTEM", "SYSTEM", "<built-in>", false);
var h$CC_GC = new h$CC("GC", "GC", "<built-in>", false);
var h$CC_OVERHEAD = new h$CC("OVERHEAD_of", "PROFILING", "<built-in>", false);
var h$CC_DONT_CARE = new h$CC("DONT_CARE", "MAIN", "<built-in>", false);
var h$CC_PINNED = new h$CC("PINNED", "SYSTEM", "<built-in>", false);
var h$CC_IDLE = new h$CC("IDLE", "IDLE", "<built-in>", false);
var h$CAF_cc = new h$CC("CAF", "CAF", "<built-in>", false);
var h$CCS_MAIN = new h$CCS(null, h$CC_MAIN);
var h$CCS_SYSTEM = new h$CCS(h$CCS_MAIN, h$CC_SYSTEM);
var h$CCS_GC = new h$CCS(h$CCS_MAIN, h$CC_GC);
var h$CCS_OVERHEAD = new h$CCS(h$CCS_MAIN, h$CC_OVERHEAD);
var h$CCS_DONT_CARE = new h$CCS(h$CCS_MAIN, h$CC_DONT_CARE);
var h$CCS_PINNED = new h$CCS(h$CCS_MAIN, h$CC_PINNED);
var h$CCS_IDLE = new h$CCS(h$CCS_MAIN, h$CC_IDLE);
var h$CAF = new h$CCS(h$CCS_MAIN, h$CAF_cc);
function h$pushRestoreCCS() {
    if(h$stack[h$sp] !== h$setCcs_e) {
        h$sp += 2;
        h$stack[h$sp-1] = h$currentThread.ccs;
        h$stack[h$sp] = h$setCcs_e;
    }
}
function h$restoreCCS(ccs) {
    h$currentThread.ccs = ccs;
    h$reportCurrentCcs();
}
function h$enterThunkCCS(ccsthunk) {
  h$currentThread.ccs = ccsthunk;
  h$reportCurrentCcs();
}
function h$enterFunCCS(ccsapp,
                       ccsfn
                       ) {
  if (ccsapp === ccsfn) {
    return;
  }
  if (ccsfn.prevStack === h$CCS_MAIN) {
    return;
  }
  h$currentThread.ccs = h$CCS_OVERHEAD;
  if (ccsfn.root !== ccsapp.root) {
    h$currentThread.ccs = h$appendCCS(ccsapp, ccsfn);
    h$reportCurrentCcs();
    return;
  }
  if (ccsapp.depth > ccsfn.depth) {
    var tmp = ccsapp;
    var dif = ccsapp.depth - ccsfn.depth;
    for (var i = 0; i < dif; i++) {
      tmp = tmp.prevStack;
    }
    h$currentThread.ccs = h$enterFunEqualStacks(ccsapp, tmp, ccsfn);
    h$reportCurrentCcs();
    return;
  }
  if (ccsfn.depth > ccsapp.depth) {
    h$currentThread.ccs = h$enterFunCurShorter(ccsapp, ccsfn, ccsfn.depth - ccsapp.depth);
    h$reportCurrentCcs();
    return;
  }
  h$currentThread.ccs = h$enterFunEqualStacks(ccsapp, ccsapp, ccsfn);
  h$reportCurrentCcs();
}
function h$appendCCS(ccs1, ccs2) {
  if (ccs1 === ccs2) {
    return ccs1;
  }
  if (ccs2 === h$CCS_MAIN || ccs2.cc.isCaf) {
    return ccs1;
  }
  return h$pushCostCentre(h$appendCCS(ccs1, ccs2.prevStack), ccs2.cc);
}
function h$enterFunCurShorter(ccsapp, ccsfn, n) {
  if (n === 0) {
    return h$enterFunEqualStacks(ccsapp, ccsapp, ccsfn);
  } else {
    return h$pushCostCentre(h$enterFunCurShorter(ccsapp, ccsfn.prevStack, n-1), ccsfn.cc);
  }
}
function h$enterFunEqualStacks(ccs0, ccsapp, ccsfn) {
  if (ccsapp === ccsfn) return ccs0;
  return h$pushCostCentre(h$enterFunEqualStacks(ccs0, ccsapp.prevStack, ccsfn.prevStack), ccsfn.cc);
}
function h$pushCostCentre(ccs, cc) {
  if (ccs === null) {
    return new h$CCS(ccs, cc);
  }
  if (ccs.cc === cc) {
    return ccs;
  } else {
    var temp_ccs = h$checkLoop(ccs, cc);
    if (temp_ccs !== null) {
      return temp_ccs;
    }
    return new h$CCS(ccs, cc);
  }
}
function h$checkLoop(ccs, cc) {
  while (ccs !== null) {
    if (ccs.cc === cc)
      return ccs;
    ccs = ccs.prevStack;
  }
  return null;
}
var h$ccsCC_offset = 4;
var h$ccsPrevStackOffset = 8;
var h$ccLabel_offset = 4;
var h$ccModule_offset = 8;
var h$ccsrcloc_offset = 12;
function h$buildCCPtr(o) {
  var cc = h$newByteArray(20);
  if (!(cc).arr) (cc).arr = []; (cc).arr[h$ccLabel_offset] = h$encodeUtf8(o.label); (cc).dv.setInt32(h$ccLabel_offset,0,true);;
  if (!(cc).arr) (cc).arr = []; (cc).arr[h$ccModule_offset] = h$encodeUtf8(o.module); (cc).dv.setInt32(h$ccModule_offset,0,true);;
  if (!(cc).arr) (cc).arr = []; (cc).arr[h$ccsrcloc_offset] = h$encodeUtf8(o.srcloc); (cc).dv.setInt32(h$ccsrcloc_offset,0,true);;
  return cc;
}
function h$buildCCSPtr(o) {
  var ccs = h$newByteArray(16);
  ccs.arr = [];
  if (o.prevStack !== null) {
    ccs.arr[h$ccsPrevStackOffset] = [h$buildCCSPtr(o.prevStack), 0];
  }
  ccs.arr[h$ccsCC_offset] = [h$buildCCPtr(o.cc), 0];
  return ccs;
}
function h$clearCCS(a) {
  throw new Error("ClearCCSOp not implemented");
}
function h$rts_isProfiled() {
  return 0;
}

var h$start = new Date();
function h$rts_eval(action, unbox) {
  return new Promise((accept, reject) =>
    h$run((h$c4(h$ap3_e, (h$ghczminternalZCGHCziInternalziJSziPrimziresolveIO), (x => { accept(unbox(x))}), (e => { reject(new h$HaskellException(e))}), (action))))
  );
}
function h$rts_eval_sync(closure, unbox) {
  var res, status = 0;
  try {
  h$runSync((h$c4(h$ap3_e, (h$ghczminternalZCGHCziInternalziJSziPrimziresolveIO), (x => { status = 1; res = unbox(x); }), (e => { status = 2; res = new h$HaskellException(e); }), (closure))), false);
  } catch(e) { status = 2; res = e; }
  switch(status) {
    case 0: throw new h$HaskellException("internal error");
    case 1: return res;
    default: throw res;
  }
}
function h$rts_apply(f, x) {
  return (h$c2(h$ap1_e,(f),(x)));
}
function h$rts_mkChar(x) { return x|0; }
function h$rts_getChar(x) { return ((typeof(x) === 'number')?(x):(x).d1); }
function h$rts_mkWord(x) { return x>>>0; }
function h$rts_getWord(x) { return ((typeof(x) === 'number')?(x):(x).d1); }
function h$rts_mkInt(x) { return x|0; }
function h$rts_getInt(x) { return ((typeof(x) === 'number')?(x):(x).d1); }
function h$rts_mkInt32(x) { return x|0; }
function h$rts_getInt32(x) { return ((typeof(x) === 'number')?(x):(x).d1); }
function h$rts_mkWord32(x) { return x>>>0; }
function h$rts_getWord32(x) { return ((typeof(x) === 'number')?(x):(x).d1); }
function h$rts_mkInt16(x) { return (x<<16)>>16; }
function h$rts_getInt16(x) { return ((typeof(x) === 'number')?(x):(x).d1); }
function h$rts_mkInt64(x) { throw new Error("rts_mkInt64"); }
function h$rts_getInt64(x) { throw new Error("rts_getInt64"); }
function h$rts_mkWord64(x) { throw new Error("rts_mkWord64"); }
function h$rts_getWord64(x) { throw new Error("rts_getWord64"); }
function h$rts_mkWord16(x) { return x&0xffff; }
function h$rts_getWord16(x) { return ((typeof(x) === 'number')?(x):(x).d1); }
function h$rts_mkInt8(x) { return (x<<24)>>24; }
function h$rts_getInt8(x) { return ((typeof(x) === 'number')?(x):(x).d1); }
function h$rts_mkWord8(x) { return x&0xff; }
function h$rts_getWord8(x) { return ((typeof(x) === 'number')?(x):(x).d1); }
function h$rts_mkFloat(x) { return x; }
function h$rts_getFloat(x) { return x; }
function h$rts_mkDouble(x) { return x; }
function h$rts_getDouble(x) { return x; }
function h$rts_mkBool(x) { return x; }
function h$rts_getBool(x) { return x; }
function h$rts_getUnit(x) { return 0; }
function h$rts_toString(x) {
  var buf;
  if(typeof x === 'object' &&
     typeof x.len === 'number' &&
     x.buf instanceof ArrayBuffer) {
      buf = x;
  } else if(typeof x === 'object' &&
            x.buffer instanceof ArrayBuffer &&
            typeof x.byteOffset === 'number') {
    buf = h$wrapBuffer(x.buffer, true, x.byteOffset, x.byteLength);
  } else if(x instanceof ArrayBuffer) {
    buf = h$wrapBuffer(x, true, 0, x.byteLength);
  } else {
    throw new Error("rts_toString: unsupported value" + x);
  }
  return h$decodeUtf8z(buf);
}
function h$rts_mkPtr(x) {
  var buf, off = 0;
  if(x === null) {
    buf = null;
    off = 0;
  }
  else if(typeof x == 'object' &&
     typeof x.offset == 'number' &&
     typeof x.array !== 'undefined') {
    buf = x.array;
    off = x.offset;
  }
  else if(typeof x == 'string') {
    buf = h$encodeUtf8(x);
    off = 0;
  }
  else if(typeof x == 'object' &&
     typeof x.len == 'number' &&
     x.buf instanceof ArrayBuffer) {
    buf = x;
    off = 0;
  }
  else if (typeof x == 'number' && h$HEAP !== null) {
    if (x == 0) {
      buf = null;
      off = 0;
    }
    else {
      buf = h$HEAP;
      off = x;
    }
  }
  else if(x.isView) {
    buf = h$wrapBuffer(x.buffer, true, 0, x.buffer.byteLength);
    off = x.byteOffset;
  }
  else if (x instanceof ArrayBuffer) {
    buf = h$wrapBuffer(x, true, 0, x.byteLength);
    off = 0;
  }
  else {
    throw new Error ("h$rts_mkPtr: invalid argument: " + x);
  }
  return (h$c2(h$ghczminternalZCGHCziInternalziPtrziPtr_con_e, (buf), (off)));
}
function h$rts_getPtr(x) {
  var arr = x.d1;
  var offset = x.d2;
  return new Uint8Array(arr.buf, offset);
}
function h$rts_mkFunPtr(x) {
  throw new Error("rts_mkFunPtr");
}
function h$rts_getFunPtr(x) {
  throw new Error("rts_getFunPtr");
}
function h$rts_toIO(x) {
  return (h$c2(h$ap1_e,(h$ghczminternalZCGHCziInternalziJSziPrimzitoIO),(x)));
}
function h$rts_evalIO_sync(closure) {
}
async function h$rts_evalIO(closure) {
}
function h$runio(c) {
  return h$c1(h$runio_e, c);
}
function h$runInitStatic() {
  if(h$initStatic.length == 0) return;
  for(var i=h$initStatic.length - 1;i>=0;i--) {
    h$initStatic[i]();
  }
  h$initStatic = [];
}
function h$o(o, typ, a, size, regs, srefs) {
  h$setObjInfo(o, typ, "", [], a, size, regs, srefs);
}
function h$setObjInfo(o, typ, name, fields, a, size, regs, srefs) {
  o.t = typ;
  o.i = fields;
  o.n = name;
  o.a = a;
  o.r = regs;
  o.s = srefs;
  o.m = 0
  o.size = size;
}
var h$gccheckcnt = 0;
function h$gc_check(next) {
  if(++h$gccheckcnt > 1000) {
    for(var i=h$sp+1;i<h$stack.length;i++) {
      h$stack[i] = null;
    }
    h$gccheckcnt = 0;
  }
  return 0;
}
function h$printcl(i) {
  var cl = i.f;
  var d = i.d1;
  var r = "";
  switch(cl.t) {
    case h$ct_thunk:
    r += "thunk";
    break;
    case h$ct_con:
    r += "con[" + cl.a + "]";
    break;
    case h$ct_fun:
    r += "fun[" + cl.a + "]";
    break;
    default:
    r += "unknown closure type";
    break;
  }
  r += " :: " + cl.n + " ->";
  var idx = 1;
  for(var i=0;i<cl.i.length;i++) {
    r += " ";
    switch(cl.i[i]) {
      case h$vt_ptr:
      r += "[ Ptr :: " + d["d"+idx] + "]";
      idx++;
      break;
      case h$vt_void:
      r += "void";
      break;
      case h$vt_double:
      r += "(" + d["d"+idx] + " :: double)";
      idx++;
      break;
      case h$vt_int:
      r += "(" + d["d"+idx] + " :: int)";
      idx++;
      break;
      case h$vt_long:
      r += "(" + d["d"+idx] + "," + d["d"+(idx+1)] + " :: long)";
      idx+=2;
      break;
      case h$vt_addr:
      r += "(" + d["d"+idx].length + "," + d["d"+(idx+1)] + " :: ptr)";
      idx+=2;
      break;
      default:
      r += "unknown field: " + cl.i[i];
    }
  }
}
function h$init_closure(c, xs) {
  c.m = 0;
  switch(xs.length) {
    case 0:
    c.d1 = null; c.d2 = null;
    return c;
    case 1:
    c.d1 = xs[0]; c.d2 = null;
    return c;
    case 2:
    c.d1 = xs[0]; c.d2 = xs[1];
    return c;
    case 3:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2] };
    return c;
    case 4:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3] };
    return c;
    case 5:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4] };
    return c;
    case 6:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4], d5: xs[5] };
    return c;
    case 7:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4], d5: xs[5], d6: xs[6] };
    return c;
    default:
    c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4], d5: xs[5], d6: xs[6] };
    for(var i=7;i<xs.length;i++) {
      c.d2["d"+i] = xs[i];
    }
    return c;
  }
}
function h$checkStack(f) {
  if(f.t === h$ct_stackframe) h$stack[h$sp] = f;
  var idx = h$sp;
  while(idx >= 0) {
    f = h$stack[idx];
    var size, offset;
    if(typeof(f) === 'function') {
      if(f === h$ap_gen) {
        size = (h$stack[idx - 1] >> 8) + 2;
        offset = 2;
      } else {
        var tag = h$stack[idx].size;
        if(tag <= 0) {
          size = h$stack[idx-1];
          offset = 2;
        } else {
          size = (tag & 0xff) + 1;
          offset = 1;
        }
      }
      idx = idx - size;
    } else {
      h$dumpStackTop(h$stack, 0, h$sp);
      throw("invalid stack object at: " + idx);
    }
  }
}
function h$printReg(r) {
  if(r === null) {
    return "null";
  } else if(typeof r === 'object' && r.hasOwnProperty('f') && r.hasOwnProperty('d1') && r.hasOwnProperty('d2')) {
    if(typeof(r.f) !== 'function') {
      return "dodgy object";
    } else if(r.f.t === h$ct_blackhole && r.x) {
      return ("blackhole: -> " + h$printReg({ f: r.x.x1, d: r.d1.x2 }) + ")");
    } else {
      return ((r.alloc ? r.alloc + ': ' : '') + r.f.n + " (" + h$closureTypeName(r.f.t) + ", " + r.f.a + ")");
    }
  } else if(typeof r === 'object') {
    var res = h$collectProps(r);
    if(res.length > 40) {
      return (res.substr(0,40)+"...");
    } else {
      return res;
    }
  } else {
    var xs = new String(r) + "";
    if(xs.length > 40) {
      return xs.substr(0,40)+"...";
    } else {
      return xs;
    }
  }
}
function h$stackFrameSize(f) {
  if(f === h$ap_gen) {
    return (h$stack[h$sp - 1] >> 8) + 2;
  } else {
    var tag = f.size;
    if(tag < 0) {
      return h$stack[h$sp-1];
    } else {
      return (tag & 0xff) + 1;
    }
  }
}
function h$throw(e, async) {
  var origSp = h$sp;
  var lastBh = null;
  var f;
  while(h$sp > 0) {
    f = h$stack[h$sp];
    if(f === null || f === undefined) {
      throw("h$throw: invalid object while unwinding stack");
    }
    if(f === h$catch_e) break;
    if(f === h$atomically_e) {
      if(async) {
        h$currentThread.transaction = null;
      } else if(!h$stmValidateTransaction()) {
      h$sp++;
      h$stack[h$sp] = h$checkInvariants_e;
      return h$stmStartTransaction(h$stack[h$sp-1]);
    }
  }
  if(f === h$catchStm_e && !async) break;
  if(f === h$upd_frame) {
    var t = h$stack[h$sp-1];
    var waiters = t.d2;
    if(waiters !== null) {
      for(var i=0;i<waiters.length;i++) {
        h$wakeupThread(waiters[i]);
      }
    }
    if(async) {
      if(lastBh === null) {
        h$makeResumable(t,h$sp+1,origSp,[]);
      } else {
        h$makeResumable(t,h$sp+1,lastBh-2,[h$ap_0_0,h$stack[lastBh-1],h$return]);
      }
      lastBh = h$sp;
    } else {
      t.f = h$raise_e;
      t.d1 = e;
      t.d2 = null;
    }
  }
  var size = h$stackFrameSize(f);
  h$sp = h$sp - size;
}
if(h$sp > 0) {
  var maskStatus = h$stack[h$p - 2];
  var handler = h$stack[h$sp - 1];
  if(f === h$catchStm_e) {
    h$currentThread.transaction = h$stack[h$sp-3];
    h$sp -= 4;
  } else if(h$sp > 3) {
  h$sp -= 3;
}
h$r1 = handler;
h$r2 = e;
if(f !== h$catchStm_e) {
if(maskStatus === 0 && h$stack[h$sp] !== h$maskFrame && h$stack[h$sp] !== h$maskUnintFrame) {
  h$stack[h$sp+1] = h$unmaskFrame;
  h$sp += 1;
} else if(maskStatus === 1) {
  h$stack[h$sp+1] = h$maskUnintFrame;
  h$sp += 1;
}
h$currentThread.mask = 2;
}
return h$ap_2_1_fast();
} else {
  throw "unhandled exception in haskell thread";
}
}
function h$logStack() {
  if(typeof h$stack[h$sp] === 'undefined') {
    h$log("warning: invalid stack frame");
    return;
  }
  var size = 0;
  var gt = h$stack[h$sp].size;
  if(gt === -1) {
    size = h$stack[h$sp - 1] & 0xff;
  } else {
    size = gt & 0xff;
  }
  h$dumpStackTop(h$stack, h$sp-size-2, h$sp);
  for(var i=Math.max(0,h$sp-size+1); i <= h$sp; i++) {
    if(typeof h$stack[i] === 'undefined') {
      throw "undefined on stack";
    }
  }
}
function h$ascii(s) {
  var res = [];
  for(var i=0;i<s.length;i++) {
    res.push(s.charCodeAt(i));
  }
  res.push(0);
  return res;
}
function h$dumpStackTop(stack, start, sp) {
  start = Math.max(start,0);
  for(var i=start;i<=sp;i++) {
    var s = stack[i];
    if(s && s.n) {
      h$log("stack[" + i + "] = " + s.n);
    } else {
      if(s === null) {
        h$log("stack[" + i + "] = null WARNING DANGER");
      } else if(typeof s === 'object' && s !== null && s.hasOwnProperty("f") && s.hasOwnProperty("d1") && s.hasOwnProperty("d2")) {
        if(typeof(s.f) !== 'function') {
          h$log("stack[" + i + "] = WARNING: dodgy object");
        } else {
          if(s.d1 === undefined) { h$log("WARNING: stack[" + i + "] d1 undefined"); }
          if(s.d2 === undefined) { h$log("WARNING: stack[" + i + "] d2 undefined"); }
          if(s.f.t === h$ct_blackhole && s.d1 && s.d1.x1 && s.d1.x1.n) {
            h$log("stack[" + i + "] = blackhole -> " + s.d1.x1.n);
          } else {
            h$log("stack[" + i + "] = -> " + (s.alloc ? s.alloc + ': ' : '') + s.f.n + " (" + h$closureTypeName(s.f.t) + ", a: " + s.f.a + ")");
          }
        }
      } else if(h$isInstanceOf(s,h$MVar)) {
        var val = s.val ===
        null ? " empty"
        : " value -> " + (typeof s.val === 'object' ? s.val.f.n + " (" + h$closureTypeName(s.val.f.t) + ", a: " + s.val.f.a + ")" : s.val);
        h$log("stack[" + i + "] = MVar " + val);
      } else if(h$isInstanceOf(s,h$MutVar)) {
        h$log("stack[" + i + "] = IORef -> " + (typeof s.val === 'object' ? (s.val.f.n + " (" + h$closureTypeName(s.val.f.t) + ", a: " + s.val.f.a + ")") : s.val));
      } else if(Array.isArray(s)) {
        h$log("stack[" + i + "] = " + ("[" + s.join(",") + "]").substring(0,50));
      } else if(typeof s === 'object') {
        h$log("stack[" + i + "] = " + h$collectProps(s).substring(0,50));
      } else if(typeof s === 'function') {
        var re = new RegExp("([^\\n]+)\\n(.|\\n)*");
        h$log("stack[" + i + "] = " + (""+s).substring(0,50).replace(re,"$1"));
      } else {
        h$log("stack[" + i + "] = " + (""+s).substring(0,50));
      }
    }
  }
}
function h$checkObj(obj) {
  if(typeof obj === 'boolean' || typeof obj === 'number') { return; }
  if(!obj.hasOwnProperty("f") ||
  obj.f === null ||
  obj.f === undefined ||
  obj.f.a === undefined ||
  typeof obj.f !== 'function') {
    h$log("h$checkObj: WARNING, something wrong with f:");
    h$log((""+obj).substring(0,200));
    h$log(h$collectProps(obj));
    h$log(typeof obj.f);
  }
  if(!obj.hasOwnProperty("d1") || obj.d1 === undefined) {
    h$log("h$checkObj: WARNING, something wrong with d1:");
    h$log((""+obj).substring(0,200));
  } else if(!obj.hasOwnProperty("d2") || obj.d2 === undefined) {
    h$log("h$checkObj: WARNING, something wrong with d2:");
    h$log((""+obj).substring(0,200));
  } else if(obj.d2 !== null && typeof obj.d2 === 'object' && obj.f.size !== 2) {
    var d = obj.d2;
    for(var p in d) {
      if(d.hasOwnProperty(p)) {
        if(p.substring(0,1) != "d") {
          h$log("h$checkObj: WARNING, unexpected field name: " + p);
          h$log((""+obj).substring(0,200));
        }
        if(d[p] === undefined) {
          h$log("h$checkObj: WARNING, undefined field detected: " + p);
          h$log((""+obj).substring(0,200));
        }
      }
    }
    switch(obj.f.size) {
      case 6: if(d.d5 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d5"); }
      case 5: if(d.d4 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d4"); }
      case 4: if(d.d3 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d3"); }
      case 3: if(d.d2 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d2"); }
      if(d.d1 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d1"); }
      default: d = obj.d2;
    }
  }
}
function h$traceForeign(f, as) {
  if(!h$rts_traceForeign) { return; }
  var bs = [];
  for(var i=0;i<as.length;i++) {
    var ai = as[i];
    if(ai === null) {
      bs.push("null");
    } else if(typeof ai === 'object') {
      var astr = ai.toString();
      if(astr.length > 40) {
        bs.push(astr.substring(0,40)+"...");
      } else {
        bs.push(astr);
      }
    } else {
      bs.push(""+ai);
    }
  }
  h$log("ffi: " + f + "(" + bs.join(",") + ")");
}
function h$papArity(cp) {
  return cp.d2.d1;
}
function h$suspendCurrentThread(next) {
  if(next === h$reschedule) { throw "suspend called with h$reschedule"; }
  if(next.t === h$ct_stackframe) h$stack[h$sp] = next;
  if(h$stack[h$sp] === h$restoreThread || next === h$return) {
    h$currentThread.sp = h$sp;
    return;
  }
  var nregs;
  var skipregs = 0;
  var t = next.t;
  if(t === h$ct_pap) {
    nregs = (h$papArity(h$r1) >> 8) + 1;
  } else if(t === h$ct_fun || t === h$ct_stackframe) {
    nregs = next.r >> 8;
    skipregs = next.r & 0xff;
  } else {
    nregs = 1;
  }
  h$sp = h$sp+nregs+skipregs+3;
  var i;
  for(i=1;i<=skipregs;i++) {
    h$stack[h$sp-2-i] = null;
  }
  for(i=skipregs+1;i<=nregs+skipregs;i++) {
    h$stack[h$sp-2-i] = h$getReg(i);
  }
  h$stack[h$sp-2] = next;
  h$stack[h$sp-1] = nregs+skipregs+3;
  h$stack[h$sp] = h$restoreThread;
  h$currentThread.sp = h$sp;
}
function h$static_thunk(f) {
  var h;
  if(!h$rts_profiling) {
    h = { f: f, d1: null, d2: null, m: 0 };
  } else {
    h = { f: f, d1: null, d2: null, m: 0, cc: h$CCS_SYSTEM };
  }
  h$CAFs.push(h);
  h$CAFsReset.push(f);
  return h;
}
function h$catch(a, handler) {
  h$sp += 3;
  h$stack[h$sp-2] = h$currentThread.mask;
  h$stack[h$sp-1] = handler;
  h$stack[h$sp] = h$catch_e;
  h$r1 = a;
  return h$ap_1_0_fast();
}
function h$keepAlive(x, f) {
  h$sp += 2;
  h$stack[h$sp-1] = x;
  h$stack[h$sp] = h$keepAlive_e;
  h$r1 = f;
  return h$ap_1_0_fast();
}

var h$stablePtrData = [null];
var h$stablePtrBuf = h$newByteArray(8);
var h$stablePtrN = 1;
var h$stablePtrFree = [];
function h$makeStablePtr(v) {
  if(!v) return 0;
  var slot = h$stablePtrFree.pop();
  if(slot === undefined) {
    slot = h$stablePtrN++;
  }
  h$stablePtrData[slot] = v;
  return slot << 2;
}
var h$foreignExports = [];
function h$foreignExport(f, packageName, moduleName, functionName, typeSig) {
  h$foreignExports.push({ exported: f,
                          package: packageName,
                          mod: moduleName,
                          name: functionName,
                          sig: typeSig
                        });
  h$makeStablePtr(f);
  if(typeof exports === 'object') {
    if(typeof exports[functionName] === 'undefined') {
      exports[functionName] = f;
    }
  }
}
function h$deRefStablePtr(stable_o) {
  var slot = stable_o >> 2;
  return h$stablePtrData[slot];
}
function h$hs_free_stable_ptr(stable_d, stable_o) {
  var slot = stable_o >> 2;
  if(h$stablePtrData[slot] !== null) {
    h$stablePtrData[slot] = null;
    h$stablePtrFree.push(slot);
  }
}
function h$addrToAny(addr_v, addr_o) {
  var slot = addr_o >> 2;
  return h$stablePtrData[slot];
}

var h$static_pointer_table = null;
var h$static_pointer_table_keys = null;
function h$hs_spt_insert(key1,key2,key3,key4,ref) {
    if(!h$static_pointer_table) {
 h$static_pointer_table = [];
 h$static_pointer_table_keys = [];
    }
    if(!h$hs_spt_lookup_key(key1,key2,key3,key4)) {
        var ba = h$newByteArray(16);
        ba.i3[0] = key2;
        ba.i3[1] = key1;
        ba.i3[2] = key4;
        ba.i3[3] = key3;
 h$static_pointer_table_keys.push(ba);
        h$retain({ root: ref, _key: -1 });
    }
    var s = h$static_pointer_table;
    if(!s[key1]) s[key1] = [];
    if(!s[key1][key2]) s[key1][key2] = [];
    if(!s[key1][key2][key3]) s[key1][key2][key3] = [];
    s[key1][key2][key3][key4] = ref;
}
function h$hs_spt_key_count() {
    return h$static_pointer_table_keys ?
              h$static_pointer_table_keys.length : 0;
}
function h$hs_spt_keys(tgt_d, tgt_o, n) {
    var ks = h$static_pointer_table_keys;
    for(var i=0;(i<n&&i<ks.length);i++) {
      if (!(tgt_d).arr) (tgt_d).arr = []; (tgt_d).arr[tgt_o+4*i] = ks[i]; (tgt_d).dv.setInt32(tgt_o+4*i,0,true);;
    }
    return Math.min(n,ks.length);
}
function h$hs_spt_lookup(key_v,key_o) {
    var key2 = key_v.i3[0] >>> 0;
    var key1 = key_v.i3[1] >>> 0;
    var key4 = key_v.i3[2] >>> 0;
    var key3 = key_v.i3[3] >>> 0;
    { h$ret1 = (0); return (h$hs_spt_lookup_key(key1,key2,key3,key4)); };
}
function h$hs_spt_lookup_key(key1,key2,key3,key4) {
    var s = h$static_pointer_table;
    if(s && s[key1] && s[key1][key2] && s[key1][key2][key3] &&
       s[key1][key2][key3][key4]) return s[key1][key2][key3][key4];
    return null;
}

var h$stmTransactionActive = 0;
var h$stmTransactionWaiting = 4;
function h$Transaction(o, parent) {
    this.action = o;
    this.tvars = new h$Map();
    this.accessed = parent===null?new h$Map():parent.accessed;
    this.parent = parent;
    this.state = h$stmTransactionActive;
    this.m = 0;
}
function h$WrittenTVar(tv,v) {
    this.tvar = tv;
    this.val = v;
}
var h$TVarN = 0;
function h$TVar(v) {
    this.val = v;
    this.blocked = new h$Set();
    this.m = 0;
    this._key = ++h$TVarN;
}
function h$TVarsWaiting(s) {
  this.tvars = s;
}
function h$LocalTVar(v) {
  this.readVal = v.val;
  this.val = v.val;
  this.tvar = v;
}
function h$atomically(o) {
  h$p2(o, h$atomically_e);
  return h$stmStartTransaction(o);
}
function h$stmStartTransaction(o) {
  var t = new h$Transaction(o, null);
  h$currentThread.transaction = t;
  h$r1 = o;
  return h$ap_1_0_fast();
}
function h$stmCommitTransaction() {
    var t = h$currentThread.transaction;
    var tvs = t.tvars;
    var wtv, i = tvs.iter();
    if(t.parent === null) {
        var thread, threadi, blockedThreads = new h$Set();
        while((wtv = i.nextVal()) !== null) {
     h$stmCommitTVar(wtv.tvar, wtv.val, blockedThreads);
 }
        threadi = blockedThreads.iter();
        while((thread = threadi.next()) !== null) {
     h$stmRemoveBlockedThread(thread.blockedOn, thread);
            h$wakeupThread(thread);
 }
    } else {
        var tpvs = t.parent.tvars;
        while((wtv = i.nextVal()) !== null) tpvs.put(wtv.tvar, wtv);
    }
    h$currentThread.transaction = t.parent;
}
function h$stmValidateTransaction() {
    var ltv, i = h$currentThread.transaction.accessed.iter();
    while((ltv = i.nextVal()) !== null) {
        if(ltv.readVal !== ltv.tvar.val) return false;
    }
    return true;
}
function h$stmAbortTransaction() {
  h$currentThread.transaction = h$currentThread.transaction.parent;
}
function h$stmRetry() {
  while(h$sp > 0) {
    var f = h$stack[h$sp];
    if(f === h$atomically_e || f === h$stmCatchRetry_e) {
      break;
    }
    var size;
    if(f === h$ap_gen) {
      size = ((h$stack[h$sp-1] >> 8) + 2);
    } else {
      var tag = f.gtag;
      if(tag < 0) {
        size = h$stack[h$sp-1];
      } else {
        size = (tag & 0xff) + 1;
      }
    }
    h$sp -= size;
  }
  if(h$sp > 0) {
    if(f === h$atomically_e) {
      return h$stmSuspendRetry();
    } else {
      var b = h$stack[h$sp-1];
      h$stmAbortTransaction();
      h$sp -= 2;
      h$r1 = b;
      return h$ap_1_0_fast();
    }
  } else {
    throw "h$stmRetry: STM retry outside a transaction";
  }
}
function h$stmSuspendRetry() {
    var tv, i = h$currentThread.transaction.accessed.iter();
    var tvs = new h$Set();
    while((tv = i.next()) !== null) {
        tv.blocked.add(h$currentThread);
        tvs.add(tv);
    }
    var waiting = new h$TVarsWaiting(tvs);
    h$currentThread.interruptible = true;
    h$p2(waiting, h$stmResumeRetry_e);
    return h$blockThread(h$currentThread, waiting);
}
function h$stmCatchRetry(a,b) {
    h$currentThread.transaction = new h$Transaction(b, h$currentThread.transaction);
    h$p2(b, h$stmCatchRetry_e);
    h$r1 = a;
    return h$ap_1_0_fast();
}
function h$catchStm(a,handler) {
    h$p4(h$currentThread.transaction, h$currentThread.mask, handler, h$catchStm_e);
    h$currentThread.transaction = new h$Transaction(handler, h$currentThread.transaction);
    h$r1 = a;
    return h$ap_1_0_fast();
}
function h$newTVar(v) {
  return new h$TVar(v);
}
function h$readTVar(tv) {
  return h$readLocalTVar(h$currentThread.transaction,tv);
}
function h$readTVarIO(tv) {
  return tv.val;
}
function h$writeTVar(tv, v) {
  h$setLocalTVar(h$currentThread.transaction, tv, v);
}
function h$sameTVar(tv1, tv2) {
  return tv1 === tv2;
}
function h$readLocalTVar(t, tv) {
  var t0 = t;
  while(t0 !== null) {
    var v = t0.tvars.get(tv);
    if(v !== null) {
      return v.val;
    }
    t0 = t0.parent;
  }
  var lv = t.accessed.get(tv);
  if(lv !== null) {
    return lv.val;
  } else {
    t.accessed.put(tv, new h$LocalTVar(tv));
    return tv.val;
  }
}
function h$setLocalTVar(t, tv, v) {
    if(!t.accessed.has(tv)) t.accessed.put(tv, new h$LocalTVar(tv));
    if(t.tvars.has(tv)) {
        t.tvars.get(tv).val = v;
    } else {
        t.tvars.put(tv, new h$WrittenTVar(tv, v));
    }
}
function h$stmCommitTVar(tv, v, threads) {
    if(v !== tv.val) {
        var thr, iter = tv.blocked.iter();
        while((thr = iter.next()) !== null) threads.add(thr);
        tv.blocked.clear();
        tv.val = v;
    }
}
function h$stmRemoveBlockedThread(s, thread) {
    var tv, i = s.tvars.iter();
    while((tv = i.next()) !== null) {
        tv.blocked.remove(thread);
    }
}

function h$str(s) {
  var enc = null;
  return function() {
    if(enc === null) {
      enc = h$encodeModifiedUtf8(s);
    }
    return enc;
  }
}
function h$pstr(s) {
  var enc = null;
  return function() {
    if(enc === null) {
      enc = h$encodePackedUtf8(s);
    }
    return enc;
  }
}
function h$rstr(d) {
  var enc = null;
  return function() {
    if(enc === null) {
      enc = h$rawStringData(d);
    }
    return enc;
  }
}
function h$strt(str) { return (h$c1(h$lazy_e, (function() { return h$toHsString(str); }))); }
function h$strta(str) { return (h$c1(h$lazy_e, (function() { return h$toHsStringA(str); }))); }
function h$strtb(arr) { return (h$c1(h$lazy_e, (function() { return h$toHsStringMU8(arr); }))); }
function h$ustra(str) { return h$toHsStringA(str); }
function h$ustr(str) { return h$toHsString(str); }
function h$urstra(arr) { return h$toHsList(arr); }
function h$urstr(arr) { return h$toHsStringMU8(arr); }
function h$caseMapping(x) {
    return (x%2)?-((x+1)>>1):(x>>1);
}
var h$toUpper = null;
function h$u_towupper(ch) {
    if(h$toUpper == null) { h$toUpper = h$decodeMapping(h$toUpperMapping, h$caseMapping); }
    return ch+(h$toUpper[ch]|0);
}
var h$toLower = null;
function h$u_towlower(ch) {
    if(h$toLower == null) { h$toLower = h$decodeMapping(h$toLowerMapping, h$caseMapping); }
    return ch+(h$toLower[ch]|0);
}
var h$toTitle = null;
function h$u_towtitle(ch) {
    if(h$toTitle == null) { h$toTitle = h$decodeMapping(h$toTitleMapping, h$caseMapping); }
    return ch+(h$toTitle[ch]|0);
}
var h$alpha = null;
function h$u_iswalpha(a) {
    if(h$alpha == null) { h$alpha = h$decodeRLE(h$alphaRanges); }
    return h$alpha[a]|0;
}
var h$alnum = null;
function h$u_iswalnum(a) {
  if(h$alnum == null) { h$alnum = h$decodeRLE(h$alnumRanges); }
  return h$alnum[a] == 1 ? 1 : 0;
}
function h$isSpace(a) {
    if(a<5760) return a===32||(a>=9&&a<=13)||a===160;
    return (a>=8192&&a<=8202)||a===5760||a===8239||a===8287||a===12288;
}
function h$u_iswspace(a) {
    return h$isSpace(a)?1:0;
}
var h$lower = null;
function h$u_iswlower(a) {
    if(h$lower == null) { h$lower = h$decodeRLE(h$lowerRanges); }
    if(a < 0x30000) return h$lower[a]|0;
    if(a < 0xE0000) return 0;
    return h$lower[a-0xB0000]|0;
}
var h$upper = null;
function h$u_iswupper(a) {
    if(h$upper == null) { h$upper = h$decodeRLE(h$upperRanges); }
    if(a < 0x30000) return h$upper[a]|0;
    if(a < 0xE0000) return 0;
    return h$upper[a-0xB0000]|0;
}
var h$cntrlChars = [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159];
var h$cntrl = null;
function h$u_iswcntrl(a) {
    if(h$cntrl === null) {
        h$cntrl = [];
        for(var i=0;i<=159;i++) h$cntrl[i] = (h$cntrlChars.indexOf(i) !== -1) ? 1 : 0;
    }
    return a <= 159 ? h$cntrl[a] : 0;
}
var h$print = null;
function h$u_iswprint(a) {
    if(h$print == null) { h$print = h$decodeRLE(h$printRanges); }
    if(a < 0x30000) return h$print[a]|0;
    if(a < 0xE0000) return 0;
    return h$print[a-0xB0000]|0;
}
function h$decodePacked(s) {
    function f(o) {
        var c = s.charCodeAt(o);
        return c<34?c-32:c<92?c-33:c-34;
    }
    var r=[], i=0;
    while(i < s.length) {
        var c = s.charCodeAt(i);
        if(c < 124) r.push(f(i++));
        else if(c === 124) {
            i += 3; r.push(90+90*f(i-2)+f(i-1));
        } else if(c === 125) {
            i += 4;
            r.push(8190+8100*f(i-3)+90*f(i-2)+f(i-1));
        } else throw ("h$decodePacked: invalid: " + c);
    }
    return r;
}
function h$decodeRLE(str) {
    var r = [], x = 0, i = 0, j = 0, v, k, a = h$decodePacked(str);
    while(i < a.length) {
        v = a[i++];
        if(v === 0) {
            k = a[i++];
            while(k--) {
                r[j++] = x;
                r[j++] = 1-x;
            }
        } else {
            if(v <= 2) {
                k = (a[i]<<16)+a[i+1];
                i+=2;
            } else k = (v-1)>>1;
            if(v%2) {
                r[j++] = x;
                x = 1-x;
            }
            while(k--) r[j++] = x;
            x = 1-x;
        }
    }
    r.shift();
    return r;
}
function h$decodeMapping(str, f) {
    var r = [], i = 0, j = 0, k, v, v2, a = h$decodePacked(str);
    while(i < a.length) {
        v = a[i++];
        if(v === 0) {
            k = a[i];
            v = f(a[i+1]);
            v2 = f(a[i+2]);
            while(k--) {
                r[j++] = v;
                r[j++] = v2;
            }
            i+=3;
        } else {
            if(v === 2) {
                k = (a[i] << 16) + a[i+1];
                v = a[i+2];
                i += 3;
            } else if(v%2) {
                k = 1;
                v = v>>1;
            } else {
                k = (v>>1)-1;
                v = a[i++];
            }
            v = f(v);
            while(k--) r[j++] = v;
        }
    }
    return r;
}
var h$unicodeCat = null;
function h$u_gencat(a) {
    if(h$unicodeCat == null) h$unicodeCat = h$decodeMapping(h$catMapping, function(x) { return x; });
    if(a >= 0xE000 && a <= 0xF8FF || a >= 0xF0000 & a <= 0xFFFFD || a >= 0x100000 && a <= 0x10FFFD) return 28;
    var c = a < 0x30000 ? (h$unicodeCat[a]|0) :
        (a < 0xE0000 ? 0 : (h$unicodeCat[a-0xB0000]|0));
    return c?c-1:29;
}
function h$localeEncoding() {
   { h$ret1 = (0); return (h$encodeUtf8("UTF-8")); };
}
function h$wcwidth(wch) {
  return 1;
}
function h$rawStringData(str) {
    var v = h$newByteArray(str.length+1);
    var u8 = v.u8;
    for(var i=0;i<str.length;i++) {
       u8[i] = str[i];
    }
    u8[str.length] = 0;
    return v;
}
function h$encodeUtf8(str) {
  return h$encodeUtf8Internal(str, false, false);
}
function h$encodeModifiedUtf8(str) {
  return h$encodeUtf8Internal(str, true, false);
}
function h$encodePackedUtf8(str) {
  return h$encodeUtf8Internal(str, false, true);
}
function h$encodeUtf8Internal(str, modified, packed) {
  var i, j, c, low, b64bytes, b64chars;
  function base64val(cc) {
    if(cc >= 65 && cc <= 90) return cc - 65;
    if(cc >= 97 && cc <= 122) return cc - 71;
    if(cc >= 48 && cc <= 57) return cc + 4;
    if(cc === 43) return 62;
    if(cc === 47) return 63;
    if(cc === 61) return 0;
    throw new Error("invalid base64 value: " + cc);
  }
  var n = 0;
  var czescape = false;
  for(i=0;i<str.length;i++) {
    var c = str.charCodeAt(i);
    if (0xD800 <= c && c <= 0xDBFF) {
      low = str.charCodeAt(i+1);
      c = ((c - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;
      i++;
    }
    if(czescape) {
      if(c === 26) {
        n+=1;
      } else if(c === 0) {
        n+=2
      } else if(c >= 0x20 && c <= 0x9f) {
        b64bytes = c - 0x1f;
        b64chars = ((b64bytes + 2) / 3) << 2;
        n += b64bytes;
        i += b64chars;
      } else {
        throw new Error("invalid cz escaped character: " + c);
      }
      czescape = false;
    } else {
      if(c === 26 && packed) {
        czescape = true;
      } else if(c === 0 && modified) {
        n+=2;
      } else if(c <= 0x7F) {
        n++;
      } else if(c <= 0x7FF) {
        n+=2;
      } else if(c <= 0xFFFF) {
        n+=3;
      } else if(c <= 0x1FFFFF) {
        n+=4;
      } else if(c <= 0x3FFFFFF) {
        n+=5;
      } else {
        n+=6;
      }
    }
  }
  var v = h$newByteArray(n+1);
  var u8 = v.u8;
  n = 0;
  for(i=0;i<str.length;i++) {
    c = str.charCodeAt(i);
    if (0xD800 <= c && c <= 0xDBFF) {
      low = str.charCodeAt(i+1);
      c = ((c - 0xD800) * 0x400) + (low - 0xDC00) + 0x10000;
      i++;
    }
    if(packed && !czescape && c === 26) {
      czescape = true;
    } else if(c === 0 && (modified || czescape)) {
      u8[n] = 192;
      u8[n+1] = 128;
      n+=2;
      czescape = false;
    } else if(czescape) {
      if(c >= 0x20 && c <= 0x9f) {
        b64bytes = c - 0x1f;
        while(b64bytes > 0) {
          var c1 = base64val(str.charCodeAt(i+1)),
              c2 = base64val(str.charCodeAt(i+2)),
              c3 = base64val(str.charCodeAt(i+3)),
              c4 = base64val(str.charCodeAt(i+4));
          i+=4;
          u8[n] = (c1<<2)|(c2>>4);
          n++;
          if(b64bytes >= 2) {
            u8[n] = ((c2&0xf)<<4)|(c3 >> 2);
            n++;
          }
          if(b64bytes >= 3) {
            u8[n] = ((c3&0x3)<<6)|c4;
            n++;
          }
          b64bytes -= 3;
        }
      } else {
        u8[n] = c;
        n++;
      }
      czescape = false;
    } else if(c <= 0x7F) {
      u8[n] = c;
      n++;
    } else if(c <= 0x7FF) {
      u8[n] = (c >> 6) | 0xC0;
      u8[n+1] = (c & 0x3F) | 0x80;
      n+=2;
    } else if(c <= 0xFFFF) {
      u8[n] = (c >> 12) | 0xE0;
      u8[n+1] = ((c >> 6) & 0x3F) | 0x80;
      u8[n+2] = (c & 0x3F) | 0x80;
      n+=3;
    } else if(c <= 0x1FFFFF) {
      u8[n] = (c >> 18) | 0xF0;
      u8[n+1] = ((c >> 12) & 0x3F) | 0x80;
      u8[n+2] = ((c >> 6) & 0x3F) | 0x80;
      u8[n+3] = (c & 0x3F) | 0x80;
      n+=4;
    } else if(c <= 0x3FFFFFF) {
      u8[n] = (c >> 24) | 0xF8;
      u8[n+1] = ((c >> 18) & 0x3F) | 0x80;
      u8[n+2] = ((c >> 12) & 0x3F) | 0x80;
      u8[n+3] = ((c >> 6) & 0x3F) | 0x80;
      u8[n+4] = (c & 0x3F) | 0x80;
      n+=5;
    } else {
      u8[n] = (c >>> 30) | 0xFC;
      u8[n+1] = ((c >> 24) & 0x3F) | 0x80;
      u8[n+2] = ((c >> 18) & 0x3F) | 0x80;
      u8[n+3] = ((c >> 12) & 0x3F) | 0x80;
      u8[n+4] = ((c >> 6) & 0x3F) | 0x80;
      u8[n+5] = (c & 0x3F) | 0x80;
      n+=6;
    }
  }
  u8[v.len-1] = 0;
  return v;
}
function h$encodeUtf16(str) {
  var n = 0;
  var i;
  for(i=0;i<str.length;i++) {
    var c = str.charCodeAt(i);
    if(c <= 0xFFFF) {
      n += 2;
    } else {
      n += 4;
    }
  }
  var v = h$newByteArray(n+1);
  var dv = v.dv;
  n = 0;
  for(i=0;i<str.length;i++) {
    var c = str.charCodeAt(i);
    if(c <= 0xFFFF) {
      dv.setUint16(n, c, true);
      n+=2;
    } else {
      var c0 = c - 0x10000;
      dv.setUint16(n, c0 >> 10, true);
      dv.setUint16(n+2, c0 & 0x3FF, true);
      n+=4;
    }
  }
  dv.setUint8(v.len-1,0);
  return v;
}
function h$decodeUtf16l(v, byteLen, start) {
  var a = [];
  for(var i=0;i<byteLen;i+=2) {
    a[i>>1] = v.dv.getUint16(i+start,true);
  }
  return h$charCodeArrayToString(arr);
}
var h$dU16 = h$decodeUtf16;
function h$decodeUtf8z(v,start) {
  var n = start;
  var max = v.len;
  while(n < max) {
    if(v.u8[n] === 0) { break; }
    n++;
  }
  return h$decodeUtf8(v,n,start);
}
function h$decodeUtf8(v,n0,start) {
  var n = n0 || v.len;
  var arr = [];
  var i = start || 0;
  var code;
  var u8 = v.u8;
  while(i < n) {
    var c = u8[i];
    while((c & 0xC0) === 0x80) {
      c = u8[++i];
    }
    if((c & 0x80) === 0) {
      code = (c & 0x7F);
      i++;
    } else if((c & 0xE0) === 0xC0) {
      code = ( ((c & 0x1F) << 6)
             | (u8[i+1] & 0x3F)
             );
      i+=2;
    } else if((c & 0xF0) === 0xE0) {
      code = ( ((c & 0x0F) << 12)
             | ((u8[i+1] & 0x3F) << 6)
             | (u8[i+2] & 0x3F)
             );
      i+=3;
    } else if ((c & 0xF8) === 0xF0) {
      code = ( ((c & 0x07) << 18)
             | ((u8[i+1] & 0x3F) << 12)
             | ((u8[i+2] & 0x3F) << 6)
             | (u8[i+3] & 0x3F)
             );
      i+=4;
    } else if((c & 0xFC) === 0xF8) {
      code = ( ((c & 0x03) << 24)
             | ((u8[i+1] & 0x3F) << 18)
             | ((u8[i+2] & 0x3F) << 12)
             | ((u8[i+3] & 0x3F) << 6)
             | (u8[i+4] & 0x3F)
             );
      i+=5;
    } else {
      code = ( ((c & 0x01) << 30)
             | ((u8[i+1] & 0x3F) << 24)
             | ((u8[i+2] & 0x3F) << 18)
             | ((u8[i+3] & 0x3F) << 12)
             | ((u8[i+4] & 0x3F) << 6)
             | (u8[i+5] & 0x3F)
             );
      i+=6;
    }
    if(code > 0xFFFF) {
      var offset = code - 0x10000;
      arr.push(0xD800 + (offset >> 10), 0xDC00 + (offset & 0x3FF));
    } else {
      arr.push(code);
    }
  }
  return h$charCodeArrayToString(arr);
}
function h$decodeUtf16(v) {
  var n = v.len;
  var arr = [];
  var dv = v.dv;
  for(var i=0;i<n;i+=2) {
    arr.push(dv.getUint16(i,true));
  }
  return h$charCodeArrayToString(arr);
}
function h$charCodeArrayToString(arr) {
    if(arr.length <= 60000) {
 return String.fromCharCode.apply(this, arr);
    }
    var r = '';
    for(var i=0;i<arr.length;i+=60000) {
 r += String.fromCharCode.apply(this, arr.slice(i, i+60000));
    }
    return r;
}
function h$hs_iconv_open(to,to_off,from,from_off) {
  h$errno = h$EINVAL;
  return -1;
}
function h$hs_iconv_close(iconv) {
  return 0;
}
function h$toHsString(str) {
  if(typeof str !== 'string') return h$ghczmprimZCGHCziTypesziZMZN;
  var i = str.length - 1;
  var r = h$ghczmprimZCGHCziTypesziZMZN;
  while(i>=0) {
    var cp = str.charCodeAt(i);
    if(cp >= 0xDC00 && cp <= 0xDFFF && i > 0) {
      --i;
      cp = (cp - 0xDC00) + (str.charCodeAt(i) - 0xD800) * 1024 + 0x10000;
    }
    r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (cp), (r)));
    --i;
  }
  return r;
}
function h$fromHsString(str) {
    var xs = '';
    while(((str).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
 var h = ((str).d1);
 xs += String.fromCodePoint(((typeof(h) === 'number')?(h):(h).d1));
        str = ((str).d2);
    }
    return xs;
}
function h$fromHsListJSVal(xs) {
    var arr = [];
    while(((xs).f === h$ghczmprimZCGHCziTypesziZC_con_e)) {
        arr.push(((((xs).d1)).d1));
        xs = ((xs).d2);
    }
    return arr;
}
function h$toHsStringA(str) {
    if(typeof str !== 'string') return h$ghczmprimZCGHCziTypesziZMZN;
    var i = str.length - 1;
    var r = h$ghczmprimZCGHCziTypesziZMZN;
    while(i>=0) {
 r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (str.charCodeAt(i)), (r)));
 --i;
    }
    return r;
}
function h$toHsStringMU8(arr) {
    var i = arr.length - 1, accept = false, b, n = 0, cp = 0, r = h$ghczmprimZCGHCziTypesziZMZN;
    while(i >= 0) {
        b = arr[i];
        if(!(b & 128)) {
            cp = b;
            accept = true;
        } else if((b & 192) === 128) {
            cp += (b & 32) * Math.pow(64, n)
        } else {
            cp += (b&((1<<(6-n))-1)) * Math.pow(64, n);
            accept = true;
        }
        if(accept) {
            r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (cp), (r)));
            cp = 0
            n = 0;
        } else {
            n++;
        }
        accept = false;
        i--;
    }
    return r;
}
function h$toHsList(arr) {
  var r = h$ghczmprimZCGHCziTypesziZMZN;
  for(var i=arr.length-1;i>=0;i--) {
    r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (arr[i]), (r)));
  }
  return r;
}
function h$toHsListJSVal(arr) {
    var r = h$ghczmprimZCGHCziTypesziZMZN;
    for(var i=arr.length-1;i>=0;i--) {
 r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, ((h$c1(h$ghczminternalZCGHCziInternalziJSziPrimziJSVal_con_e, (arr[i])))), (r)));
    }
    return r;
}
function h$appendToHsStringA(str, appendTo) {
  var i = str.length - 1;
  var r = i == 0 ? appendTo : h$c1(h$upd_thunk_e,(appendTo));
  while(i>=0) {
    r = (h$c2(h$ghczmprimZCGHCziTypesziZC_con_e, (str.charCodeAt(i)), (r)));
    --i;
  }
  return r;
}
function h$throwJSException(e) {
  var strVal;
  if(typeof e === 'string') {
    strVal = e;
  } else if(e instanceof Error) {
    strVal = e.toString() + '\n' + e.stack;
  } else {
    strVal = "" + e;
  }
  var someE = (h$c2(h$ghczminternalZCGHCziInternalziExceptionziTypeziSomeException_con_e,(h$ghczminternalZCGHCziInternalziJSziPrimzizdfExceptionJSException),((h$c2(h$ghczminternalZCGHCziInternalziJSziPrimziJSException_con_e,((h$c1(h$ghczminternalZCGHCziInternalziJSziPrimziJSVal_con_e, (e)))),(h$toHsString(strVal)))))));
  return h$throw(someE, true);
}

var h$threadIdN = 0;
var h$threads = new h$Queue();
var h$blocked = new h$Set();
function h$Thread() {
    this.tid = ++h$threadIdN;
    this.status = (0);
    this.stack = [h$done, 0, h$ghczminternalZCGHCziInternalziConcziSynczireportError, h$catch_e];
    this.sp = 3;
    this.mask = 0;
    this.interruptible = false;
    this.excep = [];
    this.delayed = false;
    this.blockedOn = null;
    this.retryInterrupted = null;
    this.transaction = null;
    this.noPreemption = false;
    this.isSynchronous = false;
    this.continueAsync = false;
    this.m = 0;
    this.result = null;
    this.resultIsException = false;
    this._key = this.tid;
}
function h$rts_getThreadId(t) {
  { h$ret1 = ((t.tid & 0xFFFFFFFF)>>>0); return ((t.tid / Math.pow(2,32))>>>0); };
}
function h$cmp_thread(t1,t2) {
  if(t1.tid < t2.tid) return -1;
  if(t1.tid > t2.tid) return 1;
  return 0;
}
function h$threadString(t) {
  if(t === null) {
    return "<no thread>";
  } else if(t.label) {
    var str = h$decodeUtf8z(t.label, 0);
    return str + " (" + t.tid + ")";
  } else {
    return (""+t.tid);
  }
}
function h$getThreadLabel(t) {
  if (t.label) {
    { h$ret1 = (t.label); return (1); };
  } else {
    { h$ret1 = (0); return (0); };
  }
}
function h$listThreads() {
  var r = h$newArray(0,null);
  var t;
  if (h$currentThread) r.push(h$currentThread);
  var threads_iter = h$threads.iter();
  while ((t = threads_iter()) !== null) r.push(t);
  var blocked_iter = h$blocked.iter();
  while ((t = blocked_iter.next()) !== null) r.push(t);
  return r;
}
function h$fork(a, inherit) {
  h$r1 = h$forkThread(a, inherit);
  return h$yield();
}
function h$forkThread(a, inherit) {
  var t = new h$Thread();
  if(inherit && h$currentThread) {
    t.mask = h$currentThread.mask;
  }
  t.stack[4] = h$ap_1_0;
  t.stack[5] = a;
  t.stack[6] = h$return;
  t.sp = 6;
  h$wakeupThread(t);
  return t;
}
function h$threadStatus(t) {
  { h$ret1 = (0); h$ret2 = (1); return (t.status); };
}
function h$waitRead(fd) {
  h$fds[fd].waitRead.push(h$currentThread);
  h$currentThread.interruptible = true;
  return h$blockThread(h$currentThread,fd,[h$waitRead,fd]);
}
function h$waitWrite(fd) {
  h$fds[fd].waitWrite.push(h$currentThread);
  h$currentThread.interruptible = true;
  return h$blockThread(h$currentThread,fd,[h$waitWrite,fd]);
}
var h$delayed = new h$HeapSet();
function h$wakeupDelayed(now) {
    while(h$delayed.size() > 0 && h$delayed.peekPrio() < now) {
        var t = h$delayed.pop();
        if(t.delayed) {
            t.delayed = false;
            h$wakeupThread(t);
        }
    }
}
function h$delayThread(time) {
  var ms = time/1000;
  return h$delayThreadMs(ms);
}
function h$sleep(secs) {
  return h$delayThreadMs(secs*1000);
}
function h$delayThreadMs(ms) {
  var now = Date.now();
  h$delayed.add(now+ms, h$currentThread);
  h$currentThread.delayed = true;
  h$currentThread.interruptible = true;
  return h$blockThread(h$currentThread, h$delayed,[h$resumeDelayThread]);
}
function h$resumeDelayThread() {
  h$r1 = false;
  return h$rs();
}
function h$yield() {
  if(h$currentThread.isSynchronous) {
    return h$stack[h$sp];
  } else {
    h$sp += 2;
    h$stack[h$sp-1] = h$r1;
    h$stack[h$sp] = h$return;
    h$currentThread.sp = h$sp;
    return h$reschedule;
  }
}
function h$killThread(t, ex) {
  if(t === h$currentThread) {
    h$sp += 2;
    h$stack[h$sp-1] = h$r1;
    h$stack[h$sp] = h$return;
    return h$throw(ex,true);
  } else {
    if(t.mask === 0 || (t.mask === 2 && t.interruptible)) {
      if(t.stack) {
        h$forceWakeupThread(t);
        t.sp += 2;
        t.stack[t.sp-1] = ex;
        t.stack[t.sp] = h$raiseAsync_frame;
      }
      return h$stack ? h$stack[h$sp] : null;
    } else {
      t.excep.push([h$currentThread,ex]);
      if(h$currentThread) {
        h$currentThread.interruptible = true;
        h$sp += 2;
        h$stack[h$sp-1] = h$r1;
        h$stack[h$sp] = h$return;
        return h$blockThread(h$currentThread,t,null);
      } else {
        return null;
      }
    }
  }
}
function h$maskStatus() {
  return h$currentThread.mask;
}
function h$maskAsync(a) {
  if(h$currentThread.mask !== 2) {
    if(h$currentThread.mask === 0 && h$stack[h$sp] !== h$maskFrame && h$stack[h$sp] !== h$maskUnintFrame) {
      h$stack[++h$sp] = h$unmaskFrame;
    }
    if(h$currentThread.mask === 1) {
      h$stack[++h$sp] = h$maskUnintFrame;
    }
    h$currentThread.mask = 2;
  }
  h$r1 = a;
  return h$ap_1_0_fast();
}
function h$maskUnintAsync(a) {
  if(h$currentThread.mask !== 1) {
    if(h$currentThread.mask === 2) {
      h$stack[++h$sp] = h$maskFrame;
    } else {
      h$stack[++h$sp] = h$unmaskFrame;
    }
    h$currentThread.mask = 1;
  }
  h$r1 = a;
  return h$ap_1_0_fast();
}
function h$unmaskAsync(a) {
  if(h$currentThread.excep.length > 0) {
    h$currentThread.mask = 0;
    h$sp += 3;
    h$stack[h$sp-2] = h$ap_1_0;
    h$stack[h$sp-1] = a;
    h$stack[h$sp] = h$return;
    return h$reschedule;
  }
  if(h$currentThread.mask !== 0) {
    if(h$stack[h$sp] !== h$unmaskFrame) {
      if(h$currentThread.mask === 2) {
        h$stack[++h$sp] = h$maskFrame;
      } else {
        h$stack[++h$sp] = h$maskUnintFrame;
      }
    }
    h$currentThread.mask = 0;
  }
  h$r1 = a;
  return h$ap_1_0_fast();
}
function h$pendingAsync() {
  var t = h$currentThread;
  return (t.excep.length > 0 && (t.mask === 0 || (t.mask === 2 && t.interruptible)));
}
function h$postAsync(alreadySuspended,next) {
    var t = h$currentThread;
    var v = t.excep.shift();
    var tposter = v[0];
    var ex = v[1];
    if(v !== null && tposter !== null) {
        h$wakeupThread(tposter);
    }
    if(!alreadySuspended) {
        h$suspendCurrentThread(next);
    }
    h$sp += 2;
    h$stack[h$sp-1] = ex;
    h$stack[h$sp] = h$raiseAsync_frame;
    t.sp = h$sp;
}
function h$wakeupThread(t) {
    if(t.status === (1)) {
        t.blockedOn = null;
        t.status = (0);
        h$blocked.remove(t);
    }
    t.interruptible = false;
    t.retryInterrupted = null;
    h$threads.enqueue(t);
    h$startMainLoop();
}
function h$forceWakeupThread(t) {
  if(t.status === (1)) {
    h$removeThreadBlock(t);
    h$wakeupThread(t);
  }
}
function h$removeThreadBlock(t) {
  var i;
  if(t.status === (1)) {
    var o = t.blockedOn;
    if(o === null || o === undefined) {
      throw ("h$removeThreadBlock: blocked on null or undefined: " + h$threadString(t));
    } else if(o === h$delayed) {
      h$delayed.remove(t);
      t.delayed = false;
    } else if(o instanceof h$MVar) {
      var r, rq = new h$Queue();
      while((r = o.readers.dequeue()) !== null) {
          if(r !== t) rq.enqueue(r);
      }
      var w, wq = new h$Queue();
      while ((w = o.writers.dequeue()) !== null) {
        if(w[0] !== t) wq.enqueue(w);
      }
      o.readers = rq;
      o.writers = wq;
      if(o.waiters) {
        var wa = [], wat;
        for(i=0;i<o.waiters.length;i++) {
          wat = o.waiters[i];
          if(wat !== t) wa.push(wat);
        }
        o.waiters = wa;
      }
    } else if(o instanceof h$Thread) {
      for(i=0;i<o.excep.length;i++) {
        if(o.excep[i][0] === t) {
          o.excep[i][0] = null;
          break;
        }
      }
    } else if (o instanceof h$TVarsWaiting) {
      h$stmRemoveBlockedThread(o, t)
    } else if((typeof (o) === 'object' && (o) && (o).f && (o).f.t === (5))) {
      h$removeFromArray(((o).d2),t);
    } else {
      throw ("h$removeThreadBlock: blocked on unknown object: " + h$collectProps(o));
    }
    if(t.retryInterrupted) {
      t.sp+=2;
      t.stack[t.sp-1] = t.retryInterrupted;
      t.stack[t.sp] = h$retryInterrupted;
    }
  }
}
function h$removeFromArray(a,o) {
  var i;
  while((i = a.indexOf(o)) !== -1) {
    a.splice(i,1);
  }
}
function h$finishThread(t) {
    t.status = (16);
    h$blocked.remove(t);
    t.stack = null;
    t.mask = 0;
    for(var i=0;i<t.excep.length;i++) {
        var v = t.excep[i];
        var tposter = v[0];
        if(v !== null && tposter !== null) {
            h$wakeupThread(tposter);
        }
    }
    t.excep = [];
}
function h$blockThread(t,o,resume) {
    if(t !== h$currentThread) {
        throw "h$blockThread: blocked thread is not the current thread";
    }
    if(o === undefined || o === null) {
        throw ("h$blockThread, no block object: " + h$threadString(t));
    }
    t.status = (1);
    t.blockedOn = o;
    t.retryInterrupted = resume;
    t.sp = h$sp;
    h$blocked.add(t);
    return h$reschedule;
}
var h$lastGc = Date.now();
var h$gcInterval = 1000;
function h$scheduler(next) {
    if(h$currentThread &&
       h$currentThread.isSynchronous &&
       h$currentThread.status === (0)) {
        return next;
    }
    var now = Date.now();
    h$wakeupDelayed(now);
    if(h$currentThread && h$pendingAsync()) {
        if(h$currentThread.status !== (0)) {
            h$forceWakeupThread(h$currentThread);
            h$currentThread.status = (0);
        }
        h$postAsync(next === h$reschedule, next);
        return h$stack[h$sp];
    }
    var t;
    while(t = h$threads.dequeue()) {
        if(t.status === (0)) { break; }
    }
    if(t === null) {
        if(h$currentThread && h$currentThread.status === (0)) {
            if(now - h$lastGc > h$gcInterval) {
                if(next !== h$reschedule && next !== null) {
                    h$suspendCurrentThread(next);
                    next = h$stack[h$sp];
                }
                var ct = h$currentThread;
                h$currentThread = null;
                h$gc(ct);
                h$currentThread = ct;
                h$stack = h$currentThread.stack;
                h$sp = h$currentThread.sp
            }
            return (next===h$reschedule || next === null)?h$stack[h$sp]:next;
        } else {
            h$currentThread = null;
            if(now - h$lastGc > h$gcInterval)
                h$gc(null);
            return null;
        }
    } else {
        if(h$currentThread !== null) {
            if(h$currentThread.status === (0)) {
                h$threads.enqueue(h$currentThread);
            }
            if(next !== h$reschedule && next !== null) {
                h$suspendCurrentThread(next);
            } else {
                h$currentThread.sp = h$sp;
            }
            if(h$pendingAsync()) h$postAsync(true, next);
        } else {
        }
        if(now - h$lastGc > h$gcInterval) {
            h$currentThread = null;
            h$gc(t);
        }
        h$currentThread = t;
        h$stack = t.stack;
        h$sp = t.sp;
        return h$stack[h$sp];
    }
}
function h$scheduleMainLoop() {
    if(h$mainLoopImmediate) return;
    h$clearScheduleMainLoop();
    if(h$delayed.size() === 0) {
        if(typeof setTimeout !== 'undefined') {
            h$mainLoopTimeout = setTimeout(h$mainLoop, h$gcInterval);
        }
        return;
    }
    var now = Date.now();
    var delay = Math.min(Math.max(h$delayed.peekPrio()-now, 0), h$gcInterval);
    if(typeof setTimeout !== 'undefined') {
        if(delay >= 1) {
            h$mainLoopTimeout = setTimeout(h$mainLoop, Math.round(delay));
        } else {
            h$mainLoopImmediate = h$setImmediate(h$mainLoop);
        }
    }
}
var h$animationFrameMainLoop = false;
function h$clearScheduleMainLoop() {
    if(h$mainLoopTimeout) {
        clearTimeout(h$mainLoopTimeout);
        h$mainLoopTimeout = null;
    }
    if(h$mainLoopImmediate) {
        h$clearImmediate(h$mainLoopImmediate);
        h$mainLoopImmediate = null;
    }
    if(h$mainLoopFrame) {
        cancelAnimationFrame(h$mainLoopFrame);
        h$mainLoopFrame = null;
    }
}
var h$setImmediate, h$clearImmediate;
if(typeof setImmediate !== 'undefined') {
  h$setImmediate = function(f) { return setImmediate(f); }
  h$clearImmediate = function(h) { clearImmediate(h); }
} else {
  h$setImmediate = function(f) { return setTimeout(f, 0); }
  h$clearImmediate = function(h) { clearTimeout(h); }
}
function h$startMainLoop() {
    if(h$running) return;
    if(typeof setTimeout !== 'undefined') {
        if(!h$mainLoopImmediate) {
            h$clearScheduleMainLoop();
            h$mainLoopImmediate = h$setImmediate(h$mainLoop);
        }
    } else {
      while(true) {
        try {
          h$mainLoop();
        } catch(e) {
          throw e;
        }
      }
    }
}
var h$busyYield = 500;
var h$schedQuantum = 25;
var h$mainLoopImmediate = null;
var h$mainLoopTimeout = null;
var h$mainLoopFrame = null;
var h$running = false;
var h$nextThread = null;
function h$mainLoop() {
  if(h$running) return;
  h$clearScheduleMainLoop();
  if(h$currentThread) {
    h$scheduleMainLoop();
    return;
  }
  h$running = true;
  h$runInitStatic();
  h$currentThread = h$nextThread;
  if(h$nextThread !== null) {
    h$stack = h$currentThread.stack;
    h$sp = h$currentThread.sp;
  }
  var c = null;
  var start = Date.now();
  do {
    c = h$scheduler(c);
    if(c === null) {
      h$nextThread = null;
      h$running = false;
      h$currentThread = null;
      h$scheduleMainLoop();
      return;
    }
    if(!h$currentThread.isSynchronous && Date.now() - start > h$busyYield) {
      if(c !== h$reschedule) h$suspendCurrentThread(c);
      h$nextThread = h$currentThread;
      h$currentThread = null;
      h$running = false;
      if(h$animationFrameMainLoop) {
        h$mainLoopFrame = requestAnimationFrame(h$mainLoop);
      } else {
        h$mainLoopImmediate = h$setImmediate(h$mainLoop);
      }
      return;
    }
    c = h$runThreadSliceCatch(c);
  } while(true);
}
function h$runThreadSliceCatch(c) {
  try {
    return h$runThreadSlice(c);
  } catch(e) {
    c = null;
    if(h$stack && h$stack[0] === h$doneMain_e) {
      h$stack = null;
      h$reportMainLoopException(e, true);
      h$doneMain_e();
    } else {
      h$stack = null;
      h$reportMainLoopException(e, false);
    }
    h$finishThread(h$currentThread);
    h$currentThread.status = (17);
    h$currentThread = null;
  }
  return h$reschedule;
}
function h$runThreadSlice(c) {
  var count, scheduled = Date.now();
  while(c !== h$reschedule &&
        (h$currentThread.noPreemption || h$currentThread.isSynchronous ||
         (Date.now() - scheduled < h$schedQuantum))) {
    count = 0;
    while(c !== h$reschedule && ++count < 1000) {
      c = c();
      c = c();
      c = c();
      c = c();
      c = c();
      c = c();
      c = c();
      c = c();
      c = c();
      c = c();
    }
    if(c === h$reschedule &&
       (h$currentThread.noPreemption || h$currentThread.isSynchronous) &&
       h$currentThread.status === (1)) {
      c = h$handleBlockedSyncThread(c);
    }
  }
  return c;
}
function h$reportMainLoopException(e, isMainThread) {
  if(e instanceof h$ThreadAbortedError) return;
  var main = isMainThread ? " main" : "";
  h$log("uncaught exception in Haskell" + main + " thread: " + e.toString());
  if(e.stack) h$log(e.stack);
  if (h$isNode()) {
    process.exit(1);
  }
}
function h$handleBlockedSyncThread(c) {
  var bo = h$currentThread.blockedOn;
  if(h$currentThread.status === (1) &&
     (typeof (bo) === 'object' && (bo) && (bo).f && (bo).f.t === (5)) &&
     h$runBlackholeThreadSync(bo)) {
    c = h$stack[h$sp];
  }
  if(h$currentThread.isSynchronous && h$currentThread.status === (1)) {
    if(h$currentThread.continueAsync) {
      h$currentThread.isSynchronous = false;
      h$currentThread.continueAsync = false;
    } else if(h$currentThread.isSynchronous) {
      h$sp += 2;
      h$currentThread.sp = h$sp;
      h$stack[h$sp-1] = h$ghczminternalZCGHCziInternalziJSziPrimziInternalziwouldBlock;
      h$stack[h$sp] = h$raiseAsync_frame;
      h$forceWakeupThread(h$currentThread);
      c = h$raiseAsync_frame;
    }
  }
  return c;
}
function h$run(a) {
  var t = h$forkThread(a, false);
  h$startMainLoop();
  return t;
}
function h$WouldBlock() {
}
h$WouldBlock.prototype.toString = function() {
  return "Haskell Operation would block";
}
function h$HaskellException(msg) {
  this._msg = msg;
}
h$HaskellException.prototype.toString = function() {
  return this._msg;
}
function h$setCurrentThreadResultWouldBlock() {
  h$currentThread.result = new h$WouldBlock();
  h$currentThread.resultIsException = true;
}
function h$setCurrentThreadResultJSException(e) {
  h$currentThread.result = e;
  h$currentThread.resultIsException = true;
}
function h$setCurrentThreadResultHaskellException(msg) {
  h$currentThread.result = new h$HaskellException(msg);
  h$currentThread.resultIsException = true;
}
function h$setCurrentThreadResultValue(v) {
  h$currentThread.result = v;
  h$currentThread.resultIsException = false;
}
function h$runSyncReturn(a, cont) {
  var t = new h$Thread();
  var aa = (h$c2(h$ap1_e,(h$ghczminternalZCGHCziInternalziJSziPrimziInternalzisetCurrentThreadResultValue),(a)));
  h$runSyncAction(t, aa, cont);
  if(t.status === (16)) {
    if(t.resultIsException) {
      throw t.result;
    } else {
      return t.result;
    }
  } else if(t.status === (1)) {
    throw new h$WouldBlock();
  } else {
    throw new Error("h$runSyncReturn: Unexpected thread status: " + t.status);
  }
}
function h$runSync(a, cont) {
  var t = new h$Thread();
  h$runSyncAction(t, a, cont);
  if(t.resultIsException) {
    if(t.result instanceof h$WouldBlock) {
      return false;
    } else {
      throw t.result;
    }
  }
  return t.status === (16);
}
function h$runSyncAction(t, a, cont) {
  h$runInitStatic();
  var c = h$return;
  t.stack[2] = h$ghczminternalZCGHCziInternalziJSziPrimziInternalzisetCurrentThreadResultException;
  t.stack[4] = h$ap_1_0;
  t.stack[5] = a;
  t.stack[6] = h$return;
  t.sp = 6;
  t.status = (0);
  t.isSynchronous = true;
  t.continueAsync = cont;
  var ct = h$currentThread;
  var csp = h$sp;
  var cr1 = h$r1;
  var caught = false, excep = null;
  h$currentThread = t;
  h$stack = t.stack;
  h$sp = t.sp;
  try {
    c = h$runThreadSlice(c);
    if(c !== h$reschedule) {
      throw new Error("h$runSyncAction: h$reschedule expected");
    }
  } catch(e) {
    h$finishThread(h$currentThread);
    h$currentThread.status = (17);
    caught = true;
    excep = e;
  }
  if(ct !== null) {
    h$currentThread = ct;
    h$stack = ct.stack;
    h$sp = csp;
    h$r1 = cr1;
  } else {
    h$currentThread = null;
    h$stack = null;
  }
  if(t.status !== (16) && !cont) {
    h$removeThreadBlock(t);
    h$finishThread(t);
  }
  if(caught) throw excep;
}
function h$runBlackholeThreadSync(bh) {
  var ct = h$currentThread;
  var sp = h$sp;
  var success = false;
  var bhs = [];
  var currentBh = bh;
  if(((bh).d1).excep.length > 0) {
    return false;
  }
  h$currentThread = ((bh).d1);
  h$stack = h$currentThread.stack;
  h$sp = h$currentThread.sp;
  var c = (h$currentThread.status === (0))?h$stack[h$sp]:h$reschedule;
  try {
    while(true) {
      while(c !== h$reschedule && (typeof (currentBh) === 'object' && (currentBh) && (currentBh).f && (currentBh).f.t === (5))) {
        c = c();
        c = c();
        c = c();
        c = c();
        c = c();
      }
      if(c === h$reschedule) {
        if((typeof (h$currentThread.blockedOn) === 'object' && (h$currentThread.blockedOn) && (h$currentThread.blockedOn).f && (h$currentThread.blockedOn).f.t === (5))) {
          bhs.push(currentBh);
          currentBh = h$currentThread.blockedOn;
          h$currentThread = ((h$currentThread.blockedOn).d1);
          if(h$currentThread.excep.length > 0) {
            break;
          }
          h$stack = h$currentThread.stack;
          h$sp = h$currentThread.sp;
          c = (h$currentThread.status === (0))?h$stack[h$sp]:h$reschedule;
        } else {
          break;
        }
      } else {
        h$suspendCurrentThread(c);
        if(bhs.length > 0) {
          currentBh = bhs.pop();
          h$currentThread = ((currentBh).d1);
          h$stack = h$currentThread.stack;
          h$sp = h$currentThread.sp;
        } else {
          success = true;
          break;
        }
      }
    }
  } catch(e) { }
  h$sp = sp;
  h$stack = ct.stack;
  h$currentThread = ct;
  return success;
}
function h$syncThreadState(tid) {
  return (tid.isSynchronous ? 1 : 0) |
    ((tid.continueAsync || !tid.isSynchronous) ? 2 : 0) |
    ((tid.noPreemption || tid.isSynchronous) ? 4 : 0);
}
function h$main(a) {
  var t = new h$Thread();
    t.stack[0] = h$doneMain_e;
  if(!h$isBrowser() && !h$isGHCJSi()) {
    t.stack[2] = h$ghczminternalZCGHCziInternalziTopHandlerzitopHandler;
  }
  t.stack[4] = h$ap_1_0;
  t.stack[5] = h$flushStdout;
  t.stack[6] = h$return;
  t.stack[7] = h$ap_1_0;
  t.stack[8] = a;
  t.stack[9] = h$return;
  t.sp = 9;
  t.label = h$encodeUtf8("main");
  h$wakeupThread(t);
  h$startMainLoop();
  return t;
}
function h$doneMain() {
  if(h$isGHCJSi()) {
    if(h$currentThread.stack) {
      global.h$GHCJSi.done(h$currentThread);
    }
  } else {
    h$exitProcess(0);
  }
  h$finishThread(h$currentThread);
  return h$reschedule;
}
function h$ThreadAbortedError(code) {
  this.code = code;
}
h$ThreadAbortedError.prototype.toString = function() {
  return "Thread aborted, exit code: " + this.code;
}
function h$exitProcess(code) {
    if(h$isNode()) {
 process.exit(code);
    } else if(h$isJvm()) {
        java.lang.System.exit(code);
    } else if(h$isJsShell()) {
        quit(code);
    } else if(h$isJsCore()) {
        if(h$base_stdoutLeftover.val !== null) print(h$base_stdoutLeftover.val);
        if(h$base_stderrLeftover.val !== null) debug(h$base_stderrLeftover.val);
        if(code !== 0) debug("GHCJS JSC exit status: " + code);
        quit();
    } else {
        if(h$currentThread) {
            h$finishThread(h$currentThread);
            h$stack = null;
            throw new h$ThreadAbortedError(code);
        }
    }
}
var h$mvarId = 0;
function h$MVar() {
  this.val = null;
  this.readers = new h$Queue();
  this.writers = new h$Queue();
  this.waiters = null;
  this.m = 0;
  this.id = ++h$mvarId;
}
function h$notifyMVarEmpty(mv) {
  var w = mv.writers.dequeue();
  if(w !== null) {
    var thread = w[0];
    var val = w[1];
    mv.val = val;
    if(thread !== null) {
      h$wakeupThread(thread);
    }
  } else {
    mv.val = null;
  }
}
function h$notifyMVarFull(mv,val) {
  if(mv.waiters && mv.waiters.length > 0) {
    for(var i=0;i<mv.waiters.length;i++) {
      var w = mv.waiters[i];
      w.sp += 2;
      w.stack[w.sp-1] = val;
      w.stack[w.sp] = h$return;
      h$wakeupThread(w);
    }
    mv.waiters = null;
  }
  var r = mv.readers.dequeue();
  if(r !== null) {
    r.sp += 2;
    r.stack[r.sp-1] = val;
    r.stack[r.sp] = h$return;
    h$wakeupThread(r);
    mv.val = null;
  } else {
    mv.val = val;
  }
}
function h$takeMVar(mv) {
  if(mv.val !== null) {
    h$r1 = mv.val;
    h$notifyMVarEmpty(mv);
    return h$stack[h$sp];
  } else {
    mv.readers.enqueue(h$currentThread);
    h$currentThread.interruptible = true;
    return h$blockThread(h$currentThread,mv,[h$takeMVar,mv]);
  }
}
function h$tryTakeMVar(mv) {
  if(mv.val === null) {
    { h$ret1 = (null); return (0); };
  } else {
    var v = mv.val;
    h$notifyMVarEmpty(mv);
    { h$ret1 = (v); return (1); };
  }
}
function h$readMVar(mv) {
  if(mv.val === null) {
    if(mv.waiters) {
      mv.waiters.push(h$currentThread);
    } else {
      mv.waiters = [h$currentThread];
    }
    h$currentThread.interruptible = true;
    return h$blockThread(h$currentThread,mv,[h$readMVar,mv]);
  } else {
    h$r1 = mv.val;
    return h$stack[h$sp];
  }
}
function h$putMVar(mv,val) {
  if(mv.val !== null) {
    mv.writers.enqueue([h$currentThread,val]);
    h$currentThread.interruptible = true;
    return h$blockThread(h$currentThread,mv,[h$putMVar,mv,val]);
  } else {
    h$notifyMVarFull(mv,val);
    return h$stack[h$sp];
  }
}
function h$tryPutMVar(mv,val) {
  if(mv.val !== null) {
    return 0;
  } else {
    h$notifyMVarFull(mv,val);
    return 1;
  }
}
function h$writeMVarJs1(mv,val) {
  var v = (h$c1(h$data1_e, (val)));
  if(mv.val !== null) {
    mv.writers.enqueue([null,v]);
  } else {
    h$notifyMVarFull(mv,v);
  }
}
function h$writeMVarJs2(mv,val1,val2) {
  var v = (h$c2(h$data1_e, (val1), (val2)));
  if(mv.val !== null) {
    mv.writers.enqueue([null,v]);
  } else {
    h$notifyMVarFull(mv,v);
  }
}
function h$MutVar(v) {
    this.val = v;
    this.m = 0;
}
function h$atomicModifyMutVar(mv, fun) {
  var oldVal = mv.val;
  var thunk = (h$c2(h$ap1_e,(fun),(oldVal)));
  mv.val = thunk;
  { h$ret1 = (thunk); return (oldVal); };
}
function h$atomicModifyMutVar2(mv, fun) {
  var oldVal = mv.val;
  var thunk = (h$c2(h$ap1_e,(fun),(oldVal)));
  mv.val = (h$c1(h$select1_e, (thunk)));
  { h$ret1 = (thunk); return (oldVal); };
}
function h$blockOnBlackhole(c) {
  if(((c).d1) === h$currentThread) {
    return h$throw(h$ghczminternalZCGHCziInternalziControlziExceptionziBasezinonTermination, true);
  }
  if(((c).d2) === null) {
    ((c).d2 = ([h$currentThread]));
  } else {
    ((c).d2).push(h$currentThread);
  }
  return h$blockThread(h$currentThread,c,[h$resumeBlockOnBlackhole,c]);
}
function h$resumeBlockOnBlackhole(c) {
  h$r1 = c;
  return h$ap_0_0_fast();
}
function h$makeResumable(bh,start,end,extra) {
  var s = h$stack.slice(start,end+1);
  if(extra) {
    s = s.concat(extra);
  }
  { (bh).f = h$resume_e; (bh).d1 = (s), (bh).d2 = null; };
}
var h$enabled_capabilities = h$newByteArray(4);
h$enabled_capabilities.i3[0] = 1;
function h$rtsSupportsBoundThreads() {
  return 0;
}
function h$rts_setMainThread(t) {
}
function h$mkForeignCallback(x) {
    return function() {
        if(x.mv === null) {
            x.mv = arguments;
        } else {
            h$notifyMVarFull(x.mv, (h$c1(h$data1_e, (arguments))));
            h$mainLoop();
        }
    }
}
function h$makeMVarListener(mv, stopProp, stopImmProp, preventDefault) {
  var f = function(event) {
    h$writeMVarJs1(mv,event);
    if(stopProp) { event.stopPropagation(); }
    if(stopImmProp) { event.stopImmediatePropagation(); }
    if(preventDefault) { event.preventDefault(); }
  }
  f.root = mv;
  return f;
}
function h$rs() {
  return h$stack[h$sp];
}

// Unicode tables generated by ghcjs/utils/genUnicode.hs
var h$printRanges = "f|!-f=|/q'/+1$J|(mo'| -')| 63Y+/EO'|$9| ('| ?'|!9?| ?'| +'AZ'$9| 3M2MA|#V2'''O0$)+'5'''+3*','U').+''O0&&&'$-+''))0+$1E7)4(N0&,'7(('@+';11(2'''O0&,'5''')3'+','G7'.))*)'$&)')));+-))*'.>M-=(PB)3(*1'(-+'71O(P6,'5(*1'1$+'7&=+2(| .(.+C'W''F)S4$'1)*/$2/7');| =+^n'$,R$P'-$.'7'+d| Yk+rk@<n|$G$-&|(E*'1$*'v*'f*'1$*'A| :*'| O'd)W/| v'/'|.r)|! 1=09Q5K;=(&;|!+'7/7/?'7/|! 1z-| U7b:+;+(x'-9|  +W/9)| E'| K]'9/7/?'A| K| b+| #)|!W3| A)A)| A1z'93z-|%U|&<'/'p'/'3 $a'| 3@>'/H')48-S1| +C''Y<)dCfA|#-+|.fU9M|H;'d'|#C| &|#<-| #$-&| 91'?S510000000|!N| )W| {;|$hW;+| I| u'|!=-z|!*y-l;| '|$y} ^y7}%1UC|9t)| 75|'fK|$+3|$;'-| )| 3+7/| 93| U3;/|!W9`)| f8+f| 65?'7'|!=S[7/'/'/510| 83|!l'7/}!e;;Q+| +}!'n|(/'|!Cp1;--W,$&&|!gE|(-C| I'| 5t?'W/| /H*+-|#!+|$7)/'/'/'))10='';VH&@'?h|!f-)+| #)| z:+| &| %|!t^)| +A[+l5`-z-`m+?x|#Q'7/l+l+t3| 19|#4|&v5O73|#E/'$|  &)&Q| X35| )I&-f)Y-| H| 9+K'| -&-3(]')+7151| Idr+;5| 5)^'Y-W1+;1| j| [| 7| /=| /1| %37|&Sb|!rt3x|# Q5| f+`A| E*?U17/| 3D5r5| f'CJ9G{| K1$*@8/| ?-7/+2'''O0&,6''')'.,1'1)-|#+|!#$(d| Y37|#b| 5'ph| S97/=I| ;17| 5Y'A+C|$;| A|!7| p;|#T3'| %'9Y| Y3| p^| ;|%p5| !>7^)d'O>| [1&{)$'437//&m&.17|&tU|$I| -=|4Q|!;|!M,9|$C|^)|7l|{ |,z}!+p|,^1b6+'|!/`'/7| U770L-I|/=|!%|!9| `+| ;1E| I+[} NC3|0j}!>j|&E| +)E+3|(l|S=|!E-=)517'+} 47|%M7r'| ^3|!KQ| U|#IK;| x5U|##| t| V&'&''+:$0| J*'30Z*,$)1|'T'|&O'|/YA-@|>71D'1&,|$f| #)?'7+'|(3| =-|<J|$E'Cv| `-7+'|1K| Q| b| C|$?+X&$'$7* #/* $)&$' &'$'+0**$6D-),D| 1'|&#|  +|!7;A'A@m7=)|!))| C| ;^=|  +51'?/|#I|5n7=)9-|!W;|! 1;K;+| 937/t3`| n;|!8*)v'/)^'|##;?'++)-)=/|>l}*Q/v} !59|$x'} F'?} m)|ez|,'|G1|%A";
var h$alnumRanges = "| +71W/W| '0'$)'(Pa|*2+;?-1$|!q-&'+$/$)$J| o|#*3|#bo'.v| WY++| #zM7+'|!4$A'1A'B$`^|! 9>z5'+,O+4(PU19| 3M2| U| 5)F07+7B+3'''O0$)+)B<'(+;'/'2/+''O0&&&b+$17')C5(N0&,)F@'+7583'''O0&,)_'(+709$/))*)'$&)')));OL=G3(PB)V)-'+731$+3(P6,)c$'+7&G3(u'B,)6+I.-G)S4$'1b7E| )&;157r'$,R$6&5&-$57'+daK;3kY-|!UzK9//++)('1)+=;$7/p$-&z|'F*'1$*'v*'f*'1$*'A| :*'| O?K)CC| v'/)|-j'EV-| `)91=*?G?G?=(A| 1j*(7/7O7/|! 1-'h$-| U7brt'-9|  +W/9nQ5| 3z7/7=|!(| 'E1+7v`=| 9Wl[7)| +'51z')v+.&),|$;| I|&3'/'p'/'3 $a'| 30$))0)+'/+=-)0|!U''/-9/=|!9*&7$)-/ $+8'+--+$| =|0/| A| fO|.#`|91| '| &|!y/+)'5&p$-&| 91BQ510000000| j|*H)U51-'-+| v/)|!!*-z|!*)+7Y| 3Cd7`3@d7rA|'-} X;| ^}%0/C|9t| O| %'|& )[K| 'Cb'| jr5'|!='| 3'-| )9(*P=/7| 1?| -[7S/)$'o7QU^1| '[9/-TuQ)2+7/Q)(| -$)''-'$R)'91);/'/'/510y:3|!U=7/}!e;;Q+| +}!'n|(/'|!Cp1;--$7<,$&&|!Ff|()G| I'| 5t;|!W-|#!I71W/W9|! )/'/'/')j;VH&@'?h|!f;| #;| ;E'|%I^)| +CY+l5`-p7`'l+3,x|#Q'7/l+l+t3| 1|#M|&v5O73|#E/'$|  &)&Q'b'p35| )I&-f+W| U| 9+K'| 'A+(]75Qbcd3Z/-C| 57O'Y-WQ1| j| [| 7| /=| /1x;7|&Sb|!rt3O9+|#+Q;| 3W`I| #dU175lA7+8j):| )?+99$+K9GT| r1$*@61| 'E793'''O0&,)F:-|#Q| 3G+-7-c| )K'$37|#b| 'v+l| )K87pz=07| 5YM;|$C|  |!W| p;|#T3'rC)[6t1L8| %Ig| ;|%p5mE@^-`|!O1&oM47//&c?07|&tI|$UMz|4O|!;|!ME|$C|^)|7l|{ |,z}!+p|,^1b6|!;`G| )C+;70L-I|/=| x|!A| `-| L=| I'$[} NC3|0j}!>j|&E| +)E+3|(l|S=|!E-=)517} DdK|!GU|##| t| V&'&''+:$0| J*'30Z*,$)1|'T'UTaTaTaTaT2'| -|S5| #71'7+'|(3|  +7|<W|$E'5| )| Q;7|1W| ?(*| b| #@|$?+X&$'$7* #/* $)&$' &'$'+0**$6D-),D|,t=|v'}*Q/v} !59|$x'} F'?} m)|ez|,'";
var h$lowerRanges = "|!3W| =uS2 <& (& 8' #)'$&('+&()'& #&$'$'($&'')/&& )' )&'$( >1'&'$+ %| SX|$=$(()GXj&)) ,,$'&'| /| ) 25 ;& '' Q| )v|a1z')|0t/|PG5|!^|  | G=g|!; l5 Q43/73333/73333?'333333-&/()&3+''337)&|&+(')X**&'3++| 2|]a| ''(' $+$'.- R'1$*:p$-}'Zi 7H .|#! ') @2 #' %*$&$) +-, &(| 4|28z-33| j}${p1;-|7`W|;?t|#%l|L3| /|d/d}%DGd}&F#WW1FWWW+$08WWWWWWWWWWWWWWWWW[[U.WU.WU.WU.WU.$} ([h";
var h$upperRanges = "| MW|!9Q0f <& (& 8' #)'$&('+&()'& #&$'$'($&)0'&& )' )&'$( >1'&'$+ %|&I$(2.$)$&D4j&)) ,,&$''| /| ) 14 <' '' P&p|a5p$-|0&| v|Pxz')|'- l4 Q433/73333/9 $23S333333-9-9+;-9-|%l*()')'(-/ $+'+7'-| B|[]| '| +$)' $+$'2) R3$*}']( 7H .|#! '( ?6 #' %+$&$( +-, &)$)}%NxW|;/t|#%l|K^| /|dpd}%DGd}&F/WWWWWW$''&''+2WWW'*'30Y'*,$)1YWWWWWWWWWWW`UfUfUfUfUf} 'sh";
var h$alphaRanges = "| MW/W| '6*,Qa|*2+;?-1$|!q-&'+$/$)$J| o|#*3|#bo'.v| WY++| #zj'|!4$A'1'7)'B$`^|! 9Rf5'+,O+4(PU19| 3M2| U| 5)F07AC+3'''O0$)+)B<'(?'72/+''O0&&&b+$I)C5(N0&,)F@'Q83'''O0&,)_'(AD$/))*)'$&)')));O| 03(PB)V)-'`*3(P6,)c$'A'G3(u'B,)3)S/-G)S4$'1| =| )&;1| ='$,R$6&5&-$M+d| F3kY-|!UzKB/++)('1)+=;Dp$-&z|'F*'1$*'v*'f*'1$*'A| :*'| OnCC| v'/)|-j'EV-| `/31=*?G?G?=(A| 1j*| N|! 1-'h$-| U7b| +`'-9|  +W| 5Q5| 3| n|!(| 'E1| 7`='7|  Wlv)7l'51z')v+.&),|$;| I|&3'/'p'/'3 $a'| 30$))0)+'/+=-)0|!W<B=|!9*&7$)-/ $+8'+--+| 0'|[[| '| &|!y/+)';p$-&| 91BQ510000000| j|*H'x--'+| v/)|!!*-z|!*EY| 3C|+E} X;| ^}%0/C|9t| O| %'|& )C7'K| 'Cb'| U| +5'|!='| 3'-| )9(*P^| 1?| -| E/)$'9[7QU^1| '[B-67-uQ)2KQ)(| -$)''-'$R)'91);/'/'/510y:3|!U^}!e;;Q+| +}!'n|(/'|!Cp1;--$7<,$&&|!Ff|()G| I'| 5t;|!W-|#!lW/W9|! )/'/'/')j;VH&@'?h|!f|(^^)| +| 'd=K2/p7`'l+3| )|#QGl+l+t3| 1|#M|&v5O73|#E/'$|  &)&Q7Q5b| KI&7O7W| U| 9/'| I@+(]x^)^j3ZY| 57O7I=G|!K| [| 7| /=| /=l|*W^72O|#IQ;| 3| `| #dUWl^8j):| )?+M$iGT| r1$*@61| 'p3'''O0&,)F:-|#Q| 3G+Kc| )K'$|$+| 'v+l| )K| >z=| VY|%+|  |!W| Ib|#T3'rC)[6t1L8| %Ig| ;|%p5mE| *`|!O1&oMT/&c?|':I|& |4O|%-|$C|^)|7l|{ |,z}!+p|,^1b|!Q`G| )C+bM-I|/=| I|!p| `-| L=| I'$[} NC3|0j}!>j|&E| +)E+3|(l|S=|!E-=)517} K-| t| V&'&''+:$0| J*'30Z*,$)1|'T'UTaTaTaTaT2|TC| #71C'|(3|  |<t|$E| ?| Q|:x+X&$'$7* #/* $)&$' &'$'+0**$6D-),D} (7}*Q/v} !59|$x'} F'?} m)|ez|,'";
var h$toLowerMapping = "| K Wb|!9 Qb!1bf  9#  !|$F  ## &'  (# &'  8#  !|!_# # #)  !|$^# ! # ! |$U !# '|$S&'  !| f|$M !|$O# ! |$S !|$W  !|$`|$[&)  !|$`|$d ! |$f $#  !|$n# ! |$n'  !#  !|$n#!'|$l ##  !|$p#) &1  !%# ! % !#  !%# ) #'  )# &'  !%# ! # ! |!. !| 6# 4 # ! |!q * #1  !}![r# ! |#X}%=]'  !#  !|$>| Q !| U# % #|&I  !# &) &3 |%0/  !n )l ! | G!'| E!Eb!5bj B3  ,# &- |!]'  !#  !.#' )|!qC| hdb| )  1# &5  <#  !?# ' #'  P# &' p| '|a5 p} hG ! } hG- }#To|0' | j})U[/1|Px z|cm' )|cm|'-  l# &5  !} p4  P# &5 303 /07 303 303 /09  $0 @3 30S 303 303 303 '0'| ZD9 +| sD9 '0'|!4; '0'|!L<9 '|!m'|!iD|&Y }#a()  !}!&:}!#V/ | 8| # CAI &|23 WU|Ht | '| '| +  !#  !}!Zc|ue}%:e'  $#  !}![R}!Zo !}![X}![V ! #' &3 '}!]> R# &3  !# &+ &}'])  7# &I  .# &|##  '# &)  ?# &7  ##  !}(b.# % #+  !# }4p*'  !# &)  +#  !}*H0}*HF !}*H>}*H* !}*H0  !}*G&}*GV !}*G,|4Y &# &)  !#  !| &}*H.}1JX}%Nx Wb|;/ tr|#% lr|K^ | /| G|dp db}%DG db}'dY hf}c/Q ";
var h$toUpperMapping = "|!1 Wa| = |A$x Qa!1a !|!`  9!  !|%.  #! $'  (! $'  7! $'  #!  !!|&]|(_'  !! $' $) $- $' |$>)  !!|#Y) |%i'  #! $' $+ $' $)  !! $' $)  !! |!N-  !!$ ! ! !$  !!$ ) ! !| e  )! $'  !!$ ! !)  4! $)  )! $3 $' '}!]? ! !+  %!  !!}![Y !}![S}![W !|$]|$T!'|$R ! |$L ! |$N}4qo)  !|$R}*H? ! |$V ! }*GS !}*H1  !|$Z|$_ !}*H1}!Zd}4q6'  !|$_  !}!Zp|$c' |)N1 }%:g'  !|$m  !}*H/|$m)  !}*GW|$m|#&'|$k|#.- |)c7  !}*G-}*G'|#b |#ez  !! $) $) )|!r| % | _)k!Ea| B5a|!m'| D ! | B|!P)  !| $| 2 !0  ,!  !!| s !| g/ !|!T |$8' $' $| 1 daC| g 2 !5  ;! $'  '!  !!> Q !| + p| &|a5 z|cn' )|cn|0t /0|PG  !} Py} Pw}#'N'} Pa !} Pc} PT !} O@}(``|%A }1H>) } pPC }1JZ|!S  l! $- |!X-  P! $313 /17 313 313 /19  $1 B3 313 '| [+| t'|!5'|!n'|!M'|!j' 313 313 313 '1 ! 37 }#R4+ F; '1? '1) >= F|'b | 6f C@+ $|2f WT|IE | '| &' $)  !}![q}![k $ !/ $' $7  R! $3  !! $+ $; p} hF ! } hF- }#Tm}'Zj  7! $I  .! $|##  '! $)  ?! $7  !! $'  %! $+ $+  !!  !!| ''  *! $9  &! $) $|49 |I6[ | j})UZ}%9' Wa|;? tq|#% lq|L3 | /| F|d/ da}%DG da}'d^ he}c.h ";
var h$toTitleMapping = "|!1 Wa| = |A$x Qa!1a !|!`  9!  !|%.  #! $'  (! $'  7! $'  #!  !!|&]|(_'  !! $' $) $- $' |$>)  !!|#Y) |%i'  #! $' $+ $' $)  !! $' $)  !! |!N+  !#  !!# ! ! !#  )!  !!| e * ! ! # # !)  4! $)  )! $3 $' '}!]? ! !+  %!  !!}![Y !}![S}![W !|$]|$T!'|$R ! |$L ! |$N}4qo)  !|$R}*H? ! |$V ! }*GS !}*H1  !|$Z|$_ !}*H1}!Zd}4q6'  !|$_  !}!Zp|$c' |)N1 }%:g'  !|$m  !}*H/|$m)  !}*GW|$m|#&'|$k|#.- |)c7  !}*G-}*G'|#b |#ez  !! $) $) )|!r| % | _)k!Ea| B5a|!m'| D ! | B|!P)  !| $| 2 !0  ,!  !!| s !| g/ !|!T |$8' $' $| 1 daC| g 2 !5  ;! $'  '!  !!> Q !| + p| &|s1 /0|PG  !} Py} Pw}#'N'} Pa !} Pc} PT !} O@}(``|%A }1H>) } pPC }1JZ|!S  l! $- |!X-  P! $313 /17 313 313 /19  $1 B3 313 '| [+| t'|!5'|!n'|!M'|!j' 313 313 313 '1 ! 37 }#R4+ F; '1? '1) >= F|'b | 6f C@+ $|2f WT|IE | '| &' $)  !}![q}![k $ !/ $' $7  R! $3  !! $+ $; p} hF ! } hF- }#Tm}'Zj  7! $I  .! $|##  '! $)  ?! $7  !! $'  %! $+ $+  !!  !!| ''  *! $9  &! $) $|49 |I6[ | j})UZ}%9' Wa|;? tq|#% lq|L3 | /| F|d/ da}%DG da}'d^ he}c.h ";
var h$catMapping = "d;P)3J)3 !/0 !34 !3.'37*'3)4'3W! !/3 !06 !-6W# !/4 !04f; !83+5 !73 !67 !&1 !4< !76 !74', !6#'3 !6, !&2),FQ!H1!S#H3# <!#$'# (!#$'# 8!#'! ##!)#'! !#!&'!&)!'#+!&'!&)!)#'!&'! ##!&'! !#!'# !!#'!&)! !#!&'!'# !&!)#+& !!$ !#! !$# !!$ )#!'# )!#$'# !!$ !#!&)! >#!1#'!&'!'# !!#+! %#!| S#,Y#G%+6;%?6-%16 !%6*E6|!O' #!# !%6 !!#' *)# !3!+ '6 !!3)! ! !!'!&E!!5!j#$'#)!)# ,!#$-# !!# !4!&'!'#| /!| )# 2!#N-'') <!#'! '#!'# Q!#!p!' */3v# !3.' '7 !5 | #' !.'F''F'' !3'3 Y&+ +&'39 /<)4'3J'3'79' !3<!'3d&*7&M'7*+3'&.|!5& !3&1' !<7/''%''N+''&7*)&'7,?3 ! < !&'`&Y'' |! &9',? 7*f&5''%N)3*' .'5O&+'*5'*)'*-'' A3!U&)''  !3 9&| 3 M&!3&M A'Xd'0| 5& !'( !'&)(3'+(.'(,1'7&'''37* !3%A&.'(!3&' '&' O&!1& ! &) +&'  !'&)(+'' '(' '( !'&3 0+ '&!)&''' 7*'&'5/, !75 !&3.' '' !( /&+ '&' O&!1&!'&!'&!'&'  !' )(''+ ''' )') .1 +& ! &1 7*'')& !'37 '' !( 5&!)&!O&!1&!'&!-&'  !'&)(-'!'' !( '(.' ,A '&''' 7* !351 ,/' ! ''(!3&' '&' O&!1&!'&!-&'  !'& !('0+'' '(' '(.3  !'(+ '&!)&''' 7* !7&/,7  !'&!/&) )&!+&) '& ! &!'&) '&) )&) ;&+ '(.'() )(!)(.' ,/ 0? 7*),/7 !57- .)(.3&!)&!Q&!C&) ,)'+(!)'!+'1 ''!)&- '&''' 7*1 F1, !7&.'(F3&!)&!Q&!7&!-&'  !'& !('-( ! ''(!'(''1 '(1  !& '&''' 7*!'&= '''(!3&!)&!v&'',)(+'!)(!)( !'&N+ )&01,)&''' 7*5,N/&' '(!G&) S&!5& ! &' 1&) .+ )()' ! '!3(/ 7*' '(F; | )&.'&1'+ J/&*3'F7*'3n '& ! &!-&!S& ! &!7&.'&5',' -& ! %!/'' 7*' +&d ,)7A3 !73)7''/77*7, $7' #/0'(3&!l&+ ?'0-'F''-&9'!l'!37./7!'7-3+7'3n z&'(+'0/'0'''('',7*/3/&'(''+&)',)('&1()&+'=&.'(''/( !'&07*)(.'7p! ! !- $' z# !3%)#|'?&!+&' 1& ! &!+&' v&!+&' f&!+&' 1& ! &!+&' A&!| ;&!+&' | O&' )'53K,) C&77/ | v!' /#' <|-j&'3E&PW& !/0) | `&)3)+3&1 =&!+&)'9 G&)''35 G&''; =&!)&!''; | 1&''01'3(.'(9')3*)3 !5&.' 7*/ 7,/ /3<+3)' !< 7*/ j&*| 3&1 -&''h& !'&- | U&7 b&!)'+('')(+ '(./()'+ N) '37*`&' -&9 |  &+ W&/ 7*8) h7Q&'''(.' '3| 3& !('01' ! ' !(''(3'/(7'' .7*/ 7*/ 13*/3' ?'2| K +'0| '& !'(-' !('-(.'(1&+ 7*13775'57) ''0`&0+''(''0)''&7*|  & !'('')( !'()''(3 +3l&3(3''('') -37*) )&7*`&/%'35#1 z!' )!333 )'F='01'+&./&.'&0'',- |  #| G%=#*h#n%| ='!-' l!#$5# Q!#$5#3!/#' /!' 3#3!3#3!/#' /!' 3# % !3#3!?#' 3#3$3#3$3#3$-#!'#+! !$6&)6)#!'#+!()6+#' '#+!!)63#-!)6' )#!'#+!('6!98-</.'3 !12>'1 !2/B33 !9:-<P53 !12+3'-)3 !4/@93 !43:73P-<!7< !,%' /,)4 !/0*7,)4 !/0!=%) d5C ='+).));'A '7$+7$'7&)!'#)! !#7$'7H-!/7 $!7+! !7#+!&+&&'7'#'!-4$+# !74'7 !#7C,j+ !!#++8'7+ -4-7'4+7H'7H'7H17Hb7'4'7 !47Hb7|%z437 #/0K7'417 !/0| l7H`7U4t7/4| S7U 97M | A,| f7O,|$)7H57H| 5734|!M7H|%Q7 (/0`,|  7-4 !/0b4 &/0C4|%b7|!v4 ,/0| G4 #/0d4 !/0|%f4| )7M4'7/4r7' d7' |!?7| '!!| '# ! !&)!'# $!#+! !#!'#$/#'%)! R#!'#/7 #!#)' !!#- +38'3p# ! #- &' | 9&1  !%3? .Q&5 1&!1&!1&!1&!1&!1&!1&!1&!d''3 #12)3 !12 !31D53<'3 !.3 !12'3 !12 %/0-3*73'.+3 !.3>=3| ) W7!|! 7; |$h7W ;7+ P)3 !7% !&+ &/0'7 %/0 !./'0N5++''(<-%'7)+ !%&F'7!| v&' '''6'% !&.|!#&F)%,- z&!|!+&!'7+,77Y&- l7; C&b7!7,`73,NA,d77,r7A,| G7!|%b7} X;&7 | I7}%0/&C M&*|9G&) | 775 t&/%'3|%z&*)3C&7*'&K  8!# !&'))F7' !3% /!#'%''| U&7+''/33 Q65%'6 '!#$)# @!#*3# #!#'! %#! !#%'6 #!# !&! !#!)# +!#-!&-! &#!&'  !!#)!| ) ,'%&1&.)&.+&.Q&'(''0+7+ /,'7 !57/ | 1&+33 '(| -&C(''3 '37*/ G'/&)3 !&3'&.7*[&3''3Q&9''(9 F^&) )'0| '&.'(+''(.+(=3 ! %7*+ '3-& !'%5&7*-&!v&/''('''(''5 )&.3& !'(' 7*' +3C&*/&)7 !&( !'(| -& !'&)''&''-&'' !&',S '&*'39&0'''('3,'% !('7 /&' /&' /&5 1&!1&!z#L+%3#3 | j#j&'(.'(.'( !3(.' 7*/ }!e;&; Q&+ | +&+ |MQ=} T7 |(/&' |!C&p 1#; -#-  !&'7&H=&!-& ! &!'&!'&!|!G&C6E |()& !0/C | I&' | 5&t ;& !57' C'13 !/0F/ C'F'.'- )/0'3 !/0+3)-)3!+3 !./ #0/@)3 !4.)4 ! 3J'3+ -&!|##&'  !< )3J)3 !/0 !34 !3.'37*'3)4'3W! !/3 !06 !-6W# !/4 !04 !/0 !3/@'37&*| #&'%b&) /&' /&' /&' )&) '5 !46N'5 ! 7+4'77 )<'7' ;&!W&!I&!'&!A&' ?&h |!f&- )3+ | #,) 57| 3++,E7',)7!;7+ N| ' | #7.|!t ^&) | +&A .Y,+ d&+,5 K&63&6- p&-'- `& ! 3l&+ 3&F-+x t!t#| f&' 7*/ l!+ l#+ t&3 | 1&9 F|#5 |&v&5 O&7 3&|#E /&'  !& |  &!'&) ,' Q& ! 33,Q&'71,b&3 5,| ) I&!'&- -,O&/,) FW&- F| I | 9&+ ','&C,' | %,,)'!''- +'+&!)&!^&' )'+ .5,1 531 ^&',F^&),d 3&N[&''+ -,135 | 5&) 13O&' 3,I&- 3,G&1 +3; 1,| j | [&| 7 | /!= | /#1 /,l&+'3 7*|&S b,|!r ^&7,,3 O&9'+,-3|#  Q&5  !('0| 3&A'13+ K,7*A )'0| #&)(+''('''3X+39 X' U&1 7*/ )'l&-'03'!7*+3,'(5 j&.'3,5 ''0| )&)(5''(+&+3+'F' 7* !&3,)3!K,9 G&!U&)()''( !'(''/3.| K 1& ! &!+&!A&!7&F/ | '&.)(3'- 7*/ '''(!3&' '&' O&!1&!'&!-&!'','(.+(' '(' )(' ,/ 0- -&'(' 1') -'|#+ | 3&)(3''()' !('+&-37* # 3 !'&d | )&)(/' !('+(''0'''& !3&3 7*|#b | '&)(+'' +(''0''Q3+&''h | )&)(3''( !'('')3,9 7*/ =3I z& !'(.'(/' !(',1 7*| 5 Y&' )''(+'0-'+ 7*',)3N|$; |  &)(5'0''F|!7 d!d#7*5,; ,|#U 3&' r& !('0+'' ''+(.)&0Y ,7't&/' !(&+'33.3 ,/''()'| %&='0'')3,-3^ | ;&|%p 5&!n&01'!/' !(',-37 7*I,) '3`&' O' ! (1'0''0''| [ 1&!'&!p&/')  !' ''!1' !&'3 7*/ /&!'&!d&-(!''!'( !'( !'&1 7*|&t I&'''('3|$I M,37+5E7= F|4O&|!; |!M+!-39 |$C&|^) |7l&!5<|zh |,z&}!+p |,^&1 b&!7*+ '3|!/ `&' -'F7 | )&1'-3+7+% !377 7*!1,!M&- I&|/= d!d#Q,+3|!9 | `&+  !'&| 7(1 +'=%| I '% !3%[ } NC&3 |0j&}!>j |&E&| + )&E +&3 |(l&|S= |!E&- =&) 5&1 7&' N''F+<} 4/ |%M77 r7' | A7'()')7/(3<3''71'`7+'| ?7Q | M7)'N|#I K,; | x75 U,|## W!W#W!1#!G#W!W# !! '!' $' '!' +!!3!+# ! #!1#!9#W!W#'!!+!' 3!!1!!W#'!!+!!-! ! !) 1!!W#W!W#W!W#W!W#W!W#W!W#W![#' U!HU#H/#U!HU#H/#U!HU#H/#U!HU#H/#U!HU#H/# !!#' | -*|+E7| 7'+7| -'37.?7.'7-3A -'!A'|>7 1'!E'' 1'!''!-'|$f | #&) 1'1%' 7*+ '&|(3 |  &+'7*- J|<K |$E&' 5,1'v h!h#1'- 7*+ '3|1K | ?,N),J+,| b | #,NA,|$? +&!Y&!'& ! &'  !& 7&!+& # &/ ,+  $& )&!'& ! &'  && '& ! &' +&!1&!+&!+& ! &!7&!E&- )&!-&!E&| 1 '4|&# |  7+ |!77; A7' A7!A7!n77 =,) |!)7) | C7| ; ^7= |  7+ 571 '7? /7|#I |%W7-6|0/77 =7) 97- |!W7; |! 71 ;7K ;7+ | 973 77/ t73 `7| n ;7!|!97!+7) v7' /7) ^7' |##7; ?7' +7+ )7- )7= /7|>l }*Q/&v } !5&9 |$x&' } F'&? } m)&|ez |,'&|AO X` |!/<|!p |%A'}PF' ";


function h$verify_rep_int(x) {
  if(typeof x === 'number' &&
     (x|0) === x
    ) return;
  throw new Error("invalid int rep " + h$show_val(x));
}
function h$verify_rep_long(x, y) {
  if(typeof x === 'number' &&
     typeof y === 'number' &&
     (x|0) === x &&
     (y|0) === y
    ) return;
  throw new Error("invalid long rep " + h$show_val(x) + " " + h$show_val(y));
}
function h$verify_rep_double(x) {
  if(typeof x === 'number') return;
  throw new Error("invalid double rep " + h$show_val(x));
}
function h$verify_rep_arr(x) {
  if(h$verify_rep_is_arr(x)) return;
  throw new Error("invalid array rep " + h$show_val(x));
}
function h$verify_rep_is_arr(x) {
  return (typeof x === 'object'
          && x
          && Array.isArray(x)
        );
}
function h$verify_rep_rtsobj(x) {
}
function h$verify_rep_is_rtsobj(o) {
 return (o instanceof h$MVar ||
         o instanceof h$MutVar ||
         o instanceof h$TVar ||
         o instanceof h$Transaction ||
         o instanceof h$Thread ||
         o instanceof h$Weak ||
         o instanceof h$StableName ||
         h$verify_rep_is_bytearray(o) ||
         h$verify_rep_is_arr(o));
}
function h$verify_rep_is_bytearray(o) {
  return (typeof o === 'object' &&
          o &&
          typeof o.buf === 'object' &&
          o.buf &&
          o.buf instanceof ArrayBuffer &&
          typeof o.len === 'number');
}
function h$verify_rep_heapobj(o) {
  if(h$verify_rep_is_rtsobj(o)) return h$verify_rep_rtsobj(o);
  if(typeof o === 'number' || typeof o === 'boolean') return;
  if(typeof o === 'object' &&
     o &&
     typeof o.f === 'function' &&
     typeof o.f.a === 'number' &&
     (typeof o.m === 'number' || (typeof o.m === 'object' && o.m))
   ) return;
  throw new Error("invalid heapobj rep " + h$show_val(o));
}
function h$verify_rep_addr(v, o) {
  if(typeof o === 'number' &&
     (o|0) === o &&
     typeof v === 'object'
    ) return;
  throw new Error("invalid addr rep " + h$show_val(v) + " " + o);
}
function h$verify_match_alg(tc, v) {
  if(typeof v === 'boolean') {
    if(tc === "ghc-prim:GHC.Types.Bool") return;
    throw new Error("invalid pattern match boolean rep " + tc);
  } else if(typeof v === 'number') {
    return;
  } else if(typeof v === 'object') {
    if(!(typeof v.f === 'function' &&
         typeof v.f.a === 'number' &&
         typeof v.f.t === 'number' &&
         v.f.t === 2
       )) {
         throw new Error("not a data constructor " + tc + ": " + h$show_val(v));
    }
    return;
  }
  throw new Error("invalid pattern match rep " + tc + ": " + h$show_val(v));
}
function h$show_val(o) {
  if(typeof o === 'undefined') return '<undefined>'
  if(o === null) return '<null>'
  if(typeof o !== 'object') return '[' + (typeof o) + ': ' + o + ']'
  return '' + o + ' [' + o.constructor.name + '] ' + h$collectProps(o);
}

var h$weakPointerList = [];
function h$finalizeWeaks(toFinalize) {
    var mark = h$gcMark;
    var i, w;
    if(toFinalize.length > 0) {
        var t = new h$Thread();
        for(i=0;i<toFinalize.length;i++) {
            w = toFinalize[i];
            t.sp += 6;
            t.stack[t.sp-5] = 0;
            t.stack[t.sp-4] = h$noop;
            t.stack[t.sp-3] = h$catch_e;
            t.stack[t.sp-2] = h$ap_1_0;
            t.stack[t.sp-1] = w.finalizer;
            t.stack[t.sp] = h$return;
            w.finalizer = null;
        }
        h$wakeupThread(t);
    }
}
var h$weakN = 0;
function h$Weak(key, val, finalizer) {
    if(typeof key !== 'object') {
        this.keym = new h$StableName(0);
    } else {
        if(typeof key.m !== 'object') {
          if(typeof key.m !== 'number') {
            h$log("attaching weak to unsupported object");
          }
          key.m = new h$StableName(key.m);
        }
        this.keym = key.m;
    }
    this.keym = key.m;
    this.val = val;
    this.finalizer = null;
    if(finalizer !== null) {
        this.finalizer = finalizer;
    }
    this.m = 0;
    this._key = ++h$weakN;
    h$weakPointerList.push(this);
}
function h$makeWeak(key, val, fin) {
    return new h$Weak(key, val, fin)
}
function h$makeWeakNoFinalizer(key, val) {
    return new h$Weak(key, val, null);
}
function h$finalizeWeak(w) {
    w.val = null;
    if(w.finalizer === null || w.finalizer.finalizer === null) {
        { h$ret1 = (0); return (null); };
    } else {
        var r = w.finalizer;
        w.finalizer = null;
        { h$ret1 = (1); return (r); };
    }
}

var h$global_has_ppr_debug_a = null; var h$global_has_ppr_debug_o = null; function h$getOrSetLibHSghcGlobalHasPprDebug(a,o) { if (!h$global_has_ppr_debug_a) { h$global_has_ppr_debug_a = a; h$global_has_ppr_debug_o = o; } { h$ret1 = (h$global_has_ppr_debug_o); return (h$global_has_ppr_debug_a); }; }
var h$global_has_no_debug_output_a = null; var h$global_has_no_debug_output_o = null; function h$getOrSetLibHSghcGlobalHasNoDebugOutput(a,o) { if (!h$global_has_no_debug_output_a) { h$global_has_no_debug_output_a = a; h$global_has_no_debug_output_o = o; } { h$ret1 = (h$global_has_no_debug_output_o); return (h$global_has_no_debug_output_a); }; }
var h$global_has_no_state_hack_a = null; var h$global_has_no_state_hack_o = null; function h$getOrSetLibHSghcGlobalHasNoStateHack(a,o) { if (!h$global_has_no_state_hack_a) { h$global_has_no_state_hack_a = a; h$global_has_no_state_hack_o = o; } { h$ret1 = (h$global_has_no_state_hack_o); return (h$global_has_no_state_hack_a); }; }
var h$global_faststring_table_a = null; var h$global_faststring_table_o = null; function h$getOrSetLibHSghcFastStringTable(a,o) { if (!h$global_faststring_table_a) { h$global_faststring_table_a = a; h$global_faststring_table_o = o; } { h$ret1 = (h$global_faststring_table_o); return (h$global_faststring_table_a); }; }

function h$clock_gettime(when, p_d, p_o) {
  var is64 = p_d.i3.length == 4 && p_o == 0;
  var o  = p_o >> 2;
  var t  = Date.now ? Date.now() : new Date().getTime();
  var tf = Math.floor(t / 1000);
  var tn = 1000000 * (t - (1000 * tf));

  if (is64) {
    p_d.i3[o]   = tf|0;
    p_d.i3[o+1] = 0;
    p_d.i3[o+2] = tn|0;
    p_d.i3[o+3] = 0;
  } else {
    p_d.i3[o]   = tf|0;
    p_d.i3[o+1] = tn|0;
  }
  return 0;
}

function h$CLOCK_REALTIME() { return 0; }

