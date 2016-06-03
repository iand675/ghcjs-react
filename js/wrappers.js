
function captureThis(f, t) {
  return function() {
    var args = Array.from(arguments);
    args.unshift(t || this);
    return f.apply(this, args);
  };
}
