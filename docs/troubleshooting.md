# Troubleshooting Guide

Common issues and solutions.

## Compilation Issues

### Files not recompiling

**Symptom:**
```bash
$ touch src/myapp.lfe
$ rebar3 lfe compile
==> All files up to date
```

**Cause:** Timestamp issue or cache problem

**Solution:**
```bash
# Force full recompilation
rebar3 lfe clean
rebar3 lfe compile
```

### Header changes ignored

**In 0.5.0, this should not happen!** If it does:

```bash
# Check header is in include/
ls -la include/*.lfe

# Verify include in source
grep "include-file" src/*.lfe

# Force recompile
rebar3 lfe clean
rebar3 lfe compile
```

**Report as bug if issue persists.**

### "Module not found" errors

**Symptom:**
```
Error: Module 'mymodule' not found
```

**Solutions:**

1. **Check filename matches module name:**
   ```lfe
   ;; In src/mymodule.lfe
   (defmodule mymodule)  ; ✅ Names match
   ```

2. **Check for typos:**
   ```lfe
   (defmodule my-module)  ; Hyphens
   ;; vs
   (defmodule my_module)  ; Underscores
   ```

3. **For packages, check path:**
   ```
   src/my/app/core.lfe → (defmodule my.app.core)
   ```

## REPL Issues

### REPL won't start

**Symptom:**
```bash
$ rebar3 lfe repl
Error: ...
```

**Solutions:**

1. **Check LFE version:**
   ```erlang
   {deps, [
       {lfe, "~> 2.0"}  % Latest compatible 2.x release
   ]}.
   ```

2. **Try without apps:**
   ```bash
   rebar3 lfe repl --apps ""
   ```

3. **Check for errors:**
   ```bash
   rebar3 lfe compile
   # Fix any compilation errors first
   ```

### Module not available in REPL

**Symptom:**
```lfe
> (mymodule:function)
** exception error: undefined function mymodule:function/0
```

**Solutions:**

1. **Compile first:**
   ```bash
   rebar3 lfe compile
   rebar3 lfe repl
   ```

2. **Reload in REPL:**
   ```lfe
   > (c "src/mymodule.lfe")
   {ok, mymodule}
   ```

## Package System Issues

### Temporary files not cleaned

**Symptom:**
```bash
$ ls src/
my.package.file.lfe  # Should not be there
```

**In 0.5.0, this should not happen!**

**Workaround:**
```bash
# Manual cleanup
find src -name "*.*.lfe" -type f -delete

# Report as bug
```

### Package name errors

**Symptom:**
```
Error: Invalid module name 'my..package'
```

**Cause:** Double dots in path or filename

**Solution:**
```bash
# ❌ Wrong
src/my//package.lfe

# ✅ Correct
src/my/package.lfe
```

## Performance Issues

### Slow compilation

**Check:**

1. **Debug logging enabled?**
   ```bash
   unset DEBUG
   rebar3 lfe compile
   ```

2. **Full rebuild happening?**
   ```bash
   # Should be fast:
   rebar3 lfe compile
   # If slow, check for issues
   ```

3. **Many files?**
   - First compile: Expected to be slow
   - Incremental: Should be fast
   - Check progress output

### Slow REPL startup

**Solutions:**

1. **Don't start all apps:**
   ```bash
   rebar3 lfe repl --apps ""
   ```

2. **Use release for production:**
   ```bash
   rebar3 lfe release
   ```

## Dependency Issues

### "ltest not found"

**Symptom:**
```
Error: ltest not found
```

**Solution:**
```erlang
{profiles, [
    {test, [
        {deps, [
            {ltest, "0.13.5"}
        ]}
    ]}
]}.
```

Then:
```bash
rebar3 as test lfe ltest
```

### Include from dependency fails

**Symptom:**
```
Error: Cannot find include file "dep/include/file.lfe"
```

**Solution:**
```lfe
;; Use include-lib
(include-lib "depname/include/file.lfe")
```

## Windows-Specific Issues

### Path separator errors

**r3lfe 0.5.0 handles this automatically!**

If you still see issues:
```bash
# Use forward slashes in config
{lfe_include_dirs, ["include"]}.  % Not "include\\"
```

## Getting More Help

### Enable Debug Logging

```bash
DEBUG=1 rebar3 lfe compile
```

### Check Versions

```bash
rebar3 lfe versions
```

### File an Issue

Include:
- r3lfe version
- Erlang/OTP version
- LFE version
- Full error message
- Steps to reproduce

**Issue Tracker:** https://github.com/lfe-rebar3/rebar3_lfe/issues
