# Release Checklist

Use this checklist when preparing a release.

## Pre-Release

- [ ] All tests passing on all supported OTP versions (24-28)
- [ ] CI green on main branch
- [ ] Code coverage >90%
- [ ] Dialyzer clean
- [ ] No known critical bugs

## Version Update

- [ ] Update version in `src/rb3lfe.app.src`
- [ ] Update docs/release-history.md with release notes
- [ ] Update README.md if needed
- [ ] Check all documentation up to date

## Testing

- [ ] Run full test suite: `make check`
- [ ] Run integration tests: `make smoke-tests`
- [ ] Test on real projects
- [ ] Test migration from 0.4.x

## Documentation

- [ ] All new features documented
- [ ] All breaking changes noted
- [ ] Examples work
- [ ] Migration guide complete

## Pre-Flight

- [ ] Create release branch: `release/0.5.x`
- [ ] Update release-history.md date
- [ ] Commit: `chore: prepare v0.5.0 release`
- [ ] Push to GitHub

## Release

- [ ] Create GitHub release with notes
- [ ] Tag: `git tag v0.5.0`
- [ ] Push tags: `git push origin v0.5.0`
- [ ] Publish to hex.pm: `rebar3 hex publish`

## Post-Release

- [ ] Verify on hex.pm
- [ ] Test installation from hex
- [ ] Announce on LFE mailing list
- [ ] Tweet about release
- [ ] Update documentation site

## If Issues Found

- [ ] Document in GitHub issues
- [ ] Plan hotfix if critical
- [ ] Update troubleshooting guide
