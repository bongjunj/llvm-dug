define i1 @icmp_equality_test_commute_select1(i64 %X, i64 %Y, i64 %Z) {
entry:
  %XeqZ = icmp eq i64 %X, %Z
  %YeqZ = icmp eq i64 %Y, %Z
  %XeqY = icmp eq i64 %X, %Y
  %and = select i1 %YeqZ, i1 false, i1 %XeqY
  %equal = select i1 %XeqZ, i1 %YeqZ, i1 %and
  ret i1 %equal
}

define i1 @icmp_equality_test_commute_select2(i64 %X, i64 %Y, i64 %Z) {
entry:
  %XeqZ = icmp eq i64 %X, %Z
  %YeqZ = icmp ne i64 %Y, %Z
  %XeqY = icmp eq i64 %X, %Y
  %and = select i1 %YeqZ, i1 %XeqY, i1 false
  %equal = select i1 %XeqZ, i1 %YeqZ, i1 %and
  ret i1 %equal
}